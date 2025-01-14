import { ParserState, type HbsConstruct } from '../errors.js';
import { GlimmerSyntaxError } from '../syntax-error';
import { isHBSLiteral } from '../utils';
import { exhaustive } from '../utils/assert';
import type { Optional } from '../utils/exists.js';
import type * as ASTv1 from '../v1/api';
import type * as HBS from '../v1/handlebars-ast';
import { ErrorExpression, ErrorStatement, ToErrorStatement } from '../v1/handlebars-utils';
import type { Parser } from './parser';

type HandlebarsCallbacks = {
  [P in keyof HBS.NodeMap]: (node: HBS.NodeMap[P]['input']) => HBS.NodeMap[P]['output'];
};

export class HandlebarsNodeVisitors implements HandlebarsCallbacks {
  static create(parser: () => Parser): HandlebarsNodeVisitors {
    return new HandlebarsNodeVisitors(parser);
  }

  #parserThunk: () => Parser;

  constructor(parser: () => Parser) {
    this.#parserThunk = parser;
  }

  get #parser() {
    return this.#parserThunk();
  }

  get #b() {
    return this.#parser.builder;
  }

  accept<K extends keyof HBS.NodeMap>(node: HBS.NodeMap[K]['input']): HBS.NodeMap[K]['output'] {
    this.#parser.traced(`${node.type}:begin`);

    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment,@typescript-eslint/no-unsafe-member-access,,@typescript-eslint/no-unsafe-call,@typescript-eslint/no-explicit-any
    const result = (this as any)[node.type as K](node);

    this.#parser.traced(`${node.type}:end`);

    return result as HBS.NodeMap[K]['output'];
  }

  Template(program: HBS.Program): ASTv1.Template {
    let node = this.#b.template({
      body: [],
      blockParams: program.blockParams,
      loc: this.#b.span(program.loc),
    });

    this.#parser.parent.begin(node);

    for (const child of program.body) {
      this.#parser.accept(child);
    }

    this.#parser.parent.finalize(node);

    return node;
  }

  Program(program: HBS.Program): ASTv1.Block {
    let node = this.#b.blockItself({
      body: [],
      blockParams: program.blockParams,
      chained: program.chained,
      loc: this.#b.span(program.loc),
    });

    this.#parser.parent.begin(node);

    for (const child of program.body) {
      this.#parser.accept(child);
    }

    this.#parser.parent.finalize(node);

    return node;
  }

  BlockStatement(block: HBS.BlockStatement): ASTv1.BlockStatement | HBS.ErrorStatement | void {
    switch (this.#parser.state()) {
      case 'comment':
        this.#parser.addChars(this.#parser.slice(block.loc));
        return;
      case 'top-level':
        break;
      default:
        this.#parser.error('hbs.syntax.invalid-block', block.loc);
    }

    const result = this.#acceptCallNodes(this, block);

    if (result.type === 'err') {
      return this.#forwardStatementError(result.error);
    }

    const { path, params, hash } = result.value;

    // These are bugs in Handlebars upstream
    if (!block.program.loc) {
      block.program.loc = this.#b.span('missing');
    }

    if (block.inverse && !block.inverse.loc) {
      block.inverse.loc = this.#b.span('missing');
    }

    let program = this.Program(block.program);
    let inverse = block.inverse ? this.Program(block.inverse) : null;

    let node = this.#b.block({
      path,
      params,
      hash,
      defaultBlock: program,
      elseBlock: inverse,
      loc: this.#b.span(block.loc),
      openStrip: block.openStrip,
      inverseStrip: block.inverseStrip,
      closeStrip: block.closeStrip,
    });

    this.#parser.parent.append(node);
  }

  MustacheStatement(
    rawMustache: HBS.MustacheStatement
  ): ASTv1.MustacheStatement | HBS.ErrorStatement | void {
    const build = (): ASTv1.MustacheStatement | HBS.ErrorStatement => {
      let { escaped, loc, strip } = rawMustache;

      if (isHBSLiteral(rawMustache.path)) {
        return this.#b.mustache({
          path: this.#parser.accept(rawMustache.path),
          params: [],
          hash: this.#b.hash([], this.#b.span(rawMustache.path.loc).collapse('end')),
          trusting: !escaped,
          loc: this.#b.span(loc),
          strip,
        });
      } else {
        let result = this.#acceptCallNodes(
          this,
          rawMustache as HBS.MustacheStatement & {
            path: HBS.PathExpression | HBS.SubExpression;
          }
        );

        if (result.type === 'err') {
          return this.#forwardStatementError(result.error);
        }

        const { path, params, hash } = result.value;

        return this.#b.mustache({
          path,
          params,
          hash,
          trusting: !escaped,
          loc: this.#b.span(loc),
          strip,
        });
      }
    };

    const state = this.#parser.state();
    const mustache = build();

    // Handle some cases where the tokenizer state has changed but no tokenizer events fired yet.
    // TODO: Enumerate these cases somewhere and synchronize the parser state with the tokenizer state.
    if (state === 'attribute:value:before') {
      this.#parser.transitionTo('attributeValueUnquoted');
      this.#parser.attr.value(false);
    }

    if (state === 'tag:top-level') {
      this.#parser.tag.body();
    }

    if (state === 'tag-name:start:before') {
      return this.#invalidCurly(ParserState.TagName, mustache.loc);
    }

    if ('error' in mustache) {
      return mustache;
    }

    switch (this.#parser.type) {
      case 'block':
      case 'element':
        this.#parser.parent.append(mustache);
        break;
      case 'comment':
      case 'text':
        this.#parser.addChars(this.#parser.slice(rawMustache.loc));
        break;
      case 'tag:name':
      case 'tag:end':
        return this.#invalidCurly(ParserState.TagName, mustache.loc);
      case 'tag:start':
        this.#parser.element.modifier(mustache);
        break;
      case 'tag:attr:name':
        this.#parser.attr.finalize();
        this.#parser.element.modifier(mustache);
        break;
      case 'tag:attr:value':
        this.#parser.attr.value.dynamic(mustache);
        break;
    }

    return mustache;

    switch (state) {
      case 'comment': {
        this.#parser.addChars(this.#parser.slice(rawMustache.loc));
        return;
      }

      case 'tag-name:start:before':
      case 'tag-name:start:in':
      case 'tag-name:end:in':
        return this.#invalidCurly(ParserState.TagName, mustache.loc);

      case 'tag:top-level': {
        this.#ifOk(mustache, (m) => {
          this.#parser.element.modifier(m);
        });
        return mustache;
      }

      case 'attribute:name:in':
      case 'attribute:name:after': {
        this.#ifOk(mustache, (m) => {
          this.#parser.attr.finalize();
          this.#parser.element.modifier(m);
        });
        return mustache;
      }

      case 'attribute:value:before': {
        this.#ifOk(mustache, (m) => {
          this.#parser.transitionTo('attributeValueUnquoted');
          this.#parser.attr.value(false);
          this.#parser.attr.value.dynamic(m);
        });
        return mustache;
      }

      case 'attribute:value:double-quoted':
      case 'attribute:value:single-quoted':
      case 'attribute:value:unquoted': {
        this.#ifOk(mustache, (m) => this.#parser.attr.value.dynamic(m));
        return mustache;
      }

      // TODO: Only append child when the tokenizer state makes
      // sense to do so, otherwise throw an error.
      default: {
        this.#parser.parent.append(mustache);
        return mustache;
      }
    }
  }

  #ifOk<T>(
    value: T | HBS.ErrorExpression | HBS.ErrorStatement,
    callback: (value: T) => void
  ): void {
    if ('error' in value) {
      this.#parser.reportError(value.error);
    } else {
      callback(value);
    }
  }

  ContentStatement(content: HBS.ContentStatement): void {
    this.#parser.tokenize(content);
  }

  CommentStatement(rawComment: HBS.CommentStatement): Optional<ASTv1.MustacheCommentStatement> {
    const state = this.#parser.state();
    const comment = this.#b.mustacheComment(rawComment.value, this.#b.span(rawComment.loc));

    switch (state) {
      case 'comment': {
        this.#parser.addChars(this.#parser.slice(rawComment.loc));
        return null;
      }

      case 'tag:top-level': {
        this.#parser.tag.comment(comment);
        return comment;
      }

      case 'top-level': {
        this.#parser.parent.append(comment);
        return comment;
      }

      case 'attribute:name:after': {
        this.#parser.attr.finalize();
        this.#parser.element.comment(comment);
        return comment;
      }

      case 'attribute:name:in': {
        this.#parser.error('html.syntax.invalid-hbs-comment', ParserState.AttrName, rawComment.loc);
        return comment;
      }

      case 'attribute:value:single-quoted':
      case 'attribute:value:double-quoted':
      case 'attribute:value:unquoted':
      case 'attribute:value:before': {
        return ErrorStatement(
          this.#parser.error(
            'html.syntax.invalid-hbs-comment',
            ParserState.AttrValue,
            rawComment.loc
          )
        );
      }

      default: {
        return ErrorStatement(
          this.#parser.error(
            'html.syntax.invalid-hbs-comment',
            ParserState.AttrValue,
            rawComment.loc
          )
        );
      }
    }
  }

  #forwardStatementError(
    error: GlimmerSyntaxError | HBS.ErrorStatement | HBS.ErrorExpression
  ): HBS.ErrorStatement {
    if (error instanceof GlimmerSyntaxError) {
      return ErrorStatement(error);
    } else if (error.type === 'StringLiteral') {
      return ToErrorStatement(error);
    } else if (error.type === 'MustacheCommentStatement') {
      return error;
    }

    exhaustive(error);
  }

  #invalidCurly(state: ParserState, loc: HBS.SourceLocation): HBS.ErrorStatement {
    return ErrorStatement(this.#parser.error('html.syntax.invalid-hbs-curly', state, loc));
  }

  #invalidHbsConstruct(construct: HbsConstruct, loc: HBS.SourceLocation): HBS.ErrorStatement {
    return ErrorStatement(this.#parser.error('hbs.syntax.unsupported-construct', construct, loc));
  }

  PartialStatement(partial: HBS.PartialStatement): HBS.ErrorStatement {
    return this.#invalidHbsConstruct('Partial', partial.loc);
  }

  PartialBlockStatement(partialBlock: HBS.PartialBlockStatement): HBS.ErrorStatement {
    return this.#invalidHbsConstruct('PartialBlock', partialBlock.loc);
  }

  Decorator(decorator: HBS.Decorator): HBS.ErrorStatement {
    return this.#invalidHbsConstruct('Decorator', decorator.loc);
  }

  DecoratorBlock(decoratorBlock: HBS.DecoratorBlock): HBS.ErrorStatement {
    return this.#invalidHbsConstruct('DecoratorBlock', decoratorBlock.loc);
  }

  SubExpression(sexpr: HBS.SubExpression): ASTv1.SubExpression | HBS.ErrorExpression {
    const result = this.#acceptCallNodes(this, sexpr);

    if (result.type === 'err') {
      return ErrorExpression(result.error);
    }

    const { path, params, hash } = result.value;

    return this.#b.sexpr({
      path,
      params,
      hash,
      loc: this.#b.span(sexpr.loc),
    });
  }

  PathExpression(path: HBS.PathExpression): ASTv1.PathExpression | HBS.ErrorExpression {
    let { original } = path;
    let parts: string[];

    if (original.indexOf('/') !== -1) {
      if (original.slice(0, 2) === './') {
        return ErrorExpression(this.#parser.error('hbs.syntax.invalid-dotslash', path.loc));
      }
      if (original.slice(0, 3) === '../') {
        return ErrorExpression(this.#parser.error('hbs.syntax.invalid-dotdot', path.loc));
      }
      if (original.indexOf('.') !== -1) {
        return ErrorExpression(this.#parser.error('hbs.syntax.invalid-slash', path.loc));
      }
      parts = [path.parts.join('/')];
    } else if (original === '.') {
      return ErrorExpression(this.#parser.error('hbs.syntax.invalid-dot', path.loc));
    } else {
      parts = path.parts;
    }

    let thisHead = false;

    // This is to fix a bug in the Handlebars AST where the path expressions in
    // `{{this.foo}}` (and similarly `{{foo-bar this.foo named=this.foo}}` etc)
    // are simply turned into `{{foo}}`. The fix is to push it back onto the
    // parts array and let the runtime see the difference. However, we cannot
    // simply use the string `this` as it means literally the property called
    // "this" in the current context (it can be expressed in the syntax as
    // `{{[this]}}`, where the square bracket are generally for this kind of
    // escaping – such as `{{foo.["bar.baz"]}}` would mean lookup a property
    // named literally "bar.baz" on `this.foo`). By convention, we use `null`
    // for this purpose.
    if (original.match(/^this(\..+)?$/)) {
      thisHead = true;
    }

    let pathHead: ASTv1.PathHead;
    if (thisHead) {
      pathHead = {
        type: 'ThisHead',
        loc: this.#b.span({
          start: path.loc.start,
          end: { line: path.loc.start.line, column: path.loc.start.column + 4 },
        }),
      };
    } else if (path.data) {
      let head = parts.shift();

      if (head === undefined) {
        this.#parser.error('hbs.syntax.invalid-argument', path.loc);
        head = `error`;
      }

      pathHead = {
        type: 'AtHead',
        name: `@${head}`,
        loc: this.#b.span({
          start: path.loc.start,
          end: {
            line: path.loc.start.line,
            column: path.loc.start.column + head.length + 1,
          },
        }),
      };
    } else {
      let head = parts.shift();

      if (head === undefined) {
        this.#parser.error('hbs.syntax.invalid-variable', path.loc);
        head = '';
      }

      pathHead = this.#b.head(
        head,
        this.#b.span({
          start: path.loc.start,
          end: {
            line: path.loc.start.line,
            column: path.loc.start.column + head.length,
          },
        })
      );
    }

    return this.#b.path({
      head: pathHead,
      tail: parts,
      loc: this.#b.span(path.loc),
    });
  }

  Hash(hash: HBS.Hash): ASTv1.Hash {
    let pairs: ASTv1.HashPair[] = [];

    for (let i = 0; i < hash.pairs.length; i++) {
      let pair = hash.pairs[i];
      pairs.push(
        this.#b.pair({
          key: pair.key,
          value: this.#parser.accept(pair.value),
          loc: this.#b.span(pair.loc),
        })
      );
    }

    return this.#b.hash(pairs, this.#b.span(hash.loc));
  }

  StringLiteral(string: HBS.StringLiteral): ASTv1.StringLiteral {
    return this.#b.literal({
      type: 'StringLiteral',
      value: string.value,
      loc: this.#b.span(string.loc),
    });
  }

  BooleanLiteral(boolean: HBS.BooleanLiteral): ASTv1.BooleanLiteral {
    return this.#b.literal({
      type: 'BooleanLiteral',
      value: boolean.value,
      loc: this.#b.span(boolean.loc),
    });
  }

  NumberLiteral(number: HBS.NumberLiteral): ASTv1.NumberLiteral {
    return this.#b.literal({
      type: 'NumberLiteral',
      value: number.value,
      loc: this.#b.span(number.loc),
    });
  }

  UndefinedLiteral(undef: HBS.UndefinedLiteral): ASTv1.UndefinedLiteral {
    return this.#b.literal({
      type: 'UndefinedLiteral',
      value: undefined,
      loc: this.#b.span(undef.loc),
    });
  }

  NullLiteral(nul: HBS.NullLiteral): ASTv1.NullLiteral {
    return this.#b.literal({
      type: 'NullLiteral',
      value: null,
      loc: this.#b.span(nul.loc),
    });
  }

  #acceptCallNodes(
    compiler: HandlebarsNodeVisitors,
    node: {
      path: HBS.Expression;
      params: HBS.Expression[];
      hash: HBS.Hash;
    }
  ):
    | {
        type: 'ok';
        value: {
          path: ASTv1.PathExpression | ASTv1.SubExpression;
          params: ASTv1.Expression[];
          hash: ASTv1.Hash;
        };
      }
    | { type: 'err'; error: GlimmerSyntaxError } {
    if (isLiteral(node.path)) {
      node.path;
      return {
        type: 'err',
        error: this.#parser.error('hbs.syntax.not-callable', node.path, node.path.loc),
      };
    }

    const path =
      node.path.type === 'PathExpression'
        ? compiler.PathExpression(node.path)
        : compiler.SubExpression(node.path as unknown as HBS.SubExpression);

    if (path.type === 'StringLiteral') {
      return { type: 'err', error: path.error };
    }

    const params = node.params ? node.params.map((e) => this.#parser.accept(e)) : [];

    // if there is no hash, position it as a collapsed node immediately after the last param (or the
    // path, if there are also no params)
    const end = params.length > 0 ? params[params.length - 1].loc : path.loc;

    const hash = node.hash
      ? compiler.Hash(node.hash)
      : ({
          type: 'Hash',
          pairs: [] as ASTv1.HashPair[],
          loc: this.#b.span(end).collapse('end'),
        } as const);

    return { type: 'ok', value: { path, params, hash } };
  }
}

function isLiteral(expression: HBS.Expression): expression is HBS.Literal {
  return expression.type.endsWith('Literal');
}
