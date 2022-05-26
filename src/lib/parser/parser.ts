import { LOCAL_DEBUG } from '@glimmer/local-debug-flags';
import type {
  EventedTokenizer,
  TokenizerState as UpstreamTokenizerState,
} from 'simple-html-tokenizer';
import type { SyntaxErrorArgs } from '../errors';
import { SymbolicSyntaxError } from '../errors';
import { voidMap } from '../generation/printer.js';
import type { SourceOffset } from '../source/loc/offset';
import type { SourceSpan } from '../source/loc/source-span';
import type { SourceTemplate } from '../source/source';
import type { GlimmerSyntaxError } from '../syntax-error.js';
import { getBlockParams, isHBSLiteral } from '../utils';
import { isPresent } from '../utils/array.js';
import { assert } from '../utils/assert.js';
import { existing } from '../utils/exists.js';
import { assign, extract } from '../utils/object.js';
import { PresentStack, Stack } from '../utils/stack.js';
import type * as ASTv1 from '../v1/api';
import type * as HBS from '../v1/handlebars-ast';
import { Phase1Builder, type ToBuilderSpan } from '../v1/parser-builders';
import type { HandlebarsNodeVisitors } from './handlebars-node-visitors';
import { Scope } from './scope';
import {
  asSimpleState,
  validate,
  type EventName,
  type SimplifiedTokenizerState,
  type TokenizerState,
} from './tokenizer-types';
import { Tracer, type TraceArgs } from './trace';

export type ParserNodeBuilder<N extends { loc: SourceSpan }> = Omit<N, 'loc'> & {
  loc: SourceOffset;
};

export interface Tag<T extends 'StartTag' | 'EndTag'> {
  readonly type: T;
  name: string;
  readonly attributes: ASTv1.AttrNode[];
  readonly modifiers: ASTv1.ElementModifierStatement[];
  readonly comments: ASTv1.MustacheCommentStatement[];
  selfClosing: boolean;
  readonly loc: SourceSpan;
}

export type AnyConstructing =
  | ParserNodeBuilder<ASTv1.CommentStatement>
  | ParserNodeBuilder<Tag<'StartTag'>>
  | ParserNodeBuilder<Tag<'EndTag'>>
  | ConstructingAttribute
  | ConstructingAttributeValue
  | ASTv1.TextNode;

export class Parser {
  static create(
    template: SourceTemplate,
    tokenizer: EventedTokenizer,
    handlebars: HandlebarsNodeVisitors
  ): Parser {
    return new Parser(
      template,
      tokenizer,
      Tracer.create(),
      handlebars,
      [],
      Stack.from([Phase1Builder.withScope(template, Scope.top(template.options))])
    );
  }

  #template: SourceTemplate;
  #tokenizer: EventedTokenizer;
  #tracer: Tracer;
  #handlebars: HandlebarsNodeVisitors;
  #errors: GlimmerSyntaxError[];
  #builderStack: Stack<Phase1Builder>;
  #constructingStack: PresentStack<Constructing>;

  constructor(
    template: SourceTemplate,
    tokenizer: EventedTokenizer,
    tracer: Tracer,
    handlebars: HandlebarsNodeVisitors,
    errors: GlimmerSyntaxError[],
    builderStack: Stack<Phase1Builder>
  ) {
    this.#template = template;
    this.#tokenizer = tokenizer;
    this.#tracer = tracer;
    this.#handlebars = handlebars;
    this.#errors = errors;
    this.#builderStack = builderStack;
    this.#constructingStack = PresentStack.create(new ConstructingBlock(this));
  }

  traced(name: `${string}:end`): void;
  traced(name: `${string}:begin`, value?: TraceArgs): void;
  traced(name: `${EventName}:trace`, value?: TraceArgs): void;
  traced(name: string, value?: TraceArgs): void {
    if (LOCAL_DEBUG) {
      const match = existing(name.match(/^(?<title>.*?)(?::(?<child>begin|end|trace))?$/));
      const groups = existing(match.groups) as
        | { title: EventName; child: 'trace' }
        | { title: string; child: 'begin' | 'end' };

      switch (groups.child) {
        case 'begin':
          this.#tracer.begin(groups.title, this.#tokenizer.state, value);
          return;
        case 'end':
          this.#tracer.end(groups.title);
          return;
        case 'trace': {
          this.#tracer.trace(
            `${this.state(groups.title)} (event: ${groups.title}, state: ${
              this.#constructingStack.current.type
            })`,
            this.#tokenizer.state,
            value
          );
        }
      }
    }
  }

  state(event?: EventName): SimplifiedTokenizerState {
    if (event) {
      validate(event, this.#tokenizer.state);
    }

    return asSimpleState(this.#tokenizer.state, event);
  }

  get builder(): Phase1Builder {
    // @ts-expect-error FIXME
    return this.#builderStack.current;
  }

  tokenize(content: HBS.ContentStatement) {
    let line = content.loc.start.line;
    let column = content.loc.start.column;

    let offsets = calculateRightStrippedOffsets(content.original, content.value);

    line = line + offsets.lines;
    if (offsets.lines) {
      column = offsets.columns;
    } else {
      column = column + offsets.columns;
    }

    this.#tokenizer.line = line;
    this.#tokenizer.column = column;

    this.#tokenizer.tokenizePart(content.value);
    this.#tokenizer.flushData();
  }

  transitionTo(state: TokenizerState) {
    this.#tokenizer.transitionTo(state as UpstreamTokenizerState);
  }

  slice(positions: { start: HBS.SourcePosition; end: HBS.SourcePosition }): string {
    return this.#template.sliceAST(positions);
  }

  acceptTemplate(node: HBS.Program): ASTv1.Template {
    const template = this.#handlebars.Template(node);

    if (isPresent(this.#errors)) {
      template.errors = this.#errors;
    }

    return template;
  }

  accept<N extends HBS.Node & { type: keyof HBS.NodeMap }>(
    node: N
  ): HBS.NodeMap[N['type']]['output'] {
    return this.#handlebars.accept(node);
  }

  reportError(error: GlimmerSyntaxError): void {
    this.#errors.push(error);
  }

  error(error: Extract<SyntaxErrorArgs, string>, span: ToBuilderSpan): GlimmerSyntaxError;
  error<K extends Extract<SyntaxErrorArgs, unknown[]>[0]>(
    name: K,
    arg: Extract<SyntaxErrorArgs, [K, any]>[1],
    span: ToBuilderSpan
  ): GlimmerSyntaxError;
  error(error: string, args: unknown | ToBuilderSpan, span?: ToBuilderSpan): GlimmerSyntaxError {
    if (span === undefined) {
      const err = new SymbolicSyntaxError(error as SyntaxErrorArgs).spanned(
        this.builder.span(args as HBS.SourceLocation | SourceSpan)
      );
      this.reportError(err);
      return err;
    } else {
      const err = new SymbolicSyntaxError([error, args] as SyntaxErrorArgs).spanned(
        this.builder.span(span)
      );
      this.reportError(err);
      return err;
    }
  }

  /**
   * For now, pass through most tokenizer errors. Ultimately, we should convert
   * these errors into `GlimmerSyntaxError`s. At the moment, we ignore errors in
   * the attribute name state, because we can recover from them (by looking for
   * invalid attribute names later on in the processing pipeline).
   */
  tokenizerError(message: string) {
    if (this.#is(ConstructingAttribute)) {
      return;
    } else {
      this.error('passthrough.tokenizer', message, this.offset().collapsed());
    }
  }

  offset(): SourceOffset {
    let { line, column } = this.#tokenizer;
    return this.#template.offsetFor(line, column);
  }

  readonly parent = {
    append: (node: ASTv1.Statement) => {
      this.#constructing(ConstructingParent).append(node);
    },

    begin: (node: ASTv1.Template | ASTv1.Block) => {
      this.#begin(new ConstructingBlock(this), { assert: ConstructingParent });
      this.#pushScope(node.blockParams);
    },

    finalize: (node: ASTv1.Template | ASTv1.Block) => {
      const block = this.#popConstructing(ConstructingBlock, { assert: false });

      if (block) {
        block.finalize(node);
        this.#popScope();
        return;
      }

      const element = existing(
        this.#nearest(ConstructingElement),
        `Unexpected parent.finalize with no parent on the stack\n${this.#tracer.print()}`
      );

      if (element) {
        this.error('elements.unclosed-element', element.name.tag, element.name.span);
      }
    },
  };

  readonly comment = {
    begin: () => {
      this.#constructingStack.push(new ConstructingComment(this, this.offset().move(-4)));
    },
    finalize: () => {
      const comment = this.#popConstructing(ConstructingComment).finish();
      this.#constructing(ConstructingParent).append(comment);
    },
  };

  readonly text = {
    begin: () => {
      this.#constructingStack.push(new ConstructingText(this));
    },
    finalize: () => {
      const text = this.#popConstructing(ConstructingText).finish();

      this.#constructing(ConstructingParent).append(text);
    },
  };

  readonly tag = {
    finalize: () => {
      if (this.#is(ConstructingParent)) {
        // we're finalizing a tag due to a self-closing tag, and there's nothing to do
        return;
      }

      const end = this.#popConstructing(ConstructingEndTag, { assert: false });

      if (end) {
        this.tag.end.finalize(end.finish());
        return;
      }

      this.tag.body();

      const start = this.#popConstructing(ConstructingStartTag).finish();

      if (start.name.tag === ':') {
        this.error('html.syntax.invalid-named-block', start.name.span);
      }

      if (voidMap[start.name.tag]) {
        this.#constructing(ConstructingParent).append(start.void(this.builder));
      } else {
        this.#begin(new ConstructingElement(this, start));
      }
    },

    body: () => {
      const name = this.#popConstructing(ConstructingTagName, { assert: false });

      if (name) {
        this.#begin(name.finish());
      }
    },

    comment: (comment: ASTv1.MustacheCommentStatement) => {
      this.tag.body();

      this.#constructing(ConstructingStartTag).comment(comment);
    },

    start: {
      begin: () => {
        this.#begin(new ConstructingTagName(this, this.offset().move(-2)));
      },
      selfClosing: () => {
        let body = this.#popConstructing(ConstructingStartTag, { assert: false });

        if (!body) {
          const name = this.#popConstructing(ConstructingTagName);
          body = name.finish();
        }

        this.#constructing(ConstructingParent).append(body.selfClosing());
      },
    },
    end: {
      begin: () => {
        this.#begin(new ConstructingEndTag(this));
      },
      finalize: (tag: EndTag) => {
        const start = this.#popConstructing(ConstructingElement, { assert: false });

        if (start) {
          this.#constructing(ConstructingParent).append(start.finish(tag));
        } else {
          this.error('elements.end-without-start-tag', tag.name, tag.span);
        }
      },
    },
  };

  readonly element = {
    modifier: (modifier: ASTv1.MustacheStatement) => {
      this.tag.body();

      if (this.#constructing(ConstructingAttribute, { assert: false })) {
        this.attr.finalize();
      }

      const start = this.#constructing(ConstructingStartTag);

      start.modifier(modifier);
      this.transitionTo('beforeAttributeName');
    },

    comment: (comment: ASTv1.MustacheCommentStatement) => {
      this.#constructing(ConstructingTag).comment(comment);
      this.transitionTo('beforeAttributeName');
    },
  };

  readonly attr = {
    finalize: () => {
      const value = this.#popConstructing(ConstructingAttributeValue, { assert: false });
      const attr = this.#popConstructing(ConstructingAttribute).finish(
        value ? value.finish() : this.builder.emptyAttrValue(this.offset().collapsed())
      );
      this.#constructing(ConstructingStartTag).appendAttribute(attr);

      this.transitionTo('afterAttributeName');
    },

    value: Fn({
      function: (quoted: boolean) =>
        this.#begin(new ConstructingAttributeValue(this), { assert: ConstructingAttribute }).quoted(
          quoted
        ),
      dynamic: (value: ASTv1.MustacheStatement) => {
        this.#constructing(ConstructingAttributeValue).dynamic(value);
      },
    }),

    name: () => {
      this.tag.body();
      this.#begin(new ConstructingAttribute(this));
    },
  };

  #begin<C extends Constructing>(constructing: C, type?: { assert: typeof Constructing }) {
    if (type) {
      this.#assert(type.assert);
    }

    this.#constructingStack.push(constructing);
    return constructing;
  }

  #return<C extends Constructing>(
    constructing: Constructing,
    type: abstract new (...args: any[]) => C
  ): C;
  #return<C extends Constructing>(
    constructing: Constructing,
    type: abstract new (...args: any[]) => C,
    options: { assert: false }
  ): C | null;
  #return<C extends Constructing>(
    constructing: Constructing,
    type: abstract new (...args: any[]) => C,
    options?: { assert: false }
  ): C | null;
  #return<C extends Constructing>(
    constructing: Constructing,
    type: abstract new (...args: any[]) => C,
    options?: { assert: false }
  ): C | null {
    if (constructing instanceof type) {
      return constructing;
    } else if (options?.assert !== false) {
      printTrace(this.#tracer);
      assert(
        false,
        `Expected the parser to be constructing a ${type.name}, but it was constructing a ${constructing.constructor.name}`
      );
    } else {
      return null;
    }
  }

  #assert<C extends Constructing>(
    type: abstract new (...args: any[]) => C,
    constructing = this.#constructingStack.current
  ): C {
    return this.#return(constructing, type);
  }

  in(type: Constructing['type']): boolean {
    return this.#constructingStack.current.type === type;
  }

  #is<C extends Constructing>(
    type: abstract new (...args: any[]) => C,
    constructing = this.#constructingStack.current
  ): constructing is C {
    return constructing instanceof type;
  }

  #constructing<C extends Constructing>(type: abstract new (...args: any[]) => C): C;
  #constructing<C extends Constructing>(
    type: abstract new (...args: any[]) => C,
    options: { assert: false }
  ): C | null;
  #constructing<C extends Constructing>(
    type: abstract new (...args: any[]) => C,
    options?: { assert: false }
  ): C | null;
  #constructing<C extends Constructing>(
    type: abstract new (...args: any[]) => C,
    options?: { assert: false }
  ): C | null {
    return this.#return(this.#constructingStack.current, type, options);
  }

  #popConstructing<C extends Constructing>(type: abstract new (...args: any[]) => C): C;
  #popConstructing<C extends Constructing>(
    type: abstract new (...args: any[]) => C,
    options: { assert: false }
  ): C | null;
  #popConstructing<C extends Constructing>(
    type: abstract new (...args: any[]) => C,
    options?: { assert: false }
  ): C | null {
    const ret = this.#constructing(type, options);

    if (ret !== null) {
      this.#constructingStack.pop();
    }

    return ret;
  }

  #nearest<C extends Constructing>(type: abstract new (...args: any[]) => C): C | null {
    return this.#constructingStack.nearest((item): item is C => item instanceof type) ?? null;
  }

  addChars(char: string) {
    this.#constructingStack.current.addChars(char);
  }

  #pushScope(locals: string[]): void {
    const current = this.builder;
    this.#builderStack.push(current.child(locals));
  }

  #popScope(): void {
    if (this.#builderStack.isEmpty()) {
      throw new Error('unbalanced scopes');
    }

    this.#builderStack.pop();
  }

  assert(condition: any, message: string): asserts condition {
    if (!condition) {
      printTrace(this.#tracer);
    }

    assert(condition, `${message}\n${this.#tracer.print()}`);
  }
}

type AttrPart = ASTv1.TextNode | ASTv1.MustacheStatement;

export abstract class Constructing {
  abstract readonly type:
    | 'block'
    | 'element'
    | 'comment'
    | 'text'
    | 'tag:name'
    | 'tag:start'
    | 'tag:end'
    | 'tag:attr:name'
    | 'tag:attr:value';

  #parser: Parser;
  #start: SourceOffset;

  constructor(parser: Parser, start: SourceOffset = parser.offset()) {
    this.#parser = parser;
    this.#start = start;
  }

  get parser() {
    return this.#parser;
  }

  get b() {
    return this.#parser.builder;
  }

  abstract addChars(chars: string): void;

  span(): SourceSpan {
    return this.#start.withEnd(this.#parser.offset());
  }

  get start(): SourceOffset {
    return this.#start;
  }
}

abstract class ConstructingParent extends Constructing {
  #statements: ASTv1.Statement[] = [];

  append(node: ASTv1.Statement): void {
    this.#statements.push(node);
  }

  get statements() {
    return this.#statements;
  }
}

class ConstructingBlock extends ConstructingParent {
  readonly type = 'block';

  addChars(_char: string): void {
    assert(false, `BUG: unexpected addChars in block`);
  }

  finalize(node: ASTv1.Template | ASTv1.Block) {
    node.body = this.statements;
  }
}

class ConstructingComment extends Constructing {
  readonly type = 'comment';

  #chars = '';

  addChars(char: string) {
    this.#chars += char;
  }

  finish(): ASTv1.CommentStatement {
    return this.b.comment(this.#chars, this.span());
  }
}

class ConstructingText extends Constructing {
  readonly type = 'text';

  #chars = '';

  addChars(chars: string) {
    this.#chars += chars;
  }

  finish(): ASTv1.TextNode {
    return this.b.text({ chars: this.#chars, loc: this.span() });
  }
}

class ConstructingTagName extends Constructing {
  readonly type = 'tag:name';

  #name = '';

  addChars(chars: string) {
    this.#name += chars;
  }

  finish(): ConstructingStartTag {
    return new ConstructingStartTag(this.parser, {
      tag: this.#name,
      span: this.start.withEnd(this.start.move(this.#name.length)),
    });
  }

  get name(): string {
    return this.#name;
  }
}

export class ConstructingElement extends ConstructingParent {
  readonly type = 'element';

  readonly #start: StartTag;

  constructor(parser: Parser, start: StartTag) {
    super(parser);
    this.#start = start;
  }

  get name(): { tag: string; span: SourceSpan } {
    return this.#start.name;
  }

  addChars(_char: string): void {
    assert(false, `BUG: unexpected addChars in element`);
  }

  finish(tag: EndTag): ASTv1.ElementNode {
    tag.verify(this.name.tag);

    return this.#start.finish(this.b, { children: this.statements, end: tag.span });
  }
}

class StartTag {
  constructor(
    readonly name: { tag: string; span: SourceSpan },
    readonly attributes: ASTv1.AttrNode[],
    readonly blockParams: string[],
    readonly modifiers: ASTv1.ElementModifierStatement[],
    readonly comments: ASTv1.MustacheCommentStatement[],
    readonly span: SourceSpan
  ) {}

  selfClosing(b: Phase1Builder): ASTv1.ElementNode {
    return b.element({
      name: this.#name,
      blockParams: this.blockParams,
      attributes: this.attributes,
      modifiers: this.modifiers,
      comments: this.comments,
      loc: this.span,
    });
  }

  void(b: Phase1Builder): ASTv1.ElementNode {
    return b.element({
      name: this.#name,
      children: [],
      blockParams: this.blockParams,
      attributes: this.attributes,
      modifiers: this.modifiers,
      comments: this.comments,
      loc: this.span,
    });
  }

  finish(
    b: Phase1Builder,
    { children, end }: { children: ASTv1.Statement[]; end: SourceSpan }
  ): ASTv1.ElementNode {
    return b.element({
      name: this.#name,
      blockParams: this.blockParams,
      attributes: this.attributes,
      modifiers: this.modifiers,
      comments: this.comments,
      children,
      loc: this.span.extend(end),
    });
  }

  get #name(): ASTv1.ElementName {
    return { type: 'ElementName', name: this.name.tag, loc: this.name.span };
  }
}

abstract class ConstructingTag extends Constructing {
  #comments: ASTv1.MustacheCommentStatement[] = [];

  comment(comment: ASTv1.MustacheCommentStatement) {
    this.#comments.push(comment);
  }

  get comments() {
    return this.#comments;
  }

  abstract readonly name: string;
}

export class ConstructingStartTag extends ConstructingTag {
  readonly type = 'tag:start';

  readonly #name: { tag: string; span: SourceSpan };
  readonly #attributes: ASTv1.AttrNode[] = [];
  readonly #modifiers: ASTv1.ElementModifierStatement[] = [];
  readonly #statements: ASTv1.Statement[] = [];

  constructor(parser: Parser, name: { tag: string; span: SourceSpan }) {
    super(parser, name.span.getStart());
    this.#name = name;
  }

  get name(): string {
    return this.#name.tag;
  }

  modifier(modifier: ASTv1.MustacheStatement) {
    const { path, params, hash, loc } = modifier;

    if (isHBSLiteral(path)) {
      this.parser.error('html.syntax.invalid-literal-modifier', path.value, path.loc);
    } else {
      this.#modifiers.push(this.b.elementModifier({ path, params, hash, loc }));
    }
  }

  addChars(_char: string): void {
    assert(false, `BUG: unexpected addChars in start tag`);
  }

  beginAttribute() {
    return new ConstructingAttribute(this.parser);
  }

  appendAttribute(attr: ASTv1.AttrNode) {
    this.#attributes.push(attr);
  }

  append(statement: ASTv1.Statement) {
    this.#statements.push(statement);
  }

  selfClosing() {
    return this.finish().selfClosing(this.b);
  }

  finish(): StartTag {
    const { attrs, blockParams } = this.#blockParams;

    return new StartTag(
      this.#name,
      attrs,
      blockParams,
      this.#modifiers,
      this.comments,
      this.span()
    );
  }

  get #blockParams(): { attrs: ASTv1.AttrNode[]; blockParams: string[] } {
    const parsedBlockParams = getBlockParams(this.#attributes);

    if (parsedBlockParams.type === 'err') {
      this.parser.reportError(parsedBlockParams.error);
    }

    return parsedBlockParams;
  }
}

class EndTag {
  #parser: Parser;

  constructor(readonly name: string, readonly span: SourceSpan, parser: Parser) {
    this.#parser = parser;
  }

  verify(openTagName: string) {
    if (this.name !== openTagName) {
      this.#parser.error(
        'elements.unbalanced-tags',
        { open: openTagName, close: this.name },
        this.span
      );
      return false;
    }

    return true;
  }
}

export class ConstructingEndTag extends ConstructingTag {
  readonly type = 'tag:end';

  #name = '';

  get name(): string {
    return this.#name;
  }

  addChars(char: string) {
    this.#name += char;
  }

  appendAttribute(attribute: ASTv1.AttrNode) {
    this.parser.error('elements.invalid-attrs-in-end-tag', attribute.loc);
  }

  finish() {
    return new EndTag(this.name, this.span(), this.parser);
  }
}

export class ConstructingAttribute extends Constructing {
  readonly type = 'tag:attr:name';

  #name = '';
  readonly #properties: {
    quoted: boolean;
    dynamic: boolean;
  } = {
    quoted: false,
    dynamic: false,
  };

  mark(property: 'quoted' | 'dynamic', as = true) {
    this.#properties[property] = as;
  }

  addChars(char: string): void {
    this.#name += char;
  }

  finish(value: ASTv1.AttrValue): ASTv1.AttrNode {
    return this.b.attr({
      name: this.#name,
      value,
      loc: this.span(),
    });
  }
}

class ConstructingAttributeValue extends Constructing {
  readonly type = 'tag:attr:value';

  #currentPart: ASTv1.TextNode | null = null;
  #parts: AttrPart[] = [];
  #properties: {
    quoted: boolean;
    dynamic: boolean;
  } = {
    quoted: false,
    dynamic: false,
  };

  dynamic(value: ASTv1.MustacheStatement) {
    if (this.#currentPart) {
      this.finishText();
    }

    this.#properties.dynamic = true;
    this.#parts.push(value);
  }

  quoted(isQuoted = true) {
    this.#properties.quoted = isQuoted;
  }

  beginText() {
    this.#currentPart = null;
  }

  addChars(char: string) {
    const current = this.#currentPart;

    if (current) {
      current.chars += char;
      current.loc = current.loc.withEnd(this.parser.offset());
    } else {
      this.#currentPart = this.b.text({
        chars: char,
        loc: this.#prevChar(char).collapsed(),
      });
    }
  }

  finishText() {
    this.#parts.push(existing(this.#currentPart));
    this.#currentPart = null;
  }

  finish(): ASTv1.AttrValue {
    if (this.#currentPart) {
      this.finishText();
    }

    return this.#assemble(this.span());
  }

  get #lastPart(): AttrPart | null {
    return this.#parts.length === 0 ? null : this.#parts[this.#parts.length - 1];
  }

  #prevChar(char: string): SourceOffset {
    const last = this.#lastPart;

    if (char === '\n') {
      return last ? last.loc.getEnd() : this.start;
    } else {
      return this.parser.offset().move(-1);
    }
  }

  #assemble(span: SourceSpan): ASTv1.AttrValue {
    const { quoted, dynamic } = this.#properties;
    const parts = this.#parts;

    if (dynamic) {
      if (quoted) {
        assert(isPresent(parts), `the concatenation parts of an element should not be empty`);
        return this.b.concat(parts, span);
      } else {
        assert(
          parts.length === 1,
          `an attribute value cannot have more than one dynamic part if it's not concatentated`
        );
        return parts[0];
      }
    } else if (parts.length === 0) {
      return this.b.emptyAttrValue(span);
    } else {
      return {
        ...parts[0],
        loc: span,
      };
    }
  }
}

export function calculateRightStrippedOffsets(original: string, value: string) {
  if (value === '') {
    // if it is empty, just return the count of newlines
    // in original
    return {
      lines: original.split('\n').length - 1,
      columns: 0,
    };
  }

  // otherwise, return the number of newlines prior to
  // `value`
  let difference = original.split(value)[0];
  let lines = difference.split(/\n/);
  let lineCount = lines.length - 1;

  return {
    lines: lineCount,
    columns: lines[lineCount].length,
  };
}

function printTrace(tracer: Tracer) {
  if (LOCAL_DEBUG) {
    console.group('EVENT TRACE');
    console.log(tracer.print());
    console.groupEnd();
  }
}

function Fn<R extends { function: Function }>(def: R): R['function'] & Omit<R, 'function'> {
  const { extracted: fn, rest } = extract(def, 'function');

  return assign(fn, rest);
}
