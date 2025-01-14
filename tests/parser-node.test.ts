import {
  Buildersv1,
  GlimmerSyntaxError,
  ParserState,
  preprocess as parse,
  preprocess,
  type ASTv1,
  type Dict,
  type SyntaxErrorArgs,
} from '@glimmer/syntax';
import { describe, expect, test } from 'vitest';
import type { PresentArray } from '../src/lib/utils/array';
import { astEqual } from './support';
import { AnnotatedSource } from './support/annotated.js';

const b = Buildersv1.forModule('', 'test-module');

function syntaxError(source: string, error: Extract<SyntaxErrorArgs, string>): void;
function syntaxError<K extends Extract<SyntaxErrorArgs, unknown[]>[0]>(
  source: string,
  name: K,
  arg: Extract<SyntaxErrorArgs, [K, any]>[1]
): void;
function syntaxError(source: string, error: string, args?: unknown) {
  const annotated = AnnotatedSource.from(source);
  const result = preprocess.normalized(annotated.source, annotated.options);

  expect(result.errors, 'errors').toSatisfy(
    (errors: PresentArray<GlimmerSyntaxError> | undefined) => {
      return errors !== undefined && errors.length === 1;
    },
    'expected exactly one syntax error'
  );

  const actualError: GlimmerSyntaxError = (result.errors as [GlimmerSyntaxError])[0];

  let expectedError: GlimmerSyntaxError;
  if (args === undefined) {
    expectedError = GlimmerSyntaxError.create(error as SyntaxErrorArgs, annotated.span);
  } else {
    expectedError = GlimmerSyntaxError.create([error, args] as SyntaxErrorArgs, annotated.span);
  }

  expect(actualError.message, 'error message').toEqual(expectedError.message);
}

describe('Parser - AST', () => {
  test('a simple piece of content', () => {
    let t = 'some content';

    astEqual(t, b.program([b.text('some content')]));
  });

  test('self-closed element', () => {
    let t = '<g />';
    astEqual(t, b.program([element('g/')]));
  });

  test('elements can have empty attributes', () => {
    let t = '<img id="">';
    astEqual(t, b.program([element('img', ['attrs', ['id', '']])]));
  });

  test('disallowed quote in element space is corrected', () => {
    astEqual(
      `<img foo="bar"" >`,
      b.program([element('img', ['attrs', ['foo', 'bar'], ['"', '']])])
    );
  });

  test('disallowed equals sign in element space is rejected', () => {
    syntaxError(
      '<img |-><-|=foo >',
      'passthrough.tokenizer',
      `attribute name cannot start with equals sign`
    );
  });

  test('svg content', () => {
    let t = '<svg></svg>';
    astEqual(t, b.program([element('svg')]));
  });

  test('svg content with attributes', () => {
    let t = '<svg viewBox="0 0 100 100"></svg>';
    astEqual(t, b.program([element('svg', ['attrs', ['viewBox', '0 0 100 100']])]));
  });

  test('html content with html content inline', () => {
    let t = '<div><p></p></div>';
    astEqual(t, b.program([element('div', ['body', element('p')])]));
  });

  test('html content with svg content inline', () => {
    let t = '<div><svg></svg></div>';
    astEqual(t, b.program([element('div', ['body', element('svg')])]));
  });

  let integrationPoints = ['foreignObject', 'desc'];
  function buildIntegrationPointTest(integrationPoint: string) {
    return function integrationPointTest() {
      let t = '<svg><' + integrationPoint + '><div></div></' + integrationPoint + '></svg>';
      astEqual(
        t,
        b.program([element('svg', ['body', element(integrationPoint, ['body', element('div')])])])
      );
    };
  }
  for (let i = 0, length = integrationPoints.length; i < length; i++) {
    test(
      'svg content with html content inline for ' + integrationPoints[i],
      buildIntegrationPointTest(integrationPoints[i])
    );
  }

  test('svg title with html content', () => {
    let t = '<svg><title><div></div></title></svg>';
    astEqual(
      t,
      b.program([element('svg', ['body', element('title', ['body', b.text('<div></div>')])])])
    );
  });

  test('a piece of content with HTML', () => {
    let t = 'some <div>content</div> done';
    astEqual(
      t,
      b.program([b.text('some '), element('div', ['body', b.text('content')]), b.text(' done')])
    );
  });

  test('a piece of Handlebars with HTML', () => {
    let t = 'some <div>{{content}}</div> done';
    astEqual(
      t,
      b.program([
        b.text('some '),
        element('div', ['body', b.mustache(b.path('content'))]),
        b.text(' done'),
      ])
    );
  });

  test('Handlebars embedded in an attribute (quoted)', () => {
    let t = 'some <div class="{{foo}}">content</div> done';
    astEqual(
      t,
      b.program([
        b.text('some '),
        element(
          'div',
          ['attrs', ['class', b.concat([b.mustache('foo')])]],
          ['body', b.text('content')]
        ),
        b.text(' done'),
      ])
    );
  });

  test('Handlebars embedded in an attribute (unquoted)', () => {
    let t = 'some <div class={{foo}}>content</div> done';
    astEqual(
      t,
      b.program([
        b.text('some '),
        element(
          'div',
          ['attrs', ['class', b.mustache(b.path('foo'))]],
          ['body', b.text('content')]
        ),
        b.text(' done'),
      ])
    );
  });

  test('Handlebars embedded in an attribute of a self-closing tag (unqouted)', () => {
    let t = '<input value={{foo}}/>';

    let el = element('input/', ['attrs', ['value', b.mustache(b.path('foo'))]]);
    astEqual(t, b.program([el]));
  });

  test('Handlebars embedded in an attribute (sexprs)', () => {
    let t = 'some <div class="{{foo (foo "abc")}}">content</div> done';
    astEqual(
      t,
      b.program([
        b.text('some '),
        element(
          'div',
          [
            'attrs',
            [
              'class',
              b.concat([b.mustache(b.path('foo'), [b.sexpr(b.path('foo'), [b.string('abc')])])]),
            ],
          ],
          ['body', b.text('content')]
        ),
        b.text(' done'),
      ])
    );
  });

  test('Handlebars embedded in an attribute with other content surrounding it', () => {
    let t = 'some <a href="http://{{link}}/">content</a> done';
    astEqual(
      t,
      b.program([
        b.text('some '),
        element(
          'a',
          ['attrs', ['href', b.concat([b.text('http://'), b.mustache('link'), b.text('/')])]],
          ['body', b.text('content')]
        ),
        b.text(' done'),
      ])
    );
  });

  test('A more complete embedding example', () => {
    let t =
      "{{embed}} {{some 'content'}} " +
      "<div class='{{foo}} {{bind-class isEnabled truthy='enabled'}}'>{{ content }}</div>" +
      " {{more 'embed'}}";
    astEqual(
      t,
      b.program([
        b.mustache(b.path('embed')),
        b.text(' '),
        b.mustache(b.path('some'), [b.string('content')]),
        b.text(' '),
        element(
          'div',
          [
            'attrs',
            [
              'class',
              b.concat([
                b.mustache('foo'),
                b.text(' '),
                b.mustache(
                  'bind-class',
                  [b.path('isEnabled')],
                  b.hash([b.pair('truthy', b.string('enabled'))])
                ),
              ]),
            ],
          ],
          ['body', b.mustache(b.path('content'))]
        ),
        b.text(' '),
        b.mustache(b.path('more'), [b.string('embed')]),
      ])
    );
  });

  test('Simple embedded block helpers', () => {
    let t = '{{#if foo}}<div>{{content}}</div>{{/if}}';
    astEqual(
      t,
      b.program([
        b.block(
          b.path('if'),
          [b.path('foo')],
          b.hash(),
          b.blockItself([element('div', ['body', b.mustache(b.path('content'))])])
        ),
      ])
    );
  });

  test('Involved block helper', () => {
    let t =
      '<p>hi</p> content {{#testing shouldRender}}<p>Appears!</p>{{/testing}} more <em>content</em> here';
    astEqual(
      t,
      b.program([
        element('p', ['body', b.text('hi')]),
        b.text(' content '),
        b.block(
          b.path('testing'),
          [b.path('shouldRender')],
          b.hash(),
          b.blockItself([element('p', ['body', b.text('Appears!')])])
        ),
        b.text(' more '),
        element('em', ['body', b.text('content')]),
        b.text(' here'),
      ])
    );
  });

  test('Element modifiers', () => {
    let t = "<p {{action 'boom'}} class='bar'>Some content</p>";
    astEqual(
      t,
      b.program([
        element(
          'p',
          ['attrs', ['class', 'bar']],
          ['modifiers', ['action', [b.string('boom')]]],
          ['body', b.text('Some content')]
        ),
      ])
    );
  });

  test('Tokenizer: MustacheStatement encountered in beforeAttributeName state', () => {
    let t = '<input {{bar}}>';
    astEqual(t, b.program([element('input', ['modifiers', 'bar'])]));
  });

  test('Tokenizer: MustacheStatement encountered in attributeName state', () => {
    let t = '<input foo{{bar}}>';
    astEqual(t, b.program([element('input', ['attrs', ['foo', '']], ['modifiers', ['bar']])]));
  });

  test('Tokenizer: MustacheStatement encountered in afterAttributeName state', () => {
    let t = '<input foo {{bar}}>';
    astEqual(t, b.program([element('input', ['attrs', ['foo', '']], ['modifiers', 'bar'])]));
  });

  test('Tokenizer: MustacheStatement encountered in afterAttributeValue state', () => {
    let t = '<input foo=1 {{bar}}>';
    astEqual(t, b.program([element('input', ['attrs', ['foo', '1']], ['modifiers', ['bar']])]));
  });

  test('Tokenizer: MustacheStatement encountered in afterAttributeValueQuoted state', () => {
    let t = "<input foo='1'{{bar}}>";
    astEqual(t, b.program([element('input', ['attrs', ['foo', '1']], ['modifiers', 'bar'])]));
  });

  test('Stripping - mustaches', () => {
    let t = 'foo {{~content}} bar';
    astEqual(
      t,
      b.program([
        b.text('foo'),
        b.mustache(b.path('content'), undefined, undefined, undefined, undefined, {
          open: true,
          close: false,
        }),
        b.text(' bar'),
      ])
    );

    // t = 'foo {{content~}} bar';
    // astEqual(
    //   t,
    //   b.program([
    //     b.text('foo '),
    //     b.mustache(b.path('content'), undefined, undefined, undefined, undefined, {
    //       open: false,
    //       close: true,
    //     }),
    //     b.text('bar'),
    //   ])
    // );
  });

  test('Stripping - blocks', () => {
    let t = 'foo {{~#wat}}{{/wat}} bar';
    astEqual(
      t,
      b.program([
        b.text('foo'),
        b.block(b.path('wat'), [], b.hash(), b.blockItself(), undefined, undefined, {
          open: true,
          close: false,
        }),
        b.text(' bar'),
      ])
    );

    t = 'foo {{#wat}}{{/wat~}} bar';
    astEqual(
      t,
      b.program([
        b.text('foo '),
        b.block(
          b.path('wat'),
          [],
          b.hash(),
          b.blockItself(),
          undefined,
          undefined,
          undefined,
          undefined,
          { open: false, close: true }
        ),
        b.text('bar'),
      ])
    );
  });

  test('Stripping - programs', () => {
    let t = '{{#wat~}} foo {{else}}{{/wat}}';
    astEqual(
      t,
      b.program([
        b.block(
          b.path('wat'),
          [],
          b.hash(),
          b.blockItself([b.text('foo ')]),
          b.blockItself(),
          undefined,
          { open: false, close: true }
        ),
      ])
    );

    t = '{{#wat}} foo {{~else}}{{/wat}}';
    astEqual(
      t,
      b.program([
        b.block(
          b.path('wat'),
          [],
          b.hash(),
          b.blockItself([b.text(' foo')]),
          b.blockItself(),
          undefined,
          undefined,
          { open: true, close: false }
        ),
      ])
    );

    t = '{{#wat}}{{else~}} foo {{/wat}}';
    astEqual(
      t,
      b.program([
        b.block(
          b.path('wat'),
          [],
          b.hash(),
          b.blockItself(),
          b.blockItself([b.text('foo ')]),
          undefined,
          undefined,
          { open: false, close: true }
        ),
      ])
    );

    t = '{{#wat}}{{else}} foo {{~/wat}}';
    astEqual(
      t,
      b.program([
        b.block(
          b.path('wat'),
          [],
          b.hash(),
          b.blockItself(),
          b.blockItself([b.text(' foo')]),
          undefined,
          undefined,
          undefined,
          { open: true, close: false }
        ),
      ])
    );
  });

  test('Stripping - removes unnecessary text nodes', () => {
    let t = '{{#each~}}\n  <li> foo </li>\n{{~/each}}';

    astEqual(
      t,
      b.program([
        b.block(
          b.path('each'),
          [],
          b.hash(),
          b.blockItself([element('li', ['body', b.text(' foo ')])]),
          null,
          undefined,
          { open: false, close: true },
          undefined,
          { open: true, close: false }
        ),
      ])
    );
  });

  test('Whitespace control - linebreaks after blocks removed by default', () => {
    let t = '{{#each}}\n  <li> foo </li>\n{{/each}}';

    astEqual(
      t,
      b.program([
        b.block(
          b.path('each'),
          [],
          b.hash(),
          b.blockItself([b.text('  '), element('li', ['body', b.text(' foo ')]), b.text('\n')]),
          null
        ),
      ])
    );
  });

  test('Whitespace control - preserve all whitespace if config is set', () => {
    let t = '{{#each}}\n  <li> foo </li>\n{{/each}}';

    astEqual(
      t,
      b.program([
        b.block(
          b.path('each'),
          [],
          b.hash(),
          b.blockItself([b.text('\n  '), element('li', ['body', b.text(' foo ')]), b.text('\n')]),
          null
        ),
      ]),
      undefined,
      {
        parseOptions: { ignoreStandalone: true },
      }
    );
  });

  // TODO: Make these throw an error.
  //test("Awkward mustache in unquoted attribute value", function() {
  //  let t = "<div class=a{{foo}}></div>";
  //  astEqual(t, b.program([
  //    element('div', [ b.attr('class', concat([b.string("a"), b.sexpr([b.path('foo')])])) ])
  //  ]));
  //
  //  t = "<div class=a{{foo}}b></div>";
  //  astEqual(t, b.program([
  //    element('div', [ b.attr('class', concat([b.string("a"), b.sexpr([b.path('foo')]), b.string("b")])) ])
  //  ]));
  //
  //  t = "<div class={{foo}}b></div>";
  //  astEqual(t, b.program([
  //    element('div', [ b.attr('class', concat([b.sexpr([b.path('foo')]), b.string("b")])) ])
  //  ]));
  //});

  test('an HTML comment', () => {
    let t = 'before <!-- some comment --> after';
    astEqual(t, b.program([b.text('before '), b.comment(' some comment '), b.text(' after')]));
  });

  test('a Handlebars comment inside an HTML comment', () => {
    let t = 'before <!-- some {{! nested thing }} comment --> after';
    astEqual(
      t,
      b.program([
        b.text('before '),
        b.comment(' some {{! nested thing }} comment '),
        b.text(' after'),
      ])
    );
  });

  test('a Handlebars comment', () => {
    let t = 'before {{! some comment }} after';
    astEqual(
      t,
      b.program([b.text('before '), b.mustacheComment(' some comment '), b.text(' after')])
    );
  });

  test('a Handlebars comment in proper element space', () => {
    let t = 'before <div {{! some comment }} data-foo="bar" {{! other comment }}></div> after';
    astEqual(
      t,
      b.program([
        b.text('before '),
        element(
          'div',
          ['attrs', ['data-foo', b.text('bar')]],
          ['comments', b.mustacheComment(' some comment '), b.mustacheComment(' other comment ')]
        ),
        b.text(' after'),
      ])
    );
  });

  test('a Handlebars comment after a valueless attribute', () => {
    let t = '<input foo {{! comment }}>';
    astEqual(
      t,
      b.program([
        element('input', ['attrs', ['foo', '']], ['comments', b.mustacheComment(' comment ')]),
      ])
    );
  });

  test('a Handlebars comment in invalid element space', () => {
    syntaxError(
      `\nbefore <div \n  a|->{{! some comment }}<-| data-foo="bar"></div> after`,
      'html.syntax.invalid-hbs-comment',
      ParserState.AttrName
    );

    syntaxError(
      `\nbefore <div \n  a=|->{{! some comment }}<-| data-foo="bar"></div> after`,
      'html.syntax.invalid-hbs-comment',
      ParserState.AttrValue
    );

    syntaxError(
      `\nbefore <div \n  a=|->{{! some comment }}<-| data-foo="bar"></div> after`,
      'html.syntax.invalid-hbs-comment',
      ParserState.AttrValue
    );

    syntaxError(
      `\nbefore <div \n  a="|->{{! some comment }}<-|" data-foo="bar"></div> after`,
      'html.syntax.invalid-hbs-comment',
      ParserState.AttrValue
    );
  });

  test('allow {{null}} to be passed as helper name', () => {
    let ast = parse('{{null}}');

    astEqual(ast, b.program([b.mustache(b.null())]));
  });

  test('allow {{null}} to be passed as a param', () => {
    let ast = parse('{{foo null}}');

    astEqual(ast, b.program([b.mustache(b.path('foo'), [b.null()])]));
  });

  test('allow {{undefined}} to be passed as helper name', () => {
    let ast = parse('{{undefined}}');

    astEqual(ast, b.program([b.mustache(b.undefined())]));
  });

  test('allow {{undefined}} to be passed as a param', () => {
    let ast = parse('{{foo undefined}}');

    astEqual(ast, b.program([b.mustache(b.path('foo'), [b.undefined()])]));
  });

  test('Handlebars partial should error', () => {
    syntaxError(`|->{{> foo}}<-|`, 'hbs.syntax.unsupported-construct', 'Partial');
  });

  test('Handlebars partial block should error', () => {
    syntaxError(`|->{{#> foo}}{{/foo}}<-|`, 'hbs.syntax.unsupported-construct', 'PartialBlock');
  });

  test('Handlebars decorator should error', () => {
    syntaxError(`|->{{* foo}}<-|`, 'hbs.syntax.unsupported-construct', 'Decorator');
  });

  test('Handlebars decorator block should error', () => {
    syntaxError(`|->{{#* foo}}{{/foo}}<-|`, 'hbs.syntax.unsupported-construct', 'DecoratorBlock');
  });

  test('disallowed mustaches in the tagName space', () => {
    syntaxError(
      `<|->{{"asdf"}}<-|></{{"asdf"}}>`,
      'html.syntax.invalid-hbs-curly',
      ParserState.TagName
    );
    syntaxError(`<input|->{{bar}}<-|>`, 'html.syntax.invalid-hbs-curly', ParserState.TagName);
  });

  test('mustache immediately followed by self closing tag does not error', () => {
    let ast = parse('<FooBar data-foo={{blah}}/>');
    let el = element('FooBar/', ['attrs', ['data-foo', b.mustache('blah')]]);
    astEqual(ast, b.program([el]));
  });

  test('named blocks', () => {
    let ast = parse(strip`
    <Tab>
      <:header>
        It's a header!
      </:header>

      <:body as |contents|>
        <div>{{contents}}</div>
      </:body>
    </Tab>
  `);

    let el = element('Tab', [
      'body',
      element(':header', ['body', b.text(`It's a header!`)]),
      element(
        ':body',
        ['body', element('div', ['body', b.mustache('contents')])],
        ['as', 'contents']
      ),
    ]);
    astEqual(ast, b.program([el]));
  });

  test('path expression with "dangling dot" throws error', () => {
    syntaxError(`{{if foo|->.<-| bar baz}}`, 'hbs.syntax.invalid-dot');
  });

  test('string literal as path throws error', () => {
    syntaxError(`{{(|->"foo-baz"<-|)}}`, 'hbs.syntax.not-callable', {
      type: 'string',
      original: `foo-baz`,
    });
  });

  test('boolean literal as path throws error', () => {
    syntaxError(`{{(|->true<-|)}}`, 'hbs.syntax.not-callable', { type: 'boolean', original: true });
  });

  test('undefined literal as path throws error', () => {
    syntaxError(`{{(|->undefined<-|)}}`, 'hbs.syntax.not-callable', { type: 'undefined' });
  });

  test('null literal as path throws error', () => {
    syntaxError(`{{(|->null<-|)}}`, 'hbs.syntax.not-callable', { type: 'null' });
  });

  test('number literal as path throws error', () => {
    syntaxError(`{{(|->42<-|)}}`, 'hbs.syntax.not-callable', { type: 'number', original: 42 });
  });
});

export function strip(strings: TemplateStringsArray, ...args: string[]) {
  return strings
    .map((str: string, i: number) => {
      return `${str
        .split('\n')
        .map((s) => s.trim())
        .join('')}${args[i] ? args[i] : ''}`;
    })
    .join('');
}

export type ElementParts =
  | ['attrs', ...AttrSexp[]]
  | ['modifiers', ...ModifierSexp[]]
  | ['body', ...ASTv1.Statement[]]
  | ['comments', ...ElementComment[]]
  | ['as', ...string[]]
  | ['loc', ASTv1.SourceLocation];

export type PathSexp = string | ['path', string, LocSexp?];

export type ModifierSexp =
  | string
  | [PathSexp, LocSexp?]
  | [PathSexp, ASTv1.Expression[], LocSexp?]
  | [PathSexp, ASTv1.Expression[], Dict<ASTv1.Expression>, LocSexp?];

export type AttrSexp = [string, ASTv1.AttrNode['value'] | string, LocSexp?];

export type LocSexp = ['loc', ASTv1.SourceLocation];

export type ElementComment = ASTv1.MustacheCommentStatement | ASTv1.SourceLocation | string;

export type SexpValue =
  | string
  | ASTv1.Expression[]
  | Dict<ASTv1.Expression>
  | LocSexp
  | PathSexp
  | undefined;

export interface BuildElementOptions {
  attrs?: ASTv1.AttrNode[];
  modifiers?: ASTv1.ElementModifierStatement[];
  children?: ASTv1.Statement[];
  comments?: ElementComment[];
  blockParams?: string[];
  loc?: ASTv1.SourceLocation;
}

export type TagDescriptor = string | { name: string; selfClosing: boolean };

export function element(tag: TagDescriptor, ...options: ElementParts[]): ASTv1.ElementNode {
  let normalized: BuildElementOptions;
  if (Array.isArray(options)) {
    normalized = normalizeElementParts(...options);
  } else {
    normalized = options || {};
  }

  let { attrs, blockParams, modifiers, comments, children, loc } = normalized;

  // this is used for backwards compat, prior to `selfClosing` being part of the ElementNode AST
  let selfClosing = false;
  if (typeof tag === 'object') {
    selfClosing = tag.selfClosing;
    tag = tag.name;
  } else {
    if (tag.slice(-1) === '/') {
      tag = tag.slice(0, -1);
      selfClosing = true;
    }
  }

  const start = b.loc(loc).getStart();

  return {
    type: 'ElementNode',
    tag: tag || '',
    name: {
      type: 'ElementName',
      name: tag,
      loc: start.move(1).withEnd(start.move(tag.length + 1)),
    },
    selfClosing: selfClosing,
    attributes: attrs || [],
    blockParams: blockParams || [],
    modifiers: modifiers || [],
    comments: (comments as ASTv1.MustacheCommentStatement[]) || [],
    children: children || [],
    loc: b.loc(loc || null),
  };
}

export function normalizeElementParts(...args: ElementParts[]): BuildElementOptions {
  let out: BuildElementOptions = {};

  for (let arg of args) {
    switch (arg[0]) {
      case 'attrs': {
        let [, ...rest] = arg;
        out.attrs = rest.map(normalizeAttr);
        break;
      }
      case 'modifiers': {
        let [, ...rest] = arg;
        out.modifiers = rest.map(normalizeModifier);
        break;
      }
      case 'body': {
        let [, ...rest] = arg;
        out.children = rest;
        break;
      }
      case 'comments': {
        let [, ...rest] = arg;

        out.comments = rest;
        break;
      }
      case 'as': {
        let [, ...rest] = arg;
        out.blockParams = rest;
        break;
      }
      case 'loc': {
        let [, rest] = arg;
        out.loc = rest;
        break;
      }
    }
  }

  return out;
}

export function normalizeAttr(sexp: AttrSexp): ASTv1.AttrNode {
  let name = sexp[0];
  let value;

  if (typeof sexp[1] === 'string') {
    value = b.text(sexp[1]);
  } else {
    value = sexp[1];
  }

  return b.attr(name, value);
}

export function normalizeModifier(sexp: ModifierSexp): ASTv1.ElementModifierStatement {
  if (typeof sexp === 'string') {
    return b.elementModifier(sexp);
  }

  let path: ASTv1.Expression = normalizeHead(sexp[0]);
  let params: ASTv1.Expression[] | undefined;
  let hash: ASTv1.Hash | undefined;
  let loc: ASTv1.SourceLocation | null = null;

  let parts = sexp.slice(1);
  let next = parts.shift();

  _process: {
    if (isParamsSexp(next)) {
      params = next;
    } else {
      break _process;
    }

    next = parts.shift();

    if (isHashSexp(next)) {
      hash = normalizeHash(next);
    } else {
      break _process;
    }
  }

  if (isLocSexp(next)) {
    loc = next[1];
  }

  return {
    type: 'ElementModifierStatement',
    path,
    params: params || [],
    hash: hash || b.hash([]),
    loc: b.loc(loc || null),
  };
}

export function normalizeHead(path: PathSexp): ASTv1.Expression {
  if (typeof path === 'string') {
    return b.path(path);
  } else {
    return b.path(path[1], path[2] && path[2][1]);
  }
}

export function normalizeHash(
  hash: Dict<ASTv1.Expression>,
  loc?: ASTv1.SourceLocation
): ASTv1.Hash {
  let pairs: ASTv1.HashPair[] = [];

  Object.keys(hash).forEach((key) => {
    pairs.push(b.pair(key, hash[key]));
  });

  return b.hash(pairs, loc);
}

export function isParamsSexp(value: SexpValue): value is ASTv1.Expression[] {
  return Array.isArray(value) && !isLocSexp(value);
}

export function isLocSexp(value: SexpValue): value is LocSexp {
  return Array.isArray(value) && value.length === 2 && value[0] === 'loc';
}

export function isHashSexp(value: SexpValue): value is Dict<ASTv1.Expression> {
  if (typeof value === 'object' && value && !Array.isArray(value)) {
    expectType<Dict<ASTv1.Expression>>(value);
    return true;
  } else {
    return false;
  }
}

function expectType<T>(_input: T): void {
  return;
}
