/* eslint-disable no-console */
import type { TokenizerDelegate } from 'simple-html-tokenizer';
import type { Parser } from './parser';

export class TokenizerEventHandlers implements TokenizerDelegate {
  static create(parser: () => Parser): TokenizerEventHandlers {
    return new TokenizerEventHandlers(parser);
  }

  #parserThunk: () => Parser;

  constructor(parserThunk: () => Parser) {
    this.#parserThunk = parserThunk;
  }

  get #parser(): Parser {
    return this.#parserThunk();
  }

  reportSyntaxError(message: string): void {
    this.#parser.tokenizerError(message);
  }

  reset(): void {
    // do nothing, because we always construct a new Tokenizer when we want to tokenize input
  }

  // Comment

  beginComment(): void {
    this.#parser.traced('beginComment:trace');
    this.#parser.comment.begin();
  }

  appendToCommentData(char: string): void {
    this.#parser.traced('appendToCommentData:trace', char);
    this.#parser.addChars(char);
  }

  finishComment(): void {
    this.#parser.traced('finishComment:trace');
    this.#parser.comment.finalize();
  }

  // Data

  beginData(): void {
    this.#parser.traced('beginData:trace');
    this.#parser.text.begin();
  }

  appendToData(char: string): void {
    this.#parser.traced('appendToData:trace', char);
    this.#parser.addChars(char);
  }

  finishData(): void {
    this.#parser.traced('finishData:trace');
    this.#parser.text.finalize();
  }

  // Tags - basic

  tagOpen(): void {
    this.#parser.traced('tagOpen:trace');
  }

  beginStartTag(): void {
    this.#parser.traced('beginStartTag:trace');
    this.#parser.tag.start.begin();
  }

  beginEndTag(): void {
    this.#parser.traced('beginEndTag:trace');
    this.#parser.tag.end.begin();
  }

  finishTag(): void {
    this.#parser.traced('finishTag:trace');

    this.#parser.tag.finalize();
  }

  markTagAsSelfClosing(): void {
    this.#parser.traced('markTagAsSelfClosing:trace');
    this.#parser.tag.start.selfClosing();
  }

  // Tags - name

  appendToTagName(char: string): void {
    this.#parser.traced('appendToTagName:trace', char);
    this.#parser.addChars(char);
  }

  // Tags - attributes

  beginAttribute(): void {
    this.#parser.traced('beginAttribute:trace');
    this.#parser.attr.name();
  }

  appendToAttributeName(char: string): void {
    this.#parser.traced(`appendToAttributeName:trace`, char);
    this.#parser.addChars(char);
  }

  beginAttributeValue(isQuoted: boolean): void {
    this.#parser.traced(`beginAttributeValue:trace`, { quoted: isQuoted });
    this.#parser.attr.value(isQuoted);
  }

  appendToAttributeValue(char: string): void {
    this.#parser.traced(`appendToAttributeValue:trace`, char);
    this.#parser.addChars(char);
  }

  finishAttributeValue(): void {
    this.#parser.traced('finishAttributeValue:trace');
    this.#parser.attr.finalize();
  }
}
