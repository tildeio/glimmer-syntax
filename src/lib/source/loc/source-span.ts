import { DEBUG } from '@glimmer/env';

import type { PresentArray } from '../../utils/array';
import type { SourceLocation, SourcePosition } from '../../v1/handlebars-ast';
import { SourceSlice } from '../slice';
import type { SourceTemplate } from '../source';
import { format, FormatSpan } from './format';
import type { BrokenPosition } from './offset';
import { SourceOffset } from './offset';
import { type SerializedSourceSpan, serializeBroken, serializeOffsets } from './serialize';

export interface SpanInterface {
  readonly module: string;
  readonly startPosition: SourcePosition;
  readonly endPosition: SourcePosition;
  readonly describe: string;

  getTemplate(): SourceTemplate;

  toAST(): SourceLocation;
  toSlice(expected?: string): SourceSlice;
  serialize(): SerializedSourceSpan;

  verify(): boolean;

  getStart(): SourceOffset | BrokenPosition;
  withStart(start: SourceOffset): SourceSpan;
  getEnd(): SourceOffset | BrokenPosition;
  withEnd(end: SourceOffset): SourceSpan;

  asString(): string;
  asAnnotatedString(): string;

  collapse(where: 'start' | 'end'): SourceSpan;
  extend(other: SourceSpan): SourceSpan;

  slice(options: { skipStart?: number; skipEnd?: number }): SourceSpan;
  sliceStartChars(options: { skipStart?: number; chars: number }): SourceSpan;
  sliceEndChars(options: { skipEnd?: number; chars: number }): SourceSpan;
  splitAt(
    options: { fromStart: number } | { fromEnd: number }
  ): [first: SourceSpan, second: SourceSpan];
}

interface SpanData {
  readonly template: SourceTemplate;
  readonly offsets: {
    start: SourceOffset | BrokenPosition;
    end: SourceOffset | BrokenPosition;
  };
}

export abstract class SourceSpan implements SpanInterface, SourceLocation {
  static from(data: SpanData): SourceSpan {
    return new ConcreteSpan(data);
  }

  static loc(template: SourceTemplate, ast: SourceLocation): SourceSpan {
    return new ConcreteSpan({
      template,
      offsets: {
        start: SourceOffset.pos(template, ast.start),
        end: SourceOffset.pos(template, ast.end),
      },
    });
  }

  static synthetic(
    template: SourceTemplate,
    offsets: { start: SourceOffset; end: SourceOffset },
    string: string
  ): SourceSpan {
    return new SyntheticSpan(template, offsets, string);
  }

  readonly #data: SpanData;
  readonly #overrides: {
    readonly start: SourceOffset | BrokenPosition | null;
    readonly end: SourceOffset | BrokenPosition | null;
  };

  constructor(
    data: SpanData,
    overrides?: { start?: SourceOffset | BrokenPosition; end?: SourceOffset | BrokenPosition }
  ) {
    this.#data = data;
    this.#overrides = {
      start: overrides?.start ?? null,
      end: overrides?.end ?? null,
    };
  }

  get source(): string {
    return this.#data.template.module;
  }

  get start(): SourcePosition {
    return this.getStart().toAST();
  }

  get end(): SourcePosition {
    return this.getEnd().toAST();
  }

  abstract withStart(start: SourceOffset | BrokenPosition): SourceSpan;
  abstract withEnd(end: SourceOffset | BrokenPosition): SourceSpan;

  abstract asString(): string;
  abstract asAnnotatedString(): string;

  abstract extend(other: SourceSpan): SourceSpan;

  abstract verify(): boolean;

  get describe() {
    const loc = this.toAST();
    return `${this.module}@${loc.start.line}:${loc.start.column}-${loc.end.line}:${loc.end.column}`;
  }

  toAST(): SourceLocation {
    return {
      source: this.#data.template.module,
      start: this.#data.offsets.start.toAST(),
      end: this.#data.offsets.end.toAST(),
    };
  }

  toJSON(): SourceLocation {
    return this.toAST();
  }

  getTemplate(): SourceTemplate {
    return this.#data.template;
  }

  get offsets(): { start: number; end: number } | null {
    const start = this.getStart().offset;
    const end = this.getEnd().offset;

    if (start !== null && end !== null && end >= start) {
      return { start, end };
    } else {
      return null;
    }
  }

  get startPosition(): SourcePosition {
    return this.getStart().toAST();
  }

  get endPosition(): SourcePosition {
    return this.getEnd().toAST();
  }

  get module() {
    return this.#data.template.module;
  }

  getStart(): SourceOffset | BrokenPosition {
    return this.#overrides.start ?? this.#data.offsets.start;
  }

  getEnd(): SourceOffset | BrokenPosition {
    return this.#overrides.end ?? this.#data.offsets.end;
  }

  collapse(where: 'start' | 'end'): SourceSpan {
    switch (where) {
      case 'start':
        return this.withEnd(this.getStart());
      case 'end':
        return this.withStart(this.getEnd());
    }
  }

  slice({ skipStart = 0, skipEnd = 0 }: { skipStart?: number; skipEnd?: number }): SourceSpan {
    return this.#span(this.getStart().move(skipStart), this.getEnd().move(-skipEnd));
  }

  sliceStartChars({ skipStart = 0, chars }: { skipStart?: number; chars: number }): SourceSpan {
    return this.#span(this.getStart().move(skipStart), this.getStart().move(skipStart + chars));
  }

  sliceEndChars({ skipEnd = 0, chars }: { skipEnd?: number; chars: number }): SourceSpan {
    return this.#span(this.getEnd().move(skipEnd - chars), this.getEnd().move(-skipEnd));
  }

  splitAt(
    options: { fromStart: number } | { fromEnd: number }
  ): [first: SourceSpan, second: SourceSpan] {
    if ('fromStart' in options) {
      return [
        this.sliceStartChars({ chars: options.fromStart }),
        this.slice({ skipStart: options.fromStart }),
      ];
    } else {
      return [
        this.slice({ skipEnd: options.fromEnd }),
        this.sliceEndChars({ chars: options.fromEnd }),
      ];
    }
  }

  #span(start: SourceOffset | BrokenPosition, end: SourceOffset | BrokenPosition): SourceSpan {
    return SourceSpan.from({
      template: this.#data.template,
      offsets: {
        start,
        end,
      },
    });
  }

  /**
   * Convert this `SourceSpan` into a `SourceSlice`. In debug mode, this method optionally checks
   * that the byte offsets represented by this `SourceSpan` actually correspond to the expected
   * string.
   */
  toSlice(expected?: string): SourceSlice {
    let chars = this.asString();

    if (DEBUG) {
      if (expected !== undefined && chars !== expected) {
        // eslint-disable-next-line no-console
        console.warn(
          `unexpectedly found ${JSON.stringify(
            chars
          )} when slicing source, but expected ${JSON.stringify(expected)}`
        );
      }
    }

    return new SourceSlice({
      loc: this,
      chars: expected || chars,
    });
  }

  serialize(): SerializedSourceSpan {
    const {
      start: { offset: start },
      end: { offset: end },
    } = this.#data.offsets;

    if (start && end) {
      return serializeOffsets({ start, end });
    } else {
      return serializeBroken({ start: this.getStart().toAST(), end: this.getEnd().toAST() });
    }
  }
}

// export class ConcreteSpan extends SourceSpan implements SourceLocation {
//   #start: SourceOffset;
//   #end: SourceOffset;

//   constructor(readonly template: SourceTemplate, start: SourceOffset, end: SourceOffset) {
//     super({ template, offsets: { start, end } });
//     this.#start = start;
//     this.#end = end;
//   }

//   verify(): boolean {
//     return this.#offsets !== null;
//   }

//   getTemplate(): SourceTemplate {
//     return this.template;
//   }

//   get module(): string {
//     return this.template.module;
//   }

//   /**
//    * Create a new span with the current span's end and a new beginning.
//    */
//   withStart(start: SourceOffset): ConcreteSpan {
//     return new ConcreteSpan(this.template, start, this.#end);
//   }

//   /**
//    * Create a new span with the current span's beginning and a new ending.
//    */
//   withEnd(end: SourceOffset): ConcreteSpan {
//     return new ConcreteSpan(this.template, this.#start, end);
//   }

//   get #offsets(): { start: number; end: number } | null {
//     const start = this.#start.offset;
//     const end = this.#end.offset;

//     if (start === null || end === null || start > end) {
//       return null;
//     }

//     return { start, end };
//   }

//   asString(): string {
//     const offsets = this.#offsets;

//     if (offsets === null) {
//       return '';
//     } else {
//       return this.template.slice(offsets.start, offsets.end);
//     }
//   }

//   asAnnotatedString(): string {
//     const template = this.template;
//     const lines = template.lines;

//     if (this.verify() && lines !== null) {
//       const loc = this.toAST();

//       return new FormatSpan(lines, loc).format(() => this.asString());
//     } else {
//       return this.asString();
//     }
//   }

//   /**
//    * For compatibility with SourceLocation in AST plugins
//    *
//    * @deprecated use startPosition instead
//    */
//   get start(): SourcePosition {
//     return this.#start.toAST();
//   }

//   /**
//    * For compatibility with SourceLocation in AST plugins
//    *
//    * @deprecated use withStart instead
//    */
//   set start(position: SourcePosition) {
//     this.data.locDidUpdate({ start: position });
//   }

//   /**
//    * For compatibility with SourceLocation in AST plugins
//    *
//    * @deprecated use endPosition instead
//    */
//   get end(): SourcePosition {
//     return this.#end.toAST();
//   }

//   /**
//    * For compatibility with SourceLocation in AST plugins
//    *
//    * @deprecated use withEnd instead
//    */
//   set end(position: SourcePosition) {
//     this.data.locDidUpdate({ end: position });
//   }

//   /**
//    * For compatibility with SourceLocation in AST plugins
//    *
//    * @deprecated use module instead
//    */
//   get source(): string {
//     return this.module;
//   }

//   extend(other: SourceSpan): SourceSpan {
//     if (other instanceof ConcreteSpan) {
//       return new ConcreteSpan(this.template, this.#start, other.#end);
//     } else {
//       return new BrokenConcreteSpan(this.template, this.#start, other.endPosition());
//     }
//   }

//   serialize(): SerializedSourceSpan {
//     const offsets = this.#offsets;

//     if (offsets === null) {
//       return serializeBroken({ start: this.#start.toAST(), end: this.#end.toAST() });
//     } else {
//       return serializeOffsets(offsets);
//     }
//   }
// }

export class ConcreteSpan extends SourceSpan {
  constructor(data: SpanData, overrides?: SpanData['offsets']) {
    super(data, overrides);
  }

  serialize(): SerializedSourceSpan {
    const offsets = this.offsets;

    if (offsets === null) {
      return serializeBroken({ start: this.getStart().toAST(), end: this.getEnd().toAST() });
    } else {
      return serializeOffsets(offsets);
    }
  }

  withStart(start: SourceOffset | BrokenPosition): SourceSpan {
    return SourceSpan.from({
      template: this.getTemplate(),
      offsets: {
        start,
        end: this.getEnd(),
      },
    });
  }

  withEnd(end: SourceOffset | BrokenPosition): SourceSpan {
    return SourceSpan.from({
      template: this.getTemplate(),
      offsets: {
        start: this.getStart(),
        end,
      },
    });
  }

  asString(): string {
    const offsets = this.offsets;

    if (offsets) {
      return this.getTemplate().slice(offsets.start, offsets.end);
    } else {
      return '';
    }
  }

  asAnnotatedString(): string {
    const template = this.getTemplate();
    const lines = template.lines;

    if (this.verify() && lines) {
      const loc = this.toAST();

      return new FormatSpan(lines, loc).format(() => this.asString());
    } else {
      return this.asString();
    }
  }

  extend(other: SourceSpan): SourceSpan {
    return SourceSpan.from({
      template: this.getTemplate(),
      offsets: {
        start: this.getStart(),
        end: other.getEnd(),
      },
    });
  }

  verify(): boolean {
    return this.offsets !== null;
  }
}

export class SyntheticSpan extends SourceSpan {
  readonly #string: string;

  constructor(
    template: SourceTemplate,
    offsets: { start: SourceOffset | BrokenPosition; end: SourceOffset | BrokenPosition },
    string: string
  ) {
    super({ template, offsets });
    this.#string = string;
  }

  serialize(): SerializedSourceSpan {
    return this.#string;
  }

  extend(other: SourceSpan): SourceSpan {
    return other;
  }

  verify(): boolean {
    return false;
  }

  withStart(_start: SourceOffset | BrokenPosition): SourceSpan {
    console.warn('withStart is not supported on MultiSpan (FIXME: warning)');
    return this;
  }
  withEnd(_end: SourceOffset | BrokenPosition): SourceSpan {
    console.warn('withEnd is not supported on MultiSpan (FIXME: warning)');
    return this;
  }

  asString(): string {
    return this.#string;
  }

  asAnnotatedString(): string {
    return format(this.#string);
  }
}

/**
 * A MultiSpan has at least one synthetic span and at least one concrete span.
 */
export class MultiSpan extends SourceSpan {
  static create(template: SourceTemplate, spans: PresentArray<SourceSpan>) {
    const concrete = spans.filter((span) => span instanceof ConcreteSpan);

    // If all of the spans are synthetic, just make a single synthetic span
    if (concrete.length === 0) {
      return new SyntheticSpan(
        template,
        { start: spans[0].getStart(), end: spans[spans.length - 1].getEnd() },
        spans.map((span) => span.asString()).join('')
      );
    }

    // If all of the spans are concrete, just make a single concrete span
    if (concrete.length === spans.length) {
      return SourceSpan.from({
        template,
        offsets: {
          start: spans[0].getStart(),
          end: spans[spans.length - 1].getEnd(),
        },
      });
    }

    return new MultiSpan(
      template,
      {
        start: concrete[0].getStart(),
        end: concrete[concrete.length - 1].getEnd(),
      },
      spans,
      concrete as PresentArray<ConcreteSpan>
    );
  }

  #spans: SourceSpan[];
  #concrete: PresentArray<ConcreteSpan>;

  constructor(
    template: SourceTemplate,
    offsets: { start: SourceOffset | BrokenPosition; end: SourceOffset | BrokenPosition },
    spans: SourceSpan[],
    concrete: PresentArray<ConcreteSpan>
  ) {
    super({ template, offsets });
    this.#spans = spans;
    this.#concrete = concrete;
  }

  serialize(): SerializedSourceSpan {
    return this.#spans.map((span) => span.asString()).join('');
  }

  extend(other: SourceSpan): SourceSpan {
    return SourceSpan.from({
      template: this.getTemplate(),
      offsets: {
        start: this.getStart(),
        end: other.getEnd(),
      },
    });
  }

  verify(): boolean {
    return this.#spans.every((span) => span.verify());
  }

  withStart(_start: SourceOffset | BrokenPosition): SourceSpan {
    console.warn('withStart is not supported on MultiSpan (FIXME: warning)');
    return this;
  }

  withEnd(_end: SourceOffset | BrokenPosition): SourceSpan {
    console.warn('withEnd is not supported on MultiSpan (FIXME: warning)');
    return this;
  }

  asString(): string {
    return this.#spans.map((span) => span.asString()).join('');
  }

  asAnnotatedString(): string {
    return format(this.asString());
  }
}
