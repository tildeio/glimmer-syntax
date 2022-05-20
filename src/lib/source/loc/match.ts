import { isPresent } from '../../utils/array.js';
import { assert } from '../../utils/assert.js';
import type { IsAbsent, IsBroken, OffsetKind } from './kind.js';
import type { BrokenPosition } from './offset';
import {
  type AbsentPosition,
  type CharPosition,
  type OffsetPosition,
  type PositionData,
  patternFor,
} from './offset';

/**
 * This file implements the DSL used by span and offset in places where they need to exhaustively
 * consider all combinations of states (Handlebars offsets, character offsets and invisible/broken
 * offsets).
 *
 * It's probably overkill, but it makes the code that uses it clear. It could be refactored or
 * removed.
 */

export const MatchAny = 'MATCH_ANY';
export type MatchAny = 'MATCH_ANY';

type Matches =
  | 'Char,Hbs'
  | 'Hbs,Char'
  | 'Hbs,Hbs'
  | 'Char,Char'
  | 'Invisible,Any'
  | 'Any,Invisible'
  | 'Broken,Any'
  | 'Any,Broken';

export type Pattern = OffsetKind | IsAbsent | IsBroken | MatchAny;

class WhenList<Out> {
  _whens: When<Out>[];

  constructor(whens: When<Out>[]) {
    this._whens = whens;
  }

  first(kind: OffsetKind): Out | null {
    for (let when of this._whens) {
      let value = when.match(kind);
      if (isPresent(value)) {
        return value[0];
      }
    }

    return null;
  }
}

class When<Out> {
  _map: Map<Pattern, Out> = new Map();

  get(pattern: Pattern, or: () => Out): Out {
    let value = this._map.get(pattern);

    if (value) {
      return value;
    }

    value = or();

    this._map.set(pattern, value);

    return value;
  }

  add(pattern: Pattern, out: Out): void {
    this._map.set(pattern, out);
  }

  match(kind: OffsetKind): Out[] {
    let pattern = patternFor(kind);

    let out: Out[] = [];

    let exact = this._map.get(pattern);
    let fallback = this._map.get(MatchAny);

    if (exact) {
      out.push(exact);
    }

    if (fallback) {
      out.push(fallback);
    }

    return out;
  }
}

type ExhaustiveCheck<Out, In extends Matches, Removed extends Matches> = Exclude<
  In,
  Removed
> extends never
  ? ExhaustiveMatcher<Out>
  : Matcher<Out, Exclude<In, Removed>>;

export type MatchFn<Out> = (left: PositionData, right: PositionData) => Out;

interface ExhaustiveMatcher<Out> {
  check(): MatchFn<Out>;
}

export function match<Out>(callback: (m: Matcher<Out>) => ExhaustiveMatcher<Out>): MatchFn<Out> {
  return callback(new Matcher()).check();
}

class Matcher<Out, M extends Matches = Matches> {
  _whens: When<When<(left: PositionData, right: PositionData) => Out>> = new When();

  /**
   * You didn't exhaustively match all possibilities.
   */
  protected check(): MatchFn<Out> {
    return (left, right) => this.matchFor(left.kind, right.kind)(left, right);
  }

  private matchFor(
    left: OffsetKind,
    right: OffsetKind
  ): (left: PositionData, right: PositionData) => Out {
    let nesteds = this._whens.match(left);

    assert(
      isPresent(nesteds),
      `no match defined for (${left}, ${right}) and no AnyMatch defined either`
    );

    let callback = new WhenList(nesteds).first(right);

    assert(
      callback !== null,
      `no match defined for (${left}, ${right}) and no AnyMatch defined either`
    );

    return callback;
  }

  // This big block is the bulk of the heavy lifting in this file. It facilitates exhaustiveness
  // checking so that matchers can ensure they've actually covered all the cases (and TypeScript
  // will treat it as an exhaustive match).
  when(
    left: OffsetKind.CharPosition,
    right: OffsetKind.HbsPosition,
    callback: (left: CharPosition, right: OffsetPosition) => Out
  ): ExhaustiveCheck<Out, M, 'Char,Hbs'>;
  when(
    left: OffsetKind.HbsPosition,
    right: OffsetKind.CharPosition,
    callback: (left: OffsetPosition, right: CharPosition) => Out
  ): ExhaustiveCheck<Out, M, 'Hbs,Char'>;
  when(
    left: OffsetKind.HbsPosition,
    right: OffsetKind.HbsPosition,
    callback: (left: OffsetPosition, right: OffsetPosition) => Out
  ): ExhaustiveCheck<Out, M, 'Hbs,Hbs'>;
  when(
    left: OffsetKind.CharPosition,
    right: OffsetKind.CharPosition,
    callback: (left: CharPosition, right: CharPosition) => Out
  ): ExhaustiveCheck<Out, M, 'Char,Char'>;
  when(
    left: IsAbsent,
    right: MatchAny,
    callback: (left: AbsentPosition, right: PositionData) => Out
  ): Matcher<Out, Exclude<M, 'Invisible,Any'>>;
  when(
    left: MatchAny,
    right: IsAbsent,
    callback: (left: PositionData, right: AbsentPosition) => Out
  ): ExhaustiveCheck<Out, M, 'Any,Invisible'>;
  when(
    left: IsBroken,
    right: MatchAny,
    callback: (left: BrokenPosition, right: PositionData) => Out
  ): Matcher<Out, Exclude<M, 'Invisible,Any'>>;
  when(
    left: MatchAny,
    right: IsBroken,
    callback: (left: PositionData, right: BrokenPosition) => Out
  ): ExhaustiveCheck<Out, M, 'Any,Invisible'>;

  when(
    left: MatchAny,
    right: MatchAny,
    callback: (left: PositionData, right: PositionData) => Out
  ): ExhaustiveMatcher<Out>;
  when(
    left: Pattern,
    right: Pattern,
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    callback: (left: any, right: any) => Out
  ): Matcher<Out, Matches> | ExhaustiveMatcher<Out> {
    this._whens.get(left, () => new When()).add(right, callback);

    return this;
  }
}
