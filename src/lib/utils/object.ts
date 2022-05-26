export type Dict<T = unknown> = { [key: string]: T };

export function dict<T>(): Dict<T> {
  return Object.create(null) as Dict<T>;
}

export function extract<R extends object, K extends keyof R>(
  object: R,
  key: K
): { extracted: R[K]; rest: Omit<R, K> } {
  const result = Object.create(null) as Omit<R, K>;
  Object.keys(object).forEach((k) => {
    if (k !== key) {
      (result as any)[k] = (object as any)[k];
    }
  });

  return { extracted: object[key], rest: result };
}

export function assign<O extends object, P extends object>(object: O, props: P): O & P {
  return Object.assign(object, props);
}
