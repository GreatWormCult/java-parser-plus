type KeysU<D> = D extends unknown ? keyof D : never;
type IsKeyOf<K, C> = K extends keyof C ? K : never;
type IfNever<T, Fallback> = [T] extends [never] ? Fallback : T;
type PickArr<T, K> = T extends unknown
  ? K extends Array<infer R>
    ? Pick<T, IsKeyOf<R, T>>
    : never
  : never;
type ValuesU<D, Fallback = never> = D extends unknown
  ? IfNever<D[keyof D], Fallback>
  : never;

export const any = ((obj: Record<string, unknown>, ...keys: string[]) => {
  keys = keys.length > 0 ? keys : Object.keys(obj);
  return keys.filter((k) => k in obj).map((k) => obj[k])[0];
}) as (<T>(ctx: T) => ValuesU<T>) &
  (<T, R extends KeysU<T>[]>(
    ctx: T,
    ...keys: R
  ) => ValuesU<PickArr<T, R>, undefined>);
