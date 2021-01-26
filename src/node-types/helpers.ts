import { IToken } from 'chevrotain';

// eslint-disable-next-line @typescript-eslint/ban-types
export type Opt<T> = T | {};

export type OptCommas = Opt<{ Comma: IToken[] }>;

export type MultiIdentifier = {
  Identifier: [IToken];
} & Opt<{
  Dot: IToken[];
  Identifier: IToken[];
}>;

export type Curly = {
  LCurly: [IToken];
  RCurly: [IToken];
};

export type Brace = {
  LBrace: [IToken];
  RBrace: [IToken];
};
