import { parse as oldParse } from 'java-parser';
import { Visitor } from 'visitor-ts';

import { CompilationUnitCstNode } from './node-types/node-types';

export * from './node-types/node-types';

export * from './any';

export { IToken, CstNode } from 'chevrotain';

export const parse = oldParse as (text: string) => CompilationUnitCstNode;
export class JavaCstVisitor<T> extends Visitor<
  T,
  'default',
  'name',
  'children'
> {
  constructor(handlers: T) {
    super(handlers, 'default', 'name', 'children');
  }
}
