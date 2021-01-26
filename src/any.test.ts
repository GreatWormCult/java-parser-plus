import test from 'ava';

import { any } from './any';

const simpleObject = { a: 1 as const, b: 'b' as const };

test('any value of simple object', (t) =>
  t.deepEqual<1 | 'b'>(any(simpleObject), 1));
