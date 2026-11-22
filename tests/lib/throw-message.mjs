import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
const file = path.resolve(process.argv[2]);
const { default: { exports: lib } } = await import(pathToFileURL(file).href);
const error = new TypeError('original message');
const empty = { name: 'Empty', message: '' };
const nonstring = { name: 7, message: 42 };
const fn = () => {};
for (const value of [error, empty, nonstring, fn, {}, undefined, null, false, true, 7, 'text']) {
  let didThrow = false;
  try { lib.throwValue(value, 'fallback'); }
  catch (e) { didThrow = true; assert.equal(e, value); }
  assert.equal(didThrow, true);
  const caught = lib.caught(value, 'fallback');
  assert.equal(caught.thrown, value);
  assert.equal(caught.message, typeof value?.message === 'string' ? value.message : 'fallback');
  assert.equal(caught.name, typeof value?.name === 'string' ? value.name : null);
}
assert.equal(lib.caught(7, '').message, '');
assert.throws(() => lib.throwValue(7, 42), e => e.lispType === 'TYPE-ERROR');
const order = [];
assert.throws(() => lib.ordered(() => { order.push('value'); return error; },
                              () => { order.push('message'); return 'fallback'; }), e => e === error);
assert.deepEqual(order, ['value', 'message']);
const accessed = [];
const getters = { get name() { accessed.push('name'); return 'Custom'; },
                  get message() { accessed.push('message'); return 'from getter'; } };
assert.equal(lib.caught(getters, 'fallback').message, 'from getter');
assert.deepEqual(accessed, ['name', 'message']);
// A generic JS:THROW retains the printer; direct calls with :MESSAGE need not.
assert.ok(fs.statSync(file.replace(/\.mjs$/, '.wasm')).size < 1400000,
          'an explicit throw message must not retain the printer');
console.log('lib: throwing with explicit messages ok');
