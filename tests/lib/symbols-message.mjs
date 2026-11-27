// Call tests/lib/symbols-message.lisp, compiled with --shake-symbols (tests/lib/run.sh):
//   node tests/lib/symbols-message.mjs build/lib-test/symbols-message.mjs
import assert from 'node:assert/strict';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const lib = await import(pathToFileURL(path.resolve(process.argv[2])).href);
const f = lib.default.exports;

const message = (thunk) => {
  try { thunk(); } catch (e) { return e.message.replace(/\s+/g, ' '); }
  assert.fail('no error');
};
assert.equal(f.currentPackage(), 'PB');
assert.equal(f.good(3), 3);
// the symbols of these names in PB and in COMMON-LISP are left out
const leftOut = /is not in this module: it was compiled with --shake-symbols/;
assert.match(message(() => f.lookUp('THING')), leftOut);
assert.match(message(() => f.lookUp('PAIRLIS')), leftOut);
// and the symbols of other packages with these names are written with their packages
assert.equal(message(() => f.notAThing(5)), 'The value 5 is not of type PA::THING');
assert.equal(message(() => f.odd(1)), 'PA::THING is odd (PC::PAIRLIS 1)');
console.log('lib: symbols-message ok');
