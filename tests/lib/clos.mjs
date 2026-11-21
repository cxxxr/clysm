// Call tests/lib/clos.lisp, compiled as a shaken library (tests/lib/run.sh):
//   node tests/lib/clos.mjs build/lib-test/clos.mjs
import assert from 'node:assert/strict';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const lib = await import(pathToFileURL(path.resolve(process.argv[2])).href);
assert.equal(lib.default.exports.shapes(), '32 "#<CIRCLE c> #<SQUARE s> 3 16\nab\n" T T');
assert.equal(lib.default.exports.noArea(), 'SIMPLE-ERROR');
console.log('lib clos: ok');
