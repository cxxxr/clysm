// tests/lib/nodispatch.lisp, compiled as a shaken library (tests/lib/run.sh)
import assert from 'node:assert/strict';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const lib = await import(pathToFileURL(path.resolve(process.argv[2])).href);
const e = lib.default.exports;
assert.equal(e.missing(), 'SIMPLE-ERROR');
assert.match(e.missingMessage(), /slot/i);
console.log('lib nodispatch: ok');
