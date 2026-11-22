import assert from 'node:assert/strict';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const { default: lib } = await import(pathToFileURL(path.resolve(process.argv[2])).href);
assert.equal(lib.exports.one(), 1);
assert.throws(() => lib.exports.lookup('UNUSED'), /is not in this module: it was compiled with --shake-symbols/);
assert.equal(lib.exports.lookup('NEW-SYMBOL'), 'NEW-SYMBOL');
assert.equal(lib.exports.lookup('NEW-SYMBOL'), 'NEW-SYMBOL');
console.log('lib: empty symbol table ok');
