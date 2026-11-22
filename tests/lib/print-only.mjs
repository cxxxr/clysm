import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
const file = path.resolve(process.argv[2]);
const { default: lib } = await import(pathToFileURL(file).href);
for (const [name, expected] of [['HELLO', '#:HELLO'], ['a b', '#:|a b|'], ['', '#:||'], ['#FOO', '#:|#FOO|'], ['A|B', '#:|A\\|B|']]) {
  assert.equal(lib.exports.printed(name), expected);
}
// Retaining the reader again adds about 900 KB to this library.
assert.ok(fs.statSync(file.replace(/\.mjs$/, '.wasm')).size < 1900000, 'a print-only library must not retain the reader');
console.log('lib: printing without the reader ok');
