import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
const [dir, mode, override] = process.argv.slice(2);
const file = name => path.resolve(dir, name);
const url = name => pathToFileURL(file(name)).href;
globalThis.compressionStarts = 0;
globalThis.eval = () => { throw new Error('eval must not be used'); };
const lib = await import(url('compressed.mjs'));
const other = await import(url('other.mjs'));
if (mode === 'lazy') {
  assert.equal(globalThis.compressionStarts, 0);
  if (override) {
    const { useLibrary } = await import(url('compressed.library.mjs'));
    const bytes = fs.readFileSync(file('compressed.wasm'));
    useLibrary(override === 'module' ? new WebAssembly.Module(bytes) : bytes,
               { debug: fs.readFileSync(file('compressed.wasm.debug')) });
  }
} else assert.equal(globalThis.compressionStarts, 1);
assert.equal(lib.next(), 1);
assert.equal(lib.next(), 2);
assert.equal(lib.text('日本語\u0000'), 'prefix:日本語\u0000');
assert.equal(other.other(), 42);
assert.equal(globalThis.compressionStarts, 1);
assert.throws(() => lib.fail(), e => e.message === 'compressed library failure'
              && /%JS-RUN \(runtime\/js\.lisp:\d+\)/.test(e.stack));
if (mode === 'eager') {
  const another = await lib.create();
  assert.equal(another.exports.next(), 1);
  assert.equal(lib.next(), 3);
  assert.equal(globalThis.compressionStarts, 2);
}
// (the names of the functions, which the trace above has, are not in the
// embedded module: a string of their own, decoded for the trace)
{
  const text = fs.readFileSync(file(mode === 'lazy' ? 'compressed.library.mjs' : 'compressed.mjs'), 'utf8');
  const strings = [...text.matchAll(/'([A-Za-z0-9+/=]{100,})'/g)].map((m) => m[1]);
  assert.equal(strings.length, 2);
  assert.match(text, /const __clysm_debug = '/);
}
console.log(`lib: compressed ${mode}${override ? ' ' + override : ''} ok`);
