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
  const { useLibrary, loadLibrary, library } = await import(url('compressed.library.mjs'));
  if (override === 'bytes' || override === 'module' || override === 'async-bytes') {
    const bytes = fs.readFileSync(file('compressed.wasm'));
    useLibrary(override === 'module' ? new WebAssembly.Module(bytes) : bytes,
               { debug: fs.readFileSync(file('compressed.wasm.debug')) });
  }
  // (loadLibrary: the module compiled by WebAssembly.compile, never by new
  // WebAssembly.Module, without holding the thread; the toplevel run once.
  // async-retry: the first compiling fails, and the next call tries again;
  // async-replace: useLibrary() gives other bytes while it compiles, which are
  // compiled in turn; async-early: an export called before it is settled loads
  // the library in the calling thread, which the promise then gives.)
  if (override?.startsWith('async')) {
    const { compile, Module } = WebAssembly;
    let compiles = 0, modules = 0, failing = override === 'async-retry';
    WebAssembly.compile = (...args) => {
      compiles++;
      if (failing) {
        failing = false;
        return Promise.reject(new Error('compiling failed'));
      }
      return compile(...args);
    };
    // (not counting xz's decoder, a module of 12,529 bytes made in the calling thread)
    WebAssembly.Module = new Proxy(Module, {
      construct(target, args) {
        if (args[0].byteLength > 65536) modules++;
        return Reflect.construct(target, args);
      },
    });
    try {
      if (override === 'async-retry') {
        await assert.rejects(loadLibrary(), /compiling failed/);
        assert.equal(globalThis.compressionStarts, 0);
      }
      const loading = loadLibrary();
      assert.equal(globalThis.compressionStarts, 0);
      if (override === 'async-replace') {
        useLibrary(fs.readFileSync(file('compressed.wasm')), { debug: fs.readFileSync(file('compressed.wasm.debug')) });
      }
      if (override === 'async-early') assert.equal(other.other(), 42);
      const loaded = await loading;
      assert.equal(globalThis.compressionStarts, 1);
      assert.equal(library(), loaded);
      assert.equal(await loadLibrary(), loaded);
      assert.deepEqual([compiles, modules], {
        'async-retry': [2, 0], 'async-replace': [2, 0], 'async-early': [1, 1],
      }[override] ?? [1, 0]);
    } finally {
      Object.assign(WebAssembly, { compile, Module });
    }
    assert.throws(() => useLibrary(new Uint8Array()), /loaded already/);
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
