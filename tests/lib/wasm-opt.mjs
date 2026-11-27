// bin/clysm library --wasm-opt (tools/wasm-opt.mjs): the library that passed
// through Binaryen's wasm-opt (OPTIMISED) answers as the one that did not
// (PLAIN), and the stack traces of its errors name the same Lisp functions
// at the same places.
//   node tests/lib/wasm-opt.mjs PLAIN/wasm-opt.mjs OPTIMISED/wasm-opt.mjs
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const [plainPath, optimisedPath] = process.argv.slice(2).map((p) => path.resolve(p));
// the frames of Lisp functions: "NAME (FILE.lisp:LINE)"
const lispFrames = (e) => String(e.stack).split('\n')
  .map((line) => /^\s+at (.* \(.*\.lisp:\d+\))$/.exec(line)?.[1]).filter((frame) => frame);
async function observe(file) {
  const lib = await import(pathToFileURL(file).href);
  const seen = { double: lib.double(1), backtrace: lib.bt(1) };
  for (const [name, argument] of [['outer', 5], ['setter', 7], ['area', 9]]) {
    try {
      lib[name](argument);
      assert.fail(`${name} did not signal`);
    } catch (e) {
      assert.equal(e.name, 'LispError');
      seen[name] = { message: e.message, frames: lispFrames(e) };
    }
  }
  return seen;
}
const plain = await observe(plainPath), optimised = await observe(optimisedPath);
const place = (line) => `(tests/lib/wasm-opt.lisp:${line})`;
assert.equal(plain.double, 2);
assert.equal(plain.outer.message, 'leaf failed with 11');
assert.deepEqual(plain.outer.frames.slice(0, 3), [`LEAF ${place(7)}`, `MIDDLE ${place(11)}`, `OUTER ${place(15)}`]);
assert.deepEqual(plain.setter.frames.slice(0, 2), [`(SETF THING) ${place(19)}`, `SETTER ${place(23)}`]);
assert.deepEqual(plain.area.frames.slice(0, 4),
  [`LEAF ${place(7)}`, `MIDDLE ${place(11)}`, `OUTER ${place(15)}`, `lambda ${place(28)}`]);
assert.match(plain.backtrace, /^\("BT" "BT-OUTER" /);
assert.deepEqual(optimised, plain);

// (it is another module, a smaller one, with the names of the functions it has left)
const size = (file, suffix) => fs.statSync(file.replace(/\.mjs$/, suffix)).size;
for (const suffix of ['.wasm', '.wasm.debug', '.library.mjs']) {
  assert.ok(size(optimisedPath, suffix) < size(plainPath, suffix),
    `${suffix}: ${size(optimisedPath, suffix)} bytes with wasm-opt, ${size(plainPath, suffix)} without`);
}
console.log(`lib: wasm-opt: ok (${size(plainPath, '.wasm')} -> ${size(optimisedPath, '.wasm')} bytes, `
  + `embedded ${size(plainPath, '.library.mjs')} -> ${size(optimisedPath, '.library.mjs')}; the same frames)`);
