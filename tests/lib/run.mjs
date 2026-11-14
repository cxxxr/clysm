// Load tests/lib/demo.lisp as a library and call its exports (tests/lib/run.sh):
//   node tests/lib/run.mjs build/lib-test/demo.mjs
import assert from 'node:assert/strict';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const entry = path.resolve(process.argv[2]);
// the generated ES module: the named exports are the Lisp functions
const lib = await import(pathToFileURL(entry).href);
const { countWords, calls, deep, fails, upcaseAll, fetchTwice } = lib;

assert.equal(countWords('hello wide  world'), 3);
assert.equal(countWords('a b c d'), 4);
assert.equal(calls(), 2);
assert.deepEqual(upcaseAll(['a', 'bc']), ['A', 'BC']);
assert.equal(lib.default.exports.path(), 'a/b');
assert.equal(deep(5000), 5000);
// a stack overflow is a RangeError, after which the instance goes on
assert.throws(() => deep(100000000), RangeError);
assert.equal(deep(10), 10);

assert.throws(() => fails(42), (e) => e.name === 'LispError' && /cannot handle 42/.test(e.message)
                                  && /^LispError: cannot handle 42\n\s+at (CHECK|FAILS) \(tests\/lib\/demo\.lisp:1[67]\)/.test(e.stack));
const sleep = (ms, v) => new Promise((r) => setTimeout(() => r(v), ms));
assert.equal(await fetchTwice((k) => sleep(k === 'a' ? 20 : 5, k.toUpperCase())), 'A+B');
// several calls wait at the same time
const all = await Promise.all([1, 2, 3].map((i) => fetchTwice((k) => sleep(10 * i, k + i))));
assert.deepEqual(all, ['a1+b1', 'a2+b2', 'a3+b3']);
// the instance lives on
assert.equal(calls(), 2);
// a long string that starts with U+FEFF keeps it
assert.equal(lib.default.exports.same('\uFEFF' + 'a'.repeat(100)).length, 101);
// subprocesses
assert.equal(lib.default.exports.echo('hi'), 'hi\n');
assert.deepEqual(await lib.default.exports.catLines(['x', 'y z']), ['x', 'y z']);
// two at the same time
assert.deepEqual(await Promise.all([lib.default.exports.catLines(['a']), lib.default.exports.catLines(['b'])]),
                 [['a'], ['b']]);
// another instance has its own state
const other = await lib.create();
assert.equal(other.exports.countWords('x'), 1);
assert.equal(other.exports.calls(), 1);
assert.equal(calls(), 2);

// in a worker with a deep stack
const { loadInWorker } = await import(pathToFileURL(path.join(path.dirname(entry), 'clysm/worker.mjs')).href);
const w = await loadInWorker(entry.replace(/\.mjs$/, '.wasm'), { stackSizeMb: 512 });
assert.equal(await w.exports.deep(1000000), 1000000);
await assert.rejects(w.exports.fails('x'), (e) => e.name === 'LispError' && /cannot handle "x"/.test(e.message));
assert.equal(await w.exports.countWords('a b'), 2);
w.terminate();
console.log('lib: ok');
