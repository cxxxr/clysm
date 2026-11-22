// Calls tests/lib/package.lisp compiled as a library (tests/lib/run.sh):
//   node tests/lib/package.mjs DIR eager|lazy
// DIR has package.mjs and places.mjs (the entry "places"; .js for the lazy
// library), and their .d.ts.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const [dir, mode] = [path.resolve(process.argv[2]), process.argv[3]];
const url = (file) => pathToFileURL(path.join(dir, file)).href;
const lazy = mode === 'lazy';
// (the lazy library's modules are .js: tests/lib/lazy.lisp)
const ext = lazy ? '.js' : '.mjs';

let loaded = null;
if (lazy) {
  // importing the modules loads nothing: the constants and classes are there
  const { library } = await import(url(`package.library${ext}`));
  loaded = library;
}
const lib = await import(url(`package${ext}`));
const places = await import(url(`places${ext}`));

if (lazy) {
  // nothing is loaded yet: the library can still be given another module
  const { useLibrary } = await import(url(`package.library${ext}`));
  useLibrary(new URL(url('package.wasm')));
}
assert.equal(lib.version, 113);
assert.deepEqual([...lib.names], ['a', 'b']);
assert.equal(places.placesVersion, 2);
assert.equal(typeof lib.PlaceError, 'function');
assert.equal(lib.PlaceError.name, 'PlaceError');
// the entry has its own exports and not those of the library
assert.deepEqual(Object.keys(places).sort(), ['firstPlace', 'lib', 'placesVersion']);
assert.equal(places.lib, 'a constant named lib');
assert.equal(lib.firstPlace, undefined);
if (lazy) {
  assert.equal(Object.hasOwn(lib, 'default'), false);
  // (a class's first use loads the library: here, the first call)
}

// a class of the library, made from JavaScript and from Lisp
const c = new lib.Counter(2);
assert.equal(c.add(3), 5);
assert.equal(c.count, 5);
assert.ok(c instanceof lib.Counter);
assert.equal(lib.Tally, lib.Counter);
assert.equal(new lib.Tally(1).add(1), 2);
assert.equal(lib.First, lib.Second);
assert.equal(new lib.First().hidden, true);
const Named = (lazy ? loaded() : lib.default).exports['to-string'];
assert.notEqual(Named, Object.prototype.toString);
assert.equal(new Named().made, true);
assert.equal(lib.library(), 'a function named library');
assert.equal(lib.classes, 'a constant named classes');
const d = lib.Counter.of(10);
assert.ok(d instanceof lib.Counter);
assert.equal(d.add(1), 11);
assert.equal(lib.loads(), 1);
assert.equal(Object.getOwnPropertyNames(lib.Counter.prototype).sort().join(), 'add,constructor,count');

// errors: a class of its own that extends Error, thrown by Lisp as it is
const e = new lib.PlaceError('why', 'the message');
assert.ok(e instanceof Error && e instanceof lib.PlaceError);
assert.equal(e.message, 'the message');
assert.equal(e.name, 'PlaceError');
assert.equal(e.reason, 'why');
assert.match(e.stack, /^PlaceError: the message/);
assert.ok(new lib.LineError('r', 'm') instanceof lib.PlaceError);
assert.deepEqual(lib.parseLine('12'), { kind: 'line', line: 12 });
assert.throws(() => lib.parseLine('x'), (err) => err instanceof lib.PlaceError && err.reason === 'not-a-line'
                                          && err.message === '[PLACE-001] not a line: x');
assert.throws(() => lib.parseLine('0'), (err) => err.constructor === Error && err.name === 'Error'
                                          && err.message === '[PLACE-002] lines are counted from 1');

// what JavaScript throws goes through Lisp as it was
const thrown = Object.assign(new Error('from JavaScript'), { code: 'ENOENT' });
assert.throws(() => lib.callThrough(() => { throw thrown; }), (err) => err === thrown);
assert.equal(lib.callHandled(() => { throw thrown; }), 'handled: from JavaScript');
const notAnError = { why: 'a value' };
assert.throws(() => lib.callThrough(() => { throw notAnError; }), (err) => err === notAnError);
for (const [kind, value] of [['undefined', undefined], ['false', false], ['7', 7]]) {
  let got = 'nothing';
  try { lib.throwValue(kind); } catch (err) { got = err; }
  assert.equal(got, value, kind);
}
await assert.rejects(lib.awaitThrough(() => Promise.reject(thrown)), (err) => err === thrown);
assert.equal(await lib.awaitThrough(() => Promise.resolve(4)), 4);
assert.throws(() => lib.lispError(), (err) => err.name === 'LispError' && /a Lisp error/.test(err.message));
assert.deepEqual(places.firstPlace([{ kind: 'line', line: 3 }]), { kind: 'line', line: 3 });

// the types: of the exports, of the classes and those declared
const dts = fs.readFileSync(path.join(dir, 'package.d.ts'), 'utf8');
assert.match(dts, /export interface Place \{ kind: 'line'; line: number \}/);
assert.match(dts, /export declare const version: number;/);
assert.match(dts, /export declare const parseLine: \(text: string\) => Place;/);
assert.match(dts, /export declare class Counter \{\n  constructor\(start\?: number\);/);
assert.match(dts, /export declare class PlaceError extends Error \{/);
assert.match(dts, /export \{ Counter as Tally \};/);
assert.match(dts, /export declare const Typed: \{ new \(n: number\): \{ n: number \}; readonly kind: 'typed' \};/);
const placesDts = fs.readFileSync(path.join(dir, 'places.d.ts'), 'utf8');
assert.match(placesDts, /import type \{ Place \} from '\.\/package\.js';\nexport type Places = Place\[\];/);
assert.match(placesDts, /export declare const firstPlace: \(places: Places\) => Place;/);
assert.doesNotMatch(placesDts, /Counter/);
assert.match(fs.readFileSync(path.join(dir, 'types.d.ts'), 'utf8'), /export interface Point \{ x: number \}/);
assert.ok(fs.existsSync(path.join(dir, `types${ext}`)));
assert.equal(new lib.Widget(3).n, 3);
assert.match(dts, /export declare class Widget \{\n  constructor\(\.\.\.args: any\[\]\);/);

assert.deepEqual(lib.kinds(undefined, null, false, 0, 's', {}, () => 1),
                 ['undefined', 'null', 'boolean', 'number', 'string', 'object', 'function']);
assert.equal(lib.indexIn('a'.repeat(100000) + 'needle', 'needle'), 100000);
for (const v of [undefined, null, false, 0, '', 'text']) assert.equal(lib.identity(v), v);
const o = {};
assert.equal(lib.identity(o), o);
assert.equal(await lib.identityLater(undefined), undefined);
assert.equal(await lib.identityLater(false), false);
assert.equal(new lib.Box(undefined).kind, 'undefined');
assert.equal(new lib.Box(null).same(false), false);
assert.equal(lib.converted(5), 'number');
assert.equal(lib.unconverted(5), 'other');
assert.ok(Object.hasOwn(lib.protoData, '__proto__'));
assert.equal(lib.protoData.enabled, undefined);
assert.equal(lib.JSON, 'json');
assert.equal(lib.process, 'process');
assert.equal(lib.Hidden, 123);
assert.deepEqual(lib.rawKinds({ u: undefined, n: null, f: false, itself() { return false; } },
                              (callback) => (callback ? callback(undefined) : false)),
                 ['undefined', 'null', 'boolean', 'boolean', 'boolean', 'undefined']);
assert.deepEqual(lib.parsePart(`x${'y'.repeat(100000)}{"a":[1,2]}z`, 100001, 100012), { a: [1, 2] });
assert.equal(await lib.awaitKind(Promise.resolve(undefined)), 'undefined');
assert.equal(await lib.awaitKind(Promise.resolve(false)), 'boolean');
assert.equal(await lib.awaitLisp(), 42);
if (lazy) assert.ok(loaded().instance);
lib.say('to the standard error');
console.log(`lib: package (${mode}) ok`);
