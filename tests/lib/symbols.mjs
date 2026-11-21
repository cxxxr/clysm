// Call tests/lib/symbols.lisp, compiled with --shake-symbols (tests/lib/run.sh):
//   node tests/lib/symbols.mjs build/lib-test/symbols.mjs
import assert from 'node:assert/strict';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const lib = await import(pathToFileURL(path.resolve(process.argv[2])).href);
const f = lib.default.exports;

// the symbols the code refers to are found by name
assert.equal(f.areaOf('circle', 2), 12);
assert.equal(f.areaOf('square', 3), 9);
assert.equal(f.keyArg('size'), 5);
assert.equal(f.printed(), 'SHAPES:CIRCLE :SIZE CAR');
assert.equal(f.readLength('(a b c)'), 3);
assert.equal(f.readSquare(), 16);
assert.equal(f.lookUp('car', 'common-lisp'), 'CAR :EXTERNAL');
assert.equal(f.lookUp('area', 'shapes'), 'SHAPES:AREA :EXTERNAL');
assert.equal(f.lookUp('car', 'shapes'), 'CAR :INHERITED');
// a name that never was a symbol is not found, and is interned anew
assert.equal(f.lookUp('no-such-name', 'cl-user'), 'NIL NIL');
assert.equal(f.internNew('fresh-name'), 'FRESH-NAME');
assert.equal(f.lookUp('fresh-name', 'cl-user'), 'FRESH-NAME :INTERNAL');
// a symbol left out is an error, not a symbol that is not the one meant:
// one of COMMON-LISP (inherited too), and the program's own
const leftOut = (e) => e.name === 'LispError' && /is not in this module: it was compiled with --shake-symbols/.test(e.message);
assert.throws(() => f.lookUp('pairlis', 'common-lisp'), leftOut);
assert.throws(() => f.lookUp('pairlis', 'cl-user'), leftOut);
assert.throws(() => f.internNew('pairlis'), leftOut);
assert.throws(() => f.lookUp('never-called', 'shapes'), leftOut);
assert.throws(() => f.readLength('(pairlis)'), leftOut);
// (DEFPACKAGE makes the symbols it exports: one nothing refers to is made anew)
assert.equal(f.lookUp('unused-export', 'shapes'), 'SHAPES:UNUSED-EXPORT :EXTERNAL');
assert.ok(f.symbolCount() > 100);
console.log('lib: symbols ok');
