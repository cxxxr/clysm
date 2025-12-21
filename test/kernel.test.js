/**
 * CLYSM Kernel Tests - Node.js built-in test runner
 */

import { describe, it, before } from 'node:test';
import assert from 'node:assert/strict';
import { loadKernel } from '../js/bridge.js';

describe('Kernel Types', () => {
  let kernel;

  before(async () => {
    kernel = await loadKernel();
  });

  describe('Fixnum', () => {
    it('creates fixnum from i32', () => {
      const n = kernel.makeFixnum(42);
      assert.strictEqual(kernel.isFixnum(n), true);
    });

    it('converts back to i32', () => {
      const n = kernel.makeFixnum(42);
      assert.strictEqual(kernel.fixnumValue(n), 42);
    });

    it('handles negative numbers', () => {
      const n = kernel.makeFixnum(-100);
      assert.strictEqual(kernel.fixnumValue(n), -100);
    });

    it('handles zero', () => {
      const n = kernel.makeFixnum(0);
      assert.strictEqual(kernel.fixnumValue(n), 0);
    });

    it('handles large positive numbers', () => {
      const n = kernel.makeFixnum(1000000);
      assert.strictEqual(kernel.fixnumValue(n), 1000000);
    });

    it('handles large negative numbers', () => {
      const n = kernel.makeFixnum(-1000000);
      assert.strictEqual(kernel.fixnumValue(n), -1000000);
    });
  });

  describe('Character', () => {
    it('creates character from code point', () => {
      const c = kernel.makeChar(65); // 'A'
      assert.strictEqual(kernel.isCharacter(c), true);
    });

    it('is not a fixnum', () => {
      const c = kernel.makeChar(65);
      assert.strictEqual(kernel.isFixnum(c), false);
    });

    it('converts back to code point', () => {
      const c = kernel.makeChar(0x3042); // 'あ'
      assert.strictEqual(kernel.charCode(c), 0x3042);
    });

    it('handles ASCII characters', () => {
      const c = kernel.makeChar(97); // 'a'
      assert.strictEqual(kernel.charCode(c), 97);
    });

    it('handles emoji code points', () => {
      const c = kernel.makeChar(0x1F600); // 😀
      assert.strictEqual(kernel.charCode(c), 0x1F600);
    });
  });

  describe('Cons', () => {
    it('creates cons cell', () => {
      const a = kernel.makeFixnum(1);
      const b = kernel.makeFixnum(2);
      const cell = kernel.cons(a, b);
      assert.strictEqual(kernel.isCons(cell), true);
    });

    it('car returns first element', () => {
      const a = kernel.makeFixnum(1);
      const b = kernel.makeFixnum(2);
      const cell = kernel.cons(a, b);
      const car = kernel.car(cell);
      assert.strictEqual(kernel.fixnumValue(car), 1);
    });

    it('cdr returns second element', () => {
      const a = kernel.makeFixnum(1);
      const b = kernel.makeFixnum(2);
      const cell = kernel.cons(a, b);
      const cdr = kernel.cdr(cell);
      assert.strictEqual(kernel.fixnumValue(cdr), 2);
    });

    it('car of NIL returns NIL', () => {
      const car = kernel.car(kernel.NIL);
      assert.strictEqual(kernel.isNull(car), true);
    });

    it('cdr of NIL returns NIL', () => {
      const cdr = kernel.cdr(kernel.NIL);
      assert.strictEqual(kernel.isNull(cdr), true);
    });

    it('rplaca modifies car', () => {
      const a = kernel.makeFixnum(1);
      const b = kernel.makeFixnum(2);
      const cell = kernel.cons(a, b);
      const newCar = kernel.makeFixnum(10);
      kernel.rplaca(cell, newCar);
      assert.strictEqual(kernel.fixnumValue(kernel.car(cell)), 10);
    });

    it('rplacd modifies cdr', () => {
      const a = kernel.makeFixnum(1);
      const b = kernel.makeFixnum(2);
      const cell = kernel.cons(a, b);
      const newCdr = kernel.makeFixnum(20);
      kernel.rplacd(cell, newCdr);
      assert.strictEqual(kernel.fixnumValue(kernel.cdr(cell)), 20);
    });
  });

  describe('Symbol', () => {
    it('NIL is a symbol', () => {
      assert.strictEqual(kernel.isSymbol(kernel.NIL), true);
    });

    it('null returns true for NIL', () => {
      assert.strictEqual(kernel.isNull(kernel.NIL), true);
    });

    it('T is a symbol', () => {
      assert.strictEqual(kernel.isSymbol(kernel.T), true);
    });

    it('T is not null', () => {
      assert.strictEqual(kernel.isNull(kernel.T), false);
    });

    it('NIL value is NIL itself', () => {
      const val = kernel.symbolValue(kernel.NIL);
      assert.strictEqual(kernel.eq(val, kernel.NIL), true);
    });

    it('T value is T itself', () => {
      const val = kernel.symbolValue(kernel.T);
      assert.strictEqual(kernel.eq(val, kernel.T), true);
    });
  });

  describe('Arithmetic', () => {
    it('adds two fixnums', () => {
      const a = kernel.makeFixnum(10);
      const b = kernel.makeFixnum(20);
      const result = kernel.fxAdd(a, b);
      assert.strictEqual(kernel.fixnumValue(result), 30);
    });

    it('subtracts two fixnums', () => {
      const a = kernel.makeFixnum(30);
      const b = kernel.makeFixnum(10);
      const result = kernel.fxSub(a, b);
      assert.strictEqual(kernel.fixnumValue(result), 20);
    });

    it('multiplies two fixnums', () => {
      const a = kernel.makeFixnum(6);
      const b = kernel.makeFixnum(7);
      const result = kernel.fxMul(a, b);
      assert.strictEqual(kernel.fixnumValue(result), 42);
    });

    it('handles negative addition', () => {
      const a = kernel.makeFixnum(-10);
      const b = kernel.makeFixnum(20);
      const result = kernel.fxAdd(a, b);
      assert.strictEqual(kernel.fixnumValue(result), 10);
    });

    it('handles negative subtraction', () => {
      const a = kernel.makeFixnum(10);
      const b = kernel.makeFixnum(20);
      const result = kernel.fxSub(a, b);
      assert.strictEqual(kernel.fixnumValue(result), -10);
    });
  });

  describe('Comparisons', () => {
    it('fx_lt returns true when a < b', () => {
      const a = kernel.makeFixnum(5);
      const b = kernel.makeFixnum(10);
      assert.strictEqual(kernel.fxLt(a, b), true);
    });

    it('fx_lt returns false when a >= b', () => {
      const a = kernel.makeFixnum(10);
      const b = kernel.makeFixnum(5);
      assert.strictEqual(kernel.fxLt(a, b), false);
    });

    it('fx_le returns true when a <= b', () => {
      const a = kernel.makeFixnum(5);
      const b = kernel.makeFixnum(5);
      assert.strictEqual(kernel.fxLe(a, b), true);
    });

    it('fx_gt returns true when a > b', () => {
      const a = kernel.makeFixnum(10);
      const b = kernel.makeFixnum(5);
      assert.strictEqual(kernel.fxGt(a, b), true);
    });

    it('fx_ge returns true when a >= b', () => {
      const a = kernel.makeFixnum(5);
      const b = kernel.makeFixnum(5);
      assert.strictEqual(kernel.fxGe(a, b), true);
    });

    it('fx_eq returns true when a == b', () => {
      const a = kernel.makeFixnum(42);
      const b = kernel.makeFixnum(42);
      assert.strictEqual(kernel.fxEq(a, b), true);
    });

    it('fx_eq returns false when a != b', () => {
      const a = kernel.makeFixnum(42);
      const b = kernel.makeFixnum(43);
      assert.strictEqual(kernel.fxEq(a, b), false);
    });
  });

  describe('eq', () => {
    it('same fixnum values are eq', () => {
      const a = kernel.makeFixnum(42);
      const b = kernel.makeFixnum(42);
      assert.strictEqual(kernel.eq(a, b), true);
    });

    it('different fixnums are not eq', () => {
      const a = kernel.makeFixnum(1);
      const b = kernel.makeFixnum(2);
      assert.strictEqual(kernel.eq(a, b), false);
    });

    it('NIL is eq to NIL', () => {
      assert.strictEqual(kernel.eq(kernel.NIL, kernel.NIL), true);
    });

    it('T is eq to T', () => {
      assert.strictEqual(kernel.eq(kernel.T, kernel.T), true);
    });

    it('NIL is not eq to T', () => {
      assert.strictEqual(kernel.eq(kernel.NIL, kernel.T), false);
    });

    it('cons cells are not eq even with same contents', () => {
      const a = kernel.makeFixnum(1);
      const cell1 = kernel.cons(a, kernel.NIL);
      const cell2 = kernel.cons(a, kernel.NIL);
      assert.strictEqual(kernel.eq(cell1, cell2), false);
    });

    it('same cons cell is eq to itself', () => {
      const a = kernel.makeFixnum(1);
      const cell = kernel.cons(a, kernel.NIL);
      assert.strictEqual(kernel.eq(cell, cell), true);
    });
  });

  describe('String', () => {
    it('creates a string of given length', () => {
      const s = kernel.makeString(10);
      assert.strictEqual(kernel.isString(s), true);
    });

    it('returns correct length', () => {
      const s = kernel.makeString(5);
      assert.strictEqual(kernel.stringLength(s), 5);
    });

    it('is not a vector', () => {
      const s = kernel.makeString(5);
      assert.strictEqual(kernel.isVector(s), false);
    });

    it('sets and gets characters', () => {
      const s = kernel.makeString(3);
      kernel.scharSet(s, 0, 65); // 'A'
      kernel.scharSet(s, 1, 66); // 'B'
      kernel.scharSet(s, 2, 67); // 'C'
      assert.strictEqual(kernel.schar(s, 0), 65);
      assert.strictEqual(kernel.schar(s, 1), 66);
      assert.strictEqual(kernel.schar(s, 2), 67);
    });

    it('converts from JavaScript string', () => {
      const s = kernel.stringFromJS('hello');
      assert.strictEqual(kernel.stringLength(s), 5);
      assert.strictEqual(kernel.schar(s, 0), 104); // 'h'
      assert.strictEqual(kernel.schar(s, 1), 101); // 'e'
    });

    it('converts to JavaScript string', () => {
      const s = kernel.stringFromJS('world');
      const js = kernel.stringToJS(s);
      assert.strictEqual(js, 'world');
    });

    it('handles UTF-8 encoding', () => {
      const s = kernel.stringFromJS('日本語');
      const js = kernel.stringToJS(s);
      assert.strictEqual(js, '日本語');
    });

    it('handles empty string', () => {
      const s = kernel.makeString(0);
      assert.strictEqual(kernel.stringLength(s), 0);
      assert.strictEqual(kernel.stringToJS(s), '');
    });
  });

  describe('Vector', () => {
    it('creates a vector of given length', () => {
      const v = kernel.makeVector(10);
      assert.strictEqual(kernel.isVector(v), true);
    });

    it('returns correct length', () => {
      const v = kernel.makeVector(5);
      assert.strictEqual(kernel.vectorLength(v), 5);
    });

    it('is not a string', () => {
      const v = kernel.makeVector(5);
      assert.strictEqual(kernel.isString(v), false);
    });

    it('initializes elements to NIL', () => {
      const v = kernel.makeVector(3);
      assert.strictEqual(kernel.isNull(kernel.svref(v, 0)), true);
      assert.strictEqual(kernel.isNull(kernel.svref(v, 1)), true);
      assert.strictEqual(kernel.isNull(kernel.svref(v, 2)), true);
    });

    it('sets and gets elements', () => {
      const v = kernel.makeVector(3);
      const a = kernel.makeFixnum(10);
      const b = kernel.makeFixnum(20);
      const c = kernel.makeFixnum(30);

      kernel.svset(v, 0, a);
      kernel.svset(v, 1, b);
      kernel.svset(v, 2, c);

      assert.strictEqual(kernel.fixnumValue(kernel.svref(v, 0)), 10);
      assert.strictEqual(kernel.fixnumValue(kernel.svref(v, 1)), 20);
      assert.strictEqual(kernel.fixnumValue(kernel.svref(v, 2)), 30);
    });

    it('can hold mixed types', () => {
      const v = kernel.makeVector(4);
      const num = kernel.makeFixnum(42);
      const char = kernel.makeChar(65);
      const cons = kernel.cons(kernel.makeFixnum(1), kernel.makeFixnum(2));

      kernel.svset(v, 0, num);
      kernel.svset(v, 1, char);
      kernel.svset(v, 2, cons);
      kernel.svset(v, 3, kernel.T);

      assert.strictEqual(kernel.isFixnum(kernel.svref(v, 0)), true);
      assert.strictEqual(kernel.isCharacter(kernel.svref(v, 1)), true);
      assert.strictEqual(kernel.isCons(kernel.svref(v, 2)), true);
      assert.strictEqual(kernel.isSymbol(kernel.svref(v, 3)), true);
    });

    it('handles empty vector', () => {
      const v = kernel.makeVector(0);
      assert.strictEqual(kernel.vectorLength(v), 0);
    });
  });

  describe('List utilities', () => {
    it('creates an empty list', () => {
      const list = kernel.list();
      assert.strictEqual(kernel.isNull(list), true);
    });

    it('creates a single element list', () => {
      const a = kernel.makeFixnum(1);
      const list = kernel.list(a);
      assert.strictEqual(kernel.isCons(list), true);
      assert.strictEqual(kernel.fixnumValue(kernel.car(list)), 1);
      assert.strictEqual(kernel.isNull(kernel.cdr(list)), true);
    });

    it('creates a multi-element list', () => {
      const a = kernel.makeFixnum(1);
      const b = kernel.makeFixnum(2);
      const c = kernel.makeFixnum(3);
      const list = kernel.list(a, b, c);

      assert.strictEqual(kernel.fixnumValue(kernel.car(list)), 1);
      assert.strictEqual(kernel.fixnumValue(kernel.car(kernel.cdr(list))), 2);
      assert.strictEqual(kernel.fixnumValue(kernel.car(kernel.cdr(kernel.cdr(list)))), 3);
      assert.strictEqual(kernel.isNull(kernel.cdr(kernel.cdr(kernel.cdr(list)))), true);
    });

    it('converts list to array', () => {
      const a = kernel.makeFixnum(10);
      const b = kernel.makeFixnum(20);
      const c = kernel.makeFixnum(30);
      const list = kernel.list(a, b, c);

      const arr = kernel.toArray(list);
      assert.strictEqual(arr.length, 3);
      assert.strictEqual(kernel.fixnumValue(arr[0]), 10);
      assert.strictEqual(kernel.fixnumValue(arr[1]), 20);
      assert.strictEqual(kernel.fixnumValue(arr[2]), 30);
    });

    it('converts empty list to empty array', () => {
      const arr = kernel.toArray(kernel.NIL);
      assert.strictEqual(arr.length, 0);
    });
  });
});
