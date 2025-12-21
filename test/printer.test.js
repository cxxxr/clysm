/**
 * Printer tests for CLYSM
 */

import { describe, it, before } from 'node:test';
import assert from 'node:assert/strict';
import { loadKernel } from '../js/bridge.js';
import { Reader } from '../js/reader.js';
import { Printer } from '../js/printer.js';

describe('Printer', () => {
  let kernel;
  let reader;
  let printer;

  before(async () => {
    kernel = await loadKernel();
    reader = new Reader(kernel);
    printer = new Printer(kernel);
  });

  describe('NIL and T', () => {
    it('prints NIL', () => {
      assert.strictEqual(printer.print(kernel.NIL), 'NIL');
    });

    it('prints T', () => {
      assert.strictEqual(printer.print(kernel.T), 'T');
    });
  });

  describe('Fixnums', () => {
    it('prints positive integers', () => {
      const num = kernel.makeFixnum(42);
      assert.strictEqual(printer.print(num), '42');
    });

    it('prints negative integers', () => {
      const num = kernel.makeFixnum(-17);
      assert.strictEqual(printer.print(num), '-17');
    });

    it('prints zero', () => {
      const num = kernel.makeFixnum(0);
      assert.strictEqual(printer.print(num), '0');
    });
  });

  describe('Characters', () => {
    it('prints regular characters', () => {
      const char = kernel.makeChar(65); // 'A'
      assert.strictEqual(printer.print(char), '#\\A');
    });

    it('prints space', () => {
      const char = kernel.makeChar(32);
      assert.strictEqual(printer.print(char), '#\\Space');
    });

    it('prints newline', () => {
      const char = kernel.makeChar(10);
      assert.strictEqual(printer.print(char), '#\\Newline');
    });

    it('prints tab', () => {
      const char = kernel.makeChar(9);
      assert.strictEqual(printer.print(char), '#\\Tab');
    });
  });

  describe('Strings', () => {
    it('prints simple strings', () => {
      const str = kernel.stringFromJS('hello');
      assert.strictEqual(printer.print(str), '"hello"');
    });

    it('prints empty strings', () => {
      const str = kernel.stringFromJS('');
      assert.strictEqual(printer.print(str), '""');
    });

    it('escapes quotes', () => {
      const str = kernel.stringFromJS('say "hi"');
      assert.strictEqual(printer.print(str), '"say \\"hi\\""');
    });

    it('escapes newlines', () => {
      const str = kernel.stringFromJS('a\nb');
      assert.strictEqual(printer.print(str), '"a\\nb"');
    });

    it('escapes backslashes', () => {
      const str = kernel.stringFromJS('a\\b');
      assert.strictEqual(printer.print(str), '"a\\\\b"');
    });
  });

  describe('Symbols', () => {
    it('prints simple symbols', () => {
      const sym = kernel.intern('FOO');
      assert.strictEqual(printer.print(sym), 'FOO');
    });

    it('prints keywords with colon', () => {
      const kw = kernel.internKeyword('TEST');
      assert.strictEqual(printer.print(kw), ':TEST');
    });

    it('prints symbols with special characters', () => {
      const sym = kernel.intern('FOO-BAR-BAZ');
      assert.strictEqual(printer.print(sym), 'FOO-BAR-BAZ');
    });
  });

  describe('Lists', () => {
    it('prints empty list as NIL', () => {
      assert.strictEqual(printer.print(kernel.NIL), 'NIL');
    });

    it('prints single element list', () => {
      const list = kernel.list(kernel.makeFixnum(42));
      assert.strictEqual(printer.print(list), '(42)');
    });

    it('prints multiple element list', () => {
      const list = kernel.list(
        kernel.makeFixnum(1),
        kernel.makeFixnum(2),
        kernel.makeFixnum(3)
      );
      assert.strictEqual(printer.print(list), '(1 2 3)');
    });

    it('prints nested lists', () => {
      const inner = kernel.list(kernel.makeFixnum(1), kernel.makeFixnum(2));
      const outer = kernel.list(inner, kernel.makeFixnum(3));
      assert.strictEqual(printer.print(outer), '((1 2) 3)');
    });

    it('prints dotted pairs', () => {
      const pair = kernel.cons(kernel.intern('A'), kernel.intern('B'));
      assert.strictEqual(printer.print(pair), '(A . B)');
    });

    it('prints improper lists', () => {
      const list = kernel.cons(
        kernel.makeFixnum(1),
        kernel.cons(
          kernel.makeFixnum(2),
          kernel.makeFixnum(3)
        )
      );
      assert.strictEqual(printer.print(list), '(1 2 . 3)');
    });

    it('prints mixed type lists', () => {
      const list = kernel.list(
        kernel.makeFixnum(42),
        kernel.intern('FOO'),
        kernel.stringFromJS('bar')
      );
      assert.strictEqual(printer.print(list), '(42 FOO "bar")');
    });
  });

  describe('Vectors', () => {
    it('prints empty vector', () => {
      const vec = kernel.makeVector(0);
      assert.strictEqual(printer.print(vec), '#()');
    });

    it('prints vector with elements', () => {
      const vec = kernel.makeVector(3);
      kernel.svset(vec, 0, kernel.makeFixnum(1));
      kernel.svset(vec, 1, kernel.makeFixnum(2));
      kernel.svset(vec, 2, kernel.makeFixnum(3));
      assert.strictEqual(printer.print(vec), '#(1 2 3)');
    });

    it('prints vector with NIL elements', () => {
      const vec = kernel.makeVector(2);
      assert.strictEqual(printer.print(vec), '#(NIL NIL)');
    });
  });

  describe('Packages', () => {
    it('prints CL-USER package', () => {
      const result = printer.print(kernel.CL_USER);
      assert.strictEqual(result, '#<PACKAGE "COMMON-LISP-USER">');
    });

    it('prints KEYWORD package', () => {
      const result = printer.print(kernel.KEYWORD);
      assert.strictEqual(result, '#<PACKAGE "KEYWORD">');
    });
  });

  describe('Unbound', () => {
    it('prints unbound marker', () => {
      assert.strictEqual(printer.print(kernel.UNBOUND), '#<UNBOUND>');
    });
  });

  describe('Read-print roundtrip', () => {
    it('roundtrips integers', () => {
      const original = '42';
      const obj = reader.readFromString(original);
      const printed = printer.print(obj);
      assert.strictEqual(printed, original);
    });

    it('roundtrips symbols', () => {
      const original = 'FOO';
      const obj = reader.readFromString('foo');
      const printed = printer.print(obj);
      assert.strictEqual(printed, original);
    });

    it('roundtrips simple lists', () => {
      const original = '(1 2 3)';
      const obj = reader.readFromString(original);
      const printed = printer.print(obj);
      assert.strictEqual(printed, original);
    });

    it('roundtrips nested lists', () => {
      const original = '((1 2) (3 4))';
      const obj = reader.readFromString(original);
      const printed = printer.print(obj);
      assert.strictEqual(printed, original);
    });

    it('roundtrips quoted expressions', () => {
      const obj = reader.readFromString("'foo");
      const printed = printer.print(obj);
      assert.strictEqual(printed, '(QUOTE FOO)');
    });

    it('roundtrips keywords', () => {
      const original = ':TEST';
      const obj = reader.readFromString(':test');
      const printed = printer.print(obj);
      assert.strictEqual(printed, original);
    });

    it('roundtrips strings', () => {
      const original = '"hello world"';
      const obj = reader.readFromString(original);
      const printed = printer.print(obj);
      assert.strictEqual(printed, original);
    });

    it('roundtrips dotted pairs', () => {
      const original = '(A . B)';
      const obj = reader.readFromString('(a . b)');
      const printed = printer.print(obj);
      assert.strictEqual(printed, original);
    });
  });

  describe('princ', () => {
    it('prints strings without quotes', () => {
      const str = kernel.stringFromJS('hello');
      assert.strictEqual(printer.princ(str), 'hello');
    });

    it('prints characters without prefix', () => {
      const char = kernel.makeChar(65);
      assert.strictEqual(printer.princ(char), 'A');
    });

    it('prints symbols normally', () => {
      const sym = kernel.intern('FOO');
      assert.strictEqual(printer.princ(sym), 'FOO');
    });
  });
});
