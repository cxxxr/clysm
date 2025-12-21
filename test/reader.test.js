/**
 * Reader tests for CLYSM
 */

import { describe, it, before } from 'node:test';
import assert from 'node:assert/strict';
import { loadKernel } from '../js/bridge.js';
import { Reader, Tokenizer, TokenType } from '../js/reader.js';

describe('Reader', () => {
  let kernel;
  let reader;

  before(async () => {
    kernel = await loadKernel();
    reader = new Reader(kernel);
  });

  describe('Tokenizer', () => {
    it('tokenizes empty input', () => {
      const tokenizer = new Tokenizer('');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.EOF);
    });

    it('tokenizes parentheses', () => {
      const tokenizer = new Tokenizer('()');
      assert.strictEqual(tokenizer.nextToken().type, TokenType.LPAREN);
      assert.strictEqual(tokenizer.nextToken().type, TokenType.RPAREN);
      assert.strictEqual(tokenizer.nextToken().type, TokenType.EOF);
    });

    it('tokenizes quote', () => {
      const tokenizer = new Tokenizer("'x");
      assert.strictEqual(tokenizer.nextToken().type, TokenType.QUOTE);
      assert.deepEqual(tokenizer.nextToken(), { type: TokenType.SYMBOL, value: 'X' });
    });

    it('tokenizes positive numbers', () => {
      const tokenizer = new Tokenizer('42');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.NUMBER);
      assert.strictEqual(token.value, 42);
    });

    it('tokenizes negative numbers', () => {
      const tokenizer = new Tokenizer('-17');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.NUMBER);
      assert.strictEqual(token.value, -17);
    });

    it('tokenizes symbols', () => {
      const tokenizer = new Tokenizer('foo');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.SYMBOL);
      assert.strictEqual(token.value, 'FOO');
    });

    it('uppercases symbols', () => {
      const tokenizer = new Tokenizer('FooBar');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.value, 'FOOBAR');
    });

    it('tokenizes + as symbol', () => {
      const tokenizer = new Tokenizer('+');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.SYMBOL);
      assert.strictEqual(token.value, '+');
    });

    it('tokenizes - as symbol', () => {
      const tokenizer = new Tokenizer('-');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.SYMBOL);
      assert.strictEqual(token.value, '-');
    });

    it('tokenizes strings', () => {
      const tokenizer = new Tokenizer('"hello"');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.STRING);
      assert.strictEqual(token.value, 'hello');
    });

    it('handles escape sequences in strings', () => {
      const tokenizer = new Tokenizer('"a\\nb"');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.value, 'a\nb');
    });

    it('tokenizes dot', () => {
      const tokenizer = new Tokenizer('.');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.DOT);
    });

    it('skips whitespace', () => {
      const tokenizer = new Tokenizer('  \t\n  foo');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.SYMBOL);
      assert.strictEqual(token.value, 'FOO');
    });

    it('skips comments', () => {
      const tokenizer = new Tokenizer('; comment\nfoo');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.SYMBOL);
      assert.strictEqual(token.value, 'FOO');
    });

    it('tokenizes multiple tokens', () => {
      const tokenizer = new Tokenizer('(+ 1 2)');
      assert.strictEqual(tokenizer.nextToken().type, TokenType.LPAREN);
      assert.deepEqual(tokenizer.nextToken(), { type: TokenType.SYMBOL, value: '+' });
      assert.deepEqual(tokenizer.nextToken(), { type: TokenType.NUMBER, value: 1 });
      assert.deepEqual(tokenizer.nextToken(), { type: TokenType.NUMBER, value: 2 });
      assert.strictEqual(tokenizer.nextToken().type, TokenType.RPAREN);
      assert.strictEqual(tokenizer.nextToken().type, TokenType.EOF);
    });

    it('tokenizes keywords', () => {
      const tokenizer = new Tokenizer(':test');
      const token = tokenizer.nextToken();
      assert.strictEqual(token.type, TokenType.SYMBOL);
      assert.strictEqual(token.value, ':TEST');
    });

    it('tokenizes backquote', () => {
      const tokenizer = new Tokenizer('`x');
      assert.strictEqual(tokenizer.nextToken().type, TokenType.BACKQUOTE);
    });

    it('tokenizes comma', () => {
      const tokenizer = new Tokenizer(',x');
      assert.strictEqual(tokenizer.nextToken().type, TokenType.COMMA);
    });

    it('tokenizes comma-at', () => {
      const tokenizer = new Tokenizer(',@x');
      assert.strictEqual(tokenizer.nextToken().type, TokenType.COMMA_AT);
    });
  });

  describe('Numbers', () => {
    it('reads positive integers', () => {
      const result = reader.readFromString('42');
      assert.strictEqual(kernel.isFixnum(result), true);
      assert.strictEqual(kernel.fixnumValue(result), 42);
    });

    it('reads negative integers', () => {
      const result = reader.readFromString('-17');
      assert.strictEqual(kernel.isFixnum(result), true);
      assert.strictEqual(kernel.fixnumValue(result), -17);
    });

    it('reads zero', () => {
      const result = reader.readFromString('0');
      assert.strictEqual(kernel.fixnumValue(result), 0);
    });

    it('reads large numbers', () => {
      const result = reader.readFromString('123456');
      assert.strictEqual(kernel.fixnumValue(result), 123456);
    });
  });

  describe('Symbols', () => {
    it('reads simple symbols', () => {
      const result = reader.readFromString('foo');
      assert.strictEqual(kernel.isSymbol(result), true);
      const name = kernel.stringToJS(kernel.symbolName(result));
      assert.strictEqual(name, 'FOO');
    });

    it('reads NIL as the NIL symbol', () => {
      const result = reader.readFromString('nil');
      assert.strictEqual(kernel.eq(result, kernel.NIL), true);
    });

    it('reads T as the T symbol', () => {
      const result = reader.readFromString('t');
      assert.strictEqual(kernel.eq(result, kernel.T), true);
    });

    it('interns symbols correctly', () => {
      const result1 = reader.readFromString('same-symbol');
      const result2 = reader.readFromString('same-symbol');
      assert.strictEqual(kernel.eq(result1, result2), true);
    });

    it('reads + symbol', () => {
      const result = reader.readFromString('+');
      assert.strictEqual(kernel.isSymbol(result), true);
      const name = kernel.stringToJS(kernel.symbolName(result));
      assert.strictEqual(name, '+');
    });

    it('reads - symbol', () => {
      const result = reader.readFromString('-');
      assert.strictEqual(kernel.isSymbol(result), true);
      const name = kernel.stringToJS(kernel.symbolName(result));
      assert.strictEqual(name, '-');
    });

    it('reads symbols with special characters', () => {
      const result = reader.readFromString('foo-bar-baz');
      const name = kernel.stringToJS(kernel.symbolName(result));
      assert.strictEqual(name, 'FOO-BAR-BAZ');
    });
  });

  describe('Keywords', () => {
    it('reads keyword symbols', () => {
      const result = reader.readFromString(':test');
      assert.strictEqual(kernel.isSymbol(result), true);
      const name = kernel.stringToJS(kernel.symbolName(result));
      assert.strictEqual(name, 'TEST');
    });

    it('keywords belong to KEYWORD package', () => {
      const result = reader.readFromString(':my-key');
      const pkg = kernel.symbolPackage(result);
      assert.strictEqual(kernel.eq(pkg, kernel.KEYWORD), true);
    });

    it('keywords evaluate to themselves', () => {
      const result = reader.readFromString(':self');
      const value = kernel.symbolValue(result);
      assert.strictEqual(kernel.eq(result, value), true);
    });
  });

  describe('Strings', () => {
    it('reads simple strings', () => {
      const result = reader.readFromString('"hello"');
      assert.strictEqual(kernel.isString(result), true);
      assert.strictEqual(kernel.stringToJS(result), 'hello');
    });

    it('reads empty strings', () => {
      const result = reader.readFromString('""');
      assert.strictEqual(kernel.stringToJS(result), '');
    });

    it('handles escape sequences', () => {
      const result = reader.readFromString('"a\\nb\\tc"');
      assert.strictEqual(kernel.stringToJS(result), 'a\nb\tc');
    });

    it('handles escaped quotes', () => {
      const result = reader.readFromString('"say \\"hi\\""');
      assert.strictEqual(kernel.stringToJS(result), 'say "hi"');
    });
  });

  describe('Lists', () => {
    it('reads empty list as NIL', () => {
      const result = reader.readFromString('()');
      assert.strictEqual(kernel.eq(result, kernel.NIL), true);
    });

    it('reads single element list', () => {
      const result = reader.readFromString('(42)');
      assert.strictEqual(kernel.isCons(result), true);
      assert.strictEqual(kernel.fixnumValue(kernel.car(result)), 42);
      assert.strictEqual(kernel.eq(kernel.cdr(result), kernel.NIL), true);
    });

    it('reads multiple element list', () => {
      const result = reader.readFromString('(1 2 3)');
      const arr = kernel.toArray(result);
      assert.strictEqual(arr.length, 3);
      assert.strictEqual(kernel.fixnumValue(arr[0]), 1);
      assert.strictEqual(kernel.fixnumValue(arr[1]), 2);
      assert.strictEqual(kernel.fixnumValue(arr[2]), 3);
    });

    it('reads nested lists', () => {
      const result = reader.readFromString('((1 2) (3 4))');
      assert.strictEqual(kernel.isCons(result), true);
      const first = kernel.car(result);
      assert.strictEqual(kernel.isCons(first), true);
      assert.strictEqual(kernel.fixnumValue(kernel.car(first)), 1);
    });

    it('reads dotted pairs', () => {
      const result = reader.readFromString('(a . b)');
      assert.strictEqual(kernel.isCons(result), true);
      const car = kernel.car(result);
      const cdr = kernel.cdr(result);
      assert.strictEqual(kernel.isSymbol(car), true);
      assert.strictEqual(kernel.isSymbol(cdr), true);
      assert.strictEqual(kernel.stringToJS(kernel.symbolName(car)), 'A');
      assert.strictEqual(kernel.stringToJS(kernel.symbolName(cdr)), 'B');
    });

    it('reads improper lists', () => {
      const result = reader.readFromString('(1 2 . 3)');
      assert.strictEqual(kernel.fixnumValue(kernel.car(result)), 1);
      const cdr1 = kernel.cdr(result);
      assert.strictEqual(kernel.fixnumValue(kernel.car(cdr1)), 2);
      const cdr2 = kernel.cdr(cdr1);
      assert.strictEqual(kernel.fixnumValue(cdr2), 3);
    });

    it('reads complex S-expression', () => {
      const result = reader.readFromString('(+ (* 2 3) (- 10 5))');
      assert.strictEqual(kernel.isCons(result), true);
      const op = kernel.car(result);
      assert.strictEqual(kernel.stringToJS(kernel.symbolName(op)), '+');
    });
  });

  describe('Quote', () => {
    it('reads quoted symbol', () => {
      const result = reader.readFromString("'foo");
      assert.strictEqual(kernel.isCons(result), true);
      const quoteSym = kernel.car(result);
      assert.strictEqual(kernel.stringToJS(kernel.symbolName(quoteSym)), 'QUOTE');
      const quoted = kernel.car(kernel.cdr(result));
      assert.strictEqual(kernel.stringToJS(kernel.symbolName(quoted)), 'FOO');
    });

    it('reads quoted list', () => {
      const result = reader.readFromString("'(1 2 3)");
      assert.strictEqual(kernel.isCons(result), true);
      const quoteSym = kernel.car(result);
      assert.strictEqual(kernel.stringToJS(kernel.symbolName(quoteSym)), 'QUOTE');
      const quotedList = kernel.car(kernel.cdr(result));
      assert.strictEqual(kernel.isCons(quotedList), true);
    });

    it('reads nested quotes', () => {
      const result = reader.readFromString("''x");
      assert.strictEqual(kernel.isCons(result), true);
      const outerQuote = kernel.car(result);
      assert.strictEqual(kernel.stringToJS(kernel.symbolName(outerQuote)), 'QUOTE');
    });
  });

  describe('Comments', () => {
    it('ignores line comments', () => {
      const result = reader.readFromString('; this is a comment\n42');
      assert.strictEqual(kernel.fixnumValue(result), 42);
    });

    it('ignores multiple comments', () => {
      const result = reader.readFromString(`
        ; comment 1
        ; comment 2
        (foo) ; inline comment
      `);
      assert.strictEqual(kernel.isCons(result), true);
    });

    it('ignores trailing comments', () => {
      const result = reader.readFromString('42 ; trailing');
      assert.strictEqual(kernel.fixnumValue(result), 42);
    });
  });

  describe('Error handling', () => {
    it('throws on unexpected )', () => {
      assert.throws(() => reader.readFromString(')'), SyntaxError);
    });

    it('throws on unterminated list', () => {
      assert.throws(() => reader.readFromString('(1 2 3'), SyntaxError);
    });

    it('throws on unterminated string', () => {
      const tokenizer = new Tokenizer('"unterminated');
      assert.throws(() => tokenizer.nextToken(), SyntaxError);
    });

    it('throws on nothing before dot', () => {
      assert.throws(() => reader.readFromString('(. x)'), SyntaxError);
    });

    it('throws on multiple elements after dot', () => {
      assert.throws(() => reader.readFromString('(a . b c)'), SyntaxError);
    });

    it('throws on unexpected EOF', () => {
      assert.throws(() => reader.readFromString(''), SyntaxError);
    });
  });

  describe('Whitespace handling', () => {
    it('handles extra whitespace', () => {
      const result = reader.readFromString('  (  1   2   3  )  ');
      const arr = kernel.toArray(result);
      assert.strictEqual(arr.length, 3);
    });

    it('handles tabs and newlines', () => {
      const result = reader.readFromString('(\n\t1\n\t2\n)');
      const arr = kernel.toArray(result);
      assert.strictEqual(arr.length, 2);
    });
  });
});
