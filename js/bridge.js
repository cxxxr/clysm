/**
 * CLYSM Kernel Bridge - JavaScript interface for WebAssembly Lisp kernel
 */

import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Load the CLYSM kernel WebAssembly module
 * @returns {Promise<LispKernel>}
 */
export async function loadKernel() {
  const wasmPath = join(__dirname, '..', 'build', 'kernel.wasm');
  const wasmBytes = await readFile(wasmPath);

  const imports = {
    env: {
      // Placeholder for future JS callbacks
    }
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);

  return new LispKernel(instance.exports);
}

/**
 * LispKernel class - High-level interface to the CLYSM kernel
 */
export class LispKernel {
  constructor(exports) {
    this.exports = exports;
    this.NIL = exports.get_nil();
    this.T = exports.get_t();
  }

  // === Fixnum operations ===

  makeFixnum(n) {
    return this.exports.make_fixnum(n);
  }

  fixnumValue(x) {
    return this.exports.fixnum_value(x);
  }

  isFixnum(x) {
    return this.exports.fixnump(x) === 1;
  }

  // === Character operations ===

  makeChar(codePoint) {
    return this.exports.make_char(codePoint);
  }

  charCode(x) {
    return this.exports.char_code(x);
  }

  isCharacter(x) {
    return this.exports.characterp(x) === 1;
  }

  // === Cons operations ===

  cons(car, cdr) {
    return this.exports.cons(car, cdr);
  }

  car(x) {
    return this.exports.car(x);
  }

  cdr(x) {
    return this.exports.cdr(x);
  }

  isCons(x) {
    return this.exports.consp(x) === 1;
  }

  rplaca(cell, val) {
    return this.exports.rplaca(cell, val);
  }

  rplacd(cell, val) {
    return this.exports.rplacd(cell, val);
  }

  // === Symbol operations ===

  isSymbol(x) {
    return this.exports.symbolp(x) === 1;
  }

  isNull(x) {
    return this.exports.null_(x) === 1;
  }

  symbolValue(sym) {
    return this.exports.symbol_value(sym);
  }

  setSymbolValue(sym, val) {
    return this.exports.set_symbol_value(sym, val);
  }

  symbolFunction(sym) {
    return this.exports.symbol_function(sym);
  }

  setSymbolFunction(sym, val) {
    return this.exports.set_symbol_function(sym, val);
  }

  // === Comparison ===

  eq(a, b) {
    return this.exports.eq(a, b) === 1;
  }

  // === Arithmetic ===

  fxAdd(a, b) {
    return this.exports.fx_add(a, b);
  }

  fxSub(a, b) {
    return this.exports.fx_sub(a, b);
  }

  fxMul(a, b) {
    return this.exports.fx_mul(a, b);
  }

  fxLt(a, b) {
    return this.exports.fx_lt(a, b) === 1;
  }

  fxLe(a, b) {
    return this.exports.fx_le(a, b) === 1;
  }

  fxGt(a, b) {
    return this.exports.fx_gt(a, b) === 1;
  }

  fxGe(a, b) {
    return this.exports.fx_ge(a, b) === 1;
  }

  fxEq(a, b) {
    return this.exports.fx_eq(a, b) === 1;
  }

  // === String operations ===

  makeString(length) {
    return this.exports.make_string(length);
  }

  stringLength(s) {
    return this.exports.string_length(s);
  }

  isString(x) {
    return this.exports.stringp(x) === 1;
  }

  schar(s, i) {
    return this.exports.schar(s, i);
  }

  scharSet(s, i, c) {
    return this.exports.schar_set(s, i, c);
  }

  /**
   * Create a string from a JavaScript string
   */
  stringFromJS(jsString) {
    const encoder = new TextEncoder();
    const bytes = encoder.encode(jsString);
    const s = this.makeString(bytes.length);
    for (let i = 0; i < bytes.length; i++) {
      this.scharSet(s, i, bytes[i]);
    }
    return s;
  }

  /**
   * Convert a Lisp string to JavaScript string
   */
  stringToJS(s) {
    const len = this.stringLength(s);
    const bytes = new Uint8Array(len);
    for (let i = 0; i < len; i++) {
      bytes[i] = this.schar(s, i);
    }
    const decoder = new TextDecoder();
    return decoder.decode(bytes);
  }

  // === Vector operations ===

  makeVector(length) {
    return this.exports.make_vector(length);
  }

  vectorLength(v) {
    return this.exports.vector_length(v);
  }

  isVector(x) {
    return this.exports.vectorp(x) === 1;
  }

  svref(v, i) {
    return this.exports.svref(v, i);
  }

  svset(v, i, val) {
    return this.exports.svset(v, i, val);
  }

  // === Utilities ===

  /**
   * Create a list from an array of values
   */
  list(...args) {
    let result = this.NIL;
    for (let i = args.length - 1; i >= 0; i--) {
      result = this.cons(args[i], result);
    }
    return result;
  }

  /**
   * Convert a list to a JavaScript array
   */
  toArray(list) {
    const result = [];
    let current = list;
    while (!this.isNull(current)) {
      if (!this.isCons(current)) {
        throw new Error('Not a proper list');
      }
      result.push(this.car(current));
      current = this.cdr(current);
    }
    return result;
  }
}

export default { loadKernel, LispKernel };
