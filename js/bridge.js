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

  /** @type {any} */
  let exports;

  const stringToJS = (s) => {
    const len = exports.string_length(s);
    const bytes = new Uint8Array(len);
    for (let i = 0; i < len; i++) {
      bytes[i] = exports.schar(s, i);
    }
    return new TextDecoder().decode(bytes);
  };

  const typeNameOf = (x) => {
    if (exports.null_(x) === 1) return 'NIL';
    if (exports.fixnump(x) === 1) return 'FIXNUM';
    if (exports.characterp(x) === 1) return 'CHARACTER';
    if (exports.stringp(x) === 1) return 'STRING';
    if (exports.consp(x) === 1) return 'CONS';
    if (exports.symbolp(x) === 1) {
      const name = stringToJS(exports.symbol_name(x));
      return `SYMBOL ${name}`;
    }
    if (exports.vectorp(x) === 1) return 'VECTOR';
    if (exports.packagep(x) === 1) return 'PACKAGE';
    if (exports.env_framep(x) === 1) return 'ENV-FRAME';
    if (exports.closurep(x) === 1) return 'CLOSURE';
    if (exports.primitivep(x) === 1) return 'PRIMITIVE';
    if (exports.interpreted_closurep?.(x) === 1) return 'FUNCTION';
    return 'UNKNOWN';
  };

  const imports = {
    env: {
      /**
       * Raise a Lisp-level error from Wasm.
       * @param {number} code
       * @param {any} a
       * @param {any} b
       */
      raise_error: (code, a, b) => {
        switch (code) {
          case 1:
            throw new Error(`CAR: ${typeNameOf(a)} is not a list`);
          case 2:
            throw new Error(`CDR: ${typeNameOf(a)} is not a list`);
          case 3: {
            const name = stringToJS(exports.symbol_name(a));
            throw new Error(`Unbound variable: ${name}`);
          }
          case 4: {
            const name = stringToJS(exports.symbol_name(a));
            throw new Error(`Undefined function: ${name}`);
          }
          case 5:
            throw new Error('Odd number of arguments to SETQ');
          case 6:
            throw new Error('Invalid function specifier');
          case 7:
            throw new Error(`Not a function: ${typeNameOf(a)}`);
          case 8: {
            const name = stringToJS(exports.symbol_name(a));
            throw new Error(`Uncaught return-from: ${name}`);
          }
          case 9:
            throw new Error('Uncaught throw');
          default:
            throw new Error(`Wasm error ${code}: ${typeNameOf(a)}`);
        }
      }
    }
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);

  exports = instance.exports;
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
    this.UNBOUND = exports.get_unbound();
    this.CL_USER = exports.get_cl_user_package();
    this.KEYWORD = exports.get_keyword_package();
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

  symbolName(sym) {
    return this.exports.symbol_name(sym);
  }

  symbolPackage(sym) {
    return this.exports.symbol_package(sym);
  }

  // === Intern operations ===

  /**
   * Intern a symbol in the default (CL-USER) package
   */
  intern(name) {
    const s = this.stringFromJS(name);
    return this.exports.intern_default(s);
  }

  /**
   * Intern a symbol in a specific package
   */
  internInPackage(name, pkg) {
    const s = this.stringFromJS(name);
    return this.exports.intern(s, pkg);
  }

  /**
   * Intern a keyword symbol
   */
  internKeyword(name) {
    const s = this.stringFromJS(name);
    return this.exports.intern_keyword(s);
  }

  /**
   * Find a symbol in a package (returns NIL if not found)
   */
  findSymbol(name, pkg) {
    const s = this.stringFromJS(name);
    return this.exports.find_symbol(s, pkg);
  }

  // === Package operations ===

  isPackage(x) {
    return this.exports.packagep(x) === 1;
  }

  packageName(pkg) {
    return this.exports.package_name(pkg);
  }

  // === Unbound marker ===

  isUnbound(x) {
    return this.exports.unboundp(x) === 1;
  }

  // === Evaluator ===

  eval(expr) {
    return this.exports.eval(expr);
  }

  isInterpretedClosure(x) {
    return this.exports.interpreted_closurep(x) === 1;
  }

  // === String utility ===

  stringHash(s) {
    return this.exports.string_hash(s);
  }

  stringEqual(a, b) {
    return this.exports.string_equal(a, b) === 1;
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

  // === Environment operations ===

  makeEnvFrame(parent, size) {
    return this.exports.make_env_frame(parent, size);
  }

  isEnvFrame(x) {
    return this.exports.env_framep(x) === 1;
  }

  envRef(env, index) {
    return this.exports.env_ref(env, index);
  }

  envSet(env, index, val) {
    return this.exports.env_set(env, index, val);
  }

  envParent(env) {
    return this.exports.env_parent(env);
  }

  envSize(env) {
    return this.exports.env_size(env);
  }

  envLookup(env, depth, index) {
    return this.exports.env_lookup(env, depth, index);
  }

  envSetAt(env, depth, index, val) {
    return this.exports.env_set_at(env, depth, index, val);
  }

  // === Closure operations ===

  isClosure(x) {
    return this.exports.closurep(x) === 1;
  }

  closureEnv(c) {
    return this.exports.closure_env(c);
  }

  closureName(c) {
    return this.exports.closure_name(c);
  }

  closureLambdaList(c) {
    return this.exports.closure_lambda_list(c);
  }

  closureArity(c) {
    return this.exports.closure_arity(c);
  }

  setClosureName(c, name) {
    return this.exports.set_closure_name(c, name);
  }

  applyClosure(c, args) {
    return this.exports.apply_closure(c, args);
  }

  // === Primitive operations ===

  isPrimitive(x) {
    return this.exports.primitivep(x) === 1;
  }

  primitiveName(p) {
    return this.exports.primitive_name(p);
  }

  primitiveArity(p) {
    return this.exports.primitive_arity(p);
  }

  applyPrimitive(p, args) {
    return this.exports.apply_primitive(p, args);
  }

  // === Function type check ===

  isFunction(x) {
    return this.exports.functionp(x) === 1;
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
