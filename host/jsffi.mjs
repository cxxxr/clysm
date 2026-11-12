// JavaScript FFI imports shared by the Node.js and browser hosts.
// JS values cross the boundary as externref; Lisp wraps them in $foreign structs.
class JsError {
  constructor(e) { this.__clysm_js_error = String((e && e.message) || e); }
}
function boxError(e) {
  if (e instanceof WebAssembly.Exception) throw e;
  return new JsError(e);
}

export function makeJsImports(state) {
  let strCache = null, strCacheArr = null;
  const codePoints = (x) => {
    if (strCache !== x) { strCache = x; strCacheArr = Array.from(x); }
    return strCacheArr;
  };
  const isLisp = (x) => x !== null && typeof x === 'object' && Object.getPrototypeOf(x) === null
                         && !Object.isExtensible(x);
  return {
      global: () => globalThis,
      get: (o, k) => o[k],
      set: (o, k, v) => { o[k] = v; },
      // a JavaScript exception is returned as a JsError box, which the Lisp
      // side turns into a Lisp error (Lisp non-local exits pass through)
      apply: (f, t, a) => {
        try { return Reflect.apply(f, t, a); } catch (e) { return boxError(e); }
      },
      construct: (f, a) => {
        try { return Reflect.construct(f, a); } catch (e) { return boxError(e); }
      },
      array: () => [],
      push: (a, v) => { a.push(v); },
      number: (d) => d,
      to_number: (x) => Number(x),
      typeof: (x) => {
        if (x === undefined) return 0;
        if (x === null) return 1;
        switch (typeof x) {
          case 'boolean': return 2; case 'number': return 3; case 'string': return 4;
          case 'function': return 6; case 'bigint': return 7; case 'symbol': return 8;
          default: return isLisp(x) ? 9 : 5;
        }
      },
      take_string: () => { const s = state.nameBuf || ''; state.nameBuf = ''; return s; },
      str_len: (x) => codePoints(x).length,
      str_char: (x, i) => codePoints(x)[i].codePointAt(0),
      bool: (b) => b !== 0,
      special: (k) => (k === 0 ? null : undefined),
      wrap_fn: (closure) => (...args) => state.instance.exports.lisp_apply(closure, args),
      truthy: (x) => (x ? 1 : 0),
      is_lisp: (x) => (isLisp(x) ? 1 : 0),
    };
}
