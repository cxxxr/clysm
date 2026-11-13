// JavaScript FFI imports shared by the Node.js and browser hosts.
// JS values cross the boundary as externref; Lisp wraps them in $foreign structs.
class JsError {
  constructor(e) { this.__clysm_js_error = String((e && e.message) || e); }
}
function boxError(e) {
  if (e instanceof WebAssembly.Exception) throw e;
  return new JsError(e);
}

// JavaScript Promise Integration: a Lisp function called through
// wrap_async_fn runs on a stack that `await` can suspend until a promise is
// settled.  Without it, wrap_async_fn is wrap_fn and `await` fails.
const jspi = typeof WebAssembly.Suspending === 'function' && typeof WebAssembly.promising === 'function';

export function makeJsImports(state) {
  let promisingApply = null;
  let strCache = null, strCacheArr = null;
  const codePoints = (x) => {
    if (strCache !== x) { strCache = x; strCacheArr = Array.from(x); }
    return strCacheArr;
  };
  const isLisp = (x) => x !== null && typeof x === 'object' && Object.getPrototypeOf(x) === null
                         && !Object.isExtensible(x);
  return {
      global: () => globalThis,
      // a property of null or undefined is undefined (NIL in Lisp), not a
      // JavaScript exception, which Lisp code could not handle
      get: (o, k) => (o === null || o === undefined ? undefined : o[k]),
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
      // (the Lisp side calls it only inside a function of wrap_async_fn)
      await: jspi
        ? new WebAssembly.Suspending(async (promise) => {
            try { return await promise; } catch (e) { return boxError(e); }
          })
        : () => new JsError('this JavaScript engine cannot suspend WebAssembly (JSPI)'),
      wrap_async_fn: (closure) => (...args) => {
        if (!jspi) return state.instance.exports.lisp_apply(closure, args);
        if (!promisingApply) promisingApply = WebAssembly.promising(state.instance.exports.lisp_apply);
        // a promise; an exit that leaves the function is reported like an uncaught exception
        return promisingApply(closure, args).catch((e) => { console.error(e); });
      },
      truthy: (x) => (x ? 1 : 0),
      is_lisp: (x) => (isLisp(x) ? 1 : 0),
    };
}
