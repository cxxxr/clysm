// JavaScript FFI imports shared by the Node.js and browser hosts.
// JS values cross the boundary as externref; Lisp wraps them in $foreign structs.
class JsError {
  constructor(e) { this.__clysm_js_error = String((e && e.message) || e); this.value = e; }
}
function boxError(e) {
  if (e instanceof WebAssembly.Exception) throw e;
  return new JsError(e);
}

// JavaScript Promise Integration: a Lisp function called through
// wrap_async_fn runs on a stack that `await` can suspend until a promise is
// settled.  Without it, wrap_async_fn is wrap_fn and `await` fails.
const jspi = typeof WebAssembly.Suspending === 'function' && typeof WebAssembly.promising === 'function';

// A Lisp function called from JavaScript returns this object (made by
// runtime/js.lisp) when it signals an error that it does not handle: the
// call throws the Error in it.
function lispResult(r) {
  if (r !== null && typeof r === 'object' && r.__clysm_failure__ !== undefined) throw r.__clysm_failure__;
  return r;
}

const utf16 = new TextDecoder('utf-16le');

export function makeJsImports(state) {
  let promisingApply = null;
  // strings and octets cross through the memory the module exports
  const memory = () => state.instance.exports.memory;
  const ensure = (bytes) => {
    const m = memory();
    const need = bytes - m.buffer.byteLength;
    if (need > 0) m.grow(Math.ceil(need / 65536));
    return m.buffer;
  };
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
      // an exception that Lisp code cannot catch (the stack exhausted, a
      // trap) skips the code that undoes dynamic bindings: lisp_recover
      // does it (a Lisp non-local exit, a WebAssembly.Exception, does not)
      wrap_fn: (closure) => (...args) => {
        const ex = state.instance.exports;
        const bsp = ex.bsp.value;
        let r;
        try { r = ex.lisp_apply(closure, args); } catch (e) {
          if (!(e instanceof WebAssembly.Exception)) {
            ex.lisp_recover(bsp);
            if (e instanceof Error && state.mapStack && !e.__clysm_mapped) {
              try { e.stack = state.mapStack(e.stack); e.__clysm_mapped = true; } catch (_) {}
            }
          }
          throw e;
        }
        return lispResult(r);
      },
      // (the Lisp side calls it only inside a function of wrap_async_fn)
      await: jspi
        ? new WebAssembly.Suspending(async (promise) => {
            try { return await promise; } catch (e) { return boxError(e); }
          })
        : () => new JsError('this JavaScript engine cannot suspend WebAssembly (JSPI)'),
      // a function that returns a promise; the Lisp function runs as a
      // computation of its own that can wait (lisp_apply_async)
      wrap_async_fn: (closure) => (...args) => {
        const ex = state.instance.exports;
        if (!jspi) {
          const bsp = ex.bsp.value;
          try { return Promise.resolve(lispResult(ex.lisp_apply_async(closure, args))); }
          catch (e) {
            if (!(e instanceof WebAssembly.Exception)) ex.lisp_recover(bsp);
            return Promise.reject(e);
          }
        }
        if (!promisingApply) promisingApply = WebAssembly.promising(ex.lisp_apply_async);
        // the computation installs a binding stack of its own and puts the
        // one outside back when it waits or ends; if it is still installed
        // when the call returns, the computation died
        const stack = ex.bstack.value;
        let recovered = false;
        const p = promisingApply(closure, args);
        if (ex.bstack.value !== stack) { ex.lisp_recover(-1); recovered = true; }
        return p.then(lispResult, (e) => {
          if (!recovered && !(e instanceof WebAssembly.Exception)) ex.lisp_recover(-1);
          throw e;
        });
      },
      // what the host gives the Lisp side: 0 the modules of (js:module name),
      // 1 the function of (js:import specifier), 2 that of (js:require specifier)
      host: (k) => {
        if (k === 0) return (state.modules ??= {});
        if (k === 1) return state.importer || ((specifier) => import(specifier));
        if (k === 2) return state.require;
        if (k === 3) return state.mapStack;
        return undefined;
      },
      // a string of the code units at the start of the memory: INFO is their
      // number of bytes, plus 1 if one is a surrogate of its own (which
      // TextDecoder would replace)
      str_from_mem: (info) => {
        const n = info >>> 1;
        const units = new Uint16Array(memory().buffer, 0, n);
        if (n < 64 || (info & 1)) {
          let s = '';
          for (let i = 0; i < n; i += 8192) s += String.fromCharCode.apply(null, units.subarray(i, i + 8192));
          return s;
        }
        return utf16.decode(units);
      },
      str_to_mem: (x) => {
        const s = String(x);
        const n = s.length;
        const units = new Uint16Array(ensure(n * 2), 0, n);
        for (let i = 0; i < n; i++) units[i] = s.charCodeAt(i);
        return n;
      },
      octets_from_mem: (n) => new Uint8Array(memory().buffer, 0, n).slice(),
      // octets of a Uint8Array (a Buffer), an ArrayBuffer or a typed array; -1 otherwise
      octets_to_mem: (x) => {
        let bytes;
        if (x instanceof Uint8Array) bytes = x;
        else if (x instanceof ArrayBuffer) bytes = new Uint8Array(x);
        else if (ArrayBuffer.isView(x)) bytes = new Uint8Array(x.buffer, x.byteOffset, x.byteLength);
        else return -1;
        new Uint8Array(ensure(bytes.length), 0, bytes.length).set(bytes);
        return bytes.length;
      },
      truthy: (x) => (x ? 1 : 0),
      is_lisp: (x) => (isLisp(x) ? 1 : 0),
    };
}
