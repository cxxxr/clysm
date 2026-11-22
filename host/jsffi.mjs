// JavaScript FFI imports shared by the Node.js and browser hosts.
// JS values cross the boundary as externref; Lisp wraps them in $foreign structs.
// Libraries may load different copies of this host. Share provenance across
// those copies, without changing the thrown Error or keeping it alive.
const errorOwners = globalThis[Symbol.for('clysm.errorOwners')] ??= new WeakMap();
class JsError {
  constructor(e, state) {
    this.__clysm_js_error = String((e && e.message) || e);
    this.value = e;
    // A condition belongs to its originating instance's packages and classes.
    // Another instance must see the Error as JS:JS-ERROR, not unwrap that heap.
    this.lispCondition = state && errorOwners.get(e) === state ? e.lispCondition : undefined;
  }
}
function boxError(e, state) {
  if (e instanceof WebAssembly.Exception) throw e;
  return new JsError(e, state);
}

// JavaScript Promise Integration: a Lisp function called through
// wrap_async_fn runs on a stack that `await` can suspend until a promise is
// settled.  Without it, wrap_async_fn is wrap_fn and `await` fails.
const jspi = typeof WebAssembly.Suspending === 'function' && typeof WebAssembly.promising === 'function';

// A Lisp function called from JavaScript returns this object (made by
// runtime/js.lisp) when it signals an error that it does not handle: the
// call throws the Error in it.
// (what was thrown may be undefined: JS:THROW of a value of JavaScript)
function lispResult(r) {
  if (r !== null && typeof r === 'object' && Object.hasOwn(r, '__clysm_failure__')) throw r.__clysm_failure__;
  return r;
}

// Classes of JS:DEFINE-CLASS (runtime/js.lisp).  The class is made here from
// SPEC (its name, the class it extends, the names of its members); its
// constructor and members call the Lisp functions that the library gives it
// (defineClass).  The modules of a lazy library (bin/clysm library) make
// its classes before it is loaded, with LOAD: the first use of such a class
// loads the library, whose JS:DEFINE-CLASS then gives the class its functions.
const implementation = Symbol.for('clysm.class');

// What the modules of a library (bin/clysm library) call, imported under names of their own: an
// export of the library may be named JSON or process.
export const jsonConstant = (text) => JSON.parse(text);
export const writeToStderr = (text) => process.stderr.write(text);

export function makeClass(spec, load = null, classes = {}) {
  // (a name of a class of JavaScript: the registries are objects whose keys are the names)
  if (!/^[A-Za-z_$][A-Za-z0-9_$]*$/.test(spec.name) || spec.name === '__proto__') {
    throw new Error(`a class of a Lisp library is named as a class of JavaScript: not ${JSON.stringify(spec.name)}`);
  }
  // (its own: a class that extends one of another library would find that one's)
  const functions = () => {
    if (!Object.hasOwn(C, implementation) && load) load();
    if (!Object.hasOwn(C, implementation)) throw new Error(`the class ${spec.name} has no functions: its Lisp library did not define it`);
    return C[implementation];
  };
  // (own properties alone: a class named constructor or toString is no function of Object)
  const base = spec.extends == null ? null
    : Object.hasOwn(classes, spec.extends) ? classes[spec.extends] : globalThis[spec.extends];
  if (spec.extends != null && typeof base !== 'function') throw new Error(`the class ${spec.name} extends ${spec.extends}, which is no class`);
  const positions = spec.superArguments;
  // (named as the class: what an error's stack and the console say)
  const C = base
    ? { [spec.name]: class extends base {
        constructor(...args) {
          super(...(positions ? positions.map((i) => args[i]) : []));
          const init = functions().init;
          if (init) init(this, ...args);
        } } }[spec.name]
    : { [spec.name]: class {
        constructor(...args) {
          const init = functions().init;
          if (init) init(this, ...args);
        } } }[spec.name];
  for (const name of spec.methods) {
    Object.defineProperty(C.prototype, name, {
      value: { [name](...args) { return functions().methods[name](this, ...args); } }[name],
      writable: true, configurable: true });
  }
  for (const name of spec.getters) {
    Object.defineProperty(C.prototype, name, {
      get() { return functions().getters[name](this); }, configurable: true });
  }
  for (const name of spec.static) {
    Object.defineProperty(C, name, {
      value: { [name](...args) { return functions().static[name](...args); } }[name],
      writable: true, configurable: true });
  }
  return C;
}

// JS:DEFINE-CLASS: the class SPEC names, made by a module of the library
// before it was loaded (LIBRARY.made) or made now, given its functions.
function defineClass(spec, functions, library) {
  library.defined ??= {};
  const made = library.made && Object.hasOwn(library.made, spec.name) ? library.made[spec.name] : null;
  const C = made ?? makeClass(spec, null, library.defined);
  C[implementation] = functions;
  library.defined[spec.name] = C;
  return C;
}
globalThis.__clysm_define_class ??= defineClass;

const utf16 = new TextDecoder('utf-16le', { ignoreBOM: true });

// The functions of JavaScript that the runtime calls (runtime/js.lisp, misc.lisp,
// thread.lisp, process.lisp), by name: (%js-helper "name").  They are written
// here, not made from text by eval, which a page's Content-Security-Policy
// forbids without 'unsafe-eval'.
const runtimeHelpers = {
  // the Error of a Lisp condition (%JS-FAILURE): its stack has the Lisp frames
  // from the function that signalled the condition, without the ones of this
  // function, SIGNAL or ERROR and the handler; the Lisp frames are named after
  // their functions (without the numbers that tell apart functions of the same
  // name; a library's functions are named from a file of their own)
  makeFailure(message, type, condition, mapStack, nameFrames, owner) {
    const old = Error.stackTraceLimit; Error.stackTraceLimit = 100;
    const error = new Error(message);
    Error.stackTraceLimit = old;
    const lines = (typeof nameFrames === 'function' ? nameFrames(error.stack) : String(error.stack)).split('\n');
    let first = 1;
    for (let i = 1; i < lines.length; i++) {
      if (/^\s*at (ERROR|%ERROR\*|CERROR|SIGNAL|%SIGNAL\*|%SIGNAL-CONDITION|%TYPE-ERROR|%CHECK-TYPE-ERROR|INVOKE-DEBUGGER)[#( ]/.test(lines[i])) first = i + 1;
    }
    if (first === 1) {
      // (a frame of Wasm, named or not: "at NAME (wasm://...)" or "at wasm://...")
      while (first < lines.length && !/wasm:/.test(lines[first])) first++;
    }
    // (the host names the places of the Lisp frames: file and line)
    error.stack = ['LispError: ' + message]
      .concat(lines.slice(first).map((l) => (typeof mapStack === 'function' ? mapStack(l)
                                           : l.replace(/at ([^ ]+?)#\d+((?:\/[a-z,]+)*) \(/, 'at $1$2 ('))))
      .join('\n');
    error.name = 'LispError';
    error.lispType = type;
    error.lispCondition = condition;
    // Register only errors created here, never inspect arbitrary thrown values.
    if (owner) errorOwners.set(error, owner);
    return { __clysm_failure__: error };
  },
  // the head of a compiled file (misc.lisp): "LENGTH-OF-HEAD WASM-SIZE FILE-SIZE", or null
  faslHead(read, fs, path) {
    const size = fs.statSync(path).size;
    const b = read(path, 0, Math.min(size, 64));
    let s = '';
    for (let i = 0; i < b.length; i++) s += String.fromCharCode(b[i]);
    const m = /^;;; Clysm compiled file\n;;; wasm (\d+)\n/.exec(s);
    return m ? m[0].length + ' ' + m[1] + ' ' + size : null;
  },
  decodeOctets(read, path, offset, length) {
    return new TextDecoder().decode(read(path, offset, length));
  },
  // the instances of classes, held by WeakRefs (js.lisp, %INSTANCE-TRACKER)
  newInstanceTracker() {
    return {
      refs: new WeakMap(),
      add(cls, obj) {
        let e = this.refs.get(cls);
        if (!e) this.refs.set(cls, e = { list: [], limit: 64 });
        e.list.push(new WeakRef(obj));
        if (e.list.length > e.limit) {
          e.list = e.list.filter((r) => r.deref() !== undefined);
          e.limit = Math.max(64, e.list.length * 2);
        }
      },
      live(cls) {
        const e = this.refs.get(cls);
        return e ? e.list.map((r) => r.deref()).filter((o) => o !== undefined) : [];
      },
    };
  },
  deleteProperty(o, k) { delete o[k]; },
  // [promise, end] (thread.lisp): END ends the wait (the promise is resolved with 0),
  // and after MS milliseconds (unless MS is negative) the promise is resolved with 1
  deferred(ms) {
    let end, timer;
    const p = new Promise((resolve) => {
      end = (v) => { if (timer !== undefined) clearTimeout(timer); resolve(v === 1 ? 1 : 0); };
      if (ms >= 0) timer = setTimeout(() => end(1), ms);
    });
    return [p, end];
  },
  yieldTurn() { return new Promise((resolve) => (globalThis.setImmediate || setTimeout)(resolve)); },
  withEnvironment(e) { return { ...process.env, ...e }; },
};
globalThis.__clysm_runtime ??= runtimeHelpers;

export function makeJsImports(state) {
  // Use this host copy's helper even if an older library installed the global
  // helpers first. The closure identifies the heap without exposing it to Lisp.
  const makeFailure = (message, type, condition) =>
    runtimeHelpers.makeFailure(message, type, condition, state.mapStack, state.nameFrames, state);
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
  // A Lisp object (a WebAssembly GC object) has no prototype, is not
  // extensible and has no property of its own: a module namespace object
  // (`import * as`) or a frozen `Object.create(null)` with properties is a
  // JavaScript object.
  const isLisp = (x) => x !== null && typeof x === 'object' && Object.getPrototypeOf(x) === null
                         && !Object.isExtensible(x) && Reflect.ownKeys(x).length === 0;
  return {
      global: () => globalThis,
      // a property of null or undefined is undefined (NIL in Lisp), not a
      // JavaScript exception, which Lisp code could not handle
      get: (o, k) => (o === null || o === undefined ? undefined : o[k]),
      set: (o, k, v) => { o[k] = v; },
      // a JavaScript exception is returned as a JsError box, which the Lisp
      // side turns into a Lisp error (Lisp non-local exits pass through)
      apply: (f, t, a) => {
        try { return Reflect.apply(f, t, a); } catch (e) { return boxError(e, state); }
      },
      construct: (f, a) => {
        try { return Reflect.construct(f, a); } catch (e) { return boxError(e, state); }
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
        const bsp = ex.get_bsp();
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
            try { return await promise; } catch (e) { return boxError(e, state); }
          })
        : () => new JsError('this JavaScript engine cannot suspend WebAssembly (JSPI)'),
      // a function that returns a promise; the Lisp function runs as a
      // computation of its own that can wait (lisp_apply_async)
      wrap_async_fn: (closure) => (...args) => {
        const ex = state.instance.exports;
        if (!jspi) {
          const bsp = ex.get_bsp();
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
        const stack = ex.get_bstack();
        let recovered = false;
        const p = promisingApply(closure, args);
        if (ex.get_bstack() !== stack) { ex.lisp_recover(-1); recovered = true; }
        return p.then(lispResult, (e) => {
          if (!recovered && !(e instanceof WebAssembly.Exception)) ex.lisp_recover(-1);
          throw e;
        });
      },
      // what the host gives the Lisp side: 0 the modules of (js:module name),
      // 1 the function of (js:import specifier), 2 that of (js:require specifier),
      // 3 the host's mapStack, 4 its nameFrames (host/common.mjs installSourceMap),
      // 5 the error factory bound to this instance
      host: (k) => {
        if (k === 0) return (state.jsModules ??= {});
        if (k === 1) return state.importer || ((specifier) => import(specifier));
        if (k === 2) return state.require;
        if (k === 3) return state.mapStack;
        if (k === 4) return state.nameFrames;
        if (k === 5) return makeFailure;
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
      // the contents of the file NAME into the memory at once (file
      // streams): the number of octets, or -1 if it cannot be read
      read_file_mem: (name) => {
        const fsys = globalThis.__clysm_fs;
        if (!fsys) return -1;
        try {
          if (typeof fsys.openSync === 'function' && typeof fsys.readSync === 'function') {
            const fd = fsys.openSync(name, 'r');
            try {
              const size = fsys.fstatSync(fd).size;
              const buffer = ensure(size);
              let done = 0;
              while (done < size) {
                const n = fsys.readSync(fd, new Uint8Array(buffer, done, size - done), 0, size - done, done);
                if (n <= 0) break;
                done += n;
              }
              return done;
            } finally { fsys.closeSync(fd); }
          }
          const bytes = fsys.readFileSync(name);
          new Uint8Array(ensure(bytes.length), 0, bytes.length).set(bytes);
          return bytes.length;
        } catch (e) { return -1; }
      },
      truthy: (x) => (x ? 1 : 0),
      is_lisp: (x) => (isLisp(x) ? 1 : 0),
    };
}
