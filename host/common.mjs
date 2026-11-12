// Host imports shared by the Node.js and browser hosts.
import { makeJsImports } from './jsffi.mjs';

export function makeCommonEnv(state) {
  const ids = new WeakMap();
  let nextId = 1;
  let charBuf = '';
  state.setCharBuf = (s) => { charBuf = s; };
  const math1 = [Math.sin, Math.cos, Math.tan, Math.asin, Math.acos, Math.atan, Math.exp, Math.log,
                 Math.sqrt, Math.sinh, Math.cosh, Math.tanh, Math.asinh, Math.acosh, Math.atanh,
                 Math.floor, Math.ceil, Math.trunc, Math.round, Math.log10, Math.log2];
  const math2 = [Math.pow, Math.atan2, (a, b) => a % b];
  return {
    identity_hash(x) {
      if (x === null || (typeof x !== 'object' && typeof x !== 'function')) return 0;
      let id = ids.get(x);
      if (id === undefined) { id = nextId++ & 0x1FFFFFFF; ids.set(x, id); }
      return id;
    },
    float_to_chars(x, mode) {
      // Shortest round-trip digits in exponential form "d.ddde+N".
      // mode 0: double, 1: single
      if (!isFinite(x)) { charBuf = isNaN(x) ? 'NaN' : (x > 0 ? 'Inf' : '-Inf'); return charBuf.length; }
      if (mode === 1) {
        let p = 1;
        for (; p < 9; p++) {
          if (Math.fround(parseFloat(x.toExponential(p - 1))) === x) break;
        }
        charBuf = x.toExponential(p - 1);
      } else {
        charBuf = x.toExponential();
      }
      if (Object.is(x, -0)) charBuf = '-' + charBuf;
      return charBuf.length;
    },
    get_char(i) { return charBuf.charCodeAt(i); },
    math1(x, op) { return math1[op](x); },
    math2(x, y, op) { return math2[op](x, y); },
    time(kind) {
      if (kind === 0) return Date.now();
      if (kind === 1) return performance.now();
      return 0;
    },
    backtrace(fd) {
      // Lisp frames appear in the JS stack with the names from the wasm name section
      const old = Error.stackTraceLimit; Error.stackTraceLimit = 60;
      const frames = (new Error().stack || '').split('\n').slice(1);
      Error.stackTraceLimit = old;
      const names = [];
      for (const f of frames) {
        const m = f.match(/at (.+?) \(wasm:/);
        if (!m) continue;
        let name = m[1].replace(/#\d+(\/generic)?$/, '');
        if (name.startsWith(':') || name.startsWith('(') || /^(BACKTRACE|%BACKTRACE|ERROR|%REPORT-AND-EXIT|INVOKE-DEBUGGER|SIGNAL|%TYPE-ERROR|%COERCE-TO-CONDITION)$/.test(name)) continue;
        if (name === 'toplevel') { names.push('<toplevel>'); break; }
        names.push(name);
      }
      const text = 'Backtrace:\n' + names.map((n, i) => `  ${i}: ${n}`).join('\n') + '\n';
      if (state.writeText) state.writeText(fd, text);
    },
    compile_module(bytes, consts) {
      // instantiate a module compiled inside the image and link it with it
      // BYTES is a string of octets, or a Uint8Array (e.g. read from a fasl)
      let u8 = bytes;
      if (!(bytes instanceof Uint8Array)) {
        u8 = new Uint8Array(bytes.length);
        for (let i = 0; i < bytes.length; i++) u8[i] = bytes.charCodeAt(i);
      }
      const mod = new WebAssembly.Module(u8);
      const ex = state.instance.exports;
      const inst = new WebAssembly.Instance(mod, {
        ...state.imports,
        clysm: { mv_count: ex.mv_count, mv_buf: ex.mv_buf, bstack: ex.bstack, bsp: ex.bsp,
                 lisp_throw: ex.lisp_throw },
      });
      return inst.exports.run(consts);
    },
    put_char_buf(c) {
      if (c < 0) { state.writeBuf = state.nameBuf; state.nameBuf = ''; return; }
      state.nameBuf = (state.nameBuf || '') + String.fromCodePoint(c);
    },
  };
}

export function makeImports(state, platform) {
  const imports = {
    js: makeJsImports(state),
    env: { ...makeCommonEnv(state), ...platform },
  };
  state.imports = imports;
  return imports;
}
