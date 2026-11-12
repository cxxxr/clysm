// Clysm host runtime for Node.js: instantiates a compiled Lisp module.
import fs from 'node:fs';

export function makeImports(state, args = []) {
  const out = { 1: [], 2: [] };
  const flush = () => {
    for (const fd of [1, 2]) {
      if (out[fd].length) { fs.writeSync(fd, out[fd].join('')); out[fd] = []; }
    }
  };
  state.flush = flush;
  const ids = new WeakMap();
  let nextId = 1;
  let charBuf = '';
  const stdinBuf = Buffer.alloc(1);
  let pending = [];
  const readByte = () => {
    try {
      const n = fs.readSync(0, stdinBuf, 0, 1, null);
      return n === 0 ? -1 : stdinBuf[0];
    } catch (e) { if (e.code === 'EAGAIN') return readByte(); return -1; }
  };
  const files = [];
  const math1 = [Math.sin, Math.cos, Math.tan, Math.asin, Math.acos, Math.atan, Math.exp, Math.log,
                 Math.sqrt, Math.sinh, Math.cosh, Math.tanh, Math.asinh, Math.acosh, Math.atanh,
                 Math.floor, Math.ceil, Math.trunc, Math.round, Math.log10, Math.log2];
  const math2 = [Math.pow, Math.atan2, (a, b) => a % b];
  return {
    env: {
      write_char(fd, code) {
        const o = out[fd] || out[1];
        o.push(String.fromCodePoint(code));
        if (fd === 2 || o.length > 8192) flush();
      },
      read_char() {
        flush();
        if (pending.length) return pending.shift();
        const b = readByte();
        if (b < 0) return -1;
        if (b < 0x80) return b;
        let n = b >= 0xF0 ? 3 : b >= 0xE0 ? 2 : 1;
        let cp = b & (0x3F >> n);
        for (let i = 0; i < n; i++) cp = (cp << 6) | (readByte() & 0x3F);
        return cp;
      },
      exit(code) { flush(); process.exit(code); },
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
      file_open(mode) {
        // the file name was accumulated by put_char_buf
        const name = state.nameBuf; state.nameBuf = '';
        try {
          if (mode === 0) {
            const data = fs.readFileSync(name, 'utf8');
            charBuf = data;
            return data.length;
          } else {
            fs.writeFileSync(name, state.writeBuf || ''); state.writeBuf = '';
            return 0;
          }
        } catch (e) { return -1; }
      },
      argc() { return args.length; },
      debug_mode() { return process.env.CLYSM_DEBUG ? 1 : 0; },
      arg_len(i) { return args[i].length; },
      arg_char(i, j) { return args[i].codePointAt(j); },
      put_char_buf(c) {
        if (c < 0) { state.writeBuf = state.nameBuf; state.nameBuf = ''; return; }
        state.nameBuf = (state.nameBuf || '') + String.fromCodePoint(c);
      },
    },
  };
}

export async function runModule(bytes, args = []) {
  const state = {};
  const imports = makeImports(state, args);
  let instance;
  try {
    ({ instance } = await WebAssembly.instantiate(bytes, imports));
  } catch (e) {
    console.error('instantiate failed:', e.message);
    process.exit(3);
  }
  try {
    instance.exports._start();
  } catch (e) {
    state.flush();
    if (e instanceof WebAssembly.Exception) {
      console.error(';; uncaught Lisp non-local exit (throw/return-from to an exited extent)');
    } else {
      console.error(';; runtime trap:', e.stack || e);
    }
    process.exit(2);
  }
  state.flush();
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const bytes = fs.readFileSync(process.argv[2]);
  await runModule(bytes, process.argv.slice(3));
}
