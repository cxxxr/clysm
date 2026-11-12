// Clysm host runtime for Node.js: instantiates a compiled Lisp module.
//   node host/run.mjs MODULE.wasm [ARGS...]
import fs from 'node:fs';
import { Worker, isMainThread, workerData } from 'node:worker_threads';
import { fileURLToPath } from 'node:url';
import { makeImports } from './common.mjs';

function nodePlatform(state, args) {
  const out = { 1: [], 2: [] };
  const flush = () => {
    for (const fd of [1, 2]) {
      if (out[fd].length) { fs.writeSync(fd, out[fd].join('')); out[fd] = []; }
    }
  };
  state.flush = flush;
  state.writeText = (fd, text) => { flush(); fs.writeSync(fd, text); };
  const stdinBuf = Buffer.alloc(1);
  const readByte = () => {
    try {
      const n = fs.readSync(0, stdinBuf, 0, 1, null);
      return n === 0 ? -1 : stdinBuf[0];
    } catch (e) { if (e.code === 'EAGAIN') return readByte(); return -1; }
  };
  return {
    write_char(fd, code) {
      const o = out[fd] || out[1];
      o.push(String.fromCodePoint(code));
      if (fd === 2 || o.length > 8192) flush();
    },
    read_char() {
      flush();
      const b = readByte();
      if (b < 0) return -1;
      if (b < 0x80) return b;
      const n = b >= 0xF0 ? 3 : b >= 0xE0 ? 2 : 1;
      let cp = b & (0x3F >> n);
      for (let i = 0; i < n; i++) cp = (cp << 6) | (readByte() & 0x3F);
      return cp;
    },
    exit(code) { flush(); process.exit(code); },
    flush() { flush(); },
    file_open(mode) {
      // the file name was accumulated by put_char_buf
      const name = state.nameBuf; state.nameBuf = '';
      try {
        if (mode === 0) {
          const data = fs.readFileSync(name, 'utf8');
          state.setCharBuf(data);
          return data.length;
        }
        const content = state.writeBuf || ''; state.writeBuf = '';
        fs.writeFileSync(name, mode === 2 ? Buffer.from(content, 'latin1') : content);
        return 0;
      } catch (e) { return -1; }
    },
    argc() { return args.length; },
    arg_len(i) { return args[i].length; },
    arg_char(i, j) { return args[i].codePointAt(j); },
    debug_mode() { return process.env.CLYSM_DEBUG ? 1 : 0; },
    is_tty() { return (process.env.CLYSM_INTERACTIVE || workerData?.tty) ? 1 : 0; },
  };
}

export async function runModule(bytes, args = []) {
  const state = {};
  const imports = makeImports(state, nodePlatform(state, args));
  let instance;
  try {
    ({ instance } = await WebAssembly.instantiate(bytes, imports));
    state.instance = instance;
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

// The module runs in a worker thread so that the stack size can be raised
// (deep non-tail recursion is common in Lisp programs).
const selfPath = fileURLToPath(import.meta.url);
if (isMainThread && process.argv[1] === selfPath) {
  const stackMb = Number(process.env.CLYSM_STACK_MB || 512);
  const worker = new Worker(selfPath, {
    workerData: { file: process.argv[2], args: process.argv.slice(3), tty: process.stdin.isTTY },
    resourceLimits: { stackSizeMb: stackMb, maxOldGenerationSizeMb: Number(process.env.CLYSM_HEAP_MB || 4096) },
    stdin: false,
  });
  worker.on('error', (e) => { console.error(';; worker error:', e); process.exit(2); });
  worker.on('exit', (code) => process.exit(code));
} else if (!isMainThread && workerData && workerData.file) {
  const bytes = fs.readFileSync(workerData.file);
  await runModule(bytes, workerData.args);
  process.exit(0);
}
