// Clysm host runtime for Node.js: instantiates a compiled Lisp module.
//   node host/run.mjs MODULE.wasm [ARGS...]
import fs from 'node:fs';
import { Worker, isMainThread, workerData } from 'node:worker_threads';
import { fileURLToPath } from 'node:url';
import net from 'node:net';
import { makeImports } from './common.mjs';
import { nodeModuleAccess } from './lib.mjs';

// file system access for the Lisp side (pathnames, directories)
globalThis.__clysm_fs = fs;

// With CLYSM_TRACE_FILES=FILE, the files the program reads and the paths it
// looks at are written to FILE as JSON { read: [...], seen: [...] } when it
// exits (contrib/lem packs those files for a page without a server).
const traceFile = process.env.CLYSM_TRACE_FILES;
const traced = { read: new Set(), seen: new Set() };
const traceRead = (path) => { if (traceFile && typeof path === 'string') traced.read.add(path); };
function writeTrace() {
  if (traceFile) {
    fs.writeFileSync(traceFile, JSON.stringify({ read: [...traced.read], seen: [...traced.seen] }));
  }
}
if (traceFile) {
  globalThis.__clysm_fs = new Proxy(fs, {
    get(target, name) {
      const value = target[name];
      if (typeof value !== 'function') return value;
      return (...args) => {
        if (typeof args[0] === 'string') (name === 'readFileSync' ? traced.read : traced.seen).add(args[0]);
        return value.apply(target, args);
      };
    },
  });
}
// octets of a file (compiled modules in fasls); the last file read is cached
{
  let cachedPath = null, cachedBuf = null, cachedMtime = 0;
  globalThis.__clysm_read_bytes = (path, offset, length) => {
    const mtime = fs.statSync(path).mtimeMs;
    if (path !== cachedPath || mtime !== cachedMtime) {
      cachedBuf = fs.readFileSync(path); cachedPath = path; cachedMtime = mtime;
    }
    traceRead(path);
    return new Uint8Array(cachedBuf.buffer, cachedBuf.byteOffset + offset, length);
  };
}
// the Clysm installation directory (REQUIRE finds bundled modules there)
globalThis.__clysm_root = fileURLToPath(new URL('..', import.meta.url));

function nodePlatform(state, args) {
  const out = { 1: [], 2: [] };
  let flushScheduled = false;
  const flush = () => {
    for (const fd of [1, 2]) {
      if (out[fd].length) { fs.writeSync(fd, out[fd].join('')); out[fd] = []; }
    }
  };
  // output written by Lisp code that JavaScript calls later (a timer, a
  // promise) is written out when that task ends
  const scheduleFlush = () => {
    if (!flushScheduled) { flushScheduled = true; setImmediate(() => { flushScheduled = false; flush(); }); }
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
      if (fd === 2 || o.length > 8192) flush(); else scheduleFlush();
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
    exit(code) { flush(); writeTrace(); process.exit(code); },
    flush() { flush(); },
    file_open(mode) {
      // the file name was accumulated by put_char_buf
      const name = state.nameBuf; state.nameBuf = '';
      try {
        if (mode === 0 || mode === 3) {
          // 0: text (UTF-8), 3: binary (one char per octet)
          traceRead(name);
          const data = fs.readFileSync(name, mode === 3 ? 'latin1' : 'utf8');
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
  const state = { ...nodeModuleAccess() };
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

// ---------------------------------------------------------------------------
// Synchronous TCP sockets for the Lisp side (used e.g. by Quicklisp).
// The Lisp program runs in a worker; a socket operation posts a request to
// the main thread, which performs it with node:net, and the worker blocks on
// Atomics.wait until the reply is in the shared buffer.
//   control: [state, status, length]  (state 0 = waiting, 1 = done)
const NET_DATA_BYTES = 1 << 20;

function makeNetServer(worker, sab) {
  const ctrl = new Int32Array(sab, 0, 3);
  const data = new Uint8Array(sab, 16);
  const sockets = new Map();
  let nextId = 1;
  const reply = (status, bytes) => {
    const n = Math.min(bytes.length, data.length);
    data.set(bytes.subarray(0, n));
    ctrl[1] = status; ctrl[2] = n;
    Atomics.store(ctrl, 0, 1);
    Atomics.notify(ctrl, 0);
  };
  const ok = (bytes = new Uint8Array(0)) => reply(0, bytes);
  const fail = (e) => reply(1, Buffer.from(String((e && e.message) || e), 'utf8'));
  const tryRead = (s, max) => {
    if (s.chunks.length) {
      let chunk = s.chunks[0];
      if (chunk.length > max) { s.chunks[0] = chunk.subarray(max); chunk = chunk.subarray(0, max); }
      else s.chunks.shift();
      ok(chunk); return true;
    }
    if (s.error) { fail(s.error); return true; }
    if (s.ended) { ok(); return true; }
    return false;
  };
  worker.on('message', (m) => {
    if (!m || m.clysmNet === undefined) return;
    const { op, args } = m;
    try {
      if (op === 'connect') {
        const [host, port] = args;
        // a null port: HOST is the path of a Unix domain socket
        const sock = (port === null || port === undefined) ? net.connect({ path: host }) : net.connect({ host, port });
        const s = { sock, chunks: [], ended: false, error: null, waiting: null };
        const wake = () => { if (s.waiting !== null && tryRead(s, s.waiting)) s.waiting = null; };
        sock.on('data', (b) => { s.chunks.push(b); wake(); });
        sock.on('end', () => { s.ended = true; wake(); });
        sock.on('close', () => { s.ended = true; wake(); });
        sock.once('connect', () => {
          const id = nextId++; sockets.set(id, s);
          sock.on('error', (e) => { s.error = e; wake(); });
          ok(Buffer.from(String(id)));
        });
        sock.once('error', (e) => { if (![...sockets.values()].includes(s)) fail(e); });
      } else if (op === 'write') {
        const s = sockets.get(args[0]);
        s.sock.write(Buffer.from(args[1], 'latin1'), (e) => (e ? fail(e) : ok()));
      } else if (op === 'read') {
        const s = sockets.get(args[0]);
        if (!tryRead(s, Math.min(args[1], NET_DATA_BYTES))) s.waiting = Math.min(args[1], NET_DATA_BYTES);
      } else if (op === 'available') {
        // is a read possible without waiting?
        const s = sockets.get(args[0]);
        ok(Buffer.from(s && (s.chunks.length || s.ended || s.error) ? '1' : '0'));
      } else if (op === 'close') {
        const s = sockets.get(args[0]);
        if (s) { s.sock.destroy(); sockets.delete(args[0]); }
        ok();
      } else fail('unknown socket operation ' + op);
    } catch (e) { fail(e); }
  });
}

function installNetClient(port, sab) {
  const ctrl = new Int32Array(sab, 0, 3);
  const data = new Uint8Array(sab, 16);
  const call = (op, ...args) => {
    Atomics.store(ctrl, 0, 0);
    port.postMessage({ clysmNet: 1, op, args });
    Atomics.wait(ctrl, 0, 0);
    const bytes = Buffer.from(data.subarray(0, ctrl[2]));
    if (ctrl[1] !== 0) throw new Error(bytes.toString('utf8'));
    return bytes;
  };
  // strings carry octets (latin1), one character per octet
  globalThis.__clysm_net = {
    connect: (host, port) => Number(call('connect', host, port).toString()),
    write: (id, octets) => { call('write', id, octets); },
    read: (id, max) => call('read', id, max).toString('latin1'),
    close: (id) => { call('close', id); },
    available: (id) => call('available', id).toString() === '1',
  };
}

// The module runs in a worker thread so that the stack size can be raised
// (deep non-tail recursion is common in Lisp programs).
const selfPath = fileURLToPath(import.meta.url);
if (isMainThread && process.argv[1] === selfPath) {
  // the compiler marks unlikely branches (the branch hinting section of the modules)
  try { (await import('node:v8')).setFlagsFromString('--experimental-wasm-branch-hinting'); } catch (e) {}
  const stackMb = Number(process.env.CLYSM_STACK_MB || 1024);
  const netBuffer = new SharedArrayBuffer(16 + NET_DATA_BYTES);
  const worker = new Worker(selfPath, {
    workerData: { file: process.argv[2], args: process.argv.slice(3), tty: process.stdin.isTTY, netBuffer },
    resourceLimits: { stackSizeMb: stackMb, maxOldGenerationSizeMb: Number(process.env.CLYSM_HEAP_MB || 4096) },
    stdin: false,
  });
  makeNetServer(worker, netBuffer);
  worker.on('error', (e) => { console.error(';; worker error:', e); process.exit(2); });
  worker.on('exit', (code) => process.exit(code));
} else if (!isMainThread && workerData && workerData.file) {
  if (workerData.netBuffer) {
    const { parentPort } = await import('node:worker_threads');
    installNetClient(parentPort, workerData.netBuffer);
  }
  const bytes = fs.readFileSync(workerData.file);
  await runModule(bytes, workerData.args);
  writeTrace();
  // the worker ends when nothing is left to do: the promises and timers that
  // the program made are settled and run first
}
