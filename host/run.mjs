// Clysm host runtime for Node.js: instantiates a compiled Lisp module.
//   node host/run.mjs MODULE.wasm [ARGS...]
import fs from 'node:fs';
import { Worker, isMainThread, workerData } from 'node:worker_threads';
import { fileURLToPath } from 'node:url';
import net from 'node:net';
import { makeImports, installSourceMap } from './common.mjs';
import { nodeModuleAccess } from './lib.mjs';
import { makeProcessManager } from './process.mjs';

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
        // (file streams read with openSync and readSync)
        if (typeof args[0] === 'string') {
          const read = name === 'readFileSync' || (name === 'openSync' && (args[1] === undefined || args[1] === 'r'));
          (read ? traced.read : traced.seen).add(args[0]);
        }
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
  // octets read ahead by stdinWait (a thread that waits for input without
  // blocking the others, runtime/thread.lisp)
  let stdinAhead = null, stdinAheadPos = 0, stdinEof = false;
  globalThis.__clysm_stdin = {
    ready: () => stdinEof || (stdinAhead !== null && stdinAheadPos < stdinAhead.length),
    wait: () => new Promise((resolve) => {
      const buf = Buffer.alloc(65536);
      fs.read(0, buf, 0, buf.length, null, (err, n) => {
        if (err && err.code === 'EAGAIN') { setTimeout(() => resolve(false), 10); return; }
        if (err || n === 0) stdinEof = true;
        else { stdinAhead = buf.subarray(0, n); stdinAheadPos = 0; }
        resolve(true);
      });
    }),
  };
  const readByte = () => {
    if (stdinAhead !== null && stdinAheadPos < stdinAhead.length) return stdinAhead[stdinAheadPos++];
    if (stdinEof) return -1;
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
    let module;
    ({ module, instance } = await WebAssembly.instantiate(bytes, imports));
    installSourceMap(state, module);
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
      console.error(';; runtime trap:', e.stack ? state.mapStack(e.stack) : e);
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

// [3]: incremented when data arrives on any socket (read without a call:
// a program can tell cheaply whether a socket may have input, see dataSeq)
//
// A worker with threads (runtime/thread.lisp) waits for a read, a connection
// or an accept without blocking: the request has an id (reqId) and the reply
// is a message, { clysmNetReply: id, status, bytes }, while the other
// threads run.
function makeNetServer(worker, sab) {
  const ctrl = new Int32Array(sab, 0, 3);
  const seq = new Int32Array(sab, 12, 1);
  const data = new Uint8Array(sab, 16);
  const sockets = new Map();
  let nextId = 1;
  const syncReply = (status, bytes) => {
    const n = Math.min(bytes.length, data.length);
    data.set(bytes.subarray(0, n));
    ctrl[1] = status; ctrl[2] = n;
    Atomics.store(ctrl, 0, 1);
    Atomics.notify(ctrl, 0);
  };
  // a reply: { ok(bytes), fail(error) }
  const replier = (send) => ({
    ok: (bytes = new Uint8Array(0)) => send(0, bytes),
    fail: (e) => send(1, Buffer.from(String((e && e.message) || e), 'utf8')),
  });
  const syncR = replier(syncReply);
  const asyncR = (id) => replier((status, bytes) =>
    worker.postMessage({ clysmNetReply: id, status, bytes: Buffer.from(bytes).toString('latin1') }));
  const tryRead = (s, max, r) => {
    if (s.chunks.length) {
      let chunk = s.chunks[0];
      if (chunk.length > max) { s.chunks[0] = chunk.subarray(max); chunk = chunk.subarray(0, max); }
      else s.chunks.shift();
      r.ok(chunk); return true;
    }
    if (s.error) { r.fail(s.error); return true; }
    if (s.ended) { r.ok(); return true; }
    return false;
  };
  const servers = new Map();
  // the reads waiting for data: [{ max, r }]
  const wake = (s) => { while (s.waiting.length && tryRead(s, s.waiting[0].max, s.waiting[0].r)) s.waiting.shift(); };
  const track = (sock) => {
    const s = { sock, chunks: [], ended: false, error: null, waiting: [] };
    sock.on('data', (b) => { s.chunks.push(b); Atomics.add(seq, 0, 1); wake(s); });
    sock.on('end', () => { s.ended = true; Atomics.add(seq, 0, 1); wake(s); });
    sock.on('close', () => { s.ended = true; Atomics.add(seq, 0, 1); wake(s); });
    return s;
  };
  // the accepts waiting for a connection: [r]
  const wakeServer = (sv) => {
    while (sv.waiting.length && (sv.pending.length || sv.error)) {
      const r = sv.waiting.shift();
      if (sv.pending.length) {
        const s = sv.pending.shift(); const id = nextId++; sockets.set(id, s);
        r.ok(Buffer.from(String(id)));
      } else r.fail(sv.error);
    }
  };
  const processes = makeProcessManager();
  const netOp = (op, args, r) => {
    if (op === 'connect') {
      const [host, port] = args;
      // a null port: HOST is the path of a Unix domain socket
      const sock = (port === null || port === undefined) ? net.connect({ path: host }) : net.connect({ host, port });
      const s = track(sock);
      sock.once('connect', () => {
        const id = nextId++; sockets.set(id, s);
        sock.on('error', (e) => { s.error = e; wake(s); });
        r.ok(Buffer.from(String(id)));
      });
      sock.once('error', (e) => { if (![...sockets.values()].includes(s)) r.fail(e); });
    } else if (op === 'listen') {
      // a server: connections that arrive are queued until accepted
      const [host, port] = args;
      const server = net.createServer();
      const sv = { server, pending: [], waiting: [], error: null };
      server.on('connection', (sock) => {
        const s = track(sock);
        sock.on('error', (e) => { s.error = e; wake(s); });
        sv.pending.push(s); Atomics.add(seq, 0, 1); wakeServer(sv);
      });
      server.once('error', (e) => { if (!servers.has(sv.id)) r.fail(e); else { sv.error = e; wakeServer(sv); } });
      server.listen({ host: host || undefined, port }, () => {
        sv.id = nextId++; servers.set(sv.id, sv);
        r.ok(Buffer.from(sv.id + ' ' + server.address().port));
      });
    } else if (op === 'accept') {
      const sv = servers.get(args[0]);
      if (!sv) r.fail('the server is closed');
      else { sv.waiting.push(r); wakeServer(sv); }
    } else if (op === 'close-server') {
      const sv = servers.get(args[0]);
      if (sv) {
        sv.server.close(); servers.delete(args[0]);
        for (const w of sv.waiting) w.fail('the server is closed');
        sv.waiting = [];
      }
      r.ok();
    } else if (op === 'write') {
      const s = sockets.get(args[0]);
      s.sock.write(Buffer.from(args[1], 'latin1'), (e) => (e ? r.fail(e) : r.ok()));
    } else if (op === 'read') {
      const s = sockets.get(args[0]);
      const max = Math.min(args[1], NET_DATA_BYTES);
      if (!s) r.ok();
      else if (s.waiting.length || !tryRead(s, max, r)) s.waiting.push({ max, r });
    } else if (op === 'available') {
      // is a read possible without waiting?
      const s = sockets.get(args[0]);
      r.ok(Buffer.from(s && (s.chunks.length || s.ended || s.error) ? '1' : '0'));
    } else if (op === 'close') {
      const s = sockets.get(args[0]);
      if (s) {
        s.sock.destroy(); sockets.delete(args[0]);
        s.ended = true; wake(s);
      }
      r.ok();
    } else r.fail('unknown socket operation ' + op);
  };
  worker.on('message', (m) => {
    if (m && m.clysmProc !== undefined) {
      // a call of the process manager (host/process.mjs): the value is sent
      // as octets (status 0), JSON (2), null (3) or text (4)
      Promise.resolve().then(() => processes[m.op](...m.args)).then((v) => {
        if (v instanceof Uint8Array) syncReply(0, v);
        else if (v === null || v === undefined) syncReply(3, new Uint8Array(0));
        else if (typeof v === 'string') syncReply(4, Buffer.from(v, 'utf8'));
        else syncReply(2, Buffer.from(JSON.stringify(v), 'utf8'));
      }, syncR.fail);
      return;
    }
    if (!m || m.clysmNet === undefined) return;
    const r = m.reqId === undefined ? syncR : asyncR(m.reqId);
    try { netOp(m.op, m.args, r); } catch (e) { r.fail(e); }
  });
}

function installNetClient(port, sab) {
  const ctrl = new Int32Array(sab, 0, 3);
  const seq = new Int32Array(sab, 12, 1);
  const data = new Uint8Array(sab, 16);
  const call = (op, ...args) => {
    Atomics.store(ctrl, 0, 0);
    port.postMessage({ clysmNet: 1, op, args });
    Atomics.wait(ctrl, 0, 0);
    const bytes = Buffer.from(data.subarray(0, ctrl[2]));
    if (ctrl[1] !== 0) throw new Error(bytes.toString('utf8'));
    return bytes;
  };
  // requests answered by a message (the port keeps the worker alive only
  // while one is pending)
  const pending = new Map();
  let nextReq = 1;
  port.on('message', (m) => {
    if (!m || m.clysmNetReply === undefined) return;
    const p = pending.get(m.clysmNetReply);
    if (!p) return;
    pending.delete(m.clysmNetReply);
    if (pending.size === 0) port.unref();
    if (m.status === 0) p.resolve(m.bytes); else p.reject(new Error(Buffer.from(m.bytes, 'latin1').toString('utf8')));
  });
  port.unref();
  const callAsync = (op, ...args) => new Promise((resolve, reject) => {
    const id = nextReq++;
    pending.set(id, { resolve, reject });
    if (pending.size === 1) port.ref();
    port.postMessage({ clysmNet: 1, op, args, reqId: id });
  });
  // the process manager of the main thread, called synchronously
  const callProc = (op, args) => {
    Atomics.store(ctrl, 0, 0);
    port.postMessage({ clysmProc: 1, op, args });
    Atomics.wait(ctrl, 0, 0);
    const status = ctrl[1];
    const bytes = Buffer.from(data.subarray(0, ctrl[2]));
    if (status === 1) throw new Error(bytes.toString('utf8'));
    if (status === 0) return new Uint8Array(bytes);
    if (status === 3) return null;
    if (status === 4) return bytes.toString('utf8');
    return JSON.parse(bytes.toString('utf8'));
  };
  globalThis.__clysm_proc_sync = new Proxy({}, {
    get: (target, op) => (...args) => callProc(op, args.map((a) => (a instanceof Uint8Array ? a : a))),
  });
  // strings carry octets (latin1), one character per octet
  globalThis.__clysm_net = {
    connect: (host, port) => Number(call('connect', host, port).toString()),
    write: (id, octets) => { call('write', id, octets); },
    read: (id, max) => call('read', id, max).toString('latin1'),
    close: (id) => { call('close', id); },
    available: (id) => call('available', id).toString() === '1',
    // a server on HOST (null: all interfaces) and PORT (0: any free port):
    // "ID PORT"
    listen: (host, port) => call('listen', host, port).toString(),
    accept: (id) => Number(call('accept', id).toString()),
    closeServer: (id) => { call('close-server', id); },
    dataSeq: () => Atomics.load(seq, 0),
    // the same, waiting without blocking the worker (promises)
    readAsync: (id, max) => callAsync('read', id, max),
    acceptAsync: (id) => callAsync('accept', id).then(Number),
    connectAsync: (host, port) => callAsync('connect', host, port).then(Number),
  };
}

// The module runs in a worker thread so that the stack size can be raised
// (deep non-tail recursion is common in Lisp programs).
const selfPath = fileURLToPath(import.meta.url);
if (isMainThread && process.argv[1] === selfPath) {
  // the compiler marks unlikely branches (the branch hinting section of the modules)
  try { (await import('node:v8')).setFlagsFromString('--experimental-wasm-branch-hinting'); } catch (e) {}
  // the stacks of computations that can wait (JSPI, the threads of
  // runtime/thread.lisp) as deep as the worker's own (they are limited by
  // --stack-size too; the main thread itself recurses little)
  try {
    const v8 = await import('node:v8');
    const kb = Number(process.env.CLYSM_STACK_MB || 1024) * 1024;
    v8.setFlagsFromString('--wasm-stack-switching-stack-size=' + kb);
    v8.setFlagsFromString('--stack-size=' + kb);
  } catch (e) {}
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
  // the program may run as a computation that can wait (sys::main, threads)
  globalThis.__clysm_deep_async_stacks = true;
  if (workerData.netBuffer) {
    const { parentPort } = await import('node:worker_threads');
    installNetClient(parentPort, workerData.netBuffer);
  }
  // CLYSM_DOM=1 (bin/clysm.mjs test --dom): a document of happy-dom, found
  // from the working directory, as in a page
  if (process.env.CLYSM_DOM) {
    try {
      const { Window } = nodeModuleAccess().require('happy-dom');
      const window = new Window({ url: 'http://localhost/' });
      for (const k of ['window', 'document', 'navigator', 'location', 'HTMLElement', 'Node', 'Event',
                       'KeyboardEvent', 'MouseEvent', 'CustomEvent', 'requestAnimationFrame', 'getComputedStyle']) {
        if (window[k] !== undefined && globalThis[k] === undefined) {
          globalThis[k] = typeof window[k] === 'function' && /^[a-z]/.test(k) ? window[k].bind(window) : window[k];
        }
      }
      globalThis.window = window;
    } catch (e) {
      console.error(';; CLYSM_DOM: happy-dom is not installed here (npm install happy-dom)');
      process.exit(2);
    }
  }
  const bytes = fs.readFileSync(workerData.file);
  await runModule(bytes, workerData.args);
  writeTrace();
  // the worker ends when nothing is left to do: the promises and timers that
  // the program made are settled and run first
}
