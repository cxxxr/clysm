// Clysm as a library in a worker thread: for computations that need a deep
// stack (the thread of the caller has about 1 MB; a worker has the stack it
// is given), or that should not block the caller's thread.
//
//   import { loadInWorker } from 'clysm/host/worker.mjs';
//   const lib = await loadInWorker(new URL('./outline.wasm', import.meta.url));
//   await lib.exports.outline(source);    // every export returns a promise
//   lib.terminate();
//
// Arguments and values cross between the threads by structured clone
// (strings, numbers, arrays, plain objects; not functions).  A Lisp error is
// thrown as an Error with name 'LispError' and the Lisp frames in its stack.
// Options: those of load() in lib.mjs that can be cloned (args), plus
// stackSizeMb (default 256) and heapMb.  Output goes to the caller's
// process.stdout and process.stderr unless stdout/stderr functions are given.
import { Worker, isMainThread, parentPort, workerData } from 'node:worker_threads';
import { fileURLToPath } from 'node:url';

const selfPath = fileURLToPath(import.meta.url);

export async function loadInWorker(source, options = {}) {
  const { stackSizeMb = 256, heapMb, stdout, stderr, args = [] } = options;
  const file = source instanceof URL ? fileURLToPath(source) : source;
  const worker = new Worker(selfPath, {
    workerData: { clysmLibrary: true, file, args },
    resourceLimits: { stackSizeMb, ...(heapMb ? { maxOldGenerationSizeMb: heapMb } : {}) },
    stdout: !!stdout, stderr: !!stderr,
  });
  if (stdout) worker.stdout.on('data', (b) => stdout(b.toString()));
  if (stderr) worker.stderr.on('data', (b) => stderr(b.toString()));
  const pending = new Map();
  let nextId = 1;
  let names = null;
  const ready = new Promise((resolve, reject) => {
    worker.on('message', (m) => {
      if (m.ready) { names = m.names; resolve(); return; }
      if (m.loadError) { reject(revive(m.loadError)); return; }
      const p = pending.get(m.id);
      if (!p) return;
      pending.delete(m.id);
      if (m.error) p.reject(revive(m.error)); else p.resolve(m.value);
    });
    worker.on('error', (e) => {
      reject(e);
      for (const p of pending.values()) p.reject(e);
      pending.clear();
    });
    worker.on('exit', (code) => {
      const e = new Error(`the Lisp worker exited with code ${code}`);
      reject(e);
      for (const p of pending.values()) p.reject(e);
      pending.clear();
    });
  });
  await ready;
  const exports = {};
  for (const name of names) {
    exports[name] = (...callArgs) => new Promise((resolve, reject) => {
      const id = nextId++;
      pending.set(id, { resolve, reject });
      worker.postMessage({ id, name, args: callArgs });
    });
  }
  return { exports, worker, terminate: () => worker.terminate() };
}

function serialize(e) {
  return { name: e?.name || 'Error', message: String(e?.message ?? e), stack: e?.stack, lispType: e?.lispType };
}
function revive(o) {
  const e = new Error(o.message);
  e.name = o.name;
  if (o.stack) e.stack = o.stack;
  if (o.lispType) e.lispType = o.lispType;
  return e;
}

if (!isMainThread && workerData && workerData.clysmLibrary) {
  const { load } = await import('./lib.mjs');
  let lib;
  try {
    lib = await load(workerData.file, { args: workerData.args });
  } catch (e) {
    parentPort.postMessage({ loadError: serialize(e) });
  }
  if (lib) {
    parentPort.on('message', async ({ id, name, args }) => {
      try {
        const f = lib.exports[name];
        const value = await f(...args);
        parentPort.postMessage({ id, value });
      } catch (e) {
        parentPort.postMessage({ id, error: serialize(e) });
      }
    });
    parentPort.postMessage({ ready: true, names: Object.keys(lib.exports) });
  }
}
