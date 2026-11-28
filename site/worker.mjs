// site/worker.mjs — the worker of the playground: the Clysm image (with the
// compiler and site/playground.lisp) runs here, so that a long computation
// does not stop the page, and the page can end it by ending the worker.
//
// From the page:
//   { type: 'boot', module, canvas }   MODULE: the compiled image;
//                                      CANVAS: an OffscreenCanvas
//   { type: 'run', id, code }          read and evaluate CODE
// To the page:
//   { type: 'ready' } | { type: 'failed', message }
//   { type: 'output', text, fd }       what Lisp writes, a line or more at a time
//   { type: 'result', id, values, error, canvas, package, ms }

let lisp = null;
let pending = '';
let pendingFd = 1;

const flush = () => {
  if (pending) self.postMessage({ type: 'output', text: pending, fd: pendingFd });
  pending = '';
};

const output = (text, fd = 1) => {
  if (fd !== pendingFd) { flush(); pendingFd = fd; }
  pending += text;
  // whole lines go to the page at once; a long line without a newline too
  if (text.includes('\n') || pending.length > 4096) flush();
};

async function boot({ module, canvas }) {
  const { loadClysm } = await import('./clysm.mjs');
  globalThis.pgCanvas = canvas;
  lisp = await loadClysm(module, output);
  flush();
  self.postMessage({ type: 'ready' });
}

function run({ id, code }) {
  const bsp = lisp.instance.exports.get_bsp();
  const start = performance.now();
  let result;
  try {
    const r = globalThis.pgRun(code);
    result = { values: r.values, error: r.error, canvas: r.canvas === 1, package: r.package };
  } catch (e) {
    // a JavaScript exception went through the Lisp code, skipping what
    // restores the special variables
    globalThis.clysmRepl.unbindTo(bsp);
    result = {
      values: '',
      error: e instanceof RangeError ? 'STORAGE-CONDITION: control stack exhausted'
                                     : `Internal error: ${e.message}`,
      canvas: false,
      package: null,
    };
  }
  flush();
  self.postMessage({ type: 'result', id, ms: performance.now() - start, ...result });
}

self.addEventListener('message', async (event) => {
  const message = event.data;
  try {
    if (message.type === 'boot') await boot(message);
    else if (message.type === 'run') run(message);
  } catch (e) {
    flush();
    self.postMessage({ type: 'failed', message: String((e && e.stack) || e) });
  }
});
