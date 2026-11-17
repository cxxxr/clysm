// contrib/world/worker.js — the worker of a world (contrib/world/world.lisp):
// a Clysm image of its own, started by the page's first message
//   { type: 'boot', module, root, args }
// MODULE is the compiled WebAssembly module of the page's image (which the
// worker so does not compile again), ROOT the Clysm installation and ARGS the command line: boot.lisp and the world's
// name.  In a worker that has Node.js (Electron, nodeIntegrationInWorker)
// the image has the files of the computer.  What Lisp writes to its
// standard output goes to the page line by line ({ type: 'output' }); the
// other messages are Lisp's (world.lisp).
self.addEventListener('message', async function boot(event) {
  const message = event.data;
  if (!message || message.type !== 'boot') return;
  self.removeEventListener('message', boot);
  try {
    const { loadClysm } = await import('./clysm.mjs');
    const pending = { 1: '', 2: '' };
    const output = (text, fd) => {
      const lines = (pending[fd] + text).split('\n');
      pending[fd] = lines.pop();
      for (const line of lines) self.postMessage({ type: 'output', fd, text: line });
    };
    const fs = typeof require === 'function' ? require('node:fs') : undefined;
    await loadClysm(message.module, output, { fs, root: message.root, args: message.args });
  } catch (e) {
    self.postMessage({ type: 'failed', message: String((e && e.stack) || e) });
  }
});
