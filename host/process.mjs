// Subprocesses for the Lisp side (UIOP:LAUNCH-PROGRAM, sys::%launch-program
// in runtime/js.lisp), on Node.js.  The manager starts processes with
// node:child_process and keeps what they write until Lisp reads it; the
// operations that wait return promises.  In the Node.js host the Lisp
// program runs in a worker and the manager in the main thread, which
// answers each call while the worker blocks (host/run.mjs); in a library
// (host/lib.mjs) the manager is in the Lisp thread and Lisp waits for its
// promises with JS:AWAIT.
import { spawn } from 'node:child_process';

export function makeProcessManager() {
  const procs = new Map();
  let nextId = 1;
  const get = (id) => {
    const p = procs.get(id);
    if (!p) throw new Error(`no process ${id}`);
    return p;
  };
  // the output of one of the process's streams: chunks, end, waiting reader
  const sink = () => ({ chunks: [], ended: false, waiting: null, decoder: new TextDecoder('utf-8') });
  const take = (s, max) => {
    let chunk = s.chunks[0];
    if (chunk.length > max) { s.chunks[0] = chunk.subarray(max); chunk = chunk.subarray(0, max); }
    else s.chunks.shift();
    return chunk;
  };
  const wake = (s) => {
    if (s.waiting && (s.chunks.length || s.ended)) {
      const { resolve, max } = s.waiting; s.waiting = null;
      resolve(s.chunks.length ? take(s, max) : null);
    }
  };
  const stdioOf = (x) => (x === 'pipe' || x === 'inherit' || x === 'ignore' ? x : 'ignore');
  return {
    // COMMAND: an array (program and arguments) or a string for the shell.
    // OPTIONS: { cwd, env (an object, added to the environment), stdin,
    // stdout, stderr: 'pipe' | 'inherit' | 'ignore', stderr 'stdout' too }
    spawn(command, options = {}) {
      const { cwd, env, stdin = 'pipe', stdout = 'pipe', stderr = 'pipe' } = options;
      const shell = typeof command === 'string';
      const merge = stderr === 'stdout';
      const child = shell
        ? spawn(command, { shell: true, cwd: cwd || undefined, env: env ? { ...process.env, ...env } : undefined,
                           stdio: [stdioOf(stdin), stdioOf(stdout), merge ? 'pipe' : stdioOf(stderr)] })
        : spawn(command[0], command.slice(1), { cwd: cwd || undefined, env: env ? { ...process.env, ...env } : undefined,
                                               stdio: [stdioOf(stdin), stdioOf(stdout), merge ? 'pipe' : stdioOf(stderr)] });
      const id = nextId++;
      const p = { child, out: { 1: sink(), 2: merge ? null : sink() }, exit: null, exitWaiters: [], error: null };
      const outOf = (fd) => (fd === 2 && merge ? p.out[1] : p.out[fd]);
      for (const fd of [1, 2]) {
        const stream = fd === 1 ? child.stdout : child.stderr;
        const s = outOf(fd);
        if (!stream || !s) { if (p.out[fd]) p.out[fd].ended = true; continue; }
        stream.on('data', (b) => { s.chunks.push(new Uint8Array(b.buffer, b.byteOffset, b.length)); wake(s); });
        stream.on('end', () => { if (fd === 1 || !merge) { s.ended = true; wake(s); } });
      }
      if (merge && child.stdout) child.stdout.on('end', () => { p.out[1].ended = true; wake(p.out[1]); });
      const finish = (code, signal) => {
        if (p.exit) return;
        p.exit = [code ?? (signal ? 128 : 1), signal || null];
        for (const fd of [1, 2]) if (p.out[fd] && !(fd === 1 ? child.stdout : child.stderr)) { p.out[fd].ended = true; wake(p.out[fd]); }
        for (const w of p.exitWaiters.splice(0)) w(p.exit);
      };
      child.on('exit', finish);
      child.on('error', (e) => {
        p.error = e;
        for (const fd of [1, 2]) if (p.out[fd]) { p.out[fd].ended = true; wake(p.out[fd]); }
        finish(127, null);
      });
      if (child.stdin) child.stdin.on('error', () => {});
      procs.set(id, p);
      return id;
    },
    pid(id) { return get(id).child.pid ?? null; },
    // the next octets (at most MAX) of output FD (1 or 2), or null at its end
    read(id, fd, max = 65536) {
      const s = get(id).out[fd] || get(id).out[1];
      if (s.chunks.length) return take(s, max);
      if (s.ended) return null;
      return new Promise((resolve) => { s.waiting = { resolve, max }; });
    },
    // the same as text (UTF-8, a character split between chunks is kept for the next read)
    async readText(id, fd, max = 65536) {
      const s = get(id).out[fd] || get(id).out[1];
      for (;;) {
        const bytes = await this.read(id, fd, max);
        if (bytes === null) { const rest = s.decoder.decode(); return rest === '' ? null : rest; }
        const text = s.decoder.decode(bytes, { stream: true });
        if (text !== '') return text;
      }
    },
    available(id, fd) {
      const s = get(id).out[fd] || get(id).out[1];
      return s.chunks.length > 0 || s.ended;
    },
    // write a string (UTF-8) or octets to the process's standard input
    write(id, data) {
      const { child } = get(id);
      if (!child.stdin) throw new Error('the standard input of the process is not a stream');
      return new Promise((resolve) => child.stdin.write(typeof data === 'string' ? data : Buffer.from(data), () => resolve(true)));
    },
    closeInput(id) { const { child } = get(id); if (child.stdin) child.stdin.end(); return true; },
    // [exit code, signal name or null]
    wait(id) {
      const p = get(id);
      if (p.exit) return p.exit;
      return new Promise((resolve) => p.exitWaiters.push(resolve));
    },
    status(id) { return get(id).exit; },
    kill(id, signal = 'SIGTERM') { get(id).child.kill(signal); return true; },
    error(id) { const e = get(id).error; return e ? String(e.message || e) : null; },
    forget(id) { procs.delete(id); return true; },
  };
}
