// Clysm as a library: load a compiled Lisp module into this thread (Node.js,
// Electron's main process) and call the functions it exports with JS:EXPORT.
//
//   import { load } from 'clysm/host/lib.mjs';
//   const lib = await load(new URL('./outline.wasm', import.meta.url));
//   lib.exports.outline(source);          // a Lisp function, called many times
//
// The module's toplevel forms run once, in load().  The instance lives on:
// every call of an export runs in it, with the global state that the earlier
// calls left.  A Lisp error that the function does not handle is thrown as an
// Error (name 'LispError', with the Lisp frames in its stack); a function
// exported with :async returns a promise and can wait with JS:AWAIT, and any
// number of such calls can wait at the same time.
//
// Options:
//   args      the command line arguments the Lisp side sees
//   stdout, stderr  functions of a string that receive the output of the
//             Lisp side (by default process.stdout and process.stderr)
//   stdin     a function that returns the next line of input (without its
//             newline), or null at end of file; by default input is empty
//   modules   an object of JavaScript modules that Lisp gets with
//             (js:module "name"), e.g. { typescript: ts }
//   import    the function that (js:import "specifier") calls; by default
//             the specifier is resolved from the working directory
//   fs        the file system (by default node:fs)
import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'node:module';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { makeImports, installSourceMap } from './common.mjs';
import { makeProcessManager } from './process.mjs';

export class LispExit extends Error {
  constructor(code) { super(`Lisp exited with code ${code}`); this.name = 'LispExit'; this.code = code; }
}

const clysmRoot = fileURLToPath(new URL('..', import.meta.url));

function makePlatform(state, { args = [], stdout, stderr, stdin, fs: fsys = fs }) {
  const sinks = {
    1: stdout || ((text) => process.stdout.write(text)),
    2: stderr || ((text) => process.stderr.write(text)),
  };
  const out = { 1: [], 2: [] };
  let scheduled = false;
  const flush = () => {
    for (const fd of [1, 2]) {
      if (out[fd].length) { const text = out[fd].join(''); out[fd] = []; sinks[fd](text); }
    }
  };
  state.flush = flush;
  state.writeText = (fd, text) => { flush(); (sinks[fd] || sinks[1])(text); };
  let input = [];
  return {
    write_char(fd, code) {
      const o = out[fd] || out[1];
      o.push(String.fromCodePoint(code));
      if (o.length > 8192) flush();
      else if (!scheduled) { scheduled = true; queueMicrotask(() => { scheduled = false; flush(); }); }
    },
    read_char() {
      flush();
      if (input.length === 0) {
        const line = stdin ? stdin() : null;
        if (line === null || line === undefined) return -1;
        input = Array.from(line + '\n', (c) => c.codePointAt(0));
      }
      return input.shift();
    },
    exit(code) { flush(); throw new LispExit(code); },
    flush() { flush(); },
    file_open(mode) {
      const name = state.nameBuf; state.nameBuf = '';
      try {
        if (mode === 0 || mode === 3) {
          const data = fsys.readFileSync(name, mode === 3 ? 'latin1' : 'utf8');
          state.setCharBuf(data);
          return data.length;
        }
        const content = state.writeBuf || ''; state.writeBuf = '';
        fsys.writeFileSync(name, mode === 2 ? Buffer.from(content, 'latin1') : content);
        return 0;
      } catch (e) { return -1; }
    },
    argc() { return args.length; },
    arg_len(i) { return args[i].length; },
    arg_char(i, j) { return args[i].codePointAt(j); },
    debug_mode() { return 0; },
    is_tty() { return 0; },
  };
}

function installGlobals(options) {
  const fsys = options.fs || fs;
  // subprocesses with streams (runtime/process.lisp), in this thread
  globalThis.__clysm_proc ??= makeProcessManager();
  globalThis.__clysm_fs ??= fsys;
  globalThis.__clysm_root ??= clysmRoot;
  if (!globalThis.__clysm_read_bytes) {
    let cachedPath = null, cachedBuf = null, cachedMtime = 0;
    globalThis.__clysm_read_bytes = (p, offset, length) => {
      const mtime = fsys.statSync(p).mtimeMs;
      if (p !== cachedPath || mtime !== cachedMtime) {
        cachedBuf = fsys.readFileSync(p); cachedPath = p; cachedMtime = mtime;
      }
      return new Uint8Array(cachedBuf.buffer, cachedBuf.byteOffset + offset, length);
    };
  }
}

// import() and require() of modules found from the working directory
export function nodeModuleAccess(directory = process.cwd()) {
  const req = createRequire(path.join(directory, 'noop.js'));
  return { importer: defaultImporter(req), require: req };
}

function defaultImporter(req) {
  return (specifier) => {
    let url = specifier;
    if (!/^(node:|file:|data:|https?:)/.test(specifier)) {
      try { url = pathToFileURL(req.resolve(specifier)).href; } catch (e) { url = specifier; }
    }
    return import(url);
  };
}

function instantiate(module, options) {
  installGlobals(options);
  const access = nodeModuleAccess();
  const state = {
    modules: options.modules || {},
    importer: options.import || access.importer,
    require: options.require || access.require,
  };
  const imports = makeImports(state, makePlatform(state, options));
  installSourceMap(state, module);
  const instance = new WebAssembly.Instance(module, imports);
  state.instance = instance;
  const exportsObject = {};
  const saved = globalThis.__clysm_exports;
  globalThis.__clysm_exports = exportsObject;
  try {
    instance.exports._start();
  } catch (e) {
    state.flush();
    if (e instanceof WebAssembly.Exception) {
      throw new Error('uncaught Lisp non-local exit while loading the module');
    }
    throw e;
  } finally {
    globalThis.__clysm_exports = saved;
  }
  state.flush();
  return {
    instance,
    exports: exportsObject,
    // a stack trace with the Lisp frames named by file and line
    mapStack: state.mapStack,
    // read and evaluate Lisp forms (in an image that has the evaluator)
    eval(source) { return instance.exports.eval_string(source); },
  };
}

async function bytesOf(source) {
  if (source instanceof URL || typeof source === 'string') {
    const p = source instanceof URL ? fileURLToPath(source) : source;
    return fs.promises.readFile(p);
  }
  return source;
}

// Load a compiled module: SOURCE is a path, a file: URL, the bytes, or a
// WebAssembly.Module.  Returns { instance, exports, eval }.
export async function load(source, options = {}) {
  const module = source instanceof WebAssembly.Module
    ? source
    : await WebAssembly.compile(await bytesOf(source));
  return instantiate(module, options);
}

// The same, synchronously (for CommonJS callers).
export function loadSync(source, options = {}) {
  let module = source;
  if (!(source instanceof WebAssembly.Module)) {
    const bytes = (source instanceof URL || typeof source === 'string')
      ? fs.readFileSync(source instanceof URL ? fileURLToPath(source) : source)
      : source;
    module = new WebAssembly.Module(bytes);
  }
  return instantiate(module, options);
}
