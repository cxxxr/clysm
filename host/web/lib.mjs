// Clysm as a library where there is no Node.js: a page, a browser's worker,
// Electron's renderer (and Node.js too, for it uses none of its modules).
// The counterpart of host/lib.mjs for a library whose module is given as its
// bytes (a library of (js:library-options :embed t), whose modules hold them):
//
//   import { loadSync } from './clysm/web/lib.mjs';
//   const lib = loadSync(bytes);
//   lib.exports.outline(source);
//
// The module is compiled and instantiated at once, in the calling thread: a
// browser allows that on its main thread for a module of 8 MB (Chrome) or more
// (Firefox, Safari). Its toplevel forms run once, and the instance lives on.
// The program has no files, no processes and no command line; what it writes
// to its standard output goes to console.log, a line at a time, and its error
// output to console.error.
//
// Options:
//   stdout, stderr  functions of a string that receive what the program writes
//   outputToErrors  the standard output goes where the error output does
//   modules   an object of JavaScript modules that Lisp gets with (js:module "name")
//   import    the function that (js:import "specifier") calls (by default import())
//   classes   the classes of JS:DEFINE-CLASS, by name, that the modules of a
//             lazy library made before loading it (they get their functions)
import { makeImports, installSourceMap } from '../common.mjs';

function lines(sink) {
  let pending = '';
  return {
    write(text) {
      pending += text;
      const end = pending.lastIndexOf('\n');
      if (end >= 0) { sink(pending.slice(0, end)); pending = pending.slice(end + 1); }
    },
    flush() { if (pending !== '') { sink(pending); pending = ''; } },
  };
}

export function loadSync(source, options = {}) {
  const module = source instanceof WebAssembly.Module ? source : new WebAssembly.Module(source);
  const err = options.stderr ? { write: options.stderr, flush() {} } : lines((t) => console.error(t));
  // (outputToErrors: the standard output goes where the error output does)
  const out = options.stdout ? { write: options.stdout, flush() {} }
    : options.outputToErrors ? err : lines((t) => console.log(t));
  const sinks = { 1: out, 2: err };
  const state = {
    jsModules: options.modules || {},
    importer: options.import || ((specifier) => import(specifier)),
    require: () => { throw new Error('a library of the web has no require'); },
  };
  const flush = () => { out.flush(); err.flush(); };
  state.flush = flush;
  state.writeText = (fd, text) => (sinks[fd] || out).write(text);
  const platform = {
    write_char(fd, code) { (sinks[fd] || out).write(String.fromCodePoint(code)); },
    read_char() { return -1; },
    exit(code) { flush(); throw new Error(`Lisp exited with code ${code}`); },
    flush() { flush(); },
    file_open() { state.nameBuf = ''; return -1; },
    argc() { return 0; },
    arg_len() { return 0; },
    arg_char() { return 0; },
    debug_mode() { return 0; },
    is_tty() { return 0; },
  };
  const imports = makeImports(state, platform);
  installSourceMap(state, module);
  const instance = new WebAssembly.Instance(module, imports);
  state.instance = instance;
  const exportsObject = {};
  const library = { exports: {}, declarations: [], classes: {}, options: {}, made: options.classes ?? {} };
  const saved = globalThis.__clysm_exports, savedLibrary = globalThis.__clysm_library;
  globalThis.__clysm_exports = exportsObject;
  globalThis.__clysm_library = library;
  try {
    instance.exports._start();
  } catch (e) {
    flush();
    if (e instanceof WebAssembly.Exception) throw new Error('uncaught Lisp non-local exit while loading the module');
    throw e;
  } finally {
    globalThis.__clysm_exports = saved;
    globalThis.__clysm_library = savedLibrary;
  }
  flush();
  return { instance, exports: exportsObject, library, mapStack: state.mapStack };
}

// The bytes of a module written as base64 (what the modules of an embedded library hold).
export function bytesOfBase64(text) {
  const binary = atob(text);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
  return bytes;
}
