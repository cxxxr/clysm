// Clysm browser host.  Usage:
//   const lisp = await loadClysm(bytes, (text, fd) => ..., { readLine: () => ... });
//   const printed = lisp.eval('(+ 1 2)');
// readLine() supplies standard input: it returns the next line (without the
// newline), or null at end of file.  Without it, standard input is empty.
//
// In a page that can use Node.js (an Electron window with nodeIntegration),
// pass { fs: require('node:fs'), root: '/path/to/clysm/', args: [...] }: Lisp
// then has the files of the computer, as on the Node.js host (OPEN,
// DIRECTORY, LOAD, ASDF...), REQUIRE finds the modules of the Clysm
// installation ROOT, and ARGS are the command line arguments.  In a page
// without Node.js, `fs` can be the file system in memory of vfs.mjs.
// BYTES can also be a compiled WebAssembly.Module; the module of the image
// is globalThis.__clysm_module, which a page can give to a worker.
import { makeImports, installSourceMap, imageBuild, imageHeader, imageOctets, compileImageModules, loadImage,
         instrumentWasm } from '../common.mjs';

// IMAGE: a heap image (runtime/image.lisp), as octets or the name of a file
// (with FS): the image starts from it if it was saved by this build of the
// image, instead of running its toplevel forms.  With TRAIN, the image and
// the modules note the functions that run (instrumentWasm), which
// globalThis.__clysm_training() returns (for compilation hints).
export async function loadClysm(bytes, onOutput = () => {}, { readLine, fs, root, args = [], image, train } = {}) {
  const state = { writeText: (fd, text) => onOutput(text, fd) };
  let input = [];   // code points of the current input line
  if (fs) {
    globalThis.__clysm_fs = fs;
    // octets of a file (compiled modules in fasls); the last file read is cached
    let cachedPath = null, cachedBuffer = null, cachedMtime = 0;
    globalThis.__clysm_read_bytes = (path, offset, length) => {
      const mtime = fs.statSync(path).mtimeMs;
      if (path !== cachedPath || mtime !== cachedMtime) {
        cachedBuffer = fs.readFileSync(path); cachedPath = path; cachedMtime = mtime;
      }
      return new Uint8Array(cachedBuffer.buffer, cachedBuffer.byteOffset + offset, length);
    };
  }
  if (root) globalThis.__clysm_root = root;
  const platform = {
    write_char(fd, code) { onOutput(String.fromCodePoint(code), fd); },
    read_char() {
      if (input.length === 0) {
        const line = readLine ? readLine() : null;
        if (line === null || line === undefined) return -1;
        input = Array.from(line + '\n', (c) => c.codePointAt(0));
      }
      return input.shift();
    },
    exit(code) { throw new Error(`Lisp exited with code ${code}`); },
    file_open(mode) {
      // the file name was accumulated by put_char_buf
      const name = state.nameBuf; state.nameBuf = '';
      if (!fs) return -1;
      try {
        if (mode === 0 || mode === 3) {
          // 0: text (UTF-8), 3: binary (one char per octet)
          const data = fs.readFileSync(name, mode === 3 ? 'latin1' : 'utf8');
          state.setCharBuf(data);
          return data.length;
        }
        const content = state.writeBuf || ''; state.writeBuf = '';
        fs.writeFileSync(name, content, mode === 2 ? 'latin1' : 'utf8');
        return 0;
      } catch (e) { return -1; }
    },
    argc() { return args.length; },
    arg_len(i) { return args[i].length; },
    arg_char(i, j) { return args[i].codePointAt(j); },
    debug_mode() { return 0; },
    is_tty() { return 0; },
    flush() {},
  };
  const imports = makeImports(state, platform);
  let transform;
  if (train) {
    // the functions of the image are numbered from 0, then those of each module
    // (their octets in the linear memory from 192 MB, above the heap image)
    const table = 192 << 20;
    const bases = [];
    const instrumented = instrumentWasm(new Uint8Array(bytes), 0, table);
    let next = instrumented.count;
    bytes = instrumented.bytes;
    transform = (b, i) => { const r = instrumentWasm(b, next, table); bases[i] = next; next += r.count; return r.bytes; };
    state.trainTable = () => next;
    globalThis.__clysm_training = () => {
      const ran = new Uint8Array(state.instance.exports.memory.buffer, table, next);
      const base = [];
      for (let f = 0; f < instrumented.count; f++) if (ran[f]) base.push(f);
      const modules = bases.map((b, i) => {
        const end = i + 1 < bases.length ? bases[i + 1] : next;
        const list = [];
        for (let f = b; f < end; f++) if (ran[f]) list.push(f - b);
        return list;
      });
      return { base, modules };
    };
  }
  // the modules of the heap image are compiled while the image is compiled
  // and instantiated; the image is compiled from the octets that the heap
  // image has of it as it starts from it, if it has them (imageOctets)
  let file = null, header = null, modules = null, octets = null;
  if (image) {
    try {
      file = image instanceof Uint8Array ? image : (fs && fs.existsSync(image) ? fs.readFileSync(image) : null);
      header = imageHeader(bytes, file);
      if (header) modules = compileImageModules(file, header, transform, (path) => fs.readFileSync(path));
      if (header && !train) octets = imageOctets(file, header);
    } catch (e) {
      console.error('the heap image could not be read:', e);
    }
  }
  const original = () => (bytes instanceof WebAssembly.Module ? bytes : WebAssembly.compile(bytes));
  let module;
  try {
    module = await (octets ? WebAssembly.compile(octets) : original());
  } catch (e) {
    if (!octets) throw e;
    octets = null;
    module = await original();
  }
  let instance = await WebAssembly.instantiate(module, imports);
  // (a worker that starts the image by its toplevel forms needs the image
  // itself: its octets, when the one compiled is the image as it starts from
  // the heap image)
  globalThis.__clysm_module = octets ? bytes : module;
  installSourceMap(state, module);
  state.instance = instance;
  state.module = module;
  state.build = imageBuild(module);
  if (train) {
    // (room for the octets of the functions)
    const memory = instance.exports.memory;
    const need = Math.ceil(((192 << 20) + state.trainTable()) / 65536);
    memory.grow(need - memory.buffer.byteLength / 65536);
  }
  let restored = false;
  if (modules) {
    try {
      await loadImage(state, file, header, await modules);
      restored = true;
    } catch (e) {
      console.error('the heap image could not be loaded:', e);
      if (octets) { module = await original(); installSourceMap(state, module); state.module = module; }
      instance = await WebAssembly.instantiate(module, imports);
      state.instance = instance;
    }
  }
  file = null;
  if (restored) instance.exports.image_resume(); else instance.exports._start();
  return {
    instance,
    eval(source) { return instance.exports.eval_string(source); },
  };
}
