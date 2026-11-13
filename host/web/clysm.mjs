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
import { makeImports, installSourceMap } from '../common.mjs';

export async function loadClysm(bytes, onOutput = () => {}, { readLine, fs, root, args = [] } = {}) {
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
  const { module, instance } = await WebAssembly.instantiate(bytes, makeImports(state, platform));
  installSourceMap(state, module);
  state.instance = instance;
  instance.exports._start();
  return {
    instance,
    eval(source) { return instance.exports.eval_string(source); },
  };
}
