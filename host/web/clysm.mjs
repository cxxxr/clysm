// Clysm browser host.  Usage:
//   const lisp = await loadClysm(bytes, (text, fd) => ..., { readLine: () => ... });
//   const printed = lisp.eval('(+ 1 2)');
// readLine() supplies standard input: it returns the next line (without the
// newline), or null at end of file.  Without it, standard input is empty.
import { makeImports } from '../common.mjs';

export async function loadClysm(bytes, onOutput = () => {}, { readLine } = {}) {
  const state = { writeText: (fd, text) => onOutput(text, fd) };
  let input = [];   // code points of the current input line
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
    file_open() { state.nameBuf = ''; return -1; },
    argc() { return 0; },
    arg_len() { return 0; },
    arg_char() { return 0; },
    debug_mode() { return 0; },
    is_tty() { return 0; },
    flush() {},
  };
  const { instance } = await WebAssembly.instantiate(bytes, makeImports(state, platform));
  state.instance = instance;
  instance.exports._start();
  return {
    instance,
    eval(source) { return instance.exports.eval_string(source); },
  };
}
