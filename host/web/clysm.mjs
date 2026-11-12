// Clysm browser host.  Usage:
//   const lisp = await loadClysm(bytes, (text, fd) => ...);
//   const printed = lisp.eval('(+ 1 2)');
import { makeImports } from '../common.mjs';

export async function loadClysm(bytes, onOutput = () => {}) {
  const state = { writeText: (fd, text) => onOutput(text, fd) };
  const platform = {
    write_char(fd, code) { onOutput(String.fromCodePoint(code), fd); },
    read_char() { return -1; },
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
