;;;; tools/static-build.lisp — `clysm build`: loaded in the REPL image, which
;;;; builds the program (src/static.lisp).
;;;;   clysm build [--system NAME]... [--asd FILE.asd]... [--load FILE]... [--asdf] [--shake] [--eval FORM]... [-o OUT.wasm] [FILE...]
(clysm::static-build-main (cdr sys::*posix-argv*))
