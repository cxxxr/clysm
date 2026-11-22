;;; tests/lib/package.lisp as a library (not lazy) whose standard output goes to
;;; the standard error output (tests/lib/run.sh).
(js:library-options :output :error)
