;;;; tests/quicklisp/install.lisp — install Quicklisp from scratch and load a
;;;; library downloaded from quicklisp.org (needs network access).
;;;;   bin/clysm script tests/quicklisp/install.lisp BOOTSTRAP-FILE HOME-DIR
(destructuring-bind (bootstrap home) (cdr sys::*posix-argv*)
  (load bootstrap)
  (require :ql-clysm)
  (funcall (find-symbol "INSTALL" "QUICKLISP-QUICKSTART") :path home))
(format t "~&implementation: ~A~%" (type-of (symbol-value (find-symbol "*IMPLEMENTATION*" "QL-IMPL"))))
(funcall (find-symbol "QUICKLOAD" "QL") :split-sequence :silent t)
(format t "split-sequence: ~S~%"
        (funcall (find-symbol "SPLIT-SEQUENCE" "SPLIT-SEQUENCE") #\, "a,b,,c"))
;; a library with several dependencies (cl-ppcre, cl-unicode, flexi-streams
;; with Gray streams, cl-change-case)
(funcall (find-symbol "QUICKLOAD" "QL") :str :silent t)
(format t "str: ~S~%" (funcall (find-symbol "SPLIT" "STR") "," "x,y"))
