;;;; The part of micros (Lem's SWANK) that lem/core and lem-lisp-syntax use.  The real micros
;;;; is a server for the Lisp mode, which needs a backend for the
;;;; implementation; Lem on Clysm evaluates in its own image instead
;;;; (contrib/lem/src/lisp-mode.lisp).
(defsystem "micros"
  :description "The part of micros used by lem/core (Clysm)"
  :components ((:file "micros")))
