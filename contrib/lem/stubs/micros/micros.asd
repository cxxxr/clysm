;;;; The part of micros (Lem's SWANK) that lem/core uses.  The real micros
;;;; is a server for the Lisp mode, which needs a backend for the
;;;; implementation; Lem on Clysm has no Lisp mode yet.
(defsystem "micros"
  :description "The part of micros used by lem/core (Clysm)"
  :components ((:file "micros")))
