;;;; trivial-garbage for Clysm.  The host's garbage collector is not visible to
;;;; Lisp: weak pointers and weak hash tables hold their objects strongly, and
;;;; finalizers are recorded but never run.  It is found before the Quicklisp
;;;; release, which has no Clysm support.
(defsystem "trivial-garbage"
  :description "Portable finalizers, weak hash-tables and weak pointers (Clysm)"
  :licence "Public Domain"
  :components ((:file "trivial-garbage")))
