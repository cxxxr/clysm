;;;; closer-mop for Clysm.  Clysm's metaobject protocol is in the package
;;;; CLYSM-MOP; this system defines Closer to MOP's packages (CLOSER-MOP,
;;;; CLOSER-COMMON-LISP and CLOSER-COMMON-LISP-USER) on top of it and its
;;;; utility functions.  It is found before the Quicklisp release, which has
;;;; no Clysm support.
(defsystem "closer-mop"
  :name "Closer to MOP"
  :description "Closer to MOP for Clysm (the built-in metaobject protocol)"
  :license "MIT-style license"
  :version "1.0.0"
  :serial t
  :components ((:file "closer-mop")))
