;;;; static-vectors for Clysm.  There is no foreign memory in Clysm (Lisp
;;;; objects are WebAssembly GC objects), so a "static" vector is an ordinary
;;;; specialized vector; the pointer functions signal an error.  It is found
;;;; before the Quicklisp release, which needs CFFI.
(defsystem "static-vectors"
  :description "Vectors allocated in static memory (Clysm: ordinary vectors)"
  :license "MIT"
  :version "1.9.3"
  :components ((:file "static-vectors")))
