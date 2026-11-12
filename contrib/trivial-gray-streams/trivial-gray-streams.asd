;;;; trivial-gray-streams for Clysm.  Clysm implements Gray streams with the
;;;; trivial-gray-streams extensions (STREAM-READ-SEQUENCE, STREAM-WRITE-SEQUENCE
;;;; with START END, STREAM-FILE-POSITION) in its GRAY package, so this system
;;;; only re-exports them.  It is found before the Quicklisp release, which has
;;;; no Clysm support.
(defsystem "trivial-gray-streams"
  :description "Gray streams compatibility layer (Clysm's built-in Gray streams)"
  :license "MIT"
  :version "2.1"
  :components ((:file "package")))
