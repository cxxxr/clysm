;;; a system edited after its system fasl was made (tests/asdf/run.lisp copies it to build/)
(defsystem "sysdev"
  :serial t
  :components ((:file "a") (:file "b")))
