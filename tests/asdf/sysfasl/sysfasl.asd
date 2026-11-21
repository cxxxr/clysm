;;; a system made into one system fasl (tests/asdf/run.lisp copies it to build/)
(defsystem "sysfasl"
  :serial t
  :components ((:file "p") (:file "m") (:file "u")))
