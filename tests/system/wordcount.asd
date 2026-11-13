;;; A small system for bin/clysm library --system (tests/system/run.sh).
(defsystem "wordcount"
  :depends-on ("split-sequence")
  :serial t
  :components ((:file "package") (:file "count")))
