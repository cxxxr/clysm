;;; A system that tests/static/geom.asd depends on (tests/static/run.sh).
(defsystem "geom-util"
  :serial t
  :components ((:file "util-package") (:file "util-macros") (:file "util")))
