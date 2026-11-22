;;; A small application of two systems, built into one module by
;;; `clysm build` (tests/static/run.sh).
(defsystem "geom"
  :depends-on ("geom-util")
  :serial t
  :components ((:file "package") (:file "shapes") (:file "registry")))
