;;;; Lem (https://github.com/lem-project/lem) on Clysm, with a frontend
;;;; written with Clysm's UI framework (ui/): the editor runs in the page of
;;;; a desktop application (Electron), and its windows are elements of the
;;;; page.  See README.md here; contrib/lem/build.sh builds the application.
;;;;
;;;; The image that loads it contains the UI framework (build.sh builds it).
(defsystem "lem-clysm"
  :description "Lem with a frontend written with Clysm's UI framework"
  :licence "MIT"
  :depends-on ("lem/core" "lem-lisp-syntax" "lem-paredit-mode")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "single-thread")
               (:file "files")
               (:file "display")
               (:file "input")
               (:file "frontend")
               ;; the grammar of Lem's Lisp mode (the rest of that mode needs
               ;; a Lisp process and native libraries)
               (:file "grammar" :pathname #.(asdf:system-relative-pathname
                                             "lem" "extensions/lisp-mode/grammar.lisp"))
               (:file "lisp-mode")))
