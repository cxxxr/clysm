;;;; setup.lisp — make Lem's systems loadable by ASDF on Clysm (after
;;;; (require :asdf)): the stubs of the libraries that Lem on Clysm does
;;;; without, the system lem-clysm, and the fixups of Lem's system
;;;; definitions.  The directories of Lem itself and of the libraries come
;;;; from the caller (prepare.lisp, or the registry that it writes).

(in-package :asdf)

(let ((here (uiop:pathname-directory-pathname *load-truename*)))
  (dolist (stub '("micros" "log4cl" "dexador" "usocket" "deploy"))
    (pushnew (merge-pathnames (format nil "stubs/~A/" stub) here) *central-registry*
             :test #'equal))
  (pushnew here *central-registry* :test #'equal))

;;; The extension manager installs packages with Quicklisp at run time; it
;;; is not part of Lem on Clysm.
(define-clysm-fixup "lem/core" (s)
  (clysm-drop-dependencies s "lem-extension-manager")
  (clysm-enable-component s '("ext" "extension-commands") nil))
