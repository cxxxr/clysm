;;;; contrib/asdf-hooks.lisp — loaded by (require :asdf) after ASDF itself.

(in-package :asdf)

;;; Systems that Clysm provides itself (compatibility layers for
;;; implementation-specific libraries) are found before others, e.g. before
;;; Quicklisp's releases.
(let ((contrib (uiop:pathname-directory-pathname
                (or *load-truename* (error "asdf-hooks.lisp must be loaded")))))
  (pushnew (merge-pathnames "trivial-gray-streams/" contrib) *central-registry* :test #'equal))

;;; When the Quicklisp client system is loaded (by Quicklisp's setup.lisp),
;;; add Clysm's implementation of Quicklisp's host interface, before the
;;; client is used.

(defmethod perform :after ((o load-op) (s system))
  (when (string= (component-name s) "quicklisp")
    (require "QL-CLYSM")
    (funcall (find-symbol "INSTALL" "QL-CLYSM-SUPPORT"))))
