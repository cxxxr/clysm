;;;; contrib/ql-clysm.lisp — Quicklisp support for Clysm
;;;;
;;;; Quicklisp selects its implementation-specific code (sockets, directory
;;;; listing, ...) with classes in QL-IMPL; Clysm is not among them, so this
;;;; file adds a CLYSM implementation class and its methods.  It is loaded
;;;; by (require :quicklisp), or by (require :ql-clysm) after Quicklisp's
;;;; setup.lisp.  The same definitions are made for the bootstrap installer
;;;; (quicklisp.lisp, packages QLQS-*) when it is loaded.

(defpackage #:ql-clysm-support (:use #:cl))
(in-package #:ql-clysm-support)

(defun directory-entries (directory)
  ;; files and subdirectories (as directory pathnames)
  (let ((dir (make-pathname :name nil :type nil :version nil :defaults directory)))
    (remove-duplicates
     (append (directory (merge-pathnames (make-pathname :name :wild :type :wild) dir))
             (directory (merge-pathnames (make-pathname :directory '(:relative :wild)) dir)))
     :test #'equal :key #'namestring)))

(defun sym (name package)
  (and (find-package package) (find-symbol (string name) package)))

(defun install-for (impl-package network-package util-package)
  "Make the Clysm implementation the current one for the Quicklisp packages
given (the client's QL-IMPL/QL-NETWORK/QL-IMPL-UTIL, or the bootstrap
installer's QLQS-* packages)."
  (let ((lisp-class (sym "LISP" impl-package)))
    (when lisp-class
      (eval `(defclass ,(intern "CLYSM" impl-package) (,lisp-class) ()))
      (let ((clysm (sym "CLYSM" impl-package)))
        (macrolet ((def (pkg name ll &body body)
                     `(let ((gf (sym ,(concatenate 'string "%" name) ,pkg)))
                        (when (and gf (fboundp gf))
                          (eval (list* 'defmethod gf (list* (list 'impl clysm) ',ll)
                                       '(declare (ignorable impl))
                                       ',body))))))
          ;; a binary socket stream: Quicklisp's default READ-OCTETS,
          ;; WRITE-OCTETS and CLOSE-CONNECTION (stream functions) apply
          (def network-package "OPEN-CONNECTION" (host port)
            (sys::open-tcp-stream host port))
          (def util-package "DIRECTORY-ENTRIES" (directory)
            (ql-clysm-support::directory-entries directory))
          (def util-package "DELETE-DIRECTORY" (entry)
            (sys::%delete-directory entry))
          (def util-package "INIT-FILE-NAME" ()
            ".clysmrc")
          (def util-package "RENAME-DIRECTORY" (from to)
            (rename-file from to)))
        (setf (symbol-value (sym "*IMPLEMENTATION*" impl-package))
              (make-instance clysm))
        t))))

(defun install ()
  (let ((done nil))
    (when (find-package "QL-IMPL")
      (setq done (install-for "QL-IMPL" "QL-NETWORK" "QL-IMPL-UTIL")))
    (when (find-package "QLQS-IMPL")
      (setq done (or (install-for "QLQS-IMPL" "QLQS-NETWORK" "QLQS-IMPL-UTIL") done)))
    done))

(install)
