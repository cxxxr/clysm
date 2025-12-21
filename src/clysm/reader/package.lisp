;;;; package.lisp - Package system for Clysm reader

(in-package #:clysm/reader/package)

(defvar *packages* (make-hash-table :test 'equal)
  "Table of defined packages.")

(defun make-package* (name)
  "Create a new package."
  (let ((pkg (list :name name :symbols (make-hash-table :test 'equal))))
    (setf (gethash name *packages*) pkg)
    pkg))

(defun find-package* (name)
  "Find a package by name."
  (gethash name *packages*))

(defun intern-symbol (name &optional (package-name "CLYSM-USER"))
  "Intern a symbol in a package."
  ;; TODO: Implement
  (declare (ignore name package-name))
  nil)

(defun find-symbol* (name &optional (package-name "CLYSM-USER"))
  "Find a symbol in a package."
  ;; TODO: Implement
  (declare (ignore name package-name))
  nil)
