;;;; generic.lisp - Generic functions (Phase 10 - T230-T232)

(in-package #:clysm/clos/generic)

;;; ============================================================
;;; Generic Function Registry
;;; ============================================================

(defvar *gf-registry* (make-hash-table :test 'eq)
  "Registry of generic functions.")

(defun register-gf (name gf)
  "Register a generic function."
  (setf (gethash name *gf-registry*) gf))

(defun find-gf (name)
  "Find a generic function by name."
  (gethash name *gf-registry*))

;;; ============================================================
;;; Generic Function Structure (T230)
;;; ============================================================

(defstruct (generic-function (:conc-name gf-))
  "Generic function."
  (name nil :type symbol)
  (methods nil :type list)
  (lambda-list nil :type list)
  (method-combination :standard))

;;; ============================================================
;;; defgeneric (T230)
;;; ============================================================

(defun defgeneric* (name lambda-list &rest options)
  "Define a generic function."
  (declare (ignore options))
  (let ((gf (make-generic-function
             :name name
             :lambda-list lambda-list
             :methods nil)))
    (register-gf name gf)
    gf))

;;; ============================================================
;;; Method Structure
;;; ============================================================

(defstruct method*
  "A method in a generic function."
  (specializers nil :type list)
  (qualifier nil)
  (function nil :type (or null function))
  (lambda-list nil :type list))

;;; ============================================================
;;; Generic Function Methods
;;; ============================================================

(defun gf-add-method (gf method)
  "Add a method to a generic function."
  ;; Remove any existing method with same specializers and qualifier
  (setf (gf-methods gf)
        (remove-if (lambda (m)
                     (and (equal (method*-specializers m)
                                 (method*-specializers method))
                          (eq (method*-qualifier m)
                              (method*-qualifier method))))
                   (gf-methods gf)))
  ;; Add the new method
  (push method (gf-methods gf))
  gf)

(defun gf-find-method (gf specializers &optional qualifier)
  "Find a method with given specializers and qualifier."
  (find-if (lambda (m)
             (and (equal (method*-specializers m) specializers)
                  (eq (method*-qualifier m) qualifier)))
           (gf-methods gf)))

