;;;; slot-access.lisp - Slot access (Phase 10 - T227-T229)

(in-package #:clysm/clos/slot-access)

;;; ============================================================
;;; slot-value (T227)
;;; ============================================================

(defun slot-value* (instance slot-name)
  "Get the value of a slot in an instance."
  (clysm/clos/mop:%slot-value instance slot-name))

;;; ============================================================
;;; (setf slot-value) (T228)
;;; ============================================================

(defun set-slot-value* (instance slot-name value)
  "Set the value of a slot in an instance."
  (setf (clysm/clos/mop:%slot-value instance slot-name) value))

(defun (setf slot-value*) (value instance slot-name)
  "Setf expander for slot-value*."
  (set-slot-value* instance slot-name value))

;;; ============================================================
;;; slot-boundp
;;; ============================================================

(defvar *unbound-slot-marker* (gensym "UNBOUND")
  "Marker for unbound slots.")

(defun slot-boundp* (instance slot-name)
  "Check if a slot is bound."
  (not (eq (slot-value* instance slot-name) *unbound-slot-marker*)))

;;; ============================================================
;;; slot-makunbound
;;; ============================================================

(defun slot-makunbound* (instance slot-name)
  "Make a slot unbound."
  (set-slot-value* instance slot-name *unbound-slot-marker*)
  instance)

;;; ============================================================
;;; Accessor Generation (T229)
;;; ============================================================

(defun make-reader-function (slot-name)
  "Create a reader function for a slot."
  (lambda (instance)
    (slot-value* instance slot-name)))

(defun make-writer-function (slot-name)
  "Create a writer function for a slot."
  (lambda (value instance)
    (set-slot-value* instance slot-name value)))

(defun make-accessor-functions (slot-name)
  "Create reader and writer functions for a slot."
  (values (make-reader-function slot-name)
          (make-writer-function slot-name)))

