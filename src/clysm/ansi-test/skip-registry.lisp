;;;; skip-registry.lisp - Skip detection configuration and functions
;;;;
;;;; Implements the skip-registry entity and *default-skip-registry* parameter.
;;;; Also contains form detection functions used by classifier.

(in-package #:clysm/ansi-test)

;;; ==========================================================================
;;; T016: skip-registry defstruct and *default-skip-registry*
;;; ==========================================================================

(defstruct (skip-registry (:constructor make-skip-registry))
  "Registry of forms and categories to skip."
  (unsupported-forms nil :type list)
  (unsupported-categories nil :type list)
  (skipped-tests nil :type list)
  (timeout-seconds 30 :type (unsigned-byte 16)))

(defparameter *default-skip-registry*
  (make-skip-registry
   :unsupported-forms
   '(;; I/O forms
     format print prin1 princ write pprint
     open close with-open-file with-open-stream
     read read-line read-char peek-char unread-char
     write-char write-string write-line terpri fresh-line
     ;; CLOS forms
     defgeneric defmethod defclass
     make-instance slot-value slot-boundp slot-makunbound
     change-class update-instance-for-redefined-class
     update-instance-for-different-class
     ;; System forms
     compile-file load require provide
     room describe inspect ed dribble
     ;; Environment forms
     documentation
     ;; Complex I/O
     *standard-input* *standard-output* *error-output*
     *trace-output* *query-io* *debug-io* *terminal-io*
     ;; T012: Control flow forms (not yet implemented in Clysm)
     loop for while do do* dotimes dolist
     let let* flet labels
     ;; T013: Macro forms
     macrolet symbol-macrolet
     ;; T014: Multiple-value forms
     multiple-value-bind multiple-value-list multiple-value-call
     multiple-value-prog1 multiple-value-setq values values-list
     nth-value
     ;; T015: Side-effect forms
     incf decf push pop pushnew
     setf setq psetf psetq rotatef shiftf
     ;; T016: Error handling forms
     handler-case handler-bind ignore-errors restart-case
     restart-bind with-simple-restart invoke-restart
     ;; T017: ANSI test framework forms (not standard CL)
     signals-error eqt equalt notnot notnot-mv
     expand-in-current-env def-fold-test
     classify-error check-type-error
     ;; ANSI test framework globals
     *universe* *numbers* *symbols* *conses* *booleans*
     *characters* *strings* *bit-vectors* *pathnames*
     *random-states* *hash-tables* *readtables* *packages*
     *functions* *conditions* *standard-chars* *mini-universe*)
   :unsupported-categories
   '("files" "streams" "pathnames" "printer" "reader"
     "environment" "system-construction")
   :skipped-tests nil
   :timeout-seconds 30)
  "Default skip registry for ANSI test suite execution.
Contains forms and categories known to be unsupported by Clysm.")

;;; ==========================================================================
;;; Skip detection functions (T052-T054 stubs for Phase 6)
;;; ==========================================================================

(defun contains-unsupported-form-p (form registry)
  "Check if FORM or any subform contains an unsupported form from REGISTRY.
Returns the first unsupported form found, or NIL.
Uses symbol name comparison to handle cross-package symbol identity."
  (let ((unsupported-names (mapcar #'symbol-name
                                   (skip-registry-unsupported-forms registry))))
    (labels ((check (expr)
               (cond
                 ((null expr) nil)
                 ((symbolp expr)
                  (when (member (symbol-name expr) unsupported-names
                                :test #'string=)
                    expr))
                 ((atom expr) nil)
                 (t
                  (or (when (and (symbolp (car expr))
                                 (member (symbol-name (car expr)) unsupported-names
                                         :test #'string=))
                        (car expr))
                      (check (car expr))
                      (check (cdr expr)))))))
      (check form))))

(defun detect-skip-reason (test-case &optional (registry *default-skip-registry*))
  "Detect if TEST-CASE should be skipped and return skip reason string, or NIL.
Checks form-level and category-level skip conditions."
  (let ((category (test-case-category test-case))
        (form (test-case-form test-case)))
    ;; Check category-level skip
    (when (member category (skip-registry-unsupported-categories registry)
                  :test #'string-equal)
      (return-from detect-skip-reason
        (format nil "unsupported-category: ~A" category)))
    ;; Check form-level skip
    (let ((unsupported-form (contains-unsupported-form-p form registry)))
      (when unsupported-form
        (return-from detect-skip-reason
          (format nil "unsupported-form: ~A" unsupported-form))))
    ;; Check specific test skip
    (when (member (test-case-name test-case)
                  (skip-registry-skipped-tests registry)
                  :test #'eq)
      (return-from detect-skip-reason
        (format nil "explicitly-skipped: ~A" (test-case-name test-case))))
    nil))
