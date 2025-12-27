;;; interpreter-api.lisp - Contract Definitions for Extended Interpreter
;;;
;;; Feature: 044-interpreter-bootstrap
;;; Date: 2025-12-28
;;;
;;; This file defines the public API contracts for the extended interpreter.
;;; All functions listed here are part of the stable API.

(in-package :clysm/eval/interpreter)

;;; ============================================================
;;; Core Interpretation API (FR-001 through FR-015)
;;; ============================================================

(declaim (ftype (function (t &optional interpreter-env) t) interpret))
;; (interpret form &optional env) → result
;; Interpret a single form in the given environment.
;; If ENV is nil, uses the default root environment.

(declaim (ftype (function (pathname &optional interpreter-env) t) interpret-file))
;; (interpret-file path &optional env) → result
;; Load and interpret all forms from a source file.
;; Handles in-package forms to switch packages during loading.
;; Returns the value of the last form.

(declaim (ftype (function (&optional interpreter-env) interpreter-env) make-interpreter-env))
;; (make-interpreter-env &optional parent) → env
;; Create a new interpreter environment.
;; If PARENT is nil, creates root environment with builtins.

(declaim (ftype (function (interpreter-env list) interpreter-env) extend-env))
;; (extend-env env bindings) → new-env
;; Create a child environment with additional bindings.
;; BINDINGS is an alist of (symbol . value) pairs.

;;; ============================================================
;;; Macro System API (FR-016 through FR-019)
;;; ============================================================

(declaim (ftype (function (symbol) (or null macro-expander)) interpreter-macro-function))
;; (interpreter-macro-function name) → expander-or-nil
;; Return the macro expander for NAME, or nil if not a macro.

(declaim (ftype (function (symbol macro-expander) macro-expander)
                (setf interpreter-macro-function)))
;; (setf (interpreter-macro-function name) expander)
;; Register a macro expander under NAME.

(declaim (ftype (function (t &optional interpreter-env) (values t boolean)) interpreter-macroexpand-1))
;; (interpreter-macroexpand-1 form &optional env) → expanded-form, expanded-p
;; Expand form once if it's a macro call.
;; Returns second value T if expansion occurred.

(declaim (ftype (function (t &optional interpreter-env) (values t boolean)) interpreter-macroexpand))
;; (interpreter-macroexpand form &optional env) → expanded-form, expanded-p
;; Repeatedly expand form until no more macro calls.

;;; ============================================================
;;; Bootstrap API (FR-028 through FR-035)
;;; ============================================================

(in-package :clysm/bootstrap)

(declaim (ftype (function (&key (:output-path pathname)
                                (:verbose boolean))
                          bootstrap-result)
                generate-stage0-via-interpreter))
;; (generate-stage0-via-interpreter &key output-path verbose) → result
;; Generate Stage 0 Wasm binary by interpreting compiler source.
;; OUTPUT-PATH defaults to "dist/clysm-stage0-interp.wasm".
;; Returns bootstrap-result struct with success status and statistics.

(declaim (ftype (function (pathname pathname) (values boolean string))
                verify-fixpoint))
;; (verify-fixpoint stage1-path stage2-path) → identical-p, message
;; Compare Stage 1 and Stage 2 binaries for byte-for-byte equality.
;; Returns T if identical, NIL if different.
;; MESSAGE describes the comparison result.

(declaim (ftype (function (&key (:stage0-path pathname)
                                (:output-dir pathname)
                                (:json-output boolean))
                          (values keyword t))
                run-full-bootstrap))
;; (run-full-bootstrap &key stage0-path output-dir json-output) → status, report
;; Run complete Stage 0 → 1 → 2 bootstrap chain.
;; Returns :ACHIEVED if fixed-point reached, :NOT-ACHIEVED otherwise.
;; REPORT is JSON string if json-output is T, otherwise plist.

;;; ============================================================
;;; Special Form Handlers (internal, but documented for testing)
;;; ============================================================

(in-package :clysm/eval/interpreter)

;; Each special form has an interpret-* function
;; Pattern: (interpret-NAME form env) → result

;; Already implemented:
;; interpret-if, interpret-progn, interpret-let, interpret-lambda
;; interpret-setq, interpret-block, interpret-return-from
;; interpret-tagbody, interpret-go, interpret-flet, interpret-labels

;; To be implemented (FR-001 through FR-015):
;; interpret-defun           ; FR-001
;; interpret-defmacro        ; FR-002
;; interpret-defvar          ; FR-003
;; interpret-defparameter    ; FR-003
;; interpret-defconstant     ; FR-003
;; interpret-defstruct       ; FR-004
;; interpret-cond            ; FR-005
;; interpret-case            ; FR-006
;; interpret-ecase           ; FR-006
;; interpret-typecase        ; FR-007
;; interpret-etypecase       ; FR-007
;; interpret-ctypecase       ; FR-007
;; interpret-loop            ; FR-008 (via macro expansion)
;; interpret-handler-case    ; FR-009
;; interpret-handler-bind    ; FR-009
;; interpret-restart-case    ; FR-010
;; interpret-restart-bind    ; FR-010
;; interpret-multiple-value-bind ; FR-011
;; interpret-unwind-protect  ; FR-012
;; interpret-the             ; FR-013
;; interpret-locally         ; FR-013
;; interpret-eval-when       ; FR-014
;; interpret-catch           ; (from blessed subset)
;; interpret-throw           ; (from blessed subset)

;;; ============================================================
;;; Error Conditions
;;; ============================================================

(define-condition interpreter-error (error)
  ((form :initarg :form :reader interpreter-error-form))
  (:documentation "Base condition for interpreter errors."))

(define-condition unbound-variable-error (interpreter-error)
  ((name :initarg :name :reader unbound-variable-name))
  (:documentation "Signaled when looking up an unbound variable."))

(define-condition undefined-function-error (interpreter-error)
  ((name :initarg :name :reader undefined-function-name))
  (:documentation "Signaled when calling an undefined function."))

(define-condition unsupported-feature-error (interpreter-error)
  ((feature :initarg :feature :reader unsupported-feature-name))
  (:documentation "Signaled when encountering an unsupported CL feature."))

(define-condition macro-expansion-depth-error (interpreter-error)
  ((depth :initarg :depth :reader macro-expansion-depth))
  (:documentation "Signaled when macro expansion exceeds depth limit (1000)."))
