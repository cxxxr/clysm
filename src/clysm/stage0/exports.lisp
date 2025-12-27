;;;; exports.lisp - Wasm exports for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Defines the export section for Stage 0

(in-package #:clysm/stage0)

;;; ============================================================
;;; Export Registry
;;; ============================================================

(defvar *exported-functions* '()
  "List of exported function entries: (name func-index)")

(defun register-export (name func-index)
  "Register a function for export"
  (push (list name func-index) *exported-functions*))

(defun clear-exports ()
  "Clear export registry"
  (setf *exported-functions* nil))

;;; ============================================================
;;; Standard Exports
;;; ============================================================

(defun generate-exports ()
  "Generate export list for Stage 0.
   Returns list of (name kind index) suitable for emit-export-section."
  ;; Standard Stage 0 exports:
  ;; - compile_form: Compile a single form (index will be assigned)
  ;; - compile_all: Compile all modules (index will be assigned)
  ;; - _initialize: Runtime initialization
  (list
   ;; Compile form function - first user function after FFI imports
   '("compile_form" #x00 4)   ; kind=func, index=4 (after 4 FFI imports)
   ;; Compile all function
   '("compile_all" #x00 5)    ; kind=func, index=5
   ;; Initialize function
   '("_initialize" #x00 6)))  ; kind=func, index=6

;;; ============================================================
;;; Export Index Tracking
;;; ============================================================

(defvar *next-export-index* 4
  "Next available function index for exports (after FFI imports)")

(defun allocate-export-index ()
  "Allocate next export index"
  (prog1 *next-export-index*
    (incf *next-export-index*)))

(defun reset-export-indices ()
  "Reset export index allocation"
  (setf *next-export-index* 4))
