;;;; compiler.lisp - Core compilation for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Provides compile-expression, compile-module, compile-forms

(in-package #:clysm/stage0)

;;; ============================================================
;;; Compilation Environment
;;; ============================================================

(defstruct compile-env
  "Compilation environment"
  (functions (make-hash-table) :type hash-table)
  (macros (make-hash-table) :type hash-table)
  (variables (make-hash-table) :type hash-table)
  (type-index +total-types+ :type integer)
  (func-index 4 :type integer)  ; After 4 FFI imports
  (global-index +total-globals+ :type integer))

(defvar *compile-env* nil
  "Current compilation environment")

;;; ============================================================
;;; Expression Compilation
;;; ============================================================

(defun compile-expression (form)
  "Compile a single expression to Wasm IR.
   Returns function-body struct or NIL on failure."
  (handler-case
      (let* ((ast (parse-expression form))
             (ir (ast-to-ir ast))
             (wasm-ir (generate-wasm-ir ir)))
        (make-function-body
         :name (if (defun-form-p form)
                   (second form)
                   nil)
         :locals '()
         :code wasm-ir))
    (error (e)
      (report-progress :compile-error
                       :form form
                       :error (format nil "~A" e))
      nil)))

;;; ============================================================
;;; Module Compilation
;;; ============================================================

(defun compile-module (module)
  "Compile a loaded module.
   Returns list of compiled function-body structs."
  (let ((compiled '())
        (succeeded 0)
        (failed 0))
    (dolist (form (loaded-module-forms module))
      (let ((result (compile-expression form)))
        (if result
            (progn
              (push result compiled)
              (incf succeeded))
            (incf failed))))
    (report-progress :module-compiled
                     :path (loaded-module-path module)
                     :succeeded succeeded
                     :failed failed)
    (nreverse compiled)))

;;; ============================================================
;;; Full Compilation
;;; ============================================================

(defun compile-forms (forms)
  "Compile list of forms to function bodies"
  (let ((compiled '()))
    (dolist (form forms)
      (let ((result (compile-expression form)))
        (when result
          (push result compiled))))
    (nreverse compiled)))

(defun compile-all-modules (modules)
  "Compile all loaded modules.
   Returns list of all compiled function bodies."
  (let ((all-compiled '()))
    (dolist (module modules)
      (let ((compiled (compile-module module)))
        (setf all-compiled (nconc all-compiled compiled))))
    all-compiled))

;;; ============================================================
;;; Graceful Degradation
;;; ============================================================

(defun compile-with-degradation (form)
  "Compile form with graceful degradation for unsupported features.
   Returns function-body or placeholder."
  (handler-case
      (compile-expression form)
    (error (e)
      ;; Log the error and return a stub function
      (report-progress :unsupported-form
                       :form (if (consp form) (car form) form)
                       :error (format nil "~A" e))
      ;; Return a stub that returns NIL
      (make-function-body
       :name nil
       :locals '()
       :code '((:global.get 0) (:end))))))  ; Return NIL

(defun compile-module-with-degradation (module)
  "Compile module with graceful degradation"
  (let ((compiled '()))
    (dolist (form (loaded-module-forms module))
      (push (compile-with-degradation form) compiled))
    (nreverse compiled)))
