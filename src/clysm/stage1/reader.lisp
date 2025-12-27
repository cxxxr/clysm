;;;; reader.lisp - Source file reading and parsing for Stage 1
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Reads and parses Lisp source files into source-form structs

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Compilable Form Predicate
;;; ==========================================================================

(defparameter *non-compilable-operators*
  '(in-package declare eval-when)
  "Operators that represent non-compilable forms (metadata, not code).")

(defparameter *special-metadata-ops*
  '(provide require)
  "Operators for module system metadata.")

(defun compilable-form-p (sexp)
  "Return T if the form is compilable by Stage 0.
Forms like IN-PACKAGE, DECLARE, and EVAL-WHEN are metadata, not code.
Top-level atoms (like docstrings) are also not compilable."
  (and (consp sexp)
       (let ((op (car sexp)))
         (and (symbolp op)
              (not (member op *non-compilable-operators*))
              (not (member op *special-metadata-ops*))))))

(defun defining-form-p (sexp)
  "Return T if the form is a defining form (defun, defmacro, etc.)."
  (and (consp sexp)
       (member (car sexp) '(defun defmacro defmethod defgeneric
                            defvar defparameter defconstant
                            defstruct defclass define-condition
                            deftype))))

;;; ==========================================================================
;;; Source Form Reading
;;; ==========================================================================

(defun read-source-forms (stream &key (module nil) (module-index 0))
  "Read all top-level forms from STREAM and return list of source-form structs.
Handles read errors gracefully by recording parse errors."
  (let ((forms nil)
        (form-index 0)
        (*package* (find-package :cl-user))) ; Start in CL-USER
    (handler-case
        (loop
          (handler-case
              (let* ((start-pos (file-position stream))
                     (sexp (read stream nil :eof)))
                (when (eq sexp :eof)
                  (return))
                ;; Track IN-PACKAGE to update current package
                (when (and (consp sexp)
                           (eq (car sexp) 'in-package))
                  (let ((pkg-name (cadr sexp)))
                    (when (and pkg-name (find-package pkg-name))
                      (setf *package* (find-package pkg-name)))))
                ;; Create source-form
                (let* ((end-pos (file-position stream))
                       (source-text (if (and start-pos end-pos)
                                        (read-source-range stream start-pos end-pos)
                                        (format nil "~S" sexp)))
                       (operator (form-operator sexp))
                       (name (form-name sexp)))
                  (push (make-source-form
                         :id (make-form-id module-index form-index)
                         :sexp sexp
                         :operator operator
                         :name name
                         :source-text source-text
                         :module module
                         :index form-index
                         :compilable-p (compilable-form-p sexp))
                        forms)
                  (incf form-index)))
            ;; Handle individual form read errors
            (error (e)
              (push (make-source-form
                     :id (make-form-id module-index form-index)
                     :sexp nil
                     :operator :read-error
                     :source-text (format nil "Read error: ~A" e)
                     :module module
                     :index form-index
                     :compilable-p nil)
                    forms)
              (incf form-index))))
      ;; Handle stream-level errors
      (error (e)
        (push (make-source-form
               :id (make-form-id module-index form-index)
               :sexp nil
               :operator :stream-error
               :source-text (format nil "Stream error: ~A" e)
               :module module
               :index form-index
               :compilable-p nil)
              forms)))
    (nreverse forms)))

(defun read-source-range (stream start end)
  "Read source text from STREAM between START and END positions.
Returns empty string if unable to seek."
  (declare (ignore stream start end))
  ;; Note: File position manipulation is not portable across all streams.
  ;; For now, return empty string. Could be enhanced with file re-reading.
  "")

(defun read-forms-from-file (path &key (module-index 0))
  "Read all forms from file at PATH.
Signals stage1-file-not-found if file doesn't exist."
  (unless (probe-file path)
    (error 'stage1-file-not-found :path path))
  (with-open-file (stream path :direction :input
                               :external-format :utf-8
                               :if-does-not-exist :error)
    (let ((module (make-source-module
                   :path (namestring (truename path))
                   :relative-path (enough-namestring path)
                   :module-index module-index
                   :status :reading)))
      (let ((forms (read-source-forms stream
                                      :module module
                                      :module-index module-index)))
        (setf (source-module-forms module) forms
              (source-module-status module) :parsed)
        module))))

;;; ==========================================================================
;;; Module Path Management
;;; ==========================================================================

(defvar *compiler-source-root* nil
  "Root directory for compiler source files. Set during initialization.")

(defparameter *module-paths*
  '(;; Backend modules
    "src/clysm/backend/leb128.lisp"
    "src/clysm/backend/sections.lisp"
    "src/clysm/backend/wasm-emit.lisp"
    "src/clysm/backend/wat-print.lisp"
    ;; Reader modules
    "src/clysm/reader/tokenizer.lisp"
    "src/clysm/reader/parser.lisp"
    "src/clysm/reader/package.lisp"
    "src/clysm/reader/reader.lisp"
    ;; Compiler core modules
    "src/clysm/compiler/ast.lisp"
    "src/clysm/compiler/env.lisp"
    "src/clysm/compiler/analyzer/free-vars.lisp"
    "src/clysm/compiler/analyzer/tail-call.lisp"
    "src/clysm/compiler/analyzer/type-infer.lisp"
    "src/clysm/compiler/analyzer/io-usage.lisp"
    "src/clysm/compiler/transform/closure.lisp"
    "src/clysm/compiler/transform/macro.lisp"
    "src/clysm/compiler/codegen/wasm-ir.lisp"
    "src/clysm/compiler/codegen/gc-types.lisp"
    "src/clysm/compiler/codegen/type-section.lisp"
    "src/clysm/compiler/codegen/func-section.lisp"
    "src/clysm/compiler/compiler.lisp"
    ;; Runtime modules
    "src/clysm/runtime/objects.lisp"
    "src/clysm/runtime/special-vars.lisp"
    "src/clysm/runtime/multi-value.lisp"
    "src/clysm/runtime/printer.lisp"
    "src/clysm/runtime/condition-runtime.lisp"
    ;; CLOS modules
    "src/clysm/clos/mop.lisp"
    "src/clysm/clos/defclass.lisp"
    "src/clysm/clos/instance.lisp"
    "src/clysm/clos/slot-access.lisp"
    "src/clysm/clos/generic.lisp"
    "src/clysm/clos/defmethod.lisp"
    "src/clysm/clos/combination.lisp"
    "src/clysm/clos/dispatch.lisp"
    "src/clysm/clos/method-combination.lisp"
    ;; Condition system
    "src/clysm/conditions/package.lisp"
    "src/clysm/conditions/types.lisp"
    "src/clysm/conditions/handlers.lisp"
    "src/clysm/conditions/restarts.lisp"
    "src/clysm/conditions/signaling.lisp"
    "src/clysm/conditions/standard.lisp"
    ;; Library
    "src/clysm/lib/utf8.lisp"
    "src/clysm/lib/setf-expanders.lisp"
    "src/clysm/lib/destructuring.lisp"
    "src/clysm/lib/macros.lisp")
  "List of compiler source module paths in dependency order.")

(defun get-module-paths (&key (root *compiler-source-root*))
  "Return list of absolute paths to all compiler source modules in dependency order."
  (mapcar (lambda (rel-path)
            (if root
                (merge-pathnames rel-path root)
                rel-path))
          *module-paths*))

(defun module-dependency-group (path)
  "Determine the dependency group for a module based on its path."
  (cond
    ((search "backend" path) "backend")
    ((search "reader" path) "reader")
    ((search "compiler" path) "compiler")
    ((search "runtime" path) "runtime")
    ((search "clos" path) "clos")
    ((search "conditions" path) "conditions")
    ((search "lib" path) "library")
    (t "other")))

;;; ==========================================================================
;;; Batch Module Reading
;;; ==========================================================================

(defun read-all-modules (&key (root *compiler-source-root*)
                              (progress-callback nil))
  "Read all compiler source modules.
Returns list of source-module structs in dependency order.
PROGRESS-CALLBACK, if provided, is called with (module-index total path) for each module."
  (let ((paths (get-module-paths :root root))
        (modules nil))
    (loop for path in paths
          for index from 1
          do (when progress-callback
               (funcall progress-callback index (length paths) path))
             (handler-case
                 (let ((module (read-forms-from-file path :module-index index)))
                   (setf (source-module-dependency-group module)
                         (module-dependency-group (namestring path)))
                   (push module modules))
               (stage1-file-not-found (e)
                 ;; Create a placeholder module for missing files
                 (push (make-source-module
                        :path (namestring path)
                        :relative-path (enough-namestring path)
                        :module-index index
                        :dependency-group (module-dependency-group (namestring path))
                        :status :error)
                       modules)
                 (warn "Module not found: ~A" (stage1-file-error-path e)))))
    (nreverse modules)))

(defun count-compilable-forms (modules)
  "Count total compilable forms across all modules."
  (reduce #'+
          (mapcar (lambda (module)
                    (count-if #'source-form-compilable-p
                              (source-module-forms module)))
                  modules)))

(defun all-compilable-forms (modules)
  "Collect all compilable forms from all modules in order."
  (loop for module in modules
        nconc (remove-if-not #'source-form-compilable-p
                             (source-module-forms module))))
