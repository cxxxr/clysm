;;;; backend/wat-print.lisp - WAT (WebAssembly Text) Output
;;;;
;;;; Generates human-readable WAT format for debugging and inspection.

(in-package #:clysm)

;;; ============================================================
;;; WAT Printer State
;;; ============================================================

(defvar *wat-indent* 0
  "Current indentation level.")

(defvar *wat-stream* *standard-output*
  "Output stream for WAT.")

(defun wat-indent ()
  "Print current indentation."
  (dotimes (i (* 2 *wat-indent*))
    (write-char #\Space *wat-stream*)))

(defun wat-line (fmt &rest args)
  "Print an indented line."
  (wat-indent)
  (apply #'format *wat-stream* fmt args)
  (terpri *wat-stream*))

(defmacro with-wat-indent (&body body)
  "Execute BODY with increased indentation."
  `(let ((*wat-indent* (1+ *wat-indent*)))
     ,@body))

;;; ============================================================
;;; Type Formatting
;;; ============================================================

(defun format-valtype (valtype)
  "Format a value type as WAT string."
  (if (and (consp valtype) (eq (car valtype) :ref))
      (let* ((rest (cdr valtype))
             (nullable (eq (car rest) :null))
             (typeidx (if nullable (cadr rest) (car rest))))
        (if nullable
            (format nil "(ref null ~A)" typeidx)
            (format nil "(ref ~A)" typeidx)))
      (string-downcase (symbol-name valtype))))

(defun format-functype (functype)
  "Format a function type as WAT string."
  (with-output-to-string (s)
    (write-string "(func" s)
    (when (wasm-functype-params functype)
      (dolist (param (wasm-functype-params functype))
        (format s " (param ~A)" (format-valtype param))))
    (when (wasm-functype-results functype)
      (dolist (result (wasm-functype-results functype))
        (format s " (result ~A)" (format-valtype result))))
    (write-string ")" s)))

(defun format-field (field)
  "Format a struct field as WAT string."
  (format nil "(field ~A~A)"
          (if (wasm-field-mutable field) "(mut " "")
          (if (wasm-field-mutable field)
              (format nil "~A)" (format-valtype (wasm-field-type field)))
              (format-valtype (wasm-field-type field)))))

(defun format-structtype (structtype)
  "Format a struct type as WAT string."
  (with-output-to-string (s)
    (write-string "(struct" s)
    (dolist (field (wasm-structtype-fields structtype))
      (format s " ~A" (format-field field)))
    (write-string ")" s)))

(defun format-arraytype (arraytype)
  "Format an array type as WAT string."
  (format nil "(array ~A~A)"
          (if (wasm-arraytype-mutable arraytype) "(mut " "")
          (if (wasm-arraytype-mutable arraytype)
              (format nil "~A)" (format-valtype (wasm-arraytype-element arraytype)))
              (format-valtype (wasm-arraytype-element arraytype)))))

;;; ============================================================
;;; Module Printing
;;; ============================================================

(defun print-wat-type (typedef index)
  "Print a type definition."
  (let* ((def (wasm-type-definition typedef))
         (name (wasm-type-name typedef))
         (type-str (etypecase def
                     (wasm-functype (format-functype def))
                     (wasm-structtype (format-structtype def))
                     (wasm-arraytype (format-arraytype def)))))
    (if name
        (wat-line "(type $~A ~A)  ;; ~D" name type-str index)
        (wat-line "(type ~A)  ;; ~D" type-str index))))

(defun print-wat-import (import index)
  "Print an import."
  (wat-line "(import ~S ~S (~A ~A))  ;; ~D"
            (wasm-import-module import)
            (wasm-import-name import)
            (string-downcase (symbol-name (wasm-import-kind import)))
            (ecase (wasm-import-kind import)
              (:func (format nil "(type ~D)" (wasm-import-desc import)))
              (:memory (format nil "~D" (wasm-limits-min (wasm-import-desc import)))))
            index))

(defun print-wat-func (func)
  "Print a function definition."
  (let ((name (wasm-func-name func))
        (type-idx (wasm-func-type-index func)))
    (wat-indent)
    (if name
        (format *wat-stream* "(func $~A (type ~D)" name type-idx)
        (format *wat-stream* "(func (type ~D)" type-idx))

    ;; Locals
    (when (wasm-func-locals func)
      (terpri *wat-stream*)
      (with-wat-indent
        (dolist (local (wasm-func-locals func))
          (dotimes (i (car local))
            (wat-line "(local ~A)" (format-valtype (cdr local)))))))

    ;; Body (simplified: just show byte count)
    (let ((body (wasm-func-body func)))
      (if body
          (progn
            (terpri *wat-stream*)
            (with-wat-indent
              (wat-line ";; ~D bytes of code" (length body))))
          (terpri *wat-stream*)))

    (wat-line ")")))

(defun print-wat-memory (memory index)
  "Print a memory definition."
  (let ((limits (wasm-memory-limits memory)))
    (if (wasm-limits-max limits)
        (wat-line "(memory ~D ~D)  ;; ~D"
                  (wasm-limits-min limits)
                  (wasm-limits-max limits)
                  index)
        (wat-line "(memory ~D)  ;; ~D"
                  (wasm-limits-min limits)
                  index))))

(defun print-wat-global (global index)
  "Print a global definition."
  (let ((name (wasm-global-name global)))
    (if name
        (wat-line "(global $~A ~A~A (...))  ;; ~D"
                  name
                  (if (wasm-global-mutable global) "(mut " "")
                  (if (wasm-global-mutable global)
                      (format nil "~A)" (format-valtype (wasm-global-type global)))
                      (format-valtype (wasm-global-type global)))
                  index)
        (wat-line "(global ~A~A (...))  ;; ~D"
                  (if (wasm-global-mutable global) "(mut " "")
                  (if (wasm-global-mutable global)
                      (format nil "~A)" (format-valtype (wasm-global-type global)))
                      (format-valtype (wasm-global-type global)))
                  index))))

(defun print-wat-export (export)
  "Print an export."
  (wat-line "(export ~S (~A ~D))"
            (wasm-export-name export)
            (string-downcase (symbol-name (wasm-export-kind export)))
            (wasm-export-index export)))

;;; ============================================================
;;; Main Entry Points
;;; ============================================================

(defun print-wat (module &optional (stream *standard-output*))
  "Print MODULE in WAT format to STREAM."
  (let ((*wat-stream* stream)
        (*wat-indent* 0))
    (module-finalize module)

    (wat-line "(module")
    (with-wat-indent
      ;; Types
      (when (wasm-module-types module)
        (wat-line ";; Types")
        (loop for typedef in (wasm-module-types module)
              for i from 0
              do (print-wat-type typedef i))
        (terpri *wat-stream*))

      ;; Imports
      (when (wasm-module-imports module)
        (wat-line ";; Imports")
        (loop for import in (wasm-module-imports module)
              for i from 0
              do (print-wat-import import i))
        (terpri *wat-stream*))

      ;; Memories
      (when (wasm-module-memories module)
        (wat-line ";; Memories")
        (loop for memory in (wasm-module-memories module)
              for i from 0
              do (print-wat-memory memory i))
        (terpri *wat-stream*))

      ;; Globals
      (when (wasm-module-globals module)
        (wat-line ";; Globals")
        (loop for global in (wasm-module-globals module)
              for i from 0
              do (print-wat-global global i))
        (terpri *wat-stream*))

      ;; Functions
      (when (wasm-module-funcs module)
        (wat-line ";; Functions")
        (dolist (func (wasm-module-funcs module))
          (print-wat-func func))
        (terpri *wat-stream*))

      ;; Exports
      (when (wasm-module-exports module)
        (wat-line ";; Exports")
        (dolist (export (wasm-module-exports module))
          (print-wat-export export))))

    (wat-line ")")))

(defun wat-to-string (module)
  "Return MODULE as a WAT string."
  (with-output-to-string (s)
    (print-wat module s)))
