;;;; io-usage.lisp - I/O usage analysis for conditional import emission
;;;;
;;;; Feature: 022-wasm-import-optimization
;;;; Purpose: Detect if compiled code uses I/O functions, enabling omission of
;;;;          clysm:io imports for modules that don't need them.

(in-package #:clysm/compiler/analyzer/io-usage)

;;; ==========================================================================
;;; T010: I/O Function Names Constant
;;; ==========================================================================

(defparameter *io-function-names*
  '(;; Output functions
    "WRITE-CHAR" "WRITE-STRING" "WRITE-BYTE" "WRITE-LINE"
    "TERPRI" "FRESH-LINE"
    "PRINT" "PRIN1" "PRINC" "PPRINT" "FORMAT" "WRITE"
    ;; Input functions
    "READ-CHAR" "READ-LINE" "READ-BYTE" "PEEK-CHAR" "READ"
    ;; Stream operations that imply I/O
    "FORCE-OUTPUT" "FINISH-OUTPUT" "CLEAR-INPUT"
    "READ-SEQUENCE" "WRITE-SEQUENCE")
  "List of I/O function names (as uppercase strings) that require FFI imports.
These functions ultimately rely on clysm:io FFI imports for execution.")

;;; ==========================================================================
;;; T011-T012: I/O Usage Analysis
;;; ==========================================================================

(defun io-function-name-p (name)
  "Check if NAME (a symbol or string) is an I/O function name."
  (let ((name-str (typecase name
                    (symbol (symbol-name name))
                    (string name)
                    (t nil))))
    (when name-str
      (member name-str *io-function-names* :test #'string-equal))))

(defun analyze-io-usage (form)
  "Analyze FORM to determine if it uses any I/O functions.
FORM is a Lisp S-expression (the source code before compilation).
Returns T if I/O is used, NIL otherwise.

This performs a conservative analysis: if any I/O function call is detected,
returns T. The analysis is recursive and handles nested forms."
  (cond
    ;; Atoms: check if it's an I/O function reference (shouldn't happen at top level)
    ((atom form)
     nil)

    ;; Function calls: check the operator
    ((consp form)
     (let ((operator (car form)))
       (cond
         ;; Direct function call - check if operator is I/O function
         ((and (symbolp operator)
               (io-function-name-p operator))
          t)

         ;; Special forms that don't change the analysis
         ((member operator '(quote function))
          nil)

         ;; Recursive analysis for all other forms
         (t
          (some #'analyze-io-usage form)))))

    (t nil)))
