;;;; tests/e2e/helpers.lisp - E2E Test Helper Functions
;;;;
;;;; Provides utilities for end-to-end testing that compiles Lisp code to Wasm,
;;;; executes it via wasmtime, and verifies the actual result strings.

(in-package #:clysm/tests)

;;; ============================================================
;;; Core E2E Evaluation Functions
;;; ============================================================

(defun eval-e2e (expr-string)
  "Compile and execute EXPR-STRING via Wasm, returning the printed result string.
This function:
1. Parses EXPR-STRING using the Clysm reader
2. Compiles the form to WebAssembly
3. Executes via wasmtime
4. Returns the prin1 representation of the result"
  (let* ((form (clysm:clysm-read-from-string expr-string))
         (wasm (clysm:compile-to-wasm (list form))))
    (clysm:run-wasm-with-print wasm)))

(defun eval-e2e-forms (forms-string)
  "Compile and execute multiple forms from FORMS-STRING via Wasm.
Returns the printed result of the last form.
Use this for tests that require definitions (defun, defvar, defmacro)
before testing expressions."
  (let* ((forms (clysm:clysm-read-all-from-string forms-string))
         (wasm (clysm:compile-to-wasm forms)))
    (clysm:run-wasm-with-print wasm)))

;;; ============================================================
;;; E2E Assertion Macros
;;; ============================================================

(defmacro is-e2e (expr-string expected-result &optional description)
  "Assert that EXPR-STRING evaluates to EXPECTED-RESULT via Wasm.
EXPECTED-RESULT should be the expected prin1 output string."
  (let ((desc (or description
                  `(format nil "~A => ~A" ,expr-string ,expected-result))))
    `(is-equal ,expected-result (eval-e2e ,expr-string) ,desc)))

(defmacro is-e2e-forms (forms-string expected-result &optional description)
  "Assert that FORMS-STRING (multiple forms) evaluates to EXPECTED-RESULT.
The result of the last form is compared to EXPECTED-RESULT."
  (let ((desc (or description
                  `(format nil "forms => ~A" ,expected-result))))
    `(is-equal ,expected-result (eval-e2e-forms ,forms-string) ,desc)))

(defmacro e2e-finishes (expr-string &optional description)
  "Assert that EXPR-STRING compiles and executes without error.
Does not check the result value."
  `(finishes (eval-e2e ,expr-string)
             ,(or description expr-string)))

(defmacro e2e-forms-finishes (forms-string &optional description)
  "Assert that FORMS-STRING compiles and executes without error."
  `(finishes (eval-e2e-forms ,forms-string)
             ,(or description "forms should complete")))

;;; ============================================================
;;; Error Testing Helpers
;;; ============================================================

(defmacro e2e-signals-compile-error (expr-string &optional description)
  "Assert that EXPR-STRING fails to compile (signals an error during compilation)."
  `(signals error
     (let ((form (clysm:clysm-read-from-string ,expr-string)))
       (clysm:compile-to-wasm (list form)))
     ,(or description (format nil "~A should signal compile error" expr-string))))

(defmacro e2e-signals-read-error (expr-string &optional description)
  "Assert that EXPR-STRING fails to parse (signals an error during reading)."
  `(signals error
     (clysm:clysm-read-from-string ,expr-string)
     ,(or description (format nil "~A should signal read error" expr-string))))

(defun eval-e2e-may-trap (expr-string)
  "Evaluate EXPR-STRING via Wasm, returning :trap if a runtime error occurs.
Use this for testing runtime errors like division by zero."
  (handler-case
      (eval-e2e expr-string)
    (error (e)
      (let ((msg (format nil "~A" e)))
        (if (or (search "trap" msg)
                (search "unreachable" msg)
                (search "error" msg :test #'char-equal))
            :trap
            (error e))))))

;;; ============================================================
;;; Test Result Comparison Utilities
;;; ============================================================

(defun e2e-result-truthy-p (result)
  "Check if RESULT represents a truthy value (not NIL).
In Clysm, NIL prints as \"NIL\"."
  (and (stringp result)
       (not (string= result "NIL"))))

(defun e2e-result-nil-p (result)
  "Check if RESULT represents NIL."
  (and (stringp result)
       (string= result "NIL")))

(defmacro is-e2e-true (expr-string &optional description)
  "Assert that EXPR-STRING evaluates to a truthy value (not NIL)."
  `(is (e2e-result-truthy-p (eval-e2e ,expr-string))
       ,(or description (format nil "~A should be truthy" expr-string))))

(defmacro is-e2e-nil (expr-string &optional description)
  "Assert that EXPR-STRING evaluates to NIL."
  `(is (e2e-result-nil-p (eval-e2e ,expr-string))
       ,(or description (format nil "~A should be NIL" expr-string))))
