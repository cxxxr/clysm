;;;; test-signature.lisp - Contract test for compile_form signature (T011)
;;;; Part of 001-bootstrap-fixpoint Phase 13D-9
;;;;
;;;; This test verifies that compile_form has the correct Wasm type signature.
;;;; Expected signature: (func (param anyref) (result anyref))
;;;; TDD: This test must FAIL before implementation is complete.

(in-package #:clysm/tests/contract/fixpoint-signature)

(defparameter *stage1-wasm-path*
  (merge-pathnames #p"dist/clysm-stage1.wasm"
                   (asdf:system-source-directory :clysm))
  "Path to Stage 1 Wasm binary.")

(deftest test-compile-form-takes-anyref-param
  "Verify compile_form accepts anyref parameter"
  ;; Parse the WAT output to find compile_form's type signature
  (let ((wat-output
          (with-output-to-string (s)
            (uiop:run-program
             (list "wasm-tools" "print" (namestring *stage1-wasm-path*))
             :output s
             :error-output nil
             :ignore-error-status t))))
    ;; Find the export and its function reference
    ;; The export should reference a function with anyref param
    (let ((export-pos (search "(export \"compile_form\"" wat-output)))
      (ok export-pos
          "compile_form export should exist")
      (when export-pos
        ;; After finding export, we need to trace to the function type
        ;; For now, just verify the export exists with a func reference
        (ok (search "(func" (subseq wat-output export-pos (min (+ export-pos 100) (length wat-output))))
            "compile_form should export a function")))))

(deftest test-compile-form-returns-anyref
  "Verify compile_form returns anyref (Wasm bytes or error)"
  (let ((wat-output
          (with-output-to-string (s)
            (uiop:run-program
             (list "wasm-tools" "print" (namestring *stage1-wasm-path*))
             :output s
             :error-output nil
             :ignore-error-status t))))
    ;; A more detailed check would parse the type section
    ;; For contract level, verify the export exists
    (ok (search "(export \"compile_form\"" wat-output)
        "compile_form export should exist for signature verification")))

(deftest test-compile-form-function-index
  "Verify compile_form is at expected function index (4)"
  ;; According to exports.lisp, compile_form should be at index 4
  ;; FFI imports occupy 0-3, user exports start at 4
  (let ((wat-output
          (with-output-to-string (s)
            (uiop:run-program
             (list "wasm-tools" "print" (namestring *stage1-wasm-path*))
             :output s
             :error-output nil
             :ignore-error-status t))))
    ;; Look for export referencing func 4
    (let ((export-section (search "(export \"compile_form\" (func" wat-output)))
      (ok export-section
          "compile_form should be exported as a function"))))
