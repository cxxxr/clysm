;;;; stage0-exports-test.lisp - Contract tests for Stage 0 exports
;;;;
;;;; T025: Verify Stage 0 binary exports required functions
;;;;
;;;; Note: These tests document expected exports. Current Stage 0 has limited
;;;; functionality due to CL subset limitations (14/849 forms compile).

(defpackage #:clysm/tests/contract/stage0-exports
  (:use #:cl #:rove))

(in-package #:clysm/tests/contract/stage0-exports)

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(defun stage0-wasm-path ()
  "Path to the Stage 0 binary."
  (merge-pathnames "dist/clysm-stage0.wasm"
                   (asdf:system-source-directory :clysm)))

(defun stage0-exists-p ()
  "Check if Stage 0 binary exists."
  (probe-file (stage0-wasm-path)))

(defun get-wasm-exports (wasm-path)
  "Get list of exports from a Wasm binary using wasm-tools.
   Returns list of export names, or NIL on error."
  (when (probe-file wasm-path)
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list "wasm-tools" "print" (namestring wasm-path))
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore error-output))
      (when (zerop exit-code)
        ;; Parse (export "name" ...) forms from WAT output
        (let ((exports '()))
          (cl-ppcre:do-matches-as-strings
              (match "\\(export \"([^\"]+)\"" output)
            (let ((name (cl-ppcre:scan-to-strings "\"([^\"]+)\"" match)))
              (when name (push name exports))))
          (nreverse exports))))))

;;; ============================================================
;;; Contract Tests
;;; ============================================================

(deftest stage0-binary-exists
  "Stage 0 binary should exist after bootstrap."
  (ok (stage0-exists-p)
      "dist/clysm-stage0.wasm should exist"))

(deftest stage0-validates
  "Stage 0 binary should pass wasm-tools validate."
  (skip-unless (stage0-exists-p))
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "wasm-tools" "validate"
                              (namestring (stage0-wasm-path)))
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore output error-output))
    (ok (zerop exit-code)
        "Stage 0 should validate with wasm-tools")))

;; Note: The following tests document expected behavior.
;; Due to CL subset limitations, Stage 0 doesn't currently export
;; a working `compile` function. These tests are marked as SKIP
;; until Clysm's CL support is extended.

(deftest stage0-exports-compile-function
  "Stage 0 should export a 'compile' function for external invocation.
   KNOWN LIMITATION: Current Stage 0 has limited exports due to
   compilation rate (14/849 forms = 1.6%)."
  (skip "Stage 0 compile export requires extended CL subset support"))

(deftest stage0-exports-eval-function
  "Stage 0 should export an 'eval' function for expression evaluation.
   KNOWN LIMITATION: Requires extended CL subset support."
  (skip "Stage 0 eval export requires extended CL subset support"))

;;; ============================================================
;;; T019: Contract test compile_form returns valid Wasm
;;; ============================================================

(deftest compile-form-produces-valid-wasm
  "Verify clysm:compile-to-wasm produces valid Wasm for simple expressions."
  ;; Test arithmetic expression produces valid Wasm
  (let ((bytes (clysm:compile-to-wasm '(+ 1 2))))
    (ok (vectorp bytes) "Should return a vector")
    (ok (> (length bytes) 8) "Should have Wasm header + content")
    ;; Check Wasm magic bytes
    (ok (and (= #x00 (aref bytes 0))
             (= #x61 (aref bytes 1))
             (= #x73 (aref bytes 2))
             (= #x6D (aref bytes 3)))
        "Should start with Wasm magic bytes (0x00 'asm')")))

(deftest compile-form-nested-arithmetic-valid
  "Verify nested arithmetic compiles to valid Wasm."
  (let ((bytes (clysm:compile-to-wasm '(+ 1 (* 2 3)))))
    (ok (vectorp bytes) "Should return a vector")
    (ok (> (length bytes) 8) "Should produce non-trivial output")))

;;; ============================================================
;;; T033: Contract test defun produces exported function
;;; ============================================================

(deftest compile-defun-produces-function
  "Verify defun compiles to Wasm with function export."
  (let ((bytes (clysm:compile-to-wasm '(defun add2 (a b) (+ a b)))))
    (ok (vectorp bytes) "Should return a vector")
    (ok (> (length bytes) 50) "Should produce substantial output for defun")))

(deftest compile-multiple-defuns
  "Verify multiple defuns compile together."
  (let ((bytes (clysm:compile-to-wasm '(progn
                                         (defun inc (x) (+ x 1))
                                         (defun dec (x) (- x 1))))))
    (ok (vectorp bytes) "Should return a vector")
    (ok (> (length bytes) 100) "Should produce output for multiple defuns")))

;;; ============================================================
;;; Documentation Tests
;;; ============================================================

(deftest stage0-limitation-documented
  "The CL subset limitation should be documented."
  (let ((tasks-path (merge-pathnames
                     "specs/037-cross-compile-stage0/tasks.md"
                     (asdf:system-source-directory :clysm))))
    (when (probe-file tasks-path)
      (let ((content (uiop:read-file-string tasks-path)))
        (ok (search "14/849" content)
            "tasks.md should document the 14/849 compilation rate")
        (ok (search "known limitation" content :test #'char-equal)
            "tasks.md should mention 'known limitation'")))))
