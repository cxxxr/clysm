;;;; bootstrap-fixpoint-test.lisp - Integration tests for interpreter bootstrap fixpoint
;;;;
;;;; Part of Feature 044: Interpreter Bootstrap Strategy
;;;; Phase 6: T101 - Fixed-point verification integration tests

(defpackage #:clysm/tests/bootstrap-fixpoint-test
  (:use #:cl #:rove))

(in-package #:clysm/tests/bootstrap-fixpoint-test)

;;; ============================================================
;;; Test: Interpreter-Generated Stage 0 Integration
;;; ============================================================

(deftest test-interpreter-stage0-generates-valid-wasm
  "Verify interpreter generates Stage 0 with valid Wasm magic bytes"
  (let ((result (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
                 :module-limit 3 :verbose nil)))
    (ok (clysm/interpreter-bootstrap:bootstrap-result-p result)
        "Returns bootstrap-result struct")
    (ok (clysm/interpreter-bootstrap:bootstrap-result-success result)
        "Generation succeeds")
    (let ((bytes (clysm/interpreter-bootstrap:bootstrap-result-wasm-bytes result)))
      (ok (vectorp bytes) "Returns byte vector")
      (ok (>= (length bytes) 8) "At least 8 bytes")
      ;; Wasm magic: \0asm
      (ok (= #x00 (aref bytes 0)) "Magic byte 0")
      (ok (= #x61 (aref bytes 1)) "Magic byte 1")
      (ok (= #x73 (aref bytes 2)) "Magic byte 2")
      (ok (= #x6d (aref bytes 3)) "Magic byte 3"))))

;;; ============================================================
;;; Test: Bootstrap Chain Progress
;;; ============================================================

(deftest test-bootstrap-chain-progress-tracking
  "Verify progress tracking through bootstrap chain"
  (let ((progress-calls nil))
    (let ((clysm/interpreter-bootstrap:*bootstrap-progress-callback*
            (lambda (phase module-count form-count)
              (push (list phase module-count form-count) progress-calls))))
      (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
       :module-limit 2 :verbose nil)
      (ok (> (length progress-calls) 0) "Progress callback was called")
      ;; Check for expected phases
      (ok (member :start (mapcar #'first progress-calls))
          "Start phase recorded")
      (ok (member :complete (mapcar #'first progress-calls))
          "Complete phase recorded"))))

;;; ============================================================
;;; Test: Form Compilability Predicate
;;; ============================================================

(deftest test-form-compilable-p-for-bootstrap
  "Verify form-compilable-p correctly identifies compilable forms"
  ;; Compilable forms
  (ok (clysm/interpreter-bootstrap:form-compilable-p '(defun foo () 1))
      "defun is compilable")
  (ok (clysm/interpreter-bootstrap:form-compilable-p '(defmacro bar (x) x))
      "defmacro is compilable")
  (ok (clysm/interpreter-bootstrap:form-compilable-p '(+ 1 2))
      "Simple expression is compilable")

  ;; Non-compilable forms (meta-level)
  (ok (not (clysm/interpreter-bootstrap:form-compilable-p '(in-package :cl-user)))
      "in-package not compilable")
  (ok (not (clysm/interpreter-bootstrap:form-compilable-p '(defpackage :foo)))
      "defpackage not compilable")
  (ok (not (clysm/interpreter-bootstrap:form-compilable-p '(eval-when (:compile-toplevel) nil)))
      "eval-when not compilable")
  (ok (not (clysm/interpreter-bootstrap:form-compilable-p '(declare (optimize speed))))
      "declare not compilable"))

;;; ============================================================
;;; Test: Wasm-tools Validation Integration
;;; ============================================================

(deftest test-validate-stage0-binary-integration
  "Verify wasm-tools validation works with interpreter-generated binary"
  (let* ((result (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
                  :module-limit 3 :verbose nil))
         (bytes (clysm/interpreter-bootstrap:bootstrap-result-wasm-bytes result)))
    (when bytes
      (multiple-value-bind (valid-p err)
          (clysm/interpreter-bootstrap:validate-stage0-binary bytes)
        (ok valid-p (format nil "Wasm-tools validates binary~@[ (error: ~A)~]" err))))))

;;; ============================================================
;;; Test: Fixpoint Infrastructure Integration
;;; ============================================================

(deftest test-fixpoint-status-types
  "Verify fixpoint status type and exit code mappings"
  ;; Status type validation
  (ok (typep :achieved 'clysm/stage1:fixpoint-status)
      ":achieved is valid status")
  (ok (typep :not-achieved 'clysm/stage1:fixpoint-status)
      ":not-achieved is valid status")
  (ok (typep :compilation-error 'clysm/stage1:fixpoint-status)
      ":compilation-error is valid status")
  (ok (typep :missing-dependency 'clysm/stage1:fixpoint-status)
      ":missing-dependency is valid status")

  ;; Exit code mappings
  (ok (= 0 (clysm/stage1:status-to-exit-code :achieved))
      "achieved -> 0")
  (ok (= 1 (clysm/stage1:status-to-exit-code :not-achieved))
      "not-achieved -> 1")
  (ok (= 2 (clysm/stage1:status-to-exit-code :compilation-error))
      "compilation-error -> 2")
  (ok (= 3 (clysm/stage1:status-to-exit-code :missing-dependency))
      "missing-dependency -> 3"))

;;; ============================================================
;;; Test: Timestamp and Git Utilities
;;; ============================================================

(deftest test-timestamp-utilities
  "Verify timestamp utilities work correctly"
  (let ((timestamp (clysm/stage1:current-iso-timestamp)))
    (ok (stringp timestamp) "Returns string")
    (ok (> (length timestamp) 10) "Has reasonable length")
    (ok (search "T" timestamp) "Contains time separator")))

;;; ============================================================
;;; Test: Full Bootstrap Chain Concept
;;; ============================================================

(deftest test-bootstrap-result-struct-complete
  "Verify bootstrap-result struct has all required fields"
  (let ((result (clysm/interpreter-bootstrap:make-bootstrap-result
                 :success t
                 :wasm-bytes (make-array 8 :element-type '(unsigned-byte 8)
                                           :initial-contents '(0 97 115 109 1 0 0 0))
                 :modules-loaded 5
                 :forms-compiled 10
                 :errors nil
                 :elapsed-time 1.5)))
    (ok (clysm/interpreter-bootstrap:bootstrap-result-p result)
        "Is bootstrap-result")
    (ok (clysm/interpreter-bootstrap:bootstrap-result-success result)
        "Has success field")
    (ok (vectorp (clysm/interpreter-bootstrap:bootstrap-result-wasm-bytes result))
        "Has wasm-bytes field")
    (ok (= 5 (clysm/interpreter-bootstrap:bootstrap-result-modules-loaded result))
        "Has modules-loaded field")
    (ok (= 10 (clysm/interpreter-bootstrap:bootstrap-result-forms-compiled result))
        "Has forms-compiled field")
    (ok (null (clysm/interpreter-bootstrap:bootstrap-result-errors result))
        "Has errors field")
    (ok (= 1.5 (clysm/interpreter-bootstrap:bootstrap-result-elapsed-time result))
        "Has elapsed-time field")))

;;; ============================================================
;;; Test: Error Handling in Bootstrap
;;; ============================================================

(deftest test-bootstrap-error-handling
  "Verify bootstrap handles errors gracefully"
  (let ((result (clysm/interpreter-bootstrap:generate-stage0-via-interpreter
                 :module-limit 5 :verbose nil)))
    ;; Even with compilation errors, should return valid result struct
    (ok (clysm/interpreter-bootstrap:bootstrap-result-p result)
        "Returns valid struct even with errors")
    (ok (listp (clysm/interpreter-bootstrap:bootstrap-result-errors result))
        "Errors is a list")
    ;; Result should still have stats
    (ok (>= (clysm/interpreter-bootstrap:bootstrap-result-modules-loaded result) 0)
        "Has module count")
    (ok (>= (clysm/interpreter-bootstrap:bootstrap-result-forms-compiled result) 0)
        "Has form count")))
