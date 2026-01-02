;;;; tests/integration/quasiquote-runtime-test.lisp
;;;; Integration tests for quasiquote runtime correctness
;;;; Feature: 001-quasiquote-local-vars

(defpackage #:clysm/tests/integration/quasiquote-runtime-test
  (:use #:cl #:rove)
  (:import-from #:clysm
                #:compile-to-wasm))

(in-package #:clysm/tests/integration/quasiquote-runtime-test)

;;; ==========================================================================
;;; Helper Functions
;;; ==========================================================================

(defun compile-and-validate (form)
  "Compile a form and validate the Wasm output with wasm-tools."
  (let ((wasm-bytes (compile-to-wasm form)))
    (when wasm-bytes
      ;; Write to temp file and validate
      (let ((temp-file "/tmp/quasiquote-test.wasm"))
        (with-open-file (out temp-file
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
          (write-sequence wasm-bytes out))
        ;; Return t if validation passes
        (zerop (nth-value 2 (uiop:run-program
                             (list "wasm-tools" "validate" temp-file)
                             :ignore-error-status t)))))))

;;; ==========================================================================
;;; Phase 3: User Story 1 - Integration Tests
;;; ==========================================================================

;; T019: Runtime simple unquote
;; Integration: Compile function with quasiquote, validate Wasm output
(deftest test-runtime-simple-unquote
  ;; Test 1: Single variable unquote
  (ok (compile-and-validate '(defun test-qq-single (x) `(result ,x)))
      "Compile and validate quasiquote with single unquoted variable")

  ;; Test 2: Multiple variable unquote
  (ok (compile-and-validate '(defun test-qq-multi (a b c) `(,a ,b ,c)))
      "Compile and validate quasiquote with multiple unquoted variables")

  ;; Test 3: Nested let with unquote
  (ok (compile-and-validate '(defun test-qq-nested ()
                               (let ((outer 1))
                                 (let ((inner 2))
                                   `(,outer ,inner)))))
      "Compile and validate quasiquote with nested let scopes")

  ;; Test 4: Mixed quoted and unquoted
  (ok (compile-and-validate '(defun test-qq-mixed (x)
                               `(if ,x then else)))
      "Compile and validate quasiquote with mixed quoted and unquoted"))

;;; ==========================================================================
;;; Phase 4: User Story 2 - Integration Tests
;;; ==========================================================================

;; T028: Runtime unquote-splicing
(deftest test-runtime-unquote-splicing
  ;; Integration: Verify runtime list splicing produces valid Wasm
  (ok (compile-and-validate '(defun test-splice-basic (args) `(call ,@args)))
      "Compile and validate basic unquote-splicing")

  (ok (compile-and-validate '(defun test-splice-multi (front back)
                               `(,@front middle ,@back)))
      "Compile and validate multiple unquote-splicing")

  (ok (compile-and-validate '(defun test-splice-prefix (items)
                               `(prefix ,@items suffix)))
      "Compile and validate unquote-splicing with surrounding elements"))

;;; ==========================================================================
;;; Phase 5: User Story 4 - Integration Tests
;;; ==========================================================================

;; T035: Runtime mixed elements
(deftest test-runtime-mixed-elements
  ;; Integration: Verify runtime produces valid Wasm for mixed elements
  (ok (compile-and-validate '(defun test-mixed-if (cond then else)
                               `(if ,cond ,then ,else)))
      "Compile and validate mixed symbol and variable quasiquote")

  (ok (compile-and-validate '(defun test-mixed-nested (val)
                               `((static sub) ,val)))
      "Compile and validate nested quoted list with dynamic element"))

;;; ==========================================================================
;;; Phase 6: User Story 3 - Integration Tests
;;; ==========================================================================

;; T043: Runtime nested quasiquote
(deftest test-runtime-nested-quasiquote
  ;; Integration: Verify runtime produces valid Wasm for nested structures
  (ok (compile-and-validate '(defun test-nested-struct ()
                               `(outer (inner value))))
      "Compile and validate nested quasiquote structure")

  (ok (compile-and-validate '(defun test-expr-in-qq (x)
                               `(result ,(+ x 1))))
      "Compile and validate expression unquote in quasiquote"))
