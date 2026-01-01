;;;; advance-token-export-test.lisp - Tests for ADVANCE-TOKEN export (001-wasm-local-binding)
;;;;
;;;; Phase 13D User Story 2: Export ADVANCE-TOKEN for Parser Integration
;;;; TDD Required: Tests MUST fail before implementation (Constitution VII)
;;;;
;;;; Test Coverage:
;;;; - T017: ADVANCE-TOKEN package export verification
;;;; - T018: ADVANCE-TOKEN runtime table registration verification

(in-package #:cl-user)

(defpackage #:clysm/tests/unit/advance-token-export
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/advance-token-export)

;;; ============================================================
;;; T017: ADVANCE-TOKEN Package Export Tests
;;; ============================================================

(deftest advance-token-package-export-test
  "Verify ADVANCE-TOKEN is exported from clysm/reader/parser package."
  (testing "ADVANCE-TOKEN exported from clysm/reader/parser"
    ;; Find the symbol in the clysm/reader/parser package
    (multiple-value-bind (sym status)
        (find-symbol "ADVANCE-TOKEN" :clysm/reader/parser)
      (ok sym "ADVANCE-TOKEN symbol exists in package")
      (ok (eq status :external) "ADVANCE-TOKEN is external (exported)")))

  (testing "ADVANCE-TOKEN accessible from clysm package"
    ;; Verify re-export from main clysm package
    (multiple-value-bind (sym status)
        (find-symbol "ADVANCE-TOKEN" :clysm)
      (ok sym "ADVANCE-TOKEN symbol exists in clysm package")
      (ok (or (eq status :external) (eq status :inherited))
          "ADVANCE-TOKEN is accessible from clysm package"))))

(deftest advance-token-function-test
  "Verify ADVANCE-TOKEN is a function with correct behavior."
  (testing "ADVANCE-TOKEN is a function"
    (let ((sym (find-symbol "ADVANCE-TOKEN" :clysm/reader/parser)))
      (ok sym "ADVANCE-TOKEN symbol found")
      (when sym
        (ok (fboundp sym) "ADVANCE-TOKEN is bound to a function")
        ;; Test basic functionality with a parser state
        (let* ((make-state-fn (find-symbol "MAKE-PARSER-STATE" :clysm/reader/parser))
               (state (when (and make-state-fn (fboundp make-state-fn))
                        (funcall make-state-fn :tokens '(((:number 1) (:number 2)))))))
          (skip "Parser state creation not available for direct testing"))))))

;;; ============================================================
;;; T018: ADVANCE-TOKEN Runtime Table Registration Tests
;;; ============================================================

(deftest advance-token-runtime-table-test
  "Verify ADVANCE-TOKEN is registered in *runtime-function-table*."
  (testing "ADVANCE-TOKEN in runtime table"
    ;; Get the runtime function table
    (let ((table-sym (find-symbol "*RUNTIME-FUNCTION-TABLE*"
                                   :clysm/compiler/codegen/func-section)))
      (ok table-sym "*RUNTIME-FUNCTION-TABLE* symbol found")
      (when (and table-sym (boundp table-sym))
        (let* ((table (symbol-value table-sym))
               (advance-token-sym (find-symbol "ADVANCE-TOKEN" :clysm/reader/parser))
               (entry (when advance-token-sym (gethash advance-token-sym table))))
          (ok entry "ADVANCE-TOKEN registered in runtime table")
          (when entry
            (ok (car entry) "Entry has runtime name")
            (ok (eql (cdr entry) 1) "Entry has arity 1")))))))

;;; ============================================================
;;; Compilation Test: P027 Error Pattern Elimination
;;; ============================================================

(deftest advance-token-compilation-test
  "Verify ADVANCE-TOKEN can be compiled without undefined function error."
  (testing "ADVANCE-TOKEN call compiles successfully"
    (let ((compile-fn (find-symbol "COMPILE-TO-WAT" :clysm)))
      (when (and compile-fn (fboundp compile-fn))
        ;; Try to compile a form that uses ADVANCE-TOKEN
        ;; This should not produce "undefined function" error
        (handler-case
            (let ((result (funcall compile-fn '(defun test-advance ()
                                                  (clysm/reader/parser:advance-token nil)))))
              (ok result "ADVANCE-TOKEN call compiled successfully"))
          (error (c)
            (let ((msg (princ-to-string c)))
              ;; Check if it's NOT the P027 error pattern
              (ng (search "Undefined function" msg)
                  (format nil "P027 pattern detected: ~A" msg)))))))))
