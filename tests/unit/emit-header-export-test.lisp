;;;; emit-header-export-test.lisp - Tests for EMIT-MODULE-HEADER export (001-wasm-local-binding)
;;;;
;;;; Phase 13D User Story 3: Export EMIT-MODULE-HEADER for Wasm Generation
;;;; TDD Required: Tests MUST fail before implementation (Constitution VII)
;;;;
;;;; Test Coverage:
;;;; - T024: EMIT-MODULE-HEADER package export verification
;;;; - T025: EMIT-MODULE-HEADER runtime table registration verification

(in-package #:cl-user)

(defpackage #:clysm/tests/unit/emit-header-export
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/emit-header-export)

;;; ============================================================
;;; T024: EMIT-MODULE-HEADER Package Export Tests
;;; ============================================================

(deftest emit-module-header-package-export-test
  "Verify EMIT-MODULE-HEADER is exported from clysm/backend/wasm-emit package."
  (testing "EMIT-MODULE-HEADER exported from clysm/backend/wasm-emit"
    ;; Find the symbol in the clysm/backend/wasm-emit package
    (multiple-value-bind (sym status)
        (find-symbol "EMIT-MODULE-HEADER" :clysm/backend/wasm-emit)
      (ok sym "EMIT-MODULE-HEADER symbol exists in package")
      (ok (eq status :external) "EMIT-MODULE-HEADER is external (exported)")))

  (testing "EMIT-MODULE-HEADER accessible from clysm package"
    ;; Verify re-export from main clysm package
    (multiple-value-bind (sym status)
        (find-symbol "EMIT-MODULE-HEADER" :clysm)
      (ok sym "EMIT-MODULE-HEADER symbol exists in clysm package")
      (ok (or (eq status :external) (eq status :inherited))
          "EMIT-MODULE-HEADER is accessible from clysm package"))))

(deftest emit-module-header-function-test
  "Verify EMIT-MODULE-HEADER is a function with correct behavior."
  (testing "EMIT-MODULE-HEADER is a function"
    (let ((sym (find-symbol "EMIT-MODULE-HEADER" :clysm/backend/wasm-emit)))
      (ok sym "EMIT-MODULE-HEADER symbol found")
      (when sym
        (ok (fboundp sym) "EMIT-MODULE-HEADER is bound to a function")
        ;; Test basic functionality
        (handler-case
            (let ((result (funcall sym)))
              (ok (vectorp result) "EMIT-MODULE-HEADER returns a vector")
              (ok (> (length result) 0) "EMIT-MODULE-HEADER returns non-empty result")
              ;; Check for Wasm magic bytes (0x00 0x61 0x73 0x6D)
              (ok (= (aref result 0) #x00) "First byte is 0x00 (Wasm magic)")
              (ok (= (aref result 1) #x61) "Second byte is 0x61 ('a')")
              (ok (= (aref result 2) #x73) "Third byte is 0x73 ('s')")
              (ok (= (aref result 3) #x6D) "Fourth byte is 0x6D ('m')"))
          (error (c)
            (fail (format nil "EMIT-MODULE-HEADER failed: ~A" c))))))))

;;; ============================================================
;;; T025: EMIT-MODULE-HEADER Runtime Table Registration Tests
;;; ============================================================

(deftest emit-module-header-runtime-table-test
  "Verify EMIT-MODULE-HEADER is registered in *runtime-function-table*."
  (testing "EMIT-MODULE-HEADER in runtime table"
    ;; Get the runtime function table
    (let ((table-sym (find-symbol "*RUNTIME-FUNCTION-TABLE*"
                                   :clysm/compiler/codegen/func-section)))
      (ok table-sym "*RUNTIME-FUNCTION-TABLE* symbol found")
      (when (and table-sym (boundp table-sym))
        (let* ((table (symbol-value table-sym))
               (emit-sym (find-symbol "EMIT-MODULE-HEADER" :clysm))
               (entry (when emit-sym (gethash emit-sym table))))
          (ok entry "EMIT-MODULE-HEADER registered in runtime table")
          (when entry
            (ok (car entry) "Entry has runtime name")
            (ok (eql (cdr entry) 0) "Entry has arity 0")))))))
