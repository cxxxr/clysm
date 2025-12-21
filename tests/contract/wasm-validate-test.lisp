;;;; wasm-validate-test.lisp - Wasm binary validation tests

(in-package #:clysm/tests/contract/wasm-validate)

(deftest test-module-header
  (let ((header (emit-module-header)))
    ;; Magic number: \0asm
    (ok (= #x00 (aref header 0)))
    (ok (= #x61 (aref header 1)))
    (ok (= #x73 (aref header 2)))
    (ok (= #x6d (aref header 3)))
    ;; Version: 1
    (ok (= #x01 (aref header 4)))
    (ok (= #x00 (aref header 5)))
    (ok (= #x00 (aref header 6)))
    (ok (= #x00 (aref header 7)))))

(deftest test-empty-module-generation
  (let ((module (emit-empty-module)))
    (ok (= 8 (length module)))
    (ok (equalp #(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00) module))))

(deftest test-empty-module-validates
  ;; This test requires wasm-tools to be available
  (let ((module (emit-empty-module)))
    ;; Write to temp file and validate
    (clysm/tests/helpers:with-temp-wasm-file (path module)
      (ok (zerop (nth-value 2 (uiop:run-program
                               (list "wasm-tools" "validate" path)
                               :ignore-error-status t)))))))
