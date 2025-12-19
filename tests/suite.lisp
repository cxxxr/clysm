;;;; suite.lisp - Test suite definitions

(in-package #:cl-wasm/tests)

;;; Define test suites

(def-suite :cl-wasm
  :description "All cl-wasm tests")

(def-suite :utils :in :cl-wasm
  :description "Utility function tests")

(def-suite :wasm :in :cl-wasm
  :description "WASM encoding tests")

(def-suite :compiler :in :cl-wasm
  :description "Compiler tests")

(def-suite :integration :in :cl-wasm
  :description "Integration tests")

;;; Test runner

(defun run-tests ()
  "Run all cl-wasm tests."
  (run! :cl-wasm))

;;; Utility Tests

(in-suite :utils)

(test leb128-unsigned
  "Test ULEB128 encoding."
  (is (equal '(0) (encode-uleb128 0)))
  (is (equal '(1) (encode-uleb128 1)))
  (is (equal '(127) (encode-uleb128 127)))
  (is (equal '(128 1) (encode-uleb128 128)))
  (is (equal '(255 1) (encode-uleb128 255)))
  (is (equal '(128 2) (encode-uleb128 256)))
  (is (equal '(232 7) (encode-uleb128 1000))))

(test leb128-signed
  "Test SLEB128 encoding."
  (is (equal '(0) (encode-sleb128 0)))
  (is (equal '(1) (encode-sleb128 1)))
  (is (equal '(127) (encode-sleb128 -1)))
  (is (equal '(128 127) (encode-sleb128 -128)))
  (is (equal '(128 1) (encode-sleb128 128)))
  (is (equal '(129 127) (encode-sleb128 -127))))

(test byte-buffer
  "Test byte buffer operations."
  (let ((buf (make-byte-buffer)))
    (buffer-write-byte buf #x00)
    (buffer-write-byte buf #x61)
    (buffer-write-byte buf #x73)
    (buffer-write-byte buf #x6d)
    (let ((result (buffer-contents buf)))
      (is (= 4 (length result)))
      (is (equalp #(#x00 #x61 #x73 #x6d) result)))))
