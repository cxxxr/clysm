;;;; encoder-tests.lisp - WASM encoder tests

(in-package #:cl-wasm/tests)

(in-suite :wasm)

(test wasm-magic-header
  "Test that WASM modules start with correct magic number and version."
  (let* ((module (make-wasm-module))
         (bytes (encode-module module)))
    ;; Magic number: \0asm = 0x00 0x61 0x73 0x6d
    (is (= #x00 (aref bytes 0)))
    (is (= #x61 (aref bytes 1)))
    (is (= #x73 (aref bytes 2)))
    (is (= #x6d (aref bytes 3)))
    ;; Version: 1 (little-endian)
    (is (= #x01 (aref bytes 4)))
    (is (= #x00 (aref bytes 5)))
    (is (= #x00 (aref bytes 6)))
    (is (= #x00 (aref bytes 7)))))

(test simple-add-function
  "Test encoding a simple function that adds two i32s."
  (let ((module (make-wasm-module)))
    ;; Add function type: (i32, i32) -> i32
    (add-func-type module
                   (list +type-i32+ +type-i32+)
                   (list +type-i32+))
    ;; Add function with body: local.get 0, local.get 1, i32.add
    (add-function module 0 nil
                  `((#x20 0)      ; local.get 0
                    (#x20 1)      ; local.get 1
                    #x6a))        ; i32.add
    ;; Export as "add"
    (add-export module "add" +export-func+ 0)
    ;; Finalize
    (finalize-module module)
    ;; Encode
    (let ((bytes (encode-module module)))
      ;; Should produce valid WASM
      (is (> (length bytes) 8))
      ;; Check magic
      (is (= #x00 (aref bytes 0)))
      (is (= #x61 (aref bytes 1))))))

(test const-return-function
  "Test encoding a function that returns a constant."
  (let ((module (make-wasm-module)))
    ;; Add function type: () -> i32
    (add-func-type module nil (list +type-i32+))
    ;; Add function with body: i32.const 42
    (add-function module 0 nil
                  `((#x41 42)))   ; i32.const 42
    ;; Export as "answer"
    (add-export module "answer" +export-func+ 0)
    ;; Finalize and encode
    (finalize-module module)
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8)))))

(test arithmetic-expression
  "Test encoding (+ 1 2)."
  (let ((module (make-wasm-module)))
    ;; Add function type: () -> i32
    (add-func-type module nil (list +type-i32+))
    ;; Add function: i32.const 1, i32.const 2, i32.add
    (add-function module 0 nil
                  `((,+op-i32-const+ 1)
                    (,+op-i32-const+ 2)
                    ,+op-i32-add+))
    ;; Export as "main"
    (add-export module "main" +export-func+ 0)
    ;; Finalize and encode
    (finalize-module module)
    (let ((bytes (encode-module module)))
      (is (> (length bytes) 8))
      ;; Verify this is valid WASM structure
      (is (= #x00 (aref bytes 0)))   ; magic
      (is (= #x61 (aref bytes 1)))
      (is (= #x73 (aref bytes 2)))
      (is (= #x6d (aref bytes 3))))))
