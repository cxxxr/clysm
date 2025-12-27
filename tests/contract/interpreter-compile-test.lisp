;;;; interpreter-compile-test.lisp - Contract tests for interpreter-based compilation
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Task: T081

(defpackage #:clysm/tests/contract/interpreter-compile-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:interpret-file
                #:make-interpreter-env)
  (:import-from #:clysm/compiler
                #:compile-to-wasm))

(in-package #:clysm/tests/contract/interpreter-compile-test)

;;; ============================================================
;;; Contract: Interpreter can execute compile-to-wasm
;;; ============================================================

(deftest test-compile-simple-form-via-interpreter
  "Contract: Interpreter can call compile-to-wasm on simple form."
  (let ((env (make-interpreter-env)))
    ;; Bind compile-to-wasm function in interpreter environment
    (interpret `(defun test-compile ()
                  (funcall (function ,#'compile-to-wasm) '(+ 1 2)))
               env)
    ;; Execute the compilation
    (let ((result (interpret '(test-compile) env)))
      ;; Should return bytes (vector)
      (ok (vectorp result))
      (ok (> (length result) 0)))))

(deftest test-compile-defun-via-interpreter
  "Contract: Interpreter can compile a defun form."
  (let ((env (make-interpreter-env)))
    (interpret `(defun test-compile-defun ()
                  (funcall (function ,#'compile-to-wasm)
                           '(defun add (a b) (+ a b))))
               env)
    (let ((result (interpret '(test-compile-defun) env)))
      (ok (vectorp result))
      ;; Wasm magic bytes
      (when (> (length result) 4)
        (ok (= #x00 (aref result 0)))
        (ok (= #x61 (aref result 1)))
        (ok (= #x73 (aref result 2)))
        (ok (= #x6d (aref result 3)))))))

(deftest test-compile-lambda-via-interpreter
  "Contract: Interpreter can compile lambda expression."
  (let ((env (make-interpreter-env)))
    (interpret `(defun test-compile-lambda ()
                  (funcall (function ,#'compile-to-wasm)
                           '(lambda (x) (* x x))))
               env)
    (let ((result (interpret '(test-compile-lambda) env)))
      (ok (vectorp result))
      (ok (> (length result) 0)))))

;;; ============================================================
;;; Contract: Compiled output is valid Wasm
;;; ============================================================

(deftest test-wasm-output-has-magic-bytes
  "Contract: Compiled Wasm starts with magic bytes."
  (let ((bytes (compile-to-wasm '(+ 1 2))))
    (ok (vectorp bytes))
    (ok (>= (length bytes) 8))
    ;; Magic: \0asm
    (ok (= #x00 (aref bytes 0)))
    (ok (= #x61 (aref bytes 1)))
    (ok (= #x73 (aref bytes 2)))
    (ok (= #x6d (aref bytes 3)))
    ;; Version: 1.0.0.0
    (ok (= #x01 (aref bytes 4)))))

(deftest test-wasm-output-is-byte-vector
  "Contract: compile-to-wasm returns (vector (unsigned-byte 8))."
  (let ((bytes (compile-to-wasm '(+ 1 2))))
    (ok (typep bytes '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; Contract: Interpreter can load compiler modules
;;; ============================================================

(deftest test-interpreter-loads-package-file
  "Contract: Interpreter can load package.lisp from compiler."
  (let* ((env (make-interpreter-env))
         (pkg-path (merge-pathnames "src/clysm/package.lisp"
                                    (asdf:system-source-directory :clysm))))
    (when (probe-file pkg-path)
      ;; Should load without error
      (ok (interpret-file pkg-path :env env)))))

;;; ============================================================
;;; Contract: Interpreted and Direct Compilation Match
;;; ============================================================

(deftest test-interpreted-compile-matches-direct
  "Contract: Interpreter-invoked compile-to-wasm produces same result."
  (let* ((form '(+ 1 2))
         (direct-result (compile-to-wasm form))
         (env (make-interpreter-env)))
    ;; Execute compile-to-wasm via interpreter
    (interpret `(defun interp-compile ()
                  (funcall (function ,#'compile-to-wasm) ',form))
               env)
    (let ((interp-result (interpret '(interp-compile) env)))
      ;; Results should be identical
      (ok (equalp direct-result interp-result)))))
