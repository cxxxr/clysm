;;;; compiler.lisp - Main compiler interface

(in-package #:clysm/compiler)

(defun compile-to-wasm (expr &key output)
  "Compile a Lisp expression to Wasm binary.
   If OUTPUT is provided, writes to file and returns pathname.
   Otherwise returns byte vector."
  ;; TODO: Implement full compilation pipeline
  (declare (ignore expr))
  (emit-empty-module :output output))

(defun compile-to-wat (expr)
  "Compile a Lisp expression to WAT text format."
  ;; TODO: Implement
  (declare (ignore expr))
  "(module)")

(defun compile-expression (expr)
  "Compile a single expression."
  ;; TODO: Implement
  (declare (ignore expr))
  nil)
