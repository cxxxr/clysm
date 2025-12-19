;;;; special-forms-tests.lisp - Special form compilation tests

(in-package #:cl-wasm/tests)

(in-suite :compiler)

(test compile-integer-constant
  "Test compiling an integer constant."
  (let* ((module (cl-wasm/wasm:make-wasm-module))
         (env (make-initial-env module))
         (code (compile-form 42 env)))
    (is (not (null code)))
    (is (equal `((,+op-i32-const+ 42)) code))))

(test compile-nil
  "Test compiling nil."
  (let* ((module (cl-wasm/wasm:make-wasm-module))
         (env (make-initial-env module))
         (code (compile-form nil env)))
    (is (equal `((,+op-i32-const+ 0)) code))))

(test compile-addition
  "Test compiling (+ 1 2)."
  (let* ((module (cl-wasm/wasm:make-wasm-module))
         (env (make-initial-env module))
         (code (compile-form '(+ 1 2) env)))
    (is (not (null code)))
    ;; Should generate: i32.const 1, i32.const 2, i32.add
    (is (member +op-i32-add+ code))))

(test compile-nested-arithmetic
  "Test compiling (+ (* 2 3) 4)."
  (let* ((module (cl-wasm/wasm:make-wasm-module))
         (env (make-initial-env module))
         (code (compile-form '(+ (* 2 3) 4) env)))
    (is (not (null code)))
    ;; Should have both mul and add
    (is (member #x6c code))   ; i32.mul
    (is (member +op-i32-add+ code))))
