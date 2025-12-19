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

;;; Quote tests

(test compile-quote-nil
  "Test compiling (quote nil)."
  (let* ((module (cl-wasm/compiler:compile-module '((quote nil))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-quote-t
  "Test compiling (quote t)."
  (let* ((module (cl-wasm/compiler:compile-module '((quote t))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-quote-integer
  "Test compiling (quote 42)."
  (let* ((module (cl-wasm/compiler:compile-module '((quote 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-quote-list
  "Test compiling (quote (1 2 3))."
  (let* ((module (cl-wasm/compiler:compile-module '((quote (1 2 3)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))
    ;; Should have memory for cons cells
    (is (not (null (cl-wasm/wasm:wasm-module-memories module))))))

(test compile-quote-nested-list
  "Test compiling (quote ((1 2) (3 4)))."
  (let* ((module (cl-wasm/compiler:compile-module '((quote ((1 2) (3 4))))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-car-quoted-list
  "Test compiling (car (quote (1 2 3)))."
  (let* ((module (cl-wasm/compiler:compile-module '((car (quote (1 2 3))))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; let* tests

(test compile-let*-empty
  "Test compiling (let* () 42)."
  (let* ((module (cl-wasm/compiler:compile-module '((let* () 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-let*-single
  "Test compiling (let* ((x 1)) x)."
  (let* ((module (cl-wasm/compiler:compile-module '((let* ((x 1)) x))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-let*-sequential
  "Test compiling (let* ((x 1) (y (+ x 1))) y)."
  (let* ((module (cl-wasm/compiler:compile-module '((let* ((x 1) (y (+ x 1))) y))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-let*-three-vars
  "Test compiling (let* ((a 1) (b (+ a 1)) (c (+ b a))) c)."
  (let* ((module (cl-wasm/compiler:compile-module
                  '((let* ((a 1) (b (+ a 1)) (c (+ b a))) c))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; when/unless tests

(test compile-when
  "Test compiling (when t 42)."
  (let* ((module (cl-wasm/compiler:compile-module '((when t 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-unless
  "Test compiling (unless nil 42)."
  (let* ((module (cl-wasm/compiler:compile-module '((unless nil 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; cond tests

(test compile-cond-empty
  "Test compiling (cond)."
  (let* ((module (cl-wasm/compiler:compile-module '((cond))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-cond-t-clause
  "Test compiling (cond (t 42))."
  (let* ((module (cl-wasm/compiler:compile-module '((cond (t 42)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-cond-multiple
  "Test compiling (cond (nil 1) (t 2))."
  (let* ((module (cl-wasm/compiler:compile-module '((cond (nil 1) (t 2)))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; and/or tests

(test compile-and-empty
  "Test compiling (and)."
  (let* ((module (cl-wasm/compiler:compile-module '((and))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-and-single
  "Test compiling (and 42)."
  (let* ((module (cl-wasm/compiler:compile-module '((and 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-and-multiple
  "Test compiling (and 1 2 3)."
  (let* ((module (cl-wasm/compiler:compile-module '((and 1 2 3))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-or-empty
  "Test compiling (or)."
  (let* ((module (cl-wasm/compiler:compile-module '((or))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-or-single
  "Test compiling (or 42)."
  (let* ((module (cl-wasm/compiler:compile-module '((or 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-or-multiple
  "Test compiling (or nil 42)."
  (let* ((module (cl-wasm/compiler:compile-module '((or nil 42))))
         (bytes (cl-wasm/wasm:encode-module module)))
    (is (> (length bytes) 8))))
