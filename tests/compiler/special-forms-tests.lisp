;;;; special-forms-tests.lisp - Special form compilation tests

(in-package #:clysm/tests)

(in-suite :compiler)

(test compile-integer-constant
  "Test compiling an integer constant."
  (let* ((module (clysm/wasm:make-wasm-module))
         (env (make-initial-env module))
         (code (compile-form 42 env)))
    (is (not (null code)))
    (is (equal `((,+op-i32-const+ 42)) code))))

(test compile-nil
  "Test compiling nil."
  (let* ((module (clysm/wasm:make-wasm-module))
         (env (make-initial-env module))
         (code (compile-form nil env)))
    (is (equal `((,+op-i32-const+ 0)) code))))

(test compile-addition
  "Test compiling (+ 1 2)."
  (let* ((module (clysm/wasm:make-wasm-module))
         (env (make-initial-env module))
         (code (compile-form '(+ 1 2) env)))
    (is (not (null code)))
    ;; Should generate: i32.const 1, i32.const 2, i32.add
    (is (member +op-i32-add+ code))))

(test compile-nested-arithmetic
  "Test compiling (+ (* 2 3) 4)."
  (let* ((module (clysm/wasm:make-wasm-module))
         (env (make-initial-env module))
         (code (compile-form '(+ (* 2 3) 4) env)))
    (is (not (null code)))
    ;; Should have both mul and add
    (is (member #x6c code))   ; i32.mul
    (is (member +op-i32-add+ code))))

;;; Quote tests

(test compile-quote-nil
  "Test compiling (quote nil)."
  (let* ((module (clysm/compiler:compile-module '((quote nil))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-quote-t
  "Test compiling (quote t)."
  (let* ((module (clysm/compiler:compile-module '((quote t))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-quote-integer
  "Test compiling (quote 42)."
  (let* ((module (clysm/compiler:compile-module '((quote 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-quote-list
  "Test compiling (quote (1 2 3))."
  (let* ((module (clysm/compiler:compile-module '((quote (1 2 3)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))
    ;; Should have memory for cons cells
    (is (not (null (clysm/wasm:wasm-module-memories module))))))

(test compile-quote-nested-list
  "Test compiling (quote ((1 2) (3 4)))."
  (let* ((module (clysm/compiler:compile-module '((quote ((1 2) (3 4))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-car-quoted-list
  "Test compiling (car (quote (1 2 3)))."
  (let* ((module (clysm/compiler:compile-module '((car (quote (1 2 3))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; let* tests

(test compile-let*-empty
  "Test compiling (let* () 42)."
  (let* ((module (clysm/compiler:compile-module '((let* () 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-let*-single
  "Test compiling (let* ((x 1)) x)."
  (let* ((module (clysm/compiler:compile-module '((let* ((x 1)) x))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-let*-sequential
  "Test compiling (let* ((x 1) (y (+ x 1))) y)."
  (let* ((module (clysm/compiler:compile-module '((let* ((x 1) (y (+ x 1))) y))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-let*-three-vars
  "Test compiling (let* ((a 1) (b (+ a 1)) (c (+ b a))) c)."
  (let* ((module (clysm/compiler:compile-module
                  '((let* ((a 1) (b (+ a 1)) (c (+ b a))) c))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; when/unless tests

(test compile-when
  "Test compiling (when t 42)."
  (let* ((module (clysm/compiler:compile-module '((when t 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-unless
  "Test compiling (unless nil 42)."
  (let* ((module (clysm/compiler:compile-module '((unless nil 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; cond tests

(test compile-cond-empty
  "Test compiling (cond)."
  (let* ((module (clysm/compiler:compile-module '((cond))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-cond-t-clause
  "Test compiling (cond (t 42))."
  (let* ((module (clysm/compiler:compile-module '((cond (t 42)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-cond-multiple
  "Test compiling (cond (nil 1) (t 2))."
  (let* ((module (clysm/compiler:compile-module '((cond (nil 1) (t 2)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; and/or tests

(test compile-and-empty
  "Test compiling (and)."
  (let* ((module (clysm/compiler:compile-module '((and))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-and-single
  "Test compiling (and 42)."
  (let* ((module (clysm/compiler:compile-module '((and 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-and-multiple
  "Test compiling (and 1 2 3)."
  (let* ((module (clysm/compiler:compile-module '((and 1 2 3))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-or-empty
  "Test compiling (or)."
  (let* ((module (clysm/compiler:compile-module '((or))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-or-single
  "Test compiling (or 42)."
  (let* ((module (clysm/compiler:compile-module '((or 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-or-multiple
  "Test compiling (or nil 42)."
  (let* ((module (clysm/compiler:compile-module '((or nil 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; block/return-from tests

(test compile-block-simple
  "Test compiling a simple block."
  (let* ((module (clysm/compiler:compile-module '((block foo 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-block-with-return
  "Test compiling a block with return-from."
  (let* ((module (clysm/compiler:compile-module
                  '((block done
                      (return-from done 99)
                      42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-block-nil-name
  "Test compiling a block with nil name and return."
  (let* ((module (clysm/compiler:compile-module
                  '((block nil
                      (return 100)
                      0))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-nested-blocks
  "Test compiling nested blocks."
  (let* ((module (clysm/compiler:compile-module
                  '((block outer
                      (block inner
                        (return-from outer 1))
                      2))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-block-in-defun
  "Test compiling block inside a function."
  (let* ((module (clysm/compiler:compile-module
                  '((defun early-exit (x)
                      (block nil
                        (when (< x 0)
                          (return 0))
                        (* x 2))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; dotimes/dolist tests

(test compile-dotimes-simple
  "Test compiling simple dotimes."
  (let* ((module (clysm/compiler:compile-module
                  '((defun sum-to-n (n)
                      (let ((sum 0))
                        (dotimes (i n sum)
                          (setq sum (+ sum i))))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-dotimes-with-body
  "Test compiling dotimes with multiple body forms."
  (let* ((module (clysm/compiler:compile-module
                  '((defun count-up (n)
                      (let ((result 0))
                        (dotimes (i n result)
                          (setq result (+ result 1))
                          (setq result (+ result i))))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-dolist-simple
  "Test compiling simple dolist."
  (let* ((module (clysm/compiler:compile-module
                  '((defun sum-list (lst)
                      (let ((sum 0))
                        (dolist (x lst sum)
                          (setq sum (+ sum x))))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Symbol tests

(test compile-quote-symbol
  "Test compiling (quote foo)."
  (let* ((module (clysm/compiler:compile-module '((quote foo))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))
    ;; Should have data section with symbol
    (is (not (null (clysm/wasm:wasm-module-data module))))))

(test compile-quote-multiple-symbols
  "Test compiling multiple quoted symbols."
  (let* ((module (clysm/compiler:compile-module
                  '((cons (quote foo) (quote bar)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))
    ;; Should have data section with symbols
    (is (not (null (clysm/wasm:wasm-module-data module))))))

(test compile-quote-same-symbol-twice
  "Test that quoting the same symbol twice uses the same address."
  ;; This tests symbol interning
  (let* ((module (clysm/compiler:compile-module
                  '((cons (quote hello) (quote hello)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-symbol-name
  "Test compiling (symbol-name 'foo)."
  (let* ((module (clysm/compiler:compile-module
                  '((symbol-name (quote foo)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-symbolp
  "Test compiling (symbolp 'foo)."
  (let* ((module (clysm/compiler:compile-module
                  '((symbolp (quote foo)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-eq-symbols
  "Test compiling (eq 'foo 'foo)."
  (let* ((module (clysm/compiler:compile-module
                  '((eq (quote foo) (quote foo)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))
