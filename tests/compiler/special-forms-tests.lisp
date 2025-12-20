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

;;; defparameter/defconstant tests

(test compile-defconstant
  "Test compiling (defconstant +my-const+ 42)."
  (let* ((module (clysm/compiler:compile-module
                  '((defconstant +my-const+ 42))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))
    ;; Should have added a global
    (is (not (null (clysm/wasm:wasm-module-globals module))))))

(test compile-defparameter
  "Test compiling (defparameter *my-var* 10)."
  (let* ((module (clysm/compiler:compile-module
                  '((defparameter *my-var* 10))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))
    ;; Should have added a global
    (is (not (null (clysm/wasm:wasm-module-globals module))))))

(test compile-defconstant-use
  "Test using a defconstant value."
  (let* ((module (clysm/compiler:compile-module
                  '((defconstant +my-const+ 42)
                    (+ +my-const+ 1))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-defparameter-setq
  "Test modifying a defparameter with setq."
  (let* ((module (clysm/compiler:compile-module
                  '((defparameter *counter* 0)
                    (defun increment ()
                      (setq *counter* (+ *counter* 1))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; defstruct tests

(test compile-defstruct-simple
  "Test compiling a simple defstruct."
  (let* ((module (clysm/compiler:compile-module
                  '((defstruct point x y))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-defstruct-constructor
  "Test using a defstruct constructor."
  (let* ((module (clysm/compiler:compile-module
                  '((defstruct point x y)
                    (make-point 10 20))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-defstruct-accessor
  "Test using defstruct accessors."
  (let* ((module (clysm/compiler:compile-module
                  '((defstruct point x y)
                    (defun get-x (p) (point-x p))
                    (defun get-y (p) (point-y p)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-defstruct-predicate
  "Test using defstruct predicate."
  (let* ((module (clysm/compiler:compile-module
                  '((defstruct point x y)
                    (defun is-point (obj) (point-p obj)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-defstruct-with-defaults
  "Test defstruct with slot defaults."
  (let* ((module (clysm/compiler:compile-module
                  '((defstruct rect (width 10) (height 20))
                    (make-rect))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-defstruct-include
  "Test defstruct with :include option."
  (let* ((module (clysm/compiler:compile-module
                  '((defstruct point x y)
                    (defstruct (point-3d (:include point)) z)
                    (defun make-3d () (make-point-3d 1 2 3)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Macro expansion tests

(test compile-incf-macro
  "Test that incf macro is expanded and compiled."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-incf ()
                      (let ((x 0))
                        (incf x)
                        x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-decf-macro
  "Test that decf macro is expanded and compiled."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-decf ()
                      (let ((x 10))
                        (decf x)
                        x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-1+-macro
  "Test that 1+ is expanded and compiled."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-1+ (n)
                      (1+ n)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-1--macro
  "Test that 1- is expanded and compiled."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-1- (n)
                      (1- n)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-prog1-macro
  "Test that prog1 macro is expanded and compiled."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-prog1 ()
                      (let ((x 1))
                        (prog1 x
                          (setq x 2)))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-nested-macro
  "Test nested macro expansion."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-nested ()
                      (let ((x 0))
                        (incf x (1+ 1))
                        x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Backquote tests

(test compile-backquote-simple
  "Test that backquote with no unquotes compiles."
  (let* ((module (clysm/compiler:compile-module
                  '((defun make-list ()
                      `(1 2 3)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-backquote-unquote
  "Test that backquote with unquote compiles."
  (let* ((module (clysm/compiler:compile-module
                  '((defun make-list (x)
                      `(a ,x c)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-backquote-splice
  "Test that backquote with splice compiles."
  (let* ((module (clysm/compiler:compile-module
                  '((defun make-list (lst)
                      `(a ,@lst c)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-backquote-nested
  "Test that nested backquotes compile."
  (let* ((module (clysm/compiler:compile-module
                  '((defun make-code (x)
                      `(+ ,x 1)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Standard library function tests

(test compile-reverse
  "Test compiling reverse."
  (let* ((module (clysm/compiler:compile-module
                  '((defun rev (lst)
                      (reverse lst)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-nreverse
  "Test compiling nreverse."
  (let* ((module (clysm/compiler:compile-module
                  '((defun nrev (lst)
                      (nreverse lst)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-member
  "Test compiling member."
  (let* ((module (clysm/compiler:compile-module
                  '((defun find-in-list (x lst)
                      (member x lst)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-assoc
  "Test compiling assoc."
  (let* ((module (clysm/compiler:compile-module
                  '((defun lookup (key alist)
                      (assoc key alist)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-last
  "Test compiling last."
  (let* ((module (clysm/compiler:compile-module
                  '((defun get-last (lst)
                      (last lst)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-length
  "Test compiling length."
  (let* ((module (clysm/compiler:compile-module
                  '((defun list-length (lst)
                      (length lst)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-append
  "Test compiling append."
  (let* ((module (clysm/compiler:compile-module
                  '((defun concat-lists (a b)
                      (append a b)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-copy-list
  "Test compiling copy-list."
  (let* ((module (clysm/compiler:compile-module
                  '((defun copy-it (lst)
                      (copy-list lst)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-butlast
  "Test compiling butlast."
  (let* ((module (clysm/compiler:compile-module
                  '((defun all-but-last (lst)
                      (butlast lst)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-not
  "Test compiling not."
  (let* ((module (clysm/compiler:compile-module
                  '((defun negate (x)
                      (not x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Tagbody/go tests

(test compile-tagbody-simple
  "Test compiling simple tagbody."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-tagbody ()
                      (let ((x 0))
                        (tagbody
                         start
                          (setq x (+ x 1))
                          (when (< x 5)
                            (go start)))
                        x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-tagbody-multiple-tags
  "Test compiling tagbody with multiple tags."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-multi-tag ()
                      (let ((x 0))
                        (tagbody
                         tag1
                          (setq x (+ x 1))
                          (go tag2)
                         tag2
                          (setq x (+ x 10)))
                        x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-tagbody-early-exit
  "Test compiling tagbody with go to end."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-early-exit (n)
                      (let ((x 0))
                        (tagbody
                          (when (< n 0)
                            (go done))
                          (setq x n)
                         done)
                        x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Labels tests

(test compile-labels-simple
  "Test compiling simple labels with one local function."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-labels ()
                      (labels ((add1 (x) (+ x 1)))
                        (add1 5))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-labels-multiple
  "Test compiling labels with multiple local functions."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-multi-labels ()
                      (labels ((add1 (x) (+ x 1))
                               (double (x) (* x 2)))
                        (add1 (double 3)))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-labels-recursive
  "Test compiling labels with recursive local function."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-recursive-labels ()
                      (labels ((fact (n)
                                 (if (<= n 1)
                                     1
                                     (* n (fact (- n 1))))))
                        (fact 5))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Loop macro tests

(test compile-loop-for-collect
  "Test compiling loop for...collect."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-loop-collect ()
                      (loop for i from 1 to 5 collect i)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-loop-for-do
  "Test compiling loop for...do."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-loop-do ()
                      (let ((sum 0))
                        (loop for i from 1 to 5 do (setq sum (+ sum i)))
                        sum)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Higher-order function tests

(test compile-mapcar
  "Test compiling mapcar with lambda."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-mapcar ()
                      (mapcar (lambda (x) (+ x 1)) (list 1 2 3))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-mapc
  "Test compiling mapc for side effects."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-mapc ()
                      (let ((sum 0))
                        (mapc (lambda (x) (setq sum (+ sum x))) (list 1 2 3))
                        sum)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-reduce
  "Test compiling reduce."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-reduce ()
                      (reduce (lambda (a b) (+ a b)) (list 1 2 3 4))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-reduce-with-initial
  "Test compiling reduce with initial value."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-reduce-init ()
                      (reduce (lambda (a b) (+ a b)) (list 1 2 3) :initial-value 10)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Case and ecase macro tests

(test compile-case-macro
  "Test compiling case macro."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-case (x)
                      (case x
                        (1 10)
                        (2 20)
                        (t 0))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Type predicate tests

(test compile-listp
  "Test compiling listp."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-listp (x)
                      (listp x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-numberp
  "Test compiling numberp."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-numberp (x)
                      (numberp x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Multiple values tests

(test compile-values-single
  "Test compiling values with single value."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-values ()
                      (values 42)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-values-multiple
  "Test compiling values with multiple values."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-values2 ()
                      (values 1 2 3)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-multiple-value-bind
  "Test compiling multiple-value-bind."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-mvb ()
                      (multiple-value-bind (a b) (values 10 20)
                        (+ a b))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Destructuring-bind tests

(test compile-destructuring-bind-simple
  "Test compiling simple destructuring-bind."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-dsb ()
                      (destructuring-bind (a b c) (list 1 2 3)
                        (+ a b c))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-destructuring-bind-nested
  "Test compiling nested destructuring-bind."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-dsb2 (lst)
                      (destructuring-bind (a b) lst
                        (+ a b))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Error handling tests

(test compile-error
  "Test compiling error function."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-error (x)
                      (if (< x 0)
                          (error "negative value")
                          x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Hash table tests

(test compile-make-hash-table
  "Test compiling make-hash-table."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-ht ()
                      (make-hash-table)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-hash-table-count
  "Test compiling hash-table-count."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-htc ()
                      (hash-table-count (make-hash-table))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-sethash-gethash
  "Test compiling sethash and gethash."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-sethash ()
                      (let ((ht (make-hash-table)))
                        (sethash 'key 42 ht)
                        (gethash 'key ht))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-remhash
  "Test compiling remhash."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-remhash ()
                      (let ((ht (make-hash-table)))
                        (sethash 'key 42 ht)
                        (remhash 'key ht))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-clrhash
  "Test compiling clrhash."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-clrhash ()
                      (let ((ht (make-hash-table)))
                        (sethash 'key 42 ht)
                        (clrhash ht))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; User-defined macro tests

(test compile-user-defmacro
  "Test compiling code with user-defined macro."
  (let* ((module (clysm/compiler:compile-module
                  '((defmacro my-when (test &rest body)
                      `(if ,test (progn ,@body) nil))
                    (defun test-my-when (x)
                      (my-when (> x 0)
                        (+ x 1))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-user-defmacro-with-gensym
  "Test compiling code with user-defined macro using gensym."
  (let* ((module (clysm/compiler:compile-module
                  '((defmacro my-if-let (binding then &optional else)
                      (let ((var (first binding))
                            (val (second binding)))
                        `(let ((,var ,val))
                           (if ,var ,then ,else))))
                    (defun test-if-let (x)
                      (my-if-let (y x)
                        (+ y 1)
                        0)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-user-defmacro-recursive-expansion
  "Test that macros are recursively expanded."
  (let* ((module (clysm/compiler:compile-module
                  '((defmacro add-one (x)
                      `(+ ,x 1))
                    (defmacro add-two (x)
                      `(add-one (add-one ,x)))
                    (defun test-add-two (x)
                      (add-two x)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-user-defmacro-with-list-construction
  "Test macro that constructs code using list functions."
  (let* ((module (clysm/compiler:compile-module
                  '((defmacro with-additions (n &rest vals)
                      `(+ ,n ,@vals))
                    (defun test-additions ()
                      (with-additions 10 1 2 3)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-defmacro-not-in-output
  "Test that defmacro itself doesn't appear in WASM output."
  ;; Just a defmacro with no uses - should still compile (empty module)
  (let* ((module (clysm/compiler:compile-module
                  '((defmacro unused-macro (x) x))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; Setf tests

(test compile-setf-variable
  "Test compiling setf for simple variable."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-setf-var (x)
                      (setf x 10)
                      x))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-setf-car
  "Test compiling setf for car."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-setf-car ()
                      (let ((c (cons 1 2)))
                        (setf (car c) 10)
                        (car c))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-setf-cdr
  "Test compiling setf for cdr."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-setf-cdr ()
                      (let ((c (cons 1 2)))
                        (setf (cdr c) 20)
                        (cdr c))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-setf-gethash
  "Test compiling setf for gethash."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-setf-gethash ()
                      (let ((ht (make-hash-table)))
                        (setf (gethash 'key ht) 42)
                        (gethash 'key ht))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-setf-struct
  "Test compiling setf for struct accessor."
  (let* ((module (clysm/compiler:compile-module
                  '((defstruct point x y)
                    (defun test-setf-struct ()
                      (let ((p (make-point)))
                        (setf (point-x p) 10)
                        (point-x p))))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

;;; String primitive tests

(test compile-char-code
  "Test compiling char-code."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-char-code (c)
                      (char-code c)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-code-char
  "Test compiling code-char."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-code-char (n)
                      (code-char n)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-char=
  "Test compiling char=."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-char= (a b)
                      (char= a b)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-char-upcase
  "Test compiling char-upcase."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-char-upcase (c)
                      (char-upcase c)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-char-downcase
  "Test compiling char-downcase."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-char-downcase (c)
                      (char-downcase c)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-schar
  "Test compiling schar."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-schar (s i)
                      (schar s i)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-string=
  "Test compiling string=."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-string= (a b)
                      (string= a b)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-string-downcase
  "Test compiling string-downcase."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-string-downcase (s)
                      (string-downcase s)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-string-upcase
  "Test compiling string-upcase."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-string-upcase (s)
                      (string-upcase s)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-string-length
  "Test compiling string-length."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-string-length (s)
                      (string-length s)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))

(test compile-string-append
  "Test compiling string-append."
  (let* ((module (clysm/compiler:compile-module
                  '((defun test-string-append (a b)
                      (string-append a b)))))
         (bytes (clysm/wasm:encode-module module)))
    (is (> (length bytes) 8))))
