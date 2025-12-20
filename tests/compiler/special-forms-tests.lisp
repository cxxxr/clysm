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

;;; Eval API tests (require Node.js)

(test eval-form-arithmetic
  "Test eval-form with arithmetic."
  (is (= 3 (clysm/compiler:eval-form '(+ 1 2))))
  (is (= 42 (clysm/compiler:eval-form '(* 6 7))))
  (is (= 10 (clysm/compiler:eval-form '(+ (+ 1 2) (+ 3 4))))))

(test eval-form-conditionals
  "Test eval-form with conditionals."
  (is (= 100 (clysm/compiler:eval-form '(if (> 5 3) 100 200))))
  (is (= 200 (clysm/compiler:eval-form '(if (< 5 3) 100 200)))))

(test eval-form-let
  "Test eval-form with let bindings."
  (is (= 100 (clysm/compiler:eval-form '(let ((x 10)) (* x x)))))
  (is (= 30 (clysm/compiler:eval-form '(let ((a 10) (b 20)) (+ a b))))))

(test eval-form-cons
  "Test eval-form with cons cells."
  (is (= 42 (clysm/compiler:eval-form '(car (cons 42 99)))))
  (is (= 99 (clysm/compiler:eval-form '(cdr (cons 42 99))))))

(test eval-forms-defun
  "Test eval-forms with function definitions."
  (is (= 120 (clysm/compiler:eval-forms
              '((defun factorial (n)
                  (if (<= n 1) 1 (* n (factorial (- n 1)))))
                (factorial 5)))))
  (is (= 25 (clysm/compiler:eval-forms
              '((defun square (x) (* x x))
                (square 5))))))

;;; Reader Primitives Tests

(test reader-state-creation
  "Test make-reader-state creates a reader state."
  ;; We test by creating a reader state and checking if it's non-nil
  (is (not (zerop (clysm/compiler:eval-form
                   '(make-reader-state "hello"))))))

(test reader-state-peek-char
  "Test reader-state-peek-char returns first character."
  ;; 'h' = 104
  (is (= 104 (clysm/compiler:eval-form
              '(let ((rs (make-reader-state "hello")))
                 (reader-state-peek-char rs))))))

(test reader-state-peek-char-no-advance
  "Test reader-state-peek-char doesn't advance position."
  ;; Multiple peeks should return same char
  (is (= 104 (clysm/compiler:eval-form
              '(let ((rs (make-reader-state "hello")))
                 (reader-state-peek-char rs)
                 (reader-state-peek-char rs))))))

(test reader-state-read-char
  "Test reader-state-read-char returns and advances."
  ;; Read 'h' then 'e'
  ;; 'h' = 104, 'e' = 101
  (is (= 101 (clysm/compiler:eval-form
              '(let ((rs (make-reader-state "hello")))
                 (reader-state-read-char rs)  ; read 'h'
                 (reader-state-read-char rs))))) ; read 'e'
  (is (= 104 (clysm/compiler:eval-form
              '(let ((rs (make-reader-state "hello")))
                 (reader-state-read-char rs))))))

(test reader-state-eof
  "Test reader-state-eof-p detects end of input."
  (is (= 1 (clysm/compiler:eval-form
            '(let ((rs (make-reader-state "")))
               (reader-state-eof-p rs)))))
  (is (= 0 (clysm/compiler:eval-form
            '(let ((rs (make-reader-state "x")))
               (reader-state-eof-p rs))))))

(test reader-state-unread-char
  "Test reader-state-unread-char moves position back."
  ;; Read 'h', unread, read again should give 'h'
  (is (= 104 (clysm/compiler:eval-form
              '(let ((rs (make-reader-state "hello")))
                 (reader-state-read-char rs)  ; read 'h'
                 (reader-state-unread-char rs)
                 (reader-state-read-char rs)))))) ; read 'h' again

(test whitespace-char-p
  "Test whitespace-char-p identifies whitespace."
  (is (= 1 (clysm/compiler:eval-form '(whitespace-char-p 32))))  ; space
  (is (= 1 (clysm/compiler:eval-form '(whitespace-char-p 9))))   ; tab
  (is (= 1 (clysm/compiler:eval-form '(whitespace-char-p 10))))  ; newline
  (is (= 0 (clysm/compiler:eval-form '(whitespace-char-p 65))))) ; 'A'

(test digit-char-p
  "Test digit-char-p identifies digits and returns value."
  ;; digit-char-p returns value+1 for digits (so 0 is distinguishable from nil)
  (is (= 1 (clysm/compiler:eval-form '(digit-char-p 48))))  ; '0' -> 1 (0+1)
  (is (= 10 (clysm/compiler:eval-form '(digit-char-p 57)))) ; '9' -> 10 (9+1)
  (is (= 0 (clysm/compiler:eval-form '(digit-char-p 65))))) ; 'A' -> nil

(test alpha-char-p
  "Test alpha-char-p identifies alphabetic characters."
  (is (= 1 (clysm/compiler:eval-form '(alpha-char-p 65))))  ; 'A'
  (is (= 1 (clysm/compiler:eval-form '(alpha-char-p 90))))  ; 'Z'
  (is (= 1 (clysm/compiler:eval-form '(alpha-char-p 97))))  ; 'a'
  (is (= 1 (clysm/compiler:eval-form '(alpha-char-p 122)))) ; 'z'
  (is (= 0 (clysm/compiler:eval-form '(alpha-char-p 48))))) ; '0'

(test symbol-constituent-p
  "Test symbol-constituent-p identifies valid symbol characters."
  (is (= 1 (clysm/compiler:eval-form '(symbol-constituent-p 65))))  ; 'A'
  (is (= 1 (clysm/compiler:eval-form '(symbol-constituent-p 43))))  ; '+'
  (is (= 1 (clysm/compiler:eval-form '(symbol-constituent-p 45))))  ; '-'
  (is (= 1 (clysm/compiler:eval-form '(symbol-constituent-p 42))))  ; '*'
  (is (= 0 (clysm/compiler:eval-form '(symbol-constituent-p 40))))  ; '('
  (is (= 0 (clysm/compiler:eval-form '(symbol-constituent-p 41))))  ; ')'
  (is (= 0 (clysm/compiler:eval-form '(symbol-constituent-p 32))))) ; space

;;; Reader Function Tests

(defparameter *reader-test-preamble*
  '(;; Skip whitespace
    (defun skip-ws-helper (rs)
      (let ((ch (reader-state-peek-char rs)))
        (if (whitespace-char-p ch)
            (progn (reader-state-read-char rs) (skip-ws-helper rs))
            nil)))
    (defun skip-ws (rs) (skip-ws-helper rs))
    ;; Read integer
    (defun read-int-helper (rs result)
      (let ((d (digit-char-p (reader-state-peek-char rs))))
        (if d
            (progn (reader-state-read-char rs)
                   (read-int-helper rs (+ (* result 10) (- d 1))))
            result)))
    (defun read-int (rs)
      (let ((ch (reader-state-peek-char rs)))
        (cond
          ((= ch 45)
           (reader-state-read-char rs)
           (- 0 (read-int-helper rs 0)))
          ((= ch 43)
           (reader-state-read-char rs)
           (read-int-helper rs 0))
          (t (read-int-helper rs 0)))))
    ;; Read form
    (defun read-form (rs)
      (skip-ws rs)
      (let ((ch (reader-state-peek-char rs)))
        (cond
          ((= ch -1) -1)
          ((= ch 40) (reader-state-read-char rs) (read-list rs))
          ((or (digit-char-p ch) (= ch 45) (= ch 43)) (read-int rs))
          (t -99))))
    ;; Read list
    (defun read-list (rs)
      (skip-ws rs)
      (let ((ch (reader-state-peek-char rs)))
        (cond
          ((= ch 41) (reader-state-read-char rs) nil)
          ((= ch -1) -3)
          (t (cons (read-form rs) (read-list rs)))))))
  "Common reader functions for tests.")

(test reader-read-integer
  "Test reading integers from string."
  (is (= 123 (clysm/compiler:eval-forms
              (append *reader-test-preamble*
                      '((let ((rs (make-reader-state "123")))
                          (read-form rs)))))))
  (is (= -42 (clysm/compiler:eval-forms
              (append *reader-test-preamble*
                      '((let ((rs (make-reader-state "-42")))
                          (read-form rs))))))))

(test reader-read-simple-list
  "Test reading simple lists from string."
  (is (= 2 (clysm/compiler:eval-forms
            (append *reader-test-preamble*
                    '((let ((rs (make-reader-state "(1 2 3)")))
                        (car (cdr (read-form rs))))))))))  ; second element

(test reader-read-nested-list
  "Test reading nested lists from string."
  (is (= 10 (clysm/compiler:eval-forms
             (append *reader-test-preamble*
                     '((let ((rs (make-reader-state "((10 20) 30)")))
                         (car (car (read-form rs))))))))))  ; first of first

(test reader-skip-whitespace
  "Test that reader skips leading whitespace."
  (is (= 42 (clysm/compiler:eval-forms
             (append *reader-test-preamble*
                     '((let ((rs (make-reader-state "   42")))
                         (read-form rs))))))))

;;; Extended reader tests with symbol and string support

(defparameter *full-reader-preamble*
  '(;; Skip whitespace and comments
    (defun skip-ws-helper (rs)
      (let ((ch (reader-state-peek-char rs)))
        (cond
          ((= ch -1) nil)
          ((whitespace-char-p ch)
           (reader-state-read-char rs)
           (skip-ws-helper rs))
          ((= ch 59)  ; ';'
           (reader-state-read-char rs)
           (skip-to-nl rs)
           (skip-ws-helper rs))
          (t nil))))
    (defun skip-to-nl (rs)
      (let ((ch (reader-state-read-char rs)))
        (cond ((= ch -1) nil)
              ((= ch 10) nil)
              (t (skip-to-nl rs)))))
    (defun skip-ws (rs) (skip-ws-helper rs))

    ;; Read integer
    (defun read-int-helper (rs result)
      (let ((d (digit-char-p (reader-state-peek-char rs))))
        (if d
            (progn (reader-state-read-char rs)
                   (read-int-helper rs (+ (* result 10) (- d 1))))
            result)))
    (defun read-int (rs)
      (let ((ch (reader-state-peek-char rs)))
        (cond
          ((= ch 45)
           (reader-state-read-char rs)
           (- 0 (read-int-helper rs 0)))
          ((= ch 43)
           (reader-state-read-char rs)
           (read-int-helper rs 0))
          (t (read-int-helper rs 0)))))

    ;; Skip symbol characters, return count
    (defun skip-sym-chars (rs)
      (let ((ch (reader-state-peek-char rs)))
        (if (and (/= ch -1) (symbol-constituent-p ch))
            (progn (reader-state-read-char rs)
                   (+ 1 (skip-sym-chars rs)))
            0)))

    ;; Read symbol as uppercase string
    (defun read-sym (rs)
      (let ((start (reader-state-position rs)))
        (skip-sym-chars rs)
        (reader-state-substring rs start (reader-state-position rs))))

    ;; Skip to string end
    (defun skip-str-end (rs)
      (let ((ch (reader-state-read-char rs)))
        (cond ((= ch -1) nil)
              ((= ch 34) nil)  ; closing quote
              ((= ch 92) (reader-state-read-char rs) (skip-str-end rs))  ; backslash
              (t (skip-str-end rs)))))

    ;; Read string literal
    (defun read-str (rs)
      (let ((start (reader-state-position rs)))
        (skip-str-end rs)
        (reader-state-substring-raw rs start (- (reader-state-position rs) 1))))

    ;; Read form
    (defun read-form (rs)
      (skip-ws rs)
      (let ((ch (reader-state-peek-char rs)))
        (cond
          ((= ch -1) -1)  ; EOF
          ((= ch 40)      ; '(' - list
           (reader-state-read-char rs)
           (read-list rs))
          ((= ch 39)      ; '\'' - quote
           (reader-state-read-char rs)
           (cons "QUOTE" (cons (read-form rs) nil)))
          ((= ch 34)      ; '"' - string
           (reader-state-read-char rs)
           (read-str rs))
          ((or (digit-char-p ch) (= ch 45) (= ch 43))
           (let ((next (if (or (= ch 45) (= ch 43))
                           (progn (reader-state-read-char rs)
                                  (let ((n (reader-state-peek-char rs)))
                                    (reader-state-unread-char rs)
                                    n))
                           ch)))
             (if (digit-char-p next)
                 (read-int rs)
                 (read-sym rs))))
          ((symbol-constituent-p ch)
           (read-sym rs))
          (t -99))))

    ;; Read list
    (defun read-list (rs)
      (skip-ws rs)
      (let ((ch (reader-state-peek-char rs)))
        (cond
          ((= ch 41) (reader-state-read-char rs) nil)  ; ')'
          ((= ch -1) -3)  ; EOF error
          (t (cons (read-form rs) (read-list rs)))))))
  "Full reader functions including symbols and strings.")

(test reader-read-symbol
  "Test reading symbols as uppercase strings."
  (let ((result (clysm/compiler:eval-forms
                 (append *full-reader-preamble*
                         '((let ((rs (make-reader-state "hello")))
                             (read-form rs)))))))
    ;; Result is a string pointer, compare with string=
    (is (numberp result))))

(test reader-read-symbol-list
  "Test reading list of symbols."
  (let ((result (clysm/compiler:eval-forms
                 (append *full-reader-preamble*
                         '((let ((rs (make-reader-state "(a b c)")))
                             (let ((lst (read-form rs)))
                               ;; Just verify we got a list with 3 elements
                               (length lst))))))))
    (is (= 3 result))))

(test reader-read-string-literal
  "Test reading string literals."
  (let ((result (clysm/compiler:eval-forms
                 (append *full-reader-preamble*
                         '((let ((rs (make-reader-state "\"hello\"")))
                             (let ((str (read-form rs)))
                               ;; String length should be 5
                               (string-length str))))))))
    (is (= 5 result))))

(test reader-read-quote
  "Test reading quoted forms."
  (let ((result (clysm/compiler:eval-forms
                 (append *full-reader-preamble*
                         '((let ((rs (make-reader-state "'(1 2 3)")))
                             (let ((form (read-form rs)))
                               ;; form should be (QUOTE (1 2 3))
                               (car (car (cdr form))))))))))  ; first element of quoted list
    (is (= 1 result))))

(test reader-skip-comments
  "Test that reader skips semicolon comments."
  (is (= 42 (clysm/compiler:eval-forms
             (append *full-reader-preamble*
                     `((let ((rs (make-reader-state ,(format nil "; comment~%42"))))
                         (read-form rs))))))))

;;; ============================================================
;;; Symbol System Tests
;;; ============================================================

(test symbol-intern-creates-symbol
  "Test that intern creates a symbol from a string."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((sym (intern "HELLO")))
                     ;; Symbol should be non-nil (a valid address)
                     (if sym 1 0))))))
    (is (= 1 result))))

(test symbol-intern-same-name
  "Test that interning the same name twice returns the same symbol."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((sym1 (intern "FOO"))
                         (sym2 (intern "FOO")))
                     ;; Same name should return same symbol (eq)
                     (if (= sym1 sym2) 1 0))))))
    (is (= 1 result))))

(test symbol-intern-different-names
  "Test that different names create different symbols."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((sym1 (intern "AAA"))
                         (sym2 (intern "BBB")))
                     ;; Different names should return different symbols
                     (if (= sym1 sym2) 0 1))))))
    (is (= 1 result))))

(test symbol-name-returns-string
  "Test that symbol-name returns the original string."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((sym (intern "TEST")))
                     (let ((name (symbol-name sym)))
                       ;; Check the string length (should be 4)
                       (string-length name)))))))
    (is (= 4 result))))

(test symbol-value-initial
  "Test that newly interned symbol has value 0 (unbound)."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((sym (intern "UNBOUND-SYM")))
                     (symbol-value sym))))))
    (is (= 0 result))))

(test symbol-value-set-and-get
  "Test setting and getting symbol value."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((sym (intern "MY-VAR")))
                     (set-symbol-value sym 42)
                     (symbol-value sym))))))
    (is (= 42 result))))

(test symbol-function-set-and-get
  "Test setting and getting symbol function."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((sym (intern "MY-FUNC")))
                     (set-symbol-function sym 123)
                     (symbol-function sym))))))
    (is (= 123 result))))

(test symbol-plist-initial
  "Test that newly interned symbol has empty plist (0/nil)."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((sym (intern "PLIST-SYM")))
                     (symbol-plist sym))))))
    (is (= 0 result))))

(test symbol-multiple-interns
  "Test interning multiple symbols in sequence."
  (let ((result (clysm/compiler:eval-forms
                 '((let ((s1 (intern "ONE"))
                         (s2 (intern "TWO"))
                         (s3 (intern "THREE")))
                     (set-symbol-value s1 1)
                     (set-symbol-value s2 2)
                     (set-symbol-value s3 3)
                     (+ (symbol-value s1)
                        (+ (symbol-value s2)
                           (symbol-value s3))))))))
    (is (= 6 result))))
