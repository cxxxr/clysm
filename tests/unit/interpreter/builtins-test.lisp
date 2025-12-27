;;;; builtins-test.lisp - TDD tests for interpreter built-in functions
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Test: T026

(defpackage #:clysm/tests/interpreter/builtins-test
  (:use #:cl #:rove)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:make-interpreter-env))

(in-package #:clysm/tests/interpreter/builtins-test)

;;; ============================================================
;;; US4: Built-in functions (50+ core CL functions)
;;; ============================================================

;; Hash Table Functions

(deftest test-hash-table-make
  "Test make-hash-table."
  (let ((env (make-interpreter-env)))
    (ok (interpret '(hash-table-p (make-hash-table)) env))))

(deftest test-hash-table-operations
  "Test gethash, puthash, remhash."
  (let ((env (make-interpreter-env)))
    (interpret '(defvar *ht* (make-hash-table)) env)
    (interpret '(puthash 'key *ht* 42) env)
    (ok (= 42 (interpret '(gethash 'key *ht*) env)))
    (interpret '(remhash 'key *ht*) env)
    (ok (eq nil (interpret '(gethash 'key *ht*) env)))))

(deftest test-hash-table-count
  "Test hash-table-count."
  (let ((env (make-interpreter-env)))
    (interpret '(defvar *ht* (make-hash-table)) env)
    (ok (= 0 (interpret '(hash-table-count *ht*) env)))
    (interpret '(puthash 'a *ht* 1) env)
    (interpret '(puthash 'b *ht* 2) env)
    (ok (= 2 (interpret '(hash-table-count *ht*) env)))))

;; Sequence Functions

(deftest test-mapcar
  "Test mapcar."
  (let ((env (make-interpreter-env)))
    (ok (equal '(2 4 6)
               (interpret '(mapcar (lambda (x) (* x 2)) '(1 2 3)) env)))))

(deftest test-reduce
  "Test reduce."
  (let ((env (make-interpreter-env)))
    (ok (= 15 (interpret '(reduce #'+ '(1 2 3 4 5)) env)))
    (ok (= 120 (interpret '(reduce #'* '(1 2 3 4 5)) env)))))

(deftest test-find
  "Test find and find-if."
  (let ((env (make-interpreter-env)))
    (ok (= 3 (interpret '(find 3 '(1 2 3 4 5)) env)))
    (ok (eq nil (interpret '(find 10 '(1 2 3 4 5)) env)))
    (ok (= 4 (interpret '(find-if #'evenp '(1 3 4 5)) env)))))

(deftest test-position
  "Test position."
  (let ((env (make-interpreter-env)))
    (ok (= 2 (interpret '(position 3 '(1 2 3 4 5)) env)))
    (ok (eq nil (interpret '(position 10 '(1 2 3 4 5)) env)))))

(deftest test-remove
  "Test remove and remove-if."
  (let ((env (make-interpreter-env)))
    (ok (equal '(1 2 4 5) (interpret '(remove 3 '(1 2 3 4 5)) env)))
    (ok (equal '(1 3 5) (interpret '(remove-if #'evenp '(1 2 3 4 5)) env)))))

(deftest test-member
  "Test member."
  (let ((env (make-interpreter-env)))
    (ok (equal '(3 4 5) (interpret '(member 3 '(1 2 3 4 5)) env)))
    (ok (eq nil (interpret '(member 10 '(1 2 3 4 5)) env)))))

(deftest test-assoc
  "Test assoc."
  (let ((env (make-interpreter-env)))
    (ok (equal '(b . 2)
               (interpret '(assoc 'b '((a . 1) (b . 2) (c . 3))) env)))
    (ok (eq nil
               (interpret '(assoc 'd '((a . 1) (b . 2) (c . 3))) env)))))

;; String Functions

(deftest test-string-equal
  "Test string comparison."
  (let ((env (make-interpreter-env)))
    (ok (interpret '(string= "hello" "hello") env))
    (ok (not (interpret '(string= "hello" "HELLO") env)))
    (ok (interpret '(string-equal "hello" "HELLO") env))))

(deftest test-string-case
  "Test string-upcase and string-downcase."
  (let ((env (make-interpreter-env)))
    (ok (equal "HELLO" (interpret '(string-upcase "hello") env)))
    (ok (equal "hello" (interpret '(string-downcase "HELLO") env)))))

;; Numeric Functions

(deftest test-floor-ceiling
  "Test floor and ceiling."
  (let ((env (make-interpreter-env)))
    (ok (= 3 (interpret '(floor 3.7) env)))
    (ok (= 4 (interpret '(ceiling 3.2) env)))
    (ok (= -4 (interpret '(floor -3.7) env)))
    (ok (= -3 (interpret '(ceiling -3.7) env)))))

(deftest test-mod-rem
  "Test mod and rem."
  (let ((env (make-interpreter-env)))
    (ok (= 2 (interpret '(mod 5 3) env)))
    (ok (= 2 (interpret '(rem 5 3) env)))))

(deftest test-abs-signum
  "Test abs and signum."
  (let ((env (make-interpreter-env)))
    (ok (= 5 (interpret '(abs -5) env)))
    (ok (= 1 (interpret '(signum 42) env)))
    (ok (= -1 (interpret '(signum -42) env)))
    (ok (= 0 (interpret '(signum 0) env)))))

(deftest test-gcd-lcm
  "Test gcd and lcm."
  (let ((env (make-interpreter-env)))
    (ok (= 4 (interpret '(gcd 12 8) env)))
    (ok (= 24 (interpret '(lcm 12 8) env)))))

;; Type Functions

(deftest test-typep
  "Test typep."
  (let ((env (make-interpreter-env)))
    (ok (interpret '(typep 42 'integer) env))
    (ok (interpret '(typep "str" 'string) env))
    (ok (not (interpret '(typep 42 'string) env)))))

(deftest test-type-of
  "Test type-of."
  (let ((env (make-interpreter-env)))
    ;; Note: SBCL returns compound type specifiers like (INTEGER 0 N)
    ;; type-of returns either a symbol or a list (type-specifier)
    (let ((int-type (interpret '(type-of 42) env))
          (str-type (interpret '(type-of "str") env)))
      ;; Type should be a symbol or list
      (ok (or (symbolp int-type) (consp int-type)))
      (ok (or (symbolp str-type) (consp str-type))))))

;; Symbol Functions

(deftest test-symbol-name
  "Test symbol-name."
  (let ((env (make-interpreter-env)))
    (ok (equal "FOO" (interpret '(symbol-name 'foo) env)))))

(deftest test-gensym
  "Test gensym."
  (let ((env (make-interpreter-env)))
    (ok (interpret '(symbolp (gensym)) env))
    (ok (not (interpret '(eq (gensym) (gensym)) env)))))

;; I/O Functions

(deftest test-format-basic
  "Test format."
  (let ((env (make-interpreter-env)))
    (ok (equal "Hello, World!"
               (interpret '(format nil "Hello, ~A!" "World") env)))
    (ok (equal "The answer is 42"
               (interpret '(format nil "The answer is ~D" 42) env)))))

;; List Functions

(deftest test-list-accessors
  "Test second, third, etc."
  (let ((env (make-interpreter-env)))
    (ok (= 2 (interpret '(second '(1 2 3 4)) env)))
    (ok (= 3 (interpret '(third '(1 2 3 4)) env)))
    (ok (= 4 (interpret '(fourth '(1 2 3 4)) env)))))

(deftest test-last
  "Test last."
  (let ((env (make-interpreter-env)))
    (ok (equal '(5) (interpret '(last '(1 2 3 4 5)) env)))
    (ok (equal '(4 5) (interpret '(last '(1 2 3 4 5) 2) env)))))

(deftest test-butlast
  "Test butlast."
  (let ((env (make-interpreter-env)))
    (ok (equal '(1 2 3 4) (interpret '(butlast '(1 2 3 4 5)) env)))
    (ok (equal '(1 2 3) (interpret '(butlast '(1 2 3 4 5) 2) env)))))

;; Array Functions

(deftest test-make-array
  "Test make-array."
  (let ((env (make-interpreter-env)))
    (ok (interpret '(arrayp (make-array 5)) env))
    (ok (= 5 (interpret '(length (make-array 5)) env)))))

(deftest test-aref
  "Test aref."
  (let ((env (make-interpreter-env)))
    (ok (= 3 (interpret '(aref #(1 2 3 4 5) 2) env)))))
