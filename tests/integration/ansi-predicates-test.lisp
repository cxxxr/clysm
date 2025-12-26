;;;; ansi-predicates-test.lisp - Integration tests for ANSI CL predicates
;;;; Feature: 023-type-predicates

(in-package #:clysm/tests/integration/ansi-predicates)

;;; Integration tests verify end-to-end behavior via wasmtime execution.
;;; Each test compiles a Lisp expression, runs it via wasmtime, and verifies the result.

;;; ============================================================
;;; Type Predicates - Full Integration
;;; ============================================================

(deftest ansi-integerp-integration
  (let ((result-fixnum (clysm/tests:compile-and-run '(integerp 42)))
        (result-float (clysm/tests:compile-and-run '(integerp 3.14))))
    (ok (eql t result-fixnum) "integerp fixnum via wasmtime")
    (ok (null result-float) "integerp float via wasmtime")))

(deftest ansi-numberp-integration
  (let ((result-int (clysm/tests:compile-and-run '(numberp 42)))
        (result-float (clysm/tests:compile-and-run '(numberp 3.14)))
        (result-ratio (clysm/tests:compile-and-run '(numberp 2/3)))
        (result-complex (clysm/tests:compile-and-run '(numberp #C(1 2))))
        (result-symbol (clysm/tests:compile-and-run '(numberp 'foo))))
    (ok (eql t result-int) "numberp integer via wasmtime")
    (ok (eql t result-float) "numberp float via wasmtime")
    (ok (eql t result-ratio) "numberp ratio via wasmtime")
    (ok (eql t result-complex) "numberp complex via wasmtime")
    (ok (null result-symbol) "numberp symbol via wasmtime")))

(deftest ansi-symbolp-integration
  (let ((result-symbol (clysm/tests:compile-and-run '(symbolp 'foo)))
        (result-nil (clysm/tests:compile-and-run '(symbolp nil)))
        (result-number (clysm/tests:compile-and-run '(symbolp 42))))
    (ok (eql t result-symbol) "symbolp symbol via wasmtime")
    (ok (eql t result-nil) "symbolp nil via wasmtime (NIL is a symbol)")
    (ok (null result-number) "symbolp number via wasmtime")))

(deftest ansi-functionp-integration
  (let ((result-fn (clysm/tests:compile-and-run '(functionp #'car)))
        (result-lambda (clysm/tests:compile-and-run '(functionp (lambda (x) x))))
        (result-symbol (clysm/tests:compile-and-run '(functionp 'car))))
    (ok (eql t result-fn) "functionp function via wasmtime")
    (ok (eql t result-lambda) "functionp lambda via wasmtime")
    (ok (null result-symbol) "functionp symbol via wasmtime")))

;;; ============================================================
;;; Numeric Predicates - Full Integration
;;; ============================================================

(deftest ansi-zerop-integration
  (let ((result-zero (clysm/tests:compile-and-run '(zerop 0)))
        (result-nonzero (clysm/tests:compile-and-run '(zerop 1)))
        (result-float-zero (clysm/tests:compile-and-run '(zerop 0.0)))
        (result-neg-zero (clysm/tests:compile-and-run '(zerop -0.0))))
    (ok (eql t result-zero) "zerop 0 via wasmtime")
    (ok (null result-nonzero) "zerop 1 via wasmtime")
    (ok (eql t result-float-zero) "zerop 0.0 via wasmtime")
    (ok (eql t result-neg-zero) "zerop -0.0 via wasmtime")))

(deftest ansi-plusp-minusp-integration
  (let ((plusp-pos (clysm/tests:compile-and-run '(plusp 5)))
        (plusp-neg (clysm/tests:compile-and-run '(plusp -3)))
        (plusp-zero (clysm/tests:compile-and-run '(plusp 0)))
        (minusp-neg (clysm/tests:compile-and-run '(minusp -5)))
        (minusp-pos (clysm/tests:compile-and-run '(minusp 3)))
        (minusp-zero (clysm/tests:compile-and-run '(minusp 0))))
    (ok (eql t plusp-pos) "plusp 5 via wasmtime")
    (ok (null plusp-neg) "plusp -3 via wasmtime")
    (ok (null plusp-zero) "plusp 0 via wasmtime")
    (ok (eql t minusp-neg) "minusp -5 via wasmtime")
    (ok (null minusp-pos) "minusp 3 via wasmtime")
    (ok (null minusp-zero) "minusp 0 via wasmtime")))

(deftest ansi-oddp-evenp-integration
  (let ((oddp-odd (clysm/tests:compile-and-run '(oddp 7)))
        (oddp-even (clysm/tests:compile-and-run '(oddp 8)))
        (evenp-even (clysm/tests:compile-and-run '(evenp 8)))
        (evenp-odd (clysm/tests:compile-and-run '(evenp 7)))
        (evenp-zero (clysm/tests:compile-and-run '(evenp 0))))
    (ok (eql t oddp-odd) "oddp 7 via wasmtime")
    (ok (null oddp-even) "oddp 8 via wasmtime")
    (ok (eql t evenp-even) "evenp 8 via wasmtime")
    (ok (null evenp-odd) "evenp 7 via wasmtime")
    (ok (eql t evenp-zero) "evenp 0 via wasmtime")))

;;; ============================================================
;;; Signum - Full Integration
;;; ============================================================

(deftest ansi-signum-integration
  (let ((neg-int (clysm/tests:compile-and-run '(signum -42)))
        (zero-int (clysm/tests:compile-and-run '(signum 0)))
        (pos-int (clysm/tests:compile-and-run '(signum 100)))
        (neg-float (clysm/tests:compile-and-run '(signum -3.14)))
        (zero-float (clysm/tests:compile-and-run '(signum 0.0)))
        (pos-float (clysm/tests:compile-and-run '(signum 2.5))))
    (ok (eql -1 neg-int) "signum -42 via wasmtime")
    (ok (eql 0 zero-int) "signum 0 via wasmtime")
    (ok (eql 1 pos-int) "signum 100 via wasmtime")
    (ok (eql -1.0 neg-float) "signum -3.14 via wasmtime")
    (ok (eql 0.0 zero-float) "signum 0.0 via wasmtime")
    (ok (eql 1.0 pos-float) "signum 2.5 via wasmtime")))
