;;;; io-usage-test.lisp - Unit tests for I/O usage analyzer
;;;;
;;;; Feature: 022-wasm-import-optimization
;;;; Tasks: T004-T008

(in-package #:clysm/tests/unit/io-usage)

;;; ==========================================================================
;;; T004: analyze-io-usage returns NIL for (+ 1 2)
;;; ==========================================================================

(deftest analyze-io-usage-addition-no-io
  (testing "analyze-io-usage returns NIL for simple arithmetic (+ 1 2)"
    (ok (null (analyze-io-usage '(+ 1 2)))
        "(+ 1 2) should not be detected as using I/O")))

;;; ==========================================================================
;;; T005: analyze-io-usage returns NIL for (* 7 6)
;;; ==========================================================================

(deftest analyze-io-usage-multiplication-no-io
  (testing "analyze-io-usage returns NIL for simple arithmetic (* 7 6)"
    (ok (null (analyze-io-usage '(* 7 6)))
        "(* 7 6) should not be detected as using I/O")))

;;; ==========================================================================
;;; T006: analyze-io-usage returns T for (print "hello")
;;; ==========================================================================

(deftest analyze-io-usage-print-uses-io
  (testing "analyze-io-usage returns T for (print \"hello\")"
    (ok (analyze-io-usage '(print "hello"))
        "(print \"hello\") should be detected as using I/O")))

;;; ==========================================================================
;;; T007: analyze-io-usage returns T for (format t "~A" x)
;;; ==========================================================================

(deftest analyze-io-usage-format-uses-io
  (testing "analyze-io-usage returns T for (format t \"~A\" x)"
    (ok (analyze-io-usage '(format t "~A" x))
        "(format t \"~A\" x) should be detected as using I/O")))

;;; ==========================================================================
;;; T008: *io-function-names* contains all required I/O functions
;;; ==========================================================================

(deftest io-function-names-completeness
  (testing "*io-function-names* contains all required I/O functions"
    ;; Output functions
    (ok (member "WRITE-CHAR" *io-function-names* :test #'string=)
        "WRITE-CHAR should be in *io-function-names*")
    (ok (member "WRITE-STRING" *io-function-names* :test #'string=)
        "WRITE-STRING should be in *io-function-names*")
    (ok (member "WRITE-BYTE" *io-function-names* :test #'string=)
        "WRITE-BYTE should be in *io-function-names*")
    (ok (member "WRITE-LINE" *io-function-names* :test #'string=)
        "WRITE-LINE should be in *io-function-names*")
    (ok (member "TERPRI" *io-function-names* :test #'string=)
        "TERPRI should be in *io-function-names*")
    (ok (member "FRESH-LINE" *io-function-names* :test #'string=)
        "FRESH-LINE should be in *io-function-names*")
    (ok (member "PRINT" *io-function-names* :test #'string=)
        "PRINT should be in *io-function-names*")
    (ok (member "PRIN1" *io-function-names* :test #'string=)
        "PRIN1 should be in *io-function-names*")
    (ok (member "PRINC" *io-function-names* :test #'string=)
        "PRINC should be in *io-function-names*")
    (ok (member "PPRINT" *io-function-names* :test #'string=)
        "PPRINT should be in *io-function-names*")
    (ok (member "FORMAT" *io-function-names* :test #'string=)
        "FORMAT should be in *io-function-names*")
    (ok (member "WRITE" *io-function-names* :test #'string=)
        "WRITE should be in *io-function-names*")
    ;; Input functions
    (ok (member "READ-CHAR" *io-function-names* :test #'string=)
        "READ-CHAR should be in *io-function-names*")
    (ok (member "READ-LINE" *io-function-names* :test #'string=)
        "READ-LINE should be in *io-function-names*")
    (ok (member "READ-BYTE" *io-function-names* :test #'string=)
        "READ-BYTE should be in *io-function-names*")
    (ok (member "PEEK-CHAR" *io-function-names* :test #'string=)
        "PEEK-CHAR should be in *io-function-names*")
    (ok (member "READ" *io-function-names* :test #'string=)
        "READ should be in *io-function-names*")
    ;; Stream operations
    (ok (member "FORCE-OUTPUT" *io-function-names* :test #'string=)
        "FORCE-OUTPUT should be in *io-function-names*")
    (ok (member "FINISH-OUTPUT" *io-function-names* :test #'string=)
        "FINISH-OUTPUT should be in *io-function-names*")
    (ok (member "CLEAR-INPUT" *io-function-names* :test #'string=)
        "CLEAR-INPUT should be in *io-function-names*")
    (ok (member "READ-SEQUENCE" *io-function-names* :test #'string=)
        "READ-SEQUENCE should be in *io-function-names*")
    (ok (member "WRITE-SEQUENCE" *io-function-names* :test #'string=)
        "WRITE-SEQUENCE should be in *io-function-names*")))

;;; ==========================================================================
;;; Additional edge case tests for analyze-io-usage
;;; ==========================================================================

(deftest analyze-io-usage-nested-io
  (testing "analyze-io-usage detects I/O in nested forms"
    (ok (analyze-io-usage '(let ((x 1)) (print x)))
        "I/O inside let should be detected")
    (ok (analyze-io-usage '(if t (format t "yes") (format t "no")))
        "I/O inside if branches should be detected")))

(deftest analyze-io-usage-quoted-forms
  (testing "analyze-io-usage ignores quoted forms"
    (ok (null (analyze-io-usage '(quote (print "hello"))))
        "Quoted print should not be detected as I/O")
    (ok (null (analyze-io-usage ''(print "hello")))
        "Quote-quoted print should not be detected as I/O")))

(deftest analyze-io-usage-function-references
  (testing "analyze-io-usage ignores function references"
    (ok (null (analyze-io-usage '(function print)))
        "#'print should not be detected as I/O call")
    (ok (null (analyze-io-usage '#'format))
        "#'format should not be detected as I/O call")))

(deftest analyze-io-usage-atoms
  (testing "analyze-io-usage handles atoms correctly"
    (ok (null (analyze-io-usage 42))
        "Number atom should return NIL")
    (ok (null (analyze-io-usage 'x))
        "Symbol atom should return NIL")
    (ok (null (analyze-io-usage "hello"))
        "String atom should return NIL")))

;;; ==========================================================================
;;; T053: Regression tests for io-usage.lisp compatibility
;;; Feature: 001-ffi-import-architecture
;;; Purpose: Ensure io-usage analyzer works correctly alongside ffi-usage analyzer
;;; ==========================================================================

(deftest io-usage-regression-progn-forms
  "T053: Regression test for progn with I/O (compatibility check)."
  (testing "progn with I/O is detected"
    (ok (analyze-io-usage '(progn (+ 1 2) (print "result")))
        "progn containing print should detect I/O"))
  (testing "progn without I/O returns nil"
    (ok (null (analyze-io-usage '(progn (+ 1 2) (* 3 4))))
        "progn with only arithmetic should return NIL")))

(deftest io-usage-regression-let-bindings
  "T053: Regression test for let forms with I/O (compatibility check)."
  (testing "let with I/O in body is detected"
    (ok (analyze-io-usage '(let ((x 1) (y 2)) (format t "~A ~A" x y)))
        "let with format in body should detect I/O"))
  (testing "let with I/O in init-forms is detected"
    (ok (analyze-io-usage '(let ((x (print 1))) x))
        "let with print in init-form should detect I/O"))
  (testing "let* with nested I/O is detected"
    (ok (analyze-io-usage '(let* ((x 1) (y (write-char #\a))) y))
        "let* with write-char should detect I/O")))

(deftest io-usage-regression-lambda-forms
  "T053: Regression test for lambda forms with I/O (compatibility check)."
  (testing "lambda with I/O in body is detected"
    (ok (analyze-io-usage '(lambda (x) (print x)))
        "lambda with print should detect I/O"))
  (testing "funcall of lambda with I/O is detected"
    (ok (analyze-io-usage '(funcall (lambda (x) (format t "~A" x)) 42))
        "funcall of lambda with format should detect I/O")))

(deftest io-usage-regression-flet-labels
  "T053: Regression test for flet/labels with I/O (compatibility check)."
  (testing "flet with I/O in local function is detected"
    (ok (analyze-io-usage '(flet ((f (x) (print x))) (f 42)))
        "flet with print should detect I/O"))
  (testing "labels with I/O in local function is detected"
    (ok (analyze-io-usage '(labels ((f (x) (format t "~A" x))) (f 42)))
        "labels with format should detect I/O")))

(deftest io-usage-regression-conditional-io
  "T053: Regression test for conditional I/O (compatibility check)."
  (testing "if with I/O in then branch"
    (ok (analyze-io-usage '(if condition (print "yes") nil))
        "if with print in then should detect I/O"))
  (testing "if with I/O in else branch"
    (ok (analyze-io-usage '(if condition nil (print "no")))
        "if with print in else should detect I/O"))
  (testing "cond with I/O"
    (ok (analyze-io-usage '(cond (t (format t "result"))))
        "cond with format should detect I/O"))
  (testing "when with I/O"
    (ok (analyze-io-usage '(when condition (print "executed")))
        "when with print should detect I/O")))

(deftest io-usage-ffi-usage-independence
  "T053: Verify io-usage analyzer is independent of ffi-usage analyzer."
  (testing "I/O functions detected regardless of FFI analysis"
    ;; These are I/O functions that should be detected by io-usage
    ;; even though they may also be in the FFI environment
    (ok (analyze-io-usage '(write-char #\a))
        "write-char should be detected by io-usage")
    (ok (analyze-io-usage '(read-char))
        "read-char should be detected by io-usage")
    (ok (analyze-io-usage '(terpri))
        "terpri should be detected by io-usage"))
  (testing "Non-I/O FFI functions are not detected"
    ;; Math functions may be FFI but are not I/O
    (ok (null (analyze-io-usage '(sin 1.0)))
        "sin is FFI but not I/O - should return NIL")
    (ok (null (analyze-io-usage '(cos 0.0)))
        "cos is FFI but not I/O - should return NIL")))
