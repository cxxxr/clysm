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
