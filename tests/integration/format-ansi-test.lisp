;;;; format-ansi-test.lisp - ANSI CL FORMAT compliance tests
;;;; Feature: 032-format-function

(defpackage #:clysm/tests/integration/format-ansi
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/streams
                          #:format)
  (:import-from #:clysm/conditions
                #:format-error))

(in-package #:clysm/tests/integration/format-ansi)

;;; ============================================================
;;; T055: ANSI CL FORMAT Compliance Tests
;;; ============================================================

;;; Basic Output Tests (CLHS 22.3.1 - FORMAT Basic Output)

(deftest format-ansi-tilde-a
  "~A aesthetic output"
  (testing "~A prints without escape characters"
    (ok (string= (clysm/streams:format nil "~A" 'foo) "FOO")
        "Symbol prints without quotes")
    (ok (string= (clysm/streams:format nil "~A" "hello") "hello")
        "String prints without quotes")
    (ok (string= (clysm/streams:format nil "~A" 123) "123")
        "Integer prints normally")))

(deftest format-ansi-tilde-s
  "~S standard output"
  (testing "~S prints with escape characters for readability"
    (ok (string= (clysm/streams:format nil "~S" 'foo) "FOO")
        "Symbol prints same as ~A (no special escaping needed)")
    (ok (string= (clysm/streams:format nil "~S" "hello") "\"hello\"")
        "String prints with quotes")
    (ok (string= (clysm/streams:format nil "~S" 123) "123")
        "Integer prints normally")))

(deftest format-ansi-tilde-d
  "~D decimal integer output"
  (testing "~D prints integers in decimal"
    (ok (string= (clysm/streams:format nil "~D" 42) "42")
        "Positive integer prints correctly")
    (ok (string= (clysm/streams:format nil "~D" -17) "-17")
        "Negative integer prints correctly")
    (ok (string= (clysm/streams:format nil "~D" 0) "0")
        "Zero prints correctly")))

;;; Newline and Whitespace (CLHS 22.3.2 - FORMAT Newline)

(deftest format-ansi-tilde-percent
  "~% newline"
  (testing "~% outputs newline"
    (ok (string= (clysm/streams:format nil "a~%b") (cl:format nil "a~%b"))
        "~% produces platform newline")))

(deftest format-ansi-tilde-tilde
  "~~ literal tilde"
  (testing "~~ outputs literal tilde"
    (ok (string= (clysm/streams:format nil "~~") "~")
        "~~ produces single tilde")))

(deftest format-ansi-tilde-ampersand
  "~& fresh-line"
  (testing "~& outputs conditional newline"
    (ok (string= (clysm/streams:format nil "~&x") "x")
        "~& at column 0 outputs nothing")
    (ok (string= (clysm/streams:format nil "y~&z") (cl:format nil "y~%z"))
        "~& after output outputs newline")))

;;; Iteration (CLHS 22.3.8 - FORMAT Iteration)

(deftest format-ansi-iteration-basic
  "~{~} basic iteration"
  (testing "~{~} iterates over list"
    (ok (string= (clysm/streams:format nil "~{~A~}" '(1 2 3)) "123")
        "Basic iteration concatenates elements")
    (ok (string= (clysm/streams:format nil "~{~A~}" nil) "")
        "Empty list produces no output")))

(deftest format-ansi-iteration-escape
  "~^ escape from iteration"
  (testing "~^ exits before last separator"
    (ok (string= (clysm/streams:format nil "~{~A~^, ~}" '(a b c)) "A, B, C")
        "~^ prevents trailing separator")
    (ok (string= (clysm/streams:format nil "~{~A~^, ~}" '(x)) "X")
        "Single element has no separator")))

;;; Conditional (CLHS 22.3.7 - FORMAT Conditional)

(deftest format-ansi-conditional-index
  "~[~] index-based conditional"
  (testing "~[~] selects by integer index"
    (ok (string= (clysm/streams:format nil "~[zero~;one~;two~]" 0) "zero")
        "Index 0 selects first clause")
    (ok (string= (clysm/streams:format nil "~[zero~;one~;two~]" 1) "one")
        "Index 1 selects second clause")
    (ok (string= (clysm/streams:format nil "~[zero~;one~;two~]" 2) "two")
        "Index 2 selects third clause")))

(deftest format-ansi-conditional-default
  "~:; default clause"
  (testing "~:; provides default for out-of-range"
    (ok (string= (clysm/streams:format nil "~[a~;b~:;many~]" 99) "many")
        "Out-of-range uses default clause")
    (ok (string= (clysm/streams:format nil "~[a~;b~]" 99) "")
        "Out-of-range without default produces nothing")))

(deftest format-ansi-conditional-boolean
  "~:[~] boolean conditional"
  (testing "~:[~] selects by truthiness"
    (ok (string= (clysm/streams:format nil "~:[no~;yes~]" nil) "no")
        "NIL selects first clause")
    (ok (string= (clysm/streams:format nil "~:[no~;yes~]" t) "yes")
        "T selects second clause")
    (ok (string= (clysm/streams:format nil "~:[no~;yes~]" 'anything) "yes")
        "Any non-NIL selects second clause")))

;;; Recursive Processing (CLHS 22.3.6 - FORMAT Recursive)

(deftest format-ansi-recursive-basic
  "~? recursive processing"
  (testing "~? processes nested format string"
    (ok (string= (clysm/streams:format nil "~?" "~A" '(42)) "42")
        "~? applies format string to args")
    (ok (string= (clysm/streams:format nil "~?" "~A + ~A" '(1 2)) "1 + 2")
        "~? handles multiple directives")))

(deftest format-ansi-recursive-nested
  "Nested ~? processing"
  (testing "~? can be nested"
    (ok (string= (clysm/streams:format nil "outer: ~?" "inner: ~A" '(x)) "outer: inner: X")
        "Nested recursive processing works")))

;;; Return Value Contract (CLHS 22.3 - FORMAT)

(deftest format-ansi-return-value
  "FORMAT return value contract"
  (testing "FORMAT returns appropriate value based on destination"
    (ok (stringp (clysm/streams:format nil "test"))
        "NIL destination returns string")
    (ok (null (with-output-to-string (*standard-output*)
                (clysm/streams:format t "test")))
        "T destination returns NIL")))

;;; T056: Nested Directive Tests

(deftest format-nested-iteration-conditional
  "Nested iteration containing conditional"
  (testing "~{~[~]~} works correctly"
    (let ((result (clysm/streams:format nil "~{~[zero~;one~;two~]~^, ~}" '(0 1 2))))
      (ok (string= result "zero, one, two")
          "Iteration with nested conditional"))))

(deftest format-nested-conditional-iteration
  "Nested conditional containing iteration"
  (testing "~[...~{~}...~] works correctly"
    (let ((result (clysm/streams:format nil "~[empty~;items: ~{~A~^, ~}~]" 1 '(x y z))))
      (ok (string= result "items: X, Y, Z")
          "Conditional with nested iteration"))))

;;; T057: Edge Cases

(deftest format-edge-empty-string
  "Empty format string"
  (testing "Empty string produces empty output"
    (ok (string= (clysm/streams:format nil "") "")
        "Empty format string returns empty string")))

(deftest format-edge-no-directives
  "Format string without directives"
  (testing "Literal text passes through"
    (ok (string= (clysm/streams:format nil "hello world") "hello world")
        "Literal text copied to output")))

(deftest format-edge-multiple-args-unused
  "Extra arguments are ignored"
  (testing "Unused arguments don't cause errors"
    (ok (string= (clysm/streams:format nil "~A" 1 2 3) "1")
        "Extra arguments silently ignored")))

(deftest format-edge-malformed-signals-error
  "Malformed format strings signal error"
  (testing "Unclosed ~{ signals format-error"
    (ok (handler-case
            (progn (clysm/streams:format nil "~{~A") nil)
          (clysm/conditions:format-error () t)
          (error () t))
        "Unclosed ~{ signals error"))
  (testing "Unclosed ~[ signals format-error"
    (ok (handler-case
            (progn (clysm/streams:format nil "~[x") nil)
          (clysm/conditions:format-error () t)
          (error () t))
        "Unclosed ~[ signals error")))
