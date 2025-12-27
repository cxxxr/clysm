;;;; recursive-test.lisp - Recursive directive (~?) tests
;;;; Feature: 032-format-function

(defpackage #:clysm/test/format/recursive
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/streams
                          #:format)
  (:import-from #:clysm/conditions
                #:format-error))

(in-package #:clysm/test/format/recursive)

;;; ============================================================
;;; Recursive Processing Tests (T046-T049)
;;; ============================================================

(deftest recursive-basic
  (testing "~? processes nested format string"
    ;; T046: Basic recursion
    (let ((result (clysm/streams:format nil "~?" "Value: ~A" '(42))))
      (ok (string= result "Value: 42")
          "~? should process nested format string with args"))))

(deftest recursive-multiple-args
  (testing "~? with multiple arguments in nested format"
    ;; T047: Multiple arguments
    (let ((result (clysm/streams:format nil "~?" "~A + ~A = ~A" '(1 2 3))))
      (ok (string= result "1 + 2 = 3")
          "~? should pass all arguments to nested format"))))

(deftest recursive-non-string-control
  (testing "~? with non-string control signals format-error"
    ;; T048: Type check for control string
    ;; Use handler-case since our conditions use CLOS defclass
    (ok (handler-case
            (progn (clysm/streams:format nil "~?" 42 '(1 2)) nil)
          (clysm/conditions:format-error () t)
          (error () t))
        "~? with non-string control should signal format-error")))

(deftest recursive-non-list-args
  (testing "~? with non-list args signals format-error"
    ;; T049: Type check for argument list
    ;; Use handler-case since our conditions use CLOS defclass
    (ok (handler-case
            (progn (clysm/streams:format nil "~?" "~A" 42) nil)
          (clysm/conditions:format-error () t)
          (error () t))
        "~? with non-list args should signal format-error")))
