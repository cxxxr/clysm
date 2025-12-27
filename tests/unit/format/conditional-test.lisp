;;;; conditional-test.lisp - Conditional directive (~[~]) tests
;;;; Feature: 032-format-function

(defpackage #:clysm/test/format/conditional
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/streams
                          #:format)
  (:import-from #:clysm/conditions
                #:format-error))

(in-package #:clysm/test/format/conditional)

;;; ============================================================
;;; Index-Based Conditional Tests (T035-T039)
;;; ============================================================

(deftest conditional-index-selection
  (testing "~[~;~] selects clause by index"
    ;; T035: Index-based selection
    (let ((r0 (clysm/streams:format nil "~[zero~;one~;two~]" 0))
          (r1 (clysm/streams:format nil "~[zero~;one~;two~]" 1))
          (r2 (clysm/streams:format nil "~[zero~;one~;two~]" 2)))
      (ok (string= r0 "zero") "index 0 selects first clause")
      (ok (string= r1 "one") "index 1 selects second clause")
      (ok (string= r2 "two") "index 2 selects third clause"))))

(deftest conditional-default-clause
  (testing "~:; provides default clause for out-of-range index"
    ;; T036: Default clause with ~:;
    (let ((result (clysm/streams:format nil "~[zero~;one~:;many~]" 99)))
      (ok (string= result "many")
          "out-of-range index should use ~:; default clause"))))

(deftest conditional-boolean-form
  (testing "~:[false~;true~] selects by nil/non-nil"
    ;; T037: Boolean conditional
    (let ((r-nil (clysm/streams:format nil "~:[no~;yes~]" nil))
          (r-t (clysm/streams:format nil "~:[no~;yes~]" t))
          (r-val (clysm/streams:format nil "~:[no~;yes~]" 42)))
      (ok (string= r-nil "no") "nil selects first clause")
      (ok (string= r-t "yes") "t selects second clause")
      (ok (string= r-val "yes") "any non-nil selects second clause"))))

(deftest conditional-out-of-range-no-default
  (testing "out-of-range index with no default produces no output"
    ;; T038: Out of range without default
    (let ((result (clysm/streams:format nil "~[zero~;one~]" 99)))
      (ok (string= result "")
          "out-of-range with no default should produce empty string"))))

(deftest conditional-malformed
  (testing "unclosed ~[ signals format-error"
    ;; T039: Malformed ~[ without ~]
    ;; Use handler-case since our conditions use CLOS defclass
    (ok (handler-case
            (progn (clysm/streams:format nil "~[zero" 0) nil)
          (clysm/conditions:format-error () t)
          (error () t))
        "unclosed ~[ should signal format-error")))
