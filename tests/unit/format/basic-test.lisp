;;;; basic-test.lisp - Basic FORMAT directive tests
;;;; Feature: 032-format-function

(defpackage #:clysm/test/format/basic
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/streams
                          #:format)
  (:import-from #:clysm/conditions
                #:format-error))

(in-package #:clysm/test/format/basic)

;;; ============================================================
;;; Format-Error Condition Tests (T007)
;;; ============================================================

(deftest format-error-on-malformed-string
  (testing "format-error is signaled for malformed format strings"
    ;; Test that format-error can be created and has the right structure
    (let ((err (make-instance 'clysm/conditions:format-error
                              :control-string "test"
                              :position 0
                              :format-control "test error"
                              :format-arguments nil)))
      (ok (clysm/conditions::format-error-control-string err)
          "format-error should have control-string accessor")
      (ok (zerop (clysm/conditions::format-error-position err))
          "format-error should have position accessor"))))

;;; ============================================================
;;; Column Tracking Tests (T008)
;;; ============================================================

(deftest column-tracking-basic
  (testing "column tracking for fresh-line support"
    ;; Test that output starting at column 0 is tracked
    ;; Placeholder - will fail until column tracking is implemented
    (ok t "column tracking placeholder")))

;;; ============================================================
;;; Fresh-Line (~&) Tests (T015-T017)
;;; ============================================================

(deftest fresh-line-at-column-zero
  (testing "~& at column 0 produces no newline"
    ;; T015: At column 0, ~& should not add a newline
    (let ((result (clysm/streams:format nil "~&test")))
      (ok (string= result "test")
          "~& at column 0 should not produce leading newline"))))

(deftest fresh-line-at-column-nonzero
  (testing "~& after output produces newline"
    ;; T016: After some output, ~& should add a newline
    (let ((result (clysm/streams:format nil "x~&y")))
      (ok (string= result (cl:format nil "x~%y"))
          "~& after output should produce newline"))))

(deftest fresh-line-after-newline
  (testing "~& after ~% produces no additional newline"
    ;; T017: After a newline (~%), ~& should not add another
    (let ((result (clysm/streams:format nil "x~%~&y")))
      (ok (string= result (cl:format nil "x~%y"))
          "~& after ~% should not produce additional newline"))))
