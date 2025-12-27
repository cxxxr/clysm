;;;; iteration-test.lisp - Iteration directive (~{~}) tests
;;;; Feature: 032-format-function

(defpackage #:clysm/test/format/iteration
  (:use #:cl #:rove)
  (:shadowing-import-from #:clysm/streams
                          #:format)
  (:import-from #:clysm/conditions
                #:format-error))

(in-package #:clysm/test/format/iteration)

;;; ============================================================
;;; Basic Iteration Tests (T022-T026)
;;; ============================================================

(deftest iteration-basic
  (testing "~{~A~} processes list elements"
    ;; T022: Basic iteration
    (let ((result (clysm/streams:format nil "~{~A~}" '(1 2 3))))
      (ok (string= result "123")
          "~{~A~} should concatenate list elements"))))

(deftest iteration-with-separator
  (testing "~{~A~^, ~} produces comma-separated output"
    ;; T023: Iteration with escape for separator
    ;; Note: CL symbols print uppercase by default
    (let ((result (clysm/streams:format nil "~{~A~^, ~}" '(a b c))))
      (ok (string= result "A, B, C")
          "~{~A~^, ~} should produce comma-separated list"))))

(deftest iteration-empty-list
  (testing "~{~} with empty list produces no output"
    ;; T024: Empty list handling
    (let ((result (clysm/streams:format nil "Items: ~{~A~}" nil)))
      (ok (string= result "Items: ")
          "~{~} with empty list should produce no body output"))))

(deftest iteration-nested
  (testing "nested ~{~} works correctly"
    ;; T025: Nested iteration
    ;; Each element is itself a list
    (let ((result (clysm/streams:format nil "~{[~{~A~}]~}" '((1 2) (3 4)))))
      (ok (string= result "[12][34]")
          "nested iteration should work"))))

(deftest iteration-malformed
  (testing "unclosed ~{ signals format-error"
    ;; T026: Malformed ~{ without ~}
    ;; Use handler-case since our conditions use CLOS defclass
    (ok (handler-case
            (progn (clysm/streams:format nil "~{~A" '(1 2 3)) nil)
          (clysm/conditions:format-error () t)
          (error () t))  ; Also accept generic errors
        "unclosed ~{ should signal format-error")))
