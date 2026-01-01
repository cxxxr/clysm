;;;; count-test.lisp - Unit tests for count runtime dispatch
;;;; Feature: 001-sequence-runtime-migration
;;;; Tests that count family dispatches to runtime functions when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_countc.htm

(in-package #:clysm/tests/unit/sequence-runtime)

;;; ============================================================
;;; T010: count runtime dispatch tests
;;; ============================================================

(deftest test-count-not-registered
  "Before registration, count uses inline codegen."
  (clysm/compiler/codegen/func-section::clear-runtime-functions)
  (let ((wat (clysm/compiler:compile-to-wat '(count 'a '(a b a c a)))))
    (ok (not (search "COUNT-RT" (string-upcase wat)))
        "Without registration, count should not call COUNT-RT")))

(deftest test-count-registered-dispatch
  "After registration, count dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'count :$count-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(count 'a '(a b a c a)))))
    (ok (search "COUNT-RT" (string-upcase wat))
        "With registration, count should call COUNT-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-count-if-registered-dispatch
  "count-if dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'count-if :$count-if-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(count-if #'evenp '(1 2 3 4 5)))))
    (ok (search "COUNT-IF-RT" (string-upcase wat))
        "count-if should call COUNT-IF-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-count-if-not-registered-dispatch
  "count-if-not dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'count-if-not :$count-if-not-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(count-if-not #'symbolp '(a 1 b 2 c)))))
    (ok (search "COUNT-IF-NOT-RT" (string-upcase wat))
        "count-if-not should call COUNT-IF-NOT-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

;;; ============================================================
;;; Runtime function behavior tests (US1 acceptance scenarios)
;;; ============================================================

(deftest test-count-basic
  "count returns number of matching elements."
  (let ((result (clysm::count-rt 'a '(a b a c a) nil nil nil nil nil)))
    (ok (= result 3)
        "count 'a in (a b a c a) should return 3")))

(deftest test-count-if-basic
  "count-if returns count of elements satisfying predicate."
  (let ((result (clysm::count-if-rt #'evenp '(1 2 3 4 5 6) nil nil nil nil)))
    (ok (= result 3)
        "count-if evenp should return 3")))

(deftest test-count-if-not-basic
  "count-if-not returns count of elements not satisfying predicate."
  (let ((result (clysm::count-if-not-rt #'symbolp '(a 1 b 2 c 3) nil nil nil nil)))
    (ok (= result 3)
        "count-if-not symbolp should return 3")))

(deftest test-count-empty-list
  "count on empty list returns 0."
  (let ((result (clysm::count-rt 1 nil nil nil nil nil nil)))
    (ok (zerop result)
        "count on empty list should return 0")))

(deftest test-count-no-matches
  "count with no matches returns 0."
  (let ((result (clysm::count-rt 99 '(1 2 3) nil nil nil nil nil)))
    (ok (zerop result)
        "count with no matches should return 0")))

;;; ============================================================
;;; US2: Keyword argument tests (T036)
;;; ============================================================

(deftest test-count-with-key
  "count with :key argument."
  (let ((result (clysm::count-rt 3 '((1 . a) (2 . b) (3 . c) (3 . d)) nil #'car nil nil nil)))
    (ok (= result 2)
        "count 3 :key #'car should return 2")))

(deftest test-count-with-test
  "count with :test argument."
  (let ((result (clysm::count-rt "foo" '("FOO" "bar" "Foo" "baz") #'string-equal nil nil nil nil)))
    (ok (= result 2)
        "count with string-equal should be case-insensitive")))

(deftest test-count-with-start-end
  "count with :start/:end bounds."
  (let ((result (clysm::count-rt 'a '(a b a c a d a) nil nil 1 5 nil)))
    (ok (= result 2)
        "count 'a :start 1 :end 5 should count only within bounds")))
