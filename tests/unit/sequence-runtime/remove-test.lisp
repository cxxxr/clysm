;;;; remove-test.lisp - Unit tests for remove runtime dispatch
;;;; Feature: 001-sequence-runtime-migration
;;;; Tests that remove family dispatches to runtime functions when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_rm_rm.htm

(in-package #:clysm/tests/unit/sequence-runtime)

;;; ============================================================
;;; T009: remove runtime dispatch tests
;;; ============================================================

(deftest test-remove-not-registered
  "Before registration, remove uses inline codegen."
  (clysm/compiler/codegen/func-section::clear-runtime-functions)
  (let ((wat (clysm/compiler:compile-to-wat '(remove 1 '(1 2 3)))))
    (ok (not (search "REMOVE-RT" (string-upcase wat)))
        "Without registration, remove should not call REMOVE-RT")))

(deftest test-remove-registered-dispatch
  "After registration, remove dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'remove :$remove-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(remove 1 '(1 2 3)))))
    (ok (search "REMOVE-RT" (string-upcase wat))
        "With registration, remove should call REMOVE-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-remove-if-registered-dispatch
  "remove-if dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'remove-if :$remove-if-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(remove-if #'evenp '(1 2 3 4)))))
    (ok (search "REMOVE-IF-RT" (string-upcase wat))
        "remove-if should call REMOVE-IF-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-remove-if-not-registered-dispatch
  "remove-if-not dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'remove-if-not :$remove-if-not-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(remove-if-not #'numberp '(1 a 2 b)))))
    (ok (search "REMOVE-IF-NOT-RT" (string-upcase wat))
        "remove-if-not should call REMOVE-IF-NOT-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

;;; ============================================================
;;; Runtime function behavior tests (US1 acceptance scenarios)
;;; ============================================================

(deftest test-remove-basic
  "remove returns list without matching elements."
  ;; Direct test of runtime function
  (let ((result (clysm::remove-rt 3 '(1 2 3 4 3 5) nil nil nil nil nil nil)))
    (ok (equal result '(1 2 4 5))
        "remove 3 from (1 2 3 4 3 5) should return (1 2 4 5)")))

(deftest test-remove-if-basic
  "remove-if returns list without elements satisfying predicate."
  (let ((result (clysm::remove-if-rt #'evenp '(1 2 3 4 5 6) nil nil nil nil nil)))
    (ok (equal result '(1 3 5))
        "remove-if evenp should return odd elements")))

(deftest test-remove-if-not-basic
  "remove-if-not keeps only elements satisfying predicate."
  (let ((result (clysm::remove-if-not-rt #'numberp '(1 a 2 b 3) nil nil nil nil nil)))
    (ok (equal result '(1 2 3))
        "remove-if-not numberp should keep only numbers")))

(deftest test-remove-empty-list
  "remove on empty list returns nil."
  (let ((result (clysm::remove-rt 1 nil nil nil nil nil nil nil)))
    (ok (null result)
        "remove on empty list should return nil")))

(deftest test-remove-no-matches
  "remove with no matches returns copy of list."
  (let ((result (clysm::remove-rt 99 '(1 2 3) nil nil nil nil nil nil)))
    (ok (equal result '(1 2 3))
        "remove with no matches should return equivalent list")))

;;; ============================================================
;;; US2: Keyword argument tests (T031-T035)
;;; ============================================================

(deftest test-remove-with-key
  "remove with :key argument."
  (let ((result (clysm::remove-rt 3 '((1 . a) (2 . b) (3 . c)) nil #'car nil nil nil nil)))
    (ok (equal result '((1 . a) (2 . b)))
        "remove 3 :key #'car should remove (3 . c)")))

(deftest test-remove-with-test
  "remove with :test argument."
  (let ((result (clysm::remove-rt 2 '(1 2 3 4) #'< nil nil nil nil nil)))
    (ok (equal result '(1 2))
        "remove 2 :test #'< should remove elements > 2")))

(deftest test-remove-with-start-end
  "remove with :start/:end bounds."
  (let ((result (clysm::remove-rt 'a '(a b a c a d a) nil nil 1 5 nil nil)))
    (ok (equal result '(a b c d a))
        "remove 'a :start 1 :end 5 should only remove within bounds")))

(deftest test-remove-with-count
  "remove with :count limit."
  (let ((result (clysm::remove-rt 'a '(a b a c a d a) nil nil nil nil 2 nil)))
    (ok (equal result '(b c a d a))
        "remove 'a :count 2 should remove only first 2 occurrences")))

(deftest test-remove-with-from-end
  "remove with :from-end processing."
  (let ((result (clysm::remove-rt 'a '(a b a c a d a) nil nil nil nil 2 t)))
    (ok (equal result '(a b a c d))
        "remove 'a :from-end t :count 2 should remove last 2 occurrences")))
