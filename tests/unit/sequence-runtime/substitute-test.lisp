;;;; substitute-test.lisp - Unit tests for substitute runtime dispatch
;;;; Feature: 001-sequence-runtime-migration
;;;; Tests that substitute family dispatches to runtime functions when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_sbs_s.htm

(in-package #:clysm/tests/unit/sequence-runtime)

;;; ============================================================
;;; T011: substitute runtime dispatch tests
;;; ============================================================

(deftest test-substitute-not-registered
  "Before registration, substitute uses inline codegen."
  (clysm/compiler/codegen/func-section::clear-runtime-functions)
  (let ((wat (clysm/compiler:compile-to-wat '(substitute 'x 'a '(a b a c)))))
    (ok (not (search "SUBSTITUTE-RT" (string-upcase wat)))
        "Without registration, substitute should not call SUBSTITUTE-RT")))

(deftest test-substitute-registered-dispatch
  "After registration, substitute dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'substitute :$substitute-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(substitute 'x 'a '(a b a c)))))
    (ok (search "SUBSTITUTE-RT" (string-upcase wat))
        "With registration, substitute should call SUBSTITUTE-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-substitute-if-registered-dispatch
  "substitute-if dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'substitute-if :$substitute-if-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(substitute-if 0 #'oddp '(1 2 3 4)))))
    (ok (search "SUBSTITUTE-IF-RT" (string-upcase wat))
        "substitute-if should call SUBSTITUTE-IF-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-substitute-if-not-registered-dispatch
  "substitute-if-not dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'substitute-if-not :$substitute-if-not-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(substitute-if-not 'x #'numberp '(1 a 2 b)))))
    (ok (search "SUBSTITUTE-IF-NOT-RT" (string-upcase wat))
        "substitute-if-not should call SUBSTITUTE-IF-NOT-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

;;; ============================================================
;;; Runtime function behavior tests (US1 acceptance scenarios)
;;; ============================================================

(deftest test-substitute-basic
  "substitute returns list with matching elements replaced."
  (let ((result (clysm::substitute-rt 'x 'a '(a b a c) nil nil nil nil nil nil)))
    (ok (equal result '(x b x c))
        "substitute 'x 'a (a b a c) should return (x b x c)")))

(deftest test-substitute-if-basic
  "substitute-if replaces elements satisfying predicate."
  (let ((result (clysm::substitute-if-rt 0 #'oddp '(1 2 3 4 5) nil nil nil nil nil)))
    (ok (equal result '(0 2 0 4 0))
        "substitute-if 0 oddp should replace odd numbers with 0")))

(deftest test-substitute-if-not-basic
  "substitute-if-not replaces elements not satisfying predicate."
  (let ((result (clysm::substitute-if-not-rt 'x #'numberp '(1 a 2 b 3) nil nil nil nil nil)))
    (ok (equal result '(1 x 2 x 3))
        "substitute-if-not numberp should replace non-numbers")))

(deftest test-substitute-empty-list
  "substitute on empty list returns nil."
  (let ((result (clysm::substitute-rt 'x 'a nil nil nil nil nil nil nil)))
    (ok (null result)
        "substitute on empty list should return nil")))

(deftest test-substitute-no-matches
  "substitute with no matches returns copy of list."
  (let ((result (clysm::substitute-rt 'x 99 '(1 2 3) nil nil nil nil nil nil)))
    (ok (equal result '(1 2 3))
        "substitute with no matches should return equivalent list")))

;;; ============================================================
;;; US2: Keyword argument tests (T037)
;;; ============================================================

(deftest test-substitute-with-key
  "substitute with :key argument."
  (let ((result (clysm::substitute-rt '(99 . z) 3 '((1 . a) (2 . b) (3 . c)) nil #'car nil nil nil nil)))
    (ok (equal result '((1 . a) (2 . b) (99 . z)))
        "substitute :key #'car should match on car")))

(deftest test-substitute-with-start-end
  "substitute with :start/:end bounds."
  (let ((result (clysm::substitute-rt 'x 'a '(a b a c a) nil nil 1 4 nil nil)))
    (ok (equal result '(a b x c a))
        "substitute :start 1 :end 4 should only replace within bounds")))

(deftest test-substitute-with-count
  "substitute with :count limit."
  (let ((result (clysm::substitute-rt 'x 'a '(a b a c a d a) nil nil nil nil 2 nil)))
    (ok (equal result '(x b x c a d a))
        "substitute :count 2 should replace only first 2")))
