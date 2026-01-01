;;;; delete-test.lisp - Unit tests for delete runtime dispatch
;;;; Feature: 001-sequence-runtime-migration
;;;; Tests that delete family dispatches to runtime functions when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_rm_rm.htm

(in-package #:clysm/tests/unit/sequence-runtime)

;;; ============================================================
;;; T012: delete runtime dispatch tests
;;; ============================================================

(deftest test-delete-not-registered
  "Before registration, delete is undefined (never had inline codegen).
   Note: Unlike remove/count/substitute, delete was never implemented inline.
   This test verifies that behavior - delete requires runtime registration."
  (clysm/compiler/codegen/func-section::clear-runtime-functions)
  ;; delete was never implemented as inline codegen (research.md: ~0 lines)
  ;; So without registration, it should fail with undefined function
  (ok (handler-case
          (progn
            (clysm/compiler:compile-to-wat '(delete 1 '(1 2 3)))
            nil)  ; Should not reach here
        (error () t))  ; Expect an error
      "Without registration, delete should error (no inline codegen exists)"))

(deftest test-delete-registered-dispatch
  "After registration, delete dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'delete :$delete-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(delete 1 '(1 2 3)))))
    (ok (search "DELETE-RT" (string-upcase wat))
        "With registration, delete should call DELETE-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-delete-if-registered-dispatch
  "delete-if dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'delete-if :$delete-if-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(delete-if #'evenp '(1 2 3 4)))))
    (ok (search "DELETE-IF-RT" (string-upcase wat))
        "delete-if should call DELETE-IF-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-delete-if-not-registered-dispatch
  "delete-if-not dispatches to runtime function."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'delete-if-not :$delete-if-not-rt nil)
  (let ((wat (clysm/compiler:compile-to-wat '(delete-if-not #'numberp '(1 a 2 b)))))
    (ok (search "DELETE-IF-NOT-RT" (string-upcase wat))
        "delete-if-not should call DELETE-IF-NOT-RT"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

;;; ============================================================
;;; Runtime function behavior tests (US1 acceptance scenarios)
;;; ============================================================

(deftest test-delete-basic
  "delete destructively removes matching elements."
  ;; Make a fresh list to avoid modifying quoted data
  (let* ((lst (list 1 nil 2 nil 3))
         (result (clysm::delete-rt nil lst nil nil nil nil nil nil)))
    (ok (equal result '(1 2 3))
        "delete nil from (1 nil 2 nil 3) should return (1 2 3)")))

(deftest test-delete-if-basic
  "delete-if destructively removes elements satisfying predicate."
  (let* ((lst (list 1 2 3 4 5 6))
         (result (clysm::delete-if-rt #'evenp lst nil nil nil nil nil)))
    (ok (equal result '(1 3 5))
        "delete-if evenp should remove even numbers")))

(deftest test-delete-if-not-basic
  "delete-if-not keeps only elements satisfying predicate."
  (let* ((lst (list 1 'a 2 'b 3))
         (result (clysm::delete-if-not-rt #'numberp lst nil nil nil nil nil)))
    (ok (equal result '(1 2 3))
        "delete-if-not numberp should keep only numbers")))

(deftest test-delete-empty-list
  "delete on empty list returns nil."
  (let ((result (clysm::delete-rt 1 nil nil nil nil nil nil nil)))
    (ok (null result)
        "delete on empty list should return nil")))

(deftest test-delete-first-element
  "delete can remove first element."
  (let* ((lst (list 1 2 3))
         (result (clysm::delete-rt 1 lst nil nil nil nil nil nil)))
    (ok (equal result '(2 3))
        "delete first element should work")))

;;; ============================================================
;;; US2: Keyword argument tests (T038)
;;; ============================================================

(deftest test-delete-with-key
  "delete with :key argument."
  (let* ((lst (list '(1 . a) '(2 . b) '(3 . c)))
         (result (clysm::delete-rt 3 lst nil #'car nil nil nil nil)))
    (ok (equal result '((1 . a) (2 . b)))
        "delete :key #'car should match on car")))

(deftest test-delete-with-count
  "delete with :count limit."
  (let* ((lst (list 'a 'b 'a 'c 'a 'd 'a))
         (result (clysm::delete-rt 'a lst nil nil nil nil 2 nil)))
    (ok (equal result '(b c a d a))
        "delete :count 2 should remove only first 2")))
