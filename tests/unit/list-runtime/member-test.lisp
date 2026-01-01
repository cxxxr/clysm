;;;; member-test.lisp - Unit tests for member runtime dispatch (T029)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that member dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_mem_m.htm

(in-package #:clysm/tests/unit/list-runtime)

;;; ============================================================
;;; T029: member runtime dispatch tests
;;; ============================================================

(deftest test-member-not-registered
  "Before registration, member uses inline codegen."
  ;; Clear the runtime function table
  (clysm/compiler/codegen/func-section::clear-runtime-functions)
  ;; Compile a member call - should use inline codegen
  (let ((wat (clysm/compiler:compile-to-wat '(member 'a '(a b c)))))
    (ok (not (search "call $member-rt" wat))
        "Without registration, member should not call $member-rt")))

(deftest test-member-registered-dispatch
  "After registration, member dispatches to runtime function."
  ;; Register member to use runtime function
  (clysm/compiler/codegen/func-section::register-runtime-function
   'member :$member-rt nil)
  ;; Compile a member call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(member 'a '(a b c)))))
    (ok (search "call $member-rt" wat)
        "With registration, member should call $member-rt"))
  ;; Clean up
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-member-with-test-keyword
  "member with :test keyword dispatches to runtime."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'member :$member-rt nil)
  ;; member with :test #'equal
  (let ((wat (clysm/compiler:compile-to-wat '(member "a" lst :test #'equal))))
    (ok (search "call $member-rt" wat)
        "member with :test should call $member-rt"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))
