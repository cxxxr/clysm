;;;; position-test.lisp - Unit tests for position runtime dispatch (T033)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that position dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_pos_p.htm

(in-package #:clysm/tests/unit/list-runtime)

;;; ============================================================
;;; T033: position runtime dispatch tests
;;; ============================================================

(deftest test-position-registered-dispatch
  "After registration, position dispatches to runtime function."
  ;; Register position to use runtime function
  (clysm/compiler/codegen/func-section::register-runtime-function
   'position :$position-rt nil)
  ;; Compile a position call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(position 'b '(a b c)))))
    (ok (search "call $position-rt" wat)
        "With registration, position should call $position-rt"))
  ;; Clean up
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-position-on-string
  "position works on strings."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'position :$position-rt nil)
  ;; position on string
  (let ((wat (clysm/compiler:compile-to-wat '(position #\n "banana"))))
    (ok (search "call $position-rt" wat)
        "position on string should call $position-rt"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-position-with-from-end
  "position with :from-end dispatches to runtime."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'position :$position-rt nil)
  ;; position with :from-end t
  (let ((wat (clysm/compiler:compile-to-wat '(position #\a "banana" :from-end t))))
    (ok (search "call $position-rt" wat)
        "position with :from-end should call $position-rt"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))
