;;;; find-test.lisp - Unit tests for find runtime dispatch (T032)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that find dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_find_.htm

(in-package #:clysm/tests/unit/list-runtime)

;;; ============================================================
;;; T032: find runtime dispatch tests
;;; ============================================================

(deftest test-find-registered-dispatch
  "After registration, find dispatches to runtime function."
  ;; Register find to use runtime function
  (clysm/compiler/codegen/func-section::register-runtime-function
   'find :$find-rt nil)
  ;; Compile a find call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(find 'b '(a b c)))))
    (ok (search "call $find-rt" wat)
        "With registration, find should call $find-rt"))
  ;; Clean up
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-find-with-start-end
  "find with :start/:end keywords dispatches to runtime."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'find :$find-rt nil)
  ;; find with :start and :end
  (let ((wat (clysm/compiler:compile-to-wat '(find #\a str :start 1 :end 5))))
    (ok (search "call $find-rt" wat)
        "find with :start/:end should call $find-rt"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-find-on-vector
  "find works on vectors as well as lists."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'find :$find-rt nil)
  ;; find on a vector
  (let ((wat (clysm/compiler:compile-to-wat '(find 3 #(1 2 3 4)))))
    (ok (search "call $find-rt" wat)
        "find on vector should call $find-rt"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))
