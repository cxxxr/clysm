;;;; jit-test.lisp - Unit tests for JIT module system
;;;; Feature: 017-eval-jit-compile
;;;; Tests for runtime imports and module linking (T040-T041)

(in-package #:clysm/tests/unit/jit)

;;; ============================================================
;;; T040: Unit test - register-runtime-import adds to table
;;; ============================================================

(deftest test-register-runtime-import
  "T040: register-runtime-import adds function to import table."
  ;; Register a custom import
  (clysm/eval/jit:register-runtime-import "test-add" #'+)

  ;; Verify it can be retrieved
  (let ((fn (clysm/eval/jit:get-runtime-import "test-add")))
    (ok fn "Registered import exists")
    (ok (functionp fn) "Registered import is a function")
    (ok (= (funcall fn 2 3) 5) "Registered function works correctly")))

(deftest test-register-multiple-imports
  "T040: Multiple imports can be registered."
  (clysm/eval/jit:register-runtime-import "my-mul" #'*)
  (clysm/eval/jit:register-runtime-import "my-sub" #'-)

  (let ((mul (clysm/eval/jit:get-runtime-import "my-mul"))
        (sub (clysm/eval/jit:get-runtime-import "my-sub")))
    (ok (= (funcall mul 3 4) 12) "my-mul works: 3 * 4 = 12")
    (ok (= (funcall sub 10 3) 7) "my-sub works: 10 - 3 = 7")))

(deftest test-register-overwrites-previous
  "T040: Re-registering an import overwrites the previous."
  ;; Register initial version
  (clysm/eval/jit:register-runtime-import "overwrite-test" #'+)
  (let ((fn1 (clysm/eval/jit:get-runtime-import "overwrite-test")))
    (ok (= (funcall fn1 2 3) 5) "Initial function adds"))

  ;; Overwrite with different function
  (clysm/eval/jit:register-runtime-import "overwrite-test" #'*)
  (let ((fn2 (clysm/eval/jit:get-runtime-import "overwrite-test")))
    (ok (= (funcall fn2 2 3) 6) "Overwritten function multiplies")))

;;; ============================================================
;;; T041: Unit test - get-runtime-import retrieves function
;;; ============================================================

(deftest test-get-runtime-import-existing
  "T041: get-runtime-import retrieves registered function."
  ;; Standard imports are initialized on load
  (let ((add-fn (clysm/eval/jit:get-runtime-import "add")))
    (ok add-fn "add import exists")
    (ok (= (funcall add-fn 1 2 3) 6) "add works: 1 + 2 + 3 = 6")))

(deftest test-get-runtime-import-nonexistent
  "T041: get-runtime-import returns nil for unknown imports."
  (let ((fn (clysm/eval/jit:get-runtime-import "nonexistent-import")))
    (ok (null fn) "Unknown import returns nil")))

(deftest test-standard-runtime-imports-exist
  "T041: Standard runtime imports are available."
  ;; Verify core imports exist
  (ok (clysm/eval/jit:get-runtime-import "add") "add exists")
  (ok (clysm/eval/jit:get-runtime-import "sub") "sub exists")
  (ok (clysm/eval/jit:get-runtime-import "mul") "mul exists")
  (ok (clysm/eval/jit:get-runtime-import "div") "div exists")
  (ok (clysm/eval/jit:get-runtime-import "lt") "lt exists")
  (ok (clysm/eval/jit:get-runtime-import "gt") "gt exists")
  (ok (clysm/eval/jit:get-runtime-import "eq") "eq exists")
  (ok (clysm/eval/jit:get-runtime-import "cons") "cons exists")
  (ok (clysm/eval/jit:get-runtime-import "car") "car exists")
  (ok (clysm/eval/jit:get-runtime-import "cdr") "cdr exists"))

(deftest test-standard-imports-functionality
  "T041: Standard runtime imports work correctly."
  (let ((cons-fn (clysm/eval/jit:get-runtime-import "cons"))
        (car-fn (clysm/eval/jit:get-runtime-import "car"))
        (cdr-fn (clysm/eval/jit:get-runtime-import "cdr")))
    (let ((pair (funcall cons-fn 1 2)))
      (ok (= (funcall car-fn pair) 1) "car of (1 . 2) is 1")
      (ok (= (funcall cdr-fn pair) 2) "cdr of (1 . 2) is 2"))))
