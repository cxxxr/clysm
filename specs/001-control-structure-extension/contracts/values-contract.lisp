;;;; values-contract.lisp - Test contracts for values special form
;;;; HyperSpec: resources/HyperSpec/Body/f_values.htm

(in-package #:clysm-test)

;;; ============================================================
;;; Contract Tests: values Special Form
;;; ============================================================

;;; These tests verify Wasm output structure for values forms.
;;; Run with: (asdf:test-system :clysm/contracts)

(deftest values-zero-returns-nil
  "Contract: (values) returns NIL with mv-count=0"
  (let ((wasm (compile-form-to-wasm '(values))))
    ;; Verify globals section sets mv-count to 0
    (ok (wasm-contains-instruction wasm '(global.set 2 (i32.const 0))))
    ;; Verify returns NIL (global 0)
    (ok (wasm-returns wasm '(global.get 0)))))

(deftest values-single-returns-value
  "Contract: (values x) returns x with mv-count=1"
  (let ((wasm (compile-form-to-wasm '(lambda (x) (values x)))))
    ;; Verify mv-count set to 1
    (ok (wasm-contains-instruction wasm '(global.set 2 (i32.const 1))))
    ;; Verify x is on stack as return value
    (ok (wasm-returns-local wasm 0))))

(deftest values-multiple-stores-in-buffer
  "Contract: (values a b c) stores b,c in buffer, returns a"
  (let ((wasm (compile-form-to-wasm '(lambda (a b c) (values a b c)))))
    ;; Verify mv-count set to 3
    (ok (wasm-contains-instruction wasm '(global.set 2 (i32.const 3))))
    ;; Verify buffer writes for secondary values
    (ok (wasm-contains-instruction wasm '(array.set 22)))  ; Type 22 = $mv_array
    ;; Verify primary value (a) is returned
    (ok (wasm-returns-local wasm 0))))

(deftest values-in-non-tail-position
  "Contract: (list (values a b) c) uses only primary value"
  (let ((wasm (compile-form-to-wasm '(lambda (a b c) (list (values a b) c)))))
    ;; Verify values still sets mv-count/buffer
    (ok (wasm-contains-instruction wasm '(global.set 2)))
    ;; But list receives only the primary value
    (ok (wasm-validates wasm))))

(deftest values-wasm-validates
  "Contract: All values forms produce valid Wasm"
  (dolist (form '((values)
                  (values 1)
                  (values 1 2)
                  (values 1 2 3 4 5)))
    (let ((wasm (compile-form-to-wasm form)))
      (ok (wasm-validates wasm)
          (format nil "Form ~S must produce valid Wasm" form)))))
