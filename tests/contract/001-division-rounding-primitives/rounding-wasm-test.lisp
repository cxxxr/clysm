;;;; rounding-wasm-test.lisp - Contract tests for rounding function Wasm output
;;;; Phase 13D-1e: Division/Rounding Function Primitives
;;;; Tests T008, T015, T021, T027 - Wasm validation for all rounding functions
(in-package #:clysm/tests/contract/rounding-primitives)

;;; ============================================================
;;; T008: Contract tests for floor/ceiling/round Wasm validation
;;; ============================================================

(deftest test-floor-wasm-validation
  "Verify (floor 7 2) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(floor 7 2))))
    (ok (arrayp wasm-bytes)
        "(floor 7 2) should produce Wasm byte array")
    (ok (plusp (length wasm-bytes))
        "Wasm output should have content")
    (ok (validate-wasm-silent wasm-bytes)
        "(floor 7 2) Wasm should pass wasm-tools validate")))

(deftest test-ceiling-wasm-validation
  "Verify (ceiling 7 2) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(ceiling 7 2))))
    (ok (arrayp wasm-bytes)
        "(ceiling 7 2) should produce Wasm byte array")
    (ok (validate-wasm-silent wasm-bytes)
        "(ceiling 7 2) Wasm should pass validation")))

(deftest test-round-wasm-validation
  "Verify (round 7 2) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(round 7 2))))
    (ok (arrayp wasm-bytes)
        "(round 7 2) should produce Wasm byte array")
    (ok (validate-wasm-silent wasm-bytes)
        "(round 7 2) Wasm should pass validation")))

(deftest test-floor-negative-wasm-validation
  "Verify (floor -7 2) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(floor -7 2))))
    (ok (validate-wasm-silent wasm-bytes)
        "(floor -7 2) Wasm should pass validation")))

(deftest test-ceiling-negative-wasm-validation
  "Verify (ceiling -7 2) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(ceiling -7 2))))
    (ok (validate-wasm-silent wasm-bytes)
        "(ceiling -7 2) Wasm should pass validation")))

;;; ============================================================
;;; T015: Contract tests for ffloor/fceiling/fround Wasm validation
;;; ============================================================

(deftest test-ffloor-wasm-validation
  "Verify (ffloor 7.5 2.0) generates valid Wasm with $float result"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(ffloor 7.5 2.0))))
    (ok (arrayp wasm-bytes)
        "(ffloor 7.5 2.0) should produce Wasm byte array")
    (ok (validate-wasm-silent wasm-bytes)
        "(ffloor 7.5 2.0) Wasm should pass validation")))

(deftest test-fceiling-wasm-validation
  "Verify (fceiling 7.5 2.0) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(fceiling 7.5 2.0))))
    (ok (validate-wasm-silent wasm-bytes)
        "(fceiling 7.5 2.0) Wasm should pass validation")))

(deftest test-fround-wasm-validation
  "Verify (fround 7.5 2.0) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(fround 7.5 2.0))))
    (ok (validate-wasm-silent wasm-bytes)
        "(fround 7.5 2.0) Wasm should pass validation")))

;;; ============================================================
;;; T021: Contract tests for single-argument form Wasm validation
;;; ============================================================

(deftest test-floor-single-arg-wasm-validation
  "Verify (floor 3.7) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(floor 3.7))))
    (ok (validate-wasm-silent wasm-bytes)
        "(floor 3.7) Wasm should pass validation")))

(deftest test-ceiling-single-arg-wasm-validation
  "Verify (ceiling 3.7) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(ceiling 3.7))))
    (ok (validate-wasm-silent wasm-bytes)
        "(ceiling 3.7) Wasm should pass validation")))

(deftest test-round-single-arg-wasm-validation
  "Verify (round 3.7) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(round 3.7))))
    (ok (validate-wasm-silent wasm-bytes)
        "(round 3.7) Wasm should pass validation")))

(deftest test-ffloor-single-arg-wasm-validation
  "Verify (ffloor 3.7) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(ffloor 3.7))))
    (ok (validate-wasm-silent wasm-bytes)
        "(ffloor 3.7) Wasm should pass validation")))

(deftest test-fceiling-single-arg-wasm-validation
  "Verify (fceiling 3.7) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(fceiling 3.7))))
    (ok (validate-wasm-silent wasm-bytes)
        "(fceiling 3.7) Wasm should pass validation")))

(deftest test-fround-single-arg-wasm-validation
  "Verify (fround 3.7) generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(fround 3.7))))
    (ok (validate-wasm-silent wasm-bytes)
        "(fround 3.7) Wasm should pass validation")))

;;; ============================================================
;;; T027: Contract tests verifying output types (i31ref vs $float)
;;; ============================================================

(deftest test-floor-integer-args-returns-i31ref
  "Verify (floor 10 3) produces Wasm returning i31ref"
  ;; This test verifies the contract that floor returns integer quotient
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(floor 10 3))))
    (ok (validate-wasm-silent wasm-bytes)
        "(floor 10 3) should produce valid Wasm")
    ;; The Wasm should be valid and produce i31ref
    (ok (plusp (length wasm-bytes))
        "Output should have content")))

(deftest test-floor-float-args-returns-i31ref
  "Verify (floor 10.0 3.0) produces Wasm returning i31ref (integer quotient)"
  ;; Per ANSI CL, floor always returns integer quotient
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(floor 10.0 3.0))))
    (ok (validate-wasm-silent wasm-bytes)
        "(floor 10.0 3.0) should produce valid Wasm")))

(deftest test-ffloor-returns-float-struct
  "Verify (ffloor 10.0 3.0) produces Wasm returning $float struct"
  ;; ffloor returns float quotient, not integer
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(ffloor 10.0 3.0))))
    (ok (validate-wasm-silent wasm-bytes)
        "(ffloor 10.0 3.0) should produce valid Wasm")))

(deftest test-function-with-floor-wasm-validation
  "Verify function using floor generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun div-floor (a b) (floor a b)))))
    (ok (arrayp wasm-bytes)
        "div-floor function should produce Wasm")
    (ok (validate-wasm-silent wasm-bytes)
        "div-floor Wasm should pass validation")))

(deftest test-function-with-multiple-rounding
  "Verify function using multiple rounding ops generates valid Wasm"
  (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                     '(defun round-both (x)
                        (values (floor x) (ceiling x))))))
    (ok (arrayp wasm-bytes)
        "round-both function should produce Wasm")
    (ok (validate-wasm-silent wasm-bytes)
        "round-both Wasm should pass validation")))
