;;;; complex-test.lisp - Complex number tests
;;;; 010-numeric-tower: User Story 4 - Complex Number Arithmetic
(in-package #:clysm/tests/integration/complex)

;;; These tests verify the complete compilation pipeline for complex operations:
;;; Lisp expression -> AST -> Wasm IR -> Wasm binary -> wasmtime execution

;;; ============================================================
;;; T067-T080: Complex Arithmetic Tests
;;; ============================================================

;;; --- Basic Complex Creation Tests ---

(deftest test-complex-literal
  "Complex literal: #C(1 2)"
  (let ((result (clysm/tests:compile-and-run-numeric '#C(1 2))))
    (ok (complexp result) "#C(1 2) should be complex")
    (ok (= 1 (realpart result)) "Real part should be 1")
    (ok (= 2 (imagpart result)) "Imaginary part should be 2")))

(deftest test-complex-zero-imaginary-simplifies
  "Complex with zero imaginary simplifies: #C(5 0) => 5 (spec scenario 2)"
  (let ((result (clysm/tests:compile-and-run-numeric '#C(5 0))))
    (ok (not (complexp result)) "#C(5 0) should not be complex")
    (ok (= 5 result) "#C(5 0) should equal 5")))

(deftest test-complex-function
  "Complex function: (complex 3 4)"
  (let ((result (clysm/tests:compile-and-run-numeric '(complex 3 4))))
    (ok (complexp result) "(complex 3 4) should be complex")
    (ok (= 3 (realpart result)) "Real part should be 3")
    (ok (= 4 (imagpart result)) "Imaginary part should be 4")))

;;; --- Complex Arithmetic Tests ---

(deftest test-complex-addition
  "Complex addition: #C(1 2) + #C(3 4) => #C(4 6) (spec scenario 1)"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ #C(1 2) #C(3 4)))))
    (ok (complexp result) "Result should be complex")
    (ok (= 4 (realpart result)) "Real part should be 4")
    (ok (= 6 (imagpart result)) "Imaginary part should be 6")))

(deftest test-complex-subtraction
  "Complex subtraction: #C(5 7) - #C(2 3) => #C(3 4)"
  (let ((result (clysm/tests:compile-and-run-numeric '(- #C(5 7) #C(2 3)))))
    (ok (= 3 (realpart result)) "Real part should be 3")
    (ok (= 4 (imagpart result)) "Imaginary part should be 4")))

(deftest test-complex-multiplication-i-squared
  "i^2 = -1: #C(0 1) * #C(0 1) => -1 (spec scenario 3)"
  (let ((result (clysm/tests:compile-and-run-numeric '(* #C(0 1) #C(0 1)))))
    (ok (not (complexp result)) "i^2 should simplify to real")
    (ok (= -1 result) "i^2 should equal -1")))

(deftest test-complex-multiplication
  "Complex multiplication: #C(2 3) * #C(4 5)"
  ;; (2+3i)(4+5i) = 8 + 10i + 12i + 15i^2 = 8 + 22i - 15 = -7 + 22i
  (let ((result (clysm/tests:compile-and-run-numeric '(* #C(2 3) #C(4 5)))))
    (ok (= -7 (realpart result)) "Real part should be -7")
    (ok (= 22 (imagpart result)) "Imaginary part should be 22")))

(deftest test-complex-division
  "Complex division: #C(4 2) / #C(1 1)"
  ;; (4+2i)/(1+i) = (4+2i)(1-i)/(1+i)(1-i) = (4-4i+2i-2i^2)/2 = (6-2i)/2 = 3-i
  (let ((result (clysm/tests:compile-and-run-numeric '(/ #C(4 2) #C(1 1)))))
    (ok (= 3 (realpart result)) "Real part should be 3")
    (ok (= -1 (imagpart result)) "Imaginary part should be -1")))

;;; --- Mixed Real/Complex Arithmetic Tests (spec scenario 4) ---

(deftest test-real-plus-complex
  "Real + complex: 1 + #C(2 3) => #C(3 3)"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ 1 #C(2 3)))))
    (ok (complexp result) "Result should be complex")
    (ok (= 3 (realpart result)) "Real part should be 3")
    (ok (= 3 (imagpart result)) "Imaginary part should be 3")))

(deftest test-complex-times-real
  "Complex * real: #C(2 3) * 2 => #C(4 6)"
  (let ((result (clysm/tests:compile-and-run-numeric '(* #C(2 3) 2))))
    (ok (= 4 (realpart result)) "Real part should be 4")
    (ok (= 6 (imagpart result)) "Imaginary part should be 6")))

(deftest test-float-plus-complex
  "Float + complex: 1.5 + #C(1 2)"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ 1.5 #C(1 2)))))
    (ok (complexp result) "Result should be complex")
    (ok (= 2.5 (realpart result)) "Real part should be 2.5")))

;;; --- Complex Accessors ---

(deftest test-realpart
  "Realpart: (realpart #C(3 4)) => 3"
  (ok (= 3 (clysm/tests:compile-and-run-numeric '(realpart #C(3 4))))
      "(realpart #C(3 4)) should be 3"))

(deftest test-imagpart
  "Imagpart: (imagpart #C(3 4)) => 4"
  (ok (= 4 (clysm/tests:compile-and-run-numeric '(imagpart #C(3 4))))
      "(imagpart #C(3 4)) should be 4"))

(deftest test-realpart-of-real
  "Realpart of real: (realpart 5) => 5"
  (ok (= 5 (clysm/tests:compile-and-run-numeric '(realpart 5)))
      "(realpart 5) should be 5"))

(deftest test-imagpart-of-real
  "Imagpart of real: (imagpart 5) => 0"
  (ok (= 0 (clysm/tests:compile-and-run-numeric '(imagpart 5)))
      "(imagpart 5) should be 0"))

;;; --- Complex Conjugate ---

(deftest test-conjugate
  "Conjugate: (conjugate #C(3 4)) => #C(3 -4)"
  (let ((result (clysm/tests:compile-and-run-numeric '(conjugate #C(3 4)))))
    (ok (= 3 (realpart result)) "Real part should be 3")
    (ok (= -4 (imagpart result)) "Imaginary part should be -4")))

;;; --- Complex with Different Component Types ---

(deftest test-complex-with-ratio
  "Complex with ratio components: #C(1/2 1/3)"
  (let ((result (clysm/tests:compile-and-run-numeric '(complex (/ 1 2) (/ 1 3)))))
    (ok (complexp result) "Result should be complex")
    (ok (= 1/2 (realpart result)) "Real part should be 1/2")
    (ok (= 1/3 (imagpart result)) "Imaginary part should be 1/3")))

(deftest test-complex-with-float
  "Complex with float components: #C(1.5 2.5)"
  (let ((result (clysm/tests:compile-and-run-numeric '#C(1.5 2.5))))
    (ok (complexp result) "Result should be complex")
    (ok (= 1.5 (realpart result)) "Real part should be 1.5")
    (ok (= 2.5 (imagpart result)) "Imaginary part should be 2.5")))

;;; --- Edge Cases ---

(deftest test-complex-zero
  "Complex zero: #C(0 0) => 0"
  (let ((result (clysm/tests:compile-and-run-numeric '#C(0 0))))
    (ok (not (complexp result)) "#C(0 0) should simplify")
    (ok (= 0 result) "#C(0 0) should equal 0")))

(deftest test-complex-negation
  "Complex negation: (- #C(3 4)) => #C(-3 -4)"
  (let ((result (clysm/tests:compile-and-run-numeric '(- #C(3 4)))))
    (ok (= -3 (realpart result)) "Real part should be -3")
    (ok (= -4 (imagpart result)) "Imaginary part should be -4")))
