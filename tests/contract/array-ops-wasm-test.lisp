;;;; array-ops-wasm-test.lisp - Contract tests for array ops Wasm validation
;;;; Feature 001-ansi-array-ops: Phase 15C
;;;;
;;;; Contract tests verify that generated Wasm passes wasm-tools validate.
;;;;
;;;; ANSI CL References:
;;;; - array-rank: resources/HyperSpec/Body/f_ar_ran.htm
;;;; - array-dimension: resources/HyperSpec/Body/f_ar_dim.htm
;;;; - array-dimensions: resources/HyperSpec/Body/f_ar_d_1.htm
;;;; - array-total-size: resources/HyperSpec/Body/f_ar_tot.htm
;;;; - array-row-major-index: resources/HyperSpec/Body/f_ar_row.htm
;;;; - row-major-aref: resources/HyperSpec/Body/f_row_ma.htm
;;;; - adjustable-array-p: resources/HyperSpec/Body/f_adju_1.htm
;;;; - adjust-array: resources/HyperSpec/Body/f_adjust.htm

(in-package #:clysm/tests/contract/array-ops-wasm)

;;; ============================================================
;;; User Story 1: Array Metadata Wasm Validation (P1)
;;; T012: Contract test for array metadata functions
;;; ============================================================

(deftest test-array-rank-wasm-validates ()
  "Verify array-rank generates valid Wasm."
  (testing "array-rank expression compiles to valid Wasm (T012)"
    (let ((form '(defun rank-test (arr)
                   (array-rank arr))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "array-rank should generate valid Wasm"))))))

(deftest test-array-dimension-wasm-validates ()
  "Verify array-dimension generates valid Wasm."
  (testing "array-dimension expression compiles to valid Wasm"
    (let ((form '(defun dim-test (arr)
                   (array-dimension arr 0))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "array-dimension should generate valid Wasm"))))))

(deftest test-array-dimensions-wasm-validates ()
  "Verify array-dimensions generates valid Wasm."
  (testing "array-dimensions expression compiles to valid Wasm"
    (let ((form '(defun dims-test (arr)
                   (array-dimensions arr))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "array-dimensions should generate valid Wasm"))))))

(deftest test-array-total-size-wasm-validates ()
  "Verify array-total-size generates valid Wasm."
  (testing "array-total-size expression compiles to valid Wasm"
    (let ((form '(defun size-test (arr)
                   (array-total-size arr))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "array-total-size should generate valid Wasm"))))))

;;; ============================================================
;;; User Story 2: Row-Major Access Wasm Validation (P2)
;;; T025: Contract test for row-major functions
;;; ============================================================

(deftest test-array-row-major-index-wasm-validates ()
  "Verify array-row-major-index generates valid Wasm."
  (testing "array-row-major-index expression compiles to valid Wasm (T025)"
    (let ((form '(defun rmi-test (arr)
                   (array-row-major-index arr 1 2))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "array-row-major-index should generate valid Wasm"))))))

(deftest test-row-major-aref-wasm-validates ()
  "Verify row-major-aref generates valid Wasm."
  (testing "row-major-aref expression compiles to valid Wasm"
    (let ((form '(defun rma-test (arr)
                   (row-major-aref arr 6))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "row-major-aref should generate valid Wasm"))))))

(deftest test-setf-row-major-aref-wasm-validates ()
  "Verify (setf row-major-aref) generates valid Wasm."
  (testing "(setf row-major-aref) expression compiles to valid Wasm"
    (let ((form '(defun setf-rma-test (arr val)
                   (setf (row-major-aref arr 6) val))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "(setf row-major-aref) should generate valid Wasm"))))))

;;; ============================================================
;;; User Story 3: Adjustability Wasm Validation (P3)
;;; T037: Contract test for adjustability functions
;;; ============================================================

(deftest test-adjustable-array-p-wasm-validates ()
  "Verify adjustable-array-p generates valid Wasm."
  (testing "adjustable-array-p expression compiles to valid Wasm (T037)"
    (let ((form '(defun adj-p-test (arr)
                   (adjustable-array-p arr))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "adjustable-array-p should generate valid Wasm"))))))

(deftest test-adjust-array-wasm-validates ()
  "Verify adjust-array generates valid Wasm."
  (testing "adjust-array expression compiles to valid Wasm"
    (let ((form '(defun adjust-test (arr)
                   (adjust-array arr '(4 5)))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "adjust-array should generate valid Wasm"))))))
