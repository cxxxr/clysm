;;;; array-ops-test.lisp - Unit tests for ANSI array operations codegen
;;;; Feature 001-ansi-array-ops: Phase 15C
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

(in-package #:clysm/tests/unit/array-ops)

;;; ============================================================
;;; User Story 1: Query Array Dimensions and Structure (Priority: P1)
;;; ============================================================

;; T008: Test compile-array-rank
(deftest test-array-rank-compiles ()
  "Verify array-rank expression compiles without error."
  (testing "array-rank compilation succeeds (T008)"
    (let ((form '(defun rank-test (arr)
                   (array-rank arr))))
      (ok (clysm:compile-to-wasm form)
          "array-rank should compile to Wasm"))))

;; T009: Test compile-array-dimension
(deftest test-array-dimension-compiles ()
  "Verify array-dimension expression compiles without error."
  (testing "array-dimension compilation succeeds (T009)"
    (let ((form '(defun dim-test (arr)
                   (array-dimension arr 0))))
      (ok (clysm:compile-to-wasm form)
          "array-dimension should compile to Wasm"))))

;; T010: Test compile-array-dimensions
(deftest test-array-dimensions-compiles ()
  "Verify array-dimensions expression compiles without error."
  (testing "array-dimensions compilation succeeds (T010)"
    (let ((form '(defun dims-test (arr)
                   (array-dimensions arr))))
      (ok (clysm:compile-to-wasm form)
          "array-dimensions should compile to Wasm"))))

;; T011: Test compile-array-total-size
(deftest test-array-total-size-compiles ()
  "Verify array-total-size expression compiles without error."
  (testing "array-total-size compilation succeeds (T011)"
    (let ((form '(defun size-test (arr)
                   (array-total-size arr))))
      (ok (clysm:compile-to-wasm form)
          "array-total-size should compile to Wasm"))))

;;; ============================================================
;;; User Story 2: Access Elements by Row-Major Index (Priority: P2)
;;; ============================================================

;; T022: Test compile-array-row-major-index
(deftest test-array-row-major-index-compiles ()
  "Verify array-row-major-index expression compiles without error."
  (testing "array-row-major-index compilation succeeds (T022)"
    (let ((form '(defun rmi-test (arr)
                   (array-row-major-index arr 1 2))))
      (ok (clysm:compile-to-wasm form)
          "array-row-major-index should compile to Wasm"))))

;; T023: Test compile-row-major-aref
(deftest test-row-major-aref-compiles ()
  "Verify row-major-aref expression compiles without error."
  (testing "row-major-aref compilation succeeds (T023)"
    (let ((form '(defun rma-test (arr)
                   (row-major-aref arr 6))))
      (ok (clysm:compile-to-wasm form)
          "row-major-aref should compile to Wasm"))))

;; T024: Test compile-setf-row-major-aref
(deftest test-setf-row-major-aref-compiles ()
  "Verify (setf row-major-aref) expression compiles without error."
  (testing "(setf row-major-aref) compilation succeeds (T024)"
    (let ((form '(defun setf-rma-test (arr val)
                   (setf (row-major-aref arr 6) val))))
      (ok (clysm:compile-to-wasm form)
          "(setf row-major-aref) should compile to Wasm"))))

;;; ============================================================
;;; User Story 3: Check and Modify Array Adjustability (Priority: P3)
;;; ============================================================

;; T035: Test compile-adjustable-array-p
(deftest test-adjustable-array-p-compiles ()
  "Verify adjustable-array-p expression compiles without error."
  (testing "adjustable-array-p compilation succeeds (T035)"
    (let ((form '(defun adj-p-test (arr)
                   (adjustable-array-p arr))))
      (ok (clysm:compile-to-wasm form)
          "adjustable-array-p should compile to Wasm"))))

;; T036: Test compile-adjust-array
(deftest test-adjust-array-compiles ()
  "Verify adjust-array expression compiles without error."
  (testing "adjust-array compilation succeeds (T036)"
    (let ((form '(defun adjust-test (arr)
                   (adjust-array arr '(4 5)))))
      (ok (clysm:compile-to-wasm form)
          "adjust-array should compile to Wasm"))))

;;; ============================================================
;;; Type Dispatch Helper Tests (Foundational)
;;; ============================================================

;; T006: Test type dispatch helper
(deftest test-type-dispatch-helper ()
  "Verify type dispatch between $mdarray and $mv_array works."
  (testing "type dispatch helper compilation (T006)"
    ;; Test that array-rank dispatches correctly for simple-vector
    (let ((form '(defun dispatch-test (v)
                   (if (simple-vector-p v)
                       1
                       (array-rank v)))))
      (ok (clysm:compile-to-wasm form)
          "type dispatch should work for array functions"))))
