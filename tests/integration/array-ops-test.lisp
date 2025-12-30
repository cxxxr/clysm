;;;; array-ops-test.lisp - Integration tests for ANSI array operations
;;;; Feature 001-ansi-array-ops: Phase 15C
;;;;
;;;; Integration tests verify end-to-end compilation and runtime execution
;;;; against SBCL reference behavior.
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

(in-package #:clysm/tests/integration/array-ops)

;;; ============================================================
;;; User Story 1: Query Array Dimensions and Structure (Priority: P1)
;;; T020: Integration test for array metadata
;;; ============================================================

(deftest test-array-rank-simple-vector ()
  "Integration test: (array-rank #(1 2 3)) => 1"
  (testing "array-rank returns 1 for simple-vector (T020)"
    (let ((result (clysm/tests:compile-and-run
                   '((array-rank #(1 2 3))))))
      (ok (= result 1) "array-rank should return 1 for simple-vector"))))

(deftest test-array-rank-2d ()
  "Integration test: (array-rank (make-array '(2 3))) => 2"
  (testing "array-rank returns 2 for 2D array"
    (let ((result (clysm/tests:compile-and-run
                   '((array-rank (make-array '(2 3)))))))
      (ok (= result 2) "array-rank should return 2 for 2D array"))))

(deftest test-array-rank-3d ()
  "Integration test: (array-rank (make-array '(2 3 4))) => 3"
  (testing "array-rank returns 3 for 3D array"
    (let ((result (clysm/tests:compile-and-run
                   '((array-rank (make-array '(2 3 4)))))))
      (ok (= result 3) "array-rank should return 3 for 3D array"))))

(deftest test-array-dimension-axis-0 ()
  "Integration test: (array-dimension (make-array '(2 3 4)) 0) => 2"
  (testing "array-dimension returns size of axis 0"
    (let ((result (clysm/tests:compile-and-run
                   '((array-dimension (make-array '(2 3 4)) 0)))))
      (ok (= result 2) "array-dimension axis 0 should return 2"))))

(deftest test-array-dimension-axis-1 ()
  "Integration test: (array-dimension (make-array '(2 3 4)) 1) => 3"
  (testing "array-dimension returns size of axis 1"
    (let ((result (clysm/tests:compile-and-run
                   '((array-dimension (make-array '(2 3 4)) 1)))))
      (ok (= result 3) "array-dimension axis 1 should return 3"))))

(deftest test-array-dimensions-3d ()
  "Integration test: (array-dimensions (make-array '(2 3 4))) => (2 3 4)"
  (testing "array-dimensions returns list of dimensions"
    (let ((result (clysm/tests:compile-and-run
                   '((length (array-dimensions (make-array '(2 3 4))))))))
      (ok (= result 3) "array-dimensions should return 3-element list"))))

(deftest test-array-total-size-3d ()
  "Integration test: (array-total-size (make-array '(2 3 4))) => 24"
  (testing "array-total-size returns product of dimensions (T021)"
    (let ((result (clysm/tests:compile-and-run
                   '((array-total-size (make-array '(2 3 4)))))))
      (ok (= result 24) "array-total-size should return 24 (2*3*4)"))))

(deftest test-array-total-size-simple-vector ()
  "Integration test: (array-total-size #(1 2 3 4 5)) => 5"
  (testing "array-total-size returns length for simple-vector"
    (let ((result (clysm/tests:compile-and-run
                   '((array-total-size #(1 2 3 4 5))))))
      (ok (= result 5) "array-total-size should return 5"))))

;;; ============================================================
;;; User Story 2: Access Elements by Row-Major Index (Priority: P2)
;;; T033: Integration test for row-major access
;;; ============================================================

(deftest test-array-row-major-index-2d ()
  "Integration test: (array-row-major-index arr 1 2) => 6 for 3x4 array"
  (testing "array-row-major-index computes correct index (T033)"
    (let ((result (clysm/tests:compile-and-run
                   '((array-row-major-index (make-array '(3 4)) 1 2)))))
      (ok (= result 6) "row-major-index(1,2) in 3x4 should be 6"))))

(deftest test-array-row-major-index-3d ()
  "Integration test: (array-row-major-index arr 1 2 3) => 23 for 2x3x4"
  (testing "array-row-major-index computes correct index for 3D"
    (let ((result (clysm/tests:compile-and-run
                   '((array-row-major-index (make-array '(2 3 4)) 1 2 3)))))
      (ok (= result 23) "row-major-index(1,2,3) in 2x3x4 should be 23"))))

(deftest test-row-major-aref-access ()
  "Integration test: (row-major-aref arr 6) accesses correct element"
  (testing "row-major-aref returns element at linear index"
    (let ((result (clysm/tests:compile-and-run
                   '((let ((arr (make-array '(3 4) :initial-element 0)))
                       (setf (aref arr 1 2) 42)
                       (row-major-aref arr 6))))))
      (ok (= result 42) "row-major-aref(6) should return element at (1,2)"))))

(deftest test-setf-row-major-aref ()
  "Integration test: (setf (row-major-aref arr 6) val) sets element"
  (testing "(setf row-major-aref) modifies element at linear index (T034)"
    (let ((result (clysm/tests:compile-and-run
                   '((let ((arr (make-array '(3 4) :initial-element 0)))
                       (setf (row-major-aref arr 6) 99)
                       (aref arr 1 2))))))
      (ok (= result 99) "(setf row-major-aref 6 99) should set element at (1,2)"))))

;;; ============================================================
;;; User Story 3: Check and Modify Array Adjustability (Priority: P3)
;;; T043: Integration test for adjustability
;;; ============================================================

(deftest test-adjustable-array-p-true ()
  "Integration test: (adjustable-array-p arr) => T for adjustable"
  (testing "adjustable-array-p returns T for adjustable arrays (T043)"
    (let ((result (clysm/tests:compile-and-run
                   '((if (adjustable-array-p
                          (make-array '(2 3) :adjustable t))
                         1 0)))))
      (ok (= result 1) "adjustable-array-p should return T"))))

(deftest test-adjustable-array-p-false ()
  "Integration test: (adjustable-array-p arr) => NIL for non-adjustable"
  (testing "adjustable-array-p returns NIL for non-adjustable arrays"
    (let ((result (clysm/tests:compile-and-run
                   '((if (adjustable-array-p
                          (make-array '(2 3) :adjustable nil))
                         1 0)))))
      (ok (= result 0) "adjustable-array-p should return NIL"))))

(deftest test-adjust-array-grow ()
  "Integration test: adjust-array increases dimensions"
  (testing "adjust-array grows array and preserves elements (T044)"
    (let ((result (clysm/tests:compile-and-run
                   '((let ((arr (make-array '(2 3)
                                            :adjustable t
                                            :initial-element 0)))
                       (setf (aref arr 0 0) 42)
                       (let ((new-arr (adjust-array arr '(4 5))))
                         (aref new-arr 0 0)))))))
      (ok (= result 42) "adjust-array should preserve existing element"))))

(deftest test-adjust-array-shrink ()
  "Integration test: adjust-array shrinks dimensions"
  (testing "adjust-array shrinks array"
    (let ((result (clysm/tests:compile-and-run
                   '((let ((arr (make-array 10 :adjustable t)))
                       (array-total-size (adjust-array arr 5)))))))
      (ok (= result 5) "adjust-array should shrink to new size"))))

(deftest test-adjust-array-initial-element ()
  "Integration test: adjust-array fills new positions"
  (testing "adjust-array fills new positions with :initial-element"
    (let ((result (clysm/tests:compile-and-run
                   '((let ((arr (make-array 3 :adjustable t :initial-element 0)))
                       (let ((new-arr (adjust-array arr 5 :initial-element 99)))
                         (aref new-arr 4)))))))
      (ok (= result 99) "new positions should have :initial-element value"))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(deftest test-array-rank-0d ()
  "Integration test: (array-rank (make-array '())) => 0"
  (testing "array-rank returns 0 for 0-dimensional array"
    (let ((result (clysm/tests:compile-and-run
                   '((array-rank (make-array '()))))))
      (ok (= result 0) "0D array should have rank 0"))))

(deftest test-array-dimensions-0d ()
  "Integration test: (array-dimensions (make-array '())) => NIL"
  (testing "array-dimensions returns NIL for 0-dimensional array"
    (let ((result (clysm/tests:compile-and-run
                   '((if (null (array-dimensions (make-array '()))) 1 0)))))
      (ok (= result 1) "0D array should have NIL dimensions"))))

(deftest test-array-total-size-0d ()
  "Integration test: (array-total-size (make-array '())) => 1"
  (testing "array-total-size returns 1 for 0-dimensional array"
    (let ((result (clysm/tests:compile-and-run
                   '((array-total-size (make-array '()))))))
      (ok (= result 1) "0D array should have total-size 1"))))
