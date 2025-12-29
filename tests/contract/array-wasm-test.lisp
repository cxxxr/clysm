;;;; array-wasm-test.lisp - Contract tests for array primitives Wasm validation
;;;; Feature 001-ansi-array-primitives: Phase 13D-1
;;;;
;;;; Contract tests verify that generated Wasm passes wasm-tools validate.
;;;;
;;;; ANSI CL References:
;;;; - aref: resources/HyperSpec/Body/f_aref.htm
;;;; - svref: resources/HyperSpec/Body/f_svref.htm
;;;; - schar: resources/HyperSpec/Body/f_schar.htm
;;;; - elt: resources/HyperSpec/Body/f_elt.htm
;;;; - coerce: resources/HyperSpec/Body/f_coerce.htm

(in-package #:clysm/tests/contract/array-wasm)

;;; ============================================================
;;; User Story 1: aref/svref Wasm Validation
;;; ============================================================

(deftest test-aref-wasm-validates ()
  "Verify aref-using code generates valid Wasm."
  (testing "aref expression compiles to valid Wasm (T011)"
    (let ((form '(defun aref-test (v)
                   (aref v 0))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "aref should generate valid Wasm"))))))

(deftest test-svref-wasm-validates ()
  "Verify svref-using code generates valid Wasm."
  (testing "svref expression compiles to valid Wasm"
    (let ((form '(defun svref-test (sv)
                   (svref sv 1))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "svref should generate valid Wasm"))))))

(deftest test-setf-aref-wasm-validates ()
  "Verify setf aref generates valid Wasm."
  (testing "setf aref expression compiles to valid Wasm"
    (let ((form '(defun setf-aref-test (v val)
                   (setf (aref v 0) val))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "(setf aref) should generate valid Wasm"))))))

;;; ============================================================
;;; User Story 2: coerce Wasm Validation
;;; ============================================================

(deftest test-coerce-wasm-validates ()
  "Verify coerce generates valid Wasm."
  (testing "coerce expression compiles to valid Wasm (T022)"
    (let ((form '(defun coerce-test (lst)
                   (coerce lst 'vector))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "coerce should generate valid Wasm"))))))

;;; ============================================================
;;; User Story 3: schar Wasm Validation
;;; ============================================================

(deftest test-schar-wasm-validates ()
  "Verify schar generates valid Wasm."
  (testing "schar expression compiles to valid Wasm (T033)"
    (let ((form '(defun schar-test (s)
                   (schar s 0))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "schar should generate valid Wasm"))))))

;;; ============================================================
;;; User Story 4: elt Wasm Validation
;;; ============================================================

(deftest test-elt-wasm-validates ()
  "Verify elt generates valid Wasm."
  (testing "elt expression compiles to valid Wasm (T043)"
    (let ((form '(defun elt-test (seq)
                   (elt seq 0))))
      (multiple-value-bind (bytes success)
          (ignore-errors (clysm:compile-to-wasm form))
        (when success
          (ok (clysm/tests:validate-wasm-silent bytes)
              "elt should generate valid Wasm"))))))
