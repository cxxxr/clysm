;;;; array-primitives-test.lisp - Unit tests for array primitive codegen
;;;; Feature 001-ansi-array-primitives: Phase 13D-1
;;;;
;;;; ANSI CL References:
;;;; - aref: resources/HyperSpec/Body/f_aref.htm
;;;; - svref: resources/HyperSpec/Body/f_svref.htm
;;;; - schar: resources/HyperSpec/Body/f_schar.htm
;;;; - elt: resources/HyperSpec/Body/f_elt.htm
;;;; - coerce: resources/HyperSpec/Body/f_coerce.htm

(in-package #:clysm/tests/unit/array-primitives)

;;; ============================================================
;;; User Story 1: Array Element Access (aref, svref)
;;; Priority: P1 - Enables 90 defstruct accessor forms to compile
;;; ============================================================

(deftest test-aref-compiles ()
  "Verify aref expression compiles without error."
  (testing "aref compilation succeeds (T008)"
    (let ((form '(defun aref-test (v)
                   (aref v 0))))
      ;; Should compile without error
      (ok (clysm:compile-to-wasm form)
          "aref should compile to Wasm"))))

(deftest test-svref-compiles ()
  "Verify svref expression compiles without error."
  (testing "svref compilation succeeds (T009)"
    (let ((form '(defun svref-test (sv)
                   (svref sv 1))))
      (ok (clysm:compile-to-wasm form)
          "svref should compile to Wasm"))))

(deftest test-setf-aref-compiles ()
  "Verify setf aref expression compiles without error."
  (testing "(setf aref) compilation succeeds (T010)"
    (let ((form '(defun setf-aref-test (v val)
                   (setf (aref v 0) val))))
      (ok (clysm:compile-to-wasm form)
          "(setf aref) should compile to Wasm"))))

;;; ============================================================
;;; User Story 2: Sequence Coercion (coerce)
;;; Priority: P2 - Enables leb128.lisp to compile
;;; ============================================================

(deftest test-coerce-compiles ()
  "Verify coerce expression compiles without error."
  (testing "coerce compilation succeeds (T020)"
    (let ((form '(defun coerce-test (x)
                   (coerce x 'vector))))
      (ok (clysm:compile-to-wasm form)
          "coerce should compile to Wasm"))))

(deftest test-coerce-to-list-compiles ()
  "Verify coerce to list expression compiles without error."
  (testing "coerce to list compilation succeeds (T021)"
    (let ((form '(defun coerce-list-test (x)
                   (coerce x 'list))))
      (ok (clysm:compile-to-wasm form)
          "coerce to list should compile to Wasm"))))

;;; ============================================================
;;; User Story 3: String Character Access (schar)
;;; Priority: P3 - Already implemented, verify it works
;;; ============================================================

(deftest test-schar-compiles ()
  "Verify schar expression compiles without error."
  (testing "schar compilation succeeds (T031)"
    (let ((form '(defun schar-test (s)
                   (schar s 0))))
      (ok (clysm:compile-to-wasm form)
          "schar should compile to Wasm"))))

(deftest test-setf-schar-compiles ()
  "Verify (setf schar) expression compiles without error."
  (testing "(setf schar) compilation succeeds (T032)"
    (let ((form '(defun setf-schar-test (s c)
                   (setf (schar s 0) c))))
      (ok (clysm:compile-to-wasm form)
          "(setf schar) should compile to Wasm"))))

;;; ============================================================
;;; User Story 4: Generic Sequence Element Access (elt)
;;; Priority: P4 - Enables generic sequence code
;;; ============================================================

(deftest test-elt-compiles ()
  "Verify elt expression compiles without error."
  (testing "elt compilation succeeds (T040-T042)"
    (let ((form '(defun elt-test (seq)
                   (elt seq 1))))
      (ok (clysm:compile-to-wasm form)
          "elt should compile to Wasm"))))

(deftest test-setf-elt-compiles ()
  "Verify (setf elt) expression compiles without error."
  (testing "(setf elt) compilation succeeds"
    (let ((form '(defun setf-elt-test (seq val)
                   (setf (elt seq 0) val))))
      (ok (clysm:compile-to-wasm form)
          "(setf elt) should compile to Wasm"))))
