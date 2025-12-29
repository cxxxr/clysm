;;;; array-test.lisp - Integration tests for array primitives
;;;; Feature 001-ansi-array-primitives: Phase 13D-1
;;;;
;;;; Integration tests verify end-to-end compilation and runtime execution.
;;;;
;;;; ANSI CL References:
;;;; - aref: resources/HyperSpec/Body/f_aref.htm
;;;; - svref: resources/HyperSpec/Body/f_svref.htm
;;;; - schar: resources/HyperSpec/Body/f_schar.htm
;;;; - elt: resources/HyperSpec/Body/f_elt.htm
;;;; - coerce: resources/HyperSpec/Body/f_coerce.htm

(in-package #:clysm/tests/integration/array)

;;; ============================================================
;;; User Story 1: aref/svref Integration Tests
;;; ============================================================

(deftest test-aref-basic-access ()
  "Integration test: (aref #(1 2 3) 1) => 2"
  (testing "aref returns correct element (T012)"
    (let ((result (clysm/tests:compile-and-run
                   '((aref #(1 2 3) 1)))))
      (ok (= result 2) "aref should return element at index"))))

(deftest test-aref-first-element ()
  "Integration test: (aref #(10 20 30) 0) => 10"
  (testing "aref index 0 returns first element"
    (let ((result (clysm/tests:compile-and-run
                   '((aref #(10 20 30) 0)))))
      (ok (= result 10) "aref 0 should return first element"))))

(deftest test-aref-last-element ()
  "Integration test: (aref #(10 20 30) 2) => 30"
  (testing "aref last index returns last element"
    (let ((result (clysm/tests:compile-and-run
                   '((aref #(10 20 30) 2)))))
      (ok (= result 30) "aref 2 should return last element"))))

(deftest test-svref-basic-access ()
  "Integration test: (svref #(a b c) 1) => b"
  (testing "svref returns correct element"
    (let ((result (clysm/tests:compile-and-run
                   '((svref #(1 2 3) 1)))))
      (ok (= result 2) "svref should return element at index"))))

(deftest test-setf-aref-modifies ()
  "Integration test: (setf (aref v 1) 99) modifies vector"
  (testing "setf aref modifies and returns value"
    (let ((result (clysm/tests:compile-and-run
                   '((let ((v (make-array 3 :initial-contents '(1 2 3))))
                       (setf (aref v 1) 99)
                       (aref v 1))))))
      (ok (= result 99) "(setf aref) should modify element"))))

;;; ============================================================
;;; User Story 2: coerce Integration Tests
;;; ============================================================

(deftest test-coerce-list-to-vector ()
  "Integration test: (coerce '(1 2 3) 'vector) => #(1 2 3)"
  (testing "coerce list to vector (T023)"
    (let ((result (clysm/tests:compile-and-run
                   '((let ((v (coerce '(1 2 3) 'vector)))
                       (aref v 1))))))
      (ok (= result 2) "coerced vector should have correct elements"))))

(deftest test-coerce-vector-to-list ()
  "Integration test: (coerce #(a b c) 'list) => (a b c)"
  (testing "coerce vector to list"
    (let ((result (clysm/tests:compile-and-run
                   '((let ((lst (coerce #(1 2 3) 'list)))
                       (second lst))))))
      (ok (= result 2) "coerced list should have correct elements"))))

(deftest test-coerce-identity ()
  "Integration test: coerce to same type returns equivalent value"
  (testing "coerce identity preserves value"
    (let ((result (clysm/tests:compile-and-run
                   '((aref (coerce #(1 2 3) 'vector) 0)))))
      (ok (= result 1) "coerce identity should preserve elements"))))

;;; ============================================================
;;; User Story 3: schar Integration Tests
;;; ============================================================

(deftest test-schar-basic-access ()
  "Integration test: (schar \"hello\" 0) => #\\h"
  (testing "schar returns correct character (T034)"
    (let ((result (clysm/tests:compile-and-run
                   '((char-code (schar "hello" 0))))))
      (ok (= result 104) "schar should return 'h' (code 104)"))))

(deftest test-schar-middle-char ()
  "Integration test: (schar \"hello\" 2) => #\\l"
  (testing "schar middle character"
    (let ((result (clysm/tests:compile-and-run
                   '((char-code (schar "hello" 2))))))
      (ok (= result 108) "schar should return 'l' (code 108)"))))

(deftest test-schar-last-char ()
  "Integration test: (schar \"hello\" 4) => #\\o"
  (testing "schar last character"
    (let ((result (clysm/tests:compile-and-run
                   '((char-code (schar "hello" 4))))))
      (ok (= result 111) "schar should return 'o' (code 111)"))))

;;; ============================================================
;;; User Story 4: elt Integration Tests
;;; ============================================================

(deftest test-elt-on-vector ()
  "Integration test: (elt #(1 2 3) 1) => 2"
  (testing "elt on vector returns correct element (T044)"
    (let ((result (clysm/tests:compile-and-run
                   '((elt #(1 2 3) 1)))))
      (ok (= result 2) "elt on vector should return element"))))

(deftest test-elt-on-list ()
  "Integration test: (elt '(a b c) 1) => b"
  (testing "elt on list returns correct element"
    (let ((result (clysm/tests:compile-and-run
                   '((elt '(10 20 30) 1)))))
      (ok (= result 20) "elt on list should return element"))))

(deftest test-elt-on-string ()
  "Integration test: (elt \"hello\" 0) => #\\h"
  (testing "elt on string returns correct character"
    (let ((result (clysm/tests:compile-and-run
                   '((char-code (elt "hello" 0))))))
      (ok (= result 104) "elt on string should return character"))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(deftest test-aref-empty-vector-error ()
  "Integration test: (aref #() 0) signals error"
  (testing "aref on empty vector should error"
    ;; Wasm will trap on out-of-bounds access
    (skip "Error handling tests require wasmtime trap detection")))

(deftest test-elt-uniform-behavior ()
  "Integration test: elt behaves uniformly across sequence types"
  (testing "elt works on all sequence types"
    ;; Test that elt(seq, 0) returns first element for all types
    (let ((vec-result (clysm/tests:compile-and-run
                       '((elt #(42 1 2) 0))))
          (list-result (clysm/tests:compile-and-run
                        '((elt '(42 1 2) 0)))))
      (ok (= vec-result 42) "elt on vector returns first")
      (ok (= list-result 42) "elt on list returns first"))))
