;;;; string-trim-test.lisp - Unit tests for ANSI string trim functions codegen
;;;; Feature 001-ansi-string-trim: Phase 16B
;;;;
;;;; ANSI CL References:
;;;; - string-trim: resources/HyperSpec/Body/f_stg_tr.htm
;;;; - string-left-trim: resources/HyperSpec/Body/f_stg_tr.htm
;;;; - string-right-trim: resources/HyperSpec/Body/f_stg_tr.htm
;;;; - nstring-upcase: resources/HyperSpec/Body/f_stg_up.htm
;;;; - nstring-downcase: resources/HyperSpec/Body/f_stg_up.htm
;;;; - nstring-capitalize: resources/HyperSpec/Body/f_stg_up.htm

(in-package #:clysm/tests/unit/string-trim)

;;; ============================================================
;;; User Story 1: String Trimming (Priority: P1) - MVP
;;; ============================================================

;; T007: Test compile-string-trim basic case
(deftest test-string-trim-compiles ()
  "Verify string-trim expression compiles without error."
  (testing "string-trim compilation succeeds (T007)"
    (let ((form '(defun trim-test (s)
                   (string-trim " " s))))
      (ok (clysm:compile-to-wasm form)
          "string-trim should compile to Wasm"))))

;; T008: Test compile-string-left-trim basic case
(deftest test-string-left-trim-compiles ()
  "Verify string-left-trim expression compiles without error."
  (testing "string-left-trim compilation succeeds (T008)"
    (let ((form '(defun ltrim-test (s)
                   (string-left-trim " " s))))
      (ok (clysm:compile-to-wasm form)
          "string-left-trim should compile to Wasm"))))

;; T009: Test compile-string-right-trim basic case
(deftest test-string-right-trim-compiles ()
  "Verify string-right-trim expression compiles without error."
  (testing "string-right-trim compilation succeeds (T009)"
    (let ((form '(defun rtrim-test (s)
                   (string-right-trim " " s))))
      (ok (clysm:compile-to-wasm form)
          "string-right-trim should compile to Wasm"))))

;; T010: Test trim with :start/:end bounds
(deftest test-trim-with-bounds-compiles ()
  "Verify trim functions with :start/:end compile without error."
  (testing "string-trim with bounds compilation succeeds (T010)"
    (let ((form '(defun trim-bounds-test (s)
                   (string-trim " " s :start 1 :end 5))))
      (ok (clysm:compile-to-wasm form)
          "string-trim with :start/:end should compile to Wasm"))))

;;; ============================================================
;;; User Story 2: Destructive Case Conversion (Priority: P2)
;;; ============================================================

;; T017: Test compile-nstring-upcase basic case
(deftest test-nstring-upcase-compiles ()
  "Verify nstring-upcase expression compiles without error."
  (testing "nstring-upcase compilation succeeds (T017)"
    (let ((form '(defun nup-test (s)
                   (nstring-upcase s))))
      (ok (clysm:compile-to-wasm form)
          "nstring-upcase should compile to Wasm"))))

;; T018: Test compile-nstring-downcase basic case
(deftest test-nstring-downcase-compiles ()
  "Verify nstring-downcase expression compiles without error."
  (testing "nstring-downcase compilation succeeds (T018)"
    (let ((form '(defun ndown-test (s)
                   (nstring-downcase s))))
      (ok (clysm:compile-to-wasm form)
          "nstring-downcase should compile to Wasm"))))

;; T019: Test compile-nstring-capitalize basic case
(deftest test-nstring-capitalize-compiles ()
  "Verify nstring-capitalize expression compiles without error."
  (testing "nstring-capitalize compilation succeeds (T019)"
    (let ((form '(defun ncap-test (s)
                   (nstring-capitalize s))))
      (ok (clysm:compile-to-wasm form)
          "nstring-capitalize should compile to Wasm"))))

;; T020: Test nstring with :start/:end bounds
(deftest test-nstring-with-bounds-compiles ()
  "Verify nstring functions with :start/:end compile without error."
  (testing "nstring-upcase with bounds compilation succeeds (T020)"
    (let ((form '(defun nup-bounds-test (s)
                   (nstring-upcase s :start 1 :end 5))))
      (ok (clysm:compile-to-wasm form)
          "nstring-upcase with :start/:end should compile to Wasm"))))

;; T021: Test nstring returns same object (destructive)
(deftest test-nstring-returns-same-object ()
  "Verify nstring-upcase modifies in place and returns same object."
  (testing "nstring-upcase returns same object (T021)"
    ;; This test verifies the compilation succeeds - actual same-object
    ;; verification requires runtime testing
    (let ((form '(defun nup-same-test (s)
                   (let ((result (nstring-upcase s)))
                     ;; result should be eq to s
                     result))))
      (ok (clysm:compile-to-wasm form)
          "nstring-upcase should compile for same-object test"))))

;;; ============================================================
;;; User Story 3: ANSI CL Compliance (Priority: P3)
;;; ============================================================

;; T028: Test empty string edge case
(deftest test-trim-empty-string-compiles ()
  "Verify trim handles empty string."
  (testing "string-trim with empty string compiles (T028)"
    (let ((form '(defun trim-empty-test ()
                   (string-trim " " ""))))
      (ok (clysm:compile-to-wasm form)
          "string-trim on empty string should compile"))))

;; T029: Test character bag containing all chars
(deftest test-trim-all-chars-compiles ()
  "Verify trim when character bag contains all string chars."
  (testing "string-trim all chars compiles (T029)"
    (let ((form '(defun trim-all-test (s bag)
                   (string-trim bag s))))
      (ok (clysm:compile-to-wasm form)
          "string-trim with variable bag should compile"))))

;; T030: Test nil/empty character bag
(deftest test-trim-empty-bag-compiles ()
  "Verify trim with nil/empty character bag."
  (testing "string-trim with empty bag compiles (T030)"
    (let ((form '(defun trim-empty-bag-test (s)
                   (string-trim "" s))))
      (ok (clysm:compile-to-wasm form)
          "string-trim with empty bag should compile"))))

;; T031: Test :start equals :end (no-op)
(deftest test-trim-start-equals-end-compiles ()
  "Verify trim when :start equals :end."
  (testing "string-trim :start=:end compiles (T031)"
    (let ((form '(defun trim-noop-test (s)
                   (string-trim " " s :start 2 :end 2))))
      (ok (clysm:compile-to-wasm form)
          "string-trim with :start=:end should compile"))))

;; T032: Test character bag as list
(deftest test-trim-list-bag-compiles ()
  "Verify trim accepts character bag as list."
  (testing "string-trim with list bag compiles (T032)"
    (let ((form '(defun trim-list-bag-test (s)
                   (string-trim '(#\Space #\Tab) s))))
      (ok (clysm:compile-to-wasm form)
          "string-trim with list bag should compile"))))

;; T033: Test type error on invalid arguments
(deftest test-trim-type-check-compiles ()
  "Verify trim type checking compiles."
  (testing "string-trim type checking compiles (T033)"
    ;; This test verifies the function compiles;
    ;; actual type error testing requires runtime
    (let ((form '(defun trim-type-test (s bag)
                   (string-trim bag s))))
      (ok (clysm:compile-to-wasm form)
          "string-trim should compile for type check test"))))

;; T034: Test bounds error on out-of-range indices
(deftest test-trim-bounds-check-compiles ()
  "Verify trim bounds checking compiles."
  (testing "string-trim bounds checking compiles (T034)"
    (let ((form '(defun trim-bounds-check-test (s start end)
                   (string-trim " " s :start start :end end))))
      (ok (clysm:compile-to-wasm form)
          "string-trim should compile for bounds check test"))))
