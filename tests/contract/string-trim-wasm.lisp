;;;; string-trim-wasm.lisp - Wasm contract tests for ANSI string trim functions
;;;; Feature 001-ansi-string-trim: Phase 16B
;;;;
;;;; ANSI CL References:
;;;; - string-trim: resources/HyperSpec/Body/f_stg_tr.htm
;;;; - nstring-upcase: resources/HyperSpec/Body/f_stg_up.htm
;;;;
;;;; These tests verify that the generated Wasm is valid.

(in-package #:clysm/tests/contract/string-trim-wasm)

;;; ============================================================
;;; User Story 1: String Trimming - Wasm Validation (Priority: P1)
;;; ============================================================

;; T011: Contract test - string-trim compiles to valid WasmGC
(deftest test-string-trim-wasm-valid ()
  "Verify string-trim produces valid WasmGC output."
  (testing "string-trim Wasm validation (T011)"
    (let ((form '(defun trim-wasm-test (s)
                   (string-trim " " s))))
      (ok (validate-wasm-silent form)
          "string-trim should produce valid Wasm"))))

;; Additional validation for string-left-trim
(deftest test-string-left-trim-wasm-valid ()
  "Verify string-left-trim produces valid WasmGC output."
  (testing "string-left-trim Wasm validation"
    (let ((form '(defun ltrim-wasm-test (s)
                   (string-left-trim " " s))))
      (ok (validate-wasm-silent form)
          "string-left-trim should produce valid Wasm"))))

;; Additional validation for string-right-trim
(deftest test-string-right-trim-wasm-valid ()
  "Verify string-right-trim produces valid WasmGC output."
  (testing "string-right-trim Wasm validation"
    (let ((form '(defun rtrim-wasm-test (s)
                   (string-right-trim " " s))))
      (ok (validate-wasm-silent form)
          "string-right-trim should produce valid Wasm"))))

;;; ============================================================
;;; User Story 2: Destructive Case Conversion - Wasm Validation (Priority: P2)
;;; ============================================================

;; T022: Contract test - nstring-upcase compiles to valid WasmGC
(deftest test-nstring-upcase-wasm-valid ()
  "Verify nstring-upcase produces valid WasmGC output."
  (testing "nstring-upcase Wasm validation (T022)"
    (let ((form '(defun nup-wasm-test (s)
                   (nstring-upcase s))))
      (ok (validate-wasm-silent form)
          "nstring-upcase should produce valid Wasm"))))

;; Additional validation for nstring-downcase
(deftest test-nstring-downcase-wasm-valid ()
  "Verify nstring-downcase produces valid WasmGC output."
  (testing "nstring-downcase Wasm validation"
    (let ((form '(defun ndown-wasm-test (s)
                   (nstring-downcase s))))
      (ok (validate-wasm-silent form)
          "nstring-downcase should produce valid Wasm"))))

;; Additional validation for nstring-capitalize
(deftest test-nstring-capitalize-wasm-valid ()
  "Verify nstring-capitalize produces valid WasmGC output."
  (testing "nstring-capitalize Wasm validation"
    (let ((form '(defun ncap-wasm-test (s)
                   (nstring-capitalize s))))
      (ok (validate-wasm-silent form)
          "nstring-capitalize should produce valid Wasm"))))

;;; ============================================================
;;; Combined Wasm Validation - All Six Functions
;;; ============================================================

(deftest test-all-string-trim-functions-wasm-valid ()
  "Verify all six string-trim/nstring functions produce valid Wasm."
  (testing "All string-trim functions Wasm validation"
    (let ((form '(defun all-trim-test (s bag)
                   (let* ((t1 (string-trim bag s))
                          (t2 (string-left-trim bag s))
                          (t3 (string-right-trim bag s))
                          (t4 (nstring-upcase s))
                          (t5 (nstring-downcase s))
                          (t6 (nstring-capitalize s)))
                     (list t1 t2 t3 t4 t5 t6)))))
      (ok (validate-wasm-silent form)
          "All six functions should produce valid Wasm together"))))
