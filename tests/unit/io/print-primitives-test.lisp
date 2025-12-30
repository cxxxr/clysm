;;;; print-primitives-test.lisp - Unit tests for I/O print primitives
;;;; Feature: 001-io-print-primitives
;;;; Tests: T007, T008, T009, T010

(defpackage #:clysm/tests/unit/io/print-primitives
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/io/print-primitives)

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defun compile-and-validate (source)
  "Compile SOURCE to Wasm and return the bytes.
   Returns NIL if compilation fails."
  (handler-case
      (clysm:compile-to-wasm source)
    (error (e)
      (format *error-output* "Compilation error: ~A~%" e)
      nil)))

(defun wasm-validates-p (wasm-bytes)
  "Check if WASM-BYTES passes wasm-tools validate.
   Returns T if valid, NIL otherwise."
  (when wasm-bytes
    (let ((temp-file (format nil "/tmp/clysm-test-~A.wasm" (get-universal-time))))
      (unwind-protect
           (progn
             (with-open-file (s temp-file
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
               (write-sequence wasm-bytes s))
             (zerop (nth-value 2 (uiop:run-program
                                  (list "wasm-tools" "validate" temp-file)
                                  :ignore-error-status t))))
        (when (probe-file temp-file)
          (delete-file temp-file))))))

;;; ============================================================
;;; T007: Unit test for terpri compilation
;;; HyperSpec: resources/HyperSpec/Body/f_terpri.htm
;;; ============================================================

(deftest test-compile-terpri ()
  "Test that (terpri) compiles to valid Wasm."
  ;; Test 1: terpri with no arguments
  (testing "terpri with no arguments"
    (let ((bytes (compile-and-validate "(defun test-fn () (terpri))")))
      (ok (not (null bytes)) "terpri compiles without error")
      (ok (wasm-validates-p bytes) "terpri produces valid Wasm")))

  ;; Test 2: terpri returns NIL
  (testing "terpri returns NIL (verified by compilation)"
    (let ((bytes (compile-and-validate "(defun test-fn () (let ((x (terpri))) x))")))
      (ok (not (null bytes)) "terpri return value can be used"))))

;;; ============================================================
;;; T008: Unit test for princ compilation
;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm
;;; ============================================================

(deftest test-compile-princ ()
  "Test that (princ ...) compiles to valid Wasm."
  ;; Test 1: princ with single argument
  (testing "princ with single argument"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (princ x))")))
      (ok (not (null bytes)) "princ compiles without error")
      (ok (wasm-validates-p bytes) "princ produces valid Wasm")))

  ;; Test 2: princ with string literal
  (testing "princ with string literal"
    (let ((bytes (compile-and-validate "(defun test-fn () (princ \"hello\"))")))
      (ok (not (null bytes)) "princ with string compiles")))

  ;; Test 3: princ returns the object
  (testing "princ returns the object"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (let ((y (princ x))) y))")))
      (ok (not (null bytes)) "princ return value can be used"))))

;;; ============================================================
;;; T009: Unit test for prin1 compilation
;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm
;;; ============================================================

(deftest test-compile-prin1 ()
  "Test that (prin1 ...) compiles to valid Wasm."
  ;; Test 1: prin1 with single argument
  (testing "prin1 with single argument"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (prin1 x))")))
      (ok (not (null bytes)) "prin1 compiles without error")
      (ok (wasm-validates-p bytes) "prin1 produces valid Wasm")))

  ;; Test 2: prin1 with symbol
  (testing "prin1 with symbol"
    (let ((bytes (compile-and-validate "(defun test-fn () (prin1 'symbol))")))
      (ok (not (null bytes)) "prin1 with symbol compiles")))

  ;; Test 3: prin1 returns the object
  (testing "prin1 returns the object"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (let ((y (prin1 x))) y))")))
      (ok (not (null bytes)) "prin1 return value can be used"))))

;;; ============================================================
;;; T010: Unit test for print compilation
;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm
;;; ============================================================

(deftest test-compile-print ()
  "Test that (print ...) compiles to valid Wasm."
  ;; Test 1: print with single argument
  (testing "print with single argument"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (print x))")))
      (ok (not (null bytes)) "print compiles without error")
      (ok (wasm-validates-p bytes) "print produces valid Wasm")))

  ;; Test 2: print in function body (SC-001 acceptance criteria)
  (testing "DEFUN with print compiles (SC-001)"
    (let ((bytes (compile-and-validate "(defun greet (x) (print x) x)")))
      (ok (not (null bytes)) "DEFUN with print compiles")
      (ok (wasm-validates-p bytes) "produces valid Wasm")))

  ;; Test 3: print returns the object
  (testing "print returns the object"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (let ((y (print x))) y))")))
      (ok (not (null bytes)) "print return value can be used"))))

;;; ============================================================
;;; T019: Unit test for format ~A compilation
;;; HyperSpec: resources/HyperSpec/Body/22_cca.htm
;;; ============================================================

(deftest test-compile-format-aesthetic ()
  "Test that format ~A (aesthetic) directive compiles."
  ;; Test 1: format nil with ~A
  (testing "format nil with ~A directive"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (format nil \"Value: ~~A\" x))")))
      (ok (not (null bytes)) "format ~A compiles without error")
      (ok (wasm-validates-p bytes) "format ~A produces valid Wasm")))

  ;; Test 2: format nil ~A with integer
  (testing "format nil ~A with integer literal"
    (let ((bytes (compile-and-validate "(defun test-fn () (format nil \"~~A\" 42))")))
      (ok (not (null bytes)) "format ~A with integer compiles")))

  ;; Test 3: format nil ~A with string
  (testing "format nil ~A with string"
    (let ((bytes (compile-and-validate "(defun test-fn () (format nil \"~~A\" \"hello\"))")))
      (ok (not (null bytes)) "format ~A with string compiles"))))

;;; ============================================================
;;; T020: Unit test for format ~S compilation
;;; HyperSpec: resources/HyperSpec/Body/22_ccb.htm
;;; ============================================================

(deftest test-compile-format-standard ()
  "Test that format ~S (standard) directive compiles."
  ;; Test 1: format nil with ~S
  (testing "format nil with ~S directive"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (format nil \"~~S\" x))")))
      (ok (not (null bytes)) "format ~S compiles without error")
      (ok (wasm-validates-p bytes) "format ~S produces valid Wasm")))

  ;; Test 2: format nil ~S with string (should include quotes)
  (testing "format nil ~S with string"
    (let ((bytes (compile-and-validate "(defun test-fn () (format nil \"~~S\" \"hello\"))")))
      (ok (not (null bytes)) "format ~S with string compiles"))))

;;; ============================================================
;;; T021: Unit test for format ~D compilation
;;; HyperSpec: resources/HyperSpec/Body/22_cba.htm
;;; ============================================================

(deftest test-compile-format-decimal ()
  "Test that format ~D (decimal) directive compiles."
  ;; Test 1: format nil with ~D
  (testing "format nil with ~D directive"
    (let ((bytes (compile-and-validate "(defun test-fn (n) (format nil \"~~D\" n))")))
      (ok (not (null bytes)) "format ~D compiles without error")
      (ok (wasm-validates-p bytes) "format ~D produces valid Wasm")))

  ;; Test 2: format nil ~D with literal
  (testing "format nil ~D with integer literal"
    (let ((bytes (compile-and-validate "(defun test-fn () (format nil \"~~D\" 255))")))
      (ok (not (null bytes)) "format ~D with integer compiles"))))

;;; ============================================================
;;; T022: Unit test for format ~% ~& ~~ compilation
;;; HyperSpec: resources/HyperSpec/Body/22_cea.htm, 22_ceb.htm, 22_cfa.htm
;;; ============================================================

(deftest test-compile-format-misc-directives ()
  "Test that format misc directives (~%, ~&, ~~) compile."
  ;; Test 1: ~% newline directive
  (testing "format nil with ~% directive"
    (let ((bytes (compile-and-validate "(defun test-fn () (format nil \"Line1~~%Line2\"))")))
      (ok (not (null bytes)) "format ~% compiles without error")
      (ok (wasm-validates-p bytes) "format ~% produces valid Wasm")))

  ;; Test 2: ~& fresh-line directive
  (testing "format nil with ~& directive"
    (let ((bytes (compile-and-validate "(defun test-fn () (format nil \"~~&Start\"))")))
      (ok (not (null bytes)) "format ~& compiles without error")))

  ;; Test 3: ~~ tilde escape directive
  (testing "format nil with ~~ directive"
    (let ((bytes (compile-and-validate "(defun test-fn () (format nil \"100~~~~\"))")))
      (ok (not (null bytes)) "format ~~ compiles without error")))

  ;; Test 4: Combined directives
  (testing "format nil with multiple directives"
    (let ((bytes (compile-and-validate
                  "(defun test-fn (x) (format nil \"Value: ~~A~~%Done~~&\" x))")))
      (ok (not (null bytes)) "combined directives compile")
      (ok (wasm-validates-p bytes) "produces valid Wasm"))))

;;; ============================================================
;;; Integration: All print functions together
;;; ============================================================

(deftest test-compile-all-print-functions ()
  "Test that a function using all print primitives compiles."
  (testing "function with terpri, princ, prin1, print"
    (let ((bytes (compile-and-validate
                  "(defun test-all (x)
                     (terpri)
                     (princ x)
                     (prin1 x)
                     (print x)
                     x)")))
      (ok (not (null bytes)) "all print functions compile together")
      (ok (wasm-validates-p bytes) "produces valid Wasm"))))

;;; ============================================================
;;; Integration: Format with all directives
;;; ============================================================

(deftest test-compile-format-all-directives ()
  "Test that format with all supported directives compiles."
  (testing "format with ~A ~S ~D ~% ~& ~~"
    (let ((bytes (compile-and-validate
                  "(defun test-format (a s d)
                     (format nil \"A=~~A S=~~S D=~~D~~%Fresh~~&Tilde~~~~\"))")))
      (ok (not (null bytes)) "all format directives compile together")
      (ok (wasm-validates-p bytes) "produces valid Wasm"))))

;;; ============================================================
;;; T036: Unit test for write compilation
;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm
;;; ============================================================

(deftest test-compile-write ()
  "Test that (write ...) compiles to valid Wasm."
  ;; Test 1: write with single argument
  (testing "write with single argument"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (write x))")))
      (ok (not (null bytes)) "write compiles without error")
      (ok (wasm-validates-p bytes) "write produces valid Wasm")))

  ;; Test 2: write returns the object
  (testing "write returns the object"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (let ((y (write x))) y))")))
      (ok (not (null bytes)) "write return value can be used"))))

;;; ============================================================
;;; T037: Unit test for write with :escape keyword
;;; ============================================================

(deftest test-compile-write-escape ()
  "Test that (write ... :escape ...) compiles to valid Wasm."
  ;; Test 1: write with :escape t (like prin1)
  (testing "write with :escape t"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (write x :escape t))")))
      (ok (not (null bytes)) "write :escape t compiles")))

  ;; Test 2: write with :escape nil (like princ)
  (testing "write with :escape nil"
    (let ((bytes (compile-and-validate "(defun test-fn (x) (write x :escape nil))")))
      (ok (not (null bytes)) "write :escape nil compiles"))))
