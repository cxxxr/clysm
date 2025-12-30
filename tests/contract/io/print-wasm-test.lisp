;;;; print-wasm-test.lisp - Contract tests for print primitives Wasm output
;;;; Feature: 001-io-print-primitives
;;;; Tests: Wasm import declarations, instruction sequences

(defpackage #:clysm/tests/contract/io/print-wasm
  (:use #:cl #:rove))

(in-package #:clysm/tests/contract/io/print-wasm)

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defun compile-to-wasm (source)
  "Compile SOURCE to Wasm and return the bytes."
  (handler-case
      (clysm:compile-to-wasm source)
    (error (e)
      (format *error-output* "Compilation error: ~A~%" e)
      nil)))

(defun wasm-has-import-p (wasm-bytes module-name func-name)
  "Check if WASM-BYTES has an import from MODULE-NAME/FUNC-NAME.
   Returns T if found, NIL otherwise."
  (when wasm-bytes
    (let ((temp-file (format nil "/tmp/clysm-test-~A.wasm" (get-universal-time))))
      (unwind-protect
           (progn
             (with-open-file (s temp-file
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
               (write-sequence wasm-bytes s))
             ;; Use wasm-tools print to get text representation
             (multiple-value-bind (output error-output exit-code)
                 (uiop:run-program
                  (list "wasm-tools" "print" temp-file)
                  :output :string
                  :error-output :string
                  :ignore-error-status t)
               (declare (ignore error-output))
               (when (zerop exit-code)
                 ;; Look for import declaration like:
                 ;; (import "clysm:io" "write-char" (func ...))
                 (and (search module-name output)
                      (search func-name output)
                      t))))
        (when (probe-file temp-file)
          (delete-file temp-file))))))

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
;;; Contract: Wasm imports for I/O operations
;;; ============================================================

(deftest test-terpri-imports ()
  "Test that terpri generates correct Wasm imports."
  (testing "terpri requires write-char import"
    (let ((bytes (compile-to-wasm "(defun test-fn () (terpri))")))
      (ok (not (null bytes)) "terpri compiles")
      (ok (wasm-has-import-p bytes "clysm:io" "write-char")
          "terpri imports clysm:io.write-char"))))

(deftest test-princ-imports ()
  "Test that princ generates correct Wasm imports."
  (testing "princ requires object printing imports"
    (let ((bytes (compile-to-wasm "(defun test-fn (x) (princ x))")))
      (ok (not (null bytes)) "princ compiles")
      ;; princ needs to convert object to string, then write
      (ok (wasm-validates-p bytes) "princ produces valid Wasm"))))

(deftest test-print-imports ()
  "Test that print generates correct Wasm imports."
  (testing "print requires write-char import for newline and space"
    (let ((bytes (compile-to-wasm "(defun test-fn (x) (print x))")))
      (ok (not (null bytes)) "print compiles")
      (ok (wasm-has-import-p bytes "clysm:io" "write-char")
          "print imports clysm:io.write-char"))))

;;; ============================================================
;;; Contract: Generated Wasm structure
;;; ============================================================

(deftest test-wasm-validates ()
  "Test that all print primitives produce valid Wasm."
  (testing "terpri Wasm validates"
    (let ((bytes (compile-to-wasm "(defun test-fn () (terpri))")))
      (ok (wasm-validates-p bytes) "terpri validates")))

  (testing "princ Wasm validates"
    (let ((bytes (compile-to-wasm "(defun test-fn (x) (princ x))")))
      (ok (wasm-validates-p bytes) "princ validates")))

  (testing "prin1 Wasm validates"
    (let ((bytes (compile-to-wasm "(defun test-fn (x) (prin1 x))")))
      (ok (wasm-validates-p bytes) "prin1 validates")))

  (testing "print Wasm validates"
    (let ((bytes (compile-to-wasm "(defun test-fn (x) (print x))")))
      (ok (wasm-validates-p bytes) "print validates"))))

;;; ============================================================
;;; Contract: Return value semantics
;;; ============================================================

(deftest test-return-value-semantics ()
  "Test that print functions have correct return value semantics."
  ;; Per HyperSpec: print, prin1, princ return the object
  ;; terpri returns NIL

  (testing "terpri returns NIL (compilable assignment)"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn () (let ((x (terpri))) x))")))
      (ok (wasm-validates-p bytes) "terpri result assignable")))

  (testing "princ returns the object (compilable assignment)"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn (x) (let ((y (princ x))) y))")))
      (ok (wasm-validates-p bytes) "princ result assignable")))

  (testing "prin1 returns the object (compilable assignment)"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn (x) (let ((y (prin1 x))) y))")))
      (ok (wasm-validates-p bytes) "prin1 result assignable")))

  (testing "print returns the object (compilable assignment)"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn (x) (let ((y (print x))) y))")))
      (ok (wasm-validates-p bytes) "print result assignable"))))

;;; ============================================================
;;; T023: Contract test for format nil Wasm structure
;;; HyperSpec: resources/HyperSpec/Body/f_format.htm
;;; ============================================================

(deftest test-format-nil-wasm-structure ()
  "Test that format nil produces valid Wasm that returns a string.
   Format to nil should NOT use FFI I/O - it builds a string in memory."
  ;; Per ANSI CL: when destination is nil, format returns a string
  (testing "format nil produces valid Wasm"
    (let ((bytes (compile-to-wasm "(defun test-fn () (format nil \"~~A\" 42))")))
      (ok (not (null bytes)) "format nil compiles")
      (ok (wasm-validates-p bytes) "format nil produces valid Wasm")))

  (testing "format nil result can be used as string"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn () (let ((s (format nil \"Hello\"))) s))")))
      (ok (wasm-validates-p bytes) "format nil result is assignable")))

  (testing "format nil with multiple directives"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn (x y) (format nil \"~~A = ~~D\" x y))")))
      (ok (wasm-validates-p bytes) "format nil with multiple args compiles")))

  (testing "format nil with newline directives"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn () (format nil \"Line1~~%Line2~~&End\"))")))
      (ok (wasm-validates-p bytes) "format nil with ~% and ~& compiles"))))

;;; ============================================================
;;; Contract: Format t requires FFI I/O imports
;;; ============================================================

(deftest test-format-t-wasm-structure ()
  "Test that format t produces valid Wasm with FFI imports."
  ;; Per ANSI CL: when destination is t, output goes to *standard-output*
  (testing "format t produces valid Wasm"
    (let ((bytes (compile-to-wasm "(defun test-fn () (format t \"Hello~~%\"))")))
      (ok (not (null bytes)) "format t compiles")
      (ok (wasm-validates-p bytes) "format t produces valid Wasm")))

  (testing "format t with argument"
    (let ((bytes (compile-to-wasm "(defun test-fn (x) (format t \"Value: ~~A\" x))")))
      (ok (wasm-validates-p bytes) "format t with arg compiles"))))

;;; ============================================================
;;; T038: Contract test for write Wasm structure
;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm
;;; ============================================================

(deftest test-write-returns-object ()
  "Test that write returns the object as per ANSI CL."
  ;; Per HyperSpec: write returns object
  (testing "write returns object (compilable assignment)"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn (x) (let ((y (write x))) y))")))
      (ok (wasm-validates-p bytes) "write result assignable")))

  (testing "write with :escape t returns object"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn (x) (let ((y (write x :escape t))) y))")))
      (ok (wasm-validates-p bytes) "write :escape t result assignable")))

  (testing "write with :escape nil returns object"
    (let ((bytes (compile-to-wasm
                  "(defun test-fn (x) (let ((y (write x :escape nil))) y))")))
      (ok (wasm-validates-p bytes) "write :escape nil result assignable"))))
