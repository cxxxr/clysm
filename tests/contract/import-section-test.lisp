;;;; import-section-test.lisp - Contract tests for import section optimization
;;;;
;;;; Features:
;;;;   - 022-wasm-import-optimization (Tasks: T014-T015, T029)
;;;;   - 001-ffi-import-architecture (Tasks: T044, T054)

(in-package #:clysm/tests/contract/import-section)

;;; ==========================================================================
;;; Helper: Check if Wasm bytes contain Import section
;;; ==========================================================================

(defun has-import-section-p (wasm-bytes)
  "Check if WASM-BYTES contains an Import section (section ID 2).
After the 8-byte header, sections appear in order. Import section has ID 2."
  (when (and (arrayp wasm-bytes) (> (length wasm-bytes) 8))
    ;; Skip 8-byte header (magic + version)
    (let ((pos 8))
      (loop while (< pos (length wasm-bytes))
            do (let ((section-id (aref wasm-bytes pos)))
                 (when (= section-id 2)  ; Import section ID
                   (return t))
                 ;; Skip this section: read LEB128 size and skip content
                 (incf pos)  ; past section ID
                 (when (>= pos (length wasm-bytes))
                   (return nil))
                 ;; Read LEB128 section size
                 (let ((size 0)
                       (shift 0))
                   (loop for byte = (when (< pos (length wasm-bytes))
                                      (aref wasm-bytes pos))
                         while byte
                         do (incf pos)
                            (setf size (logior size (ash (logand byte #x7F) shift)))
                            (incf shift 7)
                            (when (zerop (logand byte #x80))
                              (return)))
                   (incf pos size)))))))

;;; ==========================================================================
;;; T014: Non-I/O module has no Import section
;;; ==========================================================================

(deftest non-io-module-no-import-section
  (testing "Compiling (+ 1 2) produces Wasm without Import section"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (ok (not (has-import-section-p wasm-bytes))
          "Non-I/O code should not have Import section"))))

(deftest non-io-module-no-import-section-multiplication
  (testing "Compiling (* 7 6) produces Wasm without Import section"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(* 7 6))))
      (ok (not (has-import-section-p wasm-bytes))
          "Multiplication should not have Import section"))))

(deftest non-io-nested-arithmetic-no-import
  (testing "Compiling nested arithmetic produces Wasm without Import section"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ (* 2 3) (- 10 5)))))
      (ok (not (has-import-section-p wasm-bytes))
          "Nested arithmetic should not have Import section"))))

;;; ==========================================================================
;;; T015: wasm-tools validate passes for non-I/O module
;;; ==========================================================================

(deftest non-io-module-validates
  (testing "Compiled (+ 1 2) passes wasm-tools validate"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      ;; Write to temp file and validate
      (uiop:with-temporary-file (:pathname temp-path :type "wasm" :keep nil)
        (with-open-file (stream temp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          (write-sequence wasm-bytes stream))
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "wasm-tools" "validate"
                                    "--features" "gc,exceptions"
                                    (namestring temp-path))
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (ok (zerop exit-code)
              "wasm-tools validate should succeed for non-I/O module"))))))

;;; ==========================================================================
;;; T029: I/O module HAS Import section (analyzer detection tests)
;;; ==========================================================================
;;; Note: We test the analyzer directly since many I/O functions are not
;;; yet implemented in the compiler. The key is that analyze-io-usage
;;; correctly identifies I/O code, which gates import section emission.

(deftest io-usage-analyzer-detects-print
  (testing "analyze-io-usage returns T for (print 'hello)"
    (ok (clysm/compiler/analyzer/io-usage:analyze-io-usage '(print 'hello))
        "print should be detected as I/O")))

(deftest io-usage-analyzer-detects-format
  (testing "analyze-io-usage returns T for (format t \"hello\")"
    (ok (clysm/compiler/analyzer/io-usage:analyze-io-usage '(format t "hello"))
        "format should be detected as I/O")))

(deftest io-usage-analyzer-detects-write-char
  (testing "analyze-io-usage returns T for (write-char #\\a)"
    (ok (clysm/compiler/analyzer/io-usage:analyze-io-usage '(write-char #\a))
        "write-char should be detected as I/O")))

(deftest io-usage-analyzer-detects-nested-io
  (testing "analyze-io-usage returns T for nested I/O in let/progn"
    (ok (clysm/compiler/analyzer/io-usage:analyze-io-usage
         '(let ((x 1)) (print x)))
        "I/O inside let should be detected")
    (ok (clysm/compiler/analyzer/io-usage:analyze-io-usage
         '(progn (+ 1 2) (format t "~A" 3)))
        "I/O inside progn should be detected")))

;;; ==========================================================================
;;; T044: FFI mode affects import section content
;;; Feature: 001-ffi-import-architecture
;;; ==========================================================================

(deftest ffi-mode-affects-import-section
  "T044: FFI compilation mode affects import section content.
   Verifies that different modes produce different import sections."
  (testing ":auto mode produces no import section for pure code"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :auto)))
      (ok (not (has-import-section-p wasm-bytes))
          "Pure code with :auto should have no imports")))

  (testing ":full mode produces import section even for pure code"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :full)))
      (ok (has-import-section-p wasm-bytes)
          "Pure code with :full should have imports (all FFI + $dynamic-call)")))

  (testing ":minimal mode produces no import section for pure code"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :minimal)))
      (ok (not (has-import-section-p wasm-bytes))
          "Pure code with :minimal should have no imports")))

  (testing ":auto and :minimal produce same result for pure code"
    (let ((auto-bytes (clysm/compiler:compile-to-wasm '(* 7 6) :ffi-mode :auto))
          (minimal-bytes (clysm/compiler:compile-to-wasm '(* 7 6) :ffi-mode :minimal)))
      (ok (eq (has-import-section-p auto-bytes)
              (has-import-section-p minimal-bytes))
          "Both modes should produce same import section status for pure code")))

  (testing ":full produces larger module than :auto for pure code"
    (let ((auto-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :auto))
          (full-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :full)))
      (ok (< (length auto-bytes) (length full-bytes))
          ":full mode should produce larger module due to imports")))

  (testing "dynamic code produces imports in :auto mode"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm
                       '(let ((fn (intern "FOO"))) (funcall fn 42))
                       :ffi-mode :auto)))
      (ok (has-import-section-p wasm-bytes)
          "Dynamic code with :auto should have import section")))

  (testing "all modes validate with wasm-tools"
    (dolist (mode '(:auto :full :minimal))
      (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode mode)))
        (uiop:with-temporary-file (:pathname temp-path :type "wasm" :keep nil)
          (with-open-file (stream temp-path
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
            (write-sequence wasm-bytes stream))
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program (list "wasm-tools" "validate"
                                      "--features" "gc,exceptions"
                                      (namestring temp-path))
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (declare (ignore output error-output))
            (ok (zerop exit-code)
                (format nil "~A mode should produce valid Wasm" mode))))))))

;;; ==========================================================================
;;; T054: Additional import section tests for 001-ffi-import-architecture
;;; ==========================================================================

(deftest selective-ffi-imports-consistency
  "T054: Verify selective FFI imports are consistent across compilation."
  (testing "Same code produces same module size"
    (let ((bytes1 (clysm/compiler:compile-to-wasm '(+ 1 2)))
          (bytes2 (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (ok (= (length bytes1) (length bytes2))
          "Same code should produce same size module")))

  (testing "Different arithmetic produces same import characteristics"
    (let ((add-bytes (clysm/compiler:compile-to-wasm '(+ 1 2)))
          (mul-bytes (clysm/compiler:compile-to-wasm '(* 3 4)))
          (sub-bytes (clysm/compiler:compile-to-wasm '(- 10 5))))
      (ok (and (not (has-import-section-p add-bytes))
               (not (has-import-section-p mul-bytes))
               (not (has-import-section-p sub-bytes)))
          "All pure arithmetic should have no import section"))))

(deftest import-section-binary-format
  "T054: Verify import section follows Wasm binary format."
  (testing ":full mode import section starts with section ID 2"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :full)))
      ;; Find the import section and verify format
      (let ((found nil))
        (when (> (length wasm-bytes) 8)
          (loop with pos = 8
                while (< pos (length wasm-bytes))
                do (when (= (aref wasm-bytes pos) 2)  ; Import section ID
                     (setf found t)
                     (return))))
        (ok found "Import section should have ID 2 in :full mode"))))

  (testing "Module header is valid Wasm magic and version"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (ok (and (>= (length wasm-bytes) 8)
               ;; Wasm magic: \0asm
               (= (aref wasm-bytes 0) #x00)
               (= (aref wasm-bytes 1) #x61)
               (= (aref wasm-bytes 2) #x73)
               (= (aref wasm-bytes 3) #x6D)
               ;; Version 1
               (= (aref wasm-bytes 4) #x01)
               (= (aref wasm-bytes 5) #x00)
               (= (aref wasm-bytes 6) #x00)
               (= (aref wasm-bytes 7) #x00))
          "Module should have valid Wasm header"))))
