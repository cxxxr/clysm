;;;; import-section-test.lisp - Contract tests for import section optimization
;;;;
;;;; Feature: 022-wasm-import-optimization
;;;; Tasks: T014-T015, T029

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
