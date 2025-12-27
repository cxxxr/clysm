;;;; stage1-diff-test.lisp - Contract test for diff report JSON format
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; T068: Verifies diff report JSON output format

(in-package #:clysm/tests/contract/stage1-diff)

;;; ==========================================================================
;;; Diff Report JSON Contract Tests
;;; ==========================================================================

(deftest test-diff-report-has-required-fields
  "Diff report JSON should have all required fields."
  (let* ((info1 (clysm/stage1::make-binary-info
                 :path "stage0.wasm" :size-bytes 1000 :exports 5 :types 10))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm" :size-bytes 2000 :exports 8 :types 15))
         (diff (clysm/stage1::compute-diff info1 info2))
         (output (make-string-output-stream)))
    (clysm/stage1::write-diff-report-json diff output)
    (let ((json-string (get-output-stream-string output)))
      (ok (search "\"stage0\"" json-string)
          "JSON has 'stage0' field")
      (ok (search "\"stage1\"" json-string)
          "JSON has 'stage1' field")
      (ok (search "\"size_delta\"" json-string)
          "JSON has 'size_delta' field")
      (ok (search "\"exports_delta\"" json-string)
          "JSON has 'exports_delta' field"))))

(deftest test-diff-report-size-delta-calculation
  "Diff report should calculate size delta correctly."
  (let* ((info1 (clysm/stage1::make-binary-info
                 :path "stage0.wasm" :size-bytes 1000 :exports 5 :types 10))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm" :size-bytes 2500 :exports 8 :types 15))
         (diff (clysm/stage1::compute-diff info1 info2)))
    (ok (= (clysm/stage1::diff-details-size-delta diff) 1500)
        "Size delta is 1500 bytes")))

(deftest test-diff-report-exports-delta
  "Diff report should calculate exports delta correctly."
  (let* ((info1 (clysm/stage1::make-binary-info
                 :path "stage0.wasm" :size-bytes 1000 :exports 5 :types 10))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm" :size-bytes 2000 :exports 12 :types 15))
         (diff (clysm/stage1::compute-diff info1 info2)))
    (ok (= (clysm/stage1::diff-details-exports-delta diff) 7)
        "Exports delta is 7")))

(deftest test-diff-report-valid-json-syntax
  "Diff report should be valid JSON."
  (let* ((info1 (clysm/stage1::make-binary-info
                 :path "stage0.wasm" :size-bytes 1000 :exports 5 :types 10))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm" :size-bytes 2000 :exports 8 :types 15))
         (diff (clysm/stage1::compute-diff info1 info2))
         (output (make-string-output-stream)))
    (clysm/stage1::write-diff-report-json diff output)
    (let ((json-string (get-output-stream-string output)))
      ;; Basic JSON structure checks
      (ok (char= (char json-string 0) #\{)
          "JSON starts with {")
      (ok (char= (char json-string (1- (length json-string))) #\})
          "JSON ends with }")
      ;; Check for matching braces
      (let ((open-count (count #\{ json-string))
            (close-count (count #\} json-string)))
        (ok (= open-count close-count)
            "Matching brace count")))))

(deftest test-diff-report-negative-delta
  "Diff report should handle negative deltas (stage1 smaller than stage0)."
  (let* ((info1 (clysm/stage1::make-binary-info
                 :path "stage0.wasm" :size-bytes 5000 :exports 20 :types 30))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm" :size-bytes 2000 :exports 8 :types 10))
         (diff (clysm/stage1::compute-diff info1 info2)))
    (ok (= (clysm/stage1::diff-details-size-delta diff) -3000)
        "Size delta is -3000 (stage1 smaller)")
    (ok (= (clysm/stage1::diff-details-exports-delta diff) -12)
        "Exports delta is -12")))

