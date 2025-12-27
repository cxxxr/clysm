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
                 :path "stage0.wasm"
                 :size-bytes 1000
                 :exports '("main" "init")
                 :types 10))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm"
                 :size-bytes 2000
                 :exports '("main" "init" "compile")
                 :types 15))
         (report (clysm/stage1::make-diff-report
                  :stage0 info1
                  :stage1 info2
                  :differences (clysm/stage1::compute-differences info1 info2)))
         (output (make-string-output-stream)))
    (clysm/stage1::write-diff-report-json report output)
    (let ((json-string (get-output-stream-string output)))
      (ok (search "\"stage0\"" json-string)
          "JSON has 'stage0' field")
      (ok (search "\"stage1\"" json-string)
          "JSON has 'stage1' field")
      (ok (search "\"size_delta\"" json-string)
          "JSON has 'size_delta' field"))))

(deftest test-diff-details-format
  "Diff details should have properly formatted fields."
  (let* ((info1 (clysm/stage1::make-binary-info
                 :path "stage0.wasm"
                 :size-bytes 1000
                 :exports '("main" "init")
                 :types 10))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm"
                 :size-bytes 2500
                 :exports '("main" "compile")
                 :types 15))
         (diff (clysm/stage1::compute-differences info1 info2)))
    ;; Size delta is a string
    (ok (stringp (clysm/stage1::diff-details-size-delta diff))
        "Size delta is a formatted string")
    ;; Missing and new exports are lists
    (ok (listp (clysm/stage1::diff-details-missing-exports diff))
        "Missing exports is a list")
    (ok (listp (clysm/stage1::diff-details-new-exports diff))
        "New exports is a list")))

(deftest test-diff-report-valid-json-syntax
  "Diff report should be valid JSON."
  (let* ((info1 (clysm/stage1::make-binary-info
                 :path "stage0.wasm"
                 :size-bytes 1000
                 :exports '("main")
                 :types 10))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm"
                 :size-bytes 2000
                 :exports '("main" "compile")
                 :types 15))
         (report (clysm/stage1::make-diff-report
                  :stage0 info1
                  :stage1 info2
                  :differences (clysm/stage1::compute-differences info1 info2)))
         (output (make-string-output-stream)))
    (clysm/stage1::write-diff-report-json report output)
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

(deftest test-diff-export-comparison
  "Diff should correctly identify new and missing exports."
  (let* ((info1 (clysm/stage1::make-binary-info
                 :path "stage0.wasm"
                 :size-bytes 1000
                 :exports '("main" "init" "old-fn")
                 :types 10))
         (info2 (clysm/stage1::make-binary-info
                 :path "stage1.wasm"
                 :size-bytes 2000
                 :exports '("main" "init" "new-fn")
                 :types 15))
         (diff (clysm/stage1::compute-differences info1 info2)))
    (ok (member "old-fn" (clysm/stage1::diff-details-missing-exports diff)
                :test #'equal)
        "old-fn is missing in stage1")
    (ok (member "new-fn" (clysm/stage1::diff-details-new-exports diff)
                :test #'equal)
        "new-fn is new in stage1")))
