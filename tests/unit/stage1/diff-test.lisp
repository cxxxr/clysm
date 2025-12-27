;;;; diff-test.lisp - Unit tests for Stage 1 diff analysis
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for binary metadata extraction and diff computation

(in-package #:clysm/tests/unit/stage1-diff)

;;; ==========================================================================
;;; BinaryInfo Tests
;;; ==========================================================================

(deftest test-binary-info-creation
  "BinaryInfo struct should be creatable with required fields."
  (let ((info (clysm/stage1::make-binary-info
               :path "test.wasm"
               :size-bytes 1024
               :exports '("func1" "func2")
               :types 5
               :functions 8
               :valid-p t)))
    (ok (clysm/stage1::binary-info-p info)
        "make-binary-info creates binary-info struct")
    (ok (= (clysm/stage1::binary-info-size-bytes info) 1024)
        "size-bytes is 1024")
    (ok (= (length (clysm/stage1::binary-info-exports info)) 2)
        "exports has 2 items")))

;;; ==========================================================================
;;; DiffDetails Tests
;;; ==========================================================================

(deftest test-diff-details-creation
  "DiffDetails struct should be creatable with required fields."
  (let ((details (clysm/stage1::make-diff-details
                  :size-delta "+100 bytes (+10.0%)"
                  :missing-exports '("old-export")
                  :new-exports '("new-export")
                  :type-changes nil)))
    (ok (clysm/stage1::diff-details-p details)
        "make-diff-details creates diff-details struct")
    (ok (search "100" (clysm/stage1::diff-details-size-delta details))
        "size-delta contains 100")))

;;; ==========================================================================
;;; DiffReport Tests
;;; ==========================================================================

(deftest test-diff-report-creation
  "DiffReport struct should be creatable."
  (let* ((stage0 (clysm/stage1::make-binary-info
                  :path "stage0.wasm" :size-bytes 1000
                  :exports '("a" "b") :types 5 :functions 8
                  :valid-p t))
         (stage1 (clysm/stage1::make-binary-info
                  :path "stage1.wasm" :size-bytes 1100
                  :exports '("a" "b" "c") :types 6 :functions 10
                  :valid-p t))
         (details (clysm/stage1::make-diff-details
                   :size-delta "+100 bytes"
                   :missing-exports nil
                   :new-exports '("c")
                   :type-changes nil))
         (report (clysm/stage1::make-diff-report
                  :stage0 stage0
                  :stage1 stage1
                  :differences details)))
    (ok (clysm/stage1::diff-report-p report)
        "make-diff-report creates diff-report struct")
    (ok (= (clysm/stage1::binary-info-size-bytes
            (clysm/stage1::diff-report-stage0 report)) 1000)
        "stage0 size is 1000")))

;;; ==========================================================================
;;; Difference Computation Tests
;;; ==========================================================================

(deftest test-compute-differences-size
  "compute-differences should calculate size delta."
  (let* ((stage0 (clysm/stage1::make-binary-info
                  :path "stage0.wasm" :size-bytes 1000
                  :exports nil :types 5 :functions 8
                  :valid-p t))
         (stage1 (clysm/stage1::make-binary-info
                  :path "stage1.wasm" :size-bytes 1100
                  :exports nil :types 5 :functions 8
                  :valid-p t))
         (details (clysm/stage1::compute-differences stage0 stage1)))
    (ok (clysm/stage1::diff-details-p details)
        "returns diff-details struct")
    (ok (search "100" (clysm/stage1::diff-details-size-delta details))
        "size-delta mentions 100 bytes")))

(deftest test-compute-differences-identical
  "compute-differences should detect identical sizes."
  (let* ((stage0 (clysm/stage1::make-binary-info
                  :path "stage0.wasm" :size-bytes 1000
                  :exports nil :types 5 :functions 8
                  :valid-p t))
         (stage1 (clysm/stage1::make-binary-info
                  :path "stage1.wasm" :size-bytes 1000
                  :exports nil :types 5 :functions 8
                  :valid-p t))
         (details (clysm/stage1::compute-differences stage0 stage1)))
    (ok (search "identical" (clysm/stage1::diff-details-size-delta details))
        "size-delta mentions identical")))

(deftest test-compute-differences-exports
  "compute-differences should find missing and new exports."
  (let* ((stage0 (clysm/stage1::make-binary-info
                  :path "stage0.wasm" :size-bytes 1000
                  :exports '("a" "b" "old") :types 5 :functions 8
                  :valid-p t))
         (stage1 (clysm/stage1::make-binary-info
                  :path "stage1.wasm" :size-bytes 1000
                  :exports '("a" "b" "new") :types 5 :functions 8
                  :valid-p t))
         (details (clysm/stage1::compute-differences stage0 stage1)))
    (ok (member "old" (clysm/stage1::diff-details-missing-exports details)
                :test #'equal)
        "old is in missing-exports")
    (ok (member "new" (clysm/stage1::diff-details-new-exports details)
                :test #'equal)
        "new is in new-exports")))

;;; ==========================================================================
;;; Pattern Counting Tests
;;; ==========================================================================

(deftest test-count-pattern
  "count-pattern should count occurrences correctly."
  (let ((text "(export \"a\") (export \"b\") (export \"c\")"))
    (ok (= (clysm/stage1::count-pattern "(export" text) 3)
        "found 3 exports")))

(deftest test-count-pattern-empty
  "count-pattern should return 0 for no matches."
  (let ((text "no exports here"))
    (ok (= (clysm/stage1::count-pattern "(export" text) 0)
        "found 0 exports")))

;;; ==========================================================================
;;; Export Extraction Tests
;;; ==========================================================================

(deftest test-extract-exports
  "extract-exports should parse export names from WAT."
  (let* ((wat "(export \"memory\" (memory 0)) (export \"main\" (func 1))")
         (exports (clysm/stage1::extract-exports wat)))
    (ok (= (length exports) 2) "found 2 exports")
    (ok (member "memory" exports :test #'equal) "found memory export")
    (ok (member "main" exports :test #'equal) "found main export")))

;;; ==========================================================================
;;; Report Output Tests
;;; ==========================================================================

(deftest test-write-diff-json
  "write-diff-json should produce valid JSON structure."
  (let* ((stage0 (clysm/stage1::make-binary-info
                  :path "stage0.wasm" :size-bytes 1000
                  :exports nil :types 5 :functions 8 :valid-p t))
         (stage1 (clysm/stage1::make-binary-info
                  :path "stage1.wasm" :size-bytes 1100
                  :exports nil :types 6 :functions 10 :valid-p t))
         (details (clysm/stage1::compute-differences stage0 stage1))
         (report (clysm/stage1::make-diff-report
                  :stage0 stage0 :stage1 stage1 :differences details))
         (json (with-output-to-string (s)
                 (clysm/stage1::write-diff-json report s))))
    (ok (search "\"stage0\":" json) "JSON has stage0")
    (ok (search "\"stage1\":" json) "JSON has stage1")
    (ok (search "\"differences\":" json) "JSON has differences")))

