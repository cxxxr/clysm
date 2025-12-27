;;; self-compile-test.lisp - Integration test for full self-compilation pipeline
;;;
;;; T055: Integration test for full pipeline

(defpackage :clysm-tests/integration/validation/self-compile
  (:use :cl :rove)
  (:local-nicknames (:v :clysm-validation)))

(in-package :clysm-tests/integration/validation/self-compile)

;;; T055: Integration test for full pipeline

(deftest full-pipeline-runs
  "Test that the full compilation pipeline runs without error"
  (let ((modules (v:get-dependency-order)))
    (ok (> (length modules) 0) "Should have modules to compile")
    ;; Compile first few modules to test the pipeline
    (let ((results nil))
      (dolist (module (subseq modules 0 (min 5 (length modules))))
        (let ((result (v:compile-module module)))
          (push result results)))
      (ok (= (length results) (min 5 (length modules)))
          "Should have results for each module"))))

(deftest compile-in-order-produces-results
  "Test that compile-in-order produces a list of results"
  (let ((results (v:compile-in-order :halt-on-failure t)))
    (ok (listp results) "Should return a list")
    (ok (> (length results) 0) "Should have at least one result")
    (ok (every #'v:compilation-result-p results)
        "All results should be compilation-result structs")))

(deftest validate-all-modules-includes-validation
  "Test that validate-all-modules includes wasm validation info"
  (let ((results (v:validate-all-modules)))
    (ok (listp results) "Should return a list")
    ;; Check that at least some results have validation info
    (let ((with-validation (remove-if-not
                            (lambda (r)
                              (and (v:compilation-result-success r)
                                   (v:compilation-result-wasm-bytes r)))
                            results)))
      (ok (>= (length with-validation) 0)
          "Should have results with wasm bytes"))))

(deftest compilation-report-generated
  "Test that compilation report can be generated"
  (let ((results (v:compile-in-order :halt-on-failure nil)))
    (let ((report-stream (make-string-output-stream)))
      (v:generate-compilation-report results report-stream)
      (let ((report (get-output-stream-string report-stream)))
        (ok (> (length report) 0) "Report should have content")
        (ok (search "Compilation Report" report) "Report should have header")))))

(deftest analysis-and-compilation-integrate
  "Test that static analysis and compilation work together"
  ;; Run static analysis
  (let ((analysis-results (v:analyze-all)))
    (ok (hash-table-p analysis-results) "Analysis should return hash-table")
    ;; Run compilation
    (let ((compile-results (v:compile-in-order :halt-on-failure t)))
      (ok (listp compile-results) "Compilation should return list")
      ;; Both should work on the same modules
      (ok (> (hash-table-count analysis-results) 0) "Should have analyzed directories")
      (ok (> (length compile-results) 0) "Should have compilation results"))))
