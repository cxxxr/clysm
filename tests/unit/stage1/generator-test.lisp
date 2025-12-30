;;;; generator-test.lisp - Unit tests for Stage 1 binary generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for binary accumulator and generation functions

(in-package #:clysm/tests/unit/stage1-generator)

;;; ==========================================================================
;;; Form Compilation Tests (test-form-compilation)
;;; ==========================================================================

(deftest test-form-compilation-simple-expr
  "test-form-compilation should compile simple defun."
  (multiple-value-bind (success-p bytes)
      (clysm/stage1:test-form-compilation '(defun test-simple-add () (+ 1 2)))
    (ok success-p "simple defun compiles")
    (ok (or (null bytes) (vectorp bytes)) "returns vector or nil")))

(deftest test-form-compilation-error-handling
  "test-form-compilation should handle errors gracefully."
  (multiple-value-bind (success-p bytes)
      (clysm/stage1:test-form-compilation '(unknown-special-form-xyz))
    (ok (null success-p) "unknown form fails")
    (ok (null bytes) "no bytes for failed form")))

;;; ==========================================================================
;;; Classify Forms Tests
;;; ==========================================================================

(deftest test-classify-forms-returns-results
  "classify-forms should return results and stats."
  (let ((forms (list (clysm/stage1:make-source-form
                      :id "1:0" :sexp '(defun test-add-1 () (+ 1 2)) :operator 'defun :compilable-p t)
                     (clysm/stage1:make-source-form
                      :id "1:1" :sexp '(defun test-mul-1 () (* 3 4)) :operator 'defun :compilable-p t))))
    (multiple-value-bind (successful-sexps results stats)
        (clysm/stage1:classify-forms forms :validate nil)
      (ok (listp results) "returns list of results")
      (ok (listp stats) "returns stats plist")
      (ok (= (length results) 2) "two results")
      (ok (getf stats :total) "stats has :total"))))

;;; ==========================================================================
;;; Phase 13D-3: Directive Skip Tests (T006, T007)
;;; ==========================================================================

(deftest test-form-compilation-returns-skipped-for-nil-bytes
  "T006: test-form-compilation should return :skipped when compile-to-wasm returns nil.
This happens for compile-time directives like defpackage, in-package, declaim."
  ;; Test with a directive form that returns nil from compile-to-wasm
  (multiple-value-bind (result bytes)
      (clysm/stage1::test-form-compilation '(defpackage :test-pkg-for-skip (:use :cl)))
    (ok (eq result :skipped) "directive form should return :skipped")
    (ok (null bytes) "bytes should be nil for directive")))

(deftest test-classify-forms-tracks-skipped-count
  "T007: classify-forms should track skipped count separately from compiled/failed.
Directive forms returning nil should increment skipped, not failed."
  (let ((forms (list
                ;; A directive form (should be skipped)
                (clysm/stage1:make-source-form
                 :id "1:0"
                 :sexp '(defpackage :test-classify-pkg (:use :cl))
                 :operator 'defpackage
                 :compilable-p t)
                ;; A normal form (should compile)
                (clysm/stage1:make-source-form
                 :id "1:1"
                 :sexp '(defun test-fn-for-classify () 42)
                 :operator 'defun
                 :compilable-p t))))
    (multiple-value-bind (successful-sexps results stats)
        (clysm/stage1::classify-forms forms :validate nil)
      (declare (ignore results))
      ;; Check that skipped is tracked
      (ok (getf stats :skipped) "stats should have :skipped key")
      (ok (= (getf stats :skipped) 1) "one form should be skipped (defpackage)")
      (ok (= (getf stats :compiled) 1) "one form should be compiled (defun)")
      (ok (= (getf stats :failed) 0) "no forms should fail")
      ;; Successful-sexps should NOT include the skipped directive
      (ok (= (length successful-sexps) 1) "only compiled forms in successful-sexps"))))

;;; ==========================================================================
;;; Binary Output Tests
;;; ==========================================================================

(deftest test-write-stage1-binary-creates-file
  "write-stage1-binary should create output file."
  (let ((test-path (format nil "/tmp/test-stage1-~A.wasm" (get-universal-time)))
        (bytes (make-array 8 :element-type '(unsigned-byte 8)
                            :initial-contents '(0 #x61 #x73 #x6d 1 0 0 0))))
    (unwind-protect
        (progn
          (clysm/stage1:write-stage1-binary bytes test-path)
          (ok (probe-file test-path) "file was created")
          (ok (= (with-open-file (s test-path) (file-length s)) 8)
              "file has correct size"))
      (when (probe-file test-path)
        (delete-file test-path)))))

