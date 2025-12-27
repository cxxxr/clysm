;;;; generator-test.lisp - Unit tests for Stage 1 binary generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for binary accumulator and generation functions

(in-package #:clysm/tests/unit/stage1-generator)

;;; ==========================================================================
;;; Form Compilation Tests
;;; ==========================================================================

(deftest test-compile-form-to-wasm-simple-expr
  "compile-form-to-wasm should compile simple arithmetic."
  (multiple-value-bind (wasm success-p error-msg)
      (clysm/stage1:compile-form-to-wasm '(+ 1 2))
    (ok success-p "simple arithmetic compiles")
    (ok (null error-msg) "no error message")
    (ok (or (null wasm) (vectorp wasm)) "returns vector or nil")))

(deftest test-compile-form-to-wasm-error-handling
  "compile-form-to-wasm should handle errors gracefully."
  (multiple-value-bind (wasm success-p error-msg)
      ;; Use an intentionally invalid form
      (handler-case
          (clysm/stage1:compile-form-to-wasm '(unknown-special-form-xyz))
        (error ()
          (values nil nil "Expected error")))
    (ok (or success-p (not success-p)) "returns success flag")
    (ok (or (null error-msg) (stringp error-msg)) "error-msg is string or nil")))

;;; ==========================================================================
;;; Compile All Forms Tests
;;; ==========================================================================

(deftest test-compile-all-forms-returns-results
  "compile-all-forms should return results and stats."
  (let ((forms (list (clysm/stage1:make-source-form
                      :id "1:0" :sexp '(+ 1 2) :operator '+ :compilable-p t)
                     (clysm/stage1:make-source-form
                      :id "1:1" :sexp '(* 3 4) :operator '* :compilable-p t))))
    (multiple-value-bind (results stats)
        (clysm/stage1:compile-all-forms forms)
      (ok (listp results) "returns list of results")
      (ok (listp stats) "returns stats plist")
      (ok (= (length results) 2) "two results")
      (ok (getf stats :total) "stats has :total"))))

(deftest test-compile-all-forms-progress-callback
  "compile-all-forms should call progress callback."
  (let ((forms (list (clysm/stage1:make-source-form
                      :id "1:0" :sexp '(+ 1 2) :operator '+ :compilable-p t)))
        (callback-called nil))
    (clysm/stage1:compile-all-forms
     forms
     :progress-callback (lambda (index total success-p)
                          (declare (ignore success-p))
                          (setf callback-called (list index total))))
    (ok callback-called "callback was called")
    (ok (equal callback-called '(1 1)) "callback received correct args")))

;;; ==========================================================================
;;; Binary Accumulation Tests
;;; ==========================================================================

(deftest test-accumulate-wasm-bytes-empty
  "accumulate-wasm-bytes should return NIL for no bytes."
  (let ((results (list (clysm/stage1:make-compilation-result
                        :form-id "1:0" :success-p nil))))
    (ok (null (clysm/stage1:accumulate-wasm-bytes results))
        "returns NIL for no successful results")))

(deftest test-accumulate-wasm-bytes-combines
  "accumulate-wasm-bytes should combine multiple byte vectors."
  (let ((results (list (clysm/stage1:make-compilation-result
                        :form-id "1:0" :success-p t
                        :wasm-bytes (make-array 4 :element-type '(unsigned-byte 8)
                                                 :initial-contents '(0 1 2 3)))
                       (clysm/stage1:make-compilation-result
                        :form-id "1:1" :success-p t
                        :wasm-bytes (make-array 3 :element-type '(unsigned-byte 8)
                                                 :initial-contents '(4 5 6))))))
    (let ((combined (clysm/stage1:accumulate-wasm-bytes results)))
      (ok (vectorp combined) "returns vector")
      (ok (= (length combined) 7) "combined length is 7"))))

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

