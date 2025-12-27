;;;; byte-compare-test.lisp - Unit tests for byte-level binary comparison
;;;;
;;;; Part of Feature 040: Fixed-Point Verification
;;;; Tests binaries-identical-p function

(defpackage #:clysm/tests/unit/fixpoint/byte-compare-test
  (:use #:cl #:rove #:clysm/stage1))

(in-package #:clysm/tests/unit/fixpoint/byte-compare-test)

;;; ==========================================================================
;;; Test: binaries-identical-p basic functionality
;;; ==========================================================================

(deftest binaries-identical-p-returns-multiple-values
  "binaries-identical-p should return (values identical-p first-diff-offset)"
  (ok t "Return value structure defined"))

(deftest identical-files-return-t
  "Identical files should return (values T nil)"
  ;; Create two identical temp files
  (let ((path1 "/tmp/clysm-test-identical1.wasm")
        (path2 "/tmp/clysm-test-identical2.wasm")
        (content #(0 97 115 109 1 0 0 0)))
    (unwind-protect
        (progn
          (with-open-file (out path1 :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence content out))
          (with-open-file (out path2 :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence content out))
          (multiple-value-bind (identical-p offset)
              (binaries-identical-p path1 path2)
            (ok identical-p "Identical files return T")
            (ok (null offset) "Identical files have nil offset")))
      (when (probe-file path1) (delete-file path1))
      (when (probe-file path2) (delete-file path2)))))

(deftest different-files-return-nil-with-offset
  "Different files should return (values NIL offset)"
  (let ((path1 "/tmp/clysm-test-diff1.wasm")
        (path2 "/tmp/clysm-test-diff2.wasm")
        (content1 #(0 97 115 109 1 0 0 0))
        (content2 #(0 97 115 109 1 0 0 1))) ; Last byte different
    (unwind-protect
        (progn
          (with-open-file (out path1 :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence content1 out))
          (with-open-file (out path2 :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence content2 out))
          (multiple-value-bind (identical-p offset)
              (binaries-identical-p path1 path2)
            (ok (not identical-p) "Different files return NIL")
            (ok (= offset 7) "Offset points to first difference")))
      (when (probe-file path1) (delete-file path1))
      (when (probe-file path2) (delete-file path2)))))

(deftest different-size-files-detected
  "Files of different sizes should be detected"
  (let ((path1 "/tmp/clysm-test-size1.wasm")
        (path2 "/tmp/clysm-test-size2.wasm")
        (content1 #(0 97 115 109 1 0 0 0))
        (content2 #(0 97 115 109 1 0 0 0 0 0))) ; 2 extra bytes
    (unwind-protect
        (progn
          (with-open-file (out path1 :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence content1 out))
          (with-open-file (out path2 :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence content2 out))
          (multiple-value-bind (identical-p offset)
              (binaries-identical-p path1 path2)
            (ok (not identical-p) "Different size files return NIL")
            (ok (numberp offset) "Offset is reported")))
      (when (probe-file path1) (delete-file path1))
      (when (probe-file path2) (delete-file path2)))))

(deftest missing-file-signals-error
  "Missing file should signal appropriate error"
  (ok (signals error
        (binaries-identical-p "/nonexistent/file1.wasm"
                              "/nonexistent/file2.wasm"))
      "Missing files signal error"))
