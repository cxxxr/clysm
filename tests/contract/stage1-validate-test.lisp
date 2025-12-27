;;;; stage1-validate-test.lisp - Contract test for Stage 1 Wasm validation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; T047: Verifies Stage 1 binary passes wasm-tools validation

(in-package #:clysm/tests/contract/stage1-validate)

;;; ==========================================================================
;;; Stage 1 Wasm Validation Contract Tests
;;; ==========================================================================

(deftest test-stage1-binary-validates
  "Stage 1 binary should pass wasm-tools validate."
  (let* ((root (asdf:system-source-directory :clysm))
         (stage1-path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file stage1-path)
        (progn
          (ok (clysm/stage1:validate-stage1 stage1-path)
              "Stage 1 binary validates with wasm-tools"))
        (skip "Stage 1 binary not found - skipping validation test"))))

(deftest test-generated-wasm-has-magic-header
  "Generated Wasm should have correct magic number."
  (let* ((root (asdf:system-source-directory :clysm))
         (stage1-path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file stage1-path)
        (with-open-file (in stage1-path :element-type '(unsigned-byte 8))
          (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
            (read-sequence magic in)
            ;; Wasm magic: 0x00 0x61 0x73 0x6d ("\0asm")
            (ok (and (= (aref magic 0) #x00)
                     (= (aref magic 1) #x61)
                     (= (aref magic 2) #x73)
                     (= (aref magic 3) #x6d))
                "Wasm magic header is correct")))
        (skip "Stage 1 binary not found - skipping magic header test"))))

(deftest test-generated-wasm-has-version
  "Generated Wasm should have version 1."
  (let* ((root (asdf:system-source-directory :clysm))
         (stage1-path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file stage1-path)
        (with-open-file (in stage1-path :element-type '(unsigned-byte 8))
          (let ((header (make-array 8 :element-type '(unsigned-byte 8))))
            (read-sequence header in)
            ;; Version at bytes 4-7: 01 00 00 00 (little-endian 1)
            (ok (and (= (aref header 4) #x01)
                     (= (aref header 5) #x00)
                     (= (aref header 6) #x00)
                     (= (aref header 7) #x00))
                "Wasm version is 1")))
        (skip "Stage 1 binary not found - skipping version test"))))

(deftest test-write-and-validate-temp-binary
  "Writing a Wasm binary should produce valid output."
  (let* ((test-bytes (make-array 8 :element-type '(unsigned-byte 8)
                                  :initial-contents '(#x00 #x61 #x73 #x6d
                                                      #x01 #x00 #x00 #x00)))
         (temp-path (format nil "/tmp/clysm-test-~A.wasm" (get-universal-time))))
    (unwind-protect
        (progn
          (clysm/stage1:write-stage1-binary test-bytes temp-path)
          (ok (probe-file temp-path) "Temp binary was written")
          (ok (clysm/stage1:validate-stage1 temp-path)
              "Temp binary validates"))
      (when (probe-file temp-path)
        (delete-file temp-path)))))

