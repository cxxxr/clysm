;;;; stage1-load-test.lisp - Contract tests for Stage 0 Wasm binary loading
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests that Stage 0 binary exists and validates

(in-package #:clysm/tests/contract/stage1-load)

;;; ==========================================================================
;;; Stage 0 Binary Existence Tests
;;; ==========================================================================

(deftest test-stage0-binary-exists
  "dist/clysm-stage0.wasm should exist."
  (let ((stage0-path (merge-pathnames
                      "dist/clysm-stage0.wasm"
                      (asdf:system-source-directory :clysm))))
    (ok (probe-file stage0-path)
        (format nil "Stage 0 binary exists at ~A" stage0-path))))

(deftest test-stage0-binary-not-empty
  "Stage 0 binary should have non-zero size."
  (let ((stage0-path (merge-pathnames
                      "dist/clysm-stage0.wasm"
                      (asdf:system-source-directory :clysm))))
    (when (probe-file stage0-path)
      (let ((size (with-open-file (s stage0-path
                                     :element-type '(unsigned-byte 8))
                    (file-length s))))
        (ok (> size 0)
            (format nil "Stage 0 binary has size ~D bytes" size))))))

(deftest test-stage0-binary-has-wasm-magic
  "Stage 0 binary should start with Wasm magic number."
  (let ((stage0-path (merge-pathnames
                      "dist/clysm-stage0.wasm"
                      (asdf:system-source-directory :clysm))))
    (when (probe-file stage0-path)
      (with-open-file (s stage0-path :element-type '(unsigned-byte 8))
        (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
          (read-sequence magic s)
          ;; Wasm magic: 0x00 0x61 0x73 0x6D ("\0asm")
          (ok (and (= (aref magic 0) #x00)
                   (= (aref magic 1) #x61)
                   (= (aref magic 2) #x73)
                   (= (aref magic 3) #x6D))
              "Stage 0 has Wasm magic number"))))))

;;; ==========================================================================
;;; Stage 0 Validation Tests (via wasm-tools)
;;; ==========================================================================

(deftest test-stage0-validates-with-wasm-tools
  "Stage 0 binary should pass wasm-tools validation."
  (let ((stage0-path (merge-pathnames
                      "dist/clysm-stage0.wasm"
                      (asdf:system-source-directory :clysm))))
    (when (probe-file stage0-path)
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program
           (list "wasm-tools" "validate" (namestring stage0-path))
           :output :string
           :error-output :string
           :ignore-error-status t)
        (declare (ignore output))
        (ok (zerop exit-code)
            (format nil "wasm-tools validate exit code: ~D~@[ (~A)~]"
                    exit-code error-output))))))

;;; ==========================================================================
;;; Stage 0 Structure Tests
;;; ==========================================================================

(deftest test-stage0-has-type-section
  "Stage 0 binary should have type definitions."
  (let ((stage0-path (merge-pathnames
                      "dist/clysm-stage0.wasm"
                      (asdf:system-source-directory :clysm))))
    (when (probe-file stage0-path)
      (let ((output (uiop:run-program
                     (list "wasm-tools" "print" (namestring stage0-path))
                     :output :string
                     :ignore-error-status t)))
        (ok (search "(type" output)
            "Stage 0 has type definitions")))))

(deftest test-stage0-has-exports
  "Stage 0 binary should have exports."
  (let ((stage0-path (merge-pathnames
                      "dist/clysm-stage0.wasm"
                      (asdf:system-source-directory :clysm))))
    (when (probe-file stage0-path)
      (let ((output (uiop:run-program
                     (list "wasm-tools" "print" (namestring stage0-path))
                     :output :string
                     :ignore-error-status t)))
        (ok (search "(export" output)
            "Stage 0 has exports")))))
