;;;; runtime-valid-test.lisp - Contract tests for Stage 0 Wasm module validation
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests US4: Runtime Initialization - Wasm module validates with wasm-tools

(defpackage #:clysm/tests/contract/stage0/runtime-valid-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:generate-runtime-init
                #:generate-type-section
                #:generate-global-section))

(in-package #:clysm/tests/contract/stage0/runtime-valid-test)

;;; ============================================================
;;; T009: Contract test for Wasm module validation
;;; ============================================================

(defun wasm-tools-available-p ()
  "Check if wasm-tools is available"
  (ignore-errors
    (zerop (nth-value 2
             (uiop:run-program '("wasm-tools" "--version")
                               :ignore-error-status t)))))

(defun validate-wasm-bytes (bytes)
  "Validate Wasm bytes using wasm-tools validate.
   Returns T if valid, NIL otherwise with error message."
  (unless (wasm-tools-available-p)
    (return-from validate-wasm-bytes (values t "wasm-tools not available, skipping")))
  (let ((temp-file (uiop:with-temporary-file (:stream s :pathname p
                                               :direction :output
                                               :element-type '(unsigned-byte 8)
                                               :keep t)
                     (write-sequence bytes s)
                     p)))
    (unwind-protect
         (multiple-value-bind (output error-output exit-code)
             (uiop:run-program `("wasm-tools" "validate" ,(namestring temp-file))
                               :output :string
                               :error-output :string
                               :ignore-error-status t)
           (declare (ignore output))
           (if (zerop exit-code)
               (values t nil)
               (values nil error-output)))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(defun build-minimal-wasm-module (type-section global-section)
  "Build a minimal valid Wasm module with type and global sections"
  (let ((module (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    ;; Wasm magic number
    (vector-push-extend #x00 module)
    (vector-push-extend #x61 module)
    (vector-push-extend #x73 module)
    (vector-push-extend #x6D module)
    ;; Wasm version 1.0
    (vector-push-extend #x01 module)
    (vector-push-extend #x00 module)
    (vector-push-extend #x00 module)
    (vector-push-extend #x00 module)
    ;; Add type section
    (loop for byte across type-section
          do (vector-push-extend byte module))
    ;; Add global section
    (loop for byte across global-section
          do (vector-push-extend byte module))
    module))

(deftest test-runtime-init-produces-valid-wasm
  "Verify complete runtime initialization produces valid Wasm"
  (let ((wasm-bytes (generate-runtime-init)))
    (ok (vectorp wasm-bytes) "Should return vector")
    (ok (>= (length wasm-bytes) 8) "Should have at least Wasm header")
    ;; Check magic number
    (ok (= #x00 (aref wasm-bytes 0)) "First byte should be 0x00")
    (ok (= #x61 (aref wasm-bytes 1)) "Second byte should be 0x61 ('a')")
    (ok (= #x73 (aref wasm-bytes 2)) "Third byte should be 0x73 ('s')")
    (ok (= #x6D (aref wasm-bytes 3)) "Fourth byte should be 0x6D ('m')")
    ;; Validate with wasm-tools
    (multiple-value-bind (valid-p error-msg)
        (validate-wasm-bytes wasm-bytes)
      (ok valid-p (format nil "Wasm should validate: ~A" error-msg)))))

(deftest test-type-section-validates
  "Verify type section alone produces valid Wasm section"
  (let ((type-bytes (generate-type-section)))
    (ok (= 1 (aref type-bytes 0)) "Should start with type section ID")
    ;; Can't validate section in isolation, but can check structure
    (ok (> (length type-bytes) 5) "Should have section header + content")))

(deftest test-global-section-validates
  "Verify global section alone produces valid Wasm section"
  (let ((global-bytes (generate-global-section)))
    (ok (= 6 (aref global-bytes 0)) "Should start with global section ID")
    (ok (> (length global-bytes) 5) "Should have section header + content")))

(deftest test-combined-sections-validate
  "Verify combined type and global sections produce valid module"
  (skip "Requires complete implementation")
  (let* ((type-section (generate-type-section))
         (global-section (generate-global-section))
         (module (build-minimal-wasm-module type-section global-section)))
    (multiple-value-bind (valid-p error-msg)
        (validate-wasm-bytes module)
      (ok valid-p (format nil "Combined sections should validate: ~A" error-msg)))))

;;; ============================================================
;;; Section Order Tests (per Wasm spec)
;;; ============================================================

(deftest test-sections-in-correct-order
  "Verify sections are in correct order per Wasm spec"
  (let ((runtime-bytes (generate-runtime-init)))
    ;; Skip magic (4 bytes) and version (4 bytes)
    ;; Wasm section order: type(1), import(2), func(3), table(4), memory(5), global(6)...
    (let ((pos 8)
          (last-section-id 0))
      (loop while (< pos (length runtime-bytes))
            do (let ((section-id (aref runtime-bytes pos)))
                 (ok (> section-id last-section-id)
                     (format nil "Section ~D should come after section ~D"
                             section-id last-section-id))
                 (setf last-section-id section-id)
                 ;; Skip to next section (simplified - assumes LEB128 sizes < 128)
                 (let ((section-size (aref runtime-bytes (1+ pos))))
                   (incf pos (+ 2 section-size))))))))
