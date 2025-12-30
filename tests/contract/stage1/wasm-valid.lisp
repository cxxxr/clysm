;;;; wasm-valid.lisp - T029: Contract test for Wasm validation
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Verifies that dist/clysm-stage1.wasm passes wasm-tools validate

(in-package #:clysm/tests)

;;; ==========================================================================
;;; T029: Wasm Validation
;;; ==========================================================================

(deftest stage1-wasm-valid ()
  "Verify dist/clysm-stage1.wasm passes wasm-tools validate."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file path)
        (multiple-value-bind (output error-output status)
            (uiop:run-program (list "wasm-tools" "validate" (namestring path))
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output))
          (ok (zerop status)
              (format nil "wasm-tools validate exit code: ~D~@[~%~A~]"
                      status (unless (zerop status) error-output))))
        (fail "Stage 1 binary not found at ~A" path))))
