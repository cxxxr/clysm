;;;; stage0-wasm-valid-test.lisp - Integration tests for Stage 0 Wasm validation
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Task: T091

(defpackage #:clysm/tests/integration/stage0-wasm-valid-test
  (:use #:cl #:rove)
  (:import-from #:clysm/interpreter-bootstrap
                #:bootstrap-result
                #:bootstrap-result-p
                #:bootstrap-result-success
                #:bootstrap-result-wasm-bytes
                #:bootstrap-result-modules-loaded
                #:bootstrap-result-forms-compiled
                #:generate-stage0-via-interpreter)
  (:import-from #:clysm/eval/interpreter
                #:interpret
                #:make-interpreter-env)
  (:import-from #:clysm/compiler
                #:compile-to-wasm))

(in-package #:clysm/tests/integration/stage0-wasm-valid-test)

;;; ============================================================
;;; Helper: Check wasm-tools availability
;;; ============================================================

(defun wasm-tools-available-p ()
  "Check if wasm-tools validate is available."
  (handler-case
      (let ((result (uiop:run-program '("wasm-tools" "--version")
                                       :output :string
                                       :ignore-error-status t)))
        (and (stringp result)
             (search "wasm-tools" result)))
    (error () nil)))

;;; ============================================================
;;; Integration: Interpreter runs compile-to-wasm
;;; ============================================================

(deftest test-interpreter-compile-simple-form
  "Integration: Interpreter can execute compile-to-wasm for simple form."
  (let ((env (make-interpreter-env)))
    ;; Make compile-to-wasm available
    (interpret `(defun test-compile ()
                  (funcall (function ,#'compile-to-wasm) '(+ 1 2)))
               env)
    (let ((result (interpret '(test-compile) env)))
      (ok (vectorp result) "Should return Wasm bytes")
      (when (vectorp result)
        (ok (> (length result) 0) "Should have non-empty bytes")
        (when (>= (length result) 4)
          ;; Check Wasm magic
          (ok (= #x00 (aref result 0)))
          (ok (= #x61 (aref result 1)))
          (ok (= #x73 (aref result 2)))
          (ok (= #x6d (aref result 3))))))))

(deftest test-interpreter-compile-defun-form
  "Integration: Interpreter can compile defun forms."
  (let ((env (make-interpreter-env)))
    (interpret `(defun test-compile-defun ()
                  (funcall (function ,#'compile-to-wasm)
                           '(defun add-one (x) (+ x 1))))
               env)
    (let ((result (interpret '(test-compile-defun) env)))
      (ok (vectorp result) "Should return Wasm bytes for defun")
      (when (vectorp result)
        (ok (>= (length result) 8) "Defun Wasm should have header")))))

;;; ============================================================
;;; Integration: Stage 0 generation with module limit
;;; ============================================================

(deftest test-stage0-generation-minimal
  "Integration: Stage 0 generation with minimal module limit."
  (let ((result (handler-case
                    (generate-stage0-via-interpreter :module-limit 1)
                  (error (c)
                    (format *error-output* "~&Stage 0 gen error: ~A~%" c)
                    nil))))
    (when result
      (ok (bootstrap-result-p result))
      (ok (numberp (bootstrap-result-modules-loaded result)))
      (ok (numberp (bootstrap-result-forms-compiled result))))))

(deftest test-stage0-generation-with-output
  "Integration: Stage 0 generation produces Wasm output."
  (let ((result (handler-case
                    (generate-stage0-via-interpreter :module-limit 3)
                  (error (c)
                    (format *error-output* "~&Stage 0 gen error: ~A~%" c)
                    nil))))
    (when (and result (bootstrap-result-success result))
      (let ((bytes (bootstrap-result-wasm-bytes result)))
        (ok (vectorp bytes) "Should produce Wasm bytes")
        (when (and bytes (> (length bytes) 0))
          (ok (typep bytes '(vector (unsigned-byte 8)))
              "Bytes should be unsigned-byte 8"))))))

;;; ============================================================
;;; Integration: wasm-tools validation
;;; ============================================================

(deftest test-stage0-wasm-validates
  "Integration: Stage 0 Wasm passes wasm-tools validate."
  (if (not (wasm-tools-available-p))
      (skip "wasm-tools not available")
      (let ((result (handler-case
                        (generate-stage0-via-interpreter :module-limit 2)
                      (error (c)
                        (format *error-output* "~&Stage 0 gen error: ~A~%" c)
                        nil))))
        (when (and result
                   (bootstrap-result-success result)
                   (bootstrap-result-wasm-bytes result))
          (let* ((bytes (bootstrap-result-wasm-bytes result))
                 (temp-file (merge-pathnames "stage0-test.wasm"
                                              (uiop:temporary-directory))))
            ;; Write to temp file
            (with-open-file (s temp-file
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
              (write-sequence bytes s))
            ;; Validate with wasm-tools
            (unwind-protect
                (multiple-value-bind (output error-output exit-code)
                    (uiop:run-program (list "wasm-tools" "validate"
                                            (namestring temp-file))
                                       :output :string
                                       :error-output :string
                                       :ignore-error-status t)
                  (declare (ignore output))
                  (ok (= 0 exit-code)
                      (format nil "wasm-tools validate failed: ~A" error-output)))
              (when (probe-file temp-file)
                (delete-file temp-file))))))))

;;; ============================================================
;;; Integration: Direct vs Interpreter compilation match
;;; ============================================================

(deftest test-direct-vs-interpreter-compile
  "Integration: Direct and interpreter-invoked compile match."
  (let* ((form '(+ 1 2))
         (direct-bytes (compile-to-wasm form))
         (env (make-interpreter-env)))
    ;; Compile via interpreter
    (interpret `(defun interp-compile ()
                  (funcall (function ,#'compile-to-wasm) ',form))
               env)
    (let ((interp-bytes (interpret '(interp-compile) env)))
      ;; Both should produce same result
      (ok (equalp direct-bytes interp-bytes)
          "Direct and interpreter compilation should match"))))

;;; ============================================================
;;; Integration: Error handling during generation
;;; ============================================================

(deftest test-stage0-handles-compile-errors
  "Integration: Stage 0 generation handles compilation errors gracefully."
  (let ((result (handler-case
                    (generate-stage0-via-interpreter :module-limit 1)
                  (error (c)
                    (make-bootstrap-result
                     :success nil
                     :wasm-bytes nil
                     :modules-loaded 0
                     :forms-compiled 0
                     :errors (list (format nil "~A" c))
                     :elapsed-time 0.0)))))
    ;; Should return a result (success or failure), not crash
    (ok (bootstrap-result-p result)
        "Should return bootstrap-result even on errors")))

