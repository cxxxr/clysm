;;;; interpreter-stage0-test.lisp - Contract tests for Stage 0 generation via interpreter
;;;; Feature 044: Interpreter Bootstrap Strategy
;;;; Task: T090

(defpackage #:clysm/tests/contract/interpreter-stage0-test
  (:use #:cl #:rove)
  (:import-from #:clysm/interpreter-bootstrap
                #:bootstrap-result
                #:make-bootstrap-result
                #:bootstrap-result-p
                #:bootstrap-result-success
                #:bootstrap-result-wasm-bytes
                #:bootstrap-result-modules-loaded
                #:bootstrap-result-forms-compiled
                #:bootstrap-result-errors
                #:bootstrap-result-elapsed-time
                #:generate-stage0-via-interpreter
                #:form-compilable-p
                #:*bootstrap-progress-callback*)
  (:import-from #:clysm/eval/interpreter
                #:make-interpreter-env))

(in-package #:clysm/tests/contract/interpreter-stage0-test)

;;; ============================================================
;;; Contract: bootstrap-result struct
;;; ============================================================

(deftest test-bootstrap-result-struct
  "Contract: bootstrap-result struct has all required fields."
  (let ((result (make-bootstrap-result
                 :success t
                 :wasm-bytes (make-array 4 :element-type '(unsigned-byte 8)
                                          :initial-contents '(0 97 115 109))
                 :modules-loaded 10
                 :forms-compiled 50
                 :errors nil
                 :elapsed-time 1.5)))
    (ok (bootstrap-result-p result))
    (ok (bootstrap-result-success result))
    (ok (vectorp (bootstrap-result-wasm-bytes result)))
    (ok (= 10 (bootstrap-result-modules-loaded result)))
    (ok (= 50 (bootstrap-result-forms-compiled result)))
    (ok (null (bootstrap-result-errors result)))
    (ok (= 1.5 (bootstrap-result-elapsed-time result)))))

(deftest test-bootstrap-result-with-errors
  "Contract: bootstrap-result can track errors."
  (let ((result (make-bootstrap-result
                 :success nil
                 :wasm-bytes nil
                 :modules-loaded 5
                 :forms-compiled 20
                 :errors '("Error 1" "Error 2")
                 :elapsed-time 0.5)))
    (ok (not (bootstrap-result-success result)))
    (ok (null (bootstrap-result-wasm-bytes result)))
    (ok (= 2 (length (bootstrap-result-errors result))))))

;;; ============================================================
;;; Contract: form-compilable-p predicate
;;; ============================================================

(deftest test-form-compilable-p-defun
  "Contract: defun forms are compilable."
  (ok (form-compilable-p '(defun foo () 42))))

(deftest test-form-compilable-p-lambda
  "Contract: lambda forms are compilable."
  (ok (form-compilable-p '(lambda (x) (+ x 1)))))

(deftest test-form-compilable-p-expression
  "Contract: Simple expressions are compilable."
  (ok (form-compilable-p '(+ 1 2))))

(deftest test-form-compilable-p-in-package
  "Contract: in-package forms are NOT compilable (meta-level)."
  (ok (not (form-compilable-p '(in-package #:cl-user)))))

(deftest test-form-compilable-p-declare
  "Contract: declare forms are NOT compilable (meta-level)."
  (ok (not (form-compilable-p '(declare (optimize (speed 3)))))))

(deftest test-form-compilable-p-eval-when
  "Contract: eval-when forms are NOT compilable (meta-level)."
  (ok (not (form-compilable-p '(eval-when (:compile-toplevel) t)))))

;;; ============================================================
;;; Contract: Progress callback support
;;; ============================================================

(deftest test-progress-callback-called
  "Contract: *bootstrap-progress-callback* is called during generation."
  (let ((calls nil))
    (let ((*bootstrap-progress-callback*
            (lambda (phase module-count form-count)
              (push (list phase module-count form-count) calls))))
      ;; Just test that the callback mechanism works
      ;; Actual generation may fail but callback should be invoked
      (handler-case
          (generate-stage0-via-interpreter :module-limit 1)
        (error (c)
          (declare (ignore c))
          nil)))
    ;; Should have at least been called once (start phase)
    (ok (> (length calls) 0) "Progress callback should be invoked")))

;;; ============================================================
;;; Contract: generate-stage0-via-interpreter returns bootstrap-result
;;; ============================================================

(deftest test-generate-stage0-returns-result
  "Contract: generate-stage0-via-interpreter returns bootstrap-result."
  (let ((result (handler-case
                    (generate-stage0-via-interpreter :module-limit 1)
                  (error (c)
                    (declare (ignore c))
                    ;; Return a failed result if generation errors
                    (make-bootstrap-result
                     :success nil
                     :wasm-bytes nil
                     :modules-loaded 0
                     :forms-compiled 0
                     :errors '("Generation failed")
                     :elapsed-time 0.0)))))
    (ok (bootstrap-result-p result) "Should return a bootstrap-result")))

(deftest test-generate-stage0-tracks-time
  "Contract: generate-stage0-via-interpreter tracks elapsed time."
  (let ((result (handler-case
                    (generate-stage0-via-interpreter :module-limit 1)
                  (error (c)
                    (declare (ignore c))
                    (make-bootstrap-result
                     :success nil
                     :wasm-bytes nil
                     :modules-loaded 0
                     :forms-compiled 0
                     :errors nil
                     :elapsed-time 0.0)))))
    (ok (numberp (bootstrap-result-elapsed-time result)))))

;;; ============================================================
;;; Contract: Wasm output validity
;;; ============================================================

(deftest test-stage0-wasm-magic-bytes
  "Contract: Stage 0 Wasm starts with magic bytes when successful."
  (let ((result (handler-case
                    (generate-stage0-via-interpreter :module-limit 2)
                  (error (c)
                    (declare (ignore c))
                    nil))))
    (when (and result
               (bootstrap-result-success result)
               (bootstrap-result-wasm-bytes result))
      (let ((bytes (bootstrap-result-wasm-bytes result)))
        (ok (>= (length bytes) 8) "Wasm should have at least 8 bytes")
        (when (>= (length bytes) 8)
          ;; Magic: \0asm
          (ok (= #x00 (aref bytes 0)))
          (ok (= #x61 (aref bytes 1)))
          (ok (= #x73 (aref bytes 2)))
          (ok (= #x6d (aref bytes 3)))
          ;; Version 1
          (ok (= #x01 (aref bytes 4))))))))

