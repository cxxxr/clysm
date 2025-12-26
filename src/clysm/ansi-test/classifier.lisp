;;;; classifier.lisp - Test result classification
;;;;
;;;; Classifies test execution results as PASS/FAIL/SKIP.

(in-package #:clysm/ansi-test)

;;; ==========================================================================
;;; T018: expected-value-verifiable-p function
;;; ==========================================================================

(defun expected-value-verifiable-p (expected)
  "Check if EXPECTED is a value type that we can verify against Wasm output.
Only fixnums, T, and NIL are verifiable. Symbols (except T/NIL) and cons cells
are not verifiable because Wasm cannot return them in a comparable format."
  (cond
    ;; Fixnums are verifiable
    ((typep expected 'fixnum) t)
    ;; T and NIL are verifiable (booleans)
    ((eq expected t) t)
    ((null expected) t)
    ;; Other symbols are not verifiable
    ((symbolp expected) nil)
    ;; Cons cells/lists are not verifiable
    ((consp expected) nil)
    ;; Default: not verifiable
    (t nil)))

;;; ==========================================================================
;;; T028: compare-values function
;;; ==========================================================================

(defun compare-values (expected actual)
  "Compare expected and actual values for equality.
Returns :match, :mismatch, or :unverifiable."
  (cond
    ;; NIL sentinel from Wasm
    ((eql actual -2147483648) ; MIN_INT32 = NIL
     (if (null expected)
         :match
         :mismatch))
    ;; Non-fixnum sentinel from Wasm
    ((eql actual -2147483647) ; MIN_INT32 + 1 = non-fixnum
     :unverifiable)
    ;; Fixnum comparison
    ((and (integerp expected) (integerp actual))
     (if (= expected actual) :match :mismatch))
    ;; Boolean comparison
    ((and (eq expected t) (equal actual "true"))
     :match)
    ((and (eq expected nil) (equal actual "false"))
     :match)
    ;; List comparison for single values
    ((and (listp expected) (= 1 (length expected)))
     (compare-values (car expected) actual))
    ;; Default: try equal
    ((equal expected actual) :match)
    (t :mismatch)))

;;; ==========================================================================
;;; T027: classify-result function
;;; ==========================================================================

(defun classify-result (test-case actual-value error-info execution-time-ms
                        &optional (registry *default-skip-registry*))
  "Classify a test execution result into PASS/FAIL/SKIP.
Returns a test-result structure."
  ;; First check for pre-execution skip reasons
  (let ((skip-reason (detect-skip-reason test-case registry)))
    (when skip-reason
      (return-from classify-result
        (make-test-result
         :test-case test-case
         :status :skip
         :skip-reason skip-reason
         :execution-time-ms execution-time-ms))))
  ;; Check for execution errors
  (when error-info
    (return-from classify-result
      (make-test-result
       :test-case test-case
       :status :fail
       :error-message (if (stringp error-info)
                          error-info
                          (format nil "~A" error-info))
       :execution-time-ms execution-time-ms)))
  ;; Compare actual vs expected
  (let* ((expected (test-case-expected-values test-case))
         (expected-value (car expected)))
    ;; T020: Check if expected value is verifiable before comparison
    (unless (expected-value-verifiable-p expected-value)
      (return-from classify-result
        (make-test-result
         :test-case test-case
         :status :skip
         :skip-reason (format nil "unverifiable: expected ~A ~S"
                              (type-of expected-value) expected-value)
         :actual-values (list actual-value)
         :execution-time-ms execution-time-ms)))
    ;; Now compare verifiable values
    (let ((comparison (compare-values expected-value actual-value)))
      (case comparison
        (:match
         (make-test-result
          :test-case test-case
          :status :pass
          :actual-values (list actual-value)
          :execution-time-ms execution-time-ms))
        (:unverifiable
         (make-test-result
          :test-case test-case
          :status :skip
          :skip-reason "unverifiable"
          :actual-values (list actual-value)
          :execution-time-ms execution-time-ms))
        (t ; :mismatch
         (make-test-result
          :test-case test-case
          :status :fail
          :actual-values (list actual-value)
          :error-message (format nil "Expected ~S, got ~S"
                                 expected-value actual-value)
          :execution-time-ms execution-time-ms))))))
