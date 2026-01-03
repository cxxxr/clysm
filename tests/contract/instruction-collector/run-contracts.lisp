;;;; Contract Test Runner for Instruction Collector Migration
;;;; Runs all contract tests and reports results

(defpackage #:clysm/tests/contract/instruction-collector/run-contracts
  (:use #:cl)
  (:import-from #:clysm/tests/contract/instruction-collector/baseline-capture
                #:baseline-exists-p
                #:capture-equalp-baseline
                #:capture-equal-baseline
                #:capture-primitive-arithmetic-baseline
                #:capture-primitive-comparison-baseline
                #:capture-primitive-typep-baseline)
  (:import-from #:clysm/tests/contract/instruction-collector/bytecode-compare
                #:verify-equalp-contract
                #:verify-equal-contract
                #:verify-primitive-arithmetic-contract
                #:verify-primitive-comparison-contract
                #:verify-primitive-typep-contract)
  (:export #:run-all-contracts
           #:capture-all-baselines
           #:contract-status))

(in-package #:clysm/tests/contract/instruction-collector/run-contracts)

(defstruct contract-result
  "Result of a single contract test."
  name
  passed-p
  message
  duration-ms)

(defun run-single-contract (name verify-fn)
  "Run a single contract test and return a contract-result."
  (let ((start-time (get-internal-real-time)))
    (handler-case
        (multiple-value-bind (passed-p message)
            (funcall verify-fn)
          (make-contract-result
           :name name
           :passed-p passed-p
           :message message
           :duration-ms (/ (- (get-internal-real-time) start-time)
                          (/ internal-time-units-per-second 1000.0))))
      (error (e)
        (make-contract-result
         :name name
         :passed-p nil
         :message (format nil "Error: ~A" e)
         :duration-ms (/ (- (get-internal-real-time) start-time)
                        (/ internal-time-units-per-second 1000.0)))))))

(defun run-all-contracts (&key (verbose t))
  "Run all instruction collector contract tests.
   Returns (VALUES all-passed-p results-list)."
  (let* ((contracts
           '(("equalp-contract" verify-equalp-contract)
             ("equal-contract" verify-equal-contract)
             ("primitive-arithmetic-contract" verify-primitive-arithmetic-contract)
             ("primitive-comparison-contract" verify-primitive-comparison-contract)
             ("primitive-typep-contract" verify-primitive-typep-contract)))
         (results
           (loop for (name fn) in contracts
                 for result = (run-single-contract name fn)
                 collect result
                 when verbose
                   do (format t "~&~A: ~:[FAIL~;PASS~] (~,2Fms)~%"
                              name
                              (contract-result-passed-p result)
                              (contract-result-duration-ms result))
                      (unless (contract-result-passed-p result)
                        (format t "  ~A~%" (contract-result-message result)))))
         (all-passed (every #'contract-result-passed-p results)))
    (when verbose
      (format t "~&~%=== Contract Test Summary ===~%")
      (format t "Passed: ~D/~D~%"
              (count-if #'contract-result-passed-p results)
              (length results))
      (if all-passed
          (format t "Status: ALL CONTRACTS VERIFIED~%")
          (format t "Status: SOME CONTRACTS FAILED~%")))
    (values all-passed results)))

(defun capture-all-baselines (&key (verbose t))
  "Capture all baselines for contract tests.
   Should be run BEFORE any migration work."
  (when verbose
    (format t "~&=== Capturing Contract Baselines ===~%"))
  (let ((captures
          '(("equalp-baseline" capture-equalp-baseline)
            ("equal-baseline" capture-equal-baseline)
            ("primitive-arithmetic-baseline" capture-primitive-arithmetic-baseline)
            ("primitive-comparison-baseline" capture-primitive-comparison-baseline)
            ("primitive-typep-baseline" capture-primitive-typep-baseline))))
    (loop for (name fn) in captures
          do (when verbose
               (format t "Capturing ~A... " name))
             (handler-case
                 (progn
                   (funcall fn)
                   (when verbose
                     (format t "OK~%")))
               (error (e)
                 (when verbose
                   (format t "FAILED: ~A~%" e)))))
    (when verbose
      (format t "~&Baseline capture complete.~%"))))

(defun contract-status ()
  "Print a summary of which baselines exist and contract status."
  (format t "~&=== Instruction Collector Contract Status ===~%~%")
  (let ((baselines '("equalp-baseline"
                     "equal-baseline"
                     "primitive-arithmetic-baseline"
                     "primitive-comparison-baseline"
                     "primitive-typep-baseline")))
    (format t "Baselines:~%")
    (dolist (name baselines)
      (format t "  ~A: ~:[MISSING~;EXISTS~]~%"
              name (baseline-exists-p name)))
    (format t "~%Run (capture-all-baselines) to create missing baselines.~%")
    (format t "Run (run-all-contracts) to verify migration.~%")))
