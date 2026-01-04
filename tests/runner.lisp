;;;; tests/runner.lisp - Test Runner and Reporter
;;;;
;;;; Runs tests and produces human-readable output.

(in-package #:clysm/tests)

;;; ============================================================
;;; Output Formatting
;;; ============================================================

(defvar *test-output* *standard-output*
  "Stream for test output.")

(defun format-duration (seconds)
  "Format duration in human-readable form."
  (cond ((< seconds 0.001) (format nil "~,3Fms" (* seconds 1000)))
        ((< seconds 1) (format nil "~,1Fms" (* seconds 1000)))
        (t (format nil "~,2Fs" seconds))))

(defun print-test-result (result)
  "Print a single test result."
  (let* ((test (test-result-test-case result))
         (name (test-case-name test))
         (passed (test-result-passed-p result))
         (duration (test-result-duration result)))
    (format *test-output* "  ~A ~A (~A)~%"
            (if passed "[PASS]" "[FAIL]")
            name
            (format-duration duration))
    (unless passed
      (let ((info (test-result-error-info result)))
        (cond
          ((typep info 'condition)
           (format *test-output* "    Error: ~A~%" info))
          ((listp info)
           (dolist (failure info)
             (destructuring-bind (desc expected actual) failure
               (format *test-output* "    Failed: ~A~%" desc)
               (format *test-output* "      Expected: ~S~%" expected)
               (format *test-output* "      Actual:   ~S~%" actual)))))))))

(defun print-suite-summary (suite-name results)
  "Print summary for a test suite."
  (let ((total (length results))
        (passed (count-if #'test-result-passed-p results))
        (total-time (reduce #'+ results :key #'test-result-duration)))
    (format *test-output* "~%Suite ~A: ~D/~D passed (~A)~%"
            suite-name passed total (format-duration total-time))
    (values passed total)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-all-tests (&key (verbose t))
  "Run all tests in all suites. Returns T if all passed."
  (let ((all-passed t)
        (total-tests 0)
        (passed-tests 0)
        (start-time (get-internal-real-time)))
    (format *test-output* "~%Running Clysm Test Suite~%")
    (format *test-output* "========================~%~%")

    (maphash
     (lambda (suite-name tests)
       (when tests
         (format *test-output* "Suite: ~A~%" suite-name)
         (format *test-output* "~A~%" (make-string 40 :initial-element #\-))
         (let ((results (mapcar #'run-test-case (reverse tests))))
           (when verbose
             (dolist (result results)
               (print-test-result result)))
           (multiple-value-bind (suite-passed suite-total)
               (print-suite-summary suite-name results)
             (incf total-tests suite-total)
             (incf passed-tests suite-passed)
             (when (/= suite-passed suite-total)
               (setf all-passed nil))))))
     *test-suites*)

    (let ((total-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
      (format *test-output* "~%========================~%")
      (format *test-output* "Total: ~D/~D tests passed (~A)~%"
              passed-tests total-tests (format-duration total-time))
      (format *test-output* "Status: ~A~%~%"
              (if all-passed "SUCCESS" "FAILURE")))

    all-passed))

(defun run-tests-matching (pattern)
  "Run tests whose names match PATTERN (string or regex)."
  (let ((results nil))
    (maphash
     (lambda (suite tests)
       (declare (ignore suite))
       (dolist (test tests)
         (when (search pattern (string (test-case-name test))
                       :test #'char-equal)
           (push (run-test-case test) results))))
     *test-suites*)
    (dolist (result (nreverse results))
      (print-test-result result))
    results))

;;; ============================================================
;;; Interactive Helpers
;;; ============================================================

(defun list-suites ()
  "List all registered test suites."
  (let ((suites nil))
    (maphash (lambda (name tests)
               (push (cons name (length tests)) suites))
             *test-suites*)
    (format t "~%Test Suites:~%")
    (dolist (suite (sort suites #'string< :key (lambda (x) (string (car x)))))
      (format t "  ~A (~D tests)~%" (car suite) (cdr suite)))
    (values)))

(defun list-tests (&optional suite-name)
  "List all tests, optionally filtered by suite."
  (format t "~%Tests:~%")
  (maphash
   (lambda (suite tests)
     (when (or (null suite-name) (eq suite suite-name))
       (format t "  Suite ~A:~%" suite)
       (dolist (test (reverse tests))
         (format t "    - ~A~%" (test-case-name test)))))
   *test-suites*)
  (values))
