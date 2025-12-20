;;;; reporter.lisp - Generate reports from test results

(in-package #:clysm/ansi-tests)

;;; Report structure

(defstruct test-report
  "Summary report of test execution."
  (timestamp (get-universal-time) :type integer)
  (total 0 :type integer)
  (passed 0 :type integer)
  (failed 0 :type integer)
  (errors 0 :type integer)
  (skipped 0 :type integer)
  (expected-failures 0 :type integer)
  (unexpected-successes 0 :type integer)
  (results nil :type list)
  (by-category nil :type list)
  (total-time-ms 0 :type number))

;;; Report generation

(defun make-report (results)
  "Create a test-report from a list of test-results."
  (let ((report (make-test-report :results results)))
    ;; Count by status
    (dolist (result results)
      (incf (test-report-total report))
      (incf (test-report-total-time-ms report) (test-result-time-ms result))
      (case (test-result-status result)
        (:pass (incf (test-report-passed report)))
        (:fail (incf (test-report-failed report)))
        (:error (incf (test-report-errors report)))
        (:skip (incf (test-report-skipped report)))
        (:expected-failure (incf (test-report-expected-failures report)))
        (:unexpected-success (incf (test-report-unexpected-successes report)))))
    report))

;;; Summary printing

(defun print-summary (report &optional (stream *standard-output*))
  "Print a human-readable summary of test results."
  (format stream "~%~60,,,'-<~>~%")
  (format stream "ANSI Test Results Summary~%")
  (format stream "~60,,,'-<~>~%")
  (format stream "~%")
  (format stream "Total:              ~6D~%" (test-report-total report))
  (format stream "Passed:             ~6D (~5,1F%)~%"
          (test-report-passed report)
          (if (zerop (test-report-total report))
              0.0
              (* 100.0 (/ (test-report-passed report)
                          (test-report-total report)))))
  (format stream "Failed:             ~6D~%" (test-report-failed report))
  (format stream "Errors:             ~6D~%" (test-report-errors report))
  (format stream "Skipped:            ~6D~%" (test-report-skipped report))
  (when (plusp (test-report-expected-failures report))
    (format stream "Expected Failures:  ~6D~%" (test-report-expected-failures report)))
  (when (plusp (test-report-unexpected-successes report))
    (format stream "Unexpected Success: ~6D~%" (test-report-unexpected-successes report)))
  (format stream "~%")
  (format stream "Total Time:         ~6,2F seconds~%"
          (/ (test-report-total-time-ms report) 1000.0))
  (format stream "~60,,,'-<~>~%")
  ;; Print failures if any
  (let ((failures (remove-if-not (lambda (r)
                                   (member (test-result-status r) '(:fail :error)))
                                 (test-report-results report))))
    (when failures
      (format stream "~%FAILURES:~%")
      (dolist (f failures)
        (format stream "~%  ~A (~A)~%" (test-result-name f) (test-result-status f))
        (format stream "    Expected: ~S~%" (test-result-expected f))
        (format stream "    Actual:   ~S~%" (test-result-actual f))
        (when (test-result-error f)
          (format stream "    Error:    ~A~%" (test-result-error f))))))
  (format stream "~%"))

(defun print-short-summary (report &optional (stream *standard-output*))
  "Print a one-line summary."
  (format stream "~&Tests: ~D total, ~D passed, ~D failed, ~D errors, ~D skipped~%"
          (test-report-total report)
          (test-report-passed report)
          (test-report-failed report)
          (test-report-errors report)
          (test-report-skipped report)))

;;; JSON report generation

(defun generate-json-report (report &optional (stream *standard-output*))
  "Generate a JSON report for CI integration."
  (format stream "{~%")
  (format stream "  \"timestamp\": ~D,~%"
          (test-report-timestamp report))
  (format stream "  \"summary\": {~%")
  (format stream "    \"total\": ~D,~%"
          (test-report-total report))
  (format stream "    \"passed\": ~D,~%"
          (test-report-passed report))
  (format stream "    \"failed\": ~D,~%"
          (test-report-failed report))
  (format stream "    \"errors\": ~D,~%"
          (test-report-errors report))
  (format stream "    \"skipped\": ~D,~%"
          (test-report-skipped report))
  (format stream "    \"expected_failures\": ~D,~%"
          (test-report-expected-failures report))
  (format stream "    \"unexpected_successes\": ~D,~%"
          (test-report-unexpected-successes report))
  (format stream "    \"pass_rate\": ~,4F~%"
          (if (zerop (test-report-total report))
              0.0
              (/ (test-report-passed report)
                 (float (test-report-total report)))))
  (format stream "  },~%")
  ;; Failed tests details
  (format stream "  \"failures\": [~%")
  (let ((failures (remove-if-not (lambda (r)
                                   (member (test-result-status r) '(:fail :error)))
                                 (test-report-results report))))
    (loop for f in failures
          for i from 0
          do (format stream "    {~%")
             (format stream "      \"name\": \"~A\",~%"
                     (test-result-name f))
             (format stream "      \"status\": \"~A\",~%"
                     (test-result-status f))
             (format stream "      \"expected\": \"~S\",~%"
                     (test-result-expected f))
             (format stream "      \"actual\": \"~S\"~A~%"
                     (test-result-actual f)
                     (if (test-result-error f) "," ""))
             (when (test-result-error f)
               (format stream "      \"error\": \"~A\"~%"
                       (escape-json-string (test-result-error f))))
             (format stream "    }~A~%"
                     (if (< i (1- (length failures))) "," ""))))
  (format stream "  ]~%")
  (format stream "}~%"))

(defun escape-json-string (str)
  "Escape special characters for JSON string."
  (with-output-to-string (out)
    (loop for char across str
          do (case char
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (otherwise (write-char char out))))))

;;; Report file generation

(defun generate-report (report filepath &key (format :text))
  "Generate and save a report to a file.
   FORMAT can be :text or :json."
  (with-open-file (stream filepath
                          :direction :output
                          :if-exists :supersede)
    (case format
      (:json (generate-json-report report stream))
      (t (print-summary report stream))))
  filepath)

;;; Progress display

(defun make-progress-bar (current total &key (width 40))
  "Create a text progress bar."
  (let* ((pct (if (zerop total) 0.0 (/ current total)))
         (filled (round (* width pct)))
         (empty (- width filled)))
    (format nil "[~A~A] ~5,1F%"
            (make-string filled :initial-element #\#)
            (make-string empty :initial-element #\-)
            (* 100.0 pct))))

;;; Category breakdown

(defun report-by-category (results)
  "Group results by category and create per-category reports."
  (let ((by-cat (make-hash-table)))
    (dolist (r results)
      (let ((cat :unknown))  ; Would need category info in test-result
        (push r (gethash cat by-cat))))
    ;; Convert to alist of reports
    (let ((reports nil))
      (maphash (lambda (cat results)
                 (push (cons cat (make-report results)) reports))
               by-cat)
      reports)))
