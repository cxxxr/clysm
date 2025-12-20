;;;; runner.lisp - Execute ANSI tests via WASM

(in-package #:clysm/ansi-tests)

;;; Test result structure

(defstruct test-result
  "Result of running a single ANSI test."
  (name nil :type symbol)
  (status nil :type (member :pass :fail :error :skip :expected-failure :unexpected-success))
  (expected nil)
  (actual nil)
  (error nil :type (or string null))
  (time-ms 0 :type number)
  (skip-reason nil :type (or string null)))

;;; WASM execution

(defun run-wasm-via-node (wasm-bytes &key (func-name "test-main"))
  "Execute WASM bytes via Node.js and return the result."
  (let ((temp-file (merge-pathnames
                    (format nil "test-~A.wasm" (get-universal-time))
                    *temp-directory*)))
    (unwind-protect
        (progn
          ;; Write WASM to temp file
          (with-open-file (out temp-file
                               :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
            (write-sequence wasm-bytes out))
          ;; Run via Node.js
          (let* ((cmd (format nil "~A ~A ~A ~A"
                              *node-executable*
                              (namestring *wasm-runner*)
                              (namestring temp-file)
                              func-name))
                 (output (uiop:run-program cmd
                                           :output :string
                                           :error-output :string
                                           :ignore-error-status t)))
            (parse-wasm-output output)))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

;;; Single test execution

(defun run-single-test (test-spec)
  "Run a single test-spec and return a test-result."
  (let ((name (test-spec-name test-spec))
        (expected (test-spec-expected test-spec))
        (start-time (get-internal-real-time)))
    ;; Check if test should be skipped
    (when (test-spec-skip-reason test-spec)
      (return-from run-single-test
        (make-test-result
         :name name
         :status :skip
         :expected expected
         :skip-reason (test-spec-skip-reason test-spec))))
    ;; Check if form can be compiled
    (let ((missing-features (analyze-form-requirements (test-spec-form test-spec))))
      (when missing-features
        (return-from run-single-test
          (make-test-result
           :name name
           :status :skip
           :expected expected
           :skip-reason (format nil "Requires unimplemented features: ~A"
                                missing-features)))))
    ;; Try to compile and run
    (handler-case
        (let* ((wasm-bytes (compile-test-to-wasm test-spec))
               (wasm-result (run-wasm-via-node wasm-bytes))
               (end-time (get-internal-real-time))
               (elapsed-ms (/ (- end-time start-time)
                              (/ internal-time-units-per-second 1000.0)))
               (status (classify-with-expectations name wasm-result expected)))
          (make-test-result
           :name name
           :status status
           :expected expected
           :actual (if (eq (wasm-result-status wasm-result) :success)
                       (wasm-result-value wasm-result)
                       nil)
           :error (wasm-result-error wasm-result)
           :time-ms elapsed-ms))
      (error (e)
        (make-test-result
         :name name
         :status :error
         :expected expected
         :error (format nil "Compilation error: ~A" e)
         :time-ms (/ (- (get-internal-real-time) start-time)
                     (/ internal-time-units-per-second 1000.0)))))))

;;; Batch execution

(defun run-tests (test-specs &key (progress-callback nil))
  "Run a list of test-specs and return list of test-results."
  (let ((results nil)
        (total (length test-specs))
        (current 0))
    (dolist (spec test-specs)
      (incf current)
      (when progress-callback
        (funcall progress-callback current total (test-spec-name spec)))
      (push (run-single-test spec) results))
    (nreverse results)))

(defun run-category (category &key (filter :simple) (progress t))
  "Run all tests in a category.
   FILTER can be :all, :runnable, or :simple."
  (let* ((all-tests (load-category-tests category))
         (tests (case filter
                  (:all all-tests)
                  (:runnable (filter-runnable-tests all-tests))
                  (:simple (filter-simple-tests (filter-runnable-tests all-tests)))
                  (t all-tests)))
         (callback (when progress
                     (lambda (current total name)
                       (format t "~&[~D/~D] Running ~A~%" current total name)))))
    (format t "~&Running ~D tests from ~A category...~%" (length tests) category)
    (run-tests tests :progress-callback callback)))

;;; Main entry point

(defun run-ansi-tests (&key (categories '(:numbers))
                            (filter :simple)
                            (progress t))
  "Run ANSI tests for specified categories.
   Returns a test-report structure."
  (let ((all-results nil))
    (dolist (category (if (listp categories) categories (list categories)))
      (when (category-worth-running-p category)
        (let ((results (run-category category :filter filter :progress progress)))
          (setf all-results (append all-results results)))))
    (make-report all-results)))

(defun all-passed-p (&optional (report nil))
  "Check if all tests passed (for CI integration)."
  (if report
      (and (zerop (test-report-failed report))
           (zerop (test-report-errors report)))
      t))

;;; Quick test functions

(defun test-form (form &key (expected nil expected-p))
  "Quick test: compile and run a single form."
  (let* ((spec (make-test-spec :name 'quick-test
                               :form form
                               :expected expected))
         (result (run-single-test spec)))
    (format t "~&Form: ~S~%" form)
    (format t "Result: ~A~%" (test-result-status result))
    (format t "Expected: ~S~%" (test-result-expected result))
    (format t "Actual: ~S~%" (test-result-actual result))
    (when (test-result-error result)
      (format t "Error: ~A~%" (test-result-error result)))
    result))

(defun test-file (filepath &key (filter :simple))
  "Run all tests from a single file."
  (let* ((tests (read-test-file filepath))
         (filtered (case filter
                     (:all tests)
                     (:runnable (filter-runnable-tests tests))
                     (:simple (filter-simple-tests (filter-runnable-tests tests)))
                     (t tests))))
    (format t "~&Found ~D tests, ~D runnable~%" (length tests) (length filtered))
    (run-tests filtered
               :progress-callback (lambda (current total name)
                                    (format *trace-output* "~&[~D/~D] ~A~%" current total name)))))
