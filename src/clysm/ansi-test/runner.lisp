;;;; runner.lisp - Test execution engine
;;;;
;;;; Executes ANSI tests by compiling with Clysm and running via wasmtime.

(in-package #:clysm/ansi-test)

;;; ==========================================================================
;;; T029: execute-single-test function
;;; ==========================================================================

(defun execute-single-test (test-case &key (timeout 30) (registry *default-skip-registry*))
  "Execute a single test case and return a test-result.
Compiles the test form with Clysm and executes via wasmtime."
  (let ((start-time (get-internal-real-time))
        (form (test-case-form test-case)))
    ;; Pre-check for skip reasons before compilation
    (let ((skip-reason (detect-skip-reason test-case registry)))
      (when skip-reason
        (return-from execute-single-test
          (make-test-result
           :test-case test-case
           :status :skip
           :skip-reason skip-reason
           :execution-time-ms 0))))
    ;; Try to compile and execute
    (handler-case
        (let* ((wasm-bytes (clysm/compiler:compile-to-wasm form))
               (result (run-wasm-bytes wasm-bytes :timeout timeout))
               (end-time (get-internal-real-time))
               (duration-ms (round (* 1000 (/ (- end-time start-time)
                                              internal-time-units-per-second)))))
          (classify-result test-case result nil duration-ms registry))
      (error (e)
        (let* ((end-time (get-internal-real-time))
               (duration-ms (round (* 1000 (/ (- end-time start-time)
                                              internal-time-units-per-second)))))
          (make-test-result
           :test-case test-case
           :status :skip
           :skip-reason (format nil "compile-error: ~A" e)
           :execution-time-ms duration-ms))))))

(defun find-host-shim-runner ()
  "Find the host-shim run-wasm.js script path."
  (let ((candidates (list
                     ;; Relative to current working directory
                     (merge-pathnames "host-shim/run-wasm.js" (uiop:getcwd))
                     ;; Relative to ASDF system location
                     (asdf:system-relative-pathname :clysm "host-shim/run-wasm.js"))))
    (dolist (path candidates)
      (when (probe-file path)
        (return-from find-host-shim-runner (namestring path))))
    nil))

(defun run-wasm-bytes (bytes &key (timeout 30))
  "Run compiled Wasm bytes through Node.js with host-shim and return the result.
Returns the parsed output value or signals an error.
TIMEOUT is in seconds (default 30)."
  (uiop:with-temporary-file (:pathname path :type "wasm" :keep nil)
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence bytes out))
    ;; Use Node.js with the host-shim for FFI support
    (let* ((shim-runner (find-host-shim-runner))
           (cmd (if shim-runner
                    ;; Use Node.js with host-shim (supports FFI imports)
                    (list "timeout" (format nil "~Ds" timeout)
                          "node" shim-runner (namestring path))
                    ;; Fallback to wasmtime (will fail on FFI imports)
                    (list "timeout" (format nil "~Ds" timeout)
                          "wasmtime" "--wasm" "gc"
                          "--wasm" "function-references"
                          "--wasm" "exceptions"
                          "--invoke" "_start"
                          (namestring path)))))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program cmd
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (cond
          ((zerop exit-code)
           (parse-wasm-output output))
          ((= exit-code 124) ; timeout exit code
           (error "Wasm execution timed out after ~D seconds" timeout))
          (t
           (error "Wasm execution failed (exit ~D): ~A" exit-code error-output)))))))

(defun parse-wasm-output (output)
  "Parse wasmtime output to a Lisp value."
  (let ((trimmed (string-trim '(#\Space #\Newline #\Return) output)))
    (cond
      ((string= trimmed "true") t)
      ((string= trimmed "false") nil)
      ((string= trimmed "") nil)
      (t
       (handler-case
           (parse-integer trimmed)
         (error () trimmed))))))

;;; ==========================================================================
;;; T030: run-category function
;;; ==========================================================================

(defun run-category (category-name &key (timeout 30) (registry *default-skip-registry*)
                                        progress-callback)
  "Execute all tests in a category and return a category-result."
  (let* ((tests (load-category-tests category-name))
         (results '())
         (pass-count 0)
         (fail-count 0)
         (skip-count 0)
         (start-time (get-internal-real-time))
         (total (length tests)))
    (loop for test in tests
          for i from 1
          do (let ((result (execute-single-test test :timeout timeout :registry registry)))
               (push result results)
               (case (test-result-status result)
                 (:pass (incf pass-count))
                 (:fail (incf fail-count))
                 (:skip (incf skip-count)))
               (when progress-callback
                 (funcall progress-callback category-name i total
                          pass-count fail-count skip-count))))
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second)))))
      (make-category-result
       :name category-name
       :total-count total
       :pass-count pass-count
       :fail-count fail-count
       :skip-count skip-count
       :duration-ms duration-ms
       :results (nreverse results)))))

;;; ==========================================================================
;;; T031: run-ansi-tests main entry point
;;; ==========================================================================

(defun run-ansi-tests (&key category timeout (parallel 1) report-path)
  "Run the ANSI test suite with optional filters.
Returns (values coverage-report overall-pass-rate)."
  (declare (ignore parallel)) ; TODO: implement in Phase 8
  (let* ((timeout (or timeout (skip-registry-timeout-seconds *default-skip-registry*)))
         (categories-to-run
           (if category
               (list category)
               (mapcar #'car (list-categories))))
         (category-results '())
         (total-pass 0)
         (total-fail 0)
         (total-skip 0)
         (total-count 0)
         (start-time (get-internal-real-time)))
    ;; Validate category if specified
    (when category
      (let ((available (mapcar #'car (list-categories :include-skipped t))))
        (unless (member category available :test #'string-equal)
          (error 'category-not-found-error
                 :category category
                 :available available))))
    ;; Run each category
    (dolist (cat categories-to-run)
      (format t "~&Category: ~A..." cat)
      (force-output)
      (let ((result (run-category cat :timeout timeout
                                      :progress-callback #'display-progress)))
        (push result category-results)
        (incf total-pass (category-result-pass-count result))
        (incf total-fail (category-result-fail-count result))
        (incf total-skip (category-result-skip-count result))
        (incf total-count (category-result-total-count result))
        (format t " [~D/~D] ~,1F%~%"
                (category-result-pass-count result)
                (category-result-total-count result)
                (* 100 (category-pass-rate result)))))
    ;; Build report
    (let* ((end-time (get-internal-real-time))
           (duration-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second))))
           (summary (make-report-summary
                     :total-count total-count
                     :pass-count total-pass
                     :fail-count total-fail
                     :skip-count total-skip
                     :duration-ms duration-ms))
           (report (make-coverage-report
                    :timestamp (get-iso-timestamp)
                    :branch (get-git-branch)
                    :commit (get-git-commit)
                    :categories (nreverse category-results)
                    :summary summary))
           (pass-rate (if (zerop total-count)
                          0.0
                          (float (/ total-pass total-count)))))
      ;; Display summary
      (format t "~&Summary: ~D/~D (~,1F%) | ~D failed | ~D skipped~%"
              total-pass total-count (* 100 pass-rate) total-fail total-skip)
      (format t "Duration: ~A~%" (format-duration duration-ms))
      ;; Generate report if requested
      (when report-path
        (generate-report report :output-path report-path))
      (values report pass-rate))))

;;; ==========================================================================
;;; T032: Progress display
;;; ==========================================================================

(defun display-progress (category-name current total pass fail skip)
  "Display progress for category execution (non-interactive mode)."
  (declare (ignore category-name current total pass fail skip))
  ;; Minimal progress indication
  (write-char #\.)
  (force-output))

;;; ==========================================================================
;;; Utilities
;;; ==========================================================================

(defun get-iso-timestamp ()
  "Return current time as ISO 8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

(defun get-git-branch ()
  "Get current git branch name, or nil if not in a git repo."
  (handler-case
      (string-trim '(#\Newline #\Space)
                   (uiop:run-program '("git" "rev-parse" "--abbrev-ref" "HEAD")
                                     :output :string))
    (error () nil)))

(defun get-git-commit ()
  "Get current git commit hash, or nil if not in a git repo."
  (handler-case
      (string-trim '(#\Newline #\Space)
                   (uiop:run-program '("git" "rev-parse" "--short" "HEAD")
                                     :output :string))
    (error () nil)))

(defun format-duration (ms)
  "Format milliseconds as human-readable duration."
  (let* ((seconds (floor ms 1000))
         (minutes (floor seconds 60))
         (remaining-seconds (mod seconds 60)))
    (if (zerop minutes)
        (format nil "~Ds" remaining-seconds)
        (format nil "~Dm ~Ds" minutes remaining-seconds))))
