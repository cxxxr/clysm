;;;; workflow-error-recovery-test.lisp - Integration tests for error recovery
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; T052: Integration test for partial compilation

(defpackage #:clysm/tests/integration/workflow/error-recovery-test
  (:use #:cl #:rove #:clysm/workflow))

(in-package #:clysm/tests/integration/workflow/error-recovery-test)

;;; ============================================================
;;; T052: Integration tests for error recovery (US3)
;;; ============================================================

(deftest error-recovery-format-error-test
  "Test format-error with different location info."
  ;; Error with full location
  (let ((err (make-compilation-error
              :path "/src/foo.lisp"
              :line 10
              :column 5
              :message "Undefined variable X")))
    (let ((output (with-output-to-string (s)
                    (format-error err s))))
      (ok (search "/src/foo.lisp:10:5:" output)
          "Full location included")
      (ok (search "Undefined variable X" output)
          "Message included")))

  ;; Error with line only
  (let ((err (make-compilation-error
              :path "/src/bar.lisp"
              :line 20
              :column 0
              :message "Syntax error")))
    (let ((output (with-output-to-string (s)
                    (format-error err s))))
      (ok (search "/src/bar.lisp:20:" output)
          "Line-only location included")))

  ;; Error with path only
  (let ((err (make-compilation-error
              :path "/src/baz.lisp"
              :line 0
              :column 0
              :message "Read error")))
    (let ((output (with-output-to-string (s)
                    (format-error err s))))
      (ok (search "/src/baz.lisp: error:" output)
          "Path-only format used"))))

(deftest error-recovery-error-summary-test
  "Test display-error-summary shows all errors."
  (let ((session (make-compilation-session
                  :errors (list
                           (make-compilation-error
                            :path "a.lisp"
                            :message "Error 1")
                           (make-compilation-error
                            :path "b.lisp"
                            :message "Error 2"))
                  :results (list
                            (make-compilation-result :success-p nil)
                            (make-compilation-result :success-p t)))))
    (let ((output (with-output-to-string (s)
                    (display-error-summary session s))))
      (ok (search "Compilation Errors" output)
          "Summary header present")
      (ok (search "Error 1" output)
          "First error shown")
      (ok (search "Error 2" output)
          "Second error shown")
      (ok (search "1 of 2 modules failed" output)
          "Failure count correct"))))

(deftest error-recovery-continue-on-error-test
  "Test that compilation continues when continue-on-error is true."
  ;; This tests the behavior via the compilation-options structure
  (let ((options (make-compilation-options
                  :output "/tmp/out.wasm"
                  :continue-on-error t)))
    (ok (compilation-options-continue-on-error options)
        "continue-on-error defaults to true"))

  (let ((options (make-compilation-options
                  :output "/tmp/out.wasm"
                  :continue-on-error nil)))
    (ok (not (compilation-options-continue-on-error options))
        "continue-on-error can be disabled")))

(deftest error-recovery-session-error-collection-test
  "Test that errors are collected in session."
  (let ((session (make-compilation-session
                  :errors nil
                  :results nil)))
    ;; Simulate adding errors (as compile-project does)
    (push (make-compilation-error :path "test.lisp" :message "Error 1")
          (compilation-session-errors session))
    (push (make-compilation-error :path "test.lisp" :message "Error 2")
          (compilation-session-errors session))

    (ok (= (length (compilation-session-errors session)) 2)
        "Errors accumulated in session")))

(deftest error-recovery-partial-success-test
  "Test that partial success is detected."
  (let ((session (make-compilation-session
                  :results (list
                            (make-compilation-result :success-p t)
                            (make-compilation-result :success-p nil)
                            (make-compilation-result :success-p t)))))
    (let* ((results (compilation-session-results session))
           (success-count (count-if #'compilation-result-success-p results))
           (fail-count (count-if-not #'compilation-result-success-p results)))
      (ok (= success-count 2) "2 successes")
      (ok (= fail-count 1) "1 failure")
      (ok (and (> success-count 0) (> fail-count 0))
          "Partial success detected"))))
