;;;; logging.lisp - Logging utilities for Stage 1 compiler generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Provides structured logging for debugging and progress tracking

(in-package #:clysm/stage1)

;;; ==========================================================================
;;; Log Levels
;;; ==========================================================================

(defparameter *log-level* :info
  "Current log level. One of :debug :info :warn :error :none")

(defparameter *log-stream* *standard-output*
  "Stream to write log messages to.")

(defparameter *log-timestamps-p* t
  "If T, include timestamps in log messages.")

(defparameter *log-prefix* "stage1"
  "Prefix for all log messages.")

;;; ==========================================================================
;;; Log Level Ordering
;;; ==========================================================================

(defparameter *log-level-order*
  '((:debug . 0) (:info . 1) (:warn . 2) (:error . 3) (:none . 99))
  "Ordering of log levels from most verbose to least.")

(defun log-level-value (level)
  "Get numeric value for a log level."
  (or (cdr (assoc level *log-level-order*)) 1))

(defun log-enabled-p (level)
  "Return T if logging is enabled for LEVEL."
  (>= (log-level-value level) (log-level-value *log-level*)))

;;; ==========================================================================
;;; Timestamp Formatting
;;; ==========================================================================

(defun format-timestamp ()
  "Format current time as HH:MM:SS."
  (multiple-value-bind (sec min hour) (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

;;; ==========================================================================
;;; Core Logging Function
;;; ==========================================================================

(defun log-message (level format-string &rest args)
  "Log a message at LEVEL with FORMAT-STRING and ARGS."
  (when (log-enabled-p level)
    (format *log-stream* "~@[[~A] ~]~A ~A: ~?~%"
            (when *log-timestamps-p* (format-timestamp))
            *log-prefix*
            (string-upcase (symbol-name level))
            format-string
            args)
    (force-output *log-stream*)))

;;; ==========================================================================
;;; Convenience Macros
;;; ==========================================================================

(defmacro log-debug (format-string &rest args)
  "Log a debug message."
  `(log-message :debug ,format-string ,@args))

(defmacro log-info (format-string &rest args)
  "Log an info message."
  `(log-message :info ,format-string ,@args))

(defmacro log-warn (format-string &rest args)
  "Log a warning message."
  `(log-message :warn ,format-string ,@args))

(defmacro log-error (format-string &rest args)
  "Log an error message."
  `(log-message :error ,format-string ,@args))

;;; ==========================================================================
;;; Progress Logging
;;; ==========================================================================

(defun log-module-start (module-path index total)
  "Log the start of processing a module."
  (log-info "[~D/~D] Processing: ~A" index total module-path))

(defun log-module-complete (module-path compiled failed elapsed-ms)
  "Log the completion of processing a module."
  (log-info "  Completed: ~D compiled, ~D failed (~,2Fms)"
            compiled failed elapsed-ms))

(defun log-form-compiled (form-id)
  "Log successful form compilation."
  (log-debug "  Compiled: ~A" form-id))

(defun log-form-failed (form-id error-type)
  "Log failed form compilation."
  (log-debug "  Failed: ~A (~A)" form-id error-type))

;;; ==========================================================================
;;; Summary Logging
;;; ==========================================================================

(defun log-summary (total compiled failed coverage-pct elapsed-seconds)
  "Log final compilation summary."
  (log-info "~%=== Stage 1 Generation Summary ===")
  (log-info "Total forms:   ~D" total)
  (log-info "Compiled:      ~D" compiled)
  (log-info "Failed:        ~D" failed)
  (log-info "Coverage:      ~,1F%%" coverage-pct)
  (log-info "Elapsed time:  ~,2F seconds" elapsed-seconds))

(defun log-binary-written (path size-bytes)
  "Log binary file output."
  (log-info "Binary written: ~A (~D bytes)" path size-bytes))

(defun log-report-written (path)
  "Log report file output."
  (log-info "Report written: ~A" path))

;;; ==========================================================================
;;; Error Logging
;;; ==========================================================================

(defun log-condition (condition)
  "Log a condition with full details."
  (log-error "~A" condition))

(defun log-blocker (operator count priority)
  "Log a blocker identification."
  (log-warn "Blocker: ~A (count: ~D, priority: ~A)" operator count priority))

