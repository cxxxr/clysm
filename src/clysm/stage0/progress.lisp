;;;; progress.lisp - Progress reporting for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Reports compilation progress via FFI

(in-package #:clysm/stage0)

;;; ============================================================
;;; Progress Callback
;;; ============================================================

(defvar *progress-callback* nil
  "Function to call with progress updates.
   Signature: (event-type &rest args)")

(defvar *progress-log* nil
  "List of progress events for debugging")

;;; ============================================================
;;; Progress Reporting
;;; ============================================================

(defun report-progress (event-type &rest args)
  "Report progress event.
   event-type: keyword like :module-loaded, :compile-error
   args: property list of event data"
  (let ((event (list* event-type args)))
    ;; Log locally
    (push event *progress-log*)
    ;; Call callback if set
    (when *progress-callback*
      (apply *progress-callback* event-type args))
    ;; Print to standard output (for debugging)
    (format t "~&[PROGRESS] ~A~{ ~A~}~%"
            event-type args)))

;;; ============================================================
;;; Progress Event Types
;;; ============================================================

;; Module loading events
;; :module-loaded :path <path> :forms <count>
;; :all-modules-loaded :count <count> :total-forms <count>
;; :module-load-error :path <path> :error <message>

;; Compilation events
;; :form-compiled :form <form> :index <index>
;; :module-compiled :path <path> :succeeded <count> :failed <count>
;; :compile-error :form <form> :error <message>
;; :unsupported-form :form <operator> :error <message>

;; Output events
;; :output-started :path <path>
;; :output-complete :path <path> :size <bytes>
;; :output-error :path <path> :error <message>

;; Verification events
;; :validation-started :path <path>
;; :validation-passed :path <path>
;; :validation-failed :path <path> :error <message>

;;; ============================================================
;;; Progress Summary
;;; ============================================================

(defstruct progress-summary
  "Summary of compilation progress"
  (modules-loaded 0 :type integer)
  (forms-total 0 :type integer)
  (forms-compiled 0 :type integer)
  (forms-failed 0 :type integer)
  (elapsed-time 0.0 :type single-float))

(defun compute-progress-summary ()
  "Compute summary from progress log"
  (let ((summary (make-progress-summary)))
    (dolist (event (reverse *progress-log*))
      (case (first event)
        (:module-loaded
         (incf (progress-summary-modules-loaded summary)))
        (:form-compiled
         (incf (progress-summary-forms-compiled summary)))
        (:compile-error
         (incf (progress-summary-forms-failed summary)))))
    summary))

(defun clear-progress-log ()
  "Clear progress log"
  (setf *progress-log* nil))
