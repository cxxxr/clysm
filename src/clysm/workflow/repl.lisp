;;;; repl.lisp - REPL integration for compile-file
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Implements T063-T071: REPL compile-file function

(in-package #:clysm/workflow)

;;; ============================================================
;;; Condition classes (T063-T065)
;;; ============================================================

(define-condition compile-file-error (error)
  ((pathname :initarg :pathname :reader compile-file-error-pathname)
   (message :initarg :message :reader compile-file-error-message))
  (:report (lambda (condition stream)
             (format stream "Compilation error in ~A: ~A"
                     (compile-file-error-pathname condition)
                     (compile-file-error-message condition)))))

(define-condition compile-file-warning (warning)
  ((pathname :initarg :pathname :reader compile-file-warning-pathname)
   (message :initarg :message :reader compile-file-warning-message))
  (:report (lambda (condition stream)
             (format stream "Compilation warning in ~A: ~A"
                     (compile-file-warning-pathname condition)
                     (compile-file-warning-message condition)))))

(define-condition compile-file-not-found (file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Source file not found: ~A"
                     (file-error-pathname condition)))))

;;; ============================================================
;;; Special variables for compile-file behavior
;;; ============================================================

(defvar *clysm-compile-verbose* nil
  "If true, compile-file* prints progress information.")

(defvar *clysm-compile-print* nil
  "If true, compile-file* prints compiled forms (not currently used).")

;;; ============================================================
;;; T066-T070: compile-file* function
;;; ============================================================

(defun compile-file* (input-file &key
                                  (output-file nil)
                                  (verbose *clysm-compile-verbose*)
                                  (print *clysm-compile-print*)
                                  (force nil))
  "Compile INPUT-FILE to WebAssembly.

   Returns three values:
   1. Primary value: output-truename if successful, NIL otherwise
   2. warnings-p: T if warnings were signaled
   3. failure-p: T if compilation failed

   Keyword arguments:
   - OUTPUT-FILE: Path for output Wasm (default: derived from input)
   - VERBOSE: Print progress information
   - PRINT: Print compiled forms (not used)
   - FORCE: Force recompilation even if cached

   This function follows ANSI CL compile-file semantics where applicable.
   Uses * suffix to avoid shadowing cl:compile-file."
  (declare (ignore print))

  ;; Resolve input path
  (let* ((input-path (if (pathnamep input-file)
                         input-file
                         (pathname input-file)))
         (truename (handler-case
                       (truename input-path)
                     (file-error ()
                       nil))))

    ;; Check file exists
    (unless truename
      (restart-case
          (error 'compile-file-not-found :pathname input-file)
        (use-value (new-path)
          :report "Provide an alternative file path"
          :interactive (lambda ()
                         (format *query-io* "Enter alternative path: ")
                         (list (read-line *query-io*)))
          (return-from compile-file* (compile-file* new-path
                                                    :output-file output-file
                                                    :verbose verbose
                                                    :force force)))))

    ;; Derive output file if not specified
    (let ((output (or output-file
                      (derive-output-filename truename))))

      ;; Read and compile
      (let* ((content (read-file-string truename))
             (forms (handler-case
                        (read-forms-from-string content)
                      (error (e)
                        (error 'compile-file-error
                               :pathname truename
                               :message (format nil "Read error: ~A" e)))))
             (module (make-source-module
                      :path (namestring truename)
                      :relative-path (file-namestring truename)
                      :mtime (file-mtime truename)
                      :forms forms
                      :form-count (length forms)
                      :status :pending))
             (result (compile-module module :verbose verbose))
             (warnings-p nil)
             (failure-p nil))

        ;; Signal warnings
        (dolist (warn (compilation-result-warnings result))
          (setf warnings-p t)
          (warn 'compile-file-warning
                :pathname truename
                :message (compilation-error-message warn)))

        ;; Handle errors
        (if (compilation-result-success-p result)
            (progn
              ;; Write output
              (ensure-directory output)
              (let ((bytes (compilation-result-wasm-bytes result)))
                (when bytes
                  (with-open-file (stream output
                                          :direction :output
                                          :if-exists :supersede
                                          :element-type '(unsigned-byte 8))
                    (write-sequence bytes stream))))

              (when verbose
                (format t "~&; Wrote ~A~%" output))

              (values (truename output) warnings-p nil))

            ;; Compilation failed
            (progn
              (setf failure-p t)
              (dolist (err (compilation-result-errors result))
                (restart-case
                    (error 'compile-file-error
                           :pathname truename
                           :message (compilation-error-message err))
                  (continue ()
                    :report "Continue compilation"
                    nil)
                  (abort ()
                    :report "Abort compilation"
                    (return-from compile-file* (values nil warnings-p t)))))

              (values nil warnings-p t)))))))

;;; ============================================================
;;; T067: Output file derivation
;;; ============================================================

(defun derive-output-filename (input-path)
  "Derive output Wasm filename from INPUT-PATH.
   Replaces .lisp extension with .wasm."
  (let* ((name (pathname-name input-path))
         (dir (pathname-directory input-path)))
    (make-pathname :name name
                   :type "wasm"
                   :directory dir
                   :defaults input-path)))
