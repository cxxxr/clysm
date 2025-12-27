;;;; compiler.lisp - Main compilation workflow
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Implements T025-T028, T032: read-source-modules, compile-module, compile-project

(in-package #:clysm/workflow)

;;; ============================================================
;;; T025: read-source-modules - Read and parse source files
;;; ============================================================

(defun read-source-modules (patterns &optional (base-dir (uiop:getcwd)))
  "Read source modules matching PATTERNS from BASE-DIR.
   Returns a list of source-module structs.

   PATTERNS is a list of glob patterns (e.g., (\"src/**/*.lisp\"))."
  (let ((paths nil)
        (modules nil))

    ;; Expand all patterns
    (dolist (pattern patterns)
      (let ((expanded (glob-expand pattern base-dir)))
        (dolist (path expanded)
          (pushnew path paths :test #'equal))))

    ;; Sort paths for deterministic order
    (setf paths (sort paths #'string<))

    ;; Read each file
    (dolist (path paths)
      (let* ((content (read-file-string path))
             (forms (handler-case
                        (read-forms-from-string content)
                      (error (e)
                        (declare (ignore e))
                        nil)))
             (rel-path (relative-pathname path base-dir))
             (pkg (extract-package forms)))
        (push (make-source-module
               :path path
               :relative-path rel-path
               :package pkg
               :mtime (file-mtime path)
               :hash "" ; TODO: compute content hash
               :forms forms
               :form-count (length forms)
               :dependencies nil
               :status :pending)
              modules)))

    (nreverse modules)))

(defun read-forms-from-string (string)
  "Read all S-expressions from STRING.
   Returns a list of forms."
  (with-input-from-string (stream string)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

;;; ============================================================
;;; T026: compile-module - Compile a single module
;;; ============================================================

(defun compile-module (module &key verbose)
  "Compile MODULE (a source-module struct) to Wasm.
   Returns a compilation-result struct.

   Uses the existing clysm/compiler infrastructure for form compilation."
  (let ((start-time (get-internal-real-time))
        (forms-compiled 0)
        (forms-failed 0)
        (forms-skipped 0)
        (errors nil)
        (warnings nil)
        (all-bytes nil))

    (setf (source-module-status module) :compiling)

    ;; Compile each form
    (dolist (form (source-module-forms module))
      (cond
        ;; Skip metadata forms
        ((metadata-form-p form)
         (incf forms-skipped))

        ;; Compile the form
        (t
         (handler-case
             (let ((result (clysm/compiler:compile-to-wasm form)))
               (if result
                   (progn
                     (incf forms-compiled)
                     (when result
                       (push result all-bytes)))
                   (incf forms-skipped)))
           (error (e)
             (incf forms-failed)
             (push (make-compilation-error
                    :severity :error
                    :message (princ-to-string e)
                    :path (source-module-path module)
                    :line 0
                    :column 0
                    :form form
                    :operator (when (consp form) (car form)))
                   errors))))))

    ;; Calculate elapsed time
    (let* ((end-time (get-internal-real-time))
           (elapsed-ms (round (* 1000 (/ (- end-time start-time)
                                          internal-time-units-per-second))))
           (success (null errors)))

      (setf (source-module-status module)
            (if success :compiled :failed))

      (make-compilation-result
       :module module
       :success-p success
       :wasm-bytes (when all-bytes (combine-wasm-bytes (nreverse all-bytes)))
       :byte-count (if all-bytes
                       (reduce #'+ all-bytes :key #'length)
                       0)
       :form-count (source-module-form-count module)
       :forms-compiled forms-compiled
       :forms-failed forms-failed
       :forms-skipped forms-skipped
       :errors (nreverse errors)
       :warnings (nreverse warnings)
       :compile-time-ms elapsed-ms))))

(defun metadata-form-p (form)
  "Return T if FORM is a metadata/declaration form that should be skipped."
  (and (consp form)
       (member (car form) '(in-package declare eval-when))))

(defun combine-wasm-bytes (byte-vectors)
  "Combine multiple byte vectors into a single Wasm module.
   For now, just returns the last one (simplified)."
  ;; TODO: Proper module combination
  (car (last byte-vectors)))

;;; ============================================================
;;; T027: compile-project - Main compilation entry point
;;; ============================================================

(defun compile-project (patterns output &key
                                         (verbose nil)
                                         (force nil)
                                         (continue-on-error t)
                                         (cache-dir ".clysm-cache")
                                         (progress-callback nil))
  "Compile all modules matching PATTERNS to OUTPUT.
   Returns a compilation-session struct.

   Options:
   - VERBOSE: Show detailed output
   - FORCE: Ignore cache, recompile everything
   - CONTINUE-ON-ERROR: Continue compiling even if some modules fail
   - CACHE-DIR: Directory for compilation cache
   - PROGRESS-CALLBACK: Function called with progress-info updates"
  (let* ((base-dir (uiop:getcwd))
         (start-time (get-internal-real-time))
         (options (make-compilation-options
                   :output output
                   :verbose verbose
                   :force force
                   :continue-on-error continue-on-error
                   :cache-path cache-dir
                   :progress-callback progress-callback))
         (session (make-compilation-session
                   :start-time start-time
                   :project-root (namestring base-dir)
                   :output-path output
                   :input-patterns patterns
                   :options options)))

    ;; Phase 1: Read source modules
    (update-progress session :reading 0 "Reading source files...")
    (let ((modules (read-source-modules patterns base-dir)))

      (setf (compilation-session-modules session) modules)

      (when (null modules)
        ;; No files found
        (update-progress session :done 100 "No files matched patterns")
        (return-from compile-project session))

      ;; Phase 2: Load cache and find dirty modules
      (update-progress session :analyzing 10 "Analyzing dependencies...")
      (let* ((cache (unless force (load-cache cache-dir base-dir)))
             (dirty (if force
                        modules
                        (find-dirty-modules modules cache)))
             (graph (build-dependency-graph modules)))

        (setf (compilation-session-dirty-modules session) dirty)

        ;; Prune deleted files from cache
        (when cache
          (prune-deleted-files cache modules))

        ;; If no dirty modules, we're done
        (when (and (not force) (null dirty))
          (update-progress session :done 100 "No changes detected")
          (return-from compile-project session))

        ;; Add dependents of dirty modules
        (let ((to-compile (compute-modules-to-compile graph dirty)))
          (setf (compilation-session-dirty-modules session) to-compile)

          ;; Phase 3: Compile modules
          (update-progress session :compiling 20 "Compiling modules...")
          (let ((results nil)
                (total (length to-compile))
                (current 0)
                (all-success t)
                (new-cache (or cache (create-empty-cache base-dir))))

            (dolist (mod to-compile)
              (incf current)
              (let ((pct (+ 20 (* 70 (/ current total)))))
                (update-progress session :compiling pct
                                 (format nil "[~D/~D] ~A"
                                         current total
                                         (source-module-relative-path mod))))

              (let ((result (compile-module mod :verbose verbose)))
                (push result results)

                (unless (compilation-result-success-p result)
                  (setf all-success nil)
                  (dolist (err (compilation-result-errors result))
                    (push err (compilation-session-errors session)))

                  (unless continue-on-error
                    (return)))

                ;; Update cache
                (update-cache-for-module new-cache mod result)))

            (setf (compilation-session-results session) (nreverse results))

            ;; Phase 4: Write output
            (when all-success
              (update-progress session :linking 90 "Writing output...")
              (write-compilation-output session))

            ;; Save cache
            (save-cache new-cache cache-dir)

            ;; Done
            (update-progress session :done 100
                             (if all-success
                                 "Compilation complete"
                                 "Compilation completed with errors"))))))

    session))

(defun compute-modules-to-compile (graph dirty-modules)
  "Compute full list of modules to compile given DIRTY-MODULES.
   Includes all dependents of dirty modules."
  (let ((paths-to-compile (make-hash-table :test 'equal)))
    ;; Add all dirty modules
    (dolist (mod dirty-modules)
      (setf (gethash (source-module-relative-path mod) paths-to-compile) mod))

    ;; Add all dependents
    (dolist (mod dirty-modules)
      (dolist (dep-path (get-dependents graph (source-module-relative-path mod)))
        (let ((dep-mod (gethash dep-path (dependency-graph-path-index graph))))
          (when dep-mod
            (setf (gethash dep-path paths-to-compile) dep-mod)))))

    ;; Get in compilation order
    (let ((result nil))
      (dolist (path (dependency-graph-order graph))
        (let ((mod (gethash path paths-to-compile)))
          (when mod (push mod result))))
      (nreverse result))))

;;; ============================================================
;;; T028: progress-display - Progress feedback
;;; ============================================================

(defun update-progress (session phase percentage message)
  "Update progress info for SESSION."
  (let ((progress (make-progress-info
                   :phase phase
                   :total-modules (length (compilation-session-modules session))
                   :current-module (length (compilation-session-results session))
                   :current-path ""
                   :percentage (coerce percentage 'single-float)
                   :message message)))
    (setf (compilation-session-progress session) progress)

    ;; Call callback if provided
    (let ((callback (and (compilation-session-options session)
                         (compilation-options-progress-callback
                          (compilation-session-options session)))))
      (when callback
        (funcall callback progress)))))

(defun progress-display (progress-info &optional (stream *standard-output*))
  "Display PROGRESS-INFO to STREAM."
  (format stream "~A [~,1F%] ~A~%"
          (progress-info-phase progress-info)
          (progress-info-percentage progress-info)
          (progress-info-message progress-info)))

;;; ============================================================
;;; T054: format-error - Format error with location
;;; ============================================================

(defun format-error (error &optional (stream *error-output*))
  "Format ERROR with file:line:column information."
  (let ((path (compilation-error-path error))
        (line (compilation-error-line error))
        (column (compilation-error-column error))
        (message (compilation-error-message error)))
    (cond
      ((and (> line 0) (> column 0))
       (format stream "~A:~D:~D: error: ~A~%" path line column message))
      ((> line 0)
       (format stream "~A:~D: error: ~A~%" path line message))
      (t
       (format stream "~A: error: ~A~%" path message)))))

;;; ============================================================
;;; T056: error-summary - Display error summary
;;; ============================================================

(defun display-error-summary (session &optional (stream *error-output*))
  "Display summary of all errors from SESSION."
  (let ((errors (compilation-session-errors session))
        (results (compilation-session-results session)))
    (when errors
      (format stream "~%=== Compilation Errors ===~%")
      (dolist (err errors)
        (format-error err stream))
      (let ((failed-count (count-if-not #'compilation-result-success-p results))
            (total-count (length results)))
        (format stream "~%~D of ~D modules failed~%"
                failed-count total-count)))))

;;; ============================================================
;;; T059: stderr-output - Output errors to stderr
;;; ============================================================

(defun report-errors-to-stderr (session)
  "Report all errors from SESSION to *error-output*."
  (display-error-summary session *error-output*))

;;; ============================================================
;;; T032: Write Wasm output
;;; ============================================================

(defun write-compilation-output (session)
  "Write compiled Wasm to the output file specified in SESSION."
  (let ((output-path (compilation-session-output-path session)))
    (ensure-directory output-path)

    ;; Combine all successful compilation results
    (let* ((results (compilation-session-results session))
           (successful (remove-if-not #'compilation-result-success-p results))
           (all-bytes (loop for r in successful
                            when (compilation-result-wasm-bytes r)
                            collect (compilation-result-wasm-bytes r))))

      (when all-bytes
        ;; TODO: Proper module linking
        ;; For now, write the first result
        (let ((wasm (car all-bytes)))
          (when wasm
            (with-open-file (stream output-path
                                    :direction :output
                                    :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
              (write-sequence wasm stream))))))))
