;;;; open.lisp - File open and close operations
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T033, T034, T035
;;;;
;;;; Implements open-file and close-file functions for explicit file handle management.

(in-package #:clysm/filesystem)

;;; ============================================================
;;; T033, T034: open-file Implementation
;;; ============================================================

(defun open-file (pathname &key (direction :input)
                                (if-exists :supersede)
                                (if-does-not-exist nil))
  "Open a file and return a file-stream object.

   PATHNAME: String naming the file to open.
   DIRECTION: :input for reading, :output for writing.
   IF-EXISTS: For output, :supersede (default) or :error.
   IF-DOES-NOT-EXIST: :error (input default), :create (output default).

   Returns: A file-stream object.
   Signals: file-error if file cannot be opened.

   FR-001: System MUST provide open-file function to obtain a file stream handle.
   FR-007: System MUST support :direction parameter with values :input and :output.
   FR-012: System MUST support :if-exists parameter.
   FR-013: System MUST support :if-does-not-exist parameter.

   Examples:
     (open-file \"data.txt\" :direction :input)
     (open-file \"output.txt\" :direction :output :if-exists :error)"
  ;; Default if-does-not-exist based on direction
  (let ((actual-if-dne (or if-does-not-exist
                           (if (eq direction :input) :error :create))))

    ;; Validate direction
    (unless (member direction '(:input :output))
      (cl:error (make-instance 'clysm/conditions:file-error
                               :pathname pathname)))

    (handler-case
        (cond
          ;; Input direction
          ((eq direction :input)
           (open-file-for-input pathname actual-if-dne))
          ;; Output direction
          ((eq direction :output)
           (open-file-for-output pathname if-exists actual-if-dne)))
      (error (e)
        (cl:error (make-instance 'clysm/conditions:file-error
                                 :pathname pathname))))))

(defun open-file-for-input (pathname if-does-not-exist)
  "Open file for input (reading).
   FR-013: Handle :if-does-not-exist parameter."
  (let ((exists-p (probe-file pathname)))
    (cond
      ;; File exists - open it
      (exists-p
       (make-file-stream :handle nil
                         :direction :input
                         :pathname pathname
                         :open-p t))
      ;; File doesn't exist and :error
      ((eq if-does-not-exist :error)
       (cl:error (make-instance 'clysm/conditions:file-error
                                :pathname pathname)))
      ;; File doesn't exist and :create - create empty file then open
      ((eq if-does-not-exist :create)
       (with-open-file (out pathname
                            :direction :output
                            :if-exists :error
                            :if-does-not-exist :create)
         ;; Just create empty file
         )
       (make-file-stream :handle nil
                         :direction :input
                         :pathname pathname
                         :open-p t))
      ;; Default: error
      (t
       (cl:error (make-instance 'clysm/conditions:file-error
                                :pathname pathname))))))

(defun open-file-for-output (pathname if-exists if-does-not-exist)
  "Open file for output (writing).
   FR-012: Handle :if-exists parameter.
   FR-013: Handle :if-does-not-exist parameter."
  (let ((exists-p (probe-file pathname)))
    (cond
      ;; File exists
      (exists-p
       (cond
         ;; :if-exists :error - signal error
         ((eq if-exists :error)
          (cl:error (make-instance 'clysm/conditions:file-error
                                   :pathname pathname)))
         ;; :if-exists :supersede - create stream (will overwrite)
         ((eq if-exists :supersede)
          ;; Create/truncate the file
          (with-open-file (out pathname
                               :direction :output
                               :if-exists :supersede)
            ;; Just create/truncate
            )
          (make-file-stream :handle nil
                            :direction :output
                            :pathname pathname
                            :open-p t))
         ;; Unknown if-exists value
         (t
          (cl:error (make-instance 'clysm/conditions:file-error
                                   :pathname pathname)))))
      ;; File doesn't exist
      (t
       (cond
         ;; :if-does-not-exist :error - signal error
         ((eq if-does-not-exist :error)
          (cl:error (make-instance 'clysm/conditions:file-error
                                   :pathname pathname)))
         ;; :if-does-not-exist :create - create file
         ((eq if-does-not-exist :create)
          ;; Create empty file
          (with-open-file (out pathname
                               :direction :output
                               :if-does-not-exist :create)
            ;; Just create
            )
          (make-file-stream :handle nil
                            :direction :output
                            :pathname pathname
                            :open-p t))
         ;; Default for output is :create
         (t
          (with-open-file (out pathname
                               :direction :output
                               :if-does-not-exist :create)
            ;; Just create
            )
          (make-file-stream :handle nil
                            :direction :output
                            :pathname pathname
                            :open-p t)))))))

;;; ============================================================
;;; T035: close-file Implementation
;;; ============================================================

(defun close-file (stream)
  "Close a file stream and release the underlying handle.

   STREAM: A file-stream object returned by open-file.
   Returns: NIL.
   Signals: file-error if stream is already closed.

   FR-002: System MUST provide close-file function to release a file stream handle.
   FR-011: System MUST reject operations on closed file streams.

   Example:
     (let ((s (open-file \"data.txt\")))
       (close-file s))"
  (unless (file-stream-p stream)
    (cl:error (make-instance 'clysm/conditions:file-error
                             :pathname "")))

  (unless (file-stream-open-p stream)
    (cl:error (make-instance 'clysm/conditions:file-error
                             :pathname (file-stream-pathname stream))))

  ;; Mark stream as closed
  (setf (file-stream-open-p stream) nil)

  ;; In Wasm, this would call FFI %close-file with the handle
  ;; For now, we just mark the stream as closed

  nil)
