;;;; write.lisp - File writing operations
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T023, T024, T025
;;;;
;;;; Implements write-file-contents function for writing entire file contents.

(in-package #:clysm/filesystem)

;;; ============================================================
;;; write-file-contents Implementation
;;; ============================================================

(defun write-file-contents (pathname-or-stream contents)
  "Write a string to a file, replacing any existing contents.

   PATHNAME-OR-STREAM: Either a pathname string or an open output file-stream.
   CONTENTS: String to write to the file.
   Returns: NIL.
   Signals: file-error if file cannot be written.

   FR-004: System MUST provide write-file-contents function.
   FR-010: System MUST properly encode/decode UTF-8.

   Examples:
     (write-file-contents \"output.txt\" \"Hello, World!\")

     (let ((s (open-file \"data.txt\" :direction :output)))
       (unwind-protect
           (write-file-contents s \"line 1\\nline 2\\n\")
         (close-file s)))"
  (etypecase pathname-or-stream
    (string
     ;; T023: Pathname variant - open, write, close
     (write-file-contents-to-pathname pathname-or-stream contents))
    (file-stream
     ;; T024: Stream variant - write to open stream
     (write-file-contents-to-stream pathname-or-stream contents)))
  nil)

;;; ============================================================
;;; T023: Pathname Variant
;;; ============================================================

(defun write-file-contents-to-pathname (pathname contents)
  "Write entire file contents to pathname string.
   Creates or overwrites the file.
   Uses host Lisp file operations directly for now."
  (handler-case
      (with-open-file (stream pathname
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
        (write-string contents stream))
    ;; Translate host Lisp file errors to file-error
    (file-error (e)
      (cl:error (make-instance 'clysm/conditions:file-error
                               :pathname pathname)))
    (error (e)
      ;; Catch-all for other errors (e.g., directory not found)
      (cl:error (make-instance 'clysm/conditions:file-error
                               :pathname pathname)))))

;;; ============================================================
;;; T024: Stream Variant
;;; ============================================================

(defun write-file-contents-to-stream (stream contents)
  "Write entire file contents to an open file-stream.
   FR-011: System MUST reject operations on closed file streams."
  ;; Validate stream is open and for output
  (ensure-open-stream stream 'write-file-contents)
  (ensure-output-stream stream 'write-file-contents)

  ;; For now, use host Lisp file operations
  ;; In compiled Wasm, this would use FFI %write-all
  (handler-case
      (let ((pathname (file-stream-pathname stream)))
        (with-open-file (host-stream pathname
                                     :direction :output
                                     :if-exists :supersede
                                     :external-format :utf-8)
          (write-string contents host-stream)))
    (error (e)
      (cl:error (make-instance 'clysm/conditions:file-error
                               :pathname (file-stream-pathname stream))))))

;;; ============================================================
;;; T025: UTF-8 Encoding
;;; ============================================================
;;;
;;; UTF-8 encoding is handled by:
;;; - Host Lisp: :external-format :utf-8 in with-open-file
;;; - Compiled Wasm: Host shim accepts UTF-8 string
;;;
;;; The babel library could be used for explicit UTF-8 handling if needed,
;;; but the current implementation relies on the host environment.
