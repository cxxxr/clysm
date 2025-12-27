;;;; read.lisp - File reading operations
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T015, T016, T017
;;;;
;;;; Implements read-file-contents function for reading entire file contents.

(in-package #:clysm/filesystem)

;;; ============================================================
;;; read-file-contents Implementation
;;; ============================================================

(defun read-file-contents (pathname-or-stream)
  "Read the entire contents of a file as a UTF-8 string.

   PATHNAME-OR-STREAM: Either a pathname string or an open input file-stream.
   Returns: A string containing the file contents.
   Signals: file-error if file cannot be read.

   FR-003: System MUST provide read-file-contents function.
   FR-010: System MUST properly encode/decode UTF-8.

   Examples:
     (read-file-contents \"config.txt\")
     ;; => \"key=value\\n\"

     (let ((s (open-file \"data.txt\" :direction :input)))
       (unwind-protect
           (read-file-contents s)
         (close-file s)))"
  (etypecase pathname-or-stream
    (string
     ;; T015: Pathname variant - open, read, close
     (read-file-contents-from-pathname pathname-or-stream))
    (file-stream
     ;; T016: Stream variant - read from open stream
     (read-file-contents-from-stream pathname-or-stream))))

;;; ============================================================
;;; T015: Pathname Variant
;;; ============================================================

(defun read-file-contents-from-pathname (pathname)
  "Read entire file contents from pathname string.
   Opens the file, reads contents, closes file.
   Uses host Lisp file operations directly for now."
  (handler-case
      (with-open-file (stream pathname
                              :direction :input
                              :external-format :utf-8
                              :if-does-not-exist :error)
        (let* ((length (file-length stream))
               (content (make-string length)))
          ;; Handle potential length being nil for streams
          (if length
              (progn
                (read-sequence content stream)
                content)
              ;; Fallback: read character by character
              (with-output-to-string (out)
                (loop for char = (read-char stream nil nil)
                      while char
                      do (write-char char out))))))
    ;; Translate host Lisp file errors to file-error
    (file-error (e)
      (cl:error (make-instance 'clysm/conditions:file-error
                               :pathname pathname)))
    (error (e)
      ;; Catch-all for other errors
      (cl:error (make-instance 'clysm/conditions:file-error
                               :pathname pathname)))))

;;; ============================================================
;;; T016: Stream Variant
;;; ============================================================

(defun read-file-contents-from-stream (stream)
  "Read entire file contents from an open file-stream.
   FR-011: System MUST reject operations on closed file streams."
  ;; Validate stream is open and for input
  (ensure-open-stream stream 'read-file-contents)
  (ensure-input-stream stream 'read-file-contents)

  ;; For now, use host Lisp file operations
  ;; In compiled Wasm, this would use FFI %read-all
  (handler-case
      (let ((pathname (file-stream-pathname stream)))
        (with-open-file (host-stream pathname
                                     :direction :input
                                     :external-format :utf-8)
          (let* ((length (file-length host-stream))
                 (content (make-string (or length 0))))
            (if length
                (progn
                  (read-sequence content host-stream)
                  content)
                (with-output-to-string (out)
                  (loop for char = (read-char host-stream nil nil)
                        while char
                        do (write-char char out)))))))
    (error (e)
      (cl:error (make-instance 'clysm/conditions:file-error
                               :pathname (file-stream-pathname stream))))))

;;; ============================================================
;;; T017: UTF-8 Decoding
;;; ============================================================
;;;
;;; UTF-8 decoding is handled by:
;;; - Host Lisp: :external-format :utf-8 in with-open-file
;;; - Compiled Wasm: Host shim returns UTF-8 decoded string
;;;
;;; The babel library could be used for explicit UTF-8 handling if needed,
;;; but the current implementation relies on the host environment.

;;; Note: open-file and close-file are defined in open.lisp
