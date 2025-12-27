;;;; macros.lisp - Filesystem macros
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T042, T043
;;;;
;;;; Implements with-open-file* macro for safe file handling.

(in-package #:clysm/filesystem)

;;; ============================================================
;;; T042, T043: with-open-file* Implementation
;;; ============================================================

(defmacro with-open-file* ((stream-var pathname &rest options) &body body)
  "Open a file, bind it to STREAM-VAR, execute BODY, then close the file.

   Uses unwind-protect to ensure the file is closed even if an error occurs.

   STREAM-VAR: Symbol to bind the file-stream to.
   PATHNAME: String naming the file to open.
   OPTIONS: Keyword arguments passed to open-file.
            :direction :input|:output
            :if-exists :supersede|:error
            :if-does-not-exist :error|:create

   FR-005: System MUST provide with-open-file macro using unwind-protect.

   Examples:
     ;; Read file contents
     (with-open-file* (s \"data.txt\" :direction :input)
       (read-file-contents s))

     ;; Write to file
     (with-open-file* (s \"output.txt\" :direction :output)
       (write-file-contents s \"Hello!\"))

     ;; Error handling - file is still closed
     (with-open-file* (s \"data.txt\" :direction :input)
       (error \"Something went wrong\")
       ;; File is closed before error propagates
       )"
  ;; Parse keyword options
  (let ((stream-sym (gensym "STREAM"))
        (result-sym (gensym "RESULT")))
    `(let ((,stream-sym nil))
       (unwind-protect
            (progn
              ;; Open the file
              (setf ,stream-sym (open-file ,pathname ,@options))
              ;; Bind stream and execute body
              (let ((,stream-var ,stream-sym))
                ,@body))
         ;; Cleanup: close stream if it was opened
         (when (and ,stream-sym (file-stream-open-p ,stream-sym))
           (close-file ,stream-sym))))))

;;; ============================================================
;;; Note on naming: with-open-file*
;;; ============================================================
;;;
;;; The macro is named with-open-file* (with asterisk) to avoid
;;; conflict with CL:with-open-file. This follows the Clysm
;;; convention for re-implemented CL forms.
