;;;; types.lisp - Filesystem type definitions
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Task: T008
;;;;
;;;; Defines the file-stream struct for representing open file handles.

(in-package #:clysm/filesystem)

;;; ============================================================
;;; File Stream Structure (T008)
;;; ============================================================

(defstruct file-stream
  "Represents an open file handle with metadata.
   FR-001: System MUST provide open-file function to obtain a file stream handle.

   Slots:
     HANDLE    - Opaque host handle (externref in Wasm, or mock value in host Lisp)
     DIRECTION - :input for reading, :output for writing
     PATHNAME  - String identifying the file
     OPEN-P    - T if stream is open, NIL if closed"
  (handle nil)                            ; Opaque host handle
  (direction :input :type (member :input :output))
  (pathname "" :type string)
  (open-p t :type boolean))

;;; ============================================================
;;; File Stream Utilities
;;; ============================================================

(defun ensure-open-stream (stream operation)
  "Signal file-error if STREAM is not open.
   FR-011: System MUST reject operations on closed file streams."
  (unless (file-stream-open-p stream)
    (cl:error (make-instance 'clysm/conditions:file-error
                             :pathname (file-stream-pathname stream)))))

(defun ensure-input-stream (stream operation)
  "Signal file-error if STREAM is not an input stream."
  (unless (eq :input (file-stream-direction stream))
    (cl:error (make-instance 'clysm/conditions:file-error
                             :pathname (file-stream-pathname stream)))))

(defun ensure-output-stream (stream operation)
  "Signal file-error if STREAM is not an output stream."
  (unless (eq :output (file-stream-direction stream))
    (cl:error (make-instance 'clysm/conditions:file-error
                             :pathname (file-stream-pathname stream)))))
