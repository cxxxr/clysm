;;;; types.lisp - Stream type definitions
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/streams)

;;; ============================================================
;;; Direction Constants (T015)
;;; ============================================================

(defconstant +direction-input+ 0
  "Stream direction for input-only streams (stdin)")

(defconstant +direction-output+ 1
  "Stream direction for output-only streams (stdout, stderr)")

(defconstant +direction-io+ 2
  "Stream direction for bidirectional streams")

;;; ============================================================
;;; Stream Structure (T016)
;;; ============================================================

(defstruct (stream (:constructor %make-stream)
                   (:copier nil)
                   (:predicate streamp))
  "First-class stream object for I/O operations.
   Maps to WasmGC $stream struct at runtime.
   FR-014: System MUST represent streams as WasmGC struct types."
  (fd 0 :type fixnum :read-only t)
  (direction +direction-input+ :type fixnum :read-only t))

;;; ============================================================
;;; Stream Constructor (T016)
;;; ============================================================

(defun make-stream (fd direction)
  "Create a new stream object.
   FD is the host file descriptor (0=stdin, 1=stdout, 2=stderr).
   DIRECTION is one of +direction-input+, +direction-output+, +direction-io+."
  (check-type fd fixnum)
  (check-type direction fixnum)
  (%make-stream :fd fd :direction direction))

;;; ============================================================
;;; Stream Accessors (T017, T018)
;;; ============================================================
;;; Note: stream-fd and stream-direction are automatically generated
;;; by defstruct above. They provide T017 and T018 functionality.

;;; ============================================================
;;; Stream Predicates (US4: T082-T084)
;;; ============================================================

(defun streamp (object)
  "Return T if OBJECT is a stream, NIL otherwise.
   US4: Streams as first-class values."
  (typep object 'stream))

(defun input-stream-p (stream)
  "Return T if STREAM can be used for input.
   US4: Stream predicates."
  (check-type stream stream)
  (let ((dir (stream-direction stream)))
    (or (= dir +direction-input+)
        (= dir +direction-io+))))

(defun output-stream-p (stream)
  "Return T if STREAM can be used for output.
   US4: Stream predicates."
  (check-type stream stream)
  (let ((dir (stream-direction stream)))
    (or (= dir +direction-output+)
        (= dir +direction-io+))))

;;; ============================================================
;;; Standard Streams (FR-015, FR-016, FR-017, T024-T028)
;;; ============================================================

(defvar *standard-input* nil
  "The default input stream.
   FR-015: System MUST provide *standard-input* bound to host stdin.
   FR-018: Stream special variables MUST be dynamically rebindable.")

(defvar *standard-output* nil
  "The default output stream.
   FR-016: System MUST provide *standard-output* bound to host stdout.
   FR-018: Stream special variables MUST be dynamically rebindable.")

(defvar *error-output* nil
  "The default error output stream.
   FR-017: System MUST provide *error-output* bound to host stderr.
   FR-018: Stream special variables MUST be dynamically rebindable.")

(defun initialize-standard-streams ()
  "Initialize standard stream variables to host file descriptors.
   T027: Initialize streams at module start.
   Called from module start function (T028)."
  (setf *standard-input* (make-stream 0 +direction-input+))
  (setf *standard-output* (make-stream 1 +direction-output+))
  (setf *error-output* (make-stream 2 +direction-output+))
  (values))

;; Initialize standard streams when module loads (T028)
(initialize-standard-streams)
