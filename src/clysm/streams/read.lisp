;;;; read.lisp - Input operations
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/streams)

;;; ============================================================
;;; read-char (FR-004, FR-007, US2)
;;; ============================================================

(defun read-char (&optional (stream *standard-input*)
                  (eof-error-p t) eof-value recursive-p)
  "Read a character from STREAM.
   FR-004: System MUST provide read-char function.
   FR-006: Support optional stream argument (default: *standard-input*).
   FR-007: Handle end-of-file conditions appropriately.
   FR-020: Uses condition system for error signaling."
  (declare (ignore recursive-p))
  ;; Type check stream (signals clysm/conditions:type-error)
  (unless (streamp stream)
    (cl:error 'type-error
              :datum stream
              :expected-type 'stream))
  ;; Validate input capability
  (unless (input-stream-p stream)
    (cl:error 'type-error
              :datum stream
              :expected-type '(satisfies input-stream-p)))
  (let ((codepoint (%host-read-char (stream-fd stream))))
    (if (= codepoint -1)
        ;; EOF reached
        (if eof-error-p
            (cl:error 'end-of-file :stream stream)
            eof-value)
        (code-char codepoint))))

;;; ============================================================
;;; read-line (FR-005, FR-007, US2)
;;; ============================================================

(defun read-line (&optional (stream *standard-input*)
                  (eof-error-p t) eof-value recursive-p)
  "Read a line from STREAM.
   Returns two values: the line (without newline) and missing-newline-p.
   FR-005: System MUST provide read-line function.
   FR-006: Support optional stream argument.
   FR-007: Handle end-of-file conditions.
   FR-020: Uses condition system for error signaling."
  (declare (ignore recursive-p))
  ;; Type check stream (signals clysm/conditions:type-error)
  (unless (streamp stream)
    (cl:error 'type-error
              :datum stream
              :expected-type 'stream))
  ;; Validate input capability
  (unless (input-stream-p stream)
    (cl:error 'type-error
              :datum stream
              :expected-type '(satisfies input-stream-p)))
  (let ((line (%host-read-line (stream-fd stream))))
    (if (null line)
        ;; EOF reached
        (if eof-error-p
            (cl:error 'end-of-file :stream stream)
            (values eof-value t))
        ;; Return line and whether it ended without newline
        ;; The host shim strips newlines, so we check if we're at EOF
        (let ((next-char (%host-read-char (stream-fd stream))))
          (if (= next-char -1)
              (values line t)   ; EOF after line - missing newline
              (progn
                ;; Put character back conceptually -
                ;; for now we assume line was complete
                (values line nil)))))))
