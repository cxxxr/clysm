;;;; write.lisp - Output operations
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/streams)

;;; ============================================================
;;; write-char (FR-001, US1)
;;; ============================================================

(defun write-char (character &optional (stream *standard-output*))
  "Write CHARACTER to STREAM.
   FR-001: System MUST provide write-char function.
   FR-003: Support optional stream argument (default: *standard-output*).
   FR-020: Uses condition system for error signaling."
  ;; Type check character (signals clysm/conditions:type-error)
  (unless (characterp character)
    (cl:error 'type-error
              :datum character
              :expected-type 'character))
  ;; Type check stream
  (unless (streamp stream)
    (cl:error 'type-error
              :datum stream
              :expected-type 'stream))
  ;; Validate output capability
  (unless (output-stream-p stream)
    (cl:error 'type-error
              :datum stream
              :expected-type '(satisfies output-stream-p)))
  (%host-write-char (stream-fd stream) (char-code character))
  character)

;;; ============================================================
;;; write-string (FR-002, US1)
;;; ============================================================

(defun write-string (string &optional (stream *standard-output*)
                     &key (start 0) end)
  "Write STRING to STREAM, optionally using substring from START to END.
   FR-002: System MUST provide write-string function.
   FR-003: Support optional stream argument.
   FR-020: Uses condition system for error signaling."
  ;; Type check string (signals clysm/conditions:type-error)
  (unless (stringp string)
    (cl:error 'type-error
              :datum string
              :expected-type 'string))
  ;; Type check stream
  (unless (streamp stream)
    (cl:error 'type-error
              :datum stream
              :expected-type 'stream))
  ;; Validate output capability
  (unless (output-stream-p stream)
    (cl:error 'type-error
              :datum stream
              :expected-type '(satisfies output-stream-p)))
  (let* ((end (or end (length string)))
         (substring (if (and (zerop start) (= end (length string)))
                        string
                        (subseq string start end))))
    (%host-write-string (stream-fd stream) substring))
  string)
