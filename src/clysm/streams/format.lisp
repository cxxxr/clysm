;;;; format.lisp - Format function implementation
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/streams)

;;; ============================================================
;;; Format Directive Structures (T065, T066)
;;; ============================================================

(defstruct format-directive
  "Represents a single format directive.
   T065: FormatDirective struct."
  (type nil :type keyword)
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defstruct format-string-info
  "Parsed format string information.
   T066: FormatStringInfo struct."
  (control-string "" :type string)
  (directives nil :type list))

;;; ============================================================
;;; Format String Parser (T067)
;;; ============================================================

(defun parse-format-string (control-string)
  "Parse CONTROL-STRING into format-string-info.
   T067: Compile-time format string parsing.
   Supports ~A, ~S, ~D, ~%, ~~ directives."
  (let ((directives nil)
        (len (length control-string))
        (i 0))
    (loop while (< i len)
          for char = (char control-string i)
          do (if (char= char #\~)
                 (if (< (1+ i) len)
                     (let ((next-char (char control-string (1+ i))))
                       (push (make-format-directive
                              :type (case next-char
                                      ((#\A #\a) :aesthetic)
                                      ((#\S #\s) :standard)
                                      ((#\D #\d) :decimal)
                                      (#\% :newline)
                                      (#\~ :tilde)
                                      (t (error "Unknown format directive: ~~~C" next-char)))
                              :start i
                              :end (+ i 2))
                             directives)
                       (incf i 2))
                     (error "Format string ends with tilde"))
                 (incf i)))
    (make-format-string-info
     :control-string control-string
     :directives (nreverse directives))))

;;; ============================================================
;;; Format Directive Handlers (T068-T072)
;;; ============================================================

(defun format-aesthetic (object stream)
  "Format OBJECT using aesthetic (human-readable) representation.
   T068: ~A directive handler - princ style."
  (write-string (princ-to-string object) stream))

(defun format-standard (object stream)
  "Format OBJECT using standard (machine-readable) representation.
   T069: ~S directive handler - prin1 style."
  (write-string (prin1-to-string object) stream))

(defun format-decimal (object stream)
  "Format OBJECT as decimal integer.
   T070: ~D directive handler."
  (unless (integerp object)
    (error 'type-error :datum object :expected-type 'integer))
  (write-string (princ-to-string object) stream))

(defun format-newline (stream)
  "Output a newline.
   T071: ~% directive handler."
  (write-char #\Newline stream))

(defun format-tilde (stream)
  "Output a literal tilde.
   T072: ~~ directive handler."
  (write-char #\~ stream))

;;; ============================================================
;;; Princ/Prin1 to String (helpers)
;;; ============================================================

(defun princ-to-string (object)
  "Return aesthetic string representation of OBJECT."
  (typecase object
    (string object)
    (character (string object))
    (t (format nil "~A" object))))

(defun prin1-to-string (object)
  "Return standard string representation of OBJECT."
  (typecase object
    (string (concatenate 'string "\"" object "\""))
    (character (format nil "#\\~C" object))
    (t (format nil "~S" object))))

;;; ============================================================
;;; Format Function (FR-008 to FR-013, T073, T074, US3)
;;; ============================================================

(defun format (destination control-string &rest args)
  "Format output according to CONTROL-STRING with ARGS.
   FR-008: System MUST provide format function supporting destination argument.
   FR-009: ~A directive (aesthetic printing).
   FR-010: ~S directive (standard printing).
   FR-011: ~D directive (decimal printing).
   FR-012: ~% directive (newline).
   FR-013: ~~ directive (tilde).

   DESTINATION:
   - T: output to *standard-output*, return NIL
   - NIL: return formatted string
   - stream: output to stream, return NIL"
  (let* ((info (parse-format-string control-string))
         (output-stream (cond
                          ((eq destination t) *standard-output*)
                          ((null destination) nil)
                          ((streamp destination) destination)
                          (t (error 'type-error
                                   :datum destination
                                   :expected-type '(or null (eql t) stream)))))
         (result-string (when (null destination)
                          (make-array 0 :element-type 'character
                                       :adjustable t :fill-pointer 0)))
         (arg-index 0)
         (control (format-string-info-control-string info))
         (directives (format-string-info-directives info))
         (last-pos 0))

    (flet ((output (str)
             (if result-string
                 (loop for c across str do (vector-push-extend c result-string))
                 (write-string str output-stream)))
           (output-char (c)
             (if result-string
                 (vector-push-extend c result-string)
                 (write-char c output-stream))))

      ;; Process each directive
      (dolist (directive directives)
        ;; Output literal text before directive
        (let ((start last-pos)
              (end (format-directive-start directive)))
          (when (< start end)
            (output (subseq control start end))))

        ;; Process directive
        (ecase (format-directive-type directive)
          (:aesthetic
           (when (>= arg-index (length args))
             (error "Not enough arguments for format"))
           (output (princ-to-string (nth arg-index args)))
           (incf arg-index))
          (:standard
           (when (>= arg-index (length args))
             (error "Not enough arguments for format"))
           (output (prin1-to-string (nth arg-index args)))
           (incf arg-index))
          (:decimal
           (when (>= arg-index (length args))
             (error "Not enough arguments for format"))
           (let ((arg (nth arg-index args)))
             (unless (integerp arg)
               (error 'type-error :datum arg :expected-type 'integer))
             (output (princ-to-string arg)))
           (incf arg-index))
          (:newline
           (output-char #\Newline))
          (:tilde
           (output-char #\~)))

        (setf last-pos (format-directive-end directive)))

      ;; Output remaining literal text
      (when (< last-pos (length control))
        (output (subseq control last-pos))))

    ;; Return value based on destination
    (if result-string
        (coerce result-string 'string)
        nil)))
