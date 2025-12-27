;;;; utf8.lisp - Portable UTF-8 encoding/decoding
;;;;
;;;; This module provides pure Common Lisp UTF-8 encoding and decoding
;;;; functions, replacing the babel dependency for cross-implementation
;;;; portability (SBCL, CCL, ECL).

(in-package #:cl-user)

(defpackage #:clysm/lib/utf8
  (:use #:cl)
  (:export ;; Encoding
           #:string-to-utf8-octets
           ;; Decoding
           #:utf8-octets-to-string
           ;; Condition
           #:decoding-error
           #:decoding-error-position
           #:decoding-error-invalid-bytes))

(in-package #:clysm/lib/utf8)

;;; ============================================================
;;; Condition: decoding-error
;;; ============================================================

(define-condition decoding-error (error)
  ((position :initarg :position
             :reader decoding-error-position
             :type (integer 0 *)
             :documentation "Byte offset where error was detected")
   (invalid-bytes :initarg :invalid-bytes
                  :reader decoding-error-invalid-bytes
                  :type (simple-array (unsigned-byte 8) (*))
                  :documentation "The problematic byte sequence"))
  (:report (lambda (condition stream)
             (format stream "Invalid UTF-8 sequence at position ~D: ~S"
                     (decoding-error-position condition)
                     (decoding-error-invalid-bytes condition))))
  (:documentation "Signaled when utf8-octets-to-string encounters invalid UTF-8."))

;;; ============================================================
;;; Encoding: string-to-utf8-octets
;;; ============================================================

(declaim (inline calculate-utf8-byte-count))
(defun calculate-utf8-byte-count (code-point)
  "Return the number of bytes needed to encode CODE-POINT in UTF-8."
  (declare (type (integer 0 #x10FFFF) code-point))
  (cond ((< code-point #x80) 1)
        ((< code-point #x800) 2)
        ((< code-point #x10000) 3)
        (t 4)))

(defun calculate-utf8-length (string)
  "Calculate total bytes needed to encode STRING in UTF-8."
  (declare (type string string))
  (let ((total 0))
    (declare (type fixnum total))
    (loop for char across string
          for code = (char-code char)
          do (incf total (calculate-utf8-byte-count code)))
    total))

(declaim (inline encode-char-to-utf8))
(defun encode-char-to-utf8 (code-point result index)
  "Encode CODE-POINT as UTF-8 bytes into RESULT starting at INDEX.
   Returns the new index after encoding."
  (declare (type (integer 0 #x10FFFF) code-point)
           (type (simple-array (unsigned-byte 8) (*)) result)
           (type fixnum index))
  (cond
    ;; 1-byte: 0xxxxxxx
    ((< code-point #x80)
     (setf (aref result index) code-point)
     (1+ index))
    ;; 2-byte: 110xxxxx 10xxxxxx
    ((< code-point #x800)
     (setf (aref result index)
           (logior #xC0 (ash code-point -6)))
     (setf (aref result (1+ index))
           (logior #x80 (logand code-point #x3F)))
     (+ index 2))
    ;; 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
    ((< code-point #x10000)
     (setf (aref result index)
           (logior #xE0 (ash code-point -12)))
     (setf (aref result (1+ index))
           (logior #x80 (logand (ash code-point -6) #x3F)))
     (setf (aref result (+ index 2))
           (logior #x80 (logand code-point #x3F)))
     (+ index 3))
    ;; 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    (t
     (setf (aref result index)
           (logior #xF0 (ash code-point -18)))
     (setf (aref result (1+ index))
           (logior #x80 (logand (ash code-point -12) #x3F)))
     (setf (aref result (+ index 2))
           (logior #x80 (logand (ash code-point -6) #x3F)))
     (setf (aref result (+ index 3))
           (logior #x80 (logand code-point #x3F)))
     (+ index 4))))

(defun string-to-utf8-octets (string)
  "Convert STRING to a UTF-8 encoded byte vector.
   Returns (simple-array (unsigned-byte 8) (*))."
  (declare (type string string))
  (let* ((length (calculate-utf8-length string))
         (result (make-array length :element-type '(unsigned-byte 8)))
         (index 0))
    (declare (type fixnum index))
    (loop for char across string
          for code = (char-code char)
          do (setf index (encode-char-to-utf8 code result index)))
    result))

;;; ============================================================
;;; Decoding: utf8-octets-to-string
;;; ============================================================

(declaim (inline continuation-byte-p))
(defun continuation-byte-p (byte)
  "Return T if BYTE is a UTF-8 continuation byte (10xxxxxx)."
  (declare (type (unsigned-byte 8) byte))
  (= (logand byte #xC0) #x80))

(defun decode-utf8-lead-byte (byte position octets)
  "Determine sequence length from lead byte and validate.
   Returns (values sequence-length initial-code-point) or signals decoding-error."
  (declare (type (unsigned-byte 8) byte)
           (type fixnum position))
  (cond
    ;; 1-byte: 0xxxxxxx (ASCII)
    ((< byte #x80)
     (values 1 byte))
    ;; Continuation byte at start - invalid
    ((< byte #xC0)
     (error 'decoding-error
            :position position
            :invalid-bytes (make-array 1 :element-type '(unsigned-byte 8)
                                        :initial-contents (list byte))))
    ;; Overlong lead bytes 0xC0, 0xC1 - invalid
    ((< byte #xC2)
     (error 'decoding-error
            :position position
            :invalid-bytes (make-array 1 :element-type '(unsigned-byte 8)
                                        :initial-contents (list byte))))
    ;; 2-byte: 110xxxxx
    ((< byte #xE0)
     (values 2 (logand byte #x1F)))
    ;; 3-byte: 1110xxxx
    ((< byte #xF0)
     (values 3 (logand byte #x0F)))
    ;; 4-byte: 11110xxx
    ((< byte #xF5)
     (values 4 (logand byte #x07)))
    ;; Invalid lead bytes 0xF5-0xFF
    (t
     (error 'decoding-error
            :position position
            :invalid-bytes (make-array 1 :element-type '(unsigned-byte 8)
                                        :initial-contents (list byte))))))

(defun decode-utf8-sequence (octets position length initial-code-point)
  "Decode a UTF-8 sequence of LENGTH bytes starting at POSITION.
   Returns the decoded code point or signals decoding-error."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (type fixnum position length initial-code-point))
  (let ((code-point initial-code-point)
        (end-position (+ position length)))
    ;; Check if we have enough bytes
    (when (> end-position (length octets))
      (let ((available (- (length octets) position)))
        (error 'decoding-error
               :position position
               :invalid-bytes (subseq octets position (min (+ position 4) (length octets))))))
    ;; Process continuation bytes
    (loop for i from (1+ position) below end-position
          for byte = (aref octets i)
          do (unless (continuation-byte-p byte)
               ;; Missing continuation byte
               (error 'decoding-error
                      :position position
                      :invalid-bytes (subseq octets position i)))
             (setf code-point (logior (ash code-point 6)
                                      (logand byte #x3F))))
    ;; Validate: check for overlong encoding
    (let ((min-code-point (case length
                            (2 #x80)
                            (3 #x800)
                            (4 #x10000)
                            (t 0))))
      (when (< code-point min-code-point)
        (error 'decoding-error
               :position position
               :invalid-bytes (subseq octets position end-position))))
    ;; Validate: check for surrogate code points (U+D800-U+DFFF)
    (when (and (<= #xD800 code-point) (<= code-point #xDFFF))
      (error 'decoding-error
             :position position
             :invalid-bytes (subseq octets position end-position)))
    ;; Validate: check for code points above U+10FFFF
    (when (> code-point #x10FFFF)
      (error 'decoding-error
             :position position
             :invalid-bytes (subseq octets position end-position)))
    code-point))

(defun utf8-octets-to-string (octets)
  "Convert UTF-8 encoded OCTETS to a string.
   Signals DECODING-ERROR if OCTETS contains invalid UTF-8.
   OCTETS may be any vector; it will be coerced to (unsigned-byte 8) if needed."
  ;; Accept any vector type, then coerce to proper array type
  (let ((octets (if (typep octets '(simple-array (unsigned-byte 8) (*)))
                    octets
                    (coerce octets '(simple-array (unsigned-byte 8) (*))))))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets))
    (let ((len (length octets)))
      (when (zerop len)
        (return-from utf8-octets-to-string ""))
      ;; First pass: count characters
      (let ((char-count 0)
            (pos 0))
        (declare (type fixnum char-count pos))
        (loop while (< pos len)
              do (multiple-value-bind (seq-len initial)
                     (decode-utf8-lead-byte (aref octets pos) pos octets)
                   (declare (ignore initial))
                   ;; Just validate the sequence
                   (decode-utf8-sequence octets pos seq-len
                                         (if (= seq-len 1)
                                             (aref octets pos)
                                             (logand (aref octets pos)
                                                     (case seq-len
                                                       (2 #x1F)
                                                       (3 #x0F)
                                                       (4 #x07)))))
                   (incf char-count)
                   (incf pos seq-len)))
        ;; Second pass: build string
        (let ((result (make-string char-count))
              (pos 0)
              (char-index 0))
          (declare (type fixnum pos char-index))
          (loop while (< pos len)
                do (multiple-value-bind (seq-len initial)
                       (decode-utf8-lead-byte (aref octets pos) pos octets)
                     (let ((code-point (decode-utf8-sequence octets pos seq-len initial)))
                       (setf (char result char-index) (code-char code-point))
                       (incf char-index)
                       (incf pos seq-len))))
          result)))))
