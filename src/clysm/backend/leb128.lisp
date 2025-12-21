;;;; leb128.lisp - LEB128 variable-length integer encoding
;;;; Reference: WebAssembly Binary Format specification

(in-package #:clysm/backend/leb128)

(defun encode-unsigned-leb128 (value)
  "Encode an unsigned integer to LEB128 byte vector.
   Each byte contains 7 bits of data with MSB as continuation bit."
  (declare (type (integer 0) value))
  (let ((bytes '()))
    (loop
      (let ((byte (logand value #x7f)))
        (setf value (ash value -7))
        (if (zerop value)
            (progn (push byte bytes) (return))
            (push (logior byte #x80) bytes))))
    (coerce (nreverse bytes) '(vector (unsigned-byte 8)))))

(defun encode-signed-leb128 (value)
  "Encode a signed integer to LEB128 byte vector.
   Uses two's complement representation with sign extension."
  (declare (type integer value))
  (let ((bytes '())
        (more t))
    (loop while more do
      (let* ((byte (logand value #x7f))
             (value-next (ash value -7))
             (sign-bit (logand byte #x40)))
        (setf value value-next)
        (cond
          ;; Positive number, no more significant bits
          ((and (zerop value) (zerop sign-bit))
           (push byte bytes)
           (setf more nil))
          ;; Negative number, all remaining bits are 1
          ((and (= value -1) (not (zerop sign-bit)))
           (push byte bytes)
           (setf more nil))
          ;; More bytes needed
          (t
           (push (logior byte #x80) bytes)))))
    (coerce (nreverse bytes) '(vector (unsigned-byte 8)))))

(defun decode-unsigned-leb128 (bytes &optional (start 0))
  "Decode LEB128 bytes to unsigned integer.
   Returns (values result bytes-consumed)."
  (declare (type (vector (unsigned-byte 8)) bytes)
           (type fixnum start))
  (let ((result 0)
        (shift 0)
        (index start))
    (loop
      (let ((byte (aref bytes index)))
        (incf index)
        (setf result (logior result (ash (logand byte #x7f) shift)))
        (when (zerop (logand byte #x80))
          (return (values result (- index start))))
        (incf shift 7)))))

(defun decode-signed-leb128 (bytes &optional (start 0))
  "Decode LEB128 bytes to signed integer.
   Returns (values result bytes-consumed)."
  (declare (type (vector (unsigned-byte 8)) bytes)
           (type fixnum start))
  (let ((result 0)
        (shift 0)
        (index start)
        (byte 0))
    (loop
      (setf byte (aref bytes index))
      (incf index)
      (setf result (logior result (ash (logand byte #x7f) shift)))
      (incf shift 7)
      (when (zerop (logand byte #x80))
        (return)))
    ;; Sign extend if negative
    (when (and (< shift 64) (not (zerop (logand byte #x40))))
      (setf result (logior result (ash -1 shift))))
    (values result (- index start))))
