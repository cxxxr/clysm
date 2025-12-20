;;;; binary.lisp - Binary encoding utilities for WASM

(in-package #:clysm/utils)

;;; LEB128 Encoding
;;; Little Endian Base 128 - variable length integer encoding used by WASM

(defun encode-uleb128 (value)
  "Encode an unsigned integer as ULEB128 bytes. Returns a list of bytes."
  (declare (type (integer 0) value))
  (if (zerop value)
      (list 0)
      (let ((result nil))
        (do ()
            ((not (plusp value)) (nreverse result))
          (let ((byte (logand value #x7f)))
            (setf value (ash value -7))
            (push (if (plusp value)
                      (logior byte #x80)
                      byte)
                  result))))))

(defun encode-sleb128 (value)
  "Encode a signed integer as SLEB128 bytes. Returns a list of bytes."
  (declare (type integer value))
  (let ((result nil)
        (more t))
    (do ()
        ((not more) (nreverse result))
      (let ((byte (logand value #x7f)))
        (setf value (ash value -7))
        (cond
          ;; Positive number, high bit of byte is clear
          ((and (zerop value) (zerop (logand byte #x40)))
           (setf more nil))
          ;; Negative number, high bit of byte is set
          ((and (= value -1) (not (zerop (logand byte #x40))))
           (setf more nil))
          ;; Need more bytes
          (t
           (setf byte (logior byte #x80))))
        (push byte result)))))

(defun decode-uleb128 (bytes &optional (start 0))
  "Decode ULEB128 from a byte sequence. Returns (values decoded-value bytes-consumed)."
  (let ((result 0)
        (shift 0)
        (i start))
    (do ((byte (elt bytes i) (elt bytes i)))
        ((zerop (logand byte #x80))
         (setf result (logior result (ash (logand byte #x7f) shift)))
         (values result (1+ (- i start))))
      (setf result (logior result (ash (logand byte #x7f) shift)))
      (incf shift 7)
      (incf i))))

(defun decode-sleb128 (bytes &optional (start 0))
  "Decode SLEB128 from a byte sequence. Returns (values decoded-value bytes-consumed)."
  (let ((result 0)
        (shift 0)
        (i start)
        (byte nil))
    (do ()
        ((and byte (zerop (logand byte #x80)))
         ;; Sign extend if negative
         (when (and (< shift 64)
                    (not (zerop (logand byte #x40))))
           (setf result (logior result (ash -1 shift))))
         (values result (1+ (- i start))))
      (setf byte (elt bytes i))
      (setf result (logior result (ash (logand byte #x7f) shift)))
      (incf shift 7)
      (incf i))))

;;; Byte Buffer
;;; Growable byte buffer for building WASM binary output

(defstruct (byte-buffer (:constructor %make-byte-buffer))
  "A growable buffer for accumulating bytes."
  (data (make-array 256 :element-type '(unsigned-byte 8)
                        :adjustable t
                        :fill-pointer 0)
   :type (vector (unsigned-byte 8))))

(defun make-byte-buffer (&optional (initial-size 256))
  "Create a new byte buffer with optional initial capacity."
  (%make-byte-buffer
   :data (make-array initial-size :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0)))

(defun buffer-contents (buffer)
  "Return the contents of the buffer as a simple byte vector."
  (let* ((data (byte-buffer-data buffer))
         (len (length data))
         (result (make-array len :element-type '(unsigned-byte 8))))
    (replace result data)
    result))

(defun buffer-write-byte (buffer byte)
  "Write a single byte to the buffer."
  (declare (type (unsigned-byte 8) byte))
  (vector-push-extend byte (byte-buffer-data buffer))
  buffer)

(defun buffer-write-bytes (buffer bytes)
  "Write a sequence of bytes to the buffer."
  (let ((data (byte-buffer-data buffer)))
    (etypecase bytes
      (list
       (dolist (b bytes)
         (vector-push-extend b data)))
      (vector
       (dotimes (i (length bytes))
         (vector-push-extend (aref bytes i) data)))))
  buffer)

(defun buffer-write-u32 (buffer value)
  "Write a 32-bit unsigned integer in little-endian format."
  (declare (type (unsigned-byte 32) value))
  (buffer-write-byte buffer (logand value #xff))
  (buffer-write-byte buffer (logand (ash value -8) #xff))
  (buffer-write-byte buffer (logand (ash value -16) #xff))
  (buffer-write-byte buffer (logand (ash value -24) #xff))
  buffer)

;;; IEEE 754 Float Encoding
;;; Portable implementations that don't depend on SBCL internals.

(defun single-float-to-bits (value)
  "Convert a single-float to its IEEE 754 32-bit representation.
   Returns an unsigned 32-bit integer."
  (declare (type single-float value))
  (cond
    ;; Handle special cases
    ((zerop value)
     (if (minusp (float-sign value))
         #x80000000  ; -0.0
         #x00000000)) ; +0.0
    ;; Normal numbers
    (t
     (multiple-value-bind (significand exponent sign)
         (integer-decode-float value)
       ;; For single-float: 23 bits mantissa, 8 bits exponent, 1 bit sign
       ;; integer-decode-float returns significand as integer with implicit bit
       ;; We need to adjust to IEEE format
       (let* ((sign-bit (if (minusp sign) 1 0))
              ;; Normalize the significand
              ;; single-float has 24 bits of precision (23 stored + 1 implicit)
              (precision (float-precision value))
              ;; Adjust exponent: IEEE uses bias of 127
              ;; integer-decode-float exponent is for significand as integer
              (ieee-exponent (+ exponent precision 127 -1))
              ;; Get mantissa bits (remove implicit leading 1)
              (mantissa (logand significand (1- (ash 1 (1- precision))))))
         (cond
           ;; Overflow to infinity
           ((>= ieee-exponent 255)
            (logior (ash sign-bit 31) #x7f800000))
           ;; Underflow to zero (denormals not fully handled)
           ((<= ieee-exponent 0)
            (ash sign-bit 31))
           ;; Normal number
           (t
            (logior (ash sign-bit 31)
                    (ash ieee-exponent 23)
                    (ash mantissa (- 23 (1- precision)))))))))))

(defun double-float-to-bits (value)
  "Convert a double-float to its IEEE 754 64-bit representation.
   Returns an unsigned 64-bit integer."
  (declare (type double-float value))
  (cond
    ;; Handle special cases
    ((zerop value)
     (if (minusp (float-sign value))
         #x8000000000000000  ; -0.0
         #x0000000000000000)) ; +0.0
    ;; Normal numbers
    (t
     (multiple-value-bind (significand exponent sign)
         (integer-decode-float value)
       ;; For double-float: 52 bits mantissa, 11 bits exponent, 1 bit sign
       (let* ((sign-bit (if (minusp sign) 1 0))
              (precision (float-precision value))
              ;; Adjust exponent: IEEE uses bias of 1023
              (ieee-exponent (+ exponent precision 1023 -1))
              ;; Get mantissa bits (remove implicit leading 1)
              (mantissa (logand significand (1- (ash 1 (1- precision))))))
         (cond
           ;; Overflow to infinity
           ((>= ieee-exponent 2047)
            (logior (ash sign-bit 63) #x7ff0000000000000))
           ;; Underflow to zero
           ((<= ieee-exponent 0)
            (ash sign-bit 63))
           ;; Normal number
           (t
            (logior (ash sign-bit 63)
                    (ash ieee-exponent 52)
                    (ash mantissa (- 52 (1- precision)))))))))))

(defun buffer-write-f32 (buffer value)
  "Write a 32-bit IEEE 754 float in little-endian format."
  (declare (type single-float value))
  (let ((bits (single-float-to-bits value)))
    (buffer-write-u32 buffer bits)))

(defun buffer-write-f64 (buffer value)
  "Write a 64-bit IEEE 754 double in little-endian format."
  (declare (type double-float value))
  (let ((bits (double-float-to-bits value)))
    (buffer-write-byte buffer (logand bits #xff))
    (buffer-write-byte buffer (logand (ash bits -8) #xff))
    (buffer-write-byte buffer (logand (ash bits -16) #xff))
    (buffer-write-byte buffer (logand (ash bits -24) #xff))
    (buffer-write-byte buffer (logand (ash bits -32) #xff))
    (buffer-write-byte buffer (logand (ash bits -40) #xff))
    (buffer-write-byte buffer (logand (ash bits -48) #xff))
    (buffer-write-byte buffer (logand (ash bits -56) #xff)))
  buffer)

(defun buffer-write-uleb128 (buffer value)
  "Write an unsigned integer as ULEB128."
  (buffer-write-bytes buffer (encode-uleb128 value))
  buffer)

(defun buffer-write-sleb128 (buffer value)
  "Write a signed integer as SLEB128."
  (buffer-write-bytes buffer (encode-sleb128 value))
  buffer)

(defun buffer-write-name (buffer name)
  "Write a WASM name (UTF-8 string with length prefix)."
  (let ((bytes (string-to-utf8 name)))
    (buffer-write-uleb128 buffer (length bytes))
    (buffer-write-bytes buffer bytes))
  buffer)

(defun buffer-length (buffer)
  "Return the current length of the buffer."
  (length (byte-buffer-data buffer)))
