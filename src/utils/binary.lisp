;;;; binary.lisp - Binary encoding utilities for WASM

(in-package #:cl-wasm/utils)

;;; LEB128 Encoding
;;; Little Endian Base 128 - variable length integer encoding used by WASM

(defun encode-uleb128 (value)
  "Encode an unsigned integer as ULEB128 bytes. Returns a list of bytes."
  (declare (type (integer 0) value))
  (if (zerop value)
      (list 0)
      (loop with result = nil
            while (plusp value)
            for byte = (logand value #x7f)
            do (setf value (ash value -7))
               (push (if (plusp value)
                         (logior byte #x80)
                         byte)
                     result)
            finally (return (nreverse result)))))

(defun encode-sleb128 (value)
  "Encode a signed integer as SLEB128 bytes. Returns a list of bytes."
  (declare (type integer value))
  (let ((result nil)
        (more t))
    (loop while more
          for byte = (logand value #x7f)
          do (setf value (ash value -7))
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
             (push byte result))
    (nreverse result)))

(defun decode-uleb128 (bytes &optional (start 0))
  "Decode ULEB128 from a byte sequence. Returns (values decoded-value bytes-consumed)."
  (loop with result = 0
        with shift = 0
        for i from start
        for byte = (elt bytes i)
        do (setf result (logior result (ash (logand byte #x7f) shift)))
           (incf shift 7)
        until (zerop (logand byte #x80))
        finally (return (values result (1+ (- i start))))))

(defun decode-sleb128 (bytes &optional (start 0))
  "Decode SLEB128 from a byte sequence. Returns (values decoded-value bytes-consumed)."
  (loop with result = 0
        with shift = 0
        for i from start
        for byte = (elt bytes i)
        do (setf result (logior result (ash (logand byte #x7f) shift)))
           (incf shift 7)
        until (zerop (logand byte #x80))
        finally
           ;; Sign extend if negative
           (when (and (< shift 64)
                      (not (zerop (logand byte #x40))))
             (setf result (logior result (ash -1 shift))))
           (return (values result (1+ (- i start))))))

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
       (loop for b across bytes
             do (vector-push-extend b data)))))
  buffer)

(defun buffer-write-u32 (buffer value)
  "Write a 32-bit unsigned integer in little-endian format."
  (declare (type (unsigned-byte 32) value))
  (buffer-write-byte buffer (logand value #xff))
  (buffer-write-byte buffer (logand (ash value -8) #xff))
  (buffer-write-byte buffer (logand (ash value -16) #xff))
  (buffer-write-byte buffer (logand (ash value -24) #xff))
  buffer)

(defun buffer-write-f32 (buffer value)
  "Write a 32-bit IEEE 754 float in little-endian format."
  (declare (type single-float value))
  (let ((bits (sb-kernel:single-float-bits value)))
    (buffer-write-u32 buffer (ldb (byte 32 0) bits))))

(defun buffer-write-f64 (buffer value)
  "Write a 64-bit IEEE 754 double in little-endian format."
  (declare (type double-float value))
  (let ((bits (sb-kernel:double-float-bits value)))
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
  (let ((bytes (flexi-streams:string-to-octets name :external-format :utf-8)))
    (buffer-write-uleb128 buffer (length bytes))
    (buffer-write-bytes buffer bytes))
  buffer)

(defun buffer-length (buffer)
  "Return the current length of the buffer."
  (length (byte-buffer-data buffer)))
