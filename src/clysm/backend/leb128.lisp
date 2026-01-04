;;;; backend/leb128.lisp - LEB128 Variable-Length Integer Encoding
;;;;
;;;; LEB128 (Little Endian Base 128) is used throughout WebAssembly
;;;; for encoding integers in a compact, variable-length format.
;;;;
;;;; - ULEB128: Unsigned integers (indices, sizes, counts)
;;;; - SLEB128: Signed integers (i32.const, i64.const literals)

(in-package #:clysm)

;;; ============================================================
;;; ULEB128 - Unsigned LEB128
;;; ============================================================

(defun encode-uleb128 (value)
  "Encode an unsigned integer VALUE as ULEB128 bytes.
Returns a list of bytes (unsigned-byte 8)."
  (check-type value (integer 0))
  (if (zerop value)
      (list 0)
      (loop with result = nil
            while (plusp value)
            do (let ((byte (logand value #x7F)))
                 (setf value (ash value -7))
                 (when (plusp value)
                   (setf byte (logior byte #x80)))
                 (push byte result))
            finally (return (nreverse result)))))

(defun decode-uleb128 (bytes &optional (start 0))
  "Decode ULEB128 bytes starting at START index.
Returns two values: the decoded value and the number of bytes consumed."
  (loop with result = 0
        with shift = 0
        for i from start
        for byte = (if (listp bytes)
                       (nth i bytes)
                       (aref bytes i))
        do (setf result (logior result (ash (logand byte #x7F) shift)))
           (incf shift 7)
        until (zerop (logand byte #x80))
        finally (return (values result (1+ (- i start))))))

;;; ============================================================
;;; SLEB128 - Signed LEB128
;;; ============================================================

(defun encode-sleb128 (value)
  "Encode a signed integer VALUE as SLEB128 bytes.
Returns a list of bytes (unsigned-byte 8)."
  (check-type value integer)
  (let ((result nil)
        (more t))
    (loop while more
          do (let ((byte (logand value #x7F)))
               (setf value (ash value -7))
               ;; Sign bit of byte is second high order bit (0x40)
               (let ((sign-bit (logand byte #x40)))
                 (if (or (and (zerop value) (zerop sign-bit))
                         (and (= value -1) (not (zerop sign-bit))))
                     (setf more nil)
                     (setf byte (logior byte #x80))))
               (push byte result)))
    (nreverse result)))

(defun decode-sleb128 (bytes &optional (start 0))
  "Decode SLEB128 bytes starting at START index.
Returns two values: the decoded value and the number of bytes consumed."
  (loop with result = 0
        with shift = 0
        for i from start
        for byte = (if (listp bytes)
                       (nth i bytes)
                       (aref bytes i))
        do (setf result (logior result (ash (logand byte #x7F) shift)))
           (incf shift 7)
        until (zerop (logand byte #x80))
        ;; Sign extend if necessary
        finally (progn
                  (when (and (< shift (* 8 (ceiling shift 8)))
                             (not (zerop (logand byte #x40))))
                    ;; Negative number - sign extend
                    (setf result (logior result (- (ash 1 shift)))))
                  (return (values result (1+ (- i start)))))))

;;; ============================================================
;;; Specialized Encoders for Common Wasm Types
;;; ============================================================

(defun encode-u32 (value)
  "Encode a u32 value as ULEB128. Validates range."
  (check-type value (integer 0 #xFFFFFFFF))
  (encode-uleb128 value))

(defun encode-s32 (value)
  "Encode an s32 value as SLEB128. Validates range."
  (check-type value (integer #x-80000000 #x7FFFFFFF))
  (encode-sleb128 value))

(defun encode-s33 (value)
  "Encode an s33 value as SLEB128. Used for block types in Wasm."
  (check-type value (integer #x-100000000 #xFFFFFFFF))
  (encode-sleb128 value))

(defun encode-u64 (value)
  "Encode a u64 value as ULEB128. Validates range."
  (check-type value (integer 0 #xFFFFFFFFFFFFFFFF))
  (encode-uleb128 value))

(defun encode-s64 (value)
  "Encode an s64 value as SLEB128. Validates range."
  (check-type value (integer #x-8000000000000000 #x7FFFFFFFFFFFFFFF))
  (encode-sleb128 value))

;;; ============================================================
;;; Vector Encoding (for Wasm vec types)
;;; ============================================================

(defun encode-vector (encoder items)
  "Encode a vector: length followed by encoded items.
ENCODER is called for each item and should return a byte list."
  (append (encode-uleb128 (length items))
          (mappend encoder items)))

(defun encode-byte-vector (bytes)
  "Encode a byte vector: length prefix followed by raw bytes."
  (append (encode-uleb128 (length bytes))
          (if (listp bytes) bytes (coerce bytes 'list))))

;;; ============================================================
;;; Name Encoding (UTF-8 strings for Wasm)
;;; ============================================================

(defun encode-name (string)
  "Encode a name as a UTF-8 byte vector."
  ;; Simple ASCII-only for now; UTF-8 can be added later
  (let ((bytes (map 'list #'char-code string)))
    (encode-byte-vector bytes)))

;;; ============================================================
;;; Fixed-width Encoders (for floats and memory immediates)
;;; ============================================================

(defun encode-f32 (value)
  "Encode a single-precision float as 4 bytes (little-endian)."
  (let* ((bits (sb-kernel:single-float-bits (coerce value 'single-float))))
    (list (logand bits #xFF)
          (logand (ash bits -8) #xFF)
          (logand (ash bits -16) #xFF)
          (logand (ash bits -24) #xFF))))

(defun encode-f64 (value)
  "Encode a double-precision float as 8 bytes (little-endian)."
  (let ((bits (sb-kernel:double-float-bits (coerce value 'double-float))))
    (list (logand bits #xFF)
          (logand (ash bits -8) #xFF)
          (logand (ash bits -16) #xFF)
          (logand (ash bits -24) #xFF)
          (logand (ash bits -32) #xFF)
          (logand (ash bits -40) #xFF)
          (logand (ash bits -48) #xFF)
          (logand (ash bits -56) #xFF))))
