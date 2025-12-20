;;;; utf8.lisp - UTF-8 encoding utilities
;;;;
;;;; Replaces flexi-streams dependency for self-hosting.

(in-package #:clysm/utils)

(defun string-to-utf8 (string)
  "Convert a string to a vector of UTF-8 encoded bytes.
   This is a self-hosted replacement for flexi-streams:string-to-octets."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (dotimes (i (length string))
      (encode-utf8-char (char-code (char string i)) bytes))
    ;; Return as simple vector for compatibility
    (coerce bytes '(simple-array (unsigned-byte 8) (*)))))

(defun encode-utf8-char (code-point bytes)
  "Encode a single Unicode code point as UTF-8 bytes, appending to BYTES."
  (cond
    ;; ASCII: 0xxxxxxx
    ((< code-point #x80)
     (vector-push-extend code-point bytes))
    ;; 2-byte: 110xxxxx 10xxxxxx
    ((< code-point #x800)
     (vector-push-extend (logior #xC0 (ash code-point -6)) bytes)
     (vector-push-extend (logior #x80 (logand code-point #x3F)) bytes))
    ;; 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
    ((< code-point #x10000)
     (vector-push-extend (logior #xE0 (ash code-point -12)) bytes)
     (vector-push-extend (logior #x80 (logand (ash code-point -6) #x3F)) bytes)
     (vector-push-extend (logior #x80 (logand code-point #x3F)) bytes))
    ;; 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    (t
     (vector-push-extend (logior #xF0 (ash code-point -18)) bytes)
     (vector-push-extend (logior #x80 (logand (ash code-point -12) #x3F)) bytes)
     (vector-push-extend (logior #x80 (logand (ash code-point -6) #x3F)) bytes)
     (vector-push-extend (logior #x80 (logand code-point #x3F)) bytes))))
