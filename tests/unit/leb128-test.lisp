;;;; tests/unit/leb128-test.lisp - Tests for LEB128 encoding

(in-package #:clysm/tests)

(defsuite :leb128)

;;; ============================================================
;;; ULEB128 Encoding Tests
;;; ============================================================

(deftest uleb128-encode-zero
  (is-equal '(0) (encode-uleb128 0)))

(deftest uleb128-encode-small
  (is-equal '(1) (encode-uleb128 1))
  (is-equal '(127) (encode-uleb128 127)))

(deftest uleb128-encode-128
  ;; 128 = 0x80 requires two bytes: 0x80 0x01
  (is-equal '(#x80 #x01) (encode-uleb128 128)))

(deftest uleb128-encode-255
  ;; 255 = 0xFF = 127 + 128 = 0xFF 0x01
  (is-equal '(#xFF #x01) (encode-uleb128 255)))

(deftest uleb128-encode-256
  ;; 256 = 0x100 = 0x80 0x02
  (is-equal '(#x80 #x02) (encode-uleb128 256)))

(deftest uleb128-encode-624485
  ;; Example from Wikipedia: 624485 = 0xE5 0x8E 0x26
  (is-equal '(#xE5 #x8E #x26) (encode-uleb128 624485)))

(deftest uleb128-encode-max-u32
  ;; 2^32 - 1 = 4294967295
  (let ((bytes (encode-uleb128 #xFFFFFFFF)))
    (is-eql 5 (length bytes))
    ;; Should be: FF FF FF FF 0F
    (is-equal '(#xFF #xFF #xFF #xFF #x0F) bytes)))

;;; ============================================================
;;; ULEB128 Decoding Tests
;;; ============================================================

(deftest uleb128-decode-zero
  (is-eql 0 (decode-uleb128 '(0))))

(deftest uleb128-decode-small
  (is-eql 1 (decode-uleb128 '(1)))
  (is-eql 127 (decode-uleb128 '(127))))

(deftest uleb128-decode-128
  (is-eql 128 (decode-uleb128 '(#x80 #x01))))

(deftest uleb128-decode-624485
  (is-eql 624485 (decode-uleb128 '(#xE5 #x8E #x26))))

(deftest uleb128-decode-with-offset
  (multiple-value-bind (value bytes-read)
      (decode-uleb128 '(99 #xE5 #x8E #x26 99) 1)
    (is-eql 624485 value)
    (is-eql 3 bytes-read)))

(deftest uleb128-roundtrip
  (dolist (n '(0 1 127 128 255 256 1000 65535 624485 #xFFFFFFFF))
    (is-eql n (decode-uleb128 (encode-uleb128 n))
            (format nil "Roundtrip for ~D" n))))

;;; ============================================================
;;; SLEB128 Encoding Tests
;;; ============================================================

(deftest sleb128-encode-zero
  (is-equal '(0) (encode-sleb128 0)))

(deftest sleb128-encode-positive-small
  (is-equal '(1) (encode-sleb128 1))
  (is-equal '(63) (encode-sleb128 63)))

(deftest sleb128-encode-64
  ;; 64 needs sign bit clear in high position
  ;; 64 = 0x40, but 0x40 has sign bit set, so need 0xC0 0x00
  (is-equal '(#xC0 #x00) (encode-sleb128 64)))

(deftest sleb128-encode-negative-one
  ;; -1 = 0x7F (all 1s in 7 bits, sign extend)
  (is-equal '(#x7F) (encode-sleb128 -1)))

(deftest sleb128-encode-negative-small
  ;; -2 = 0x7E
  (is-equal '(#x7E) (encode-sleb128 -2))
  ;; -64 = 0x40
  (is-equal '(#x40) (encode-sleb128 -64)))

(deftest sleb128-encode-negative-65
  ;; -65 needs two bytes: 0xBF 0x7F
  (is-equal '(#xBF #x7F) (encode-sleb128 -65)))

(deftest sleb128-encode-minus-123456
  ;; Example: -123456 = 0xC0 0xBB 0x78
  (is-equal '(#xC0 #xBB #x78) (encode-sleb128 -123456)))

;;; ============================================================
;;; SLEB128 Decoding Tests
;;; ============================================================

(deftest sleb128-decode-zero
  (is-eql 0 (decode-sleb128 '(0))))

(deftest sleb128-decode-positive
  (is-eql 1 (decode-sleb128 '(1)))
  (is-eql 63 (decode-sleb128 '(63)))
  (is-eql 64 (decode-sleb128 '(#xC0 #x00))))

(deftest sleb128-decode-negative
  (is-eql -1 (decode-sleb128 '(#x7F)))
  (is-eql -2 (decode-sleb128 '(#x7E)))
  (is-eql -64 (decode-sleb128 '(#x40)))
  (is-eql -65 (decode-sleb128 '(#xBF #x7F))))

(deftest sleb128-decode-minus-123456
  (is-eql -123456 (decode-sleb128 '(#xC0 #xBB #x78))))

(deftest sleb128-roundtrip
  (dolist (n '(0 1 63 64 127 128 -1 -2 -64 -65 -128 -123456
               #x7FFFFFFF #x-80000000))
    (is-eql n (decode-sleb128 (encode-sleb128 n))
            (format nil "Roundtrip for ~D" n))))

;;; ============================================================
;;; Specialized Encoder Tests
;;; ============================================================

(deftest encode-u32-valid
  (finishes (encode-u32 0))
  (finishes (encode-u32 #xFFFFFFFF)))

(deftest encode-u32-invalid
  (signals type-error (encode-u32 -1))
  (signals type-error (encode-u32 #x100000000)))

(deftest encode-s32-valid
  (finishes (encode-s32 0))
  (finishes (encode-s32 #x7FFFFFFF))
  (finishes (encode-s32 #x-80000000)))

(deftest encode-s32-invalid
  (signals type-error (encode-s32 #x80000000))
  (signals type-error (encode-s32 #x-80000001)))

;;; ============================================================
;;; Vector Encoding Tests
;;; ============================================================

(deftest encode-vector-empty
  (is-equal '(0) (encode-vector #'identity nil)))

(deftest encode-vector-basic
  ;; Vector of 3 items, each encoded as a single byte
  (is-equal '(3 1 2 3)
            (encode-vector #'list '(1 2 3))))

(deftest encode-byte-vector-basic
  (is-equal '(5 104 101 108 108 111)
            (encode-byte-vector '(104 101 108 108 111))))

;;; ============================================================
;;; Name Encoding Tests
;;; ============================================================

(deftest encode-name-empty
  (is-equal '(0) (encode-name "")))

(deftest encode-name-basic
  ;; "add" = 97, 100, 100
  (is-equal '(3 97 100 100) (encode-name "add")))

(deftest encode-name-hello
  (is-equal '(5 104 101 108 108 111) (encode-name "hello")))
