;;;; leb128-test.lisp - LEB128 encoding tests

(in-package #:clysm/tests/contract/leb128)

(defun bytes (&rest values)
  "Create a byte vector from values."
  (make-array (length values)
              :element-type '(unsigned-byte 8)
              :initial-contents values))

(deftest test-encode-unsigned-leb128-zero
  (ok (equalp (bytes 0) (encode-unsigned-leb128 0))))

(deftest test-encode-unsigned-leb128-small
  (ok (equalp (bytes 1) (encode-unsigned-leb128 1)))
  (ok (equalp (bytes 127) (encode-unsigned-leb128 127))))

(deftest test-encode-unsigned-leb128-continuation
  (ok (equalp (bytes 128 1) (encode-unsigned-leb128 128)))
  (ok (equalp (bytes 255 1) (encode-unsigned-leb128 255))))

(deftest test-encode-unsigned-leb128-large
  (ok (equalp (bytes 128 128 1) (encode-unsigned-leb128 16384))))

(deftest test-encode-signed-leb128-zero
  (ok (equalp (bytes 0) (encode-signed-leb128 0))))

(deftest test-encode-signed-leb128-positive
  (ok (equalp (bytes 1) (encode-signed-leb128 1)))
  (ok (equalp (bytes 63) (encode-signed-leb128 63))))

(deftest test-encode-signed-leb128-negative
  (ok (equalp (bytes 127) (encode-signed-leb128 -1)))
  (ok (equalp (bytes 64) (encode-signed-leb128 -64))))

(deftest test-encode-signed-leb128-continuation
  (ok (equalp (bytes 191 127) (encode-signed-leb128 -65))))

(deftest test-decode-unsigned-leb128
  (ok (= 0 (decode-unsigned-leb128 (bytes 0))))
  (ok (= 127 (decode-unsigned-leb128 (bytes 127))))
  (ok (= 128 (decode-unsigned-leb128 (bytes 128 1)))))

(deftest test-decode-signed-leb128
  (ok (= 0 (decode-signed-leb128 (bytes 0))))
  (ok (= -1 (decode-signed-leb128 (bytes 127))))
  (ok (= -64 (decode-signed-leb128 (bytes 64)))))

(deftest test-leb128-roundtrip
  (loop for n in '(0 1 127 128 255 256 16383 16384)
        do (ok (= n (decode-unsigned-leb128 (encode-unsigned-leb128 n)))))
  (loop for n in '(0 1 -1 63 -64 64 -65 127 -128)
        do (ok (= n (decode-signed-leb128 (encode-signed-leb128 n))))))
