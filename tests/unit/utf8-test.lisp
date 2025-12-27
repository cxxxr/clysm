;;;; utf8-test.lisp - Unit tests for portable UTF-8 encoding/decoding
;;;; Phase 10B - 034-portable-utf8
;;;; Tests follow TDD: written FIRST, must FAIL before implementation

(defpackage #:clysm/tests/unit/utf8
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/utf8)

;;; ============================================================
;;; Phase 3: User Story 1 - Encode Lisp Strings to UTF-8 Bytes
;;; ============================================================

(deftest encode-ascii
  (testing "ASCII string 'hello' -> #(104 101 108 108 111)"
    (let ((result (clysm/lib/utf8:string-to-utf8-octets "hello")))
      (ok (= 5 (length result)))
      (ok (equalp #(104 101 108 108 111) result)))))

(deftest encode-2-byte
  (testing "2-byte encoding for Latin-1 extended (ñ -> #(195 177))"
    (let ((result (clysm/lib/utf8:string-to-utf8-octets "ñ")))
      (ok (= 2 (length result)))
      (ok (equalp #(195 177) result)))))

(deftest encode-3-byte
  (testing "3-byte encoding for CJK '日本語' -> 9 bytes"
    (let ((result (clysm/lib/utf8:string-to-utf8-octets "日本語")))
      (ok (= 9 (length result)))
      ;; 日 = U+65E5 -> E6 97 A5 (230 151 165)
      ;; 本 = U+672C -> E6 9C AC (230 156 172)
      ;; 語 = U+8A9E -> E8 AA 9E (232 170 158)
      (ok (equalp #(230 151 165 230 156 172 232 170 158) result)))))

(deftest encode-4-byte
  (testing "4-byte encoding for emoji '🎉' -> #(240 159 142 137)"
    (let ((result (clysm/lib/utf8:string-to-utf8-octets "🎉")))
      (ok (= 4 (length result)))
      (ok (equalp #(240 159 142 137) result)))))

(deftest encode-empty-string
  (testing "Empty string '' -> #()"
    (let ((result (clysm/lib/utf8:string-to-utf8-octets "")))
      (ok (= 0 (length result)))
      (ok (equalp #() result)))))

(deftest encode-max-code-point
  (testing "Maximum code point U+10FFFF encodes to 4 bytes"
    (let* ((max-char (code-char #x10FFFF))
           (result (clysm/lib/utf8:string-to-utf8-octets (string max-char))))
      (ok (= 4 (length result)))
      ;; U+10FFFF -> F4 8F BF BF (244 143 191 191)
      (ok (equalp #(244 143 191 191) result)))))

(deftest encode-nul-character
  (testing "NUL character U+0000 -> #(0)"
    (let ((result (clysm/lib/utf8:string-to-utf8-octets (string (code-char 0)))))
      (ok (= 1 (length result)))
      (ok (equalp #(0) result)))))

;;; ============================================================
;;; Phase 4: User Story 2 - Decode UTF-8 Bytes to Lisp Strings
;;; ============================================================

(deftest decode-ascii
  (testing "ASCII bytes #(104 101 108 108 111) -> 'hello'"
    (let ((result (clysm/lib/utf8:utf8-octets-to-string #(104 101 108 108 111))))
      (ok (string= "hello" result)))))

(deftest decode-2-byte
  (testing "2-byte decoding for Latin-1 extended"
    (let ((result (clysm/lib/utf8:utf8-octets-to-string #(195 177))))
      (ok (string= "ñ" result)))))

(deftest decode-3-byte
  (testing "3-byte decoding for CJK"
    (let ((result (clysm/lib/utf8:utf8-octets-to-string
                   #(230 151 165 230 156 172 232 170 158))))
      (ok (string= "日本語" result)))))

(deftest decode-4-byte
  (testing "4-byte decoding for emoji"
    (let ((result (clysm/lib/utf8:utf8-octets-to-string #(240 159 142 137))))
      (ok (string= "🎉" result)))))

(deftest decode-empty-vector
  (testing "Empty vector #() -> ''"
    (let ((result (clysm/lib/utf8:utf8-octets-to-string #())))
      (ok (string= "" result)))))

(deftest round-trip
  (testing "Round-trip: decode(encode(s)) = s for various strings"
    (dolist (str '("hello" "日本語" "🎉" "" "Hello, 世界! 🌍"))
      (let ((result (clysm/lib/utf8:utf8-octets-to-string
                     (clysm/lib/utf8:string-to-utf8-octets str))))
        (ok (string= str result) (format nil "Round-trip failed for: ~S" str))))))

;;; ============================================================
;;; Phase 5: User Story 3 - Handle Invalid UTF-8 Sequences
;;; ============================================================

(deftest invalid-lead-byte-ff
  (testing "Invalid lead byte 0xFF signals decoding-error"
    (ok (signals (clysm/lib/utf8:utf8-octets-to-string #(255 0))
                 'clysm/lib/utf8:decoding-error))))

(deftest overlong-lead-byte-c0
  (testing "Overlong lead byte 0xC0 signals decoding-error"
    (ok (signals (clysm/lib/utf8:utf8-octets-to-string #(192 128))
                 'clysm/lib/utf8:decoding-error))))

(deftest overlong-lead-byte-c1
  (testing "Overlong lead byte 0xC1 signals decoding-error"
    (ok (signals (clysm/lib/utf8:utf8-octets-to-string #(193 128))
                 'clysm/lib/utf8:decoding-error))))

(deftest unexpected-continuation
  (testing "Unexpected continuation byte 0x80 at start signals error"
    (ok (signals (clysm/lib/utf8:utf8-octets-to-string #(128))
                 'clysm/lib/utf8:decoding-error))))

(deftest missing-continuation
  (testing "Missing continuation (truncated sequence) signals error"
    ;; 2-byte lead byte without continuation
    (ok (signals (clysm/lib/utf8:utf8-octets-to-string #(194))
                 'clysm/lib/utf8:decoding-error))))

(deftest overlong-3-byte
  (testing "Overlong 3-byte encoding for ASCII signals error"
    ;; Attempt to encode ASCII 'a' (0x61) as 3 bytes: E0 80 A1
    ;; This is an overlong encoding and should be rejected
    (ok (signals (clysm/lib/utf8:utf8-octets-to-string #(224 128 161))
                 'clysm/lib/utf8:decoding-error))))

(deftest surrogate-code-point
  (testing "Surrogate code point U+D800 (encoded) signals error"
    ;; U+D800 would encode as ED A0 80, which is invalid
    (ok (signals (clysm/lib/utf8:utf8-octets-to-string #(237 160 128))
                 'clysm/lib/utf8:decoding-error))))

(deftest error-includes-position
  (testing "decoding-error includes correct position"
    (handler-case
        (clysm/lib/utf8:utf8-octets-to-string #(104 101 255 108 111))
      (clysm/lib/utf8:decoding-error (e)
        (ok (= 2 (clysm/lib/utf8:decoding-error-position e)))))))

(deftest error-includes-invalid-bytes
  (testing "decoding-error includes invalid-bytes vector"
    (handler-case
        (clysm/lib/utf8:utf8-octets-to-string #(255 0))
      (clysm/lib/utf8:decoding-error (e)
        (ok (arrayp (clysm/lib/utf8:decoding-error-invalid-bytes e)))
        (ok (> (length (clysm/lib/utf8:decoding-error-invalid-bytes e)) 0))))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(deftest mixed-byte-lengths
  (testing "Mixed ASCII, 2-byte, 3-byte, 4-byte in one string"
    (let* ((str "a ñ 日 🎉")
           (encoded (clysm/lib/utf8:string-to-utf8-octets str))
           (decoded (clysm/lib/utf8:utf8-octets-to-string encoded)))
      (ok (string= str decoded)))))

(deftest boundary-code-points
  (testing "Boundary code points encode correctly"
    ;; U+007F (1-byte max)
    (ok (= 1 (length (clysm/lib/utf8:string-to-utf8-octets
                      (string (code-char #x7F))))))
    ;; U+0080 (2-byte min)
    (ok (= 2 (length (clysm/lib/utf8:string-to-utf8-octets
                      (string (code-char #x80))))))
    ;; U+07FF (2-byte max)
    (ok (= 2 (length (clysm/lib/utf8:string-to-utf8-octets
                      (string (code-char #x7FF))))))
    ;; U+0800 (3-byte min)
    (ok (= 3 (length (clysm/lib/utf8:string-to-utf8-octets
                      (string (code-char #x800))))))
    ;; U+FFFF (3-byte max)
    (ok (= 3 (length (clysm/lib/utf8:string-to-utf8-octets
                      (string (code-char #xFFFF))))))
    ;; U+10000 (4-byte min)
    (ok (= 4 (length (clysm/lib/utf8:string-to-utf8-octets
                      (string (code-char #x10000))))))))
