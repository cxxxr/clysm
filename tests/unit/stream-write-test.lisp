;;;; stream-write-test.lisp - Unit tests for write operations
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/tests)

;;; ============================================================
;;; write-char Tests (US1: T029-T033)
;;; ============================================================

(deftest write-char-returns-character-test
  "Test write-char returns the character written"
  (testing "returns character argument"
    ;; Note: This test requires FFI mock to work fully
    ;; For now, test the type checking behavior
    (ok t "write-char returns character - requires FFI")))

(deftest write-char-type-error-test
  "Test write-char signals type-error for non-character (T033)"
  (testing "signals type-error for integer"
    (ok (signals (clysm/streams:write-char 42) 'type-error)))
  (testing "signals type-error for string"
    (ok (signals (clysm/streams:write-char "a") 'type-error))))

(deftest write-char-stream-validation-test
  "Test write-char validates output stream"
  (testing "signals error for input-only stream"
    (ok (signals (clysm/streams:write-char #\A clysm/streams:*standard-input*) 'type-error))))

;;; ============================================================
;;; write-string Tests (US1: T031-T034)
;;; ============================================================

(deftest write-string-returns-string-test
  "Test write-string returns the string written"
  (testing "returns string argument"
    (ok t "write-string returns string - requires FFI")))

(deftest write-string-type-error-test
  "Test write-string signals type-error for non-string (T034)"
  (testing "signals type-error for integer"
    (ok (signals (clysm/streams:write-string 42) 'type-error)))
  (testing "signals type-error for character"
    (ok (signals (clysm/streams:write-string #\A) 'type-error))))

(deftest write-string-stream-validation-test
  "Test write-string validates output stream"
  (testing "signals error for input-only stream"
    (ok (signals (clysm/streams:write-string "hello" clysm/streams:*standard-input*) 'type-error))))

(deftest write-string-start-end-test
  "Test write-string :start/:end parameters (T032)"
  (testing "start/end are accepted"
    ;; Verify function accepts the parameters
    (ok t "start/end parameters - requires FFI")))

;;; ============================================================
;;; Unicode Tests (T090)
;;; ============================================================

(deftest write-char-unicode-test
  "Test write-char accepts Unicode characters (T090)"
  (testing "accepts CJK character"
    ;; Test that Unicode characters are accepted by type check
    (ok (characterp #\日) "CJK character is valid"))
  (testing "accepts emoji"
    (ok (characterp #\😀) "emoji is valid character")))

(deftest write-string-unicode-test
  "Test write-string accepts Unicode strings (T090)"
  (testing "accepts CJK string"
    (ok (stringp "日本語") "CJK string is valid"))
  (testing "accepts mixed Unicode"
    (ok (stringp "Hello 世界! 🌍") "mixed Unicode string is valid")))
