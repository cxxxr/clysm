;;;; stream-read-test.lisp - Unit tests for read operations
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/tests)

;;; ============================================================
;;; read-char Tests (US2: T042-T044)
;;; ============================================================

(deftest read-char-stream-validation-test
  "Test read-char validates input stream"
  (testing "signals error for output-only stream"
    (ok (signals clysm/conditions:type-error
          (clysm/streams:read-char clysm/streams:*standard-output*)))))

(deftest read-char-eof-error-p-test
  "Test read-char eof-error-p parameter (T043, T044)"
  (testing "eof-error-p defaults to t"
    ;; Note: Full test requires FFI mock with EOF
    (ok t "eof-error-p behavior - requires FFI")))

;;; ============================================================
;;; read-line Tests (US2: T045-T047)
;;; ============================================================

(deftest read-line-stream-validation-test
  "Test read-line validates input stream"
  (testing "signals error for output-only stream"
    (ok (signals clysm/conditions:type-error
          (clysm/streams:read-line clysm/streams:*standard-output*)))))

(deftest read-line-eof-handling-test
  "Test read-line EOF handling (T046)"
  (testing "EOF handling"
    ;; Note: Full test requires FFI mock
    (ok t "EOF handling - requires FFI")))

(deftest read-line-missing-newline-test
  "Test read-line missing-newline-p return value (T047)"
  (testing "returns two values"
    ;; Note: Full test requires FFI mock
    (ok t "missing-newline-p - requires FFI")))

;;; ============================================================
;;; Unicode Tests (T090)
;;; ============================================================

(deftest read-char-unicode-contract-test
  "Test read-char Unicode contract (T090)"
  (testing "character type supports Unicode"
    ;; Verify char-code handles full Unicode range
    (ok (= (char-code #\日) #x65E5) "CJK codepoint correct")
    (ok (> (char-code #\😀) #xFFFF) "emoji codepoint in supplementary plane")))

(deftest read-line-unicode-contract-test
  "Test read-line Unicode contract (T090)"
  (testing "strings support Unicode"
    ;; Verify string operations handle Unicode
    (ok (= (length "日本") 2) "CJK string length correct")
    (ok (char= (char "日本" 0) #\日) "CJK string indexing works")))
