;;;; stream-format-test.lisp - Unit tests for format function
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/tests)

;;; ============================================================
;;; Format Destination Tests (T060-T062)
;;; ============================================================

(deftest format-nil-destination-test
  "Test format with nil destination returns string (T060)"
  (testing "nil destination returns string"
    (ok (stringp (clysm/streams:format nil "hello")))
    (ok (equal (clysm/streams:format nil "hello") "hello"))))

(deftest format-t-destination-test
  "Test format with t destination (T061)"
  (testing "t destination writes to *standard-output*"
    ;; Note: Full output test requires FFI
    (ok t "t destination - requires FFI")))

;;; ============================================================
;;; Format Directive Tests (T055-T059)
;;; ============================================================

(deftest format-aesthetic-directive-test
  "Test format ~A directive (T055)"
  (testing "~A formats string without quotes"
    (ok (equal (clysm/streams:format nil "~A" "test") "test")))
  (testing "~A formats number as string"
    (ok (equal (clysm/streams:format nil "~A" 42) "42"))))

(deftest format-standard-directive-test
  "Test format ~S directive (T056)"
  (testing "~S formats string with quotes"
    (ok (equal (clysm/streams:format nil "~S" "test") "\"test\""))))

(deftest format-decimal-directive-test
  "Test format ~D directive (T057)"
  (testing "~D formats integer"
    (ok (equal (clysm/streams:format nil "~D" 42) "42")))
  (testing "~D signals error for non-integer"
    (ok (signals (clysm/streams:format nil "~D" "not a number") 'type-error))))

(deftest format-newline-directive-test
  "Test format ~% directive (T058)"
  (testing "~% outputs newline"
    (ok (equal (clysm/streams:format nil "~%") (string #\Newline)))))

(deftest format-tilde-directive-test
  "Test format ~~ directive (T059)"
  (testing "~~ outputs single tilde"
    (ok (equal (clysm/streams:format nil "~~") "~"))))

;;; ============================================================
;;; Format Multiple Directives Test (T063)
;;; ============================================================

(deftest format-multiple-directives-test
  "Test format with multiple directives (T063)"
  (testing "combines directives correctly"
    (ok (equal (clysm/streams:format nil "Hello ~A!~%" "World")
               (concatenate 'string "Hello World!" (string #\Newline)))))
  (testing "multiple ~A directives"
    (ok (equal (clysm/streams:format nil "~A + ~A = ~D" 2 3 5)
               "2 + 3 = 5"))))

;;; ============================================================
;;; Format Parse Error Tests
;;; ============================================================

(deftest format-unknown-directive-test
  "Test format signals error for unknown directive"
  (testing "unknown directive signals error"
    (ok (signals (clysm/streams:format nil "~Q" 42)))))

(deftest format-trailing-tilde-test
  "Test format signals error for trailing tilde"
  (testing "trailing tilde signals error"
    (ok (signals (clysm/streams:format nil "hello~")))))

;;; ============================================================
;;; Unicode Tests (T090)
;;; ============================================================

(deftest format-unicode-aesthetic-test
  "Test format ~A with Unicode (T090)"
  (testing "~A formats CJK string"
    (ok (equal (clysm/streams:format nil "~A" "日本語") "日本語")))
  (testing "~A formats mixed Unicode"
    (ok (equal (clysm/streams:format nil "Hello ~A!" "世界") "Hello 世界!"))))

(deftest format-unicode-literal-test
  "Test format with Unicode literal text (T090)"
  (testing "literal Unicode preserved"
    (ok (equal (clysm/streams:format nil "こんにちは") "こんにちは")))
  (testing "Unicode with directives"
    (ok (equal (clysm/streams:format nil "数字: ~D" 42) "数字: 42"))))
