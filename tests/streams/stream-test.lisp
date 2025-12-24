;;;; stream-test.lisp - Tests for stream I/O functionality
;;;; FFI-based stream I/O (015-ffi-stream-io)

(in-package #:clysm/tests/streams)

;;; ============================================================
;;; Stream Type Tests (T005-T008)
;;; ============================================================

(deftest stream-type-index-test
  "Test that +type-stream+ is defined at index 19"
  (testing "stream type index"
    (ok (= clysm/compiler/codegen/gc-types:+type-stream+ 19))))

(deftest stream-type-constructor-test
  "Test make-stream-type generates proper WasmGC struct"
  (testing "stream type has correct fields"
    (let ((stream-type (clysm/compiler/codegen/gc-types:make-stream-type)))
      (ok (typep stream-type 'clysm/compiler/codegen/gc-types:wasm-struct-type))
      (ok (= (clysm/compiler/codegen/gc-types:gc-type-index stream-type) 19))
      (ok (eq (clysm/compiler/codegen/gc-types:wasm-struct-type-name stream-type)
              'clysm/compiler/codegen/gc-types::$stream))
      (let ((fields (clysm/compiler/codegen/gc-types:wasm-struct-type-fields stream-type)))
        (ok (= (length fields) 2))
        (ok (eq (clysm/compiler/codegen/gc-types:wasm-field-name (first fields)) 'fd))
        (ok (eq (clysm/compiler/codegen/gc-types:wasm-field-name (second fields)) 'direction))))))

;;; ============================================================
;;; Stream Condition Tests (T009-T010)
;;; ============================================================

(deftest stream-error-condition-test
  "Test stream-error condition class"
  (testing "stream-error is defined"
    (ok (subtypep 'clysm/conditions:stream-error 'clysm/conditions:error)))
  (testing "stream-error has stream slot"
    (let ((condition (make-instance 'clysm/conditions:stream-error
                                    :stream *standard-output*)))
      (ok (eq (clysm/conditions:stream-error-stream condition) *standard-output*)))))

(deftest end-of-file-condition-test
  "Test end-of-file condition class"
  (testing "end-of-file is subtype of stream-error"
    (ok (subtypep 'clysm/conditions:end-of-file 'clysm/conditions:stream-error))))

;;; ============================================================
;;; Stream Type Infrastructure Tests (T015-T018)
;;; ============================================================

(deftest direction-constants-test
  "Test stream direction constants"
  (testing "direction constants have correct values"
    (ok (= +direction-input+ 0))
    (ok (= +direction-output+ 1))
    (ok (= +direction-io+ 2))))

(deftest make-stream-test
  "Test make-stream constructor"
  (testing "creates stream with correct fd and direction"
    (let ((s (make-stream 1 +direction-output+)))
      (ok (streamp s))
      (ok (= (stream-fd s) 1))
      (ok (= (stream-direction s) +direction-output+)))))

;;; ============================================================
;;; Stream Predicate Tests (US4: T082-T084)
;;; ============================================================

(deftest streamp-test
  "Test streamp predicate"
  (testing "streamp returns t for streams"
    (ok (streamp *standard-output*))
    (ok (streamp *standard-input*))
    (ok (streamp *error-output*)))
  (testing "streamp returns nil for non-streams"
    (ok (not (streamp 42)))
    (ok (not (streamp "hello")))
    (ok (not (streamp nil)))))

(deftest input-stream-p-test
  "Test input-stream-p predicate"
  (testing "returns t for input streams"
    (ok (input-stream-p *standard-input*)))
  (testing "returns nil for output-only streams"
    (ok (not (input-stream-p *standard-output*)))
    (ok (not (input-stream-p *error-output*)))))

(deftest output-stream-p-test
  "Test output-stream-p predicate"
  (testing "returns t for output streams"
    (ok (output-stream-p *standard-output*))
    (ok (output-stream-p *error-output*)))
  (testing "returns nil for input-only streams"
    (ok (not (output-stream-p *standard-input*)))))

;;; ============================================================
;;; Standard Streams Tests (T024-T028)
;;; ============================================================

(deftest standard-streams-initialized-test
  "Test that standard streams are initialized"
  (testing "*standard-input* is initialized"
    (ok (streamp *standard-input*))
    (ok (= (stream-fd *standard-input*) 0))
    (ok (input-stream-p *standard-input*)))
  (testing "*standard-output* is initialized"
    (ok (streamp *standard-output*))
    (ok (= (stream-fd *standard-output*) 1))
    (ok (output-stream-p *standard-output*)))
  (testing "*error-output* is initialized"
    (ok (streamp *error-output*))
    (ok (= (stream-fd *error-output*) 2))
    (ok (output-stream-p *error-output*))))

;;; ============================================================
;;; Write Function Tests (US1: T029-T034)
;;; ============================================================

(deftest write-char-type-check-test
  "Test write-char type checking"
  (testing "signals type-error for non-character"
    (ok (signals (clysm/conditions:type-error)
          (write-char 42)))))

(deftest write-string-type-check-test
  "Test write-string type checking"
  (testing "signals type-error for non-string"
    (ok (signals (clysm/conditions:type-error)
          (write-string 42)))))

;;; ============================================================
;;; Read Function Tests (US2: T042-T047)
;;; ============================================================

(deftest read-char-stream-check-test
  "Test read-char validates input stream"
  (testing "signals error for output-only stream"
    (ok (signals (clysm/conditions:type-error)
          (read-char *standard-output*)))))

(deftest read-line-stream-check-test
  "Test read-line validates input stream"
  (testing "signals error for output-only stream"
    (ok (signals (clysm/conditions:type-error)
          (read-line *standard-output*)))))

;;; ============================================================
;;; Format Tests (US3: T055-T063)
;;; ============================================================

(deftest format-nil-destination-test
  "Test format with nil destination returns string"
  (testing "format to nil returns string"
    (ok (stringp (format nil "hello")))
    (ok (equal (format nil "hello") "hello"))))

(deftest format-aesthetic-test
  "Test format ~A directive"
  (testing "~A formats string without quotes"
    (ok (equal (format nil "~A" "test") "test")))
  (testing "~A formats number"
    (ok (equal (format nil "~A" 42) "42"))))

(deftest format-standard-test
  "Test format ~S directive"
  (testing "~S formats string with quotes"
    (ok (equal (format nil "~S" "test") "\"test\""))))

(deftest format-decimal-test
  "Test format ~D directive"
  (testing "~D formats integer"
    (ok (equal (format nil "~D" 42) "42")))
  (testing "~D signals error for non-integer"
    (ok (signals (clysm/conditions:type-error)
          (format nil "~D" "not a number")))))

(deftest format-newline-test
  "Test format ~% directive"
  (testing "~% outputs newline"
    (ok (equal (format nil "~%") (string #\Newline)))))

(deftest format-tilde-test
  "Test format ~~ directive"
  (testing "~~ outputs single tilde"
    (ok (equal (format nil "~~") "~"))))

(deftest format-multiple-directives-test
  "Test format with multiple directives"
  (testing "combines directives correctly"
    (ok (equal (format nil "Hello ~A!~%" "World")
               (concatenate 'string "Hello World!" (string #\Newline))))))
