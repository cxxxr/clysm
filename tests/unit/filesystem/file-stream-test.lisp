;;;; file-stream-test.lisp - Unit tests for file-stream struct
;;;; TDD: These tests define expected behavior BEFORE implementation
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Task: T005

(in-package #:clysm/tests)

;;; ============================================================
;;; File Stream Struct Existence Tests
;;; ============================================================

(deftest file-stream-struct-exists-test
  "Test that file-stream struct is defined"
  (ok (fboundp 'clysm/filesystem:make-file-stream)
      "make-file-stream constructor should exist")
  (ok (fboundp 'clysm/filesystem:file-stream-p)
      "file-stream-p predicate should exist"))

;;; ============================================================
;;; File Stream Constructor Tests
;;; ============================================================

(deftest file-stream-constructor-test
  "Test file-stream constructor with all slots"
  (let ((stream (clysm/filesystem:make-file-stream
                 :handle :mock-handle
                 :direction :input
                 :pathname "test.txt"
                 :open-p t)))
    (ok (clysm/filesystem:file-stream-p stream)
        "make-file-stream should create file-stream instance")))

(deftest file-stream-default-open-p-test
  "Test file-stream default open-p is t"
  (let ((stream (clysm/filesystem:make-file-stream
                 :handle nil
                 :direction :input
                 :pathname "")))
    (ok (eq t (clysm/filesystem:file-stream-open-p stream))
        "file-stream open-p should default to t")))

;;; ============================================================
;;; File Stream Accessor Tests
;;; ============================================================

(deftest file-stream-handle-accessor-test
  "Test file-stream-handle accessor"
  (let ((stream (clysm/filesystem:make-file-stream
                 :handle :test-handle
                 :direction :input
                 :pathname "file.txt")))
    (ok (eq :test-handle (clysm/filesystem:file-stream-handle stream))
        "file-stream-handle should return handle")))

(deftest file-stream-direction-accessor-test
  "Test file-stream-direction accessor"
  (let ((input-stream (clysm/filesystem:make-file-stream
                       :handle nil
                       :direction :input
                       :pathname "input.txt"))
        (output-stream (clysm/filesystem:make-file-stream
                        :handle nil
                        :direction :output
                        :pathname "output.txt")))
    (ok (eq :input (clysm/filesystem:file-stream-direction input-stream))
        "file-stream-direction should return :input")
    (ok (eq :output (clysm/filesystem:file-stream-direction output-stream))
        "file-stream-direction should return :output")))

(deftest file-stream-pathname-accessor-test
  "Test file-stream-pathname accessor"
  (let ((stream (clysm/filesystem:make-file-stream
                 :handle nil
                 :direction :input
                 :pathname "/path/to/file.txt")))
    (ok (string= "/path/to/file.txt"
                 (clysm/filesystem:file-stream-pathname stream))
        "file-stream-pathname should return pathname")))

(deftest file-stream-open-p-accessor-test
  "Test file-stream-open-p accessor"
  (let ((open-stream (clysm/filesystem:make-file-stream
                      :handle nil
                      :direction :input
                      :pathname "open.txt"
                      :open-p t))
        (closed-stream (clysm/filesystem:make-file-stream
                        :handle nil
                        :direction :input
                        :pathname "closed.txt"
                        :open-p nil)))
    (ok (eq t (clysm/filesystem:file-stream-open-p open-stream))
        "file-stream-open-p should return t for open stream")
    (ok (eq nil (clysm/filesystem:file-stream-open-p closed-stream))
        "file-stream-open-p should return nil for closed stream")))

;;; ============================================================
;;; File Stream Unicode Tests
;;; ============================================================

(deftest file-stream-unicode-pathname-test
  "Test file-stream with Unicode pathname"
  (let ((stream (clysm/filesystem:make-file-stream
                 :handle nil
                 :direction :output
                 :pathname "/データ/日本語.txt")))
    (ok (string= "/データ/日本語.txt"
                 (clysm/filesystem:file-stream-pathname stream))
        "file-stream should preserve Unicode pathname")))

;;; ============================================================
;;; File Stream Type Validation Tests
;;; ============================================================

(deftest file-stream-predicate-positive-test
  "Test file-stream-p returns t for file-stream"
  (let ((stream (clysm/filesystem:make-file-stream
                 :handle nil
                 :direction :input
                 :pathname "test.txt")))
    (ok (eq t (clysm/filesystem:file-stream-p stream))
        "file-stream-p should return t for file-stream")))

(deftest file-stream-predicate-negative-test
  "Test file-stream-p returns nil for non-file-stream"
  (ok (eq nil (clysm/filesystem:file-stream-p "not a stream"))
      "file-stream-p should return nil for string")
  (ok (eq nil (clysm/filesystem:file-stream-p 42))
      "file-stream-p should return nil for integer")
  (ok (eq nil (clysm/filesystem:file-stream-p nil))
      "file-stream-p should return nil for nil")
  (ok (eq nil (clysm/filesystem:file-stream-p '(:handle nil :direction :input)))
      "file-stream-p should return nil for list"))
