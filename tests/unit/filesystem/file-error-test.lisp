;;;; file-error-test.lisp - Unit tests for file-error condition
;;;; TDD: These tests define expected behavior BEFORE implementation
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Task: T004

(in-package #:clysm/tests)

;;; ============================================================
;;; File Error Condition Class Tests
;;; ============================================================

(deftest file-error-class-exists-test
  "Test that file-error class is defined (FR-006)"
  (ok (find-class 'clysm/filesystem:file-error nil)
      "file-error class should exist"))

(deftest file-error-inherits-error-test
  "Test that file-error is a subtype of error"
  (ok (subtypep 'clysm/filesystem:file-error 'clysm/conditions:error)
      "file-error should be a subtype of error"))

(deftest file-error-inherits-condition-test
  "Test that file-error is a subtype of condition"
  (ok (subtypep 'clysm/filesystem:file-error 'clysm/conditions:condition)
      "file-error should be a subtype of condition"))

;;; ============================================================
;;; File Error Slot Tests
;;; ============================================================

(deftest file-error-pathname-slot-test
  "Test that file-error has pathname slot with accessor"
  (let ((err (make-instance 'clysm/filesystem:file-error
                            :pathname "/path/to/file.txt")))
    (ok (string= "/path/to/file.txt"
                 (clysm/filesystem:clysm-file-error-pathname err))
        "file-error-pathname accessor should return pathname")))

(deftest file-error-pathname-empty-test
  "Test file-error with empty pathname"
  (let ((err (make-instance 'clysm/filesystem:file-error
                            :pathname "")))
    (ok (string= "" (clysm/filesystem:clysm-file-error-pathname err))
        "file-error should accept empty pathname")))

(deftest file-error-unicode-pathname-test
  "Test file-error with Unicode pathname"
  (let ((err (make-instance 'clysm/filesystem:file-error
                            :pathname "/日本語/ファイル.txt")))
    (ok (string= "/日本語/ファイル.txt"
                 (clysm/filesystem:clysm-file-error-pathname err))
        "file-error should preserve Unicode in pathname")))

;;; ============================================================
;;; Make-Condition Tests
;;; ============================================================

(deftest file-error-make-condition-test
  "Test creating file-error via make-condition"
  (let ((err (clysm/conditions:make-condition 'clysm/filesystem:file-error
                                               :pathname "test.txt")))
    (ok (typep err 'clysm/filesystem:file-error)
        "make-condition should create file-error instance")
    (ok (string= "test.txt" (clysm/filesystem:clysm-file-error-pathname err))
        "make-condition should set pathname slot")))

;;; ============================================================
;;; Condition System Integration Tests
;;; ============================================================

(deftest file-error-signal-test
  "Test that file-error can be signaled"
  (ok (handler-case
          (progn
            (error (make-instance 'clysm/filesystem:file-error
                                  :pathname "missing.txt"))
            nil)  ; Should not reach here
        (clysm/filesystem:file-error (c)
          (string= "missing.txt" (clysm/filesystem:clysm-file-error-pathname c))))
      "file-error should be catchable with pathname"))

(deftest file-error-handler-case-test
  "Test handler-case with file-error"
  (let ((result (handler-case
                    (error (make-instance 'clysm/filesystem:file-error
                                          :pathname "/no/such/file"))
                  (clysm/filesystem:file-error (c)
                    (format nil "Error on: ~A"
                            (clysm/filesystem:clysm-file-error-pathname c))))))
    (ok (string= "Error on: /no/such/file" result)
        "handler-case should catch file-error")))
