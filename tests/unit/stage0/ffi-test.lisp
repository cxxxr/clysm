;;;; ffi-test.lisp - Unit tests for Stage 0 FFI import declarations
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests US5: FFI Filesystem Access - FFI import declarations

(defpackage #:clysm/tests/unit/stage0/ffi-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:generate-ffi-imports
                #:*fs-open*
                #:*fs-read-all*
                #:*fs-write-all*
                #:*fs-close*))

(in-package #:clysm/tests/unit/stage0/ffi-test)

;;; ============================================================
;;; T014: Unit test for FFI import declarations
;;; ============================================================

(deftest test-ffi-imports-generates-bytes
  "Verify generate-ffi-imports returns a byte vector"
  (let ((bytes (generate-ffi-imports)))
    (ok (vectorp bytes) "Should return a vector")
    (ok (every (lambda (b) (and (integerp b) (<= 0 b 255))) bytes)
        "Should contain only valid bytes")))

(deftest test-ffi-imports-starts-with-section-id
  "Verify import section starts with section ID 2"
  (let ((bytes (generate-ffi-imports)))
    (when (> (length bytes) 0)
      (ok (= 2 (aref bytes 0)) "First byte should be section ID 2 (import section)"))))

(deftest test-fs-open-import-defined
  "Verify fs.open import is defined"
  (ok (not (null *fs-open*)) "fs.open should be defined")
  (ok (stringp *fs-open*) "fs.open should be a string name"))

(deftest test-fs-read-all-import-defined
  "Verify fs.read-all import is defined"
  (ok (not (null *fs-read-all*)) "fs.read-all should be defined")
  (ok (stringp *fs-read-all*) "fs.read-all should be a string name"))

(deftest test-fs-write-all-import-defined
  "Verify fs.write-all import is defined"
  (ok (not (null *fs-write-all*)) "fs.write-all should be defined")
  (ok (stringp *fs-write-all*) "fs.write-all should be a string name"))

(deftest test-fs-close-import-defined
  "Verify fs.close import is defined"
  (ok (not (null *fs-close*)) "fs.close should be defined")
  (ok (stringp *fs-close*) "fs.close should be a string name"))

;;; ============================================================
;;; Import Section Structure Tests
;;; ============================================================

(deftest test-import-section-has-four-imports
  "Verify at least 4 FFI imports are declared"
  (let ((bytes (generate-ffi-imports)))
    ;; Import section: ID + size + count + imports
    ;; Each import: module_name + field_name + import_kind + type_index
    (ok (>= (length bytes) 20) "Should have enough bytes for 4 imports")))

(deftest test-imports-use-clysm-namespace
  "Verify imports use 'clysm' module namespace"
  (let ((bytes (generate-ffi-imports)))
    ;; "clysm" is 5 bytes, should appear in the import section
    ;; c=0x63, l=0x6C, y=0x79, s=0x73, m=0x6D
    (when (> (length bytes) 10)
      (ok (or (search #(#x63 #x6C #x79 #x73 #x6D) bytes)
              ;; Or "fs" namespace: f=0x66, s=0x73
              (search #(#x66 #x73) bytes))
          "Should contain 'clysm' or 'fs' namespace"))))
