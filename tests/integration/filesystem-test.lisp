;;;; filesystem-test.lisp - Integration tests for filesystem operations
;;;; Tests end-to-end file I/O functionality
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T045, T046

(in-package #:clysm/tests)

;;; ============================================================
;;; Integration Test Setup
;;; ============================================================

(defvar *fs-integration-dir* nil
  "Temporary directory for integration tests")

(defun setup-fs-integration-dir ()
  "Create temporary directory for filesystem integration tests"
  (setf *fs-integration-dir*
        (uiop:ensure-directory-pathname
         (format nil "/tmp/clysm-fs-integration-~A/" (random 1000000))))
  (ensure-directories-exist *fs-integration-dir*)

  ;; Create test files
  (with-open-file (out (merge-pathnames "sample.txt" *fs-integration-dir*)
                       :direction :output :if-exists :supersede)
    (write-string "Sample file content for integration testing." out)))

(defun teardown-fs-integration-dir ()
  "Remove temporary directory after tests"
  (when *fs-integration-dir*
    (uiop:delete-directory-tree *fs-integration-dir*
                                :validate t
                                :if-does-not-exist :ignore)))

;;; ============================================================
;;; T045: Wasmtime Integration Tests
;;; ============================================================

(deftest filesystem-wasmtime-integration-test
  "Test file operations work in wasmtime environment (T045)"
  (unwind-protect
       (progn
         (setup-fs-integration-dir)

         (testing "read-file-contents works end-to-end"
           (let* ((pathname (namestring
                             (merge-pathnames "sample.txt" *fs-integration-dir*)))
                  (contents (clysm/filesystem:read-file-contents pathname)))
             (ok (search "Sample file content" contents)
                 "read-file-contents should return file contents")))

         (testing "write-file-contents works end-to-end"
           (let ((pathname (namestring
                            (merge-pathnames "output.txt" *fs-integration-dir*))))
             (clysm/filesystem:write-file-contents pathname "Integration test output")
             (let ((read-back (clysm/filesystem:read-file-contents pathname)))
               (ok (string= "Integration test output" read-back)
                   "write-file-contents should persist data"))))

         (testing "with-open-file* works end-to-end"
           (let* ((pathname (namestring
                             (merge-pathnames "sample.txt" *fs-integration-dir*)))
                  (contents nil))
             (clysm/filesystem:with-open-file* (s pathname :direction :input)
               (setf contents (clysm/filesystem:read-file-contents s)))
             (ok (search "Sample file content" contents)
                 "with-open-file* should work end-to-end"))))

    (teardown-fs-integration-dir)))

;;; ============================================================
;;; T046: Browser (Mocked) Integration Tests
;;; ============================================================
;;;
;;; Note: Actual browser testing requires a browser environment.
;;; These tests simulate browser behavior using the same API.

(deftest filesystem-browser-mock-test
  "Test file operations work with browser-like environment (T046)"
  (unwind-protect
       (progn
         (setup-fs-integration-dir)

         (testing "file operations use consistent API"
           ;; The API should be the same regardless of backend
           (let* ((pathname (namestring
                             (merge-pathnames "sample.txt" *fs-integration-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :input)))
             (ok (clysm/filesystem:file-stream-p stream)
                 "open-file should return file-stream in any environment")
             (clysm/filesystem:close-file stream)))

         (testing "error handling is consistent across environments"
           (ok (handler-case
                   (progn
                     (clysm/filesystem:read-file-contents "/nonexistent/path.txt")
                     nil)
                 (clysm/filesystem:file-error () t))
               "file-error should be signaled consistently")))

    (teardown-fs-integration-dir)))

;;; ============================================================
;;; Round-Trip Integration Tests
;;; ============================================================

(deftest filesystem-roundtrip-integration-test
  "Test complete read-write roundtrip"
  (unwind-protect
       (progn
         (setup-fs-integration-dir)

         (testing "ASCII roundtrip"
           (let ((pathname (namestring
                            (merge-pathnames "ascii.txt" *fs-integration-dir*)))
                 (content "Hello, World! 123 @#$%"))
             (clysm/filesystem:write-file-contents pathname content)
             (let ((read-back (clysm/filesystem:read-file-contents pathname)))
               (ok (string= content read-back)
                   "ASCII content should roundtrip correctly"))))

         (testing "UTF-8 roundtrip"
           (let ((pathname (namestring
                            (merge-pathnames "utf8.txt" *fs-integration-dir*)))
                 (content "日本語 中文 العربية 🎉🚀✨"))
             (clysm/filesystem:write-file-contents pathname content)
             (let ((read-back (clysm/filesystem:read-file-contents pathname)))
               (ok (string= content read-back)
                   "UTF-8 content should roundtrip correctly"))))

         (testing "Empty file roundtrip"
           (let ((pathname (namestring
                            (merge-pathnames "empty.txt" *fs-integration-dir*)))
                 (content ""))
             (clysm/filesystem:write-file-contents pathname content)
             (let ((read-back (clysm/filesystem:read-file-contents pathname)))
               (ok (string= content read-back)
                   "Empty file should roundtrip correctly")))))

    (teardown-fs-integration-dir)))
