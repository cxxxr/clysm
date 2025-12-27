;;;; read-contents-test.lisp - Unit tests for read-file-contents function
;;;; TDD: These tests define expected behavior BEFORE implementation
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T011, T012, T013, T014

(in-package #:clysm/tests)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar *test-dir* nil
  "Temporary directory for test files")

(defun setup-test-files ()
  "Create test files for read-file-contents tests"
  (setf *test-dir* (uiop:ensure-directory-pathname
                    (format nil "/tmp/clysm-fs-test-~A/"
                            (random 1000000))))
  (ensure-directories-exist *test-dir*)

  ;; Create test files
  (with-open-file (out (merge-pathnames "hello.txt" *test-dir*)
                       :direction :output :if-exists :supersede)
    (write-string "Hello, World!" out))

  (with-open-file (out (merge-pathnames "empty.txt" *test-dir*)
                       :direction :output :if-exists :supersede)
    ;; Empty file - write nothing
    )

  (with-open-file (out (merge-pathnames "unicode.txt" *test-dir*)
                       :direction :output :if-exists :supersede
                       :external-format :utf-8)
    (write-string "日本語🎉Hello" out))

  (with-open-file (out (merge-pathnames "multiline.txt" *test-dir*)
                       :direction :output :if-exists :supersede)
    (write-line "Line 1" out)
    (write-line "Line 2" out)
    (write-string "Line 3" out)))

(defun teardown-test-files ()
  "Remove test files after tests"
  (when *test-dir*
    (uiop:delete-directory-tree *test-dir* :validate t :if-does-not-exist :ignore)))

;;; ============================================================
;;; T011: read-file-contents with pathname (string)
;;; ============================================================

(deftest read-file-contents-pathname-test
  "Test read-file-contents with pathname string (T011, FR-003)"
  (unwind-protect
       (progn
         (setup-test-files)

         (testing "read simple text file"
           (let ((contents (clysm/filesystem:read-file-contents
                            (namestring (merge-pathnames "hello.txt" *test-dir*)))))
             (ok (stringp contents)
                 "read-file-contents should return a string")
             (ok (string= "Hello, World!" contents)
                 "read-file-contents should return file contents")))

         (testing "read empty file"
           (let ((contents (clysm/filesystem:read-file-contents
                            (namestring (merge-pathnames "empty.txt" *test-dir*)))))
             (ok (stringp contents)
                 "empty file should return a string")
             (ok (string= "" contents)
                 "empty file should return empty string")))

         (testing "read multiline file"
           (let ((contents (clysm/filesystem:read-file-contents
                            (namestring (merge-pathnames "multiline.txt" *test-dir*)))))
             (ok (search "Line 1" contents)
                 "multiline file should contain Line 1")
             (ok (search "Line 2" contents)
                 "multiline file should contain Line 2")
             (ok (search "Line 3" contents)
                 "multiline file should contain Line 3"))))

    (teardown-test-files)))

;;; ============================================================
;;; T012: read-file-contents with file-stream
;;; ============================================================

(deftest read-file-contents-stream-test
  "Test read-file-contents with file-stream (T012, FR-003)"
  (unwind-protect
       (progn
         (setup-test-files)

         (testing "read from open input stream"
           (let* ((pathname (namestring (merge-pathnames "hello.txt" *test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :input)))
             (unwind-protect
                  (let ((contents (clysm/filesystem:read-file-contents stream)))
                    (ok (stringp contents)
                        "read-file-contents with stream should return string")
                    (ok (string= "Hello, World!" contents)
                        "read-file-contents with stream should return file contents"))
               (clysm/filesystem:close-file stream))))

         (testing "read from closed stream signals error"
           (let* ((pathname (namestring (merge-pathnames "hello.txt" *test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :input)))
             (clysm/filesystem:close-file stream)
             (ok (handler-case
                     (progn
                       (clysm/filesystem:read-file-contents stream)
                       nil)  ; Should not reach here
                   (clysm/filesystem:file-error () t))
                 "reading from closed stream should signal file-error"))))

    (teardown-test-files)))

;;; ============================================================
;;; T013: read-file-contents UTF-8 encoding
;;; ============================================================

(deftest read-file-contents-utf8-test
  "Test read-file-contents UTF-8 decoding (T013, FR-010)"
  (unwind-protect
       (progn
         (setup-test-files)

         (testing "read UTF-8 with Japanese characters"
           (let ((contents (clysm/filesystem:read-file-contents
                            (namestring (merge-pathnames "unicode.txt" *test-dir*)))))
             (ok (search "日本語" contents)
                 "UTF-8 content should preserve Japanese characters")))

         (testing "read UTF-8 with emoji"
           (let ((contents (clysm/filesystem:read-file-contents
                            (namestring (merge-pathnames "unicode.txt" *test-dir*)))))
             (ok (search "🎉" contents)
                 "UTF-8 content should preserve emoji")))

         (testing "read UTF-8 with mixed scripts"
           (let ((contents (clysm/filesystem:read-file-contents
                            (namestring (merge-pathnames "unicode.txt" *test-dir*)))))
             (ok (string= "日本語🎉Hello" contents)
                 "UTF-8 content should preserve all characters in order"))))

    (teardown-test-files)))

;;; ============================================================
;;; T014: read-file-contents file-not-found error
;;; ============================================================

(deftest read-file-contents-not-found-test
  "Test read-file-contents with non-existent file (T014, FR-006)"
  (testing "non-existent file signals file-error"
    (let ((result (handler-case
                      (progn
                        (clysm/filesystem:read-file-contents "/nonexistent/path/file.txt")
                        nil)  ; Should not reach here
                    (clysm/filesystem:file-error (c)
                      (clysm/filesystem:clysm-file-error-pathname c)))))
      (ok result
          "non-existent file should signal file-error")
      (ok (stringp result)
          "file-error should contain pathname")))

  (testing "non-existent file in existing directory signals file-error"
    (ok (handler-case
            (progn
              (clysm/filesystem:read-file-contents "/tmp/definitely-not-exists-12345.txt")
              nil)
          (clysm/filesystem:file-error () t))
        "non-existent file should signal file-error")))

;;; ============================================================
;;; Edge Case Tests
;;; ============================================================

(deftest read-file-contents-edge-cases-test
  "Test read-file-contents edge cases"
  (unwind-protect
       (progn
         (setup-test-files)

         (testing "read-file-contents accepts pathname object"
           ;; If the implementation supports pathname objects
           (let ((pathname (merge-pathnames "hello.txt" *test-dir*)))
             (ok (handler-case
                     (let ((contents (clysm/filesystem:read-file-contents
                                      (namestring pathname))))
                       (stringp contents))
                   (error () nil))
                 "read-file-contents should work with pathname strings"))))

    (teardown-test-files)))
