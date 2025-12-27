;;;; write-contents-test.lisp - Unit tests for write-file-contents function
;;;; TDD: These tests define expected behavior BEFORE implementation
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T019, T020, T021, T022

(in-package #:clysm/tests)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar *write-test-dir* nil
  "Temporary directory for write tests")

(defun setup-write-test-dir ()
  "Create test directory for write-file-contents tests"
  (setf *write-test-dir* (uiop:ensure-directory-pathname
                          (format nil "/tmp/clysm-write-test-~A/"
                                  (random 1000000))))
  (ensure-directories-exist *write-test-dir*))

(defun teardown-write-test-dir ()
  "Remove test directory after tests"
  (when *write-test-dir*
    (uiop:delete-directory-tree *write-test-dir* :validate t :if-does-not-exist :ignore)))

(defun read-test-file (filename)
  "Read a test file contents for verification"
  (let ((pathname (merge-pathnames filename *write-test-dir*)))
    (with-open-file (stream pathname :direction :input :external-format :utf-8)
      (with-output-to-string (out)
        (loop for char = (read-char stream nil nil)
              while char
              do (write-char char out))))))

;;; ============================================================
;;; T019: write-file-contents with pathname (string)
;;; ============================================================

(deftest write-file-contents-pathname-test
  "Test write-file-contents with pathname string (T019, FR-004)"
  (unwind-protect
       (progn
         (setup-write-test-dir)

         (testing "write simple text to new file"
           (let ((pathname (namestring (merge-pathnames "new.txt" *write-test-dir*))))
             (clysm/filesystem:write-file-contents pathname "Hello, World!")
             (let ((contents (read-test-file "new.txt")))
               (ok (string= "Hello, World!" contents)
                   "write-file-contents should create file with correct contents"))))

         (testing "write empty string to file"
           (let ((pathname (namestring (merge-pathnames "empty.txt" *write-test-dir*))))
             (clysm/filesystem:write-file-contents pathname "")
             (let ((contents (read-test-file "empty.txt")))
               (ok (string= "" contents)
                   "write-file-contents should create empty file"))))

         (testing "overwrite existing file"
           (let ((pathname (namestring (merge-pathnames "overwrite.txt" *write-test-dir*))))
             ;; Write initial content
             (clysm/filesystem:write-file-contents pathname "Initial")
             ;; Overwrite with new content
             (clysm/filesystem:write-file-contents pathname "New Content")
             (let ((contents (read-test-file "overwrite.txt")))
               (ok (string= "New Content" contents)
                   "write-file-contents should overwrite existing file"))))

         (testing "write multiline content"
           (let ((pathname (namestring (merge-pathnames "multiline.txt" *write-test-dir*))))
             (clysm/filesystem:write-file-contents pathname "Line 1
Line 2
Line 3")
             (let ((contents (read-test-file "multiline.txt")))
               (ok (search "Line 1" contents)
                   "multiline content should contain Line 1")
               (ok (search "Line 2" contents)
                   "multiline content should contain Line 2")))))

    (teardown-write-test-dir)))

;;; ============================================================
;;; T020: write-file-contents with file-stream
;;; ============================================================

(deftest write-file-contents-stream-test
  "Test write-file-contents with file-stream (T020, FR-004)"
  (unwind-protect
       (progn
         (setup-write-test-dir)

         (testing "write to open output stream"
           (let* ((pathname (namestring (merge-pathnames "stream-write.txt" *write-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :output)))
             (unwind-protect
                  (clysm/filesystem:write-file-contents stream "Stream Content")
               (clysm/filesystem:close-file stream))
             (let ((contents (read-test-file "stream-write.txt")))
               (ok (string= "Stream Content" contents)
                   "write-file-contents with stream should write correctly"))))

         (testing "write to closed stream signals error"
           (let* ((pathname (namestring (merge-pathnames "closed-stream.txt" *write-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :output)))
             (clysm/filesystem:close-file stream)
             (ok (handler-case
                     (progn
                       (clysm/filesystem:write-file-contents stream "test")
                       nil)  ; Should not reach here
                   (clysm/filesystem:file-error () t))
                 "writing to closed stream should signal file-error"))))

    (teardown-write-test-dir)))

;;; ============================================================
;;; T021: write-file-contents UTF-8 encoding
;;; ============================================================

(deftest write-file-contents-utf8-test
  "Test write-file-contents UTF-8 encoding (T021, FR-010)"
  (unwind-protect
       (progn
         (setup-write-test-dir)

         (testing "write Japanese characters"
           (let ((pathname (namestring (merge-pathnames "japanese.txt" *write-test-dir*))))
             (clysm/filesystem:write-file-contents pathname "日本語テスト")
             (let ((contents (read-test-file "japanese.txt")))
               (ok (string= "日本語テスト" contents)
                   "UTF-8 Japanese characters should round-trip correctly"))))

         (testing "write emoji"
           (let ((pathname (namestring (merge-pathnames "emoji.txt" *write-test-dir*))))
             (clysm/filesystem:write-file-contents pathname "🎉✨🚀")
             (let ((contents (read-test-file "emoji.txt")))
               (ok (string= "🎉✨🚀" contents)
                   "UTF-8 emoji should round-trip correctly"))))

         (testing "write mixed scripts"
           (let ((pathname (namestring (merge-pathnames "mixed.txt" *write-test-dir*))))
             (clysm/filesystem:write-file-contents pathname "Hello 世界 🌍")
             (let ((contents (read-test-file "mixed.txt")))
               (ok (string= "Hello 世界 🌍" contents)
                   "UTF-8 mixed scripts should round-trip correctly")))))

    (teardown-write-test-dir)))

;;; ============================================================
;;; T022: write-file-contents directory-not-found error
;;; ============================================================

(deftest write-file-contents-dir-not-found-test
  "Test write-file-contents with non-existent directory (T022, FR-006)"
  (testing "non-existent parent directory signals file-error"
    (let ((result (handler-case
                      (progn
                        (clysm/filesystem:write-file-contents
                         "/nonexistent/directory/path/file.txt"
                         "content")
                        nil)  ; Should not reach here
                    (clysm/filesystem:file-error (c)
                      (clysm/filesystem:clysm-file-error-pathname c)))))
      (ok result
          "non-existent directory should signal file-error")
      (ok (stringp result)
          "file-error should contain pathname")))

  (testing "deeply nested non-existent path signals file-error"
    (ok (handler-case
            (progn
              (clysm/filesystem:write-file-contents
               "/a/b/c/d/e/f/g/h/file.txt"
               "content")
              nil)
          (clysm/filesystem:file-error () t))
        "deeply nested non-existent path should signal file-error")))

;;; ============================================================
;;; Round-Trip Tests
;;; ============================================================

(deftest write-read-roundtrip-test
  "Test that written content can be read back correctly"
  (unwind-protect
       (progn
         (setup-write-test-dir)

         (testing "simple roundtrip"
           (let ((pathname (namestring (merge-pathnames "roundtrip.txt" *write-test-dir*)))
                 (original "Round trip test content"))
             (clysm/filesystem:write-file-contents pathname original)
             (let ((read-back (clysm/filesystem:read-file-contents pathname)))
               (ok (string= original read-back)
                   "read-back content should match written content"))))

         (testing "UTF-8 roundtrip"
           (let ((pathname (namestring (merge-pathnames "utf8-roundtrip.txt" *write-test-dir*)))
                 (original "日本語🎉中文العربية"))
             (clysm/filesystem:write-file-contents pathname original)
             (let ((read-back (clysm/filesystem:read-file-contents pathname)))
               (ok (string= original read-back)
                   "UTF-8 content should round-trip correctly")))))

    (teardown-write-test-dir)))
