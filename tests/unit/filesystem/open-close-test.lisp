;;;; open-close-test.lisp - Unit tests for open-file and close-file functions
;;;; TDD: These tests define expected behavior BEFORE implementation
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T027, T028, T029, T030, T031, T032

(in-package #:clysm/tests)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar *open-test-dir* nil
  "Temporary directory for open/close tests")

(defun setup-open-test-dir ()
  "Create test directory and files for open-file tests"
  (setf *open-test-dir* (uiop:ensure-directory-pathname
                         (format nil "/tmp/clysm-open-test-~A/"
                                 (random 1000000))))
  (ensure-directories-exist *open-test-dir*)

  ;; Create existing test file
  (with-open-file (out (merge-pathnames "existing.txt" *open-test-dir*)
                       :direction :output :if-exists :supersede)
    (write-string "Existing file content" out)))

(defun teardown-open-test-dir ()
  "Remove test directory after tests"
  (when *open-test-dir*
    (uiop:delete-directory-tree *open-test-dir* :validate t :if-does-not-exist :ignore)))

;;; ============================================================
;;; T027: open-file input existing file
;;; ============================================================

(deftest open-file-input-existing-test
  "Test open-file with existing input file (T027, FR-001)"
  (unwind-protect
       (progn
         (setup-open-test-dir)

         (testing "open existing file for input returns file-stream"
           (let* ((pathname (namestring (merge-pathnames "existing.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :input)))
             (unwind-protect
                  (progn
                    (ok (clysm/filesystem:file-stream-p stream)
                        "open-file should return a file-stream")
                    (ok (eq :input (clysm/filesystem:file-stream-direction stream))
                        "file-stream should have :input direction")
                    (ok (string= pathname (clysm/filesystem:file-stream-pathname stream))
                        "file-stream should store pathname")
                    (ok (clysm/filesystem:file-stream-open-p stream)
                        "file-stream should be open"))
               (clysm/filesystem:close-file stream)))))

    (teardown-open-test-dir)))

;;; ============================================================
;;; T028: open-file input not-found error
;;; ============================================================

(deftest open-file-input-not-found-test
  "Test open-file with non-existent input file (T028, FR-006)"
  (testing "non-existent input file signals file-error"
    (let ((result (handler-case
                      (progn
                        (clysm/filesystem:open-file "/nonexistent/file.txt"
                                                    :direction :input)
                        nil)  ; Should not reach here
                    (clysm/filesystem:file-error (c)
                      (clysm/filesystem:clysm-file-error-pathname c)))))
      (ok result
          "non-existent file should signal file-error")
      (ok (stringp result)
          "file-error should contain pathname")))

  (testing ":if-does-not-exist :error is default for input"
    (ok (handler-case
            (progn
              (clysm/filesystem:open-file "/nonexistent-file.txt"
                                          :direction :input)
              nil)
          (clysm/filesystem:file-error () t))
        "default :if-does-not-exist for input should be :error")))

;;; ============================================================
;;; T029: open-file output create file
;;; ============================================================

(deftest open-file-output-create-test
  "Test open-file creates output file if doesn't exist (T029, FR-001, FR-013)"
  (unwind-protect
       (progn
         (setup-open-test-dir)

         (testing "open non-existent file for output creates it"
           (let* ((pathname (namestring (merge-pathnames "new-output.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :output)))
             (unwind-protect
                  (progn
                    (ok (clysm/filesystem:file-stream-p stream)
                        "open-file should return a file-stream")
                    (ok (eq :output (clysm/filesystem:file-stream-direction stream))
                        "file-stream should have :output direction")
                    (ok (clysm/filesystem:file-stream-open-p stream)
                        "file-stream should be open"))
               (clysm/filesystem:close-file stream))
             ;; Verify file was created
             (ok (probe-file pathname)
                 "output file should be created"))))

    (teardown-open-test-dir)))

;;; ============================================================
;;; T030: open-file :if-exists :error
;;; ============================================================

(deftest open-file-if-exists-error-test
  "Test open-file :if-exists :error (T030, FR-012)"
  (unwind-protect
       (progn
         (setup-open-test-dir)

         (testing ":if-exists :error signals file-error for existing file"
           (let ((pathname (namestring (merge-pathnames "existing.txt" *open-test-dir*))))
             (ok (handler-case
                     (progn
                       (clysm/filesystem:open-file pathname
                                                   :direction :output
                                                   :if-exists :error)
                       nil)  ; Should not reach here
                   (clysm/filesystem:file-error () t))
                 ":if-exists :error should signal file-error for existing file")))

         (testing ":if-exists :supersede overwrites existing file"
           (let* ((pathname (namestring (merge-pathnames "existing.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname
                                                      :direction :output
                                                      :if-exists :supersede)))
             (unwind-protect
                  (ok (clysm/filesystem:file-stream-p stream)
                      ":if-exists :supersede should succeed")
               (clysm/filesystem:close-file stream)))))

    (teardown-open-test-dir)))

;;; ============================================================
;;; T031: close-file success
;;; ============================================================

(deftest close-file-success-test
  "Test close-file successfully closes stream (T031, FR-002)"
  (unwind-protect
       (progn
         (setup-open-test-dir)

         (testing "close-file sets open-p to nil"
           (let* ((pathname (namestring (merge-pathnames "existing.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :input)))
             (ok (clysm/filesystem:file-stream-open-p stream)
                 "stream should be open before close")
             (clysm/filesystem:close-file stream)
             (ok (not (clysm/filesystem:file-stream-open-p stream))
                 "stream should be closed after close-file")))

         (testing "close-file returns nil"
           (let* ((pathname (namestring (merge-pathnames "existing.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :input)))
             (let ((result (clysm/filesystem:close-file stream)))
               (ok (null result)
                   "close-file should return nil")))))

    (teardown-open-test-dir)))

;;; ============================================================
;;; T032: close-file already-closed error
;;; ============================================================

(deftest close-file-already-closed-test
  "Test close-file signals error on already closed stream (T032, FR-011)"
  (unwind-protect
       (progn
         (setup-open-test-dir)

         (testing "close-file on closed stream signals file-error"
           (let* ((pathname (namestring (merge-pathnames "existing.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :input)))
             (clysm/filesystem:close-file stream)
             (ok (handler-case
                     (progn
                       (clysm/filesystem:close-file stream)
                       nil)  ; Should not reach here
                   (clysm/filesystem:file-error () t))
                 "close-file on closed stream should signal file-error"))))

    (teardown-open-test-dir)))

;;; ============================================================
;;; Additional Tests
;;; ============================================================

(deftest open-file-direction-parameter-test
  "Test open-file :direction parameter (FR-007)"
  (unwind-protect
       (progn
         (setup-open-test-dir)

         (testing ":direction :input opens for reading"
           (let* ((pathname (namestring (merge-pathnames "existing.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :input)))
             (unwind-protect
                  (ok (eq :input (clysm/filesystem:file-stream-direction stream))
                      ":direction :input should set direction to :input")
               (clysm/filesystem:close-file stream))))

         (testing ":direction :output opens for writing"
           (let* ((pathname (namestring (merge-pathnames "output-dir.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname :direction :output)))
             (unwind-protect
                  (ok (eq :output (clysm/filesystem:file-stream-direction stream))
                      ":direction :output should set direction to :output")
               (clysm/filesystem:close-file stream)))))

    (teardown-open-test-dir)))

(deftest open-file-if-does-not-exist-test
  "Test open-file :if-does-not-exist parameter (FR-013)"
  (unwind-protect
       (progn
         (setup-open-test-dir)

         (testing ":if-does-not-exist :create creates file"
           (let* ((pathname (namestring (merge-pathnames "create-me.txt" *open-test-dir*)))
                  (stream (clysm/filesystem:open-file pathname
                                                      :direction :output
                                                      :if-does-not-exist :create)))
             (unwind-protect
                  (ok (probe-file pathname)
                      ":if-does-not-exist :create should create file")
               (clysm/filesystem:close-file stream))))

         (testing ":if-does-not-exist :error signals error for input"
           (ok (handler-case
                   (progn
                     (clysm/filesystem:open-file "/nonexistent.txt"
                                                 :direction :input
                                                 :if-does-not-exist :error)
                     nil)
                 (clysm/filesystem:file-error () t))
               ":if-does-not-exist :error should signal file-error")))

    (teardown-open-test-dir)))
