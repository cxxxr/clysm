;;;; with-open-file-test.lisp - Unit tests for with-open-file* macro
;;;; TDD: These tests define expected behavior BEFORE implementation
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Tasks: T038, T039, T040, T041

(in-package #:clysm/tests)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar *wof-test-dir* nil
  "Temporary directory for with-open-file tests")

(defun setup-wof-test-dir ()
  "Create test directory and files for with-open-file tests"
  (setf *wof-test-dir* (uiop:ensure-directory-pathname
                        (format nil "/tmp/clysm-wof-test-~A/"
                                (random 1000000))))
  (ensure-directories-exist *wof-test-dir*)

  ;; Create test file with content
  (with-open-file (out (merge-pathnames "test-input.txt" *wof-test-dir*)
                       :direction :output :if-exists :supersede)
    (write-string "Test file content" out)))

(defun teardown-wof-test-dir ()
  "Remove test directory after tests"
  (when *wof-test-dir*
    (uiop:delete-directory-tree *wof-test-dir* :validate t :if-does-not-exist :ignore)))

;;; ============================================================
;;; T038: with-open-file normal completion
;;; ============================================================

(deftest with-open-file-normal-completion-test
  "Test with-open-file* normal completion (T038, FR-005)"
  (unwind-protect
       (progn
         (setup-wof-test-dir)

         (testing "stream is bound during body execution"
           (let ((pathname (namestring (merge-pathnames "test-input.txt" *wof-test-dir*)))
                 (stream-was-open nil))
             (clysm/filesystem:with-open-file* (s pathname :direction :input)
               (setf stream-was-open (clysm/filesystem:file-stream-open-p s)))
             (ok stream-was-open
                 "stream should be open during body execution")))

         (testing "stream is closed after normal body completion"
           (let* ((pathname (namestring (merge-pathnames "test-input.txt" *wof-test-dir*)))
                  (captured-stream nil))
             (clysm/filesystem:with-open-file* (s pathname :direction :input)
               (setf captured-stream s))
             (ok (not (clysm/filesystem:file-stream-open-p captured-stream))
                 "stream should be closed after with-open-file* exits")))

         (testing "body result is returned"
           (let* ((pathname (namestring (merge-pathnames "test-input.txt" *wof-test-dir*)))
                  (result (clysm/filesystem:with-open-file* (s pathname :direction :input)
                            (clysm/filesystem:read-file-contents s))))
             (ok (string= "Test file content" result)
                 "with-open-file* should return body result"))))

    (teardown-wof-test-dir)))

;;; ============================================================
;;; T039: with-open-file error propagation with cleanup
;;; ============================================================

(deftest with-open-file-error-cleanup-test
  "Test with-open-file* closes stream even when error occurs (T039, FR-005)"
  (unwind-protect
       (progn
         (setup-wof-test-dir)

         (testing "stream is closed when error occurs in body"
           (let* ((pathname (namestring (merge-pathnames "test-input.txt" *wof-test-dir*)))
                  (captured-stream nil))
             (handler-case
                 (clysm/filesystem:with-open-file* (s pathname :direction :input)
                   (setf captured-stream s)
                   (cl:error "Test error"))
               (error () nil))  ; Ignore the error
             (ok (not (clysm/filesystem:file-stream-open-p captured-stream))
                 "stream should be closed even when error occurs")))

         (testing "error is propagated after cleanup"
           (let ((pathname (namestring (merge-pathnames "test-input.txt" *wof-test-dir*)))
                 (error-propagated nil))
             (handler-case
                 (clysm/filesystem:with-open-file* (s pathname :direction :input)
                   (cl:error "Propagated error"))
               (error (e)
                 (setf error-propagated t)))
             (ok error-propagated
                 "error should propagate after stream cleanup"))))

    (teardown-wof-test-dir)))

;;; ============================================================
;;; T040: with-open-file input direction
;;; ============================================================

(deftest with-open-file-input-test
  "Test with-open-file* with :direction :input (T040)"
  (unwind-protect
       (progn
         (setup-wof-test-dir)

         (testing ":direction :input opens for reading"
           (let* ((pathname (namestring (merge-pathnames "test-input.txt" *wof-test-dir*)))
                  (direction nil))
             (clysm/filesystem:with-open-file* (s pathname :direction :input)
               (setf direction (clysm/filesystem:file-stream-direction s)))
             (ok (eq :input direction)
                 ":direction :input should set stream direction to :input")))

         (testing "can read from input stream"
           (let* ((pathname (namestring (merge-pathnames "test-input.txt" *wof-test-dir*)))
                  (contents nil))
             (clysm/filesystem:with-open-file* (s pathname :direction :input)
               (setf contents (clysm/filesystem:read-file-contents s)))
             (ok (stringp contents)
                 "should be able to read from input stream"))))

    (teardown-wof-test-dir)))

;;; ============================================================
;;; T041: with-open-file output direction
;;; ============================================================

(deftest with-open-file-output-test
  "Test with-open-file* with :direction :output (T041)"
  (unwind-protect
       (progn
         (setup-wof-test-dir)

         (testing ":direction :output opens for writing"
           (let* ((pathname (namestring (merge-pathnames "output-test.txt" *wof-test-dir*)))
                  (direction nil))
             (clysm/filesystem:with-open-file* (s pathname :direction :output)
               (setf direction (clysm/filesystem:file-stream-direction s)))
             (ok (eq :output direction)
                 ":direction :output should set stream direction to :output")))

         (testing "can write to output stream"
           (let ((pathname (namestring (merge-pathnames "write-test.txt" *wof-test-dir*))))
             (clysm/filesystem:with-open-file* (s pathname :direction :output)
               (clysm/filesystem:write-file-contents s "Written content"))
             ;; Verify written content
             (let ((contents (clysm/filesystem:read-file-contents pathname)))
               (ok (string= "Written content" contents)
                   "should be able to write to output stream"))))

         (testing ":if-exists :supersede overwrites file"
           (let ((pathname (namestring (merge-pathnames "overwrite-test.txt" *wof-test-dir*))))
             ;; Write initial content
             (clysm/filesystem:with-open-file* (s pathname :direction :output)
               (clysm/filesystem:write-file-contents s "Initial"))
             ;; Overwrite with new content
             (clysm/filesystem:with-open-file* (s pathname :direction :output
                                                   :if-exists :supersede)
               (clysm/filesystem:write-file-contents s "New"))
             (let ((contents (clysm/filesystem:read-file-contents pathname)))
               (ok (string= "New" contents)
                   ":if-exists :supersede should overwrite file")))))

    (teardown-wof-test-dir)))

;;; ============================================================
;;; Additional Tests
;;; ============================================================

(deftest with-open-file-keyword-args-test
  "Test with-open-file* keyword argument handling"
  (unwind-protect
       (progn
         (setup-wof-test-dir)

         (testing ":if-exists :error signals error for existing file"
           (let ((pathname (namestring (merge-pathnames "test-input.txt" *wof-test-dir*))))
             (ok (handler-case
                     (progn
                       (clysm/filesystem:with-open-file* (s pathname
                                                            :direction :output
                                                            :if-exists :error)
                         nil)
                       nil)  ; Should not reach here
                   (clysm/filesystem:file-error () t))
                 ":if-exists :error should signal file-error for existing file")))

         (testing ":if-does-not-exist :error signals error for missing file"
           (ok (handler-case
                   (progn
                     (clysm/filesystem:with-open-file* (s "/nonexistent-file.txt"
                                                          :direction :input
                                                          :if-does-not-exist :error)
                       nil)
                     nil)  ; Should not reach here
                 (clysm/filesystem:file-error () t))
               ":if-does-not-exist :error should signal file-error for missing file")))

    (teardown-wof-test-dir)))
