;;;; platform-test.lisp - Unit tests for platform abstraction
;;;;
;;;; Part of Feature 041: Development Workflow Establishment
;;;; Tests glob expansion, file timestamps, and file reading

(defpackage #:clysm/tests/unit/workflow/platform-test
  (:use #:cl #:rove #:clysm/workflow))

(in-package #:clysm/tests/unit/workflow/platform-test)

;;; ============================================================
;;; T018: Unit tests for platform abstraction
;;; ============================================================

;;; Helper to get project root (tests are run from project root)
(defun project-root ()
  "Return the project root directory."
  (uiop:getcwd))

(deftest file-exists-p-test
  "Test file-exists-p function."
  (testing "existing file returns T"
    (ok (file-exists-p (uiop:merge-pathnames* "clysm.asd" (project-root)))))
  (testing "non-existent file returns NIL"
    (ok (not (file-exists-p "/nonexistent/file/that/does/not/exist.lisp"))))
  (testing "directory returns NIL (not a file)"
    (ok (not (file-exists-p (uiop:merge-pathnames* "src/" (project-root)))))))

(deftest file-mtime-test
  "Test file-mtime function."
  (let* ((test-file (uiop:merge-pathnames* "clysm.asd" (project-root)))
         (mtime (file-mtime test-file)))
    (testing "returns positive integer for existing file"
      (ok (and (integerp mtime) (> mtime 0))))
    (testing "returns reasonable Unix timestamp (after year 2020)"
      (ok (> mtime 1577836800))) ; 2020-01-01 00:00:00 UTC
    (testing "returns 0 for non-existent file"
      (ok (= (file-mtime "/nonexistent/file.lisp") 0)))))

(deftest read-file-string-test
  "Test read-file-string function."
  (let* ((test-file (uiop:merge-pathnames* "clysm.asd" (project-root)))
         (contents (read-file-string test-file)))
    (testing "returns a string"
      (ok (stringp contents)))
    (testing "string is non-empty"
      (ok (> (length contents) 0)))
    (testing "contains expected content (defsystem)"
      (ok (search "defsystem" contents)))))

(deftest ensure-directory-test
  "Test ensure-directory function."
  (let* ((temp-dir (uiop:temporary-directory))
         (test-path (uiop:merge-pathnames* "clysm-test-dir/subdir/file.txt" temp-dir)))
    (testing "creates parent directories"
      (let ((dir (ensure-directory test-path)))
        (ok (uiop:directory-exists-p dir))
        ;; Clean up
        (uiop:delete-empty-directory
         (uiop:merge-pathnames* "clysm-test-dir/subdir/" temp-dir))
        (uiop:delete-empty-directory
         (uiop:merge-pathnames* "clysm-test-dir/" temp-dir))))))

(deftest relative-pathname-test
  "Test relative-pathname function."
  (let ((base (project-root)))
    (testing "returns relative path for file under base"
      (let ((abs-path (uiop:merge-pathnames* "src/clysm/package.lisp" base)))
        (ok (equal (relative-pathname abs-path base) "src/clysm/package.lisp"))))
    (testing "returns absolute path for file not under base"
      (let ((outside-path "/etc/passwd"))
        (when (uiop:file-exists-p outside-path)
          (ok (char= #\/ (char (relative-pathname outside-path base) 0))))))))

(deftest absolute-pathname-test
  "Test absolute-pathname function."
  (let ((base (project-root)))
    (testing "relative path is resolved"
      (let ((abs (absolute-pathname "src/clysm/package.lisp" base)))
        (ok (char= #\/ (char abs 0)))))
    (testing "absolute path is unchanged"
      (let ((path "/usr/bin/ls"))
        (ok (equal (absolute-pathname path base) path))))))

;;; ============================================================
;;; Glob expansion tests
;;; ============================================================

(deftest glob-expand-single-file-test
  "Test glob-expand with single file (no wildcards)."
  (let ((base (project-root)))
    (testing "existing file returns single-element list"
      (let ((result (glob-expand "clysm.asd" base)))
        (ok (= (length result) 1))
        (ok (search "clysm.asd" (car result)))))
    (testing "non-existent file returns empty list"
      (let ((result (glob-expand "nonexistent-file-12345.lisp" base)))
        (ok (null result))))))

(deftest glob-expand-simple-wildcard-test
  "Test glob-expand with simple wildcard patterns."
  (let ((base (project-root)))
    (testing "*.asd returns asd files"
      (let ((result (glob-expand "*.asd" base)))
        (ok (>= (length result) 1))
        (ok (every (lambda (p) (search ".asd" p)) result))))))

;; Note: ** recursive pattern tests are more complex and depend on directory structure.
;; Basic functionality is tested; comprehensive tests would require a known test fixture.

(deftest glob-expand-returns-sorted-test
  "Test that glob-expand returns sorted results."
  (let* ((base (project-root))
         (result (glob-expand "*.asd" base)))
    (when (> (length result) 1)
      (testing "results are sorted alphabetically"
        (ok (equal result (sort (copy-list result) #'string<)))))))
