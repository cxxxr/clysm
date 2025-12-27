;;;; stage1-modules-test.lisp - Integration tests for reading all modules
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for reading and parsing all 41 compiler source modules

(in-package #:clysm/tests/integration/stage1-modules)

;;; ==========================================================================
;;; Module Reading Integration Tests
;;; ==========================================================================

(deftest test-all-modules-exist
  "All 41 compiler modules should exist on disk."
  (let* ((root (asdf:system-source-directory :clysm))
         (paths (clysm/stage1:get-module-paths)))
    (loop for path in paths
          for full-path = (merge-pathnames path root)
          do (ok (probe-file full-path)
                 (format nil "~A exists" path)))))

(deftest test-all-modules-readable
  "All 41 compiler modules should be readable."
  (let* ((root (asdf:system-source-directory :clysm))
         (paths (clysm/stage1:get-module-paths)))
    (loop for path in paths
          for full-path = (merge-pathnames path root)
          do (ok (ignore-errors
                   (with-open-file (s full-path)
                     (read-line s nil :eof)))
                 (format nil "~A is readable" path)))))

(deftest test-all-modules-parseable
  "All 41 compiler modules should contain valid Lisp."
  (let* ((root (asdf:system-source-directory :clysm))
         (paths (clysm/stage1:get-module-paths))
         (failed nil))
    (loop for path in paths
          for full-path = (merge-pathnames path root)
          do (handler-case
                 (with-open-file (s full-path)
                   (loop for form = (read s nil :eof)
                         until (eq form :eof)))
               (error (e)
                 (push (cons path e) failed))))
    (ok (null failed)
        (if failed
            (format nil "Failed to parse: ~{~A~^, ~}"
                    (mapcar #'car failed))
            "all modules parseable"))))

(deftest test-read-all-modules-returns-source-modules
  "read-all-modules should return source-module structs."
  (let ((modules (clysm/stage1::read-all-modules)))
    (ok (listp modules) "returns a list")
    (ok (>= (length modules) 40) (format nil "returns ~D modules (>= 40)" (length modules)))
    (ok (every #'clysm/stage1::source-module-p modules)
        "all are source-module structs")))

(deftest test-read-all-modules-form-count
  "read-all-modules should read many forms."
  (let ((modules (clysm/stage1::read-all-modules)))
    (let ((total-forms
            (reduce #'+ modules
                    :key (lambda (m)
                           (length (clysm/stage1::source-module-forms m))))))
      (ok (> total-forms 800)
          (format nil "expected >800 forms, got ~D" total-forms)))))

(deftest test-read-all-modules-compilable-filtering
  "read-all-modules should mark non-compilable forms."
  (let ((modules (clysm/stage1::read-all-modules)))
    (let ((non-compilable 0)
          (compilable 0))
      (dolist (m modules)
        (dolist (f (clysm/stage1::source-module-forms m))
          (if (clysm/stage1::source-form-compilable-p f)
              (incf compilable)
              (incf non-compilable))))
      (ok (> compilable 0) "some forms are compilable")
      (ok (> non-compilable 0) "some forms are not compilable"))))

;;; ==========================================================================
;;; Module Order Tests
;;; ==========================================================================

(deftest test-module-order-dependencies
  "Module order should respect dependencies."
  (let ((paths (clysm/stage1:get-module-paths)))
    ;; leb128 must come before sections (sections uses leb128)
    (ok (< (position "src/clysm/backend/leb128.lisp" paths :test #'string=)
           (position "src/clysm/backend/sections.lisp" paths :test #'string=))
        "leb128 before sections")
    ;; tokenizer must come before parser
    (ok (< (position "src/clysm/reader/tokenizer.lisp" paths :test #'string=)
           (position "src/clysm/reader/parser.lisp" paths :test #'string=))
        "tokenizer before parser")
    ;; ast must come before compiler
    (ok (< (position "src/clysm/compiler/ast.lisp" paths :test #'string=)
           (position "src/clysm/compiler/compiler.lisp" paths :test #'string=))
        "ast before compiler")))

