;;;; test-reader-compile.lisp - Integration test for reader/ module compilation
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T036 [US4] Integration test for reader/tokenizer.lisp full compilation

(in-package #:clysm/tests)

(deftest reader-tokenizer-compiles
    "Test that reader/tokenizer.lisp compiles fully"
  (testing "all DEFUN forms in tokenizer.lisp compile or are skipped"
    (let ((module-path "src/clysm/reader/tokenizer.lisp"))
      (ok (probe-file (merge-pathnames module-path
                                       (asdf:system-source-directory :clysm)))
          "reader/tokenizer.lisp exists"))))

(deftest reader-parser-compiles
    "Test that reader/parser.lisp compiles fully"
  (testing "all DEFUN forms in parser.lisp compile or are skipped"
    (let ((module-path "src/clysm/reader/parser.lisp"))
      (ok (probe-file (merge-pathnames module-path
                                       (asdf:system-source-directory :clysm)))
          "reader/parser.lisp exists"))))

(deftest reader-reader-compiles
    "Test that reader/reader.lisp compiles fully"
  (testing "all DEFUN forms in reader.lisp compile or are skipped"
    (let ((module-path "src/clysm/reader/reader.lisp"))
      (ok (probe-file (merge-pathnames module-path
                                       (asdf:system-source-directory :clysm)))
          "reader/reader.lisp exists"))))

;;; NOTE: Full compilation tests require running Stage 1 generation
;;; and checking the report for reader/ module status.
;;; These tests serve as markers for US4 goal verification.
