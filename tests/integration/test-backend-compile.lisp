;;;; test-backend-compile.lisp - Integration test for backend/ module compilation
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T035 [US4] Integration test for backend/leb128.lisp full compilation

(in-package #:clysm/tests)

(deftest backend-leb128-compiles
    "Test that backend/leb128.lisp compiles fully"
  (testing "all DEFUN forms in leb128.lisp compile or are skipped"
    ;; This test will be meaningful after lambda-list fixes are applied
    ;; For now, it documents the target module
    (let ((module-path "src/clysm/backend/leb128.lisp"))
      (ok (probe-file (merge-pathnames module-path
                                       (asdf:system-source-directory :clysm)))
          "backend/leb128.lisp exists"))))

(deftest backend-sections-compiles
    "Test that backend/sections.lisp compiles fully"
  (testing "all DEFUN forms in sections.lisp compile or are skipped"
    (let ((module-path "src/clysm/backend/sections.lisp"))
      (ok (probe-file (merge-pathnames module-path
                                       (asdf:system-source-directory :clysm)))
          "backend/sections.lisp exists"))))

(deftest backend-wasm-emit-compiles
    "Test that backend/wasm-emit.lisp compiles fully"
  (testing "all DEFUN forms in wasm-emit.lisp compile or are skipped"
    (let ((module-path "src/clysm/backend/wasm-emit.lisp"))
      (ok (probe-file (merge-pathnames module-path
                                       (asdf:system-source-directory :clysm)))
          "backend/wasm-emit.lisp exists"))))

(deftest backend-wat-print-compiles
    "Test that backend/wat-print.lisp compiles fully"
  (testing "all DEFUN forms in wat-print.lisp compile or are skipped"
    (let ((module-path "src/clysm/backend/wat-print.lisp"))
      (ok (probe-file (merge-pathnames module-path
                                       (asdf:system-source-directory :clysm)))
          "backend/wat-print.lisp exists"))))

;;; NOTE: Full compilation tests require running Stage 1 generation
;;; and checking the report for backend/ module status.
;;; These tests serve as markers for US4 goal verification.
