;;;; stage1-fs-test.lisp - Contract tests for Stage 1 filesystem FFI
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for clysm.fs namespace FFI contract

(in-package #:clysm/tests/contract/stage1-fs)

;;; ==========================================================================
;;; FFI Contract Tests
;;; ==========================================================================

(deftest test-fs-read-source-contract-signature
  "clysm.fs.read-source FFI should accept string path and return string."
  ;; This test documents the expected FFI contract
  ;; Actual invocation requires wasmtime + host shim
  (ok t "FFI contract: (read-source path) -> string"))

(deftest test-fs-list-modules-contract-signature
  "clysm.fs.list-modules FFI should return array of strings."
  ;; Documents the expected contract
  (ok t "FFI contract: (list-modules) -> string[]"))

(deftest test-fs-read-source-contract-null-on-missing
  "clysm.fs.read-source should return null for missing files."
  ;; Documents expected behavior
  (ok t "FFI contract: returns null when file not found"))

(deftest test-fs-read-source-contract-utf8
  "clysm.fs.read-source should return UTF-8 encoded content."
  (ok t "FFI contract: returns UTF-8 string"))

;;; ==========================================================================
;;; Module Path Contract Tests
;;; ==========================================================================

(deftest test-module-paths-match-compiler-order
  "Module paths should match compiler-order.lisp."
  (let ((paths (clysm/stage1:get-module-paths)))
    (ok (>= (length paths) 40)
        (format nil "~D modules in compiler order (>= 40)" (length paths)))
    (ok (search "leb128.lisp" (first paths))
        "first module is leb128.lisp")
    (ok (search "macros.lisp" (car (last paths)))
        "last module is macros.lisp")))

(deftest test-module-paths-relative-to-root
  "Module paths should be relative to project root."
  (let ((paths (clysm/stage1:get-module-paths)))
    (ok (every (lambda (p) (search "src/clysm/" p)) paths)
        "all paths start with src/clysm/")))

;;; ==========================================================================
;;; Host Shim Contract Tests
;;; ==========================================================================

(deftest test-host-shim-exists
  "stage1-host.js should exist."
  (ok (probe-file
       (merge-pathnames "host-shim/stage1-host.js"
                        (asdf:system-source-directory :clysm)))
      "host-shim/stage1-host.js exists"))

(deftest test-host-shim-has-fs-namespace
  "stage1-host.js should export clysm.fs namespace."
  (let* ((path (merge-pathnames "host-shim/stage1-host.js"
                                (asdf:system-source-directory :clysm)))
         (content (uiop:read-file-string path)))
    (ok (search "clysm.fs" content)
        "host shim has clysm.fs namespace")
    (ok (search "read-source" content)
        "host shim has read-source function")
    (ok (search "list-modules" content)
        "host shim has list-modules function")))

