;;;; cxr-macro-test.lisp - Unit tests for define-cxr-compiler macro
;;;; Feature: 001-cxr-compiler-macro
;;;;
;;;; Tests macro expansion, function signature, and validation logic.

(in-package #:clysm/tests/unit/cxr-macro)

;;; T004: Test macro expansion produces correct defun form
(deftest test-define-cxr-compiler-expansion
  "Macro expands to a defun with correct structure"
  (let ((expansion (macroexpand-1 '(clysm/compiler/codegen/func-section::define-cxr-compiler caddr "dda"))))
    (ok (consp expansion) "Expansion should be a list")
    (ok (eq (first expansion) 'defun) "First element should be DEFUN")
    (ok (eq (second expansion) 'clysm/compiler/codegen/func-section::compile-caddr)
        "Function name should be COMPILE-CADDR")))

;;; T005: Test generated function has (args env) signature
(deftest test-define-cxr-compiler-function-signature
  "Generated function has correct (args env) signature"
  (let ((expansion (macroexpand-1 '(clysm/compiler/codegen/func-section::define-cxr-compiler caar "aa"))))
    (ok (consp expansion) "Expansion should be a list")
    (let ((params (third expansion)))
      ;; Compare symbol names to be package-agnostic
      (ok (equal (mapcar #'symbol-name params) '("ARGS" "ENV"))
          "Function parameters should be (args env)"))))

;;; T006: Test generated function calls compile-cxr-chain with correct ops
(deftest test-define-cxr-compiler-calls-chain
  "Generated function body calls compile-cxr-chain with ops string"
  (let ((expansion (macroexpand-1 '(clysm/compiler/codegen/func-section::define-cxr-compiler cadr "da"))))
    (ok (consp expansion) "Expansion should be a list")
    ;; Body is after defun, name, params, docstring
    (let* ((body (cddddr expansion))
           (call (first body)))
      (ok (consp call) "Body should contain a form")
      (ok (eq (first call) 'clysm/compiler/codegen/func-section::compile-cxr-chain)
          "Body should call compile-cxr-chain")
      (ok (equal (second call) "da")
          "Should pass ops string 'da'"))))

;;; T007: Test validation rejects empty operation string
(deftest test-define-cxr-compiler-rejects-empty-string
  "Macro signals error for empty operation string"
  (ok (handler-case
          (progn
            (macroexpand-1 '(clysm/compiler/codegen/func-section::define-cxr-compiler foo ""))
            nil)  ; Should not reach here
        (error () t))  ; Error signaled - return T
      "Empty ops string should signal an error"))

;;; T008: Test validation rejects invalid characters
(deftest test-define-cxr-compiler-rejects-invalid-chars
  "Macro signals error for invalid characters in operation string"
  (ok (handler-case
          (progn
            (macroexpand-1 '(clysm/compiler/codegen/func-section::define-cxr-compiler bar "xyz"))
            nil)
        (error () t))
      "Invalid ops chars should signal an error")
  (ok (handler-case
          (progn
            (macroexpand-1 '(clysm/compiler/codegen/func-section::define-cxr-compiler baz "ab"))
            nil)
        (error () t))
      "Mixed valid/invalid ('b') should signal an error"))

;;; Additional test: docstring generation
(deftest test-define-cxr-compiler-generates-docstring
  "Generated function includes a descriptive docstring"
  (let ((expansion (macroexpand-1 '(clysm/compiler/codegen/func-section::define-cxr-compiler caddr "dda"))))
    (ok (consp expansion) "Expansion should be a list")
    ;; Docstring is fourth element (after defun, name, params)
    (let ((docstring (fourth expansion)))
      (ok (stringp docstring) "Fourth element should be a docstring")
      (ok (search "caddr" docstring :test #'char-equal)
          "Docstring should mention the function name")
      (ok (search "car" docstring :test #'char-equal)
          "Docstring should describe the operations"))))
