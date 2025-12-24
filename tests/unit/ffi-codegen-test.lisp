;;;; ffi-codegen-test.lisp - Unit tests for FFI code generation (T015, T027-T028)
;;;;
;;;; Tests for define-foreign-function and export-function macro expansion.
;;;; These tests will be implemented as part of User Stories 1 and 2.

(in-package #:clysm/tests/unit/ffi-codegen)

;;; ============================================================
;;; T015: define-foreign-function Macro Expansion Tests
;;; ============================================================

(deftest define-foreign-function-expansion
  "Test that define-foreign-function expands correctly."
  (testing "macro produces valid expansion"
    (let ((expansion (macroexpand-1
                      '(clysm/ffi:define-foreign-function host-log
                         "host.log"
                         (:string)
                         :void))))
      (ok (listp expansion))
      (ok (not (null expansion)))))
  (testing "expansion creates a function"
    ;; After macro expansion, the symbol should be fbound
    (clysm/ffi:define-foreign-function test-host-add
      "host.add"
      (:fixnum :fixnum)
      :fixnum)
    (ok (fboundp 'test-host-add))))

(deftest define-foreign-function-registers-declaration
  "Test that define-foreign-function registers the declaration."
  (testing "declaration is retrievable from global registry"
    (clysm/ffi:define-foreign-function registered-func
      "env.test"
      (:fixnum)
      :fixnum)
    ;; Check that we can look up the registered function
    (ok (clysm/ffi:lookup-foreign-function
         clysm/ffi:*ffi-environment*
         'registered-func))))

;;; ============================================================
;;; T027: export-function Macro Expansion Tests
;;; ============================================================

(deftest export-function-expansion
  "Test that export-function expands correctly."
  ;; Reset for clean test
  (clysm/ffi:reset-ffi-environment)
  (testing "macro produces valid expansion"
    (let ((expansion (macroexpand-1
                       '(clysm/ffi:export-function my-add
                          :as "myAdd"
                          :signature ((:fixnum :fixnum) :fixnum)))))
      (ok (listp expansion))
      (ok (not (null expansion)))))
  (testing "expansion without :as uses lisp name as export name"
    (let ((expansion (macroexpand-1
                       '(clysm/ffi:export-function my-func
                          :signature ((:string) :void)))))
      (ok (listp expansion))))
  (clysm/ffi:reset-ffi-environment))

(deftest export-function-registers-declaration
  "Test that export-function registers the declaration."
  (clysm/ffi:reset-ffi-environment)
  ;; Define a function to export
  (defun test-export-add (a b)
    (+ a b))
  ;; Export it
  (eval '(clysm/ffi:export-function test-export-add
           :as "testAdd"
           :signature ((:fixnum :fixnum) :fixnum)))
  (testing "export is registered in FFI environment"
    (let ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*)))
      (ok (not (null exports)))
      (ok (>= (length exports) 1))))
  (testing "export has correct name"
    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (export-decl (find 'test-export-add exports
                              :key #'clysm/ffi:ed-lisp-name)))
      (ok export-decl)
      (ok (string= "testAdd" (clysm/ffi:ed-export-name export-decl)))))
  (clysm/ffi:reset-ffi-environment))

;;; ============================================================
;;; T028: Export Wrapper Generation Tests
;;; ============================================================

(deftest export-wrapper-generation
  "Test that export wrappers are generated correctly."
  (clysm/ffi:reset-ffi-environment)
  ;; Define a simple function
  (defun test-wrapper-fn (x)
    (* x 2))
  ;; Export it
  (eval '(clysm/ffi:export-function test-wrapper-fn
           :as "doubleIt"
           :signature ((:fixnum) :fixnum)))
  (testing "export declaration has correct param types"
    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (export-decl (find 'test-wrapper-fn exports
                              :key #'clysm/ffi:ed-lisp-name)))
      (ok export-decl)
      (ok (equal '(:fixnum) (clysm/ffi:ed-param-types export-decl)))))
  (testing "export declaration has correct return type"
    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (export-decl (find 'test-wrapper-fn exports
                              :key #'clysm/ffi:ed-lisp-name)))
      (ok (eq :fixnum (clysm/ffi:ed-return-type export-decl)))))
  (clysm/ffi:reset-ffi-environment))

(deftest export-wrapper-marshalling
  "Test that export wrappers handle type marshalling."
  (clysm/ffi:reset-ffi-environment)
  ;; Define a function with string param
  (defun test-string-fn (s)
    (length s))
  ;; Export it
  (eval '(clysm/ffi:export-function test-string-fn
           :as "stringLength"
           :signature ((:string) :fixnum)))
  (testing "string type is accepted in signature"
    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (export-decl (find 'test-string-fn exports
                              :key #'clysm/ffi:ed-lisp-name)))
      (ok export-decl)
      (ok (equal '(:string) (clysm/ffi:ed-param-types export-decl)))))
  (clysm/ffi:reset-ffi-environment))

(deftest export-void-return
  "Test that export with void return type works."
  (clysm/ffi:reset-ffi-environment)
  ;; Define a side-effect only function
  (defun test-void-fn (msg)
    (declare (ignore msg))
    nil)
  ;; Export it
  (eval '(clysm/ffi:export-function test-void-fn
           :as "logMessage"
           :signature ((:string) :void)))
  (testing "void return type is accepted"
    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (export-decl (find 'test-void-fn exports
                              :key #'clysm/ffi:ed-lisp-name)))
      (ok export-decl)
      (ok (eq :void (clysm/ffi:ed-return-type export-decl)))))
  (clysm/ffi:reset-ffi-environment))
