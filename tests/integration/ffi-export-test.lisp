;;;; ffi-export-test.lisp - Integration tests for FFI exports (T030)
;;;;
;;;; End-to-end tests for Lisp function exports called from host.
;;;; These tests verify the FFI export infrastructure works correctly.

(in-package #:clysm/tests/integration/ffi-export)

;;; ============================================================
;;; T030: Lisp Function Export Integration Tests
;;; ============================================================

;;; Since actual Wasm runtime execution requires an external host,
;;; these integration tests verify:
;;; 1. export-function creates proper FFI environment entries
;;; 2. Export declarations are properly validated
;;; 3. The full FFI export pipeline from declaration to section generation

(deftest export-function-registers-export
  "Test that export-function registers export in FFI environment."
  ;; Reset to clean state
  (clysm/ffi:reset-ffi-environment)
  ;; Define a function to export
  (defun test-export-calc (x y)
    (+ x y))
  ;; Export it
  (eval '(clysm/ffi:export-function test-export-calc
           :as "calculate"
           :signature ((:fixnum :fixnum) :fixnum)))
  (testing "export is registered"
    (let ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*)))
      (ok (not (null exports)))
      (let ((decl (find 'test-export-calc exports
                        :key #'clysm/ffi:ed-lisp-name)))
        (ok decl "Export should be registered")
        (ok (string= "calculate" (clysm/ffi:ed-export-name decl)))
        (ok (equal '(:fixnum :fixnum) (clysm/ffi:ed-param-types decl)))
        (ok (eq :fixnum (clysm/ffi:ed-return-type decl))))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest export-function-with-default-name
  "Test that export-function uses lisp name when :as is not provided."
  (clysm/ffi:reset-ffi-environment)
  ;; Define a function to export
  (defun my-lisp-function ()
    42)
  ;; Export without :as keyword
  (eval '(clysm/ffi:export-function my-lisp-function
           :signature (() :fixnum)))
  (testing "export uses lisp name as export name"
    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (decl (find 'my-lisp-function exports
                       :key #'clysm/ffi:ed-lisp-name)))
      (ok decl)
      ;; Export name should be the string form of the lisp name
      (ok (string= "my-lisp-function" (clysm/ffi:ed-export-name decl)))))
  (clysm/ffi:reset-ffi-environment))

(deftest multiple-exports-pipeline
  "Test full pipeline with multiple exports."
  ;; Reset to clean state
  (clysm/ffi:reset-ffi-environment)
  ;; Define some functions
  (defun test-exp-add (a b) (+ a b))
  (defun test-exp-sub (a b) (- a b))
  (defun test-exp-mul (a b) (* a b))
  ;; Register multiple exports manually (direct API)
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'test-exp-add
    :export-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'test-exp-sub
    :export-name "subtract"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'test-exp-mul
    :export-name "multiply"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  ;; Collect exports
  (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
    (testing "all exports collected"
      (ok (= 3 (length exports))))
    (testing "exports have valid structure"
      (dolist (exp exports)
        (ok (symbolp (clysm/ffi:ed-lisp-name exp)))
        (ok (stringp (clysm/ffi:ed-export-name exp)))
        (ok (listp (clysm/ffi:ed-param-types exp))))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest export-index-assignment
  "Test that export wrapper indices are assigned correctly."
  ;; Reset to clean state
  (clysm/ffi:reset-ffi-environment)
  ;; Register exports
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'first-export
    :export-name "first"
    :param-types '()
    :return-type :fixnum))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'second-export
    :export-name "second"
    :param-types '()
    :return-type :fixnum))
  ;; Before assignment, wrapper-func-index should be nil
  (testing "indices unassigned initially"
    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (decl (find 'first-export exports :key #'clysm/ffi:ed-lisp-name)))
      (ok (null (clysm/ffi:ed-wrapper-func-index decl)))))
  ;; Assign indices
  (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)
  ;; After assignment, wrapper-func-index should be set
  (testing "indices assigned after call"
    (let* ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*))
           (decl1 (find 'first-export exports :key #'clysm/ffi:ed-lisp-name))
           (decl2 (find 'second-export exports :key #'clysm/ffi:ed-lisp-name)))
      (ok (numberp (clysm/ffi:ed-wrapper-func-index decl1)))
      (ok (numberp (clysm/ffi:ed-wrapper-func-index decl2)))
      ;; Indices should be different
      (ok (/= (clysm/ffi:ed-wrapper-func-index decl1)
              (clysm/ffi:ed-wrapper-func-index decl2)))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest empty-exports-environment
  "Test behavior with no FFI exports."
  (clysm/ffi:reset-ffi-environment)
  (testing "no exports when environment is empty"
    (ok (= 0 (clysm/ffi:get-ffi-export-count))))
  (testing "collect-ffi-exports returns empty list"
    (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
      (ok (null exports))))
  (testing "emit-ffi-exports produces no output for empty env"
    (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
      (clysm/ffi:emit-ffi-exports clysm/ffi:*ffi-environment* buffer)
      (ok (= 0 (length buffer))))))

(deftest export-all-marshal-types
  "Test that all marshal types can be used in exports."
  (clysm/ffi:reset-ffi-environment)
  ;; Register exports with various types
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'fn-fixnum
    :export-name "fnFixnum"
    :param-types '(:fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'fn-float
    :export-name "fnFloat"
    :param-types '(:float)
    :return-type :float))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'fn-string
    :export-name "fnString"
    :param-types '(:string)
    :return-type :string))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'fn-boolean
    :export-name "fnBoolean"
    :param-types '(:boolean)
    :return-type :boolean))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'fn-void
    :export-name "fnVoid"
    :param-types '(:anyref)
    :return-type :void))
  (testing "all exports with different types are registered"
    (ok (= 5 (clysm/ffi:get-ffi-export-count))))
  (clysm/ffi:reset-ffi-environment))
