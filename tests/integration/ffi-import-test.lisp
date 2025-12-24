;;;; ffi-import-test.lisp - Integration tests for FFI imports (T018)
;;;;
;;;; End-to-end tests for host function calls from Lisp.
;;;; These tests verify the FFI import infrastructure works correctly.

(in-package #:clysm/tests/integration/ffi-import)

;;; ============================================================
;;; T018: Host Function Call Integration Tests
;;; ============================================================

;;; Since actual Wasm runtime execution requires an external host,
;;; these integration tests verify:
;;; 1. define-foreign-function creates proper FFI environment entries
;;; 2. Generated stub functions signal correct errors in interpreter mode
;;; 3. The full FFI pipeline from declaration to import generation

(deftest define-foreign-function-registers-import
  "Test that define-foreign-function registers import in FFI environment."
  ;; Reset to clean state
  (clysm/ffi:reset-ffi-environment)
  ;; Define a foreign function using the macro
  (eval '(clysm/ffi:define-foreign-function test-console-log
           "console.log"
           (:string)
           :void))
  (testing "foreign function is registered"
    (let ((decl (clysm/ffi:lookup-foreign-function
                 clysm/ffi:*ffi-environment*
                 'test-console-log)))
      (ok decl "Function should be registered")
      (ok (string= "console" (clysm/ffi:ffd-module-name decl)))
      (ok (string= "log" (clysm/ffi:ffd-field-name decl)))
      (ok (equal '(:string) (clysm/ffi:ffd-param-types decl)))
      (ok (eq :void (clysm/ffi:ffd-return-type decl)))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest host-function-stub-signals-error
  "Test that host function stub signals FFI-HOST-ERROR in interpreter."
  ;; Reset to clean state
  (clysm/ffi:reset-ffi-environment)
  ;; Define a foreign function
  (eval '(clysm/ffi:define-foreign-function test-host-add
           "host.add"
           (:fixnum :fixnum)
           :fixnum))
  (testing "calling stub signals ffi-host-error"
    ;; Use handler-case since rove's signals macro has issues with interned symbols
    (let ((error-signaled nil))
      (handler-case
          (funcall (symbol-function 'test-host-add) 1 2)
        (clysm/ffi:ffi-host-error ()
          (setf error-signaled t)))
      (ok error-signaled "Calling foreign function in interpreter should signal error")))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest call-host-signals-error-in-interpreter
  "Test that ffi:call-host signals FFI-HOST-ERROR in interpreter."
  (testing "call-host signals error for any function"
    (let ((error-signaled nil))
      (handler-case
          (clysm/ffi:call-host "host.random")
        (clysm/ffi:ffi-host-error ()
          (setf error-signaled t)))
      (ok error-signaled "call-host should signal error in interpreter")))
  (testing "call-host includes function name in error"
    (let ((function-name-in-error nil))
      (handler-case
          (clysm/ffi:call-host "test.function")
        (clysm/ffi:ffi-host-error (e)
          (setf function-name-in-error
                (clysm/ffi:ffi-host-error-function-name e))))
      (ok (string= "test.function" function-name-in-error)
          "Error should contain function name"))))

(deftest multiple-imports-pipeline
  "Test full pipeline with multiple imports."
  ;; Reset to clean state
  (clysm/ffi:reset-ffi-environment)
  ;; Register multiple foreign functions
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-log
    :module-name "console"
    :field-name "log"
    :param-types '(:string)
    :return-type :void))
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-add
    :module-name "math"
    :field-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-random
    :module-name "math"
    :field-name "random"
    :param-types '()
    :return-type :float))
  ;; Assign indices
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  ;; Collect imports
  (let ((imports (clysm/ffi:collect-ffi-imports clysm/ffi:*ffi-environment*)))
    (testing "all imports collected"
      (ok (= 3 (length imports))))
    (testing "imports have valid structure"
      (dolist (import imports)
        (ok (stringp (clysm/ffi:wi-module-name import)))
        (ok (stringp (clysm/ffi:wi-field-name import)))
        (ok (eq :func (clysm/ffi:wi-kind import))))))
  ;; Emit to buffer
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (clysm/ffi:emit-ffi-imports clysm/ffi:*ffi-environment* buffer)
    (testing "import section is generated"
      (ok (> (length buffer) 0)))
    (testing "section starts with correct ID"
      (ok (= 2 (aref buffer 0)))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest import-index-assignment
  "Test that import indices are assigned correctly."
  ;; Reset to clean state
  (clysm/ffi:reset-ffi-environment)
  ;; Register functions
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-first
    :module-name "host"
    :field-name "first"
    :param-types '()
    :return-type :fixnum))
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-second
    :module-name "host"
    :field-name "second"
    :param-types '()
    :return-type :fixnum))
  ;; Before assignment, type-index should be nil
  (testing "indices unassigned initially"
    (let ((decl (clysm/ffi:lookup-foreign-function
                 clysm/ffi:*ffi-environment* 'test-first)))
      (ok (null (clysm/ffi:ffd-type-index decl)))))
  ;; Assign indices
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  ;; After assignment, type-index should be set
  (testing "indices assigned after call"
    (let ((decl1 (clysm/ffi:lookup-foreign-function
                  clysm/ffi:*ffi-environment* 'test-first))
          (decl2 (clysm/ffi:lookup-foreign-function
                  clysm/ffi:*ffi-environment* 'test-second)))
      (ok (numberp (clysm/ffi:ffd-type-index decl1)))
      (ok (numberp (clysm/ffi:ffd-type-index decl2)))
      ;; Indices should be different
      (ok (/= (clysm/ffi:ffd-type-index decl1)
              (clysm/ffi:ffd-type-index decl2)))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest empty-ffi-environment
  "Test behavior with no FFI declarations."
  (clysm/ffi:reset-ffi-environment)
  (testing "no imports when environment is empty"
    (ok (= 0 (clysm/ffi:get-ffi-import-count))))
  (testing "collect-ffi-imports returns empty list"
    (let ((imports (clysm/ffi:collect-ffi-imports clysm/ffi:*ffi-environment*)))
      (ok (null imports))))
  (testing "emit-ffi-imports produces no output for empty env"
    (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
      (clysm/ffi:emit-ffi-imports clysm/ffi:*ffi-environment* buffer)
      (ok (= 0 (length buffer))))))

(deftest nested-module-names
  "Test that first dot separates module from field per spec."
  (clysm/ffi:reset-ffi-environment)
  ;; Parse a deeply nested name - first part is module, rest is field
  ;; This matches Wasm import semantics where module is typically short
  (multiple-value-bind (module field)
      (clysm/ffi:parse-host-name "env.console.log")
    (testing "module is first segment"
      (ok (string= "env" module)))
    (testing "field is everything after first dot"
      (ok (string= "console.log" field))))
  ;; Single-part name uses "env" as default module
  (multiple-value-bind (module field)
      (clysm/ffi:parse-host-name "log")
    (testing "default module is env"
      (ok (string= "env" module)))
    (testing "field is the whole name"
      (ok (string= "log" field))))
  ;; Register and verify
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-nested
    :module-name "host"
    :field-name "console.log"
    :param-types '(:string)
    :return-type :void))
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  (let ((imports (clysm/ffi:collect-ffi-imports clysm/ffi:*ffi-environment*)))
    (testing "module name is preserved"
      (ok (string= "host" (clysm/ffi:wi-module-name (first imports)))))
    (testing "field name with dots is preserved"
      (ok (string= "console.log" (clysm/ffi:wi-field-name (first imports))))))
  (clysm/ffi:reset-ffi-environment))
