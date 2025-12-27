;;;; ffi-multi-host-test.lisp - Integration tests for multi-host FFI (T061-T062)
;;;;
;;;; Tests for FFI functionality across wasmtime and JavaScript environments.
;;;; These tests verify that FFI import/export section encoding is correct
;;;; and that the generated Wasm validates correctly for multi-host usage.

(in-package #:clysm/tests/integration/ffi-multi-host)

;;; ============================================================
;;; T061: Wasmtime Integration Tests
;;; ============================================================

(deftest wasmtime-validates-with-ffi-imports
  "Test that Wasm with FFI imports validates for wasmtime."
  ;; Reset FFI environment
  (clysm/ffi:reset-ffi-environment)
  ;; Register a foreign function import
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'wasmtime-test-add
    :module-name "host"
    :field-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  ;; Compile a simple expression (FFI imports will be included in module)
  (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
    ;; Validate that wasmtime can parse the module
    (testing "compiled module with FFI imports is valid Wasm"
      (ok (clysm/tests/helpers:validate-wasm-silent bytes)
          "Module should validate with wasm-tools")))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest wasmtime-validates-with-ffi-exports
  "Test that Wasm with FFI exports validates for wasmtime."
  ;; Reset FFI environment
  (clysm/ffi:reset-ffi-environment)
  ;; Register an export declaration
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'wasmtime-test-export
    :export-name "testFunc"
    :param-types '(:fixnum)
    :return-type :fixnum))
  ;; Compile a simple expression (FFI exports will be included in module)
  (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
    ;; Validate that the module is well-formed
    (testing "compiled module with FFI exports is valid Wasm"
      (ok (clysm/tests/helpers:validate-wasm-silent bytes)
          "Module should validate with wasm-tools")))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest wasmtime-import-section-encoding
  "Test that FFI import section is correctly encoded."
  (clysm/ffi:reset-ffi-environment)
  ;; Register multiple imports with different signatures
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'wt-log
    :module-name "host"
    :field-name "log"
    :param-types '(:string)
    :return-type :void))
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'wt-random
    :module-name "host"
    :field-name "random"
    :param-types '()
    :return-type :float))
  ;; Assign indices
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  ;; Emit import section
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (clysm/ffi:emit-ffi-imports clysm/ffi:*ffi-environment* buffer)
    (testing "import section has correct section ID"
      (ok (= 2 (aref buffer 0)) "Section ID should be 2 (Import)"))
    (testing "import section is not empty"
      (ok (> (length buffer) 5) "Buffer should have content")))
  (clysm/ffi:reset-ffi-environment))

;;; ============================================================
;;; T062: Node.js Integration Tests
;;; ============================================================

(deftest nodejs-compatible-import-names
  "Test that import names are compatible with JavaScript."
  (clysm/ffi:reset-ffi-environment)
  ;; Register functions with JavaScript-compatible names
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'js-console-log
    :module-name "host"
    :field-name "log"
    :param-types '(:string)
    :return-type :void))
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'js-math-add
    :module-name "host"
    :field-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  ;; Collect imports and verify names
  (let ((imports (clysm/ffi:collect-ffi-imports clysm/ffi:*ffi-environment*)))
    (testing "import module names are valid identifiers"
      (ok (every (lambda (imp)
                   (let ((name (clysm/ffi:wi-module-name imp)))
                     (and (stringp name) (> (length name) 0))))
                 imports)))
    (testing "import field names are valid identifiers"
      (ok (every (lambda (imp)
                   (let ((name (clysm/ffi:wi-field-name imp)))
                     (and (stringp name) (> (length name) 0))))
                 imports))))
  (clysm/ffi:reset-ffi-environment))

(deftest nodejs-compatible-export-names
  "Test that export names are compatible with JavaScript."
  (clysm/ffi:reset-ffi-environment)
  ;; Register exports with JavaScript-compatible names
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'js-add
    :export-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'js-multiply
    :export-name "multiply"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)
  ;; Collect exports and verify names
  (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
    (testing "export names are valid JavaScript identifiers"
      (ok (every (lambda (exp)
                   (let ((name (clysm/ffi:ed-export-name exp)))
                     (and (stringp name)
                          (> (length name) 0)
                          ;; Valid JS identifier: starts with letter/underscore
                          (or (alpha-char-p (char name 0))
                              (char= (char name 0) #\_)))))
                 exports))))
  (clysm/ffi:reset-ffi-environment))

(deftest nodejs-export-section-encoding
  "Test that FFI export section is correctly encoded for JavaScript."
  (clysm/ffi:reset-ffi-environment)
  ;; Register exports
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'node-calc
    :export-name "calculate"
    :param-types '(:fixnum)
    :return-type :fixnum))
  (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)
  ;; Emit export section
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (clysm/ffi:emit-ffi-exports clysm/ffi:*ffi-environment* buffer)
    (testing "export section has correct section ID"
      (ok (= 7 (aref buffer 0)) "Section ID should be 7 (Export)"))
    (testing "export section contains function name"
      ;; Look for "calculate" in the buffer
      (let ((calc-bytes (clysm/lib/utf8:string-to-utf8-octets "calculate")))
        (ok (search (coerce calc-bytes 'list)
                    (coerce buffer 'list))
            "Buffer should contain 'calculate' string"))))
  (clysm/ffi:reset-ffi-environment))

;;; ============================================================
;;; Cross-Environment Compatibility Tests
;;; ============================================================

(deftest same-binary-structure-both-hosts
  "Test that the same Wasm binary structure works for both hosts."
  (clysm/ffi:reset-ffi-environment)
  ;; Register both imports and exports
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'common-log
    :module-name "host"
    :field-name "log"
    :param-types '(:string)
    :return-type :void))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'common-add
    :export-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  ;; Compile with FFI
  (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
    (testing "module with mixed imports/exports validates"
      (ok (clysm/tests/helpers:validate-wasm-silent bytes)
          "Module should validate for both wasmtime and JS")))
  (clysm/ffi:reset-ffi-environment))

(deftest marshal-types-compatible-both-hosts
  "Test that marshal types are compatible across hosts."
  ;; Test that marshal types produce consistent Wasm types
  ;; Use string-equal on symbol-name to compare across packages
  (testing "fixnum maps to i31ref consistently"
    (ok (string-equal "I31REF"
                      (symbol-name (clysm/ffi:marshal-type-to-wasm-type :fixnum)))))
  (testing "float maps to f64 consistently"
    (ok (string-equal "F64"
                      (symbol-name (clysm/ffi:marshal-type-to-wasm-type :float)))))
  (testing "string maps to externref consistently"
    (ok (string-equal "EXTERNREF"
                      (symbol-name (clysm/ffi:marshal-type-to-wasm-type :string)))))
  (testing "boolean maps to i32 consistently"
    (ok (string-equal "I32"
                      (symbol-name (clysm/ffi:marshal-type-to-wasm-type :boolean)))))
  (testing "anyref maps to anyref consistently"
    (ok (string-equal "ANYREF"
                      (symbol-name (clysm/ffi:marshal-type-to-wasm-type :anyref))))))

(deftest ffi-environment-isolation
  "Test that FFI environment is properly isolated between tests."
  (clysm/ffi:reset-ffi-environment)
  (testing "environment starts empty"
    (ok (= 0 (clysm/ffi:get-ffi-import-count)))
    (ok (= 0 (clysm/ffi:get-ffi-export-count))))
  ;; Add some declarations
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'isolation-test
    :module-name "test"
    :field-name "func"
    :param-types '()
    :return-type :void))
  (testing "environment has declaration"
    (ok (= 1 (clysm/ffi:get-ffi-import-count))))
  ;; Reset and verify
  (clysm/ffi:reset-ffi-environment)
  (testing "environment is empty after reset"
    (ok (= 0 (clysm/ffi:get-ffi-import-count)))
    (ok (= 0 (clysm/ffi:get-ffi-export-count)))))
