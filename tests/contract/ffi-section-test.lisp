;;;; ffi-section-test.lisp - Contract tests for FFI Wasm section encoding (T006)
;;;;
;;;; Tests for Import/Export section encoding and validation

(in-package #:clysm/tests/contract/ffi-section)

;;; ============================================================
;;; T006: Import Section Encoding Tests
;;; ============================================================

(deftest import-section-encoding-single
  "Test encoding a single function import."
  (let ((import (clysm/ffi:make-wasm-import
                 :module-name "host"
                 :field-name "log"
                 :kind :func
                 :type-index 0)))
    (testing "import section can be created"
      (let ((section (clysm/backend/sections:make-import-section (list import))))
        (ok section)
        (ok (= clysm/backend/sections:+section-id-import+
               (clysm/backend/sections:section-id section)))))))

(deftest import-section-encoding-multiple
  "Test encoding multiple function imports."
  (let ((imports (list
                  (clysm/ffi:make-wasm-import
                   :module-name "host"
                   :field-name "log"
                   :kind :func
                   :type-index 0)
                  (clysm/ffi:make-wasm-import
                   :module-name "host"
                   :field-name "add"
                   :kind :func
                   :type-index 1))))
    (testing "import section with multiple imports"
      (let ((section (clysm/backend/sections:make-import-section imports)))
        (ok section)))))

(deftest encode-import-function
  "Test encoding an individual import entry."
  (let ((import (clysm/ffi:make-wasm-import
                 :module-name "host"
                 :field-name "log"
                 :kind :func
                 :type-index 0)))
    (testing "encode-import returns bytes"
      (let ((bytes (clysm/backend/sections:encode-import import)))
        (ok (typep bytes '(vector (unsigned-byte 8))))
        ;; Should contain: module name length + "host" + field name length + "log" + 0x00 (func) + type index
        (ok (> (length bytes) 0))))))

;;; ============================================================
;;; Import Section Structure Tests
;;; ============================================================

(deftest import-section-structure
  "Test that import section has correct structure."
  (let* ((import (clysm/ffi:make-wasm-import
                  :module-name "env"
                  :field-name "log"
                  :kind :func
                  :type-index 0))
         (section (clysm/backend/sections:make-import-section (list import)))
         (encoded (clysm/backend/sections:encode-section section)))
    (testing "section starts with section ID 2 (import)"
      (ok (= 2 (aref encoded 0))))
    (testing "section has valid length"
      (ok (> (length encoded) 1)))))

;;; ============================================================
;;; T017: Generated Wasm Import Section Contract Test
;;; ============================================================

(deftest generated-import-section-structure
  "Test that define-foreign-function generates correct import section."
  ;; Reset FFI environment for clean test
  (clysm/ffi:reset-ffi-environment)
  ;; Register a foreign function
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-host-log
    :module-name "host"
    :field-name "log"
    :param-types '(:string)
    :return-type :void))
  ;; Assign indices
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  ;; Collect imports
  (let ((imports (clysm/ffi:collect-ffi-imports clysm/ffi:*ffi-environment*)))
    (testing "one import is collected"
      (ok (= 1 (length imports))))
    (testing "import has correct module name"
      (ok (string= "host" (clysm/ffi:wi-module-name (first imports)))))
    (testing "import has correct field name"
      (ok (string= "log" (clysm/ffi:wi-field-name (first imports)))))
    (testing "import is function kind"
      (ok (eq :func (clysm/ffi:wi-kind (first imports))))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest emit-imports-produces-valid-section
  "Test that emit-ffi-imports produces valid Wasm binary."
  ;; Reset FFI environment for clean test
  (clysm/ffi:reset-ffi-environment)
  ;; Register two foreign functions
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-host-add
    :module-name "host"
    :field-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'test-host-sub
    :module-name "host"
    :field-name "sub"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  ;; Assign indices
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  ;; Emit to buffer
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (clysm/ffi:emit-ffi-imports clysm/ffi:*ffi-environment* buffer)
    (testing "buffer is not empty"
      (ok (> (length buffer) 0)))
    (testing "buffer starts with import section ID (2)"
      (ok (= 2 (aref buffer 0)))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

;;; ============================================================
;;; T029: Generated Wasm Export Section Contract Test
;;; ============================================================

(deftest generated-export-section-structure
  "Test that export-function generates correct export section."
  ;; Reset FFI environment for clean test
  (clysm/ffi:reset-ffi-environment)
  ;; Register an export declaration
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'test-add
    :export-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  ;; Check that it's registered
  (let ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*)))
    (testing "one export is registered"
      (ok (= 1 (length exports))))
    (testing "export has correct lisp name"
      (ok (eq 'test-add (clysm/ffi:ed-lisp-name (first exports)))))
    (testing "export has correct export name"
      (ok (string= "add" (clysm/ffi:ed-export-name (first exports)))))
    (testing "export has correct param types"
      (ok (equal '(:fixnum :fixnum) (clysm/ffi:ed-param-types (first exports)))))
    (testing "export has correct return type"
      (ok (eq :fixnum (clysm/ffi:ed-return-type (first exports))))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

(deftest emit-exports-produces-valid-section
  "Test that emit-ffi-exports produces valid Wasm binary."
  ;; Reset FFI environment for clean test
  (clysm/ffi:reset-ffi-environment)
  ;; Register two export declarations
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'test-add
    :export-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'test-sub
    :export-name "sub"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  ;; Collect exports and generate section
  (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
    (testing "two exports are collected"
      (ok (= 2 (length exports)))))
  ;; Emit to buffer
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (clysm/ffi:emit-ffi-exports clysm/ffi:*ffi-environment* buffer)
    (testing "buffer is not empty"
      (ok (> (length buffer) 0)))
    (testing "buffer starts with export section ID (7)"
      (ok (= 7 (aref buffer 0)))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

;;; ============================================================
;;; T053: Multiple Imports Contract Test
;;; ============================================================

(deftest multiple-imports-validation
  "Test that multiple imports produce valid Wasm binary structure."
  ;; Reset FFI environment for clean test
  (clysm/ffi:reset-ffi-environment)
  ;; Register multiple foreign functions with different signatures
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'import-log
    :module-name "host"
    :field-name "log"
    :param-types '(:string)
    :return-type :void))
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'import-add
    :module-name "host"
    :field-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'import-random
    :module-name "host"
    :field-name "random"
    :param-types '()
    :return-type :float))
  ;; Assign indices
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  ;; Collect imports
  (let ((imports (clysm/ffi:collect-ffi-imports clysm/ffi:*ffi-environment*)))
    (testing "three imports are collected"
      (ok (= 3 (length imports))))
    (testing "all imports have valid module names"
      (ok (every (lambda (imp) (stringp (clysm/ffi:wi-module-name imp))) imports)))
    (testing "all imports are function kind"
      (ok (every (lambda (imp) (eq :func (clysm/ffi:wi-kind imp))) imports))))
  ;; Emit to buffer and verify structure
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (clysm/ffi:emit-ffi-imports clysm/ffi:*ffi-environment* buffer)
    (testing "import section has valid header"
      (ok (= 2 (aref buffer 0))))  ; Section ID = 2 (Import)
    (testing "import section has reasonable size"
      (ok (> (length buffer) 10))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

;;; ============================================================
;;; T054: Multiple Exports Contract Test
;;; ============================================================

(deftest multiple-exports-validation
  "Test that multiple exports produce valid Wasm binary structure."
  ;; Reset FFI environment for clean test
  (clysm/ffi:reset-ffi-environment)
  ;; Register multiple export declarations
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'export-add
    :export-name "add"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'export-sub
    :export-name "sub"
    :param-types '(:fixnum :fixnum)
    :return-type :fixnum))
  (clysm/ffi:register-export
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-export-decl
    :lisp-name 'export-greet
    :export-name "greet"
    :param-types '(:string)
    :return-type :string))
  ;; Assign indices
  (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)
  ;; Collect exports
  (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
    (testing "three exports are collected"
      (ok (= 3 (length exports))))
    (testing "all exports have valid names"
      (ok (every (lambda (exp) (stringp (clysm/ffi:ed-export-name exp))) exports)))
    (testing "exports have wrapper function indices assigned"
      (ok (every (lambda (exp) (integerp (clysm/ffi:ed-wrapper-func-index exp))) exports))))
  ;; Emit to buffer and verify structure
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (clysm/ffi:emit-ffi-exports clysm/ffi:*ffi-environment* buffer)
    (testing "export section has valid header"
      (ok (= 7 (aref buffer 0))))  ; Section ID = 7 (Export)
    (testing "export section has reasonable size"
      (ok (> (length buffer) 10))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))

;;; ============================================================
;;; T055: wasm-tools Validate Contract Test
;;; ============================================================

(deftest ffi-wasm-validates
  "Test that FFI import section encoding is structurally valid."
  ;; This test verifies that the import section encoding follows
  ;; the correct Wasm binary format. Full wasm-tools validation
  ;; requires a complete module which is tested in integration tests.
  (clysm/ffi:reset-ffi-environment)
  ;; Register a simple import
  (clysm/ffi:register-foreign-function
   clysm/ffi:*ffi-environment*
   (clysm/ffi:make-foreign-function-decl
    :lisp-name 'validate-test
    :module-name "test"
    :field-name "func"
    :param-types '(:fixnum)
    :return-type :fixnum))
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  ;; Emit and verify binary structure
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (clysm/ffi:emit-ffi-imports clysm/ffi:*ffi-environment* buffer)
    (testing "buffer starts with correct section ID"
      (ok (= 2 (aref buffer 0))))
    (testing "section size byte follows section ID"
      ;; After section ID, next byte(s) are LEB128 size
      (ok (> (aref buffer 1) 0)))
    ;; Verify module name "test" appears in the buffer
    (testing "module name 'test' is encoded in buffer"
      (let ((test-chars (list (char-code #\t) (char-code #\e)
                               (char-code #\s) (char-code #\t))))
        (ok (loop for i from 0 below (- (length buffer) 4)
                  thereis (and (= (aref buffer i) (first test-chars))
                              (= (aref buffer (+ i 1)) (second test-chars))
                              (= (aref buffer (+ i 2)) (third test-chars))
                              (= (aref buffer (+ i 3)) (fourth test-chars))))))))
  ;; Clean up
  (clysm/ffi:reset-ffi-environment))
