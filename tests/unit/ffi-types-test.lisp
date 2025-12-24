;;;; ffi-types-test.lisp - Unit tests for FFI type definitions (T004-T006)
;;;;
;;;; Tests for MarshalType, ForeignFunctionDecl, ExportDecl, and WasmImport structures

(in-package #:clysm/tests/unit/ffi-types)

;;; ============================================================
;;; T004: MarshalType Tests
;;; ============================================================

(deftest marshal-type-valid-types
  "Test that all valid marshal types are recognized."
  (testing "fixnum type"
    (ok (clysm/ffi:valid-marshal-type-p :fixnum)))
  (testing "float type"
    (ok (clysm/ffi:valid-marshal-type-p :float)))
  (testing "string type"
    (ok (clysm/ffi:valid-marshal-type-p :string)))
  (testing "boolean type"
    (ok (clysm/ffi:valid-marshal-type-p :boolean)))
  (testing "anyref type"
    (ok (clysm/ffi:valid-marshal-type-p :anyref)))
  (testing "void type"
    (ok (clysm/ffi:valid-marshal-type-p :void))))

(deftest marshal-type-invalid-types
  "Test that invalid types are rejected."
  (testing "unknown keyword"
    (ng (clysm/ffi:valid-marshal-type-p :unknown)))
  (testing "symbol"
    (ng (clysm/ffi:valid-marshal-type-p 'fixnum)))
  (testing "string"
    (ng (clysm/ffi:valid-marshal-type-p "fixnum")))
  (testing "nil"
    (ng (clysm/ffi:valid-marshal-type-p nil))))

;;; ============================================================
;;; T004: ForeignFunctionDecl Tests
;;; ============================================================

(deftest foreign-function-decl-creation
  "Test creating ForeignFunctionDecl structures."
  (let ((decl (clysm/ffi:make-foreign-function-decl
               :lisp-name 'host-log
               :module-name "host"
               :field-name "log"
               :param-types '(:string)
               :return-type :void)))
    (testing "lisp-name accessor"
      (ok (eq 'host-log (clysm/ffi:ffd-lisp-name decl))))
    (testing "module-name accessor"
      (ok (string= "host" (clysm/ffi:ffd-module-name decl))))
    (testing "field-name accessor"
      (ok (string= "log" (clysm/ffi:ffd-field-name decl))))
    (testing "param-types accessor"
      (ok (equal '(:string) (clysm/ffi:ffd-param-types decl))))
    (testing "return-type accessor"
      (ok (eq :void (clysm/ffi:ffd-return-type decl))))
    (testing "type-index initially nil"
      (ok (null (clysm/ffi:ffd-type-index decl))))))

(deftest foreign-function-decl-validation
  "Test ForeignFunctionDecl validation."
  (testing "valid declaration passes"
    (ok (clysm/ffi:validate-foreign-function-decl
         (clysm/ffi:make-foreign-function-decl
          :lisp-name 'host-add
          :module-name "host"
          :field-name "add"
          :param-types '(:fixnum :fixnum)
          :return-type :fixnum))))
  (testing "empty module name fails"
    (ok (handler-case
            (progn
              (clysm/ffi:validate-foreign-function-decl
               (clysm/ffi:make-foreign-function-decl
                :lisp-name 'bad
                :module-name ""
                :field-name "test"
                :param-types '()
                :return-type :void))
              nil)
          (error () t))))
  (testing "invalid param type fails"
    (ok (handler-case
            (progn
              (clysm/ffi:validate-foreign-function-decl
               (clysm/ffi:make-foreign-function-decl
                :lisp-name 'bad
                :module-name "host"
                :field-name "test"
                :param-types '(:invalid)
                :return-type :void))
              nil)
          (error () t)))))

;;; ============================================================
;;; T004: ExportDecl Tests
;;; ============================================================

(deftest export-decl-creation
  "Test creating ExportDecl structures."
  (let ((decl (clysm/ffi:make-export-decl
               :lisp-name 'calculate-tax
               :export-name "calculateTax"
               :param-types '(:fixnum)
               :return-type :fixnum)))
    (testing "lisp-name accessor"
      (ok (eq 'calculate-tax (clysm/ffi:ed-lisp-name decl))))
    (testing "export-name accessor"
      (ok (string= "calculateTax" (clysm/ffi:ed-export-name decl))))
    (testing "param-types accessor"
      (ok (equal '(:fixnum) (clysm/ffi:ed-param-types decl))))
    (testing "return-type accessor"
      (ok (eq :fixnum (clysm/ffi:ed-return-type decl))))
    (testing "wrapper-func-index initially nil"
      (ok (null (clysm/ffi:ed-wrapper-func-index decl))))))

(deftest export-decl-validation
  "Test ExportDecl validation."
  (testing "valid declaration passes"
    (ok (clysm/ffi:validate-export-decl
         (clysm/ffi:make-export-decl
          :lisp-name 'my-func
          :export-name "myFunc"
          :param-types '(:fixnum)
          :return-type :fixnum))))
  (testing "empty export name fails"
    (ok (handler-case
            (progn
              (clysm/ffi:validate-export-decl
               (clysm/ffi:make-export-decl
                :lisp-name 'bad
                :export-name ""
                :param-types '()
                :return-type :void))
              nil)
          (error () t)))))

;;; ============================================================
;;; T005: WasmImport Tests
;;; ============================================================

(deftest wasm-import-creation
  "Test creating WasmImport structures."
  (let ((import (clysm/ffi:make-wasm-import
                 :module-name "host"
                 :field-name "log"
                 :kind :func
                 :type-index 0)))
    (testing "module-name accessor"
      (ok (string= "host" (clysm/ffi:wi-module-name import))))
    (testing "field-name accessor"
      (ok (string= "log" (clysm/ffi:wi-field-name import))))
    (testing "kind accessor"
      (ok (eq :func (clysm/ffi:wi-kind import))))
    (testing "type-index accessor"
      (ok (= 0 (clysm/ffi:wi-type-index import))))))

;;; ============================================================
;;; T016: Host Name Parsing Tests
;;; ============================================================

(deftest parse-host-name-simple
  "Test parsing simple host names."
  (multiple-value-bind (module field)
      (clysm/ffi:parse-host-name "host.log")
    (testing "module name"
      (ok (string= "host" module)))
    (testing "field name"
      (ok (string= "log" field)))))

(deftest parse-host-name-nested
  "Test parsing nested host names."
  (multiple-value-bind (module field)
      (clysm/ffi:parse-host-name "env.console.log")
    (testing "module name (first segment)"
      (ok (string= "env" module)))
    (testing "field name (remaining segments)"
      (ok (string= "console.log" field)))))

(deftest parse-host-name-no-dot
  "Test parsing host names without dots."
  (testing "single segment defaults to env module"
    (multiple-value-bind (module field)
        (clysm/ffi:parse-host-name "log")
      (ok (string= "env" module))
      (ok (string= "log" field)))))

;;; ============================================================
;;; FFI Environment Tests
;;; ============================================================

(deftest ffi-environment-creation
  "Test creating FFIEnvironment structures."
  (let ((env (clysm/ffi:make-ffi-environment)))
    (testing "imports hash-table"
      (ok (hash-table-p (clysm/ffi:ffi-env-imports env))))
    (testing "exports list"
      (ok (null (clysm/ffi:ffi-env-exports env))))
    (testing "next-import-func-index"
      (ok (= 0 (clysm/ffi:ffi-env-next-import-func-index env))))
    (testing "type-cache hash-table"
      (ok (hash-table-p (clysm/ffi:ffi-env-type-cache env))))))

(deftest ffi-environment-register-foreign-function
  "Test registering foreign functions in environment."
  (let ((env (clysm/ffi:make-ffi-environment)))
    (clysm/ffi:register-foreign-function
     env
     (clysm/ffi:make-foreign-function-decl
      :lisp-name 'host-log
      :module-name "host"
      :field-name "log"
      :param-types '(:string)
      :return-type :void))
    (testing "function registered"
      (ok (clysm/ffi:lookup-foreign-function env 'host-log)))
    (testing "import index incremented"
      (ok (= 1 (clysm/ffi:ffi-env-next-import-func-index env))))))

;;; ============================================================
;;; FFI Condition Tests
;;; ============================================================

(deftest ffi-host-error-condition
  "Test FFI-HOST-ERROR condition."
  (let ((condition (make-condition 'clysm/ffi:ffi-host-error
                                   :function-name "host.log"
                                   :message "Host function not found")))
    (testing "function-name accessor"
      (ok (string= "host.log" (clysm/ffi:ffi-host-error-function-name condition))))
    (testing "message accessor"
      (ok (string= "Host function not found" (clysm/ffi:ffi-host-error-message condition))))))

(deftest ffi-type-error-condition
  "Test FFI-TYPE-ERROR condition."
  (let ((condition (make-condition 'clysm/ffi:ffi-type-error
                                   :expected-type :fixnum
                                   :actual-value "not a number")))
    (testing "expected-type accessor"
      (ok (eq :fixnum (clysm/ffi:ffi-type-error-expected-type condition))))
    (testing "actual-value accessor"
      (ok (string= "not a number" (clysm/ffi:ffi-type-error-actual-value condition))))))
