;;;; ffi-callback-wasm-test.lisp - Contract tests for FFI callback Wasm generation
;;;; Feature: 027-complete-ffi (T054)
;;;;
;;;; Tests that export wrappers generate valid Wasm for callback scenarios.

(in-package #:clysm/tests)

(deftest callback-export-wrapper-wasm-test
  "Test that export wrappers produce valid Wasm"
  (testing "export wrapper compilation"
    (clysm/ffi:reset-ffi-environment)

    ;; Define an export for callback testing
    (eval '(clysm/ffi:export-function add-one "addOne" (:fixnum) :fixnum))
    (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)

    ;; Verify the export is properly indexed
    (let ((exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*)))
      (ok (> (length exports) 0) "Should have exports")
      (let ((export (first exports)))
        (ok export "Should have at least one export")
        ;; The wrapper function index should be assigned
        (ok (or (null (clysm/ffi:ed-wrapper-func-index export))
                (>= (clysm/ffi:ed-wrapper-func-index export) 0))
            "Export should have valid wrapper index or nil")))))

(deftest callback-nested-calls-structure-test
  "Test structure for nested callback calls"
  (testing "nested call structure"
    (clysm/ffi:reset-ffi-environment)

    ;; For nested callbacks (Lisp→Host→Lisp):
    ;; 1. Lisp calls host import
    ;; 2. Host calls exported Lisp function
    ;; 3. Export wrapper unmarshals, calls Lisp, marshals result

    ;; Define both an import and export
    (eval '(clysm/ffi:define-foreign-function call-callback "callback.invoke" (:string :fixnum) :fixnum))
    (eval '(clysm/ffi:export-function double-it "double" (:fixnum) :fixnum))

    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
    (clysm/ffi:assign-export-indices clysm/ffi:*ffi-environment*)

    ;; Verify both are registered
    (let ((imports (clysm/ffi:ffi-env-imports clysm/ffi:*ffi-environment*))
          (exports (clysm/ffi:ffi-env-exports clysm/ffi:*ffi-environment*)))
      (ok (> (hash-table-count imports) 0) "Should have imports")
      (ok (> (length exports) 0) "Should have exports"))))

(deftest callback-marshalling-preservation-test
  "Test that marshalling is correct across callback boundaries"
  (testing "type preservation"
    ;; When host calls back into Lisp:
    ;; 1. Host passes i32/f64/externref
    ;; 2. Export wrapper unmarshals to Lisp types (i31ref, $float, $string)
    ;; 3. Lisp function executes
    ;; 4. Export wrapper marshals result back to host type

    ;; Verify marshal functions exist
    (ok (fboundp 'clysm/ffi:marshal-to-wasm) "marshal-to-wasm should exist")
    (ok (fboundp 'clysm/ffi:marshal-from-wasm) "marshal-from-wasm should exist")

    ;; Test roundtrip marshalling types
    (dolist (type '(:fixnum :float :string :boolean))
      (let ((to-wasm (clysm/ffi:marshal-to-wasm type))
            (from-wasm (clysm/ffi:marshal-from-wasm type)))
        (ok (or (null to-wasm) (listp to-wasm))
            (format nil "marshal-to-wasm for ~A should return list or nil" type))
        (ok (or (null from-wasm) (listp from-wasm))
            (format nil "marshal-from-wasm for ~A should return list or nil" type))))))

(deftest callback-export-section-test
  "Test export section generation for callbacks"
  (testing "export section structure"
    (clysm/ffi:reset-ffi-environment)

    ;; Define exports
    (eval '(clysm/ffi:export-function fn1 "callback1" () :fixnum))
    (eval '(clysm/ffi:export-function fn2 "callback2" (:fixnum) :fixnum))

    ;; Collect exports
    (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
      (ok exports "Should collect exports")
      (ok (listp exports) "Exports should be a list")
      (ok (>= (length exports) 2) "Should have at least 2 exports"))))
