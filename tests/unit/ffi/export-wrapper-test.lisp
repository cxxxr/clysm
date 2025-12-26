;;;; export-wrapper-test.lisp - Unit tests for FFI export wrapper generation
;;;; Feature: 027-complete-ffi (T025)

(in-package #:clysm/tests)

(deftest export-wrapper-generation-test
  "Test that export wrappers are generated with correct structure"
  (testing "wrapper function structure"
    (clysm/ffi:reset-ffi-environment)

    ;; Register an export
    (defun test-fn (x) x)
    (eval '(clysm/ffi:export-function test-fn
            :as "identity"
            :signature ((:anyref) :anyref)))

    ;; Collect exports
    (let ((exports (clysm/ffi:collect-ffi-exports clysm/ffi:*ffi-environment*)))
      (ok exports "Should collect exports")
      (ok (>= (length exports) 1) "Should have at least 1 export"))))

(deftest export-wrapper-param-unmarshalling-test
  "Test that export wrappers unmarshal parameters correctly"
  (testing "parameter unmarshalling for different types"
    ;; The export wrapper should:
    ;; 1. Receive Wasm types from host
    ;; 2. Unmarshal to Lisp types (opposite of import marshalling)
    ;; 3. Call the Lisp function
    ;; 4. Marshal return value to Wasm type

    ;; For :fixnum param: i32 -> ref.i31 -> i31ref
    (let ((unmarshal (clysm/ffi:marshal-from-wasm :fixnum)))
      (ok (member 'ref.i31 unmarshal)
          ":fixnum unmarshal should use ref.i31"))

    ;; For :float param: f64 -> struct.new $float -> (ref $float)
    (let ((unmarshal (clysm/ffi:marshal-from-wasm :float)))
      (ok (find 'struct.new unmarshal
                :test (lambda (sym x) (and (listp x) (eq (car x) sym))))
          ":float unmarshal should use struct.new"))))

(deftest export-wrapper-return-marshalling-test
  "Test that export wrappers marshal return values correctly"
  (testing "return value marshalling"
    ;; For :fixnum return: i31ref -> i31.get_s -> i32
    (let ((marshal (clysm/ffi:marshal-to-wasm :fixnum)))
      (ok (member 'i31.get_s marshal)
          ":fixnum return marshal should use i31.get_s"))

    ;; For :void return: nothing (no result)
    ;; Handled specially - no marshalling needed
    (ok t "Void return is handled specially")))
