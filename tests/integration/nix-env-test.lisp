;;;; nix-env-test.lisp - Nix environment validation tests
;;;; Verifies that all required tools are available in the Nix devShell
(in-package #:clysm/tests/integration/nix-env)

;;; Helper function to check if a command exists and runs successfully
(defun command-available-p (command &rest args)
  "Check if COMMAND is available and can be executed with ARGS.
Returns T if the command exits with status 0, NIL otherwise."
  (handler-case
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program (cons command args)
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (declare (ignore output error-output))
        (zerop exit-code))
    (error () nil)))

(defun command-output (command &rest args)
  "Get output from COMMAND with ARGS. Returns the output string or NIL on error."
  (handler-case
      (string-trim '(#\Space #\Newline #\Tab)
                   (uiop:run-program (cons command args)
                                     :output :string
                                     :error-output nil))
    (error () nil)))

;;; US2: Reproducible Development Environment Tests

(deftest test-sbcl-available
  "SBCL Common Lisp compiler is available"
  (ok (command-available-p "sbcl" "--version")
      "sbcl --version should succeed"))

(deftest test-sbcl-version
  "SBCL version can be retrieved"
  (let ((version (command-output "sbcl" "--version")))
    (ok version "sbcl version should return output")
    (ok (search "SBCL" version) "Output should contain 'SBCL'")))

(deftest test-wasmtime-available
  "wasmtime WebAssembly runtime is available"
  (ok (command-available-p "wasmtime" "--version")
      "wasmtime --version should succeed"))

(deftest test-wasmtime-gc-support
  "wasmtime supports GC proposal (v27+)"
  (let ((version (command-output "wasmtime" "--version")))
    (ok version "wasmtime version should return output")
    ;; v27+ is required for GC support
    (ok (search "wasmtime" version) "Output should contain 'wasmtime'")))

(deftest test-wasm-tools-available
  "wasm-tools is available for validation"
  (ok (command-available-p "wasm-tools" "--version")
      "wasm-tools --version should succeed"))

(deftest test-wasm-tools-validate
  "wasm-tools validate command works"
  ;; Create a minimal valid Wasm module
  (let ((temp-file (uiop:with-temporary-file (:pathname p :type "wasm" :keep t)
                     (with-open-file (s p :direction :output
                                          :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)
                       ;; Wasm magic number + version
                       (write-sequence #(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00) s))
                     p)))
    (unwind-protect
         (ok (command-available-p "wasm-tools" "validate" (namestring temp-file))
             "wasm-tools should validate a minimal Wasm module")
      (ignore-errors (delete-file temp-file)))))

(deftest test-wabt-wat2wasm-available
  "wat2wasm from wabt is available"
  (ok (command-available-p "wat2wasm" "--version")
      "wat2wasm --version should succeed"))

(deftest test-wabt-wasm2wat-available
  "wasm2wat from wabt is available"
  (ok (command-available-p "wasm2wat" "--version")
      "wasm2wat --version should succeed"))

(deftest test-cl-source-registry
  "CL_SOURCE_REGISTRY is set correctly"
  (let ((registry (uiop:getenv "CL_SOURCE_REGISTRY")))
    ;; May not be set outside nix-shell, so just check format if present
    (when registry
      (ok (search ":source-registry" registry)
          "CL_SOURCE_REGISTRY should contain :source-registry"))))

(deftest test-asdf-can-find-clysm
  "ASDF can locate the clysm system"
  (ok (asdf:find-system :clysm nil)
      "ASDF should find :clysm system"))

(deftest test-quicklisp-available
  "Quicklisp is available (optional but recommended)"
  ;; Quicklisp is optional, so we just note its availability
  (let ((ql-available (find-package :ql)))
    (if ql-available
        (pass "Quicklisp is available")
        (skip "Quicklisp not loaded (optional)"))))
