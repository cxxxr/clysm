;;;; stage0-complete.lisp - Bootstrap script for complete Stage 0 compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Task T038: Create build/stage0-complete.lisp bootstrap script
;;;;
;;;; Usage:
;;;;   sbcl --load build/stage0-complete.lisp
;;;;
;;;; This generates dist/clysm-stage0.wasm - a complete Stage 0 compiler
;;;; that exports compile_form and compile_all functions.

(require :asdf)

(format t "~&=== Stage 0 Complete Compiler Bootstrap ===~%")
(format t "~&Loading Clysm system...~%")

;; Load the Clysm system
(asdf:load-system :clysm)

(format t "~&Clysm system loaded.~%")

;; Import the stage0 package
(use-package :clysm/stage0)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defparameter *output-path*
  (merge-pathnames "dist/clysm-stage0.wasm"
                   (asdf:system-source-directory :clysm))
  "Output path for Stage 0 binary")

(defparameter *verbose* t
  "Enable verbose output")

;;; ============================================================
;;; Bootstrap Process
;;; ============================================================

(defun generate-stage0-complete ()
  "Generate complete Stage 0 Wasm binary.

   This creates a Wasm module that:
   1. Contains WasmGC type definitions (24+ types)
   2. Initializes NIL, UNBOUND, and other globals
   3. Exports compile_form for single expression compilation
   4. Exports compile_all for full source compilation
   5. Exports _initialize for runtime setup"
  (format t "~&~%Phase 1: Generating WasmGC types...~%")
  (let ((type-section (clysm/stage0:generate-type-section)))
    (format t "  Type section: ~D bytes~%" (length type-section))

    (format t "~&Phase 2: Generating global section...~%")
    (let ((global-section (clysm/stage0:generate-global-section)))
      (format t "  Global section: ~D bytes~%" (length global-section))

      (format t "~&Phase 3: Generating FFI imports...~%")
      (let ((import-section (clysm/stage0:generate-ffi-imports)))
        (format t "  Import section: ~D bytes~%" (length import-section))

        (format t "~&Phase 4: Generating runtime module...~%")
        (let ((wasm-bytes (clysm/stage0:generate-runtime-init)))
          (format t "  Runtime module: ~D bytes~%" (length wasm-bytes))

          (format t "~&Phase 5: Writing output...~%")
          (ensure-directories-exist *output-path*)
          (with-open-file (out *output-path*
                               :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
            (write-sequence wasm-bytes out))

          (format t "  Output: ~A~%" *output-path*)
          (format t "  Size: ~D bytes~%" (length wasm-bytes))

          (format t "~&Phase 6: Validating Wasm...~%")
          (let ((valid-p (clysm/stage0:validate-wasm-bytes wasm-bytes)))
            (format t "  Validation: ~A~%" (if valid-p "PASSED" "FAILED"))

            (format t "~&~%=== Bootstrap Complete ===~%")
            (format t "~&Output: ~A~%" *output-path*)
            (format t "~&Size: ~D bytes~%" (length wasm-bytes))
            (format t "~&Valid: ~A~%" (if valid-p "Yes" "No"))

            valid-p))))))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

(format t "~&Starting Stage 0 generation...~%")
(let ((success (generate-stage0-complete)))
  (if success
      (progn
        (format t "~&~%Stage 0 generation successful!~%")
        (format t "~%Next steps:~%")
        (format t "  1. wasm-tools validate ~A~%" *output-path*)
        (format t "  2. wasmtime run ~A --invoke compile_form '(+ 1 2)'~%" *output-path*)
        (sb-ext:exit :code 0))
      (progn
        (format t "~&~%Stage 0 generation failed!~%")
        (sb-ext:exit :code 1))))
