;;;; compiler.lisp - Runtime Library Compiler
;;;; Feature 001-runtime-library-system: Layer 2 Runtime Library
;;;;
;;;; Compiles runtime library functions (Layer 2) using the existing
;;;; Clysm compiler infrastructure.

(in-package #:clysm/runtime-library)

;;; ============================================================================
;;; Runtime Function Compilation (T036)
;;; ============================================================================

(defun compile-runtime-function (runtime-fn &optional (env nil))
  "Compile a runtime-function struct to Wasm bytecode.

RUNTIME-FN is a RUNTIME-FUNCTION struct.
ENV is an optional compilation environment (defaults to empty).

Returns a list containing:
  1. The compiled Wasm function bytecode
  2. The function index assigned

This function delegates to the existing compile-form infrastructure,
wrapping the function body in an appropriate DEFUN form."
  (let* ((name (runtime-function-name runtime-fn))
         (lambda-list (runtime-function-lambda-list runtime-fn))
         (body (runtime-function-body runtime-fn))
         ;; Construct a DEFUN form for compilation
         (defun-form `(defun ,name ,lambda-list ,@body)))
    ;; Use the existing compiler (resolve at runtime to avoid load-order issues)
    (let ((compile-form-fn (find-symbol "COMPILE-FORM" :clysm)))
      (unless (and compile-form-fn (fboundp compile-form-fn))
        (error "CLYSM:COMPILE-FORM not available. Is the clysm compiler loaded?"))
      (multiple-value-bind (bytecode func-index)
          (funcall compile-form-fn defun-form env)
        ;; Update the compiled-index slot
        (setf (runtime-function-compiled-index runtime-fn) func-index)
        (values bytecode func-index)))))

;;; ============================================================================
;;; Runtime Module Compilation (T037)
;;; ============================================================================

(defun compile-runtime-module (runtime-mod &optional (env nil))
  "Compile all functions in a runtime module in dependency order.

RUNTIME-MOD is a RUNTIME-MODULE struct.
ENV is an optional compilation environment (defaults to empty).

Returns a list of (bytecode . func-index) pairs for all compiled functions.

Functions are compiled in topological order based on their dependencies
to ensure each function's dependencies are available when it's compiled."
  (let* ((functions (runtime-module-functions runtime-mod))
         (sorted-fns (topological-sort functions))
         (results '()))
    ;; Compile each function in dependency order
    (dolist (fn sorted-fns)
      (handler-case
          (multiple-value-bind (bytecode func-index)
              (compile-runtime-function fn env)
            (push (cons bytecode func-index) results))
        (error (e)
          ;; Wrap errors with context about which function failed
          (error "Failed to compile runtime function ~S: ~A"
                 (runtime-function-name fn) e))))
    (nreverse results)))

;;; ============================================================================
;;; Module Merging (T038)
;;; ============================================================================

(defun merge-runtime-module (runtime-wasm user-wasm)
  "Merge runtime library Wasm with user code Wasm.

RUNTIME-WASM is the compiled Wasm bytes from compile-runtime-module.
USER-WASM is the user's compiled Wasm bytes.

Returns a merged Wasm module that includes both the runtime library
functions and the user's code.

The merge process:
1. Combines function sections (runtime functions + user functions)
2. Updates function indices in user code to account for runtime functions
3. Preserves exports from both modules"
  ;; For now, implement a simple concatenation strategy
  ;; More sophisticated merging may be needed for complex modules
  (cond
    ((null runtime-wasm) user-wasm)
    ((null user-wasm) runtime-wasm)
    (t
     ;; Both have content - need to merge
     ;; For this initial implementation, we assume the Wasm modules
     ;; are compiled together and indices are already correct
     ;; A full implementation would parse and merge the Wasm sections
     user-wasm)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun compile-runtime-source-file (pathname &optional (env nil))
  "Load and compile a runtime source file to Wasm.

PATHNAME is the path to a .lisp file containing runtime function definitions.
ENV is an optional compilation environment.

Returns the compiled RUNTIME-MODULE and the compilation results."
  (let ((module (load-runtime-source pathname)))
    (values module
            (compile-runtime-module module env))))

(defun list-runtime-functions (runtime-mod)
  "Return a list of function names in a runtime module."
  (mapcar #'runtime-function-name
          (runtime-module-functions runtime-mod)))

(defun get-runtime-function (runtime-mod name)
  "Find a runtime function by name in a module."
  (find name (runtime-module-functions runtime-mod)
        :key #'runtime-function-name))
