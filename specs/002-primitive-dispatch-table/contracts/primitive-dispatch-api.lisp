;;; Primitive Dispatch API Contract
;;; Feature: 002-primitive-dispatch-table
;;; Date: 2026-01-03
;;;
;;; This file defines the public API contract for the primitive dispatch system.
;;; Implementation exists in src/clysm/compiler/codegen/primitive-dispatch.lisp

;;; ============================================================================
;;; Type Definitions
;;; ============================================================================

;; Compiler function signature: (args env) -> instruction-list
;; - args: list of AST nodes (primitive arguments)
;; - env: lexical-env structure (compilation environment)
;; - returns: list of Wasm instruction s-expressions

;;; ============================================================================
;;; Registration API (FR-003, FR-005)
;;; ============================================================================

;; register-primitive-compiler symbol compiler-fn &key arity flags string-name
;;
;; Register a primitive compiler function in the dispatch table.
;;
;; Parameters:
;;   symbol      - Symbol naming the primitive (required, MUST NOT be NIL)
;;   compiler-fn - Code generator function (required, MUST NOT be NIL)
;;   :arity      - Expected argument count (optional, NIL = variadic)
;;   :flags      - Metadata plist (optional)
;;   :string-name - Alternate string key for cross-package lookup (optional)
;;
;; Returns: The symbol (for chaining)
;;
;; Errors:
;;   - Signals error if symbol is NIL (FR-009)
;;   - Signals error if compiler-fn is NIL (FR-009)
;;   - Signals warning if symbol already registered (FR-008)
;;
;; Examples:
;;   (register-primitive-compiler 'car #'compile-car :arity 1)
;;   (register-primitive-compiler 'cons #'compile-cons :arity 2)
;;   (register-primitive-compiler '+ #'compile-+ :arity nil)  ; variadic

;;; ============================================================================
;;; Dispatch API (FR-001, FR-002, FR-010)
;;; ============================================================================

;; dispatch-primitive op args env
;;
;; Look up and invoke the code generator for a primitive.
;;
;; Parameters:
;;   op   - Primitive operator symbol
;;   args - List of AST argument nodes
;;   env  - Compilation environment
;;
;; Returns:
;;   - instruction-list if primitive found and compiled
;;   - NIL if primitive not registered (FR-010)
;;
;; Complexity: O(1) average case (FR-001)
;;
;; Lookup order:
;;   1. Symbol table (eq test)
;;   2. String table (equal test on symbol-name)

;; lookup-primitive-compiler op
;;
;; Look up a primitive entry without invoking the compiler.
;;
;; Parameters:
;;   op - Primitive operator symbol or string
;;
;; Returns:
;;   - primitive-entry structure if found
;;   - NIL if not registered

;;; ============================================================================
;;; Query API (FR-006, FR-007)
;;; ============================================================================

;; primitive-registered-p name
;;
;; Check if a primitive is registered.
;;
;; Parameters:
;;   name - Symbol or string naming the primitive
;;
;; Returns:
;;   - T if registered
;;   - NIL if not registered

;; list-registered-primitives &key category
;;
;; Enumerate all registered primitives.
;;
;; Parameters:
;;   :category - Optional filter by category flag (default: all)
;;
;; Returns:
;;   - List of primitive symbols

;; get-primitive-info name
;;
;; Retrieve metadata for a registered primitive.
;;
;; Parameters:
;;   name - Symbol or string naming the primitive
;;
;; Returns:
;;   - primitive-entry structure if found
;;   - NIL if not registered

;;; ============================================================================
;;; Data Structure Contract
;;; ============================================================================

;; (defstruct primitive-entry
;;   (compiler-fn nil :type (or function null))
;;   (arity nil :type (or fixnum null))
;;   (flags nil :type list))
;;
;; Fields:
;;   compiler-fn - The code generator function
;;   arity       - Expected argument count, NIL for variadic
;;   flags       - Property list with metadata:
;;                 :category - keyword categorizing the primitive
;;                 :doc      - documentation string
;;                 :since    - version introduced

;;; ============================================================================
;;; Constants
;;; ============================================================================

;; +primitive-symbol-table-size+ = 512
;; Initial size for symbol dispatch table (power of 2 above primitive count)

;; +primitive-string-table-size+ = 64
;; Initial size for string dispatch table (cross-package symbols only)

;;; ============================================================================
;;; Usage Contract
;;; ============================================================================

;; Registration Phase (load time):
;;   1. Load primitive-dispatch.lisp (infrastructure)
;;   2. Load primitive-registry.lisp (registrations)
;;   3. All primitives registered before first compilation
;;
;; Compilation Phase (runtime):
;;   1. compile-primitive-call invokes dispatch-primitive
;;   2. dispatch-primitive returns instructions or NIL
;;   3. NIL triggers fallback to general function call

;;; ============================================================================
;;; Backward Compatibility Contract (FR-004)
;;; ============================================================================

;; Guarantee: Wasm output from dispatch-based compilation MUST be
;; byte-identical to case-statement based compilation for all 248
;; existing primitives.
;;
;; Verification: Binary diff of compiled test suite before/after migration.
