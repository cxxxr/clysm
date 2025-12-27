;;;; Macro API Contract Definitions
;;;; Feature: 042-advanced-defmacro
;;;; Date: 2025-12-28
;;;;
;;;; This file defines the function signatures and contracts for the
;;;; advanced defmacro feature. These contracts serve as the specification
;;;; for implementation and testing.

(in-package :clysm/contracts)

;;; ==========================================================================
;;; SECTION 1: Macro Lambda-List Parsing
;;; ==========================================================================

(defcontract parse-macro-lambda-list
    "Parse a macro lambda-list, extracting &whole and &environment parameters.

     ANSI CL Reference: Section 3.4.4 (Macro Lambda Lists)

     Arguments:
       LAMBDA-LIST - A macro lambda-list form

     Returns:
       macro-lambda-list-info struct with fields:
         - whole-var: symbol or nil
         - env-var: symbol or nil
         - required: list of required params
         - optional: list of optional params
         - rest-var: symbol or nil
         - rest-kind: :rest or :body or nil
         - keys: list of keyword params
         - allow-other-keys: boolean

     Preconditions:
       - lambda-list is a proper list
       - If &whole present, it must be first
       - &environment may appear anywhere

     Postconditions:
       - All &-parameters extracted and removed from remaining list
       - Original ordering of required params preserved

     Errors:
       MACRO-LAMBDA-LIST-MALFORMED if:
         - &whole appears after other parameters
         - &whole or &environment lacks following variable"
    (lambda-list)
  (declare (type list lambda-list))
  (values macro-lambda-list-info))

;;; ==========================================================================
;;; SECTION 2: Macro Environment
;;; ==========================================================================

(defcontract make-macro-environment
    "Create a new macro expansion environment.

     Arguments:
       &key LOCAL-MACROS - macro-registry for locally defined macros
       &key PARENT - parent macro-environment (for lexical nesting)

     Returns:
       A fresh macro-environment struct

     Postconditions:
       - local-macros slot set (or nil if not provided)
       - parent slot set (or nil if not provided)"
    (&key local-macros parent)
  (declare (type (or null macro-registry) local-macros)
           (type (or null macro-environment) parent))
  (values macro-environment))

(defcontract env-macro-function
    "Look up a macro function in the environment chain.

     Arguments:
       ENV - macro-environment to search
       NAME - symbol naming the macro

     Returns:
       The macro expander function, or NIL if not found

     Search Order:
       1. ENV's local-macros registry
       2. ENV's parent chain (recursively)
       3. Global macro registry (*global-macro-registry*)

     Postconditions:
       - Returns a function or nil
       - Does not modify ENV"
    (env name)
  (declare (type (or null macro-environment) env)
           (type symbol name))
  (values (or function null)))

;;; ==========================================================================
;;; SECTION 3: Core Macro Functions (ANSI CL)
;;; ==========================================================================

(defcontract macro-function
    "Return the macro function associated with SYMBOL.

     ANSI CL Reference: Function MACRO-FUNCTION

     Arguments:
       SYMBOL - a symbol that may name a macro
       &optional ENVIRONMENT - a macro-environment or nil

     Returns:
       The macro expander function, or NIL if SYMBOL is not a macro

     Notes:
       - When ENV is nil, searches only global registry
       - When ENV is provided, searches env chain then global
       - The returned function accepts (form &optional env)"
    (symbol &optional environment)
  (declare (type symbol symbol)
           (type (or null macro-environment) environment))
  (values (or function null)))

(defcontract (setf macro-function)
    "Set the macro function for SYMBOL.

     ANSI CL Reference: (SETF MACRO-FUNCTION)

     Arguments:
       NEW-FUNCTION - the new macro expander function
       SYMBOL - a symbol to associate with the macro
       &optional ENVIRONMENT - must be nil (global only)

     Returns:
       NEW-FUNCTION

     Preconditions:
       - ENVIRONMENT must be nil (ANSI CL restriction)
       - NEW-FUNCTION must accept (form &optional env)

     Postconditions:
       - (macro-function SYMBOL) returns NEW-FUNCTION
       - Any previous macro function is replaced

     Errors:
       - Error if ENVIRONMENT is non-nil"
    (new-function symbol &optional environment)
  (declare (type function new-function)
           (type symbol symbol)
           (type null environment))
  (values function))

(defcontract macroexpand-1
    "Expand FORM once if it is a macro call.

     ANSI CL Reference: Function MACROEXPAND-1

     Arguments:
       FORM - any Lisp form
       &optional ENVIRONMENT - macro-environment for local macros

     Returns:
       Two values:
         1. The expanded form (or FORM unchanged)
         2. T if expansion occurred, NIL otherwise

     Algorithm:
       1. If FORM is not a cons, return (values FORM nil)
       2. If (car FORM) is not a symbol, return (values FORM nil)
       3. Get macro-function for (car FORM) in ENVIRONMENT
       4. If no macro-function, return (values FORM nil)
       5. Call (funcall macro-fn FORM ENVIRONMENT)
       6. Return (values result t)

     Notes:
       - Does not recursively expand
       - Preserves FORM identity when no expansion occurs"
    (form &optional environment)
  (declare (type t form)
           (type (or null macro-environment) environment))
  (values t boolean))

(defcontract macroexpand
    "Repeatedly expand FORM until it is no longer a macro call.

     ANSI CL Reference: Function MACROEXPAND

     Arguments:
       FORM - any Lisp form
       &optional ENVIRONMENT - macro-environment for local macros

     Returns:
       Two values:
         1. The fully expanded form
         2. T if any expansion occurred, NIL otherwise

     Algorithm:
       1. Call macroexpand-1 on FORM
       2. If no expansion, return (values FORM ever-expanded-p)
       3. If expansion, increment depth counter
       4. If depth >= 1000, signal MACRO-EXPANSION-DEPTH-EXCEEDED
       5. Goto 1 with expanded form

     Errors:
       MACRO-EXPANSION-DEPTH-EXCEEDED if >1000 expansions occur"
    (form &optional environment)
  (declare (type t form)
           (type (or null macro-environment) environment))
  (values t boolean))

;;; ==========================================================================
;;; SECTION 4: Defmacro Compilation
;;; ==========================================================================

(defcontract compile-defmacro
    "Compile a defmacro form into a macro expander function.

     Arguments:
       RESULT - a defmacro-result struct from parse-defmacro

     Returns:
       A function suitable for registration as a macro expander

     Generated Function Behavior:
       1. Binds &whole variable to entire form (if specified)
       2. Binds &environment variable to env argument (if specified)
       3. Destructures (cdr form) against remaining lambda-list
       4. Evaluates body in binding environment
       5. Returns the expansion result

     Postconditions:
       - Returned function accepts (form &optional env)
       - Function is registered in *global-macro-registry*"
    (result)
  (declare (type defmacro-result result))
  (values function))

;;; ==========================================================================
;;; SECTION 5: Conditions
;;; ==========================================================================

(define-condition macro-lambda-list-malformed (program-error)
  ((lambda-list :initarg :lambda-list :reader mlf-lambda-list)
   (reason :initarg :reason :reader mlf-reason))
  (:report (lambda (c s)
             (format s "Malformed macro lambda-list ~S: ~A"
                     (mlf-lambda-list c) (mlf-reason c))))
  (:documentation
   "Signaled when a macro lambda-list violates ANSI CL syntax rules."))

(define-condition macro-expansion-depth-exceeded (error)
  ((form :initarg :form :reader mede-form)
   (depth :initarg :depth :reader mede-depth :initform 1000))
  (:report (lambda (c s)
             (format s "Macro expansion depth exceeded (~D steps) for form ~S"
                     (mede-depth c) (mede-form c))))
  (:documentation
   "Signaled when macroexpand exceeds the 1000-step limit.
    This typically indicates circular macro definitions."))

(define-condition undefined-macro-function (error)
  ((name :initarg :name :reader umf-name))
  (:report (lambda (c s)
             (format s "No macro function defined for ~S" (umf-name c))))
  (:documentation
   "Signaled when attempting to expand an undefined macro.
    Note: macroexpand-1 returns the form unchanged rather than signaling.
    This condition is for explicit macro-function access with error checking."))

;;; ==========================================================================
;;; SECTION 6: Runtime Wasm Interface
;;; ==========================================================================

(defcontract runtime-macro-function
    "Wasm-callable wrapper for macro-function lookup.

     Used by compiled code to query macro registry at runtime.

     Arguments:
       SYMBOL-REF - anyref pointing to a Wasm symbol struct

     Returns:
       anyref pointing to closure, or null if not a macro

     Notes:
       - Searches global registry only (env not supported at runtime)
       - Returns null for undefined macros (no error)"
    (symbol-ref)
  (values anyref))

(defcontract runtime-macroexpand-1
    "Wasm-callable single-step macro expansion.

     Arguments:
       FORM-REF - anyref pointing to a cons cell (the macro call)

     Returns:
       Two anyref values via multiple-value protocol:
         1. Expanded form (or original if not a macro)
         2. Boolean (i31ref 1 for T, null for NIL)

     Notes:
       - Uses runtime-macro-function for lookup
       - Invokes macro closure via call_ref"
    (form-ref)
  (values anyref anyref))

(defcontract runtime-macroexpand
    "Wasm-callable full macro expansion.

     Arguments:
       FORM-REF - anyref pointing to a form

     Returns:
       Two anyref values via multiple-value protocol:
         1. Fully expanded form
         2. Boolean indicating if any expansion occurred

     Errors:
       Signals macro-expansion-depth-exceeded after 1000 steps"
    (form-ref)
  (values anyref anyref))

;;; ==========================================================================
;;; END OF CONTRACT DEFINITIONS
;;; ==========================================================================
