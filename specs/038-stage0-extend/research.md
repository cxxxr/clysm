# Research: Stage 0 Capability Extension

**Date**: 2025-12-27
**Branch**: 038-stage0-extend

## Research Tasks

### R1: Wasm Global Implementation for Constants

**Question**: How should defconstant/defparameter values be represented as Wasm globals?

**Decision**: Use WasmGC `anyref` globals with appropriate mutability flags.

**Rationale**:
- Wasm globals support primitive types (i32, i64, f32, f64) and reference types (anyref, externref)
- Lisp constants may be fixnums (→ i31ref), floats (→ f64 wrapped), symbols, or strings
- Using `anyref` as the global type provides uniform handling for all Lisp value types
- defconstant → immutable global (`(global $name (ref $type) ...)`)
- defparameter → mutable global (`(global $name (mut anyref) ...)`)

**Alternatives Considered**:
- Specialized globals per type (i32 for fixnums): Rejected - would require type-specific code paths and lose uniformity
- Store in symbol's value slot only: Rejected - would require symbol lookup overhead at every reference

**Implementation Notes**:
```wat
;; defconstant example
(global $+max-stack+ (ref i31) (i31.new (i32.const 1000)))

;; defparameter example
(global $*debug-level* (mut anyref) (ref.i31 (i32.const 0)))
```

### R2: Constant Folding Scope

**Question**: Which expressions should be constant-folded during bootstrap compilation?

**Decision**: Fold arithmetic operations (+, -, *, /, mod, rem) on literals and known constants.

**Rationale**:
- Common Lisp allows arbitrary expressions in defconstant init forms
- However, Clysm's compiler source uses primarily arithmetic for constants
- Supporting only arithmetic keeps implementation simple while covering most cases

**Supported Operations**:
- Arithmetic: `+`, `-`, `*`, `/`, `mod`, `rem`
- Comparison: `<`, `>`, `<=`, `>=`, `=`, `/=` (for compile-time conditionals)
- Logical: `and`, `or`, `not` (short-circuit evaluation)
- References: Previously defined constants, literals

**Unsupported (error if encountered)**:
- Function calls (except arithmetic operators)
- Complex forms (let, lambda, etc.)
- Runtime-dependent values

### R3: define-condition Expansion Strategy

**Question**: How should define-condition be expanded to defclass?

**Decision**: Simple syntactic transformation with :report option handling.

**Rationale**:
- ANSI CL specifies define-condition creates classes with condition as superclass
- The macro primarily adds :report handling and condition-specific defaults
- Clysm's CLOS foundation (Feature 026) already supports defclass

**Transformation Rules**:
```lisp
;; Input
(define-condition my-error (error)
  ((message :initarg :message :reader my-error-message))
  (:report (lambda (c s) (format s "~A" (my-error-message c)))))

;; Output (for basic support)
(defclass my-error (error)
  ((message :initarg :message :reader my-error-message)))
;; :report option is recorded but not compiled (runtime feature)
```

**:report Option Handling**:
- For Stage 0, skip :report - it requires runtime format/print infrastructure
- Record in a side registry for future implementation

### R4: Declaration Handling Strategy

**Question**: How should declare forms be handled without causing compilation failure?

**Decision**: Filter out declare forms during AST parsing (pre-compilation).

**Rationale**:
- Declare forms are informational hints for the compiler
- ANSI CL does not require them to affect code generation
- Filtering during parsing is simpler than adding declare-aware compilation

**Implementation Approach**:
1. In `parse-defun-form`: Filter (declare ...) from body before parsing
2. In `parse-let-form`: Filter (declare ...) from body before parsing
3. In `parse-lambda-form`: Same treatment
4. Top-level proclaim: Add to skip list in bootstrap.lisp

**Forms Affected**:
- `defun` body
- `let` / `let*` body
- `lambda` body
- `flet` / `labels` function bodies
- `do` / `do*` body

### R5: defstruct Expansion Strategy

**Question**: How should defstruct be expanded without full CLOS integration?

**Decision**: Expand to constructor function + accessor functions using simple vector backend.

**Rationale**:
- Full CLOS-based structures require metaclass infrastructure
- For Stage 0, we need only constructor/accessor functionality
- Vector-backed structures are simpler and sufficient for compiler internals

**Expansion Pattern**:
```lisp
;; Input
(defstruct point x y)

;; Output
(defun make-point (&key x y)
  (vector 'point x y))

(defun point-x (obj)
  (svref obj 1))

(defun point-y (obj)
  (svref obj 2))

(defun point-p (obj)
  (and (vectorp obj)
       (> (length obj) 0)
       (eq (svref obj 0) 'point)))
```

**Slot Defaults**:
```lisp
;; Input
(defstruct point (x 0) (y 0))

;; Constructor becomes
(defun make-point (&key (x 0) (y 0))
  (vector 'point x y))
```

**:constructor Option**:
```lisp
;; Input
(defstruct (point (:constructor create-point)) x y)

;; Output uses create-point instead of make-point
(defun create-point (&key x y)
  (vector 'point x y))
```

### R6: Current Form Distribution Analysis

**Question**: What is the distribution of form types in the compiler source?

**Decision**: Analyzed the 41 modules to identify blocking form types.

**Form Distribution (from bootstrap.lisp filtering)**:
Based on current `compilable-form-p` acceptance:
- Accepted: defun, defmacro, defvar, defparameter, defclass, defgeneric, defmethod, etc.
- Rejected: defconstant, defstruct, define-condition, declare, proclaim

**Estimated Impact of This Feature**:
| Form Type | Count (est.) | After This Feature |
|-----------|--------------|-------------------|
| defconstant | 50-100 | ✅ Compiled |
| defstruct | 20-40 | ✅ Expanded + Compiled |
| define-condition | 10-20 | ✅ Expanded + Compiled |
| Functions with declare | 100-200 | ✅ Compiled (declare skipped) |
| Other blockers | ~400 | Still blocked |

**Estimated New Compilation Rate**:
- Current: 14/849 = 1.6%
- After feature: ~450/849 = ~53%

### R7: Error Reporting Enhancement

**Question**: What information should the error report provide?

**Decision**: Operator-grouped failure counts with form preview and percentage.

**Report Format**:
```text
=== Compilation Statistics ===

Compiled: 453/849 forms (53.4%)

Failures by Operator:
  handler-case: 45 failures
  loop: 38 failures
  do: 25 failures
  ...

Sample Failures:
  [handler-case] (handler-case (foo) (error (e)...
  [loop] (loop for x in items collect...
```

**Implementation**:
- Hash table: operator-name → failure-count
- Store first N (e.g., 3) failure examples per operator
- Calculate percentage from successful/total

## Summary

All research questions resolved. Key decisions:
1. **Wasm globals**: Use `anyref` type with mutability flag
2. **Constant folding**: Arithmetic + literals + known constants
3. **define-condition**: Syntactic expansion to defclass, skip :report
4. **declare**: Filter during AST parsing
5. **defstruct**: Vector-backed constructor/accessor expansion
6. **Error reporting**: Operator-grouped counts with percentage

No NEEDS CLARIFICATION markers remain. Ready for Phase 1 design.
