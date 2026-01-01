# Research: Type Constant and Package Primitive Export

**Feature Branch**: `001-type-package-export`
**Date**: 2026-01-01
**Purpose**: Document research findings for all Technical Context unknowns

## Research Findings

### 1. Type Constant Export Mechanism

**Question**: How should type constants be accessible in Wasm-compiled code?

**Decision**: Compile-time constant folding with re-export to clysm package

**Rationale**:
- Type constants (+TYPE-CONS+, +TYPE-SYMBOL+, etc.) are already defined in `clysm/compiler/codegen/gc-types` package (gc-types.lisp:10-104)
- Constants are already exported from that package (package.lisp:493-574)
- However, compiled code references symbols in the `:clysm` package, not `clysm/compiler/codegen/gc-types`
- Solution: Re-export constants to `:clysm` package and add DEFCONSTANT handling in directive.lisp

**Alternatives Considered**:
1. **Global Wasm variables**: Would require runtime lookup. Rejected because constants should be inlined at compile-time.
2. **Symbol-value lookup**: Runtime cost for every reference. Rejected for performance.
3. **Import-from only**: Would require users to know internal package structure. Rejected for usability.

**Implementation Path**:
1. Add `:export` entries to `:clysm` package definition for all type constants
2. Add DEFCONSTANT handler in directive.lisp to record constant bindings
3. Modify variable reference compilation to check constant bindings first
4. Emit `i32.const <value>` for constant references

---

### 2. DEFCONSTANT Compile-Time Handling

**Question**: How should [defconstant](resources/HyperSpec/Body/m_defcon.htm) forms be processed during compilation?

**Decision**: Extend directive.lisp with constant binding registry

**Rationale**:
- Current directive.lisp handles IN-PACKAGE, DEFPACKAGE, DECLAIM, PROCLAIM
- DEFCONSTANT is similar: it establishes a compile-time binding
- Unlike DEFVAR, the value must be evaluated at compile-time
- Value is immutable; repeated references should use same inlined value

**Implementation Details**:
```lisp
;; In directive.lisp
(defparameter *constant-bindings* (make-hash-table :test 'eq)
  "Maps constant symbols to their compile-time values")

(defun handle-defconstant (name value-form &optional doc)
  "Process DEFCONSTANT form at compile-time"
  (let ((value (eval value-form)))  ; Evaluate at compile-time
    (setf (gethash name *constant-bindings*) value)
    :skipped))  ; Form handled, no Wasm output needed
```

**Alternatives Considered**:
1. **Treat as DEFVAR**: Would create mutable global. Rejected per ANSI CL semantics.
2. **Macro expansion to literal**: Would lose symbolic name. Rejected for debugging.

---

### 3. Package Primitive Implementation

**Question**: How should package primitives be implemented for Wasm runtime?

**Decision**: Wasm struct-based package representation with runtime functions

**Rationale**:
- Package primitives are already registered in runtime function table (func-section.lisp:174-186)
- Registration exists but implementations are missing
- Need actual Wasm functions that can be called at runtime

**Implementation for Each Primitive**:

#### PACKAGEP* (FR-009)
```lisp
;; Reference: resources/HyperSpec/Body/f_pkgp.htm
;; Wasm implementation: ref.test for package struct type
;; Returns t if object is a package, nil otherwise
```
- Compile as `ref.test` instruction against package struct type
- If ref.test succeeds → return T global
- If ref.test fails → return NIL global

#### FIND-PACKAGE* (FR-006)
```lisp
;; Reference: resources/HyperSpec/Body/f_find_p.htm
;; Implementation: FFI call to host runtime or symbol table lookup
```
- Requires access to package registry (hash-table of name→package)
- Use existing hash-table primitives or FFI to host

#### INTERN* (FR-007)
```lisp
;; Reference: resources/HyperSpec/Body/f_intern.htm
;; Implementation: Symbol table management
```
- Variadic (1-2 args): name, optional package
- Returns symbol and status (second value)
- Requires package's internal symbol table access

#### SYMBOL-PACKAGE* (FR-008)
```lisp
;; Not in runtime table - needs addition
;; Implementation: struct.get on symbol's package field
```
- Simple field access on symbol struct
- Currently missing from runtime function registration
- Add to register-package-runtime-functions

**Alternatives Considered**:
1. **Pure FFI to host**: Every call crosses Wasm boundary. Rejected for performance.
2. **Interpreter fallback**: Use Wasm interpreter for package ops. Rejected for complexity.

---

### 4. Existing Infrastructure Analysis

**Finding**: Significant infrastructure already exists

**Current State**:

| Component | Location | Status |
|-----------|----------|--------|
| Type constants | gc-types.lisp | Defined ✓ |
| gc-types exports | package.lisp:491-600 | Exported ✓ |
| Runtime function table | func-section.lisp:69-106 | Exists ✓ |
| PACKAGEP* registration | func-section.lisp:180 | Registered ✓ |
| FIND-PACKAGE* registration | func-section.lisp:183 | Registered ✓ |
| INTERN* registration | func-section.lisp:186 | Registered ✓ |
| SYMBOL-PACKAGE* | N/A | **Missing** |
| DEFCONSTANT handler | directive.lisp | **Missing** |
| Constant folding | compiler.lisp | **Missing** |

**Key Insight**: The P951 errors occur because functions are registered but the actual Wasm implementations don't exist. The P846 errors occur because constants aren't resolved at compile-time.

---

### 5. Error Pattern Analysis (from stage1-report.json)

**P846: Unbound variable +TYPE-CONS+**
- Count: 25 occurrences (2.86%)
- Affected modules: func-section.lisp, macros.lisp
- Root cause: DEFCONSTANT forms not processed; constants not visible during compilation
- Solution: Add DEFCONSTANT handling + constant folding

**P951: Undefined function PACKAGEP***
- Count: 79 occurrences (9.04%)
- Affected modules: setf-expanders.lisp, slot-access.lisp, func-section.lisp, package.lisp
- Root cause: Runtime function registered but no actual Wasm implementation
- Solution: Implement PACKAGEP* as ref.test instruction sequence

---

## Summary of Decisions

| Area | Decision | Key Reason |
|------|----------|------------|
| Constant access | Compile-time folding | Performance (inlined i32.const) |
| DEFCONSTANT | Directive handler + registry | Match IN-PACKAGE pattern |
| PACKAGEP* | ref.test instruction | WasmGC native type checking |
| FIND-PACKAGE* | FFI or runtime lookup | Requires host symbol table |
| INTERN* | Runtime library call | Variadic, complex symbol mgmt |
| SYMBOL-PACKAGE* | struct.get instruction | Simple field access |

## Open Questions (None)

All technical questions resolved. Ready for Phase 1 design.
