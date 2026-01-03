# Research: Sequence and Array Runtime Migration

**Feature**: 001-sequence-array-runtime
**Date**: 2026-01-04
**Status**: Complete

## Research Questions

### R1: File Organization - New File vs Extend Existing

**Question**: Should subseq-rt and adjust-array-rt go in a new file or extend sequence-runtime.lisp?

**Research Findings**:
- `sequence-runtime.lisp` (454 lines) contains remove, count, substitute, delete families
- `string-runtime.lisp` (361 lines) contains char, trim, capitalize, compare families
- Both files are function-type specific (sequence operations vs string operations)
- subseq operates on sequences (lists, vectors, strings) - fits sequence-runtime.lisp
- adjust-array operates on arrays (vectors are arrays) - related to sequences

**Decision**: Extend `sequence-runtime.lisp`
**Rationale**: subseq and adjust-array are sequence/array operations that fit the existing sequence-runtime.lisp scope. Keeping related functions together improves discoverability and maintainability.
**Alternatives Considered**:
- New file `array-runtime.lisp`: Rejected - creates unnecessary file proliferation
- Separate files for each function: Rejected - too granular

### R2: UTF-8 String Handling Pattern

**Question**: How should subseq-rt handle UTF-8 encoded strings with character indices?

**Research Findings**:
- string-runtime.lisp provides two key helpers:
  - `utf8-continuation-byte-p`: Checks if byte is continuation (10xxxxxx pattern)
  - `decode-utf8-char`: Decodes UTF-8 character at byte position, returns (char, bytes-consumed)
- compile-subseq in func-section.lisp uses byte-level iteration with UTF-8 awareness
- ANSI CL specifies that indices refer to character positions, not byte positions

**Decision**: Reuse UTF-8 helpers from string-runtime.lisp; implement character-to-byte-position conversion
**Rationale**: Existing helpers are proven correct and follow Layer 1 primitive constraints.
**Alternatives Considered**:
- Inline UTF-8 logic: Rejected - duplicates existing code
- Byte-based indices: Rejected - violates ANSI CL specification

### R3: Layer 1 Primitives Availability

**Question**: Which Layer 1 primitives are available and sufficient for implementation?

**Research Findings**:
From existing runtime libraries and compile-subseq/compile-adjust-array implementations:
- String operations: `char`, `length`, `make-string`, `(setf char)`
- Array operations: `aref`, `length`, `make-array`, `(setf aref)`
- List operations: `car`, `cdr`, `cons`, `nthcdr`, `copy-list`
- Type predicates: `stringp`, `listp`, `vectorp`, `arrayp`
- Arithmetic: `+`, `-`, `<`, `<=`, `>=`, `=`, `min`

**Decision**: All required primitives are available
**Rationale**: Existing runtime libraries successfully use these primitives; no new primitives needed.
**Alternatives Considered**: None - primitives already proven

### R4: adjust-array Multidimensional Support

**Question**: Should adjust-array support multidimensional arrays in this migration?

**Research Findings**:
- compile-adjust-array comment states: "MVP implementation: only handles 1D arrays"
- Multidimensional arrays use `$mdarray` type (index 28) with storage indirection
- Supporting multidimensional would require additional complexity for dimension handling

**Decision**: 1D arrays only (MVP scope)
**Rationale**: Matches existing inline codegen scope; multidimensional can be added later.
**Alternatives Considered**:
- Full multidimensional support: Rejected - scope creep, not in spec

### R5: Runtime Function Registration Pattern

**Question**: How should the new functions be registered in *runtime-function-table*?

**Research Findings**:
From func-section.lisp, the registration pattern is:
```lisp
(defun register-sequence-array-runtime-functions ()
  "Register subseq and adjust-array for runtime dispatch."
  (register-runtime-function 'subseq :$subseq-rt nil)  ; variadic for optional end
  (register-runtime-function 'adjust-array :$adjust-array-rt nil))  ; variadic for keyword args
```
- Arity `nil` indicates variadic functions (keyword arguments)
- Runtime names follow `:$function-name-rt` convention

**Decision**: Follow existing registration pattern with `nil` arity for both functions
**Rationale**: Both functions have optional/keyword arguments requiring variadic handling.
**Alternatives Considered**: None - established pattern works

### R6: Error Signaling Strategy

**Question**: How should out-of-bounds and type errors be signaled?

**Research Findings**:
- Existing runtime libraries use `(error "message")` for error conditions
- ANSI CL specifies `type-error` for wrong types and `bounding-indices-designators` violations
- Layer 1 constraint: Only `error` function available

**Decision**: Use `(error "descriptive message")` with context (indices, lengths)
**Rationale**: Matches existing runtime library error handling; provides debugging context.
**Alternatives Considered**:
- Silent nil return: Rejected - violates ANSI CL specification
- Condition restart: Rejected - not available in Layer 1

## Summary

All research questions resolved. No NEEDS CLARIFICATION items remain.

| Question | Decision |
|----------|----------|
| R1: File Organization | Extend sequence-runtime.lisp |
| R2: UTF-8 Handling | Reuse string-runtime.lisp helpers |
| R3: Layer 1 Primitives | All available, sufficient |
| R4: Multidimensional | 1D only (MVP scope) |
| R5: Registration | Follow existing pattern, arity nil |
| R6: Error Signaling | Use (error "message") with context |
