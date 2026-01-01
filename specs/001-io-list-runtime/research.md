# Research: I/O and List Operations Runtime Migration

**Date**: 2026-01-01
**Feature**: 001-io-list-runtime

## Overview

This document consolidates research findings for migrating I/O and list operations from inline codegen to runtime library implementations.

---

## 1. Current Compile-* Implementation Analysis

### I/O Functions (lines 17818-18233 in func-section.lisp)

| Function | Lines | Complexity | Migration Approach |
|----------|-------|------------|-------------------|
| compile-princ | 17818-17855 | Medium | FFI → runtime call |
| compile-prin1 | 17857-17885 | Medium | FFI → runtime call |
| compile-print | 17887-17934 | Medium | FFI → runtime call |
| compile-write | 17936-17991 | High (keyword parsing) | FFI → runtime call |
| compile-format | 17993-18023 | Low (dispatcher) | Delegates to variants |
| compile-format-constant | 18024-18049 | Medium | Runtime format call |
| compile-format-to-string | 18051-18125 | High | Runtime format call |
| compile-format-to-stream | 18127-18213 | High | Runtime format call |
| compile-format-dynamic | 18215-18233 | Low | Runtime format call |

**Total I/O lines**: ~415 lines

**Key patterns**:
- All use `%host-write-char` and `%host-write-string` FFI primitives
- Stream FD computation via `compile-stream-fd-for-output`
- Object-to-string conversion via `:call :$princ-to-string` or `:call :$prin1-to-string`

### List Functions (lines 11092-12943 in func-section.lisp)

| Function | Lines | Complexity | Migration Approach |
|----------|-------|------------|-------------------|
| compile-find | 11092-11172 | High (inline Wasm loop) | Runtime call |
| compile-find-if | 11174-11233 | High (closure dispatch) | Runtime call |
| compile-position | 11235-11325 | High | Runtime call |
| compile-position-if | 11326-11415 | High | Runtime call |
| compile-member | 11985-12064 | High | Runtime call |
| compile-assoc | 12066-12151 | High | Runtime call |
| compile-rassoc | 12153-12240 | High | Runtime call |
| compile-member-if | 12816-12878 | High | Runtime call |
| compile-member-if-not | 12880-12942 | Medium | Runtime call |
| compile-assoc-if | 12943-... | Medium | Runtime call |

**Total list lines**: ~850+ lines

**Key patterns**:
- All use `block/loop` Wasm control structures
- Type dispatch via `ref.test :i31` for fixnum comparison
- `ref.cast` and `struct.get` for cons cell access
- Closure dispatch via `call_ref` for predicate functions

---

## 2. FFI Primitive Infrastructure

### Decision: Use existing FFI primitives

**Rationale**: FFI primitives are already defined and functional in `streams/ffi-io.lisp`

**Available primitives**:
```lisp
;; Output (T011, T012)
(clysm/ffi:define-foreign-function %host-write-char
    "clysm:io.write-char" (:fixnum :fixnum) :void)
(clysm/ffi:define-foreign-function %host-write-string
    "clysm:io.write-string" (:fixnum :anyref) :void)

;; Input (T013, T014)
(clysm/ffi:define-foreign-function %host-read-char
    "clysm:io.read-char" (:fixnum) :fixnum)
(clysm/ffi:define-foreign-function %host-read-line
    "clysm:io.read-line" (:fixnum) :anyref)
```

**Alternatives considered**:
- Define new primitives: Rejected (duplicates existing infrastructure)
- WASI Preview 2: Deferred (current FFI sufficient for this feature)

---

## 3. Runtime Function Dispatch Pattern

### Decision: Emit `:call :$function-name` for runtime functions

**Rationale**: Current codegen already uses this pattern for helper functions

**Example from compile-princ**:
```lisp
`((:call :$princ-to-string))  ; Call runtime function
```

**How compiler resolves runtime calls**:
1. Compiler emits `:call :$symbol-name`
2. Symbol resolution happens at link time
3. Runtime library exports functions under canonical names

**Alternatives considered**:
- Inline FFI calls directly: Rejected (increases binary size, no optimization opportunity)
- Closure indirection: Rejected (unnecessary overhead for fixed functions)

---

## 4. List Operations Implementation Strategy

### Decision: Use car/cdr/consp primitives with iterative loops

**Rationale**:
- Existing lib/list-ops.lisp uses LOOP macro (not available in minimal runtime)
- Runtime needs primitives only: car, cdr, consp, null, eq, eql

**Implementation pattern** (based on existing list-ops.lisp member*):
```lisp
;; Runtime version using primitives only
(defun member-runtime (item list test key)
  (let ((current list))
    (loop
      (when (null current) (return nil))
      (let ((elem (car current)))
        (when (funcall test item (funcall key elem))
          (return current)))
      (setf current (cdr current)))))
```

**Alternatives considered**:
- Reuse list-ops.lisp directly: Rejected (uses LOOP, needs compilation)
- Recursive implementation: Rejected (stack overflow risk on large lists)

---

## 5. Keyword Argument Handling

### Decision: Runtime functions accept optional keyword arguments with defaults

**Rationale**: ANSI CL requires :test, :test-not, :key, :start, :end, :from-end

**Pattern**:
```lisp
(defun find-runtime (item sequence &key (test #'eql) (key #'identity)
                                        (start 0) end from-end)
  ...)
```

**Compiler side**:
- Parse keyword arguments at compile time
- Emit calls with appropriate argument structure
- Use default values for omitted keywords

---

## 6. Performance Considerations

### Decision: Accept ~10% overhead for maintainability benefits

**Rationale**:
- Inline codegen is ~7,000+ lines of complex Wasm IR generation
- Runtime library is ~2,000 lines of maintainable Lisp
- Call overhead is acceptable for I/O-bound and list-traversal operations

**Mitigation**:
- Hot paths (simple member/assoc with eql test) can be specialized later
- Compiler can inline trivial cases in future optimization pass

---

## 7. Format Directive Support

### Decision: Support core ANSI CL format directives in runtime

**Rationale**: Current codegen supports ~A, ~S, ~D, ~B, ~O, ~X, ~%, ~&, ~~, ~R, ~C, ~F, ~E, ~G, ~$

**Implementation approach**:
1. Parse format string at compile time (existing parse-format-string)
2. Generate runtime calls for each directive
3. Runtime format function handles directive execution

**Deferred directives**: ~{iteration~}, ~[conditional~], ~<justification~>

---

## 8. Migration Order

### Decision: Migrate in dependency order

**Phase 1**: List primitives (no external dependencies)
1. member, member-if, member-if-not
2. assoc, assoc-if
3. find, find-if
4. position, position-if

**Phase 2**: I/O functions (depend on printer helpers)
1. princ, prin1, print (depend on princ-to-string, prin1-to-string)
2. write (keyword parsing)
3. format (directive processing)

**Rationale**: Enables incremental testing and reduces risk

---

## Summary of Decisions

| Topic | Decision | Rationale |
|-------|----------|-----------|
| FFI primitives | Use existing | Already defined, functional |
| Runtime dispatch | `:call :$function-name` | Existing pattern |
| List implementation | car/cdr/consp primitives | No LOOP dependency |
| Keyword handling | Runtime defaults | ANSI CL conformance |
| Performance target | Accept 10% overhead | Maintainability priority |
| Format support | Core directives only | Current codegen scope |
| Migration order | List first, I/O second | Dependency ordering |
