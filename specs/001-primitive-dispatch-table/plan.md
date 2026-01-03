# Implementation Plan: Primitive Dispatch Table

**Branch**: `001-primitive-dispatch-table` | **Date**: 2026-01-03 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-primitive-dispatch-table/spec.md`

## Summary

Replace the monolithic 248-branch case statement in `compile-primitive-call` with a hash-table driven dispatch mechanism. The system provides two lookup modes (symbol-based for standard primitives, string-based for cross-package symbols) and a registration API for adding new primitive compilers without modifying the core dispatch function.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria (hash-table utilities)
**Storage**: N/A (in-memory compilation only)
**Testing**: rove (unit tests), wasm-tools validate (contract tests)
**Target Platform**: WasmGC
**Project Type**: Single compiler project
**Performance Goals**: O(1) primitive lookup, no regression in compile time
**Constraints**: Backward compatibility with all existing primitives, no changes to compiled Wasm output
**Scale/Scope**: 248 case-statement primitives + 18 string-matched primitives = 266 total

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | N/A | Host-side refactoring only |
| II. Lisp Object Representation | N/A | No changes to object representation |
| III. Function/Closure Implementation | N/A | Dispatch mechanism, not function structure |
| IV. Wasm Control Flow | N/A | No changes to generated control flow |
| V. Shallow Binding | N/A | Not affected |
| VI. Tiered Eval/JIT | N/A | Compile-time change only |
| VII. Test-Driven Development | **REQUIRED** | Tests before implementation |
| VIII. Nix-First Workflow | Pass | Use existing nix develop environment |
| IX. ANSI CL HyperSpec References | Pass | Document primitives with HyperSpec links |

**Gate Result**: PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/001-primitive-dispatch-table/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
├── func-section.lisp    # Current location - refactor target
├── primitive-dispatch.lisp  # NEW: Dispatch table and registration API
└── primitive-registry.lisp  # NEW: Primitive compiler registrations

tests/
├── unit/
│   └── primitive-dispatch-test.lisp  # NEW: Dispatch mechanism tests
└── contract/
    └── primitive-dispatch-wasm-test.lisp  # NEW: Wasm output verification
```

**Structure Decision**: Extend existing single-project layout. New files in `src/clysm/compiler/codegen/` for dispatch infrastructure, keeping primitive compiler functions in `func-section.lisp`.

## Complexity Tracking

No violations - design follows Constitution principles.

## Architecture Overview

### Current State (compile-primitive-call)

```
                        +----------------------------------+
                        | compile-primitive-call           |
                        +----------------------------------+
                               |
           +-------------------+-------------------+
           |                                       |
    (cond string=)                           (case op)
    18 branches                              248 branches
    %SETF-*, MAKE-INSTANCE*, etc.            +, -, cons, car, ...
```

### Target State (Hash-Table Dispatch)

```
                        +----------------------------------+
                        | compile-primitive-call           |
                        +----------------------------------+
                               |
           +-------------------+-------------------+
           |                                       |
    (gethash *symbol-table*)             (gethash *string-table*)
           |                                       |
    +------+------+                         +------+------+
    |             |                         |             |
  found?      fallback                    found?      fallback
    |             |                         |             |
 dispatch    default-call               dispatch    default-call
```

### Key Design Decisions

1. **Two-Table Strategy**: Separate hash-tables for symbol and string lookup
   - `*primitive-symbol-table*`: `(make-hash-table :test 'eq)` for O(1) symbol lookup
   - `*primitive-string-table*`: `(make-hash-table :test 'equal)` for string-based lookup

2. **Lookup Order**: Symbol-first, string-fallback
   - Performance: eq-based lookup is faster than string comparison
   - Most primitives (248/266) use symbol lookup

3. **Registration Entry Structure**:
   ```lisp
   (defstruct primitive-entry
     compiler-fn    ; Function: (op args env) -> instructions
     arity          ; Optional arity hint (nil = variadic)
     flags)         ; Optional plist for future extensions
   ```

4. **Registration API**:
   ```lisp
   (register-primitive-compiler symbol compiler-fn &key arity flags string-name)
   ```

5. **Migration Strategy**: Incremental extraction
   - Extract compiler functions from case branches
   - Register each in the new table
   - Remove from case statement
   - Final: Remove empty case statement

## Files to Modify/Create

| File | Action | Purpose |
|------|--------|---------|
| `src/clysm/compiler/codegen/primitive-dispatch.lisp` | CREATE | Dispatch tables and core API |
| `src/clysm/compiler/codegen/primitive-registry.lisp` | CREATE | All primitive registrations |
| `src/clysm/compiler/codegen/func-section.lisp` | MODIFY | Remove case statement, use dispatch |
| `tests/unit/primitive-dispatch-test.lisp` | CREATE | Unit tests for dispatch mechanism |
| `tests/contract/primitive-dispatch-wasm-test.lisp` | CREATE | Wasm output validation |
| `clysm.asd` | MODIFY | Add new source files |

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Regression in existing primitives | Medium | High | Comprehensive test suite before refactoring |
| Performance degradation | Low | Medium | Benchmark compile times before/after |
| Symbol package confusion | Medium | Medium | Use eq for symbols, equal for strings |
| Incomplete migration | Low | Low | Incremental approach, one category at a time |
