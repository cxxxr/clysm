# Implementation Plan: CLOS Foundation

**Branch**: `026-clos-foundation` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/026-clos-foundation/spec.md`

## Summary

Implement the foundational CLOS (Common Lisp Object System) for the clysm compiler targeting WasmGC. This phase covers `defclass` (with `:initarg`, `:accessor`, `:initform`), `make-instance`, `defmethod` with type specialization, and generic function dispatch. The implementation leverages existing compile-time CLOS infrastructure and adds WasmGC code generation for runtime instances.

**Key Technical Approach**:
- Utilize reserved WasmGC type indices 6 (`$instance`) and 7 (`$standard-class`) from gc-types.lisp
- Generate accessor functions as generic functions with single-method dispatch
- Implement class metadata as compile-time data structures embedded in Wasm globals
- Runtime dispatch uses `ref.test`/`ref.cast` for type checking against class hierarchy

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler; WasmGC for output
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory class/instance registry)
**Testing**: rove (unit tests), wasm-tools validate (contract tests), wasmtime (integration)
**Target Platform**: WasmGC (browsers, wasmtime with --wasm-features=gc)
**Project Type**: Single (compiler extending existing clysm/clos modules)
**Performance Goals**: Instance creation <1μs, method dispatch <100ns (single specializer)
**Constraints**: Must use WasmGC struct types, no linear memory, single inheritance only
**Scale/Scope**: Support 100+ classes, 1000+ methods, 5-level inheritance depth

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. WasmGC-First型システム設計 | ✅ PASS | Using $instance/$standard-class struct types (indices 6-7), `ref.test`/`ref.cast` for dispatch |
| II. Lispオブジェクト表現規約 | ✅ PASS | NIL handled via eq check, UNBOUND for uninitialized slots |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Accessors as closures, methods use existing $closure type |
| IV. Wasm制御フロー活用 | ✅ PASS | No non-local control flow in basic CLOS; error signaling uses existing condition system |
| V. シャローバインディング | N/A | No special variables introduced |
| VI. 段階的動的コンパイル | ✅ PASS | Class/method definitions compile to Wasm at compile-time; Tier 2 JIT can handle runtime defclass |
| VII. テスト駆動開発（TDD） | ✅ PASS | Test suite required before implementation |
| VIII. Nix-Firstワークフロー | ✅ PASS | Existing flake.nix provides dev environment |

**Gate Result**: PASS - All applicable principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/026-clos-foundation/
├── plan.md              # This file
├── research.md          # Phase 0: WasmGC patterns for OO dispatch
├── data-model.md        # Phase 1: Class/Instance/Method data structures
├── quickstart.md        # Phase 1: Developer quick reference
├── contracts/           # Phase 1: Wasm type/section contracts
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── clos/                        # Existing CLOS compile-time support
│   ├── mop.lisp                 # standard-class, standard-instance structs (existing)
│   ├── defclass.lisp            # defclass parsing (existing)
│   ├── instance.lisp            # make-instance* logic (existing)
│   ├── generic.lisp             # generic-function, method* structs (existing)
│   ├── defmethod.lisp           # defmethod parsing (existing)
│   ├── dispatch.lisp            # compute-applicable-methods (existing)
│   └── combination.lisp         # call-next-method support (existing)
├── compiler/
│   ├── ast.lisp                 # ADD: ast-defclass, ast-defmethod, ast-make-instance nodes
│   ├── compiler.lisp            # MODIFY: emit-type-section for types 6-7
│   └── codegen/
│       ├── gc-types.lisp        # MODIFY: Define $instance, $standard-class structs
│       └── func-section.lisp    # ADD: make-instance, accessor, dispatch codegen
└── package.lisp                 # MODIFY: Export new CLOS codegen symbols

tests/
├── unit/
│   └── clos/                    # NEW: Unit tests for CLOS codegen
│       ├── defclass-test.lisp
│       ├── make-instance-test.lisp
│       ├── accessor-test.lisp
│       └── defmethod-test.lisp
├── contract/
│   └── clos-wasm-test.lisp      # NEW: Wasm validation for CLOS types
└── integration/
    └── clos-ansi-test.lisp      # NEW: End-to-end CLOS scenarios
```

**Structure Decision**: Single project extending existing `clysm/clos/` and `clysm/compiler/` modules. No new top-level directories needed.

## Complexity Tracking

No Constitution violations requiring justification.

## Key Implementation Decisions

### WasmGC Type Design

**$standard-class (type index 7)**:
```wat
(type $standard-class (struct
  (field $name (ref $symbol))           ;; Class name symbol
  (field $superclass (ref null $standard-class))  ;; Single parent (null = no parent)
  (field $slot-count i32)               ;; Total slots including inherited
  (field $slot-names (ref $slot-name-array))      ;; Slot name lookup
  (field $initforms (ref $initform-array))        ;; Default value expressions
))
```

**$instance (type index 6)**:
```wat
(type $instance (struct
  (field $class (ref $standard-class))  ;; Reference to class metadata
  (field $slots (ref $slot-vector))     ;; Slot values array
))

(type $slot-vector (array (mut anyref)))  ;; Mutable slot storage
```

### Dispatch Strategy

1. **Single-specializer dispatch**: Use `ref.test` on first argument to check class membership
2. **Inheritance lookup**: Walk superclass chain at dispatch time (or cache in class precedence list)
3. **Method table**: Store method implementations in Wasm function table, indexed by (gf-id, class-id)
4. **Fallback**: Signal `no-applicable-method` error if no match found

### Accessor Generation

For slot `x` with `:accessor point-x`:
- Reader `point-x`: Generic function with method specialized on owning class
- Writer `(setf point-x)`: Generic function for slot update
- Both compile to struct.get/struct.set on $slot-vector

## Dependencies on Prior Features

| Feature | Dependency | Status |
|---------|------------|--------|
| 002-special-vars-compiler | Binding frame for initform evaluation | ✅ Complete |
| 013-package-system | Symbol/package resolution | ✅ Complete |
| 014-condition-system | Error signaling (no-applicable-method) | ✅ Complete |
| 016-macro-system | defclass/defmethod macro expansion | ✅ Complete |
| 017-eval-jit-compile | Runtime class definition (optional) | ✅ Complete |
