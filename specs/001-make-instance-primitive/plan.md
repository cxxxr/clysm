# Implementation Plan: make-instance* Primitive

**Branch**: `001-make-instance-primitive` | **Date**: 2025-12-31 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-make-instance-primitive/spec.md`

## Summary

Add `make-instance*` as a compiler primitive to enable DEFSTRUCT-generated constructor code to compile to WebAssembly. This unblocks 1,953 DEFSTRUCT usages and is expected to improve Stage 1 compilation rate from 14% to 30%+.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), wasm-tools validate (contract tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler project)
**Performance Goals**: Compilation correctness over speed; wasm-tools validate must pass
**Constraints**: Must follow existing primitive dispatch pattern
**Scale/Scope**: 1 primitive, 1 compiler function, ~50-100 lines of code

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Uses $instance, $slot-vector types |
| II. Lispオブジェクト表現規約 | PASS | NIL via global, UNBOUND for uninitialized slots |
| III. 関数・クロージャ実装戦略 | N/A | Not a closure/function feature |
| IV. Wasm制御フロー活用 | N/A | No control flow changes |
| V. シャローバインディング | N/A | No dynamic scope changes |
| VI. 段階的動的コンパイル | PASS | Compile-time class lookup |
| VII. TDD (非交渉) | PASS | Unit test before implementation |
| VIII. Nix-Firstワークフロー | PASS | No new dependencies |
| IX. ANSI CL仕様参照 | PASS | [make-instance](resources/HyperSpec/Body/f_mk_ins.htm) reference |

**Re-check post-design**: All principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/001-make-instance-primitive/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 research findings
├── data-model.md        # Entity definitions
├── quickstart.md        # Developer quickstart
├── contracts/           # Internal API contracts
│   └── compile-make-instance-star.md
├── checklists/          # Quality checklists
│   └── requirements.md
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       └── func-section.lisp    # MODIFIED: Add primitive + handler
├── clos/
│   └── instance.lisp            # EXISTING: Runtime make-instance*
└── lib/
    └── defstruct.lisp           # EXISTING: Calls make-instance*

tests/
├── unit/
│   └── clos/
│       └── make-instance-test.lisp  # NEW: Unit tests
└── contract/
    └── defstruct/
        └── test-defstruct-wasm.lisp # NEW: Contract tests
```

**Structure Decision**: Single project, compiler-only changes to func-section.lisp

## Implementation Approach

### Phase 1: Primitive Registration

1. Add `make-instance*` to primitive list in `compile-call` (~line 844)
2. Add case dispatch in `compile-primitive-call` cond block

### Phase 2: Compiler Function

Implement `compile-make-instance*` that:
1. Extracts class name from first argument (quoted symbol)
2. Validates class exists in compile-time registry
3. Compiles initarg key-value pairs
4. Emits Wasm instructions for instance creation

### Phase 3: Testing & Validation

1. Unit tests for primitive recognition
2. Contract tests for generated Wasm validity
3. Integration test via Stage 1 build

## Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| Symbol-name matching | Cross-package compatibility (DEFSTRUCT uses qualified name) |
| Compile-time class lookup | Existing infrastructure in func-section.lisp |
| Simplified slot init (nil) | Focus on compilation success; full initarg support can follow |
| Reuse $instance type | Consistent with existing CLOS infrastructure |

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Class not found at compile time | Compilation fails | Use existing find-compile-time-class with clear error |
| Initarg order mismatch | Incorrect slot values | Follow class slot ordering from metadata |
| Type index mismatch | Invalid Wasm | Use existing +type-instance+ constant |

## Complexity Tracking

> No violations requiring justification.

## References

- [make-instance](resources/HyperSpec/Body/f_mk_ins.htm) - ANSI CL specification
- [research.md](research.md) - Detailed research findings
- [data-model.md](data-model.md) - Entity definitions
- [contracts/compile-make-instance-star.md](contracts/compile-make-instance-star.md) - Function contract
