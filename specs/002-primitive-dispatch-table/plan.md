# Implementation Plan: Primitive Dispatch Table

**Branch**: `002-primitive-dispatch-table` | **Date**: 2026-01-03 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/002-primitive-dispatch-table/spec.md`

## Summary

Migrate 240+ primitive compilers from a 538-line case statement in `compile-primitive-call` to a hash-table driven dispatch system. Leverage existing infrastructure in `primitive-dispatch.lisp` and populate `primitive-registry.lisp` with registrations. Maintain byte-identical Wasm output while reducing core dispatch logic to under 30 lines.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria (hash-table utilities)
**Storage**: N/A (in-memory dispatch tables)
**Testing**: rove (unit tests), wasm-tools (contract validation)
**Target Platform**: WasmGC (compile-time infrastructure only)
**Project Type**: single
**Performance Goals**: O(1) primitive lookup
**Constraints**: Byte-identical Wasm output to baseline
**Scale/Scope**: 240+ primitives, 538 lines case → <30 lines dispatch

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | N/A | Compile-time infrastructure, no Wasm types affected |
| II. Lisp Object Representation | N/A | No object representation changes |
| III. Function/Closure Strategy | N/A | Code generators remain unchanged |
| IV. Wasm Control Flow | N/A | No control flow changes |
| V. Shallow Binding | N/A | No dynamic scope changes |
| VI. Tiered Eval/JIT | N/A | Not affected |
| VII. TDD (Non-negotiable) | **REQUIRED** | TDD cycle for each primitive category migration |
| VIII. Nix-First Workflow | **REQUIRED** | Tests via `nix flake check` |
| IX. ANSI CL Reference | **REQUIRED** | HyperSpec links in documentation |

**Gate Status**: PASS - No violations. TDD and Nix requirements are workflow constraints, not blocking issues.

## Project Structure

### Documentation (this feature)

```text
specs/002-primitive-dispatch-table/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 research findings
├── data-model.md        # Entity definitions
├── quickstart.md        # Developer quickstart guide
├── contracts/           # API contracts
│   └── primitive-dispatch-api.lisp
└── checklists/
    └── requirements.md  # Spec quality checklist
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
├── primitive-dispatch.lisp   # Dispatch infrastructure (existing, 134 lines)
├── primitive-registry.lisp   # Primitive registrations (to populate, 51 lines)
└── func-section.lisp         # Case statement to replace (lines 1184-1722)

tests/
├── unit/
│   └── primitive-dispatch-test.lisp      # API unit tests
└── contract/
    └── primitive-dispatch-wasm-test.lisp # Wasm validation tests
```

**Structure Decision**: Single project structure - this is an internal compiler refactoring.

## Complexity Tracking

No Constitution violations requiring justification. The feature is a pure refactoring with no new abstractions or patterns beyond what already exists.

## Implementation Phases

### Phase 1: Foundation (P1 - Primitive Compilation Lookup)

**Objective**: Integrate dispatch-primitive into compile-primitive-call

**Tasks**:
1. Verify existing dispatch infrastructure in `primitive-dispatch.lisp`
2. Create baseline Wasm output for all primitives (regression testing)
3. Modify `compile-primitive-call` to use `dispatch-primitive`
4. Add fallback for unregistered primitives

**Deliverables**:
- Modified `compile-primitive-call` (~30 lines)
- Baseline test artifacts
- Integration tests

### Phase 2: Registration (P1 continuation)

**Objective**: Populate primitive-registry.lisp with all 240+ primitives

**Migration Order** (by category, TDD per category):

| Order | Category | Count | Priority |
|-------|----------|-------|----------|
| 1 | Type Predicates | 25 | High - used everywhere |
| 2 | List Operations | 35 | High - core Lisp |
| 3 | Equality | 4 | High - fundamental |
| 4 | Arithmetic | 28 | High - basic math |
| 5 | Comparison | 12 | High - control flow |
| 6 | Sequence | 25 | Medium |
| 7 | Array | 20 | Medium |
| 8 | String | 22 | Medium |
| 9 | Character | 15 | Medium |
| 10 | Bit Operations | 10 | Medium |
| 11 | Trigonometric | 12 | Low |
| 12 | Hyperbolic | 6 | Low |
| 13 | Symbol | 8 | Low |
| 14 | Hash Table | 6 | Low |
| 15 | Control | 8 | Low |
| 16 | Misc | 14 | Low |

**Per-Category TDD Cycle**:
1. Write test: compile expressions, capture baseline Wasm
2. Register primitives in category
3. Run test: verify byte-identical output
4. Remove corresponding case branches

### Phase 3: Cleanup (P1 completion)

**Objective**: Remove case statement, verify final state

**Tasks**:
1. Remove all migrated case branches from `func-section.lisp`
2. Verify `compile-primitive-call` is under 30 lines (SC-004)
3. Run full test suite
4. Generate Stage 1 and validate Wasm

### Phase 4: Extensibility (P2 - Register New Primitive)

**Objective**: Verify and document extensibility

**Tasks**:
1. Create test primitive registration example
2. Verify no core code changes needed (SC-003)
3. Document in quickstart.md

### Phase 5: Introspection (P3 - Query Registered Primitives)

**Objective**: Implement and test query API

**Tasks**:
1. Verify `list-registered-primitives` works
2. Verify `primitive-registered-p` works
3. Verify `get-primitive-info` returns correct metadata
4. Add category filtering if needed

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Byte-identical breakage | Per-category baselines, immediate rollback capability |
| Missing primitives | Automated count verification (240+ registered) |
| Performance regression | Hash-table sizing (512 buckets), O(1) verification |
| Dispatch order issues | String table only for cross-package symbols |

## Success Verification

| Criterion | Verification Method |
|-----------|---------------------|
| SC-001: All 248 primitives compile | Automated test coverage |
| SC-002: O(1) lookup | Hash-table implementation guarantee |
| SC-003: Single registration call | Manual test + documentation |
| SC-004: <30 lines dispatch | Line count check |
| SC-005: All tests pass | `asdf:test-system :clysm` |
| SC-006: No recompilation needed | Dynamic registration test |

## Next Steps

1. Run `/speckit.tasks` to generate task breakdown
2. Create baseline Wasm artifacts for regression testing
3. Begin Phase 1 implementation with TDD cycle
