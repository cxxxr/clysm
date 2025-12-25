# Implementation Plan: ANSI Test Execution

**Branch**: `021-ansi-test-execution` | **Date**: 2025-12-25 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/021-ansi-test-execution/spec.md`

## Summary

Enable actual execution of ANSI Common Lisp test suite with measurable pass rates. The 020-ansi-test infrastructure exists but all tests currently skip or fail due to compilation errors. This feature bridges the gap by analyzing which test patterns can compile successfully, improving result comparison, and ensuring basic arithmetic/predicate tests PASS.

**Key insight**: Most ANSI tests use complex forms (`loop`, `let`, `defvar`, `*universe*`, `signals-error`, etc.) that Clysm doesn't support. The strategy is to:
1. Identify tests with simple, compilable patterns
2. Improve skip classification to distinguish "unsupported form" from "compilation error"
3. Enhance result comparison to correctly match fixnum/boolean results
4. Add more unsupported forms to the skip registry to avoid false failures

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - test harness modification
**Primary Dependencies**: Existing 020-ansi-test (loader, runner, classifier), clysm/compiler
**Storage**: N/A (in-memory test execution)
**Testing**: Rove for harness tests; ANSI suite is the test payload
**Target Platform**: Linux (primary), macOS (secondary via Nix)
**Project Type**: Single project - modifies existing src/clysm/ansi-test/
**Performance Goals**: Single category completion within 2 minutes
**Constraints**: Only fixnum/boolean results verifiable; complex objects marked "unverifiable"
**Scale/Scope**: ~20,000 tests; target 10%+ numbers pass rate, 5%+ cons pass rate

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Research Gate Evaluation

| Principle | Requirement | Status | Notes |
|-----------|-------------|--------|-------|
| I. WasmGC-First | Use WasmGC types | ✅ PASS | Uses existing compiler output; no new Wasm types |
| II. Lisp Object Representation | NIL/UNBOUND as singletons | ✅ PASS | Leverages existing runtime representation |
| III. Function/Closure Strategy | Closure struct dispatch | ✅ PASS | Tests existing function system |
| IV. Wasm Control Flow | tail_call, try_table | ✅ PASS | Harness is host-side Lisp |
| V. Shallow Binding | Special variable binding | ⚠️ N/A | Most tests using specials will be skipped |
| VI. Tiered Eval/JIT | 2-tier execution model | ✅ PASS | Tests compiled modules |
| VII. TDD | Red-Green-Refactor | ✅ REQUIRED | All changes must have tests first |
| VIII. Nix-First | nix develop/build/check | ✅ REQUIRED | Use existing Nix infrastructure |

### Key Constraints

1. **TDD Compliance** (VII): Each improvement to skip-registry, classifier, or runner must have tests first
2. **Nix Integration** (VIII): `nix flake check` must continue to pass
3. **Existing Infrastructure**: Modify, don't replace, the 020-ansi-test components

## Project Structure

### Documentation (this feature)

```text
specs/021-ansi-test-execution/
├── plan.md              # This file
├── research.md          # Phase 0: test pattern analysis
├── data-model.md        # Phase 1: refinements to existing entities
├── quickstart.md        # Phase 1: command reference
├── contracts/           # Phase 1: interface contracts
│   └── cli.md           # CLI interface specification
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (modifications to existing)

```text
src/
├── clysm/
│   └── ansi-test/              # MODIFY: Existing harness
│       ├── skip-registry.lisp  # Add more unsupported forms
│       ├── classifier.lisp     # Improve result comparison
│       └── runner.lisp         # Handle more edge cases (summary format)

tests/
├── unit/
│   └── ansi-test/              # ADD: New tests for changes
│       ├── skip-registry-test.lisp  # Test new skip patterns
│       └── classifier-test.lisp     # Test improved comparison
└── integration/
    └── ansi-test/
        └── execution-test.lisp      # Test actual ANSI test execution
```

**Structure Decision**: Modify existing 020-ansi-test structure. No new packages needed; extend existing `clysm/ansi-test` package.

## Complexity Tracking

> **No Constitution violations identified. This section is empty.**

All gates pass. This feature modifies host-side Lisp code only, extending the existing test harness infrastructure.

---

## Post-Design Constitution Re-Check

*Gate evaluation after Phase 1 design completion.*

### Design Artifacts Review

| Artifact | Constitution Alignment | Notes |
|----------|----------------------|-------|
| data-model.md | ✅ PASS | Extends existing defstructs minimally |
| contracts/cli.md | ✅ PASS | No new CLI interfaces; uses existing `run-ansi-tests` |
| research.md | ✅ PASS | Analysis of test patterns without adding complexity |

### Final Gate Status

| Principle | Post-Design Status | Verification |
|-----------|-------------------|--------------|
| VII. TDD | ✅ REQUIRED | New tests in tests/unit/ansi-test/ |
| VIII. Nix-First | ✅ REQUIRED | Continue using existing Nix setup |
| All Others | ✅ N/A | No new Wasm-level constructs |

### Complexity Analysis

**Lines of Code Changed** (estimated):
- skip-registry.lisp: +30-50 LOC (more unsupported forms)
- classifier.lisp: +20-40 LOC (improved comparison)
- runner.lisp: +10-20 LOC (summary format edge cases)
- New tests: +100-150 LOC
- **Total: ~160-260 LOC** (minimal changes)

**New Dependencies**: None

**Conclusion**: Design passes all Constitution gates. Proceed to task generation.
