# Implementation Plan: ANSI Common Lisp Test Suite Integration

**Branch**: `020-ansi-test` | **Date**: 2025-12-25 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/020-ansi-test/spec.md`

## Summary

Integrate the pfdietz/ansi-test suite (~20,000 tests across 28 categories) into Clysm to measure ANSI Common Lisp compliance. Tests are loaded dynamically, compiled via Clysm's compile-to-wasm pipeline, executed through wasmtime, and results classified as PASS/FAIL/SKIP. Features include category filtering, automatic skip detection for unsupported forms, Markdown report generation, and regression detection against baselines.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - test harness implementation
**Primary Dependencies**: alexandria, rove (test framework), uiop (process execution), wasmtime (Wasm runtime), wasm-tools (validation)
**Storage**: N/A (in-memory test execution; optional JSON/Markdown for reports and baselines)
**Testing**: Rove for unit tests of harness; the ANSI suite itself is the test payload
**Target Platform**: Linux (primary), macOS (secondary via Nix)
**Project Type**: Single project - extends existing src/tests structure
**Performance Goals**: Full suite completion within 30 minutes; single category within 2 minutes (SC-003, SC-006)
**Constraints**: 30-second timeout per test; graceful degradation on unsupported forms
**Scale/Scope**: ~20,000 tests, 28 categories; initial cons 40%+, numbers 30%+ pass rate targets

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Research Gate Evaluation

| Principle | Requirement | Status | Notes |
|-----------|-------------|--------|-------|
| I. WasmGC-First | Use WasmGC types | ✅ PASS | Tests execute existing Clysm output; no new Wasm types |
| II. Lisp Object Representation | NIL/UNBOUND as singletons | ✅ PASS | Test harness uses existing runtime representation |
| III. Function/Closure Strategy | Closure struct dispatch | ✅ PASS | No new function representation; tests existing system |
| IV. Wasm Control Flow | tail_call, try_table | ✅ PASS | Test harness is host-side Lisp; no Wasm control flow |
| V. Shallow Binding | Special variable binding | ✅ PASS | Test harness may use specials but follows existing patterns |
| VI. Tiered Eval/JIT | 2-tier execution model | ✅ PASS | Tests compiled modules; JIT not involved in harness |
| VII. TDD | Red-Green-Refactor | ✅ REQUIRED | Harness development MUST follow TDD |
| VIII. Nix-First | nix develop/build/check | ✅ REQUIRED | ansi-test submodule pinned in flake inputs |

### Key Constraints

1. **TDD Compliance** (VII): Every harness component must have tests first
   - Test loader tests before loader implementation
   - Category filter tests before filter implementation
   - Report generator tests before generator implementation

2. **Nix Integration** (VIII):
   - pfdietz/ansi-test as flake input (pinned commit)
   - `nix flake check` must pass with new test infrastructure
   - devShell includes all tools for test execution

3. **Wasm Validation**: All test executions use existing `wasm-tools validate` infrastructure

## Project Structure

### Documentation (this feature)

```text
specs/020-ansi-test/
├── plan.md              # This file
├── research.md          # Phase 0: ansi-test format, skip strategies
├── data-model.md        # Phase 1: TestCase, TestResult, CategoryResult entities
├── quickstart.md        # Phase 1: Quick command reference
├── contracts/           # Phase 1: CLI interface contracts
│   └── cli.md           # Command-line interface specification
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   └── ansi-test/              # NEW: ANSI test harness
│       ├── package.lisp        # Package definition
│       ├── data-model.lisp     # Entity defstructs (TestCase, TestResult, etc.)
│       ├── loader.lisp         # Test file parsing and loading
│       ├── runner.lisp         # Test execution engine
│       ├── classifier.lisp     # PASS/FAIL/SKIP classification
│       ├── skip-registry.lisp  # Unsupported form detection
│       ├── conditions.lisp     # Error conditions (category-not-found-error)
│       ├── reporter.lisp       # Markdown report generation
│       └── baseline.lisp       # Regression detection

tests/
├── unit/
│   └── ansi-test/              # NEW: Harness unit tests
│       ├── package.lisp        # Test package definition
│       ├── data-model-test.lisp
│       ├── loader-test.lisp
│       ├── runner-test.lisp
│       ├── classifier-test.lisp
│       ├── skip-registry-test.lisp
│       ├── reporter-test.lisp
│       └── baseline-test.lisp
├── integration/
│   └── ansi-test/              # NEW: End-to-end harness tests
│       ├── runner-test.lisp
│       ├── reporter-test.lisp
│       └── baseline-test.lisp
└── contract/
    └── ansi-test/              # NEW: Output format contracts
        └── report-format-test.lisp

scripts/                        # NEW: Shell scripts for CI
└── ansi-test.sh                # CLI wrapper for test execution

.github/
└── workflows/
    └── ansi-compliance.yml     # GitHub Actions workflow

ansi-test/                      # Git submodule (pfdietz/ansi-test)
├── cons/                       # Category: cons operations
├── numbers/                    # Category: numeric operations
├── strings/                    # Category: string operations
└── ...                         # 28+ categories total

baselines/                      # NEW: Regression baselines
└── current.json                # Latest known-good results
```

**Structure Decision**: Single project structure. Test harness is a new package under `src/clysm/ansi-test/`, following existing Clysm package organization. The ANSI test suite itself is a git submodule at repository root.

## Complexity Tracking

> **No Constitution violations identified. This section is empty.**

All gates pass without requiring justification. The test harness is host-side Common Lisp code that exercises the existing Clysm compiler infrastructure without introducing new Wasm-level complexity.

---

## Post-Design Constitution Re-Check

*Gate evaluation after Phase 1 design completion.*

### Design Artifacts Review

| Artifact | Constitution Alignment | Notes |
|----------|----------------------|-------|
| data-model.md | ✅ PASS | Defstruct-based entities; no complex OOP patterns |
| contracts/cli.md | ✅ PASS | Simple function-based CLI; no over-engineering |
| research.md | ✅ PASS | Decisions favor minimal complexity |

### Final Gate Status

| Principle | Post-Design Status | Verification |
|-----------|-------------------|--------------|
| VII. TDD | ✅ REQUIRED | Harness tests defined in tests/unit/ansi-test/ |
| VIII. Nix-First | ✅ REQUIRED | flake.nix input for ansi-test submodule planned |
| All Others | ✅ N/A | No new Wasm-level constructs in design |

### Complexity Analysis

**Lines of New Code** (estimated from research.md):
- Test loader & category filtering: 100-150 LOC
- Skip detection & reason collection: 80-120 LOC
- Report generation (Markdown): 120-180 LOC
- Regression comparison tool: 100-150 LOC
- **Total: ~400-600 LOC** (modest addition)

**New Dependencies**: None required (optional cl-json, lparallel)

**Conclusion**: Design passes all Constitution gates. Proceed to task generation.
