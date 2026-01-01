# Implementation Plan: Compiler Internal Function Consolidation

**Branch**: `001-compiler-internal-consolidation` | **Date**: 2026-01-01 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-compiler-internal-consolidation/spec.md`

## Summary

Increase Stage 1 compilation rate from ~22% to 30%+ by implementing Wasm-compilable versions of internal compiler functions (ENV-ADD-LOCAL, ENV-LOOKUP, COMPILE-TO-INSTRUCTIONS) and completing the migration of list functions (member, assoc, find, position) from inline Wasm emission to runtime library calls. The existing `*runtime-function-table*` dispatch pattern provides proven infrastructure.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, Wasm binary output to `dist/`)
**Testing**: rove test framework, wasm-tools validate
**Target Platform**: WasmGC (WebAssembly with GC proposal), wasmtime runtime
**Project Type**: Single project (compiler)
**Performance Goals**: Stage 1 compilation completes successfully, Wasm validates
**Constraints**: Must maintain backward compatibility with 5632+ currently-compiling forms
**Scale/Scope**: ~26,000 total forms in compiler, targeting 30%+ compilation rate

### Existing Infrastructure (Verified)

| Component | Location | Status |
|-----------|----------|--------|
| Environment mgmt | `src/clysm/compiler/codegen/func-section.lisp:10-182` | Defined (host-only) |
| compile-to-instructions | `src/clysm/compiler/codegen/func-section.lisp:327-434` | Defined (host-only) |
| *runtime-function-table* | `src/clysm/compiler/codegen/func-section.lisp:66-148` | Working |
| list-runtime.lisp | `src/clysm/lib/list-runtime.lisp` | 12 functions implemented |
| Type indices | `src/clysm/compiler/codegen/gc-types.lisp` | 29 types (0-28) |
| FFI infrastructure | `src/clysm/ffi/types.lisp` | Basic structure exists |

### Key Insight from Codebase Analysis

The environment management and compile-to-instructions functions ARE defined in the host compiler, but they cannot be compiled TO Wasm because they use host-specific constructs. The goal is to either:
1. Create Wasm-compilable versions (*-wasm suffix), OR
2. Refactor to use only Wasm-compatible primitives

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | Uses existing type indices (0-28), struct-based objects |
| II. Lispオブジェクト表現規約 | PASS | NIL as singleton, UNBOUND as sentinel - no changes needed |
| III. 関数・クロージャ実装戦略 | PASS | Closure structure maintained, runtime dispatch compatible |
| IV. Wasm制御フロー活用 | PASS | No changes to control flow strategy |
| V. シャローバインディング | PASS | Environment management preserves shallow binding |
| VI. 段階的動的コンパイル | PASS | Stage 1 compilation is Tier 2 (static compilation) |
| VII. TDD（非交渉） | PASS | Tests required before implementation per constitution |
| VIII. Nix-Firstワークフロー | PASS | Using existing devShell with wasm-tools, wasmtime |
| IX. ANSI CL仕様参照規約 | PASS | HyperSpec links included for ANSI functions |

**Gate Status**: PASS - No constitutional violations detected.

## Project Structure

### Documentation (this feature)

```text
specs/001-compiler-internal-consolidation/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (N/A for compiler feature)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   ├── func-section.lisp    # MODIFY: Register list functions, remove inline compile-*
│   │   └── gc-types.lisp        # Reference: Type indices
│   └── transform/               # No changes needed
├── lib/
│   ├── list-runtime.lisp        # EXISTS: Runtime implementations
│   ├── io-runtime.lisp          # Reference: Runtime pattern
│   ├── env-runtime.lisp         # NEW: Wasm-compilable env functions
│   └── package-stubs.lisp       # NEW: FFI-backed package operations
├── ffi/
│   └── types.lisp               # REFERENCE: FFI infrastructure
└── bootstrap/                    # No changes needed

tests/
├── contract/
│   └── runtime-dispatch/        # NEW: Verify runtime function dispatch
├── integration/
│   └── stage1/                  # MODIFY: Regression tests
└── unit/
    └── env-runtime/             # NEW: Environment function tests
```

**Structure Decision**: Single project, extending existing `src/clysm/lib/` with new runtime modules.

## Implementation Strategy

### Phase 1: Runtime Function Registration (Low Risk)

Complete the migration of list functions by updating `*runtime-function-table*` registration:

| Function | Runtime Name | Registration Status |
|----------|--------------|---------------------|
| [member](resources/HyperSpec/Body/f_mem_m.htm) | :$member-rt | Already registered |
| [assoc](resources/HyperSpec/Body/f_assocc.htm) | :$assoc-rt | Already registered |
| [rassoc](resources/HyperSpec/Body/f_rassoc.htm) | :$rassoc-rt | Already registered |
| [find](resources/HyperSpec/Body/f_find_.htm) | :$find-rt | Already registered |
| [position](resources/HyperSpec/Body/f_pos_p.htm) | :$position-rt | Already registered |

**Action Required**: Remove inline `compile-member`, `compile-assoc`, etc. from func-section.lisp (~600 lines).

### Phase 2: Environment Runtime (Medium Risk)

Create `env-runtime.lisp` with Wasm-compilable versions:

```lisp
;; Wasm-compilable environment functions
(defun env-add-local-wasm (env name &optional type)
  "Add local variable - uses only Wasm-compilable constructs")

(defun env-lookup-wasm (env name)
  "Lookup variable - returns binding info or nil")

(defun env-add-closure-var-wasm (env name)
  "Register captured variable in closure environment")
```

### Phase 3: Package System Stubs (Low Risk)

Create `package-stubs.lisp` with FFI-backed operations:

```lisp
;; FFI stubs for package operations
(defun packagep* (obj)
  "Check if object is a package via host FFI")

(defun find-package* (name)
  "Find package by name via host FFI")

(defun intern* (name &optional package)
  "Intern symbol via host FFI")
```

### Phase 4: Type Construction (Medium Risk)

Implement dynamic type registration if needed for new struct types:

```lisp
(defun make-wasm-struct-type (name fields)
  "Create new WasmGC struct type, return type index")
```

**Note**: Existing type indices 0-28 may be sufficient; evaluate during implementation.

## Complexity Tracking

> No violations detected - table left empty per template instructions.

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Regression in 5632+ compiling forms | HIGH | Run full Stage 1 before/after, compare reports |
| Runtime dispatch overhead | LOW | Proven pattern with I/O functions |
| Wasm validation failures | MEDIUM | Validate after each change with wasm-tools |
| Environment refactor breaks closures | HIGH | Extensive unit tests for closure scenarios |

## Success Metrics

From spec SC-001 through SC-008:
- [ ] Compilation rate: 22% → 30%+
- [ ] ENV-ADD-LOCAL removed from blockers
- [ ] COMPILE-TO-INSTRUCTIONS removed from blockers
- [ ] Zero regressions (5632+ forms)
- [ ] Wasm validates (exit code 0)
- [ ] File size < 50KB (2x baseline)
- [ ] Runtime library complete
- [ ] Inline compile-* removed
