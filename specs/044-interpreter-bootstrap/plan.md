# Implementation Plan: Interpreter Bootstrap Strategy

**Branch**: `044-interpreter-bootstrap` | **Date**: 2025-12-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/044-interpreter-bootstrap/spec.md`

## Summary

Extend the existing Tier 1 S-expression interpreter (`src/clysm/eval/interpreter.lisp`) to support all Common Lisp features required by the Clysm compiler source code. This enables self-hosting bootstrap without SBCL: the interpreter evaluates compiler modules and generates Stage 0 Wasm, which then produces Stage 1 and Stage 2 for fixed-point verification.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host interpreter; WasmGC for compiler output
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing); wasmtime (Wasm runtime), wasm-tools (validation)
**Storage**: N/A (in-memory evaluation; file-based source reading)
**Testing**: rove (unit/contract/integration tests); wasmtime for Wasm execution verification
**Target Platform**: Linux (primary), macOS (secondary); Wasm output runs on any WasmGC-compatible runtime
**Project Type**: single (Common Lisp ASDF system)
**Performance Goals**: Stage 0 generation in under 5 minutes; individual form interpretation under 10ms
**Constraints**: Interpreter delegates to SBCL's reader for source parsing; fixed-point must be byte-for-byte identical
**Scale/Scope**: 45 compiler modules (~10K LOC); 400+ unit tests; 100+ built-in functions

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Interpreter evaluates source; output is WasmGC binary per existing compiler |
| II. Lisp Object Representation | PASS | NIL/UNBOUND handling inherited from compiler; interpreter uses host CL values |
| III. Function/Closure Strategy | PASS | Interpreted closures wrap host lambdas; compiled output uses $closure struct |
| IV. Wasm Control Flow | PASS | Not applicable to interpreter; compiled output uses tail-call/EH |
| V. Shallow Binding | PASS | Not applicable to interpreter (uses host CL's dynamic scope); compiled output unchanged |
| VI. Tiered Eval/JIT | PASS | This feature extends Tier 1 interpreter; Tier 2 JIT infrastructure unchanged |
| VII. TDD (Non-Negotiable) | PASS | All interpreter extensions developed with tests first; Red-Green-Refactor |
| VIII. Nix-First Workflow | PASS | `nix flake check` includes interpreter tests; CI validates all changes |

**Gate Result**: All gates PASS. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/044-interpreter-bootstrap/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── eval/
│   ├── interpreter.lisp      # EXTEND: Tier 1 interpreter (400 LOC → ~2000 LOC)
│   ├── interpreter-macros.lisp  # NEW: Macro system for interpreter
│   ├── interpreter-builtins.lisp # NEW: 100+ built-in functions
│   └── interpreter-file.lisp  # NEW: File interpretation support
├── bootstrap/               # NEW: Bootstrap infrastructure
│   ├── package.lisp
│   ├── interpreter-stage0.lisp  # generate-stage0-via-interpreter
│   └── fixpoint.lisp         # Fixed-point verification
└── ... (existing modules unchanged)

tests/
├── unit/
│   └── interpreter/          # NEW: Interpreter unit tests
│       ├── defun-test.lisp
│       ├── defmacro-test.lisp
│       ├── defstruct-test.lisp
│       ├── loop-test.lisp
│       ├── handler-case-test.lisp
│       └── builtins-test.lisp
├── contract/
│   └── interpreter-stage0-test.lisp  # NEW: Stage 0 validation
└── integration/
    └── bootstrap-fixpoint-test.lisp  # NEW: Full bootstrap verification
```

**Structure Decision**: Single ASDF system extension. New files added under `src/clysm/eval/` for interpreter extensions and `src/clysm/bootstrap/` for Stage 0 generation. Existing compiler modules unchanged.

## Complexity Tracking

No constitution violations requiring justification. All changes extend existing interpreter architecture without adding new patterns.
