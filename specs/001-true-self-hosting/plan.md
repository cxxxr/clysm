# Implementation Plan: Phase 13D - True Self-Hosting Achievement

**Branch**: `001-true-self-hosting` | **Date**: 2025-12-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-true-self-hosting/spec.md`

## Summary

Implement a minimal but functional Lisp interpreter in Wasm that replaces the current Stage 0 stub functions. The interpreter must support basic types (fixnum, symbol, cons), primitives (car, cdr, cons, +, -, *, /, <, >, =, eq), and control structures (if, let, let*, defun, lambda, quote). The goal is achieving a non-trivial fixed-point where Stage 1 >= 1KB and Stage 1 == Stage 2.

**Technical Approach**: Interpreter-first strategy per Constitution Principle VI (Tiered Eval/JIT). Extend existing `src/clysm/stage0/` infrastructure with actual evaluation logic instead of stubs.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host, WasmGC for target
**Primary Dependencies**: alexandria, babel (UTF-8), wasmtime, wasm-tools
**Storage**: N/A (in-memory compilation only)
**Testing**: Rove (unit tests), wasm-tools validate (contract tests), wasmtime (integration)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single (compiler toolchain)
**Performance Goals**: Per Constitution - JIT compile 1000 lines/100ms, function call <5ns
**Constraints**: Per Constitution - No linear memory, WasmGC-first, all objects GC-managed
**Scale/Scope**: Minimal bootstrap (< 20 forms to support, Stage 1 >= 1KB)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | PASS | Using i31ref (fixnum), struct (cons, symbol, closure) |
| II. Lisp Object Representation | PASS | NIL as singleton struct, UNBOUND sentinel |
| III. Closure Implementation | PASS | Arity-dispatched closure struct for lambda/defun |
| IV. Wasm Control Flow | PARTIAL | Minimal if/branching only; no exception handling in bootstrap |
| V. Shallow Binding | DEFERRED | No special variables in minimal bootstrap |
| VI. Tiered Eval/JIT | PASS | Interpreter-first approach (Tier 1) |
| VII. TDD (Non-negotiable) | REQUIRED | Tests before implementation |
| VIII. Nix-First Workflow | REQUIRED | nix flake check must pass |

**Gate Status**: PASS with justified deferrals (IV partial, V deferred for bootstrap scope)

## Project Structure

### Documentation (this feature)

```text
specs/001-true-self-hosting/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── wasm-exports.md  # compile_form, compile_all interface
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── stage0/              # EXTEND: Stage 0 bootstrap compiler
│   ├── package.lisp     # Package definition
│   ├── types.lisp       # Type indices and definitions
│   ├── globals.lisp     # Global variables (NIL, UNBOUND, etc.)
│   ├── reader.lisp      # S-expression reader (existing)
│   ├── ast.lisp         # AST representation
│   ├── ir.lisp          # Intermediate representation
│   ├── eval.lisp        # NEW: Core interpreter/evaluator
│   ├── primitives.lisp  # NEW: Primitive implementations
│   ├── codegen.lisp     # Wasm code generation
│   ├── compiler.lisp    # Compilation orchestration
│   ├── output.lisp      # Binary output
│   ├── exports.lisp     # Wasm exports
│   └── entry.lisp       # Entry point
│
├── bootstrap/           # Bootstrap infrastructure
│   ├── interpreter-stage0.lisp  # Interpreter-based Stage 0
│   └── fixpoint.lisp    # Fixed-point verification
│
└── lib/                 # Shared utilities
    └── utf8.lisp        # UTF-8 encoding

tests/
├── unit/
│   └── stage0/          # NEW: Stage 0 interpreter tests
│       ├── eval-test.lisp
│       ├── primitives-test.lisp
│       └── codegen-test.lisp
├── contract/
│   └── stage0-exports-test.lisp
└── integration/
    └── fixpoint-test.lisp

build/
├── bootstrap.lisp       # SBCL bootstrap script
├── stage0-complete.lisp # Stage 0 generation
├── stage1-gen.lisp      # Stage 1 generation
├── stage2-gen.lisp      # Stage 2 generation
└── fixpoint-check.lisp  # Verification

dist/
├── clysm-stage0.wasm    # Stage 0 binary (currently 275 bytes, target > 1KB)
├── clysm-stage1.wasm    # Stage 1 binary (currently 17 bytes, target >= 1KB)
└── clysm-stage2.wasm    # Stage 2 binary (must == Stage 1)
```

**Structure Decision**: Single project extending existing `src/clysm/stage0/` module. New files: `eval.lisp`, `primitives.lisp` for interpreter core. Tests in `tests/unit/stage0/`.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| Principle IV partial (no EH) | Bootstrap scope limitation | Full exception handling adds complexity; can iterate after fixed-point |
| Principle V deferred | No special variables needed for minimal bootstrap | Shallow binding adds 3+ files; not required for target forms |

## Phase 0: Research Findings

See [research.md](./research.md) for detailed findings.

**Key Decisions**:
1. **Interpreter Architecture**: Recursive evaluator with environment passing
2. **S-expression Input**: Host parses string to pre-constructed AST, Stage 0 evaluates
3. **Wasm Output Strategy**: Direct binary emission (existing codegen infrastructure)
4. **Symbol Interning**: String-based equality (no package system)
5. **Fixed-Point Source**: Minimal compiler definition as S-expressions
