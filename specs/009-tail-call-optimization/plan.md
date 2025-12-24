# Implementation Plan: Tail Call Optimization

**Branch**: `009-tail-call-optimization` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/009-tail-call-optimization/spec.md`

## Summary

Implement tail call optimization (TCO) for the clysm compiler by:
1. Adding a `tail-position-p` flag to the compilation environment
2. Modifying code generation to emit `return_call` for direct tail calls
3. Modifying code generation to emit `return_call_ref` for indirect tail calls (`funcall`, local functions)

This enables recursive Lisp functions to execute without stack overflow by leveraging WebAssembly's native tail call instructions.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compiler generates Wasm binaries)
**Testing**: rove test framework with `nix flake check`
**Target Platform**: WebAssembly GC with tail-call extension (wasmtime)
**Project Type**: Single project (compiler)
**Performance Goals**: 10,000+ recursion depth without stack overflow
**Constraints**: Must maintain backward compatibility with existing non-tail calls
**Scale/Scope**: ~2-3 files modified, ~100-200 lines of new code

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | PASS | Uses native Wasm tail call instructions |
| II. Lisp Object Representation | PASS | No changes to object representation |
| III. Function/Closure Strategy | PASS | Extends existing closure dispatch with tail variants |
| IV. Wasm Control Flow Activation | PASS | **Core requirement**: return_call/return_call_ref |
| V. Shallow Binding | PASS | No changes to dynamic scope handling |
| VI. Tiered Eval/JIT | N/A | Does not affect eval/JIT tiers |
| VII. TDD (Non-negotiable) | REQUIRED | Tests exist in tco-test.lisp, implementation needed |
| VIII. Nix-First Workflow | PASS | Uses existing Nix environment |

**Gate Status**: PASS - Aligns with Constitution Principle IV (Wasm Control Flow Activation)

## Project Structure

### Documentation (this feature)

```text
specs/009-tail-call-optimization/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/compiler/
├── codegen/
│   └── func-section.lisp    # MODIFY: Add tail-position flag and aware code generation
└── compiler.lisp            # MODIFY: Add return_call/return_call_ref instruction emission

tests/
├── integration/
│   └── tco-test.lisp        # EXISTS: Already has TCO tests
└── unit/
    └── tail-position-test.lisp  # CREATE: Unit tests for tail position detection
```

**Structure Decision**: Single project structure following existing compiler organization. New code integrates into existing codegen/ directory. Tail position flag added inline to func-section.lisp rather than separate analyzer module.

## Complexity Tracking

No constitution violations requiring justification.
