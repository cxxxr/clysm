# Implementation Plan: Advanced Defmacro and Compile-Time Macro Expansion

**Branch**: `042-advanced-defmacro` | **Date**: 2025-12-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/042-advanced-defmacro/spec.md`

## Summary

Implement advanced `defmacro` features including `&whole` (complete macro call form access) and `&environment` (lexical environment access) parameters, plus runtime `macroexpand`/`macroexpand-1` functions in compiled Wasm code. This extends the existing Feature 016 macro system to achieve full ANSI CL compliance and enable self-compilation of Clysm's 27 `defmacro` definitions.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing); existing clysm/compiler (Feature 016 macro system), clysm/lib/destructuring (Feature 031)
**Storage**: N/A (in-memory macro registry, WasmGC globals for runtime)
**Testing**: rove (unit/contract/integration), wasm-tools validate
**Target Platform**: WasmGC (browsers with GC proposal support, wasmtime)
**Project Type**: Single - compiler extension
**Performance Goals**: Macro expansion <1ms per form, runtime macroexpand <10ms for 100-form chain
**Constraints**: No linear memory access, WasmGC-only types, ANSI CL compliance
**Scale/Scope**: 27 defmacro forms in Clysm source, 1000-step circular expansion limit

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. WasmGC-First | PASS | Environment struct uses WasmGC types (anyref fields), no linear memory |
| II. Lisp Object Representation | PASS | NIL/UNBOUND handling preserved, macro registry uses symbol keys |
| III. Closure Implementation | PASS | Macro expanders are closures with $code_1 (form arg) |
| IV. Wasm Control Flow | PASS | No control flow changes needed for macro expansion |
| V. Shallow Binding | N/A | Macros are compile-time, no dynamic scope interaction |
| VI. Tiered Eval/JIT | PASS | Runtime macroexpand integrates with Tier 1 interpreter |
| VII. TDD (Non-negotiable) | PASS | Test files defined for each phase before implementation |
| VIII. Nix-First | PASS | No new external dependencies, uses existing devShell tools |

**Gate Result**: PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/042-advanced-defmacro/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── macro-api.lisp   # Lisp function signatures
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── transform/
│   │   └── macro.lisp        # MODIFY: &whole/&environment parsing, macro-function
│   ├── codegen/
│   │   ├── func-section.lisp # MODIFY: runtime macroexpand compilation
│   │   └── gc-types.lisp     # MODIFY: add $macro-environment type
│   └── ast.lisp              # MODIFY: add ast-macro-function node
├── lib/
│   └── destructuring.lisp    # READ: reuse &whole parsing logic
└── runtime/
    └── macro-runtime.lisp    # NEW: runtime macro registry functions

tests/
├── unit/
│   └── macro/
│       ├── whole-test.lisp           # NEW
│       ├── environment-test.lisp     # NEW
│       └── macro-function-test.lisp  # NEW
├── contract/
│   └── macro-wasm-test.lisp          # NEW
└── integration/
    └── macro-ansi-test.lisp          # NEW
```

**Structure Decision**: Single project extending existing compiler structure. Macro system files are in `compiler/transform/` with runtime support in `runtime/`.

## Complexity Tracking

No violations detected. Implementation extends existing patterns without adding architectural complexity.

## Post-Design Constitution Check

*Re-evaluation after Phase 1 design completion.*

| Principle | Status | Post-Design Notes |
|-----------|--------|-------------------|
| I. WasmGC-First | PASS | $macro-environment (Type 24) defined with anyref fields per data-model.md |
| II. Lisp Object Representation | PASS | Environment uses symbol keys, NIL as parent terminator |
| III. Closure Implementation | PASS | Macro expanders use standard $closure with $code_1 |
| IV. Wasm Control Flow | PASS | No new control flow patterns needed |
| V. Shallow Binding | N/A | No dynamic variable interaction |
| VI. Tiered Eval/JIT | PASS | Runtime macroexpand specified for Tier 1 integration |
| VII. TDD (Non-negotiable) | PASS | Test structure defined in quickstart.md |
| VIII. Nix-First | PASS | No flake.nix changes required |

**Post-Design Gate Result**: PASS - Ready for /speckit.tasks
