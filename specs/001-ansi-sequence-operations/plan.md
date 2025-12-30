# Implementation Plan: ANSI CL Sequence Operations

**Branch**: `001-ansi-sequence-operations` | **Date**: 2025-12-29 | **Spec**: [spec.md](./spec.md)
**Input**: Phase 13D-2: ANSI CL シーケンス操作関数を実装する

## Summary

Implement ANSI CL sequence operation primitives ([subseq](../../resources/HyperSpec/Body/f_subseq.htm), [concatenate](../../resources/HyperSpec/Body/f_concat.htm), [make-string](../../resources/HyperSpec/Body/f_mk_stg.htm), [make-array](../../resources/HyperSpec/Body/f_mk_ar.htm) extensions, [copy-seq](../../resources/HyperSpec/Body/f_cp_seq.htm)) required for string/byte-array processing in the compiler. These functions are essential for compiling tokenizer.lisp, utf8.lisp, and leb128.lisp, targeting a 40%+ compilation rate.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), wasmtime, wasm-tools
**Storage**: N/A (in-memory compilation)
**Testing**: rove testing framework (`sbcl --eval "(asdf:test-system :clysm)"`)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler)
**Performance Goals**: Compilation of 1000+ forms/sec; runtime array operations at native Wasm speed
**Constraints**: WasmGC array instructions only; no linear memory access
**Scale/Scope**: ~23% → 40%+ compilation rate improvement

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | PASS | Uses WasmGC `array.new`, `array.copy`, `array.new_fixed` |
| II. Lisp Object Representation | PASS | NIL singleton, sequence types follow existing patterns |
| III. Function/Closure Strategy | N/A | Not adding new closure types |
| IV. Wasm Control Flow | PASS | Bounds checking via `ref.test`/`br_if` |
| V. Shallow Binding | N/A | No dynamic scope changes |
| VI. Tiered Eval/JIT | PASS | Interpreter builtins already exist; adding compiler support |
| VII. TDD (Non-Negotiable) | PASS | Unit/contract/integration tests required before implementation |
| VIII. Nix-First Workflow | PASS | `nix flake check` must pass |
| IX. ANSI CL HyperSpec Reference | PASS | All HyperSpec links included in this plan |

**No violations. Proceeding to Phase 0.**

## Project Structure

### Documentation (this feature)

```text
specs/001-ansi-sequence-operations/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── sequence-ops.md  # Wasm instruction contracts
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── codegen/
│   │   └── func-section.lisp  # ADD: compile-subseq, compile-concatenate, etc.
│   └── compiler.lisp          # ADD: dispatch for sequence operations
├── lib/
│   └── setf-expanders.lisp    # MODIFY: (setf subseq) already exists, verify
└── eval/
    └── interpreter-builtins.lisp  # EXISTS: subseq, copy-seq, concatenate bindings

tests/
├── contract/
│   └── sequence-wasm-test.lisp  # NEW: Wasm output validation
├── integration/
│   └── sequence-test.lisp       # EXISTS: extend with new test cases
└── unit/
    └── sequence-codegen-test.lisp  # NEW: codegen unit tests
```

**Structure Decision**: Single project following existing Clysm compiler layout. New sequence operation codegen functions added to `func-section.lisp` following the pattern of existing primitives.

## Complexity Tracking

No violations requiring justification.

## HyperSpec References

| Function | HyperSpec | Priority |
|----------|-----------|----------|
| [subseq](../../resources/HyperSpec/Body/f_subseq.htm) | f_subseq.htm | P1 |
| [concatenate](../../resources/HyperSpec/Body/f_concat.htm) | f_concat.htm | P2 |
| [make-string](../../resources/HyperSpec/Body/f_mk_stg.htm) | f_mk_stg.htm | P3 |
| [make-array](../../resources/HyperSpec/Body/f_mk_ar.htm) | f_mk_ar.htm | P4 |
| [copy-seq](../../resources/HyperSpec/Body/f_cp_seq.htm) | f_cp_seq.htm | P5 |

## WasmGC Instructions Used

| Instruction | Purpose |
|-------------|---------|
| `array.new` | Create new array with default value (for make-string, make-array :initial-element) |
| `array.new_fixed` | Create array with specified initial values (for make-array :initial-contents) |
| `array.copy` | Copy elements between arrays (for subseq on vectors, copy-seq) |
| `array.len` | Get array length (for bounds validation, default end index) |
| `array.get` | Read element (for subseq on lists → vector conversion) |
| `array.set` | Write element (for setf subseq) |

## Implementation Strategy

### Phase 1: subseq (P1)

1. Add `compile-subseq` function to `func-section.lisp`
2. Type dispatch: string → byte-level subseq, vector → array.copy, list → iterate cons cells
3. Bounds validation with `bounding-indices-bad-error` signal
4. Wire up `(setf subseq)` to existing setf expander infrastructure

### Phase 2: concatenate (P2)

1. Add `compile-concatenate` function
2. Result-type dispatch: `'string`, `'vector`, `'list`
3. Multi-sequence iteration and accumulation
4. Handle empty sequence arguments

### Phase 3: make-string (P3)

1. Add `compile-make-string` function
2. Use `array.new` with `:initial-element` character UTF-8 byte value
3. Handle default (null character) case

### Phase 4: make-array extensions (P4)

1. Extend existing `compile-make-array` (if present) or add new function
2. `:initial-element` → `array.new`
3. `:initial-contents` → `array.new_fixed` or loop initialization

### Phase 5: copy-seq (P5)

1. Add `compile-copy-seq` function
2. Type dispatch similar to subseq
3. Full sequence copy (start=0, end=length)
