# Implementation Plan: ANSI Common Lisp Multiple Values Support

**Branch**: `025-multiple-values` | **Date**: 2025-12-26 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/025-multiple-values/spec.md`

## Summary

Implement ANSI Common Lisp multiple values support by:
1. Adding two Wasm globals: `$mv-count` (mutable i32) for value count and `$mv-buffer` (mutable array of anyref) for secondary values
2. Implementing `values` special form that stores secondary values in buffer and returns primary value
3. Implementing receiving forms: `multiple-value-bind`, `multiple-value-list`, `nth-value`, `values-list`, `multiple-value-prog1`, `multiple-value-call`
4. Updating `floor`, `truncate`, `ceiling`, `round` to return quotient and remainder as two values

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for compiler; WasmGC for output
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing); existing clysm/compiler, clysm/runtime modules
**Storage**: N/A (in-memory globals within Wasm module)
**Testing**: rove + wasmtime + wasm-tools validate
**Target Platform**: WasmGC (browsers, wasmtime with GC support)
**Project Type**: Single compiler project
**Performance Goals**: No performance regression for single-value functions; O(1) value access
**Constraints**: Max 20 values (ANSI minimum for `multiple-values-limit`); globals must be initialized before first function call
**Scale/Scope**: 8 new forms + 4 updated arithmetic functions + 2 new globals

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | PASS | mv-buffer uses WasmGC array type; mv-count uses i32 |
| II. Lisp Object Representation | PASS | Values stored as anyref; NIL used for missing values |
| III. Function/Closure Strategy | PASS | Multi-value buffer follows constitution pattern (III.多値バッファ) |
| IV. Wasm Control Flow | PASS | No new control flow; values stored before return |
| V. Shallow Binding | N/A | Multiple values are not dynamic variables |
| VI. Tiered Eval/JIT | PASS | Forms compile to Wasm; interpreter can use same buffer |
| VII. TDD (Non-negotiable) | PASS | Tests written first per methodology |
| VIII. Nix-First Workflow | PASS | Using existing nix develop environment |

**Pre-Design Gate**: PASSED - No violations

## Project Structure

### Documentation (this feature)

```text
specs/025-multiple-values/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (via /speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                    # Add values AST node handling
│   └── codegen/
│       └── func-section.lisp       # Add compile-values, update floor/truncate/etc
├── runtime/
│   ├── multi-value.lisp            # Extend with Wasm global generators
│   └── objects.lisp                # Add mv-count-global-index, mv-buffer-global-index
└── lib/
    └── macros.lisp                 # Add multiple-value-bind, multiple-value-list, etc

tests/
├── unit/
│   └── multiple-values-test.lisp   # Unit tests for each form
├── contract/
│   └── mv-wasm-test.lisp           # Wasm validation tests
└── integration/
    └── mv-ansi-test.lisp           # ANSI compliance tests
```

**Structure Decision**: Single project structure. All new code integrates with existing compiler and runtime modules. Tests follow established pattern (unit/contract/integration).

## Complexity Tracking

*No violations requiring justification*

## Implementation Architecture

### Global Variables

```
Index 0: $nil (const anyref) - existing
Index 1: $unbound (const anyref) - existing
Index 2: $mv-count (mut i32) - NEW: number of values
Index 3: $mv-buffer (mut (ref $mv-array)) - NEW: array of 20 anyref slots
Index 4+: special variables (as before, shifted by 2)
```

### Type Definitions

```wat
;; New array type for multiple values buffer
(type $mv-array (array (mut anyref)))
```

### Value Protocol

1. **Returning values**: `(values v1 v2 v3)`
   - Store count 3 in `$mv-count`
   - Store v2 at `$mv-buffer[0]`, v3 at `$mv-buffer[1]`
   - Return v1 on stack (primary value)

2. **Receiving values**: `(multiple-value-bind (a b c) form ...)`
   - Evaluate form (primary value on stack)
   - Read `$mv-count` to determine how many values available
   - Read secondary values from `$mv-buffer` by index
   - Bind NIL for variables beyond available values

3. **Single-value context**:
   - Primary value used directly from stack
   - `$mv-count` and `$mv-buffer` are implicitly ignored
