# Implementation Plan: LOOP Macro Extension

**Branch**: `001-loop-extension` | **Date**: 2025-12-30 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-loop-extension/spec.md`

## Summary

Extend the existing LOOP macro implementation (`src/clysm/lib/macros.lisp`) to support hash-table iteration, WITH clause bindings, FINALLY clause execution, and INTO accumulator naming. The parser infrastructure exists; this feature focuses on completing code generation for these clauses.

Reference: [loop](resources/HyperSpec/Body/m_loop.htm)

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8)
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), contract tests (Wasm validation)
**Target Platform**: WasmGC (via clysm compiler)
**Project Type**: Single compiler project
**Performance Goals**: N/A (compile-time macro expansion)
**Constraints**: Must generate valid WasmGC code, ANSI CL semantics compliance
**Scale/Scope**: ~40 hash-table iterations in compiler source

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | PASS | LOOP expands to Wasm-compilable forms (tagbody, go, let*, setq) |
| II. Lispオブジェクト表現規約 | N/A | No NIL/UNBOUND changes |
| III. 関数・クロージャ実装戦略 | N/A | No closure changes |
| IV. Wasm制御フロー活用 | PASS | Uses tagbody/go for iteration (maps to Wasm block/br) |
| V. シャローバインディング | N/A | No special variable changes |
| VI. 段階的動的コンパイル | N/A | Macro expansion, not JIT |
| VII. テスト駆動開発（TDD） | REQUIRED | Tests first for each clause type |
| VIII. Nix-Firstワークフロー | REQUIRED | `nix flake check` must pass |
| IX. ANSI CL仕様参照規約 | REQUIRED | HyperSpec links in code comments |

## Project Structure

### Documentation (this feature)

```text
specs/001-loop-extension/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── contracts/           # Phase 1 output (N/A - no API contracts)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   └── macros.lisp      # Primary modification target (LOOP macro)
└── compiler/
    └── ...              # Codegen consumes LOOP expansion

tests/
├── unit/
│   └── loop-extension/  # New test files for this feature
└── contract/
    └── loop/            # Wasm output validation tests
```

**Structure Decision**: Single project layout. LOOP extension modifies `src/clysm/lib/macros.lisp` and adds tests in `tests/unit/loop-extension/`.

## Complexity Tracking

No violations requiring justification. The implementation extends existing LOOP infrastructure without architectural changes.

---

## Implementation Strategy

### Current State Analysis

The existing LOOP implementation in `src/clysm/lib/macros.lisp` (lines 600-1530) provides:

**Already Implemented (Parser)**:
- `parse-with-clause` (lines 986-1003): Handles `with var = val` and `and` for parallel bindings
- `parse-for-hash` (lines 961-984): Parses `being the hash-keys/hash-values of` syntax
- `parse-accumulation-clause` (lines 1005-1024): Parses `into` modifier
- FINALLY parsing in `parse-loop-clauses` (lines 792-797)

**Missing (Code Generation)**:
1. Hash-table iteration bindings/stepping/termination in `generate-*` functions
2. `using (hash-value v)` / `using (hash-key k)` secondary variable parsing
3. Proper initialization of INTO accumulator variables
4. WITH clause integration with iteration variable initialization order

### Modification Points

| File | Function | Change |
|------|----------|--------|
| `src/clysm/lib/macros.lisp` | `parse-for-hash` | Add `using` clause parsing |
| `src/clysm/lib/macros.lisp` | `generate-iteration-bindings` | Add hash-table iterator setup |
| `src/clysm/lib/macros.lisp` | `generate-termination-tests` | Add hash-table exhaustion test |
| `src/clysm/lib/macros.lisp` | `generate-iteration-steps` | Add hash-table next-entry stepping |
| `src/clysm/lib/macros.lisp` | `generate-accumulator-bindings` | Ensure INTO vars properly initialized |
| `src/clysm/lib/macros.lisp` | `expand-loop` | Already handles finally-forms (verify) |

### Hash-Table Iteration Strategy

ANSI CL requires hash-table iteration to use `with-hash-table-iterator`. The expansion pattern:

```lisp
;; Input:
(loop for k being the hash-keys of ht using (hash-value v) collect (cons k v))

;; Expansion strategy (pseudo):
(let* ((iter-fn (make-hash-table-iterator ht))
       (k nil) (v nil) (result nil))
  (block nil
    (tagbody
     loop-start
       (multiple-value-bind (more-p key val) (funcall iter-fn)
         (unless more-p (go loop-end))
         (setq k key v val))
       ;; body
       (setq result (nconc result (list (cons k v))))
       (go loop-start)
     loop-end)
    result))
```

**Key Decision**: Use `maphash` internally for simplicity since Clysm doesn't yet support `with-hash-table-iterator`. This is semantically equivalent for our use case.

### Implementation Phases

**Phase A: Hash-Table Iteration (P1)**
1. Extend `loop-iter-hash` struct with `using-var` and `using-type` fields
2. Update `parse-for-hash` to parse `using (hash-value v)` / `using (hash-key k)`
3. Implement `generate-hash-iteration-bindings`
4. Implement `generate-hash-termination-test`
5. Implement `generate-hash-iteration-step`

**Phase B: WITH Clause Completion (P2)**
1. Verify `with` bindings appear before iteration bindings in `all-bindings`
2. Add test for sequential binding (let* semantics)
3. Add test for parallel binding (and keyword)

**Phase C: FINALLY Clause Verification (P2)**
1. Verify `finally-forms` execute after loop-end tag
2. Add test for `return` within finally
3. Add test for multiple forms in finally

**Phase D: INTO Accumulator (P3)**
1. Verify INTO variables accessible in body via `loop-accumulation-clause-into-var`
2. Add test for multiple INTO accumulators
3. Add test for INTO + FINALLY interaction
