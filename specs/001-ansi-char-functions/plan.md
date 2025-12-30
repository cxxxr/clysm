# Implementation Plan: Phase 16A - ANSI Character Functions

**Branch**: `001-ansi-char-functions` | **Date**: 2025-12-31 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-ansi-char-functions/spec.md`

## Summary

Implement 6 ANSI CL character functions to achieve 80%+ compliance in the characters category:
- `graphic-char-p` - printable character predicate
- `standard-char-p` - standard 96-character predicate
- `both-case-p` - has upper/lower case variants
- `char-name` / `name-char` - character ↔ name conversion
- `digit-char` - weight to digit character
- `char-int` - character to integer

All functions compile to WasmGC bytecode using i31ref character representation, consistent with existing `char-upcase`, `char-downcase`, `alpha-char-p`, `digit-char-p`.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) host compiler, WasmGC target
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams
**Storage**: N/A (in-memory compilation)
**Testing**: rove (unit tests), wasm-tools validate (contract tests)
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: single (compiler library)
**Performance Goals**: Character operations compile to inline Wasm with O(1) execution
**Constraints**: Characters represented as i31ref (signed i32), ASCII-focused for bootstrap phase
**Scale/Scope**: 6 new functions, ~300 LOC additions to func-section.lisp

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | Characters use i31ref, no linear memory |
| II. Lispオブジェクト表現規約 | ✅ PASS | NIL returned as proper singleton, not null |
| III. 関数・クロージャ実装戦略 | ✅ PASS | Functions compile to inline code, no closure overhead |
| IV. Wasm制御フロー活用 | ✅ PASS | Uses block/if/br for control flow |
| V. シャローバインディング | N/A | No dynamic variables involved |
| VI. 段階的動的コンパイル | N/A | Static compilation only |
| VII. テスト駆動開発（TDD） | ✅ REQUIRED | Tests must be written first |
| VIII. Nix-Firstワークフロー | ✅ PASS | Uses existing Nix environment |
| IX. ANSI CL仕様参照規約 | ✅ REQUIRED | HyperSpec links for all functions |

**Gate Status**: PASS - No violations, proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/001-ansi-char-functions/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── checklists/
    └── requirements.md  # Quality checklist
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       └── func-section.lisp    # Add 6 new compile-* functions
└── lib/
    └── builtins.lisp            # Register function dispatchers

tests/
├── unit/
│   └── character-functions.lisp # New unit tests
└── contract/
    └── character-wasm.lisp      # Wasm validation tests
```

**Structure Decision**: Follows existing pattern - character functions are added to `func-section.lisp` alongside existing `compile-char-upcase`, `compile-char-downcase`, etc.

## ANSI CL References

Per Constitution Principle IX, all functions reference local HyperSpec:

| Function | HyperSpec Reference |
|----------|---------------------|
| graphic-char-p | [f_graphi.htm](../../resources/HyperSpec/Body/f_graphi.htm) |
| standard-char-p | [f_std_ch.htm](../../resources/HyperSpec/Body/f_std_ch.htm) |
| both-case-p | [f_upper_.htm](../../resources/HyperSpec/Body/f_upper_.htm) |
| char-name | [f_char_n.htm](../../resources/HyperSpec/Body/f_char_n.htm) |
| name-char | [f_name_c.htm](../../resources/HyperSpec/Body/f_name_c.htm) |
| digit-char | [f_digit_.htm](../../resources/HyperSpec/Body/f_digit_.htm) |
| char-int | [f_char_i.htm](../../resources/HyperSpec/Body/f_char_i.htm) |

## Implementation Strategy

### Pattern from Existing Functions

Existing character functions (`compile-char-upcase`, `compile-alpha-char-p`, `compile-digit-char-p`) follow this pattern:

1. Extract i32 from i31ref: `(:ref.cast :i31) :i31.get_s (:local.set ,c-local)`
2. Range comparisons: `(:i32.const N) :i32.ge_s` / `:i32.le_s`
3. Boolean AND via multiply: `:i32.mul`
4. Conditional branches: `(:if (:result :anyref)) ... :else ... :end`
5. Return T: `(:i32.const 1) :ref.i31`
6. Return NIL: `(:ref.null :none)`

### Function-Specific Strategies

| Function | Strategy |
|----------|----------|
| graphic-char-p | Range check 32-126 |
| standard-char-p | Check against 96-character set (A-Z, a-z, 0-9, space, newline, 13 punctuation) |
| both-case-p | Check A-Z or a-z (same as alpha-char-p) |
| char-name | Lookup table for 9 named characters, return $string or NIL |
| name-char | String comparison (case-insensitive) for 9 names |
| digit-char | Inverse of digit-char-p: weight → character code |
| char-int | Identity (return i31ref as-is, equivalent to char-code) |

## Complexity Tracking

> No constitution violations requiring justification.

| Item | Rationale |
|------|-----------|
| None | All implementations follow established patterns |
