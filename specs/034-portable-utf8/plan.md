# Implementation Plan: Portable UTF-8 Encoding

**Branch**: `034-portable-utf8` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/034-portable-utf8/spec.md`

## Summary

Implement portable UTF-8 encoding/decoding functions (`string-to-utf8-octets`, `utf8-octets-to-string`) in pure Common Lisp to replace Babel dependency. The implementation will handle the full Unicode range (U+0000 to U+10FFFF), validate UTF-8 sequences, and signal `decoding-error` for invalid input. All 6 existing `babel:string-to-octets` call sites in the compiler will be replaced, eliminating the Babel dependency for cross-implementation portability (SBCL, CCL, ECL).

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+, CCL, ECL - portable subset)
**Primary Dependencies**: None (pure portable CL; removes babel dependency)
**Storage**: N/A (in-memory byte vectors)
**Testing**: Rove framework (existing test infrastructure)
**Target Platform**: SBCL 2.4+, CCL, ECL (any ANSI CL implementation)
**Project Type**: Single project (compiler library)
**Performance Goals**: Comparable to Babel (~1000 chars/ms for typical compiler strings)
**Constraints**: No SBCL-specific internals; pure portable Common Lisp only
**Scale/Scope**: 6 call sites to replace; strings typically <1KB in compiler

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | N/A | Library code, not Wasm output |
| II. Lispオブジェクト表現規約 | N/A | Library code, not Wasm output |
| III. 関数・クロージャ実装戦略 | N/A | Library code, not Wasm output |
| IV. Wasm制御フロー活用 | N/A | Library code, not Wasm output |
| V. シャローバインディング | N/A | Library code, not Wasm output |
| VI. 段階的動的コンパイル | N/A | Library code, not Wasm output |
| VII. TDD (非交渉) | **PASS** | Unit tests for encoding/decoding, contract tests for Wasm output validation |
| VIII. Nix-Firstワークフロー | **PASS** | Uses existing Nix environment; no new dependencies |

**Gate Result**: PASS - Feature is a pure library addition that doesn't affect WasmGC output structure.

## Project Structure

### Documentation (this feature)

```text
specs/034-portable-utf8/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── lib/
│   │   ├── utf8.lisp           # NEW: Portable UTF-8 implementation
│   │   ├── macros.lisp
│   │   ├── setf-expanders.lisp
│   │   └── ...
│   ├── compiler/
│   │   ├── compiler.lisp       # MODIFY: Replace babel usage (line 545)
│   │   └── codegen/
│   │       └── func-section.lisp  # MODIFY: Replace babel usage (line 418)
│   ├── backend/
│   │   └── sections.lisp       # MODIFY: Replace babel usage (line 74)
│   └── ffi/
│       ├── import-gen.lisp     # MODIFY: Replace babel usages (lines 89, 93)
│       └── export-gen.lisp     # MODIFY: Replace babel usage (line 51)

tests/
├── unit/
│   └── utf8-test.lisp          # NEW: Unit tests for encoding/decoding
├── contract/
│   └── utf8-wasm-test.lisp     # NEW: Verify Wasm output unchanged
└── integration/
    └── utf8-migration-test.lisp # NEW: End-to-end migration verification
```

**Structure Decision**: Single project layout (existing structure). New file `src/clysm/lib/utf8.lisp` follows existing pattern in lib/ directory.

## Complexity Tracking

> No violations requiring justification. This is a straightforward library replacement.

## Implementation Phases

### Phase 1: Core UTF-8 Implementation (P1)

1. Create `src/clysm/lib/utf8.lisp` with:
   - `string-to-utf8-octets` function
   - UTF-8 encoding logic using `char-code` (portable CL)
   - Optimized byte vector construction

2. Unit tests for encoding:
   - ASCII strings
   - 2-byte sequences (Latin extended, etc.)
   - 3-byte sequences (CJK, Japanese)
   - 4-byte sequences (emoji, supplementary planes)
   - Empty strings
   - Maximum code point (U+10FFFF)

### Phase 2: UTF-8 Decoding Implementation (P2)

1. Add `utf8-octets-to-string` function
2. Define `decoding-error` condition with slots:
   - `position`: byte offset of error
   - `invalid-bytes`: the problematic byte sequence
3. Implement validation for:
   - Invalid lead bytes (0xC0-0xC1, 0xF5-0xFF)
   - Missing continuation bytes
   - Unexpected continuation bytes
   - Overlong encodings
   - Surrogate code points (U+D800-U+DFFF)

### Phase 3: Compiler Migration (P1)

1. Replace all 6 `babel:string-to-octets` calls:
   - `src/clysm/compiler/compiler.lisp:545`
   - `src/clysm/compiler/codegen/func-section.lisp:418`
   - `src/clysm/ffi/import-gen.lisp:89`
   - `src/clysm/ffi/import-gen.lisp:93`
   - `src/clysm/ffi/export-gen.lisp:51`
   - `src/clysm/backend/sections.lisp:74`

2. Verification:
   - `grep -r 'babel:' src/` returns 0 matches
   - All existing tests pass
   - Wasm output validates with `wasm-tools validate`

### Phase 4: Package Cleanup

1. Update `clysm.asd`:
   - Add `utf8.lisp` to :components
   - Remove babel from :depends-on (if no other usages)
2. Export `string-to-utf8-octets` and `utf8-octets-to-string` from clysm package
