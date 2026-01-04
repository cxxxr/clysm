# Progress Tracking: Compiler Code Generation Cleanup

**Feature**: 001-codegen-cleanup
**Started**: 2026-01-04
**Target**: func-section.lisp < 8,000 lines (from 15,973)

## Baseline Metrics (Phase 1)

| Metric | Value | Target |
|--------|-------|--------|
| Line count | 15,973 | <8,000 |
| Quasiquote patterns | 111 | 0 |
| Stage 1 size | 21,263 bytes | - |
| Wasm validation | PASS | PASS |
| Coverage rate | 9.86% | ≥22% |
| Test suite | PASS | PASS |

---

## Batch Removal Progress

### Batch 1: String Functions
**Status**: ✅ COMPLETE
**Target**: ~14,500 lines

| Checkpoint | Value | Date |
|------------|-------|------|
| Functions removed | 12 (all string functions already removed in 001-string-runtime-migration) | 2026-01-04 |
| Lines removed | ~500 (trim functions + nstring-capitalize) | 2026-01-04 |
| New line count | 14,894 | 2026-01-04 |
| Tests | PASS | 2026-01-04 |
| Wasm validation | PASS | 2026-01-04 |
| Commit | Pending (batch consolidation) | - |

### Batch 2: Numeric Functions
**Status**: ✅ COMPLETE
**Target**: ~13,700 lines

| Checkpoint | Value | Date |
|------------|-------|------|
| Functions removed | 5 (signum, phase, rationalize, write-to-string, parse-integer) | 2026-01-04 |
| Lines removed | ~900 | 2026-01-04 |
| New line count | 13,994 | 2026-01-04 |
| Tests | Stage 1 compiles (20,906 bytes) | 2026-01-04 |
| Wasm validation | PASS | 2026-01-04 |
| Commit | Pending (batch consolidation) | - |

### Batch 3: Sequence Functions
**Status**: ✅ ALREADY COMPLETE (previous migration)
**Target**: N/A

| Checkpoint | Value | Date |
|------------|-------|------|
| Functions removed | 15 (in 001-sequence-runtime-migration) | Previous |
| Lines removed | Included in previous migration | Previous |
| New line count | N/A | - |
| Tests | PASS | - |
| Wasm validation | PASS | - |
| Commit | N/A | - |

### Batch 4: List Functions
**Status**: ✅ ALREADY COMPLETE (previous migration)
**Target**: N/A

| Checkpoint | Value | Date |
|------------|-------|------|
| Functions removed | 13 (in 001-io-list-runtime) | Previous |
| Lines removed | Included in previous migration | Previous |
| New line count | N/A | - |
| Tests | PASS | - |
| Wasm validation | PASS | - |
| Commit | N/A | - |

### Batch 5: I/O Functions
**Status**: ✅ ALREADY COMPLETE (previous migration)
**Target**: N/A

| Checkpoint | Value | Date |
|------------|-------|------|
| Functions removed | 6 (in 001-io-list-runtime) | Previous |
| Lines removed | Included in previous migration | Previous |
| New line count | N/A | - |
| Tests | PASS | - |
| Wasm validation | PASS | - |
| Commit | N/A | - |

### Batch 6: Miscellaneous Helpers
**Status**: ✅ COMPLETE (partial - orphaned helpers only)
**Target**: <8,000 lines (NOT MET - further analysis needed)

| Checkpoint | Value | Date |
|------------|-------|------|
| Functions removed | 10 (orphaned helpers) | 2026-01-04 |
| Lines removed | ~389 | 2026-01-04 |
| New line count | 13,605 | 2026-01-04 |
| Tests | Stage 1 compiles (17,336 bytes) | 2026-01-04 |
| Wasm validation | PASS | 2026-01-04 |
| Commit | Pending | - |

**Removed functions:**
- emit-numeric-type-p, emit-get-numeric-type, numeric-type-contagion, emit-coerce-to-float, emit-coerce-to-complex (numeric tower infrastructure)
- emit-string-comparison-case-sensitive, emit-string-comparison-case-insensitive (superseded)
- compile-string-compare-at-end, compile-string-compare-at-diff (superseded by v2)
- %generate-in-char-bag-check (orphaned)

---

## Quasiquote Migration Progress

**Status**: Pending (After Batch 6)

| Phase | Patterns | Remaining | Date |
|-------|----------|-----------|------|
| Baseline | 111 | 111 | 2026-01-04 |
| After dead code removal | - | - | - |
| Migration batch 1 (A-G) | - | - | - |
| Migration batch 2 (H-M) | - | - | - |
| Migration batch 3 (N-S) | - | - | - |
| Migration batch 4 (T-Z) | - | - | - |
| **Final** | - | 0 | - |

---

## Success Criteria Checklist

| ID | Criterion | Status |
|----|-----------|--------|
| SC-001 | func-section.lisp < 8,000 lines | ❌ Pending |
| SC-002 | All unit tests pass (100%) | ✅ PASS |
| SC-003 | Stage 1 Wasm validates | ✅ PASS |
| SC-004 | Zero quasiquote patterns | ❌ Pending |
| SC-005 | Coverage rate ≥ 22% | ❌ Pending (9.86%) |
| SC-006 | Validated batch removal | ❌ In Progress |

---

## Line Count History

| Date | Event | Lines | Δ |
|------|-------|-------|---|
| 2026-01-04 | Baseline | 15,973 | - |
| 2026-01-04 | After Batch 1 (String) | 14,894 | -1,079 |
| 2026-01-04 | After Batch 2 (Numeric) | 13,994 | -900 |
| - | Batch 3-5 already removed | (previous migrations) | - |
| 2026-01-04 | After Batch 6 (Helpers) | 13,605 | -389 |
| - | After Migration | - | - |

**Total removed so far**: 2,368 lines (15,973 → 13,605)
**Remaining gap to target (<8,000)**: ~5,605 lines
