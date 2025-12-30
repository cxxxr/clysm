# Research: Bootstrap Fixpoint Achievement

**Created**: 2025-12-30
**Feature**: 001-bootstrap-fixpoint

## Research Questions

1. How to export compile_form from Stage 1?
2. How to achieve deterministic Wasm binary output?
3. How to compare Wasm binaries for fixpoint verification?

---

## 1. Stage 1 Export Mechanism

### Current State

**Stage 0 Export Registry** (`src/clysm/stage0/exports.lisp`):
- Declares `compile_form` at index 4, `compile_all` at index 5, `_initialize` at index 6
- Uses `generate-exports()` to return list of `(name kind index)`
- Pattern: FFI imports at indices 0-3, user functions at 4+

**Stage 1 Stub Module** (`src/clysm/stage1/generator.lisp:156-175`):
- Only exports `_start` function at index 8 (hardcoded bytes)
- Does NOT call `generate-exports()` from Stage 0
- Does NOT export compile_form, compile_all, or _initialize

**Main Compiler** (`src/clysm/compiler/compiler.lisp:140-141`):
- Hardcodes only `_start` as export at index 0

### Decision: Use Stage 0 Export Pattern

**Rationale**: Stage 0 already has the export registry pattern. Stage 1 should:
1. Import `generate-exports()` from `clysm/stage0`
2. Replace hardcoded export section with dynamic generation
3. Wire exported functions to bundled implementations

**Alternative Considered**: Direct hardcoding in Stage 1 generator
- Rejected because: Duplicates logic, increases maintenance burden

### Required Changes

| File | Change |
|------|--------|
| `src/clysm/stage1/generator.lisp` | Replace hardcoded export-section with `generate-exports()` call |
| `src/clysm/stage0/entry.lisp` | Ensure compile_form implementation is bundled |
| `src/clysm/backend/sections.lisp` | Verify `make-export-section` handles multi-export case |

---

## 2. Deterministic Wasm Output

### Requirements

FR-010: "System MUST produce deterministic output (same input always yields identical Wasm bytes)"

### Sources of Non-Determinism

| Source | Mitigation |
|--------|------------|
| Hash table iteration order | Use sorted key lists |
| Timestamps | Don't embed build timestamps |
| Random symbols (gensym) | Use deterministic counter |
| Function ordering | Process files in fixed order |
| Floating point printing | Use consistent format |

### Decision: Sort All Iteration

**Rationale**: Hash tables in SBCL don't guarantee iteration order.

**Implementation**:
- `maphash` → `sort (alexandria:hash-table-keys ...) #'string<` then iterate
- Symbol interning order should be fixed by file processing order
- Global ID counters reset per compilation

**Alternative Considered**: Ordered hash table implementation
- Rejected because: Over-engineering; sorting at output points is simpler

---

## 3. Fixpoint Verification Strategy

### Binary Comparison

**Approach**: Byte-by-byte comparison using shell tools
```bash
cmp -l dist/clysm-stage1.wasm dist/clysm-stage2.wasm
```

### Diff Reporting

When files differ, provide actionable information:

| Metric | Tool |
|--------|------|
| Size difference | `stat -f %z` / `wc -c` |
| First divergence offset | `cmp -l \| head -1` |
| Section-level diff | `wasm-tools objdump` comparison |
| Function count diff | `wasm-tools print \| grep 'func'` |

### Decision: Multi-Level Diff

**Rationale**: Raw byte offset is not actionable. Section-level diff helps identify which part of compilation differs.

**Implementation**:
1. First check: `cmp -s` for quick pass/fail
2. If fail: Extract section info via wasm-tools
3. Report which sections differ and by how much

---

## 4. Blocker Report Format

### Existing Infrastructure

`src/clysm/stage1/blocker.lisp`:
- Already has blocker tracking mechanism
- Categorizes failures by error type
- Aggregates by frequency

### Decision: JSON Report with Categorization

**Format** (`dist/stage2-report.json`):
```json
{
  "compilation_rate": 14.2,
  "forms_total": 1157,
  "forms_success": 164,
  "forms_failed": 993,
  "blockers": [
    {
      "category": "unsupported-macro",
      "count": 450,
      "examples": ["defstruct", "loop"],
      "remediation": "Implement macro expansion"
    }
  ],
  "top_5_blockers": ["defstruct", "loop", "handler-case", "..."],
  "fixpoint_status": "not_achieved"
}
```

**Rationale**: JSON enables CI integration and programmatic analysis.

---

## 5. Compilation Rate Baseline

### Current State (from CLAUDE.md)

- Forms total: 1157
- Forms compiled: 164
- Compilation rate: 14.2%

### Top Blockers (from Phase 13D-7 analysis)

1. `defstruct` - not fully supported
2. `loop` - macro expansion incomplete
3. Complex `defmacro` forms
4. `handler-case` - now supported per 13D-6
5. `values`, `the`, `labels` - now supported per 13D-6

### Decision: Proceed with 14% Rate

**Rationale**: This feature focuses on infrastructure (export, Stage 2 gen, verification). Improving compilation rate is parallel work.

**Implication**: Stage 2 will be mostly stub functions initially. Fixpoint may not be achieved until compilation rate reaches 80%+.

---

## Summary of Decisions

| Topic | Decision | Rationale |
|-------|----------|-----------|
| Export mechanism | Reuse Stage 0 `generate-exports()` | Avoid duplication |
| Determinism | Sort hash table keys | Simple, sufficient |
| Comparison | Multi-level diff (bytes → sections) | Actionable output |
| Report format | JSON with categorization | CI-friendly |
| Compilation rate | Proceed at 14% | Infrastructure first |
