# Data Model: Bootstrap Fixpoint Achievement

**Created**: 2025-12-30
**Feature**: 001-bootstrap-fixpoint

## Entities

### Stage (N)

A Stage represents a compiled Clysm compiler at a specific generation level.

| Field | Type | Description |
|-------|------|-------------|
| n | Integer | Stage number (0, 1, 2, ...) |
| wasm_path | String | Path to compiled Wasm binary |
| size_bytes | Integer | Binary size in bytes |
| compilation_rate | Float | Percentage of forms successfully compiled (0-100) |
| report_path | String | Path to compilation report JSON |

**Identity**: Unique by `n` within a bootstrap run.

**Relationships**:
- Stage N is compiled by Stage N-1 (except Stage 0, compiled by SBCL host)

### CompilationForm

A single Lisp expression or definition to be compiled.

| Field | Type | Description |
|-------|------|-------------|
| id | Integer | Sequential form index |
| source_file | String | Originating source file path |
| form_type | Symbol | Type: defun, defmacro, defstruct, etc. |
| form_name | Symbol | Name of defined entity (if applicable) |
| status | Symbol | :success, :failed, :skipped |
| error_category | String | Error category if failed |
| error_message | String | Error details if failed |

**Identity**: Unique by `(source_file, id)`.

### BlockerReport

Aggregated analysis of compilation failures.

| Field | Type | Description |
|-------|------|-------------|
| stage | Integer | Which stage generation this report covers |
| total_forms | Integer | Total forms processed |
| success_count | Integer | Forms compiled successfully |
| fail_count | Integer | Forms that failed |
| categories | List<BlockerCategory> | Aggregated failure categories |
| top_blockers | List<String> | Top 5 blocker names by frequency |
| timestamp | String | ISO 8601 generation timestamp |

### BlockerCategory

A category of compilation failures.

| Field | Type | Description |
|-------|------|-------------|
| name | String | Category name (e.g., "unsupported-macro") |
| count | Integer | Number of failures in this category |
| examples | List<String> | Example form names |
| remediation | String | Suggested fix |

### FixpointResult

Result of comparing two stages.

| Field | Type | Description |
|-------|------|-------------|
| stage1_path | String | Path to Stage 1 binary |
| stage2_path | String | Path to Stage 2 binary |
| identical | Boolean | True if byte-identical |
| stage1_size | Integer | Stage 1 size in bytes |
| stage2_size | Integer | Stage 2 size in bytes |
| first_diff_offset | Integer | Byte offset of first difference (if any) |
| section_diffs | List<SectionDiff> | Section-level differences |

### SectionDiff

Difference at Wasm section level.

| Field | Type | Description |
|-------|------|-------------|
| section_id | Integer | Wasm section ID (1-13) |
| section_name | String | Human-readable name (type, import, func, etc.) |
| stage1_size | Integer | Section size in Stage 1 |
| stage2_size | Integer | Section size in Stage 2 |

## State Transitions

### Bootstrap Pipeline

```
┌─────────────┐
│   SBCL +    │
│   Clysm     │
└─────┬───────┘
      │ compiles
      ▼
┌─────────────┐
│  Stage 1    │
│  (24.5KB)   │
└─────┬───────┘
      │ exports compile_form
      │ compiles
      ▼
┌─────────────┐
│  Stage 2    │
│  (??KB)     │
└─────┬───────┘
      │ compare
      ▼
┌─────────────────────────────────────┐
│  Fixpoint Check                     │
│  Stage 1 == Stage 2 ?               │
│  • YES: Fixpoint achieved           │
│  • NO:  Generate blocker report     │
└─────────────────────────────────────┘
```

### Form Compilation States

```
┌─────────┐
│ pending │
└────┬────┘
     │ compile_form()
     ▼
┌──────────┐     ┌────────┐
│ compiling├────►│ success│
└────┬─────┘     └────────┘
     │ error
     ▼
┌────────┐
│ failed │
└────────┘
```

## Data Volume Assumptions

| Entity | Expected Count |
|--------|----------------|
| Stages | 2-3 (Stage 0, 1, 2) |
| CompilationForms | ~1157 per stage |
| BlockerCategories | ~10-20 unique categories |
| SectionDiffs | 0-13 per comparison |

## Validation Rules

- Stage N can only exist if Stage N-1 exists (except N=0)
- Compilation rate must be 0-100
- BlockerReport.total_forms = success_count + fail_count
- FixpointResult.identical = true implies stage1_size == stage2_size
