# Data Model: Stage 1 Compiler Generation

**Feature**: 039-stage1-compiler-gen
**Date**: 2025-12-27
**Status**: Complete

## Entity Definitions

### SourceModule

Represents a Lisp source file in the compiler codebase.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| path | string | Absolute file path | Must exist |
| relative_path | string | Path relative to repo root | Matches compilation-order entry |
| module_index | integer | Position in compilation order | 1-41 |
| forms | list[SourceForm] | Parsed top-level forms | May be empty |
| dependency_group | string | Logical grouping | One of: backend, reader, compiler, runtime, clos, conditions |

### SourceForm

A single top-level S-expression from a source file.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| id | string | Unique identifier | format: `{module_index}:{form_index}` |
| operator | symbol | Head of the form | e.g., defun, defmacro, defstruct |
| name | symbol | Name being defined (if applicable) | nil for non-defining forms |
| source_text | string | Original source code | Preserved for error reporting |
| compilable | boolean | Whether form is in blessed subset | Determined by compilable-form-p |

### CompilationResult

Result of attempting to compile a single form.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| form_id | string | Reference to SourceForm | Must match existing form |
| success | boolean | Whether compilation succeeded | - |
| wasm_bytes | byte[] | Compiled output (if success) | nil on failure |
| error_type | symbol | Category of error (if failed) | One of error categories |
| error_message | string | Detailed error description | - |
| unsupported_feature | symbol | Specific CL feature causing failure | nil if not feature-related |

### ProgressReport

Aggregated statistics for a compilation run.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| timestamp | datetime | When report was generated | ISO 8601 |
| stage0_version | string | Version/commit of Stage 0 used | - |
| modules | list[ModuleStats] | Per-module statistics | 41 entries |
| summary | Summary | Overall statistics | - |

### ModuleStats

Per-module compilation statistics.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| path | string | Module relative path | - |
| total_forms | integer | Number of forms in module | >= 0 |
| compiled | integer | Successfully compiled forms | <= total_forms |
| failed | integer | Failed forms | = total_forms - compiled |
| failures | list[FailureGroup] | Grouped by operator | - |

### FailureGroup

Failures grouped by operator type.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| operator | symbol | The failing operator | - |
| count | integer | Number of failures | > 0 |
| example | string | Representative failing form | First encountered |

### Summary

Overall compilation summary.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| total_forms | integer | Forms across all modules | - |
| compiled | integer | Total successful compilations | - |
| coverage_pct | float | Compilation success rate | 0.0 - 100.0 |
| top_blockers | list[BlockerInfo] | Top 5 blockers by impact | Max 5 entries |

### BlockerInfo

Information about a compilation blocker.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| operator | symbol | Unsupported operator | - |
| affected_forms | integer | Forms affected by this blocker | - |
| impact_pct | float | Percentage of total forms | 0.0 - 100.0 |
| priority | string | Recommended priority | HIGH, MEDIUM, LOW |
| recommendation | string | Suggested action | - |
| examples | list[string] | Up to 3 example forms | Max 3 |

### DiffReport

Comparison between two Wasm binaries.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| stage0 | BinaryInfo | Stage 0 binary details | - |
| stage1 | BinaryInfo | Stage 1 binary details | - |
| differences | DiffDetails | What changed | - |

### BinaryInfo

Wasm binary metadata.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| path | string | File path | Must exist |
| size_bytes | integer | File size | > 0 |
| exports | list[string] | Exported function names | - |
| types | integer | Number of type definitions | >= 0 |
| functions | integer | Number of functions | >= 0 |

### DiffDetails

Differences between binaries.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| size_delta | string | Size change description | e.g., "+516 bytes" |
| missing_exports | list[string] | Exports in Stage 0 not in Stage 1 | - |
| new_exports | list[string] | Exports in Stage 1 not in Stage 0 | - |
| type_changes | list[TypeChange] | Modified type definitions | - |

---

## Entity Relationships

```text
SourceModule (1) ──────< (N) SourceForm
      │
      └── module_index determines compilation order

SourceForm (1) ──────< (0..1) CompilationResult
      │
      └── compilable determines if compilation attempted

ProgressReport (1) ──────< (41) ModuleStats
      │
      └── modules in compilation order

ModuleStats (1) ──────< (N) FailureGroup
      │
      └── failures grouped by operator

ProgressReport (1) ────── (1) Summary
      │
      └── summary.top_blockers derived from all failures

DiffReport (1) ────── (2) BinaryInfo
      │
      └── stage0 and stage1 binaries
```

---

## State Transitions

### Compilation Lifecycle

```text
                    ┌─────────────┐
                    │   PENDING   │
                    │ (form read) │
                    └──────┬──────┘
                           │
              ┌────────────┴────────────┐
              │ compilable-form-p check │
              └────────────┬────────────┘
                           │
            ┌──────────────┴──────────────┐
            │                              │
      ┌─────▼─────┐                 ┌─────▼─────┐
      │  SKIPPED  │                 │ COMPILING │
      │(non-form) │                 │           │
      └───────────┘                 └─────┬─────┘
                                          │
                        ┌─────────────────┴─────────────────┐
                        │                                   │
                  ┌─────▼─────┐                       ┌─────▼─────┐
                  │ COMPILED  │                       │  FAILED   │
                  │ (success) │                       │  (error)  │
                  └───────────┘                       └───────────┘
```

### Progress Report Generation

```text
┌────────────────┐     ┌─────────────────┐     ┌───────────────┐
│ COLLECTING     │ ──► │ AGGREGATING     │ ──► │ GENERATED     │
│ (form results) │     │ (module stats)  │     │ (JSON output) │
└────────────────┘     └─────────────────┘     └───────────────┘
```

---

## Validation Rules

1. **SourceModule.path**: Must be readable file; UTF-8 encoding assumed
2. **SourceForm.operator**: Must be a symbol; nil forms are invalid
3. **CompilationResult.form_id**: Must reference existing SourceForm
4. **ModuleStats.failed**: Must equal total_forms - compiled
5. **Summary.coverage_pct**: Must equal (compiled / total_forms) * 100
6. **BlockerInfo.impact_pct**: Must equal (affected_forms / total_forms) * 100
7. **DiffReport binaries**: Both must pass wasm-tools validation
