# Data Model: Compiler Subset Validation

**Date**: 2025-12-27
**Feature**: 036-compiler-subset-validation

## Entities

### 1. CL-Feature

Represents a Common Lisp symbol/form and its support status in Clysm.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| symbol | symbol | The CL symbol (e.g., `defun`, `loop`) | Required, unique |
| category | keyword | Feature type | `:special-form`, `:macro`, `:function`, `:type` |
| status | keyword | Support level | `:supported`, `:partial`, `:unsupported`, `:internal` |
| notes | string | Usage notes for partial features | Optional, non-nil for `:partial` |

**Validation Rules**:
- If `status` is `:partial`, `notes` MUST be non-nil
- `symbol` must be a valid CL symbol (interned in CL or CLYSM package)

### 2. Module

Represents a Lisp source file in the compiler codebase.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| path | pathname | File path relative to project root | Required, must exist |
| directory | keyword | Parent module directory | `:backend`, `:reader`, `:compiler`, `:runtime`, `:clos`, `:conditions` |
| dependencies | list of Module | Modules this depends on | May be empty for leaf modules |
| symbols-used | list of symbol | All CL symbols referenced | Computed by analyzer |

**Validation Rules**:
- `path` must point to an existing `.lisp` file
- `directory` must be one of the 6 target directories

### 3. Feature-Usage

Represents the usage of a CL feature within a specific file.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| symbol | symbol | The CL symbol used | Required |
| file | pathname | File where symbol appears | Required |
| line-numbers | list of integer | Lines where symbol appears | Non-empty |
| occurrence-count | integer | Total occurrences | >= 1 |

### 4. Coverage-Report

Represents the analysis results for a module or the entire codebase.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| scope | keyword or pathname | `:all` or specific file | Required |
| total-symbols | integer | Total unique symbols analyzed | >= 0 |
| supported-count | integer | Symbols with `:supported` status | >= 0 |
| partial-count | integer | Symbols with `:partial` status | >= 0 |
| unsupported-count | integer | Symbols with `:unsupported` status | >= 0 |
| coverage-pct | float | (supported + partial) / total * 100 | 0.0 to 100.0 |
| unsupported-details | list of Feature-Usage | Details for unsupported features | May be empty |

### 5. Compilation-Result

Represents the result of attempting to compile a module with Clysm.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| module | Module | The module compiled | Required |
| success | boolean | Whether compilation succeeded | Required |
| wasm-bytes | vector of uint8 | Compiled Wasm binary | Non-nil if success=T |
| error-message | string | Error details if failed | Non-nil if success=NIL |
| unsupported-feature | symbol | CL feature that caused failure | Optional |
| validation-passed | boolean | wasm-tools validate result | Only set if success=T |
| validation-error | string | wasm-tools error output | Only set if validation failed |

### 6. Blessed-Subset

The final documentation of self-compilable CL features.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| version | string | Generation date/version | Required |
| special-forms | list of symbol | Supported special forms | Required |
| macros | list of symbol | Supported macros | Required |
| functions | list of symbol | Supported functions | Required |
| types | list of symbol | Supported type specifiers | Required |
| partial-notes | alist | (symbol . notes) for partial features | May be empty |

## Relationships

```text
┌─────────────┐     uses      ┌─────────────┐
│   Module    │───────────────│ CL-Feature  │
└─────────────┘               └─────────────┘
      │ 1                            │ 1
      │                              │
      │ *                            │ *
┌─────────────┐     references ┌─────────────┐
│Feature-Usage│───────────────│Coverage-Rep.│
└─────────────┘               └─────────────┘
      │ *
      │
      │ 1
┌─────────────┐     produces  ┌─────────────┐
│Compilation- │───────────────│  Blessed-   │
│   Result    │               │   Subset    │
└─────────────┘               └─────────────┘
```

## State Transitions

### Module Analysis State

```text
┌──────────┐   analyze   ┌──────────┐   report   ┌──────────┐
│  Unread  │───────────▶│ Analyzed │───────────▶│ Reported │
└──────────┘             └──────────┘             └──────────┘
```

### Compilation Pipeline State

```text
┌──────────┐   compile   ┌──────────┐   validate   ┌──────────┐
│  Pending │────────────▶│ Compiled │─────────────▶│ Validated│
└──────────┘             └──────────┘              └──────────┘
     │                        │                         │
     │ error                  │ wasm-error              │ success
     ▼                        ▼                         ▼
┌──────────┐             ┌──────────┐             ┌──────────┐
│  Failed  │             │Invalid   │             │  Passed  │
│(CL issue)│             │  Wasm    │             │          │
└──────────┘             └──────────┘             └──────────┘
```

## Data Flow

1. **Analysis Phase** (11A):
   - Input: Source files from 6 directories
   - Process: S-expression walker extracts symbols
   - Output: Feature-Usage records per file

2. **Classification Phase** (11A):
   - Input: Feature-Usage records + Feature Registry
   - Process: Hash-table lookup for each symbol
   - Output: Coverage-Report per module and aggregate

3. **Compilation Phase** (11B):
   - Input: Modules in dependency order
   - Process: Clysm compile → wasm-tools validate
   - Output: Compilation-Result per module

4. **Documentation Phase** (11D):
   - Input: All Compilation-Results with success=T
   - Process: Aggregate verified features
   - Output: Blessed-Subset (blessed-subset.lisp)
