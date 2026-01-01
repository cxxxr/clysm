# Data Model: Internal Function Export System

**Feature**: `001-internal-function-export`
**Date**: 2026-01-01

## Overview

This feature modifies the package export structure, not data storage. The "data model" here describes the symbol visibility relationships between packages.

## Package Export Relationships

### Current State (Problem)

```
┌─────────────────────────────────────────────────────────────────┐
│ Main Package: clysm                                              │
│ ┌─────────────────────────────────────────────────────────────┐ │
│ │ Exported symbols (partial):                                  │ │
│ │ - compile-to-wasm                                            │ │
│ │ - compile-unary-math-ffi  ✓                                  │ │
│ │ - compile-cxr-chain       ✓                                  │ │
│ │                                                              │ │
│ │ Missing re-exports:                                          │ │
│ │ - lexical-env-parent      ✗                                  │ │
│ │ - compile-to-instructions ✗                                  │ │
│ │ - make-wasm-struct-type   ✗                                  │ │
│ │ - ast-literal-value       ✗                                  │ │
│ └─────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
           │                    │                    │
           ▼                    ▼                    ▼
┌──────────────────┐ ┌─────────────────────┐ ┌──────────────────────┐
│ clysm/compiler/  │ │ clysm/compiler/     │ │ clysm/compiler/      │
│ env              │ │ codegen/func-section│ │ codegen/gc-types     │
├──────────────────┤ ├─────────────────────┤ ├──────────────────────┤
│ Exports:         │ │ Exports:            │ │ Exports:             │
│ - lexical-env-   │ │ - compile-to-       │ │ - make-wasm-struct-  │
│   parent ✓       │ │   instructions ✓    │ │   type ✓             │
└──────────────────┘ └─────────────────────┘ └──────────────────────┘
```

### Target State (Solution)

```
┌─────────────────────────────────────────────────────────────────┐
│ Main Package: clysm                                              │
│ ┌─────────────────────────────────────────────────────────────┐ │
│ │ Exported symbols (complete):                                 │ │
│ │ - compile-to-wasm                                            │ │
│ │ - compile-unary-math-ffi  ✓                                  │ │
│ │ - compile-cxr-chain       ✓                                  │ │
│ │ - lexical-env-parent      ✓ (NEW)                            │ │
│ │ - compile-to-instructions ✓ (NEW)                            │ │
│ │ - make-wasm-struct-type   ✓ (NEW)                            │ │
│ │ - ast-literal-value       ✓ (NEW)                            │ │
│ └─────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
           │                    │                    │
    :import-from          :import-from          :import-from
           │                    │                    │
           ▼                    ▼                    ▼
┌──────────────────┐ ┌─────────────────────┐ ┌──────────────────────┐
│ clysm/compiler/  │ │ clysm/compiler/     │ │ clysm/compiler/      │
│ env              │ │ codegen/func-section│ │ codegen/gc-types     │
└──────────────────┘ └─────────────────────┘ └──────────────────────┘
```

## Entities

### Symbol Export Entry

Represents a symbol to be re-exported from the main package.

| Field | Type | Description |
|-------|------|-------------|
| symbol-name | keyword | The symbol identifier (e.g., `#:lexical-env-parent`) |
| source-package | keyword | The internal package exporting it |
| error-pattern | string | The error pattern ID this fixes (e.g., "P114") |
| error-count | integer | Number of occurrences in stage1-report.json |

### Type Predicate Entry

Represents a new type predicate to be added.

| Field | Type | Description |
|-------|------|-------------|
| predicate-name | symbol | The predicate symbol (e.g., `PACKAGEP*`) |
| type-index | integer | WasmGC type index to test against |
| pattern | code | The compilation pattern (ref.test + i32→boolean) |

## Symbols to Re-export

| Symbol | Source Package | Error Pattern | Count |
|--------|---------------|---------------|-------|
| `lexical-env-parent` | `clysm/compiler/env` | P114 | 119 |
| `lexical-env-bindings` | `clysm/compiler/env` | P114 | - |
| `make-lexical-env` | `clysm/compiler/env` | P114 | - |
| `compile-to-instructions` | `clysm/compiler/codegen/func-section` | P944 | 36 |
| `make-wasm-struct-type` | `clysm/compiler/codegen/gc-types` | P321 | 17 |
| `wasm-struct-type-p` | `clysm/compiler/codegen/gc-types` | P321 | - |
| `wasm-struct-type-fields` | `clysm/compiler/codegen/gc-types` | P321 | - |
| `ast-literal-value` | `clysm/compiler/ast` | P543 | 11 |
| `ast-literal-p` | `clysm/compiler/ast` | P543 | - |

## Type Predicates to Add

| Predicate | Type Index | Error Pattern | Count |
|-----------|-----------|---------------|-------|
| `PACKAGEP*` | TBD ($package) | P951 | 25 |

## Validation Rules

1. All symbols in "Symbols to Re-export" table MUST be accessible via `clysm:symbol-name` after implementation
2. `PACKAGEP*` MUST return Lisp T for package objects and NIL for non-packages
3. Stage 1 regeneration MUST show zero occurrences of fixed error patterns
4. `wasm-tools validate` MUST pass on generated Wasm
