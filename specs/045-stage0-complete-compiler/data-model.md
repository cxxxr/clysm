# Data Model: Stage 0 Complete Compiler

**Date**: 2025-12-28
**Feature**: 045-stage0-complete-compiler

## Core Entities

### Stage Binary

Represents a Wasm binary at a particular bootstrap stage.

| Field | Type | Description |
|-------|------|-------------|
| path | pathname | File path (e.g., dist/clysm-stage0.wasm) |
| stage | integer (0, 1, 2) | Bootstrap stage number |
| size | integer | Binary size in bytes |
| valid | boolean | Passes wasm-tools validation |
| exports | list of string | Exported function names |
| created-at | timestamp | Generation timestamp |

**Validation Rules**:
- Size must be > 8 bytes for non-placeholder
- Must contain valid Wasm magic bytes (0x00, 0x61, 0x73, 0x6d)
- Version must be 1 (bytes 4-7: 0x01, 0x00, 0x00, 0x00)
- Stage 0/1 must export `compile_form` and `compile_all`

**State Transitions**:
```
PLACEHOLDER → VALID → VERIFIED
     ↓           ↓
   INVALID    FAILED
```

### Compilation Result

Result of compiling a single source module.

| Field | Type | Description |
|-------|------|-------------|
| module-path | pathname | Source file path |
| success | boolean | Compilation succeeded |
| wasm-bytes | byte-vector | Compiled Wasm output |
| forms-total | integer | Total forms in module |
| forms-compiled | integer | Successfully compiled forms |
| forms-failed | integer | Failed forms |
| error-message | string or nil | Error if failed |
| unsupported-feature | symbol or nil | Blocking CL feature |

**Validation Rules**:
- wasm-bytes must pass wasm-tools validation
- forms-compiled + forms-failed = forms-total

### Module Info

Metadata about a source module in compilation order.

| Field | Type | Description |
|-------|------|-------------|
| path | pathname | Source file path |
| directory | pathname | Containing directory |
| index | integer | Position in compilation order |
| dependencies | list of pathname | Modules this depends on |
| symbols-defined | list of symbol | Symbols defined by this module |
| symbols-used | list of symbol | External symbols used |

**Validation Rules**:
- Path must exist and be readable
- Index must be unique in [0, 44]

### Verification Result

Result of fixed-point verification (Stage 1 == Stage 2).

| Field | Type | Description |
|-------|------|-------------|
| status | symbol | :achieved, :not-achieved, :compilation-error, :missing-dependency |
| timestamp | string (ISO-8601) | When verification ran |
| stage1-info | stage-binary | Stage 1 binary info |
| stage2-info | stage-binary | Stage 2 binary info |
| identical-p | boolean | Binaries are byte-identical |
| first-diff-offset | integer or nil | First differing byte offset |
| diff-byte-count | integer | Total differing bytes |
| elapsed-ms | integer | Verification time in ms |

**State Transitions**:
```
PENDING → RUNNING → ACHIEVED
              ↓
       NOT_ACHIEVED
              ↓
       COMPILATION_ERROR
              ↓
       MISSING_DEPENDENCY
```

### FFI Import

Host function imported by Stage 0.

| Field | Type | Description |
|-------|------|-------------|
| namespace | string | FFI namespace (e.g., "clysm.fs") |
| name | string | Function name (e.g., "read-all") |
| params | list of wasm-type | Parameter types |
| result | wasm-type | Return type |
| host-impl | string | Host shim function name |

**Wasm Types**: i32, i64, f32, f64, externref, anyref

### FFI Export

Function exported by Stage 0 for host invocation.

| Field | Type | Description |
|-------|------|-------------|
| name | string | Export name (e.g., "compile_form") |
| params | list of wasm-type | Parameter types |
| result | wasm-type | Return type |
| func-index | integer | Function table index |

## Relationships

```
                    ┌───────────────┐
                    │  Stage Binary │
                    │   (Stage 0)   │
                    └───────┬───────┘
                            │ generates
                            ▼
                    ┌───────────────┐
                    │  Stage Binary │
                    │   (Stage 1)   │
                    └───────┬───────┘
                            │ generates
                            ▼
                    ┌───────────────┐
                    │  Stage Binary │
                    │   (Stage 2)   │
                    └───────────────┘
                            │
                            ▼
                    ┌───────────────────┐
                    │ Verification      │
                    │ Result            │
                    │ (Stage1 == Stage2)│
                    └───────────────────┘

┌─────────────────────────────────────────────────────┐
│                     Stage 0 Binary                  │
│                                                     │
│  ┌────────────┐    ┌───────────────┐               │
│  │ FFI Import │◄───│  Host Shim    │               │
│  │ (clysm.fs) │    │ (stage1-host) │               │
│  └────────────┘    └───────────────┘               │
│                                                     │
│  ┌────────────┐                                    │
│  │ FFI Export │ compile_form, compile_all          │
│  └────────────┘                                    │
│                                                     │
│  ┌────────────────────────────────────────────┐    │
│  │              Compiled Modules              │    │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐   │    │
│  │  │ Module 1 │ │ Module 2 │ │   ...    │   │    │
│  │  │ leb128   │ │ sections │ │  (×45)   │   │    │
│  │  └──────────┘ └──────────┘ └──────────┘   │    │
│  └────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────┘
```

## WasmGC Type Definitions

Stage 0 uses these WasmGC types (from gc-types.lisp):

| Index | Name | Structure |
|-------|------|-----------|
| 0 | $nil | `(struct)` - Empty singleton |
| 1 | $cons | `(struct (field $car (mut anyref)) (field $cdr (mut anyref)))` |
| 2 | $symbol | `(struct (field $name (ref $string)) (field $value (mut anyref)) (field $plist (mut anyref)))` |
| 3 | $string | `(array (mut i8))` - UTF-8 bytes |
| 4 | $vector | `(array (mut anyref))` - General vector |
| 5 | $closure | `(struct (field $code_0 (ref null $func_0)) (field $code_1 (ref null $func_1)) ... (field $env (mut anyref)))` |
| 6 | $instance | `(struct (field $class (ref $standard-class)) (field $slots (ref $slot-vector)))` |
| 7 | $standard-class | `(struct (field $name (ref $symbol)) (field $superclass (ref null $standard-class)) ...)` |
| 8 | $float | `(struct (field $value f64))` |
| 9 | $ratio | `(struct (field $numerator i64) (field $denominator i64))` |
| 10-21 | (internal) | Function types, arrays, etc. |
| 22 | $mv_array | `(array (mut anyref))` - Multiple values buffer |
| 23 | $keyword-array | `(array (ref $symbol))` - Initarg keywords |
| 24 | $macro-environment | `(struct (field $local-macros anyref) (field $parent (ref null $macro-environment)))` |

## Global Variables

| Index | Name | Type | Description |
|-------|------|------|-------------|
| 0 | NIL | `(ref $nil)` | The NIL singleton |
| 1 | UNBOUND | `(ref $nil)` | Unbound marker |
| 2 | mv-count | `i32` | Current multiple values count |
| 3 | mv-buffer | `(ref $mv_array)` | Multiple values buffer |

## File Locations

| Entity | Location |
|--------|----------|
| Stage 0 Binary | `dist/clysm-stage0.wasm` |
| Stage 1 Binary | `dist/clysm-stage1.wasm` |
| Stage 2 Binary | `dist/clysm-stage2.wasm` |
| Stage 1 Report | `dist/stage1-report.json` |
| Verification History | `dist/verification-history.jsonl` |
| Source Modules | `src/clysm/**/*.lisp` (45 files) |
| Compilation Order | `src/clysm/validation/compiler-order.lisp` |
| Host Shim | `host-shim/stage1-host.js` |
