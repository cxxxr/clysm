# Data Model: Stage 1 Runtime Environment

**Date**: 2025-12-30
**Feature**: 001-stage1-runtime

## Entities

### WasmModule

Represents a WebAssembly module (Stage 1 input or Stage 2 output).

| Field | Type | Description |
|-------|------|-------------|
| path | string | Filesystem path to .wasm file |
| size | number | File size in bytes |
| exports | string[] | List of exported function names |
| imports | ImportSpec[] | List of required imports |

**Validation Rules**:
- Path must exist and be readable
- File must start with Wasm magic bytes: `0x00 0x61 0x73 0x6d`
- Version must be 1 (bytes 4-7)

**State Transitions**:
```
[Not Loaded] --load--> [Compiled] --instantiate--> [Running]
                                                       |
                                          --_start--> [Initialized]
```

### ImportSpec

Represents a WebAssembly import requirement.

| Field | Type | Description |
|-------|------|-------------|
| module | string | Import namespace (e.g., "clysm:io") |
| name | string | Function name (e.g., "write-char") |
| kind | "function" | Import kind |

### FileDescriptor

Runtime mapping of integer FDs to open file handles.

| Field | Type | Description |
|-------|------|-------------|
| id | number | Integer file descriptor |
| handle | object | Node.js fs.FileHandle |
| path | string | Original file path |
| direction | "input" \| "output" | Read or write mode |
| closed | boolean | Whether handle is closed |

**Reserved Descriptors**:
- 0: stdin
- 1: stdout
- 2: stderr
- 3+: User-opened files

### CompilationReport

JSON output documenting compilation results.

| Field | Type | Description |
|-------|------|-------------|
| timestamp | string | ISO 8601 timestamp |
| stage_version | string | Source stage identifier |
| summary | Summary | Aggregated statistics |
| modules | ModuleResult[] | Per-module results |

#### Summary (nested)

| Field | Type | Description |
|-------|------|-------------|
| total_forms | number | Total Lisp forms processed |
| compiled | number | Successfully compiled forms |
| failed | number | Forms that failed compilation |
| skipped | number | Forms intentionally skipped |
| coverage_pct | number | Compilation success rate (0-100) |

#### ModuleResult (nested)

| Field | Type | Description |
|-------|------|-------------|
| path | string | Source file path |
| compiled | number | Forms compiled in this module |
| failed | number | Forms failed in this module |
| total | number | Total forms in this module |

### ExitCode

Standardized exit codes for CLI operations.

| Code | Constant | Description |
|------|----------|-------------|
| 0 | EXIT_SUCCESS | Operation completed successfully |
| 1 | EXIT_PARTIAL | Partial success (e.g., some forms failed) |
| 2 | EXIT_FAILURE | Operation failed |
| 3 | EXIT_MISSING_DEP | Required dependency not found |
| 77 | EXIT_SKIP | Known limitation (operation skipped) |

## Relationships

```
┌─────────────────┐
│   WasmModule    │
│  (Stage 1)      │
└────────┬────────┘
         │ requires
         ▼
┌─────────────────┐       ┌─────────────────┐
│   ImportSpec[]  │◄──────│   Host Shim     │
│                 │provides│  (io/fs shims)  │
└─────────────────┘       └────────┬────────┘
                                   │ uses
                                   ▼
                          ┌─────────────────┐
                          │ FileDescriptor  │
                          │    Table        │
                          └─────────────────┘

┌─────────────────┐       ┌─────────────────┐
│   Stage 1       │ ──────│ CompilationReport│
│   Execution     │produces│    (JSON)       │
└────────┬────────┘       └─────────────────┘
         │ generates
         ▼
┌─────────────────┐
│   WasmModule    │
│  (Stage 2)      │
└─────────────────┘
```

## JSON Schema: CompilationReport

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["timestamp", "stage_version", "summary"],
  "properties": {
    "timestamp": {
      "type": "string",
      "format": "date-time"
    },
    "stage_version": {
      "type": "string",
      "pattern": "^stage[0-9]+-v[0-9]+\\.[0-9]+$"
    },
    "summary": {
      "type": "object",
      "required": ["total_forms", "compiled", "failed", "coverage_pct"],
      "properties": {
        "total_forms": { "type": "integer", "minimum": 0 },
        "compiled": { "type": "integer", "minimum": 0 },
        "failed": { "type": "integer", "minimum": 0 },
        "skipped": { "type": "integer", "minimum": 0 },
        "coverage_pct": { "type": "number", "minimum": 0, "maximum": 100 }
      }
    },
    "modules": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["path", "compiled", "failed", "total"],
        "properties": {
          "path": { "type": "string" },
          "compiled": { "type": "integer", "minimum": 0 },
          "failed": { "type": "integer", "minimum": 0 },
          "total": { "type": "integer", "minimum": 0 }
        }
      }
    }
  }
}
```
