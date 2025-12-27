# Research: Fixed-Point Verification (Phase 13B)

**Date**: 2025-12-27
**Feature**: 040-fixed-point-verification
**Status**: Complete

## Overview

This document captures technical decisions and research findings for implementing fixed-point verification of the Clysm compiler.

---

## R1: Byte-Level Binary Comparison Strategy

**Decision**: Use direct file I/O with `(unsigned-byte 8)` element type for byte-by-byte comparison.

**Rationale**:
- Common Lisp provides efficient binary file reading via `with-open-file`
- No external dependencies required for comparison
- Existing `diff.lisp` already uses binary file access for size extraction
- Simple, deterministic, and easily testable

**Alternatives Considered**:
| Alternative | Rejected Because |
|-------------|------------------|
| `cmp` shell command | Less portable, harder to get byte offset of first difference |
| Memory-mapped comparison | Unnecessary complexity for 1-5MB files |
| Hash comparison (SHA256) | Doesn't provide diff location information |

**Implementation Pattern**:
```lisp
(defun binaries-identical-p (path1 path2)
  "Compare two binary files byte-by-byte.
   Returns (values identical-p first-diff-offset)."
  (with-open-file (s1 path1 :element-type '(unsigned-byte 8))
    (with-open-file (s2 path2 :element-type '(unsigned-byte 8))
      (when (/= (file-length s1) (file-length s2))
        (return-from binaries-identical-p
          (values nil :size-mismatch)))
      (loop for byte1 = (read-byte s1 nil :eof)
            for byte2 = (read-byte s2 nil :eof)
            for offset from 0
            do (cond
                 ((eq byte1 :eof) (return (values t nil)))
                 ((/= byte1 byte2) (return (values nil offset))))))))
```

---

## R2: Stage 1 Execution via wasmtime

**Decision**: Execute Stage 1 on wasmtime via Node.js host shim with FFI support.

**Rationale**:
- Stage 1 requires FFI for file I/O (reading Clysm source modules)
- Existing `host-shim/stage1-host.js` provides FFI implementation
- wasmtime supports WasmGC and exception handling proposals
- Consistent with 039's approach

**Alternatives Considered**:
| Alternative | Rejected Because |
|-------------|------------------|
| Direct wasmtime CLI | No FFI support for file reading |
| Browser-based execution | Complex setup, unnecessary for CLI workflow |
| wasmer | Less mature WasmGC support |

**Execution Pattern**:
```bash
# Stage 1 execution command
node host-shim/stage1-host.js \
  --wasm dist/clysm-stage1.wasm \
  --compile-sources src/clysm/ \
  --output dist/clysm-stage2.wasm
```

---

## R3: Integration with Existing 039 Infrastructure

**Decision**: Extend existing modules rather than rewrite.

**Rationale**:
- `diff.lisp` already has binary info extraction, export comparison
- `runner.lisp` already has wasmtime availability checks
- `generator.lisp` already has Stage 1 generation logic
- `types.lisp` already has `binary-info`, `diff-report` structs

**Files to Extend**:

| File | Extensions Needed |
|------|-------------------|
| `types.lisp` | Add `verification-result`, `fixpoint-status` structs |
| `diff.lisp` | Add `binaries-identical-p`, `byte-diff-report` |
| `runner.lisp` | Implement `run-form` to actually invoke wasmtime |
| `generator.lisp` | Add Stage 2 generation entry point |

**New Files**:

| File | Purpose |
|------|---------|
| `fixpoint.lisp` | Main verification orchestration |
| `build/stage2-gen.lisp` | Stage 2 generation CLI |
| `scripts/verify-fixpoint.sh` | CI/developer verification script |

---

## R4: Exit Code Convention

**Decision**: Use Unix-style exit codes for CI integration.

**Rationale**:
- FR-007 specifies: 0 (achieved), 1 (not achieved), 2 (compilation error), 3 (missing dependencies)
- Matches standard Unix conventions
- Enables simple CI integration (`$? == 0` for success)

**Exit Code Map**:
| Code | Meaning | JSON `status` |
|------|---------|---------------|
| 0 | Fixed-point achieved | `"ACHIEVED"` |
| 1 | Fixed-point not achieved | `"NOT_ACHIEVED"` |
| 2 | Stage 2 compilation failed | `"COMPILATION_ERROR"` |
| 3 | Missing wasmtime/wasm-tools/Stage 1 | `"MISSING_DEPENDENCY"` |

---

## R5: JSON Output Format

**Decision**: Consistent JSON structure for all verification results.

**Rationale**:
- FR-006 requires machine-readable output
- Extends existing `diff.lisp` JSON format
- Enables CI parsing and reporting

**JSON Schema**:
```json
{
  "status": "ACHIEVED|NOT_ACHIEVED|COMPILATION_ERROR|MISSING_DEPENDENCY",
  "timestamp": "2025-12-27T12:00:00Z",
  "stage1": {
    "path": "dist/clysm-stage1.wasm",
    "size_bytes": 1584,
    "valid": true
  },
  "stage2": {
    "path": "dist/clysm-stage2.wasm",
    "size_bytes": 1584,
    "valid": true,
    "compilation_rate": 0.196
  },
  "comparison": {
    "identical": true,
    "first_diff_offset": null,
    "diff_bytes": 0
  },
  "timing": {
    "stage2_generation_ms": 120000,
    "comparison_ms": 50
  }
}
```

---

## R6: Host Shim Extensions

**Decision**: Extend `stage1-host.js` to support compilation mode.

**Rationale**:
- Current shim supports individual expression execution
- Need batch compilation of source files
- Need Wasm binary output generation

**Required Shim Functions**:
| Function | Purpose |
|----------|---------|
| `fs.readDir(path)` | List source modules |
| `fs.readFile(path)` | Read source files |
| `fs.writeFile(path, bytes)` | Write Stage 2 binary |
| `compiler.compileModule(source)` | Compile single module |
| `compiler.linkModules(compiled)` | Link into single binary |

---

## R7: Progress Tracking (P3)

**Decision**: Simple JSON-based history log.

**Rationale**:
- User Story 5 is P3 priority
- Minimal implementation: append-only JSON Lines file
- Simple to query with grep/jq

**History File Format** (`dist/verification-history.jsonl`):
```jsonl
{"timestamp":"2025-12-27T12:00:00Z","status":"NOT_ACHIEVED","diff_bytes":1234,"compilation_rate":0.196}
{"timestamp":"2025-12-28T10:00:00Z","status":"NOT_ACHIEVED","diff_bytes":500,"compilation_rate":0.350}
```

---

## Summary

| Decision | Key Choice |
|----------|------------|
| R1: Binary comparison | Direct byte-by-byte in Common Lisp |
| R2: Stage 1 execution | wasmtime via Node.js host shim |
| R3: Integration | Extend existing 039 modules |
| R4: Exit codes | Unix-style (0/1/2/3) |
| R5: JSON format | Structured with status, timing, comparison |
| R6: Host shim | Extend for batch compilation |
| R7: Progress tracking | JSON Lines append-only log |

All NEEDS CLARIFICATION items: **None** (Technical Context was complete)
