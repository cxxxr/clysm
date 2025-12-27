# Research: Stage 1 Compiler Generation

**Feature**: 039-stage1-compiler-gen
**Date**: 2025-12-27
**Status**: Complete

## Research Summary

This document captures research findings and design decisions for implementing Stage 1 compiler generation infrastructure.

## R1: Stage 0 Current State and Capabilities

### Decision
Use existing Stage 0 binary (dist/clysm-stage0.wasm) as-is for Stage 1 generation attempts, accepting the 19.6% compilation rate limitation.

### Rationale
- Phase 038 achieved 168/849 forms compiled (19.6%)
- Stage 0 binary validates with wasm-tools
- Known limitation: combined form compilation fails with "Unknown instruction" error
- Individual form compilation works correctly
- Further improvement requires extending Clysm's CL subset (loop, &optional defaults, etc.)

### Alternatives Considered
- **Wait for full CL support**: Rejected - would delay indefinitely; partial Stage 1 still validates self-hosting path
- **Rewrite Clysm in blessed subset**: Rejected - high effort; better to extend compiler incrementally

### Current Blockers (from Phase 038)
1. `loop` macro - not supported
2. `&optional` with default expressions - partially supported
3. `make-hash-table` and library functions - not implemented
4. `format` with complex directives - partial support
5. Combined form compilation - internal issue with unknown instructions

---

## R2: wasmtime Execution Strategy

### Decision
Use Node.js as the host environment for wasmtime execution, leveraging existing host-shim/ infrastructure.

### Rationale
- Existing host-shim/verify-stage0.js provides template for Wasm module loading
- Node.js WebAssembly API supports WasmGC types
- FFI patterns already established (host.read_string, host.write_bytes, host.log, host.error)
- fs-shim.js provides filesystem access via FFI

### Alternatives Considered
- **wasmtime CLI directly**: Rejected - lacks WasmGC support in CLI mode; requires programmatic API
- **Deno runtime**: Rejected - less tested with existing shims; Node.js already proven

### Implementation Pattern
```javascript
// host-shim/stage1-host.js
const imports = {
  clysm: {
    fs: { ... },      // File system access
    compile: { ... }, // Compilation callbacks
    report: { ... }   // Progress reporting
  }
};
const instance = await WebAssembly.instantiate(module, imports);
instance.exports.compile_file(sourcePtr);
```

---

## R3: Source File Reading Protocol

### Decision
Use existing FFI filesystem infrastructure (Feature 035) with staged reading: (1) read file as string, (2) parse on Wasm side.

### Rationale
- fs-shim.js already implements fs.open, fs.read-all, fs.write-all
- Clysm reader exists in Stage 0 (modules 5-8 in compilation order)
- Keep parsing in Wasm for consistency with self-hosting goal

### Alternatives Considered
- **Parse on host, send AST**: Rejected - defeats purpose of self-hosting; need Wasm-side reader
- **Memory-mapped files**: Rejected - WasmGC doesn't use linear memory; externref strings work

### FFI Interface
```javascript
// Extended from fs-shim.js
clysm.fs = {
  'read-source': (pathRef) => string_ref,  // Returns externref string
  'list-modules': () => array_of_paths,     // Returns compilation order
};
```

---

## R4: Compilation Progress Tracking

### Decision
Implement progress tracking in host-shim JavaScript, collecting stats from Wasm callbacks.

### Rationale
- Host has reliable I/O for logging and file output
- Wasm can report per-form results via FFI callbacks
- JSON format enables downstream tooling and historical comparison

### Progress Report Format
```json
{
  "timestamp": "2025-12-27T12:00:00Z",
  "stage0_version": "039",
  "modules": [
    {
      "path": "src/clysm/backend/leb128.lisp",
      "total_forms": 25,
      "compiled": 20,
      "failed": 5,
      "failures": [
        {"operator": "loop", "count": 3, "example": "(loop for x ...)"},
        {"operator": "declare", "count": 2, "example": "(declare (type ...))"}
      ]
    }
  ],
  "summary": {
    "total_forms": 849,
    "compiled": 212,
    "coverage_pct": 24.97,
    "top_blockers": [...]
  }
}
```

---

## R5: Stage 1 Binary Generation Strategy

### Decision
Generate Stage 1 by concatenating compiled Wasm bytes from each successful form, producing a single module.

### Rationale
- Same approach as Stage 0 bootstrap (build/bootstrap.lisp)
- Single-module output simplifies validation and distribution
- Partial binary still validates, enabling incremental progress

### Alternatives Considered
- **Multi-module linking**: Rejected - WasmGC module linking complex; single module simpler
- **Skip on failure**: Accept - compile what we can, report what we can't

### Output Path
```text
dist/clysm-stage1.wasm    # Primary output
dist/stage1-report.json   # Progress report
```

---

## R6: Blocker Analysis and Categorization

### Decision
Extend Phase 038's operator-grouped failure tracking with impact estimation based on form frequency.

### Rationale
- Phase 038 already groups failures by operator (record-failure, generate-failure-report)
- Impact = (forms using operator / total forms) * 100
- Enables prioritized feature development roadmap

### Blocker Report Format
```json
{
  "blockers": [
    {
      "operator": "loop",
      "affected_forms": 45,
      "impact_pct": 5.3,
      "priority": "HIGH",
      "recommendation": "Implement loop macro expansion",
      "examples": ["(loop for x from 1 to 10 collect x)", ...]
    }
  ],
  "future_work": [
    {"feature": "loop-macro", "impact": 45, "estimated_complexity": "HIGH"},
    {"feature": "optional-defaults", "impact": 23, "estimated_complexity": "MEDIUM"}
  ]
}
```

---

## R7: Binary Diff Analysis

### Decision
Implement diff analysis using wasm-tools objdump output comparison.

### Rationale
- wasm-tools already validates binaries
- `wasm-tools objdump --details` provides section-level information
- Text diff of objdump output reveals structural differences

### Diff Report Format
```json
{
  "stage0": {
    "size_bytes": 1584,
    "exports": ["compile", "eval", ...],
    "types": 23,
    "functions": 168
  },
  "stage1": {
    "size_bytes": 2100,
    "exports": ["compile", ...],
    "types": 25,
    "functions": 212
  },
  "differences": {
    "size_delta": "+516 bytes",
    "missing_exports": [],
    "new_exports": [],
    "type_changes": [...]
  }
}
```

---

## R8: Error Handling Strategy

### Decision
Graceful degradation: log errors, continue compilation, aggregate failures in report.

### Rationale
- Stopping on first error prevents understanding full scope of issues
- All errors captured with context (file, form, operator, message)
- Final report shows complete picture for prioritization

### Error Categories
1. **Parse Error**: Malformed Lisp syntax (should not occur in valid source)
2. **Unsupported Feature**: Known CL construct not in Clysm subset
3. **Unknown Instruction**: Internal compiler issue (current blocker)
4. **Wasm Validation Failure**: Generated bytes don't validate
5. **Runtime Error**: Unexpected exception during compilation

---

## Technology Decisions Summary

| Component | Technology | Rationale |
|-----------|------------|-----------|
| Wasm Runtime | Node.js + WebAssembly API | Existing shims, WasmGC support |
| Progress Tracking | JSON files | Portable, tooling-friendly |
| Validation | wasm-tools | Standard toolchain |
| Diff Analysis | wasm-tools objdump + text diff | Available, reliable |
| Test Framework | Rove + shell scripts | Consistent with project |

---

## Outstanding Questions (Deferred)

1. **Memory limits for large compilations**: Monitor during implementation; wasmtime defaults likely sufficient
2. **Incremental compilation checkpoints**: Nice-to-have; implement basic flow first
3. **Cross-platform testing**: Focus on Linux first; wasmtime CLI differences minimal
