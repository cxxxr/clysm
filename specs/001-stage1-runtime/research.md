# Research: Stage 1 Runtime Environment

**Date**: 2025-12-30
**Feature**: 001-stage1-runtime

## Research Questions

### Q1: What FFI functions does Stage 1 require?

**Decision**: Use existing `io-shim.js` and `fs-shim.js` implementations.

**Rationale**: Analysis of `dist/clysm-stage1.wasm` via `wasm-tools print` shows imports match existing shims exactly:

```
(import "clysm:io" "write-char" (func ...))
(import "clysm:io" "write-string" (func ...))
(import "clysm:io" "read-char" (func ...))
(import "clysm:io" "read-line" (func ...))
(import "clysm:fs" "open" (func ...))
(import "clysm:fs" "close" (func ...))
(import "clysm:fs" "read-all" (func ...))
(import "clysm:fs" "write-all" (func ...))
```

**Alternatives Considered**:
- Creating new FFI implementations: Rejected - existing shims already provide all needed functions
- Using WASI directly: Rejected - Stage 1 uses custom clysm:* namespaces, not WASI

### Q2: How should the runner script handle missing exports?

**Decision**: Check for exports and report with appropriate exit codes following `stage1-host.js` conventions.

**Rationale**: `host-shim/stage1-host.js` establishes exit code semantics:
- 0: Success
- 1: Partial success (e.g., placeholder generated)
- 2: Failure
- 3: Missing dependency
- 77: Known limitation (skip)

Stage 1 only exports `_start`, so if `compile_form` is requested but not available, exit with code 77 and clear message.

**Alternatives Considered**:
- Throwing errors: Rejected - unhelpful for automation
- Generic exit code 1: Rejected - loses information for scripts

### Q3: What Node.js version is required for WasmGC?

**Decision**: Node.js 20+ (LTS with WasmGC enabled by default)

**Rationale**:
- Node.js 20.0+ includes V8 11.2+ with WasmGC support
- `--experimental-wasm-gc` flag is no longer needed in Node.js 20+
- Project flake.nix already specifies Node.js 20+

**Alternatives Considered**:
- Node.js 18: Rejected - requires experimental flags, WasmGC support incomplete
- Node.js 22: Not necessary - Node.js 20 LTS is sufficient

### Q4: How should stage1-runner.js be structured?

**Decision**: Follow `run-wasm.js` pattern with Stage 1-specific enhancements.

**Rationale**: `run-wasm.js` provides proven pattern:
1. Parse command line arguments
2. Load Wasm buffer from file
3. Merge all FFI imports
4. Instantiate module
5. Call `_start` export

Stage 1 runner adds:
- Export discovery (`_start`, `compile_form`, `compile_all`)
- Progress reporting (if Stage 1 supports it)
- Exit code mapping

**Alternatives Considered**:
- Extending stage1-host.js: Rejected - that file is for Stage 0→1 generation, not Stage 1 execution
- Creating monolithic script: Rejected - loses modularity

### Q5: How should compile_form be invoked if available?

**Decision**: Accept Lisp expression via `--expr` argument, convert to externref, call export, validate result.

**Rationale**:
- Stage 1 may export `compile_form(anyref) -> anyref`
- Input is S-expression string
- Output should be Uint8Array of Wasm bytes
- Validation with `wasm-tools validate` confirms correctness

**Alternatives Considered**:
- REPL mode: Deferred - complexity not needed for verification
- File-based input: Could be added later, but inline expression simpler for testing

### Q6: What report format should Stage 2 generation use?

**Decision**: Follow `dist/stage1-report.json` schema from existing infrastructure.

**Rationale**: Existing schema from `stage1-host.js`:
```json
{
  "timestamp": "ISO8601",
  "stage0_version": "string",
  "summary": {
    "total_forms": number,
    "compiled": number,
    "failed": number,
    "skipped": number,
    "coverage_pct": number
  },
  "modules": [...]
}
```

Stage 2 report uses same format with `stage1_version` field.

**Alternatives Considered**:
- New schema: Rejected - consistency aids tooling
- Plain text: Rejected - not machine-parseable

## Technical Findings

### Stage 1 Wasm Analysis

From `wasm-tools print dist/clysm-stage1.wasm`:
- Size: 24,554 bytes (24.5KB)
- Exports: `_start` only
- Imports: 8 functions across `clysm:io` and `clysm:fs` namespaces
- Type count: 100+ (WasmGC struct/array types)
- Function count: 100+ (compiled Lisp forms)

### Existing Infrastructure Reuse

| Component | Status | Action |
|-----------|--------|--------|
| io-shim.js | ✓ Complete | Import as-is |
| fs-shim.js | ✓ Complete | Import as-is |
| math-shim.js | ✓ Complete | Import if needed |
| run-wasm.js | ✓ Complete | Use as pattern |
| stage1-host.js | ✓ Complete | Reuse exit codes |

### Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Stage 1 crashes on _start | Medium | High | Trap handler with stack trace |
| compile_form not exported | High | Medium | Exit 77 with clear message |
| FFI type mismatch | Low | High | Validate at instantiation |
| Large file I/O performance | Low | Low | Not in scope (basic operations only) |
