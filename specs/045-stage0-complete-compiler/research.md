# Research: Stage 0 Complete Compiler

**Date**: 2025-12-28
**Feature**: 045-stage0-complete-compiler

## Current State Analysis

### Stage 0 Binary Status

**Current state**: 8 bytes (minimal Wasm magic + version header only)

```
dist/clysm-stage0.wasm: 8 bytes
00000000: 0061 736d 0100 0000  .asm....
```

**Why it's 8 bytes**: Previous bootstrap attempts (Features 037, 038, 044) generated placeholder binaries because:
1. The bootstrap script encountered CL features not yet supported by Clysm
2. Individual forms compiled, but combined module compilation failed
3. ~23% compilation rate was achieved - internal compiler functions cannot be primitives

**Required exports missing**:
- `compile_form` - Compile single S-expression to Wasm bytes
- `compile_all` - Compile all source modules to complete binary

### Compilation Rate Analysis (Feature 043)

From `dist/final-rate.json`:
- 219/936 forms compiled (23.40%)
- Blockers: Internal compiler functions (ENV-ADD-LOCAL, COMPILE-TO-INSTRUCTIONS, EMIT-*)
- These are internal implementation details, not primitives

**Key insight**: The ~23% ceiling is structural - internal compiler functions have no meaning as standalone Wasm primitives.

## Bootstrap Strategy Research

### Decision: Two-Path Bootstrap

**Chosen approach**: Support both SBCL-compiled and interpreter-generated Stage 0.

**Rationale**:
1. SBCL path provides faster compilation and more complete CL support
2. Interpreter path enables SBCL-free development (Feature 044)
3. Both paths converge on same Stage 0 binary format

**Alternatives considered**:
- Single SBCL-only path: Rejected - limits development flexibility
- Single interpreter-only path: Rejected - slower, less CL coverage

### Decision: Compile Whole Compiler as Single Module

**Chosen approach**: Compile entire Clysm compiler as one Wasm module with exported functions.

**Rationale**:
1. Wasm modules are the unit of compilation and instantiation
2. Single module simplifies FFI and memory sharing
3. `compile_form` and `compile_all` as named exports

**Alternatives considered**:
- Multiple linked modules: Rejected - Wasm module linking is complex and poorly supported
- Interpreter-in-Wasm: Rejected - Already have Tier 1 interpreter, need Tier 2 compiled code

## FFI Design Research

### Decision: Minimal FFI Surface

**Required imports** (clysm.fs namespace):
- `fs.open` - Open file, return handle
- `fs.read-all` - Read entire file as string
- `fs.write-all` - Write string to file
- `fs.close` - Close file handle

**Required exports**:
- `compile_form(expr: externref) -> externref` - Compile S-expression string, return Wasm bytes
- `compile_all() -> i32` - Compile all modules, write to output, return exit code
- `_initialize()` - Runtime initialization (called automatically on instantiation)

**Rationale**:
1. Minimizes attack surface (Wasm sandbox maintained)
2. All file I/O goes through host shim
3. Matches existing host-shim/stage1-host.js interface

### Decision: CLI via wasmtime Arguments

**Chosen approach**: Use wasmtime's `--invoke` and WASI args for CLI.

```bash
# Compile single expression
wasmtime run dist/clysm-stage0.wasm --invoke compile_form '(+ 1 2)'

# Compile all to Stage 1
wasmtime run --dir=. dist/clysm-stage0.wasm
```

**Rationale**:
1. Leverages wasmtime's built-in CLI capabilities
2. No custom argument parsing needed in Wasm
3. WASI provides standard I/O

## Runtime Initialization Research

### Decision: Start Function for Initialization

**Chosen approach**: Use Wasm start function + explicit _initialize export.

**Initialization steps**:
1. Allocate WasmGC types (24+ type definitions)
2. Initialize global variables (NIL, UNBOUND, mv-count, mv-buffer)
3. Set up symbol table for interned symbols
4. Register FFI imports

**Rationale**:
1. Start function runs automatically on instantiation
2. _initialize export allows explicit re-initialization if needed
3. Follows WASI conventions

### Type Section Requirements

From existing gc-types.lisp, Stage 0 needs these WasmGC types:

| Index | Name | Description |
|-------|------|-------------|
| 0 | $nil | NIL singleton |
| 1 | $cons | Cons cell (car, cdr) |
| 2 | $symbol | Symbol with name, value, plist |
| 3 | $string | UTF-8 string array |
| 4 | $vector | General vector array |
| 5 | $closure | Closure with code refs + env |
| 6 | $instance | CLOS instance |
| 7 | $standard-class | Class metaobject |
| ... | ... | ... |
| 22 | $mv_array | Multiple values buffer |
| 23 | $ffi-import | FFI function info |
| 24 | $macro-environment | Macro environment |

## Fixed-Point Verification Research

### Decision: Byte-by-Byte Comparison

**Chosen approach**: Compare Stage 1 and Stage 2 binaries byte-by-byte.

**Why this works**:
1. Deterministic compilation: same input → same output
2. No timestamps or random elements in Wasm binary
3. Binary identity proves functional equivalence

**Verification script**: `scripts/verify-fixpoint.sh`

Exit codes:
- 0: ACHIEVED (Stage 1 == Stage 2)
- 1: NOT_ACHIEVED (binaries differ)
- 2: COMPILATION_ERROR
- 3: MISSING_DEPENDENCY

### Decision: Track First Difference Offset

**Chosen approach**: When binaries differ, report first difference offset.

**Rationale**: Aids debugging by identifying which section/function differs first.

## Implementation Strategy Research

### Decision: Incremental Build Approach

**Chosen approach**: Build Stage 0 incrementally with verification at each step.

**Phases**:
1. **P1: Minimal Stage 0** - Wasm header + type section + minimal runtime
2. **P2: Reader** - S-expression parsing in Wasm
3. **P3: Compiler Core** - AST → Wasm IR translation
4. **P4: Codegen** - Wasm binary emission
5. **P5: Entry Points** - compile_form, compile_all exports
6. **P6: Verification** - Fixed-point testing

**Rationale**:
1. Each phase is testable independently
2. Failures are isolated to specific components
3. Matches existing module structure

### Decision: Reuse Existing Compiler Logic

**Chosen approach**: Cross-compile existing SBCL compiler code to Wasm.

**What to reuse**:
- `src/clysm/compiler/ast.lisp` - AST definitions
- `src/clysm/compiler/codegen/func-section.lisp` - Function compilation
- `src/clysm/backend/wasm-emit.lisp` - Binary emission
- `src/clysm/reader/reader.lisp` - S-expression reading

**What's new** (src/clysm/stage0/):
- `entry.lisp` - Wasm exports and CLI handling
- `runtime.lisp` - Wasm-native runtime initialization

**Rationale**: Avoids reimplementing compiler logic; cross-compilation is the goal.

## Risk Analysis

### High Risk: Blessed Subset Insufficiency

**Risk**: Current blessed subset may not cover all compiler source.

**Mitigation**:
1. Analyze remaining unsupported forms
2. Prioritize adding support for blocking features
3. Accept graceful degradation with logging

### Medium Risk: Memory Limits

**Risk**: wasmtime memory may be exhausted during compilation.

**Mitigation**:
1. Compile modules in batches
2. Release intermediate data structures
3. Monitor memory usage in progress reports

### Low Risk: Non-deterministic Output

**Risk**: Compilation may produce different binaries on each run.

**Mitigation**:
1. Ensure no timestamps in output
2. Use stable ordering for all collections
3. Seed any random generators deterministically

## Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Bootstrap Strategy | Two-path (SBCL + interpreter) | Flexibility without sacrificing completeness |
| Module Structure | Single complete module | Simplifies instantiation and linking |
| FFI Surface | Minimal (4 imports, 3 exports) | Security and simplicity |
| CLI Interface | wasmtime --invoke | Leverages existing runtime |
| Runtime Init | Start function + _initialize | WASI convention |
| Verification | Byte-by-byte comparison | Proves functional equivalence |
| Build Approach | Incremental phases | Testable, debuggable |
| Code Reuse | Cross-compile existing | Avoids reimplementation |
