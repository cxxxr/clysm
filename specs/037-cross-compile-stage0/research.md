# Research: Cross-Compile Stage 0

**Phase 0 Output** | **Date**: 2025-12-27

## R1: Module Compilation Strategy

**Question**: How should 41 source modules be compiled into a single Wasm binary?

**Decision**: Source-level concatenation with unified compilation

**Rationale**:
1. The current `compile-to-wasm` function compiles single expressions to standalone Wasm modules
2. Each standalone module has its own type section, function indices, and global indices
3. Wasm does not support module linking in the MVP - only Component Model (future)
4. Therefore, we must compile all source forms together as one large expression

**Implementation**:
1. Read all 41 source files in `*compilation-order*` dependency order
2. Filter each file's forms using `compilable-form-p` (skip in-package, defpackage, declare)
3. Collect all compilable forms into a single list
4. Wrap in `(progn ...)` and call `compile-to-wasm`
5. The existing compiler handles defun extraction, function indexing, global allocation

**Alternatives Considered**:
- **Separate compilation + linking**: Rejected. Wasm MVP has no linking. Component Model is not yet stable.
- **Compile each module to WAT, concatenate WAT, assemble**: Rejected. Would break function indices and type references.
- **Use wasm-ld linker**: Rejected. Designed for linear memory, not WasmGC.

## R2: Entry Point Interface

**Question**: How does Stage 0 receive input and produce output?

**Decision**: Export `compile(source: externref) -> externref` function

**Rationale**:
1. wasmtime supports `--invoke` to call exported functions with arguments
2. String I/O requires host interaction via FFI (no raw string passing in Wasm)
3. Using `externref` allows the host shim to handle UTF-8 string marshalling
4. Output is Wasm bytes as externref (host interprets as Uint8Array)

**Implementation**:
1. Stage 0 exports: `_start` (default entry), `compile` (main API)
2. Host shim (verify-stage0.js) provides:
   - `host.read_string(externref) -> string`
   - `host.write_bytes(externref)`
3. Verification tests call `compile` with Lisp source, validate output with `wasm-tools`

**Alternatives Considered**:
- **File-based I/O via WASI**: Rejected. Adds complexity; wasmtime --invoke is simpler for verification.
- **Return i32 only**: Rejected. Cannot pass Wasm bytes back; need externref for byte array.

## R3: Bootstrap Environment Isolation

**Question**: How to prevent SBCL's CL symbols from conflicting with Clysm's cross-compiled symbols?

**Decision**: Use existing package structure; reset compiler state between modules

**Rationale**:
1. Clysm already uses separate packages (clysm/compiler, clysm/reader, etc.)
2. The `compile-to-module` function already resets lambda state and special-var globals
3. Cross-compilation reads source files, doesn't `load` them into SBCL
4. Symbol conflicts only occur if we tried to `load` the source files

**Implementation**:
1. Bootstrap script loads Clysm compiler via ASDF (normal)
2. Reads source files as data (strings → S-expressions)
3. Compiles S-expressions through Clysm's `compile-to-wasm`
4. No symbol conflicts because we're not defining functions in SBCL

**Alternatives Considered**:
- **Fresh package namespace**: Rejected. Clysm doesn't need it - we're reading, not loading.
- **Shadow all CL symbols**: Rejected. Over-engineering; current approach works.

## R4: Wasm Type Section Allocation

**Question**: How are type indices allocated across 41 modules?

**Decision**: Use existing type allocation in `emit-type-section`

**Rationale**:
1. The existing compiler pre-allocates types 0-22 for GC types and function signatures
2. Regular function types start at index 23
3. Since we compile all forms together, the compiler handles type allocation automatically
4. Lambda functions reuse types 8-12 (arity dispatch)

**Implementation**:
1. No changes needed - existing `emit-type-section` handles this
2. Each `defun` gets a unique type index >= 23
3. FFI types (if any) are appended after regular function types

**Alternatives Considered**:
- **Pre-scan all functions for type deduplication**: Rejected. Current approach already works.

## R5: Function Index Management

**Question**: How are function indices allocated for hundreds of functions across 41 modules?

**Decision**: Sequential allocation during `compile-to-module`

**Rationale**:
1. `compile-to-module` extracts defuns in first pass, registers them with `env-add-function`
2. Function indices are assigned sequentially: 0 = `$main`, 1+ = defuns
3. Lambda functions are compiled after defuns and appended
4. Cross-references within the same compilation unit work correctly

**Implementation**:
1. Main function always gets index 0
2. Defuns get indices 1, 2, 3, ... in order of definition
3. Lambdas get indices after all defuns
4. `(:call N)` instructions reference correct indices

**Alternatives Considered**:
- **Two-phase compilation with fixup**: Rejected. Sequential allocation is simpler and works.

## R6: Global Variable Management

**Question**: How are global variables (NIL, UNBOUND, special vars) managed?

**Decision**: Use existing `generate-runtime-globals` + special var allocation

**Rationale**:
1. Runtime globals (NIL=0, UNBOUND=1, mv-count=2, mv-buffer=3) are pre-allocated
2. Special variables get symbol globals starting at index 4+
3. `allocate-special-var-global` handles new special vars as they're parsed
4. Single compilation unit means all globals are in one module

**Implementation**:
1. `compile-to-module` calls `generate-runtime-globals` for base globals
2. During AST parsing, `defvar`/`defparameter` register special variables
3. `allocate-special-var-global` assigns global indices
4. Final module includes all globals in correct order

**Alternatives Considered**:
- **Separate global section per module**: Rejected. Single module needs unified globals.

## R7: Verification Strategy

**Question**: How do we verify Stage 0 works correctly?

**Decision**: Three-tier verification: wasm-tools validate, wasmtime invoke, result comparison

**Rationale**:
1. **Static validation**: `wasm-tools validate` catches structural errors
2. **Execution**: wasmtime can invoke exported functions
3. **Result verification**: Compare computed values against expected

**Implementation**:
1. After build: `wasm-tools validate dist/clysm-stage0.wasm`
2. Create `verify-stage0.js` host shim that:
   - Loads Stage 0 module
   - Calls `compile("(+ 1 2)")`
   - Validates returned Wasm bytes
   - Executes them and checks result = 3
3. Repeat for `defun` and `if` tests

**Alternatives Considered**:
- **Pure wasmtime without host shim**: Rejected. Need string/byte array marshalling.
- **SBCL-based verification**: Rejected. Would test SBCL, not wasmtime execution.

## Summary

| Decision | Choice | Key Reason |
|----------|--------|------------|
| R1 Module Strategy | Source concatenation | Wasm has no module linking |
| R2 Entry Point | `compile(externref)->externref` | wasmtime --invoke compatible |
| R3 Isolation | Package structure + state reset | No symbol conflicts when reading |
| R4 Types | Existing allocation (0-22 reserved) | Already works |
| R5 Functions | Sequential allocation | Simple, correct |
| R6 Globals | Runtime + special var globals | Already works |
| R7 Verification | wasm-tools + wasmtime + host shim | Three-tier confidence |
