# Feature Specification: Wasm Local Instruction Binding

**Feature Branch**: `001-wasm-local-binding`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Build a compiler enhancement system for Clysm that resolves Wasm local variable instruction binding issues (LOCAL.SET and LOCAL.TEE are currently unbound, causing 60 compilation failures) and exports missing internal functions"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Fix Local Variable Instruction Compilation (Priority: P1)

A compiler developer compiles Clysm source code to WebAssembly, and the compiler successfully emits local.set and local.tee bytecode instructions for local variable assignments. Currently, 60 functions fail to compile because these instruction symbols are unbound, blocking progress on the bootstrap pipeline.

**Why this priority**: This blocks 60 function compilations (40 LOCAL.SET + 20 LOCAL.TEE errors per Stage 1 report), representing the largest single addressable error category. Fixing this is essential for improving compilation coverage.

**Independent Test**: Can be fully tested by compiling functions that use local variable assignments (e.g., COMPILE-GETHASH, COMPILE-DENOMINATOR) and verifying the generated Wasm passes validation.

**Acceptance Scenarios**:

1. **Given** a Lisp function using local variable assignment, **When** the compiler processes LOCAL.SET instructions, **Then** valid Wasm bytecode (opcode 0x21) is emitted without "unbound variable" errors.
2. **Given** a Lisp function using local variable assignment with value return, **When** the compiler processes LOCAL.TEE instructions, **Then** valid Wasm bytecode (opcode 0x22) is emitted and the value remains on the stack.
3. **Given** the current compiler source files, **When** Stage 1 generation runs, **Then** the P221 and P987 error patterns are eliminated (0 occurrences).

---

### User Story 2 - Export ADVANCE-TOKEN for Parser Integration (Priority: P2)

A compiler developer writes parser code that needs to advance the tokenizer. The ADVANCE-TOKEN function is available from the clysm package and can be called from any parsing module.

**Why this priority**: 22 compilation failures reference undefined ADVANCE-TOKEN (P027 error pattern). This function is fundamental to the reader/parser subsystem.

**Independent Test**: Can be tested by verifying ADVANCE-TOKEN is exported from clysm package, registered in runtime function table, and parser functions that call it compile successfully.

**Acceptance Scenarios**:

1. **Given** ADVANCE-TOKEN exists in reader/tokenizer module, **When** another module calls (clysm:advance-token ...), **Then** the call resolves without "undefined function" error.
2. **Given** parser.lisp functions call ADVANCE-TOKEN, **When** Stage 1 generation compiles these functions, **Then** they succeed and P027 error count drops to 0.

---

### User Story 3 - Export EMIT-MODULE-HEADER for Wasm Generation (Priority: P3)

A compiler developer writes Wasm binary generation code that needs to emit module headers. The EMIT-MODULE-HEADER function is available from the clysm package for constructing valid Wasm modules.

**Why this priority**: 10 compilation failures reference undefined EMIT-MODULE-HEADER (P143 error pattern). This function is needed for the backend/wasm-emit module self-compilation.

**Independent Test**: Can be tested by verifying EMIT-MODULE-HEADER is exported from clysm package and functions calling it compile successfully.

**Acceptance Scenarios**:

1. **Given** EMIT-MODULE-HEADER exists in backend/wasm-emit, **When** EMIT-EMPTY-MODULE or other functions call it, **Then** no "undefined function" error occurs.
2. **Given** Stage 1 generation runs, **When** P143 pattern is checked, **Then** error count is 0.

---

### User Story 4 - Handle AST-TAGBODY Structure Serialization (Priority: P3)

A compiler developer compiles control flow code that uses TAGBODY/GO constructs. The AST-TAGBODY structure is properly serialized when appearing in compiled code, rather than causing unhandled structure errors.

**Why this priority**: 9 errors (P943) show AST-TAGBODY structures appearing in output where they should be serialized. This affects control flow compilation in multiple modules.

**Independent Test**: Can be tested by compiling functions with TAGBODY forms and verifying no raw #S(...) structures appear in error output.

**Acceptance Scenarios**:

1. **Given** a function containing TAGBODY/GO control flow, **When** compiled to Wasm, **Then** the AST-TAGBODY structure is properly handled (either serialized or transformed) without error.
2. **Given** Stage 1 generation runs, **When** P943 pattern is checked, **Then** error count is 0.

---

### User Story 5 - Verify Compilation Rate Improvement (Priority: P1)

A compiler developer regenerates the Stage 1 report after implementing fixes and observes a measurable improvement in compilation coverage from 19% to at least 25%.

**Why this priority**: The ultimate goal is improving self-hosting capability. Verification ensures changes have the intended impact and no regressions occurred.

**Independent Test**: Can be tested by running `sbcl --load build/stage1-complete.lisp` and comparing the generated report's coverage_pct field.

**Acceptance Scenarios**:

1. **Given** all previous fixes are implemented, **When** Stage 1 generation runs, **Then** coverage_pct in stage1-report.json is >= 25%.
2. **Given** Stage 1 Wasm is generated, **When** `wasm-tools validate dist/clysm-stage1.wasm` runs, **Then** exit code is 0.

---

### Edge Cases

- What happens when LOCAL.SET/LOCAL.TEE are used with different value types (i32, i64, f64, externref)?
- How does the system handle nested TAGBODY/GO with multiple tags?
- What if ADVANCE-TOKEN is called when tokenizer is at end-of-input?
- What if EMIT-MODULE-HEADER is called with invalid parameters?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST bind LOCAL.SET symbol to emit Wasm opcode 0x21 with correct local index encoding
- **FR-002**: System MUST bind LOCAL.TEE symbol to emit Wasm opcode 0x22 with correct local index encoding
- **FR-003**: System MUST export ADVANCE-TOKEN function from clysm package for parser integration
- **FR-004**: System MUST register ADVANCE-TOKEN in *runtime-function-table* with correct arity
- **FR-005**: System MUST export EMIT-MODULE-HEADER function from clysm package for Wasm generation
- **FR-006**: System MUST register EMIT-MODULE-HEADER in *runtime-function-table* with correct arity
- **FR-007**: System MUST handle AST-TAGBODY structures during compilation without unhandled structure errors
- **FR-008**: System MUST maintain backward compatibility with all currently compiled forms
- **FR-009**: System MUST generate Wasm that passes wasm-tools validate

### Key Entities

- **LOCAL.SET/LOCAL.TEE**: Wasm local variable instruction symbols that must be bound in the codegen environment
- **ADVANCE-TOKEN**: Tokenizer function for parser integration (reader/tokenizer.lisp)
- **EMIT-MODULE-HEADER**: Binary emitter function for Wasm module construction (backend/wasm-emit.lisp)
- **AST-TAGBODY**: AST structure representing TAGBODY control flow (compiler/ast.lisp)
- **\*runtime-function-table\***: Registry mapping function names to arities for Wasm dispatch

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 1 compilation coverage improves from 19% to at least 25% (6+ percentage point increase)
- **SC-002**: P221 error pattern (LOCAL.SET unbound) count reduces from 40 to 0
- **SC-003**: P987 error pattern (LOCAL.TEE unbound) count reduces from 20 to 0
- **SC-004**: P027 error pattern (ADVANCE-TOKEN undefined) count reduces from 22 to 0
- **SC-005**: P143 error pattern (EMIT-MODULE-HEADER undefined) count reduces from 10 to 0
- **SC-006**: P943 error pattern (AST-TAGBODY serialization) count reduces from 9 to 0
- **SC-007**: Generated Wasm passes wasm-tools validate with exit code 0
- **SC-008**: All existing unit tests continue to pass (no regressions)

## Assumptions

- The LOCAL.SET and LOCAL.TEE symbols need to be defined in the codegen layer's instruction binding environment (likely in func-section.lisp or wasm-ir.lisp)
- ADVANCE-TOKEN already exists and functions correctly; it only needs to be exported and registered
- EMIT-MODULE-HEADER already exists and functions correctly; it only needs to be exported and registered
- AST-TAGBODY handling requires adding a serialization case to the AST processing code
- The 25% coverage target assumes fixing the identified error patterns eliminates those specific failures
