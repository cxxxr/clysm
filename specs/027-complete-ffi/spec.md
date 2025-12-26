# Feature Specification: Complete FFI Foundation

**Feature Branch**: `027-complete-ffi`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 8C: FFI基盤を完成させる。目標はホスト環境との相互運用基盤の確立。"

## Background: Existing FFI Infrastructure

The FFI module (`src/clysm/ffi/`) provides substantial infrastructure:

**Implemented Components:**
- Type definitions: `marshal-type`, `foreign-function-decl`, `export-decl`, `wasm-import`, `ffi-environment`
- Marshalling: All Lisp↔Wasm type conversions for `:fixnum`, `:float`, `:string`, `:boolean`, `:anyref`
- Macros: `define-foreign-function`, `export-function`
- Import/Export generation: Collection, index assignment, section emission
- Conditions: `ffi-host-error`, `ffi-type-error`

**Gaps Requiring Completion:**
1. Error handling integration (try_table/catch wrappers are placeholder code)
2. Compiler pipeline integration (FFI declarations not processed during compilation)
3. Dynamic `call-host` implementation (currently signals error)
4. Callback support (host calling Lisp functions during FFI execution)

## User Scenarios & Testing

### User Story 1 - Declare and Call Host Function (Priority: P1)

A compiler developer declares a host function using `ffi:define-foreign-function` and the compiled Wasm module correctly imports and calls that function at runtime.

**Why this priority**: This is the core FFI use case. Without this, no host interoperability is possible.

**Independent Test**: Can be fully tested by declaring a host function (e.g., `host.log`), compiling Lisp code that calls it, and verifying the generated Wasm module contains the correct import declaration and call instruction.

**Acceptance Scenarios**:

1. **Given** a Lisp file with `(ffi:define-foreign-function console-log "host.log" (:string) :void)` and code `(console-log "Hello")`, **When** compiled to Wasm, **Then** the module contains an import for `"host"."log"` with proper function type and the body contains a `call` to that import.

2. **Given** a host function declaration with parameter type `:fixnum`, **When** calling the function with a Lisp integer, **Then** the generated code includes `i31.get_s` to marshal the i31ref to i32 before the call.

3. **Given** a host function returning `:float`, **When** the call completes, **Then** the generated code includes `struct.new $float` to wrap the returned f64 in a Lisp float object.

---

### User Story 2 - Export Lisp Function to Host (Priority: P2)

A compiler developer exports a Lisp function using `ffi:export-function` so that host code (JavaScript/wasmtime) can invoke it.

**Why this priority**: Bidirectional interop requires both calling into host and being called by host. Export is essential for event handlers, callbacks, and integration points.

**Independent Test**: Can be fully tested by exporting a Lisp function, compiling to Wasm, and verifying the module contains the correct export entry with a wrapper function that handles marshalling.

**Acceptance Scenarios**:

1. **Given** a Lisp function `my-add` and declaration `(ffi:export-function my-add :as "add" :signature ((:fixnum :fixnum) :fixnum))`, **When** compiled to Wasm, **Then** the module exports a function named `"add"` with correct parameter and result types.

2. **Given** an exported function receiving `:string` parameters, **When** the host calls it with externref values, **Then** the wrapper function correctly unmarshals each externref to the internal string type before calling the Lisp function.

3. **Given** an exported function returning `:boolean`, **When** the Lisp function returns `t` or `nil`, **Then** the wrapper marshals this to i32 `1` or `0` respectively.

---

### User Story 3 - Handle Host Call Errors (Priority: P2)

A compiler developer wants FFI calls to handle errors gracefully when host functions fail, translating host exceptions to Lisp conditions.

**Why this priority**: Robust error handling is essential for reliable programs. Without it, host failures crash the entire Wasm module.

**Independent Test**: Can be tested by compiling code that calls a host function designed to throw, and verifying the generated code contains try_table/catch structure that signals `ffi-host-error`.

**Acceptance Scenarios**:

1. **Given** a host function that throws an exception, **When** called from compiled Lisp code, **Then** the exception is caught and translated to an `ffi:ffi-host-error` condition with the function name and error message.

2. **Given** an FFI call wrapped in Lisp condition handling (`handler-case`), **When** a host error occurs, **Then** the Lisp error handler receives the `ffi-host-error` condition and can handle it.

---

### User Story 4 - Dynamic Host Function Invocation (Priority: P3)

A compiler developer calls host functions dynamically using `ffi:call-host` for functions not known at compile time.

**Why this priority**: Dynamic invocation enables REPL-style development and runtime-determined host interaction, but static declarations cover most use cases.

**Independent Test**: Can be tested by compiling code using `(ffi:call-host "host.random")` and verifying it generates a runtime lookup and call mechanism.

**Acceptance Scenarios**:

1. **Given** a call `(ffi:call-host "host.random")`, **When** compiled, **Then** the generated code looks up the function by name and invokes it dynamically.

2. **Given** a dynamic call with arguments `(ffi:call-host "host.add" 1 2)`, **When** executed, **Then** arguments are marshalled according to their runtime types and the result is returned.

3. **Given** a call to a non-existent host function, **When** executed at runtime, **Then** an `ffi:ffi-host-error` condition is signaled with an appropriate message.

---

### User Story 5 - Callback Support from Host (Priority: P3)

A compiler developer wants host code to call back into Lisp during an FFI call execution (re-entrant calls).

**Why this priority**: Advanced integration patterns like event handling and iterators require callbacks, but basic FFI works without them.

**Independent Test**: Can be tested by having host code (in test shim) invoke an exported Lisp function while inside another FFI call, verifying correct stack management.

**Acceptance Scenarios**:

1. **Given** an exported callback function and a host function that invokes it, **When** the host function is called from Lisp, **Then** the callback executes correctly and control returns properly.

2. **Given** multiple nested callbacks (Lisp calls host, host calls Lisp, that Lisp calls another host function), **When** executed, **Then** all calls resolve correctly with proper stack unwinding.

---

### Edge Cases

- What happens when marshalling a value outside the valid range for `:fixnum` (31-bit)? The system signals `ffi-type-error`.
- How does the system handle null/nil passed where a host expects a value? Nil is marshalled as externref null for strings, or i32 0 for booleans.
- What happens if an export wrapper receives an incorrectly-typed externref from host? Type cast failure signals `ffi-type-error`.
- What if `call-host` is called with mismatched argument count? The system signals `ffi-type-error` at runtime.

## Requirements

### Functional Requirements

- **FR-001**: System MUST integrate FFI declarations into the main compiler pipeline, processing `define-foreign-function` and `export-function` forms during compilation.
- **FR-002**: System MUST generate correct Wasm import section entries for all declared foreign functions, with proper type signatures.
- **FR-003**: System MUST generate Wasm export section entries for all exported Lisp functions, with wrapper functions handling marshalling.
- **FR-004**: System MUST generate inline marshalling instructions for all supported marshal types (`:fixnum`, `:float`, `:string`, `:boolean`, `:anyref`).
- **FR-005**: System MUST wrap FFI import calls in try_table/catch blocks to translate host exceptions to Lisp conditions.
- **FR-006**: System MUST support `ffi:call-host` for dynamic runtime invocation of host functions.
- **FR-007**: System MUST signal `ffi:ffi-host-error` when a host function call fails, including function name and error message.
- **FR-008**: System MUST signal `ffi:ffi-type-error` when a value cannot be marshalled to the expected type.
- **FR-009**: System MUST support re-entrant callbacks (host calling exported Lisp functions during FFI execution).
- **FR-010**: System MUST NOT use linear memory for FFI data transfer (WasmGC-First principle compliance).
- **FR-011**: System MUST generate correct function type indices in the type section for FFI signatures.

### Key Entities

- **ForeignFunctionDecl**: Declaration of an imported host function with Lisp name, host module/field names, parameter types, and return type.
- **ExportDecl**: Declaration of an exported Lisp function with export name and signature.
- **FFI Environment**: Compile-time registry of all import and export declarations.
- **Marshal Type**: Type specifier (`:fixnum`, `:float`, `:string`, `:boolean`, `:anyref`, `:void`) controlling Lisp↔Wasm conversion.

## Success Criteria

### Measurable Outcomes

- **SC-001**: All 6 marshal types round-trip correctly: a value marshalled to host and back equals the original.
- **SC-002**: Compiled Wasm modules with FFI declarations validate without errors.
- **SC-003**: FFI calls complete with acceptable overhead (host function invocation adds minimal latency).
- **SC-004**: 100% of FFI-related tests pass (unit, contract, and integration tests).
- **SC-005**: Error conditions are properly signaled and can be caught by Lisp condition handlers.
- **SC-006**: Re-entrant callback chains of depth 3 or more execute correctly without stack corruption.

## Assumptions

- Host environment provides standard JavaScript/wasmtime import mechanisms.
- Exception handling uses WasmGC exception handling proposal (try_table/throw/throw_ref).
- String representation in host is compatible with externref passing of WasmGC arrays.
- The condition system (Feature 014) is functional and can signal/handle conditions from FFI context.
