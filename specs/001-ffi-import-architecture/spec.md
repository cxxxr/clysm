# Feature Specification: FFI Import Architecture for Static and Dynamic Calls

**Feature Branch**: `001-ffi-import-architecture`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "FFI Import Architecture for Static and Dynamic Calls - Selective import of only used FFI functions to enable standalone Wasm execution"

## Clarifications

### Session 2025-12-31

- Q: What is the default compilation mode when none is specified? → A: Auto (detect and include dynamic-call support only when needed)
- Q: How should dynamic call failure (unknown function) be handled? → A: Throw a runtime error with the function name (standard CL behavior)
- Q: Should existing compiled modules (with all FFI imports) continue to work? → A: Full backward compatibility (host runtime accepts unused imports)

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Pure Computation Runs Standalone (Priority: P1)

A compiler user compiles a simple arithmetic expression like `(+ 1 2)` to WebAssembly. The resulting Wasm module should execute successfully in a standalone runtime (e.g., wasmtime) without requiring any host-provided imports. Currently, all registered FFI functions are included in the import section regardless of usage, causing "unknown import" errors.

**Why this priority**: This is the fundamental blocking issue. Until pure computations can run standalone, the compiler cannot produce useful output for the majority of use cases. This directly impacts self-hosting goals and developer experience.

**Independent Test**: Compile `(+ 1 2)` and execute with `wasmtime` directly. Success means the module runs and returns 3 without any host environment.

**Acceptance Scenarios**:

1. **Given** an expression with no FFI calls, **When** compiled to Wasm, **Then** the import section is empty or absent
2. **Given** a compiled Wasm module with no imports, **When** executed with wasmtime, **Then** it runs without "unknown import" errors
3. **Given** nested arithmetic expressions `(* (+ 1 2) (- 5 3))`, **When** compiled, **Then** no FFI imports are generated

---

### User Story 2 - Static FFI Calls Import Only Used Functions (Priority: P2)

A compiler user writes code that explicitly calls FFI functions like `(write-char #\A)` or `(sin 1.0)`. The compiled Wasm should only import the specific FFI functions that are statically referenced in the code, not all registered FFI functions.

**Why this priority**: Once standalone execution works, users need to use I/O and math functions. Importing only used functions keeps the module minimal and explicit about its dependencies.

**Independent Test**: Compile `(sin 1.0)` and verify the import section contains only the math-related import, not I/O functions.

**Acceptance Scenarios**:

1. **Given** code calling `(sin 1.0)`, **When** compiled, **Then** only `clysm:math/sin` is imported
2. **Given** code calling `(write-char #\A)`, **When** compiled, **Then** only `clysm:io/write-char` is imported, not math functions
3. **Given** code with quoted funcall `(funcall 'write-char #\A)`, **When** analyzed, **Then** it is treated as a static call to write-char

---

### User Story 3 - Dynamic Calls Work Through Host Runtime (Priority: P3)

A compiler user writes code with dynamic function calls where the function name is computed at runtime, such as `(funcall (intern "FOO") x)`. The system should detect these dynamic calls and provide a mechanism to resolve them at runtime through the host environment.

**Why this priority**: Dynamic calls are an advanced feature needed for full Common Lisp compatibility. Most code can work with static calls, so this is lower priority than the fundamental fixes.

**Independent Test**: Compile code with `(funcall (intern "IDENTITY") 42)` and run with the host runtime. The dynamic call should resolve correctly and return 42.

**Acceptance Scenarios**:

1. **Given** code with runtime-computed function name, **When** analyzed, **Then** it is marked as requiring dynamic call support
2. **Given** a module with dynamic calls, **When** executed with host runtime, **Then** function names are resolved at runtime
3. **Given** dynamic call to an unregistered function, **When** executed, **Then** a clear error message is produced

---

### User Story 4 - Compilation Modes Control FFI Behavior (Priority: P4)

A compiler user can select a compilation mode to control how FFI imports are handled: minimal mode for static-only imports, full mode for dynamic call support, or auto mode that adapts based on the code being compiled.

**Why this priority**: This provides flexibility for different deployment scenarios but is not essential for basic functionality.

**Independent Test**: Compile the same code with different modes and verify the import section changes accordingly.

**Acceptance Scenarios**:

1. **Given** code with dynamic calls compiled in minimal mode, **When** executed, **Then** compilation fails with a clear error about unsupported dynamic calls
2. **Given** code with dynamic calls compiled in full mode, **When** examined, **Then** the dynamic-call import is included
3. **Given** code with only static calls compiled in auto mode, **When** examined, **Then** no dynamic-call import is included

---

### Edge Cases

- What happens when a function is both called statically and dynamically in the same code?
- How does the system handle macro-generated FFI calls that aren't visible in source?
- What happens when `funcall` is called with a symbol that isn't an FFI function?
- How are FFI calls inside `labels` or `flet` local functions detected?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST analyze compiled code to detect which FFI functions are statically referenced
- **FR-002**: System MUST generate import sections containing only the detected FFI functions
- **FR-003**: System MUST distinguish between static calls (function known at compile time) and dynamic calls (function computed at runtime)
- **FR-004**: System MUST recognize `([funcall](resources/HyperSpec/Body/f_funcal.htm) 'symbol ...)` as a static call to the quoted symbol
- **FR-005**: System MUST recognize `([funcall](resources/HyperSpec/Body/f_funcal.htm) expr ...)` where expr is not a quoted symbol as a dynamic call
- **FR-005b**: System MUST detect `([apply](resources/HyperSpec/Body/f_apply.htm) expr ...)` with non-literal function as a dynamic call
- **FR-006**: System MUST provide a `dynamic-call` import when dynamic calls are detected
- **FR-007**: System MUST support three compilation modes: minimal, full, and auto (default: auto)
- **FR-008**: System MUST emit a clear error in minimal mode when dynamic calls are present
- **FR-009**: System MUST walk macro-expanded code for FFI analysis, not source code
- **FR-010**: System MUST handle FFI calls within local function definitions (labels, flet)
- **FR-011**: Dynamic calls to unknown functions MUST throw a runtime error including the function name
- **FR-012**: Host runtime MUST maintain backward compatibility with modules compiled before this change (accept unused imports)

### Key Entities

- **FFI Function**: A function implemented outside Wasm that requires a host import (name, module, signature)
- **Static Call**: A function call where the target function is known at compile time
- **Dynamic Call**: A function call where the target function is determined at runtime
- **FFI Analysis Result**: Contains list of used FFI functions and whether dynamic calls are present
- **Compilation Mode**: Configuration determining how FFI imports are generated (minimal/full/auto)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Pure arithmetic expressions compile to Wasm that executes in wasmtime without host imports
- **SC-002**: Modules with static FFI calls include only the imports for functions actually used (zero unused imports)
- **SC-003**: FFI analysis correctly identifies static vs dynamic calls with zero false positives on static code
- **SC-004**: Dynamic calls execute correctly when routed through the host runtime
- **SC-005**: Compilation time overhead from FFI analysis is less than 15% compared to current compilation
