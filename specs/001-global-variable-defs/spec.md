# Feature Specification: Phase 13D-4 Global Variable Definitions

**Feature Branch**: `001-global-variable-defs`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 13D-4: グローバル変数定義を実装する。目標はコンパイラが使用する58種類のグローバル変数（defvar/defparameter）をWasmにコンパイル可能にすること。"

## Overview

This feature enables the Clysm compiler to compile Common Lisp global variable definitions ([defvar](../../resources/HyperSpec/Body/m_defpar.htm)/[defparameter](../../resources/HyperSpec/Body/m_defpar.htm)) to WasmGC. The goal is to support the prioritized 58 global variables (out of 142+ total) used by the compiler itself, enabling progress toward true self-hosting.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile [defvar](../../resources/HyperSpec/Body/m_defpar.htm)/[defparameter](../../resources/HyperSpec/Body/m_defpar.htm) Forms (Priority: P1)

A compiler developer writes Common Lisp code containing [defvar](../../resources/HyperSpec/Body/m_defpar.htm) or [defparameter](../../resources/HyperSpec/Body/m_defpar.htm) definitions and compiles it to WasmGC. The compiler generates valid Wasm global declarations with proper initialization.

**Why this priority**: This is the core functionality. Without basic [defvar](../../resources/HyperSpec/Body/m_defpar.htm)/[defparameter](../../resources/HyperSpec/Body/m_defpar.htm) compilation, none of the compiler's global state can be represented in Wasm.

**Independent Test**: Can be tested by compiling a simple `(defvar *foo* 42)` form and validating the generated Wasm contains a global with the correct initial value.

**Acceptance Scenarios**:

1. **Given** a source file containing `(defvar *counter* 0)`, **When** compiled with `clysm:compile-to-wasm`, **Then** generated Wasm contains a mutable global initialized to 0
2. **Given** a source file containing `(defparameter *name* "default")`, **When** compiled, **Then** generated Wasm contains a global initialized to the string value
3. **Given** a `defvar` with no initial value `(defvar *unset*)`, **When** compiled, **Then** generated Wasm contains a global initialized to the UNBOUND marker

---

### User Story 2 - Compile Runtime Registries (Priority: P2)

A compiler developer compiles code containing runtime registry globals like `*macro-registry*`, `*function-registry*`, `*setf-expanders*`, and `*compiler-macro-registry*`. These registries are initialized as hash tables or association lists.

**Why this priority**: The compiler's core dispatch mechanism depends on these registries. Without them, macro expansion and function lookup cannot work in the compiled Wasm.

**Independent Test**: Can be tested by compiling a file containing registry definitions and verifying the Wasm module initializes hash table or list structures for each registry.

**Acceptance Scenarios**:

1. **Given** `(defvar *macro-registry* (make-hash-table :test 'eq))`, **When** compiled, **Then** Wasm initializes a hash-table typed global
2. **Given** `(defvar *function-registry* (make-hash-table :test 'equal))`, **When** compiled, **Then** Wasm contains proper hash-table initialization
3. **Given** `(defvar *setf-expanders* '())`, **When** compiled, **Then** Wasm initializes global to NIL

---

### User Story 3 - Compile Package System Globals (Priority: P2)

A compiler developer compiles code with package system globals like `*current-package*`, `*packages*`, and `*keyword-package*`. These support the symbol interning and package lookup functionality.

**Why this priority**: Package handling is essential for symbol resolution. The compiler frequently references `*current-package*` during compilation.

**Independent Test**: Can be tested by compiling package-related defvar forms and verifying proper Wasm package structure initialization.

**Acceptance Scenarios**:

1. **Given** `(defvar *current-package* nil)`, **When** compiled, **Then** Wasm global is initialized to NIL and can be reassigned at runtime
2. **Given** `(defvar *packages* '())`, **When** compiled, **Then** Wasm global initialized as empty list for package registry
3. **Given** `(defparameter *keyword-package* (find-package :keyword))`, **When** compiled, **Then** Wasm contains deferred initialization for package lookup

---

### User Story 4 - Compile I/O Stream Globals (Priority: P3)

A compiler developer compiles code containing I/O stream globals like `*standard-output*`, `*standard-input*`, `*error-output*`, and `*debug-io*`. These streams interface with the host environment via FFI.

**Why this priority**: I/O is needed for compiler diagnostics and error reporting, but can be initially stubbed with minimal implementations.

**Independent Test**: Can be tested by compiling stream definitions and verifying Wasm generates proper stream object references or FFI bindings.

**Acceptance Scenarios**:

1. **Given** `(defvar *standard-output* nil)`, **When** compiled, **Then** Wasm global can hold stream reference type
2. **Given** `(defvar *trace-output* *standard-output*)`, **When** compiled, **Then** Wasm properly references another global in initialization
3. **Given** stream globals are compiled, **When** Wasm executes, **Then** streams can be bound to FFI host implementations

---

### User Story 5 - Compile Condition System Globals (Priority: P3)

A compiler developer compiles condition system globals like `*restart-clusters*`, `*handler-clusters*`, and `*debugger-hook*`. These support error handling and recovery in compiled code.

**Why this priority**: Condition handling is important for robust error recovery, but the compiler can initially operate with basic error signaling.

**Independent Test**: Can be tested by compiling condition system definitions and verifying Wasm produces correct cluster list structures.

**Acceptance Scenarios**:

1. **Given** `(defvar *restart-clusters* '())`, **When** compiled, **Then** Wasm global initialized to empty list
2. **Given** `(defvar *handler-clusters* '())`, **When** compiled, **Then** Wasm global supports dynamic extent binding
3. **Given** `(defvar *debugger-hook* nil)`, **When** compiled, **Then** Wasm global can hold function reference or NIL

---

### Edge Cases

- What happens when [defvar](../../resources/HyperSpec/Body/m_defpar.htm) initialization form cannot be evaluated at compile time? (Deferred initialization pattern using module $init function)
- How does the system handle circular references between globals? (Initialization order analysis detects cycles, uses deferred initialization)
- What happens when [defparameter](../../resources/HyperSpec/Body/m_defpar.htm) is recompiled with a new value? (Re-initialization semantics: [defparameter](../../resources/HyperSpec/Body/m_defpar.htm) always reinitializes, [defvar](../../resources/HyperSpec/Body/m_defpar.htm) preserves existing)
- How are globals with complex types (hash-tables, CLOS instances) initialized? (Type-specific initialization functions called from $init)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST generate Wasm global declarations for each [defvar](../../resources/HyperSpec/Body/m_defpar.htm) and [defparameter](../../resources/HyperSpec/Body/m_defpar.htm) form
- **FR-002**: Compiler MUST initialize globals with constant values at module instantiation time
- **FR-003**: Compiler MUST support deferred initialization for globals with non-constant initial values
- **FR-004**: Compiler MUST generate mutable globals (Wasm `global.set` compatible) for all Lisp special variables
- **FR-005**: Compiler MUST maintain a global symbol-to-index mapping for runtime lookup
- **FR-006**: Compiler MUST handle globals without initial values by setting to UNBOUND marker (global index 1)
- **FR-007**: Compiler MUST support re-assignment of globals via `setq` and dynamic binding via `let`/`let*`
- **FR-008**: Compiler MUST generate valid Wasm that passes `wasm-tools validate`
- **FR-009**: Compiler MUST support all prioritized 58 global variables (of 142+ total) used in the compiler source code
- **FR-010**: Compiler MUST preserve special variable semantics (dynamic scoping) for compiled globals

### Key Entities

- **Global Variable**: A Lisp special variable declared with [defvar](../../resources/HyperSpec/Body/m_defpar.htm) or [defparameter](../../resources/HyperSpec/Body/m_defpar.htm), represented as a Wasm global with `(ref null any)` type
- **Global Registry**: Internal mapping from symbol names to Wasm global indices, used for code generation and runtime lookup
- **Initialization Form**: The value expression provided to [defvar](../../resources/HyperSpec/Body/m_defpar.htm)/[defparameter](../../resources/HyperSpec/Body/m_defpar.htm), which may be constant or require runtime evaluation
- **UNBOUND Marker**: Special sentinel value (global index 1) indicating a variable has been declared but not initialized

### Global Variable Categories

| Category | Count | Examples |
|----------|-------|----------|
| Runtime Registries | ~10 | `*macro-registry*`, `*function-registry*`, `*setf-expanders*` |
| Package System | ~5 | `*current-package*`, `*packages*`, `*keyword-package*` |
| I/O Streams | ~10 | `*standard-output*`, `*standard-input*`, `*error-output*` |
| Condition System | ~8 | `*restart-clusters*`, `*handler-clusters*`, `*debugger-hook*` |
| Compilation State | ~15 | `*compile-file-pathname*`, `*gensym-counter*`, `*features*` |
| Reader State | ~10 | `*read-base*`, `*readtable*`, `*read-suppress*` |

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All prioritized 58 compiler-used global variables successfully compile without errors
- **SC-002**: Generated Wasm module passes `wasm-tools validate` with zero validation errors
- **SC-003**: Compiled globals maintain correct initial values when Wasm module is instantiated
- **SC-004**: Dynamic binding (`let` with special variables) works correctly for compiled globals
- **SC-005**: `setq` on compiled globals correctly updates the Wasm global value
- **SC-006**: Compiler compilation rate increases from current ~23% baseline (global-related forms now compile)

## Assumptions

- Wasm global indices 0-3 are reserved (NIL, UNBOUND, mv-count, mv-buffer) and new globals start at index 4 or higher
- All special variables use `(ref null any)` type for maximum flexibility
- Hash-table initialization requires the hash-table type (index 18) to be properly defined
- Complex initialization expressions will use a module-level `$init` function called at instantiation
- The host environment (Node.js shim) provides necessary FFI bindings for I/O streams

## Out of Scope

- Thread-local special variables (single-threaded execution assumed)
- Weak references for globals
- Global variable declaration at runtime (only compile-time [defvar](../../resources/HyperSpec/Body/m_defpar.htm)/[defparameter](../../resources/HyperSpec/Body/m_defpar.htm))
- Symbol-macro support (handled separately)

## Dependencies

- Phase 13D-3 (Compile-time Directives) - for conditional compilation of globals
- Existing Wasm type indices (hash-table type 18, etc.) properly defined
- FFI bindings for stream operations (from existing FFI implementation)
