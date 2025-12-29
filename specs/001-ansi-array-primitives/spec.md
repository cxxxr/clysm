# Feature Specification: ANSI CL Array/Sequence Primitives

**Feature Branch**: `001-ansi-array-primitives`
**Created**: 2025-12-29
**Status**: Draft
**Input**: Phase 13D-1: ANSI CL配列・シーケンスプリミティブを実装する。目標はセルフホスティングのブロッカー解消。

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Array Element Access (Priority: P1)

As a compiler developer, I want to compile code containing `aref` and `svref` expressions so that defstruct-generated accessor functions can be compiled to Wasm, enabling compilation of the 90 defstruct-related forms currently blocking self-hosting.

**Why this priority**: This directly addresses the largest category of compilation blockers. defstruct accessors internally use `svref` for slot access, making this the highest-impact primitive to implement.

**Independent Test**: Can be tested by compiling `(aref #(1 2 3) 1)` and verifying it returns `2`. Also test that defstruct accessor forms compile successfully.

**Acceptance Scenarios**:

1. **Given** a simple vector literal `#(1 2 3)`, **When** `aref` is called with index 1, **Then** the compiled Wasm returns the value `2`
2. **Given** a defstruct with slots, **When** the generated accessor function is compiled, **Then** the accessor compiles to valid Wasm using `array.get`
3. **Given** an out-of-bounds index, **When** `aref` is called, **Then** an appropriate error condition is signaled

---

### User Story 2 - Sequence Coercion (Priority: P2)

As a compiler developer, I want to compile code containing `coerce` expressions so that list-to-vector and vector-to-list conversions work correctly, enabling compilation of utility code in leb128.lisp and other bootstrap files.

**Why this priority**: Many compiler utility functions use `coerce` to convert between sequence types. This is essential for compiling bootstrap infrastructure code.

**Independent Test**: Can be tested by compiling `(coerce '(1 2 3) 'vector)` and verifying it returns `#(1 2 3)`.

**Acceptance Scenarios**:

1. **Given** a list `'(1 2 3)`, **When** coerced to `'vector`, **Then** the result is a simple-vector `#(1 2 3)`
2. **Given** a vector `#(a b c)`, **When** coerced to `'list`, **Then** the result is a list `'(a b c)`
3. **Given** a string `"hello"`, **When** coerced to `'list`, **Then** the result is a list of characters

---

### User Story 3 - String Character Access (Priority: P3)

As a compiler developer, I want to compile code containing `schar` expressions so that string manipulation code compiles correctly, supporting UTF-8 string processing in the compiler.

**Why this priority**: String character access is used in lexer/parser code and symbol name processing. Less blocking than array access but still important for full compiler functionality.

**Independent Test**: Can be tested by compiling `(schar "hello" 0)` and verifying it returns the character `#\h`.

**Acceptance Scenarios**:

1. **Given** a string `"hello"`, **When** `schar` is called with index 0, **Then** the compiled Wasm returns the character `#\h`
2. **Given** a UTF-8 string with multibyte characters, **When** `schar` is called, **Then** the correct Unicode character is returned
3. **Given** an out-of-bounds index, **When** `schar` is called, **Then** an appropriate error condition is signaled

---

### User Story 4 - Generic Sequence Element Access (Priority: P4)

As a compiler developer, I want to compile code containing `elt` expressions so that generic sequence operations work uniformly across lists, vectors, and strings.

**Why this priority**: `elt` is the generic accessor that works on any sequence type. While specific accessors like `aref` and `schar` are more commonly used, `elt` is needed for generic sequence-processing code.

**Independent Test**: Can be tested by compiling `(elt '(a b c) 1)` returning `b`, and `(elt #(1 2 3) 1)` returning `2`.

**Acceptance Scenarios**:

1. **Given** a list, **When** `elt` is called with a valid index, **Then** the nth element is returned (0-indexed)
2. **Given** a vector, **When** `elt` is called with a valid index, **Then** the element at that index is returned
3. **Given** a string, **When** `elt` is called with a valid index, **Then** the character at that index is returned

---

### Edge Cases

- What happens when accessing an element with a negative index? - Signal a type-error for invalid index
- How does the system handle empty sequences? - Accessing any element signals an error
- What happens when coercing an empty list to vector? - Returns an empty vector `#()`
- How are multidimensional arrays handled by `aref`? - Support single-dimension only initially; multidimensional arrays are out of scope for Phase 13D-1
- What happens when the index is not an integer? - Signal a type-error

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST compile `aref` expressions to Wasm `array.get` instructions for simple-vector access
- **FR-002**: Compiler MUST compile `svref` expressions to Wasm `array.get` instructions (specialized for simple-vectors)
- **FR-003**: Compiler MUST compile `schar` expressions to Wasm `array.get` instructions for string character access
- **FR-004**: Compiler MUST compile `elt` expressions with type dispatch for lists, vectors, and strings
- **FR-005**: Compiler MUST compile `coerce` expressions for conversions between list and vector types
- **FR-006**: Compiler MUST compile `coerce` expressions for string-to-list and list-to-string conversions (character lists)
- **FR-007**: Compiled code MUST signal appropriate error conditions for out-of-bounds access
- **FR-008**: Compiled code MUST signal type-error for non-integer indices
- **FR-009**: `setf` forms for `aref`, `svref`, `schar`, and `elt` MUST be compilable (compile to Wasm `array.set`)
- **FR-010**: defstruct-generated accessor functions MUST compile to valid Wasm after these primitives are implemented

### Key Entities

- **Simple-Vector**: WasmGC array type representing Lisp simple-vector `#()`, element type is `externref` (any Lisp value)
- **String**: WasmGC array type representing UTF-8 encoded strings, element type is `i8` (byte)
- **Sequence**: Abstract concept encompassing lists, vectors, and strings for generic operations

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Compilation rate increases from 12.9% to at least 30% of compiler forms
- **SC-002**: All 90 defstruct-related forms that previously failed to compile now compile successfully
- **SC-003**: The test expression `(aref #(1 2 3) 1)` compiles and executes correctly, returning `2`
- **SC-004**: The test expression `(coerce '(1 2 3) 'vector)` compiles and executes correctly, returning `#(1 2 3)`
- **SC-005**: leb128.lisp compiles completely without array/sequence-related errors
- **SC-006**: Generated Wasm modules pass `wasm-tools validate` without errors

## Assumptions

- Single-dimensional arrays only are in scope for this phase; multidimensional array support will be addressed in a future phase
- The existing WasmGC type system (type indices 0-24 as documented) is sufficient for array operations
- Character encoding for strings is UTF-8 as already established in the codebase
- Error signaling uses the existing condition system infrastructure
- Performance optimization (bounds-check elimination, etc.) is not in scope for this phase

## Dependencies

- Existing WasmGC type infrastructure (type index 2 for `$string`)
- Existing codegen infrastructure for primitive operations
- defstruct macro expansion (already implemented)
- setf expander infrastructure (feature 028)

## Out of Scope

- Multidimensional arrays (ANSI CL `make-array` with multiple dimensions)
- Specialized array types (bit-vector, adjustable arrays, fill-pointers)
- Sequence functions like `map`, `reduce`, `find` (to be addressed in later phases)
- Array displacement and shared structure
- Performance optimizations for array access
