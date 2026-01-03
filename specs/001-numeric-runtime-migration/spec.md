# Feature Specification: Numeric Runtime Migration

**Feature Branch**: `001-numeric-runtime-migration`
**Created**: 2026-01-04
**Status**: Draft
**Input**: User description: "Build a compiler code generation optimization for a Common Lisp to WebAssembly compiler that migrates numeric functions (parse-integer, write-to-string, rationalize, signum, phase) from inline Wasm codegen to a Lisp runtime library. The runtime functions should use only Layer 1 primitives (arithmetic, type predicates) and support ANSI CL keyword arguments. Functions should be registered in *runtime-function-table* for dispatch. Include unit tests for each function with edge cases (negative numbers, floats, complex numbers, radix conversion)."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compiler Runtime Dispatch for Numeric Functions (Priority: P1)

When the compiler encounters a call to `parse-integer`, `write-to-string`, `rationalize`, `signum`, or `phase`, it should emit a call to the corresponding runtime library function instead of generating inline Wasm instructions.

**Why this priority**: Core functionality - without runtime dispatch, no functions can be migrated. This is the foundation for all other stories.

**Independent Test**: Can be fully tested by compiling code that calls each numeric function and verifying the generated Wasm contains runtime calls rather than inline instructions.

**Acceptance Scenarios**:

1. **Given** a Lisp form `(parse-integer "123")`, **When** the compiler processes it, **Then** the output Wasm calls the runtime `parse-integer` function
2. **Given** a Lisp form containing `signum`, **When** compiled, **Then** no inline arithmetic is generated; instead a runtime dispatch occurs
3. **Given** all 5 target functions are called in a program, **When** compiled and validated, **Then** Wasm validation passes with all runtime calls properly linked

---

### User Story 2 - ANSI CL Keyword Argument Support (Priority: P2)

Runtime functions must support all ANSI Common Lisp keyword arguments as specified in the HyperSpec for each function.

**Why this priority**: ANSI conformance is essential for correct behavior; without keyword support, the functions would not match expected CL semantics.

**Independent Test**: Can be tested by calling each function with all valid keyword argument combinations and verifying correct results.

**Acceptance Scenarios**:

1. **Given** `(parse-integer "  -42xyz" :start 2 :end 5 :radix 10 :junk-allowed t)`, **When** executed, **Then** returns -42 and 5 as multiple values
2. **Given** `(write-to-string 255 :base 16)`, **When** executed, **Then** returns "FF"
3. **Given** `(parse-integer "1010" :radix 2)`, **When** executed, **Then** returns 10

---

### User Story 3 - Type-Correct Numeric Operations (Priority: P2)

Each runtime function must handle all applicable numeric types (integers, floats, ratios, complex numbers) correctly per ANSI CL specifications.

**Why this priority**: Type correctness ensures the functions work with the full numeric tower, critical for mathematical applications.

**Independent Test**: Can be tested by passing values of each numeric type and verifying correct behavior.

**Acceptance Scenarios**:

1. **Given** `(signum -5)`, **When** executed, **Then** returns -1
2. **Given** `(signum 3.14)`, **When** executed, **Then** returns 1.0 (float result for float input)
3. **Given** `(signum #C(3 4))`, **When** executed, **Then** returns the unit complex number #C(0.6 0.8)
4. **Given** `(phase #C(0 1))`, **When** executed, **Then** returns pi/2 (approximately 1.5707963...)
5. **Given** `(rationalize 0.5)`, **When** executed, **Then** returns 1/2

---

### User Story 4 - Layer 1 Primitive Compliance (Priority: P3)

Runtime functions must be implemented using only Layer 1 primitives (arithmetic operations, type predicates) without depending on higher-level constructs.

**Why this priority**: Ensures the runtime can bootstrap correctly and avoids circular dependencies in the compiler.

**Independent Test**: Can be verified by code review confirming only Layer 1 primitives are used, and by testing that Stage 1 compilation succeeds.

**Acceptance Scenarios**:

1. **Given** the runtime library source code, **When** analyzed, **Then** no dependencies on Layer 2+ constructs are found
2. **Given** the full compiler with runtime library, **When** Stage 1 is generated, **Then** Wasm validation passes

---

### User Story 5 - Comprehensive Unit Test Coverage (Priority: P3)

Each migrated function must have unit tests covering normal cases, edge cases, and error conditions.

**Why this priority**: Testing ensures correctness and prevents regressions as the compiler evolves.

**Independent Test**: Can be run via `sbcl --eval "(asdf:test-system :clysm)"` to verify all tests pass.

**Acceptance Scenarios**:

1. **Given** the test suite, **When** executed, **Then** all numeric runtime tests pass
2. **Given** edge cases (zero, negative, large numbers, special floats), **When** tested, **Then** correct results are returned
3. **Given** invalid inputs, **When** tested, **Then** appropriate errors are signaled

---

### Edge Cases

- What happens when `parse-integer` receives an empty string?
- How does `signum` handle positive/negative zero?
- How does `rationalize` handle infinity or NaN?
- What does `phase` return for real positive numbers (should be 0)?
- What does `phase` return for real negative numbers (should be pi)?
- How does `write-to-string` handle ratio and complex number types?
- What is the maximum radix supported by `parse-integer` and `write-to-string` (should be 36)?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST dispatch calls to `parse-integer` to the runtime library function
- **FR-002**: Compiler MUST dispatch calls to `write-to-string` to the runtime library function
- **FR-003**: Compiler MUST dispatch calls to `rationalize` to the runtime library function
- **FR-004**: Compiler MUST dispatch calls to `signum` to the runtime library function
- **FR-005**: Compiler MUST dispatch calls to `phase` to the runtime library function
- **FR-006**: All 5 functions MUST be registered in `*runtime-function-table*` with correct arities
- **FR-007**: `parse-integer` MUST support `:start`, `:end`, `:radix`, and `:junk-allowed` keyword arguments per ANSI CL
- **FR-008**: `write-to-string` MUST support `:base` keyword argument (radix 2-36) for integer output
- **FR-009**: `rationalize` MUST convert floats to ratios using continued fraction approximation
- **FR-010**: `signum` MUST return type-congruent results (integer for integer input, float for float, complex for complex)
- **FR-011**: `phase` MUST return the angle in radians for complex numbers and 0 or pi for real numbers
- **FR-012**: Runtime functions MUST use only Layer 1 primitives (arithmetic: +, -, *, /, mod, floor, ceiling, truncate; type predicates: integerp, floatp, rationalp, complexp, numberp)
- **FR-013**: Unit tests MUST cover each function with at least 5 test cases including edge cases
- **FR-014**: Wasm output MUST pass `wasm-tools validate` after migration

### Key Entities

- **Runtime Function Table**: Hash table mapping function names to runtime implementations and arities
- **Numeric Tower**: Integer, float, ratio, and complex number types with appropriate type predicates
- **Keyword Arguments**: ANSI CL keyword parameters with defaults and validation

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 5 numeric functions compile via runtime dispatch rather than inline Wasm
- **SC-002**: Generated Wasm passes `wasm-tools validate` with exit code 0
- **SC-003**: All unit tests pass (minimum 25 tests total across 5 functions)
- **SC-004**: Stage 1 compilation succeeds with the runtime library included
- **SC-005**: Each function correctly handles all applicable numeric types per ANSI CL
- **SC-006**: Keyword argument parsing works for all supported parameters
- **SC-007**: Runtime library source is maintainable Lisp (no inline Wasm assembly)

## Assumptions

- Layer 1 primitives include: arithmetic operations (+, -, *, /, mod, floor, ceiling, truncate, abs), type predicates (integerp, floatp, rationalp, complexp, numberp, zerop, minusp, plusp), and basic control flow
- Complex number support uses the existing `#C(real imag)` representation and `realpart`/`imagpart` accessors
- The `*runtime-function-table*` infrastructure from `001-io-list-runtime` is available and working
- `write-to-string` for non-integer types (ratios, floats, complex) uses standard print representation
- `rationalize` uses the standard continued fraction algorithm for float-to-ratio conversion
- Error signaling uses the existing condition system infrastructure
