# Feature Specification: Sequence and Array Runtime Migration

**Feature Branch**: `001-sequence-array-runtime`
**Created**: 2026-01-04
**Status**: Draft
**Input**: User description: "Build a compiler code generation refactoring for Clysm that migrates the remaining inline Wasm codegen functions (subseq, adjust-array) from func-section.lisp to the Lisp runtime library."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Subseq on Strings with UTF-8 (Priority: P1)

Developers using the Clysm compiler need the `subseq` function to correctly extract substrings from UTF-8 encoded strings, where indices refer to character positions rather than byte positions.

**Why this priority**: UTF-8 string handling is the most complex aspect of this migration due to multi-byte character boundaries. Getting this right is critical for ANSI compliance and correctness.

**Independent Test**: Can be fully tested by compiling Lisp code containing `(subseq "hello" 1 3)` and `(subseq "日本語" 1 2)` calls, then verifying the Wasm output produces correct results when executed.

**Acceptance Scenarios**:

1. **Given** a Lisp form `(subseq "hello" 1 3)`, **When** compiled through Clysm, **Then** the result is "el" (characters at indices 1 and 2)
2. **Given** a Lisp form `(subseq "日本語" 1)`, **When** compiled through Clysm, **Then** the result is "本語" (characters from index 1 to end, respecting UTF-8 boundaries)
3. **Given** a Lisp form `(subseq "café" 0 4)`, **When** compiled through Clysm, **Then** the result is "café" (correctly handling the accented character)

---

### User Story 2 - Subseq on Lists and Vectors (Priority: P1)

Developers using the Clysm compiler need `subseq` to work on lists and vectors (not just strings), returning subsequences of the appropriate type.

**Why this priority**: Lists and vectors are equally important sequence types. ANSI compliance requires subseq to work on all sequence types.

**Independent Test**: Can be fully tested by compiling Lisp code containing `(subseq '(a b c d) 1 3)` and `(subseq #(1 2 3 4) 2)` calls, verifying correct subsequence extraction.

**Acceptance Scenarios**:

1. **Given** a Lisp form `(subseq '(a b c d) 1 3)`, **When** compiled through Clysm, **Then** the result is `(b c)`
2. **Given** a Lisp form `(subseq #(1 2 3 4) 2)`, **When** compiled through Clysm, **Then** the result is `#(3 4)`
3. **Given** a Lisp form `(subseq '(1 2 3) 1 1)`, **When** compiled through Clysm, **Then** the result is `nil` (empty subsequence)

---

### User Story 3 - Adjust-Array Basic Resizing (Priority: P2)

Developers using the Clysm compiler need `adjust-array` to resize arrays, preserving existing elements and optionally filling new slots with an initial element.

**Why this priority**: Array adjustment is essential for dynamic array operations but less commonly used than subseq. Required for complete ANSI compliance.

**Independent Test**: Can be fully tested by compiling Lisp code containing `(adjust-array #(1 2 3) 5 :initial-element 0)` and verifying the resized array.

**Acceptance Scenarios**:

1. **Given** a Lisp form `(adjust-array #(1 2 3) 5 :initial-element 0)`, **When** compiled through Clysm, **Then** the result is `#(1 2 3 0 0)` (extended with initial element)
2. **Given** a Lisp form `(adjust-array #(1 2 3 4 5) 3)`, **When** compiled through Clysm, **Then** the result is `#(1 2 3)` (truncated)
3. **Given** a Lisp form `(adjust-array #(a b c) '(3))`, **When** compiled through Clysm, **Then** the result is `#(a b c)` (dimensions as list accepted)

---

### User Story 4 - Runtime Dispatch Integration (Priority: P2)

The compiler must dispatch calls to `subseq` and `adjust-array` through the `*runtime-function-table*` mechanism, eliminating inline Wasm codegen from func-section.lisp.

**Why this priority**: This is the architectural goal of the migration - moving complexity from inline Wasm to maintainable Lisp code. Enables easier future enhancements.

**Independent Test**: Can be verified by checking that the `compile-subseq` and `compile-adjust-array` functions no longer exist in func-section.lisp, and that the functions are registered in `*runtime-function-table*`.

**Acceptance Scenarios**:

1. **Given** `subseq` is called in compiled code, **When** the compiler processes it, **Then** the dispatch is via `*runtime-function-table*` to `:$subseq-rt`
2. **Given** `adjust-array` is called in compiled code, **When** the compiler processes it, **Then** the dispatch is via `*runtime-function-table*` to `:$adjust-array-rt`
3. **Given** Stage 1 compilation is run, **When** the build completes, **Then** the generated Wasm validates successfully with wasm-tools

---

### User Story 5 - Dead Code Removal (Priority: P3)

After successful migration, the original `compile-subseq` (~115 lines) and `compile-adjust-array` (~60 lines) functions must be removed from func-section.lisp to reduce code size and maintenance burden.

**Why this priority**: Cleanup is essential to achieve the refactoring goal, but can only happen after migration is verified working.

**Independent Test**: Can be verified by grepping func-section.lisp for `compile-subseq` and `compile-adjust-array` - should return no matches.

**Acceptance Scenarios**:

1. **Given** migration is complete, **When** func-section.lisp is examined, **Then** `compile-subseq` function does not exist
2. **Given** migration is complete, **When** func-section.lisp is examined, **Then** `compile-adjust-array` function does not exist
3. **Given** migration is complete, **When** func-section.lisp line count is checked, **Then** it is reduced by approximately 175 lines

---

### Edge Cases

- What happens when subseq start index equals end index? Returns empty sequence of same type
- What happens when subseq indices are out of bounds? Signals an error (ANSI-compliant)
- How does subseq handle UTF-8 continuation bytes at boundary? Indices count characters, not bytes; never splits multi-byte sequences
- What happens when adjust-array shrinks an array? Elements beyond new size are discarded
- What happens when adjust-array new size equals old size? Returns array unchanged (or copy if displaced)
- How does adjust-array handle multidimensional arrays? MVP supports 1D arrays only; multidimensional deferred to future

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement `subseq-rt` as a pure Lisp function in a new or existing runtime library file
- **FR-002**: System MUST implement `adjust-array-rt` as a pure Lisp function in the runtime library
- **FR-003**: `subseq-rt` MUST support all three ANSI sequence types: strings, lists, and vectors
- **FR-004**: `subseq-rt` MUST handle UTF-8 encoded strings correctly, with indices referring to character positions
- **FR-005**: `subseq-rt` MUST accept the standard signature `(sequence start &optional end)` per ANSI CL
- **FR-006**: `adjust-array-rt` MUST accept `:initial-element` keyword argument for filling new slots
- **FR-007**: `adjust-array-rt` MUST preserve existing elements when resizing arrays
- **FR-008**: Both functions MUST be registered in `*runtime-function-table*` with appropriate arities
- **FR-009**: Both functions MUST use only Layer 1 primitives (char, aref, length, make-array, etc.)
- **FR-010**: The `compile-subseq` function MUST be removed from func-section.lisp after migration
- **FR-011**: The `compile-adjust-array` function MUST be removed from func-section.lisp after migration
- **FR-012**: Stage 1 compilation MUST succeed after migration
- **FR-013**: Generated Wasm MUST pass `wasm-tools validate` after migration

### Key Entities

- **Runtime Library File**: Source file (e.g., `sequence-array-runtime.lisp` or extending `sequence-runtime.lisp`) containing the pure Lisp implementations
- **Runtime Function Table Entry**: Registration in `*runtime-function-table*` mapping symbol to runtime dispatch name and arity
- **UTF-8 Character Boundary**: Position in a string where a character starts (not a continuation byte)
- **Layer 1 Primitive**: Built-in functions already compiled to Wasm (length, char, aref, make-array, cons, car, cdr)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Runtime library implementation is less than 300 lines of pure Lisp code
- **SC-002**: func-section.lisp is reduced by approximately 175 lines (removal of compile-subseq and compile-adjust-array)
- **SC-003**: All 6 acceptance scenarios for subseq pass verification
- **SC-004**: All 3 acceptance scenarios for adjust-array pass verification
- **SC-005**: Stage 1 compilation produces valid Wasm (wasm-tools validate exit code 0)
- **SC-006**: Unit test suite achieves 100% coverage of implemented runtime functions
- **SC-007**: UTF-8 string operations correctly handle characters up to 4 bytes in length
- **SC-008**: No regression in existing sequence-runtime.lisp functionality

## Assumptions

- **A-001**: The existing UTF-8 helper functions from string-runtime.lisp (utf8-continuation-byte-p, decode-utf8-char) can be reused
- **A-002**: Layer 1 primitives (char, aref, length, make-array, make-string, cons) are available and functional in the runtime
- **A-003**: The `*runtime-function-table*` dispatch mechanism from func-section.lisp works correctly for new registrations
- **A-004**: MVP scope for adjust-array is 1D arrays only; multidimensional support can be added later if needed
- **A-005**: The existing registration pattern (register-*-runtime-functions) should be followed for consistency
