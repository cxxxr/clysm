# Feature Specification: String Runtime Migration

**Feature Branch**: `001-string-runtime-migration`
**Created**: 2026-01-03
**Status**: Draft
**Input**: User description: "Build a compiler code generation refactoring for Clysm that migrates string manipulation functions from inline Wasm codegen to a Lisp runtime library."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Runtime Library Implementation (Priority: P1)

Compiler developers need string manipulation functions implemented as pure Lisp in a runtime library rather than inline Wasm codegen. This enables easier maintenance, debugging, and future self-hosting since the same Lisp code compiles for both host and target.

**Why this priority**: Core infrastructure - without the runtime library, no functions can be migrated. This is the foundation for all other work.

**Independent Test**: Can be tested by loading string-runtime.lisp in SBCL and verifying each function produces correct results for standard string operations.

**Acceptance Scenarios**:

1. **Given** a string "Hello World" and character bag " ", **When** calling string-trim-rt, **Then** returns "Hello World" unchanged (no trimming needed)
2. **Given** a string "  hello  " and character bag " ", **When** calling string-trim-rt, **Then** returns "hello"
3. **Given** a string "hello world", **When** calling string-capitalize-rt with no keywords, **Then** returns "Hello World"
4. **Given** a string and index within bounds, **When** calling string-char-rt, **Then** returns the character at that index
5. **Given** two strings "ABC" and "abc", **When** calling string-compare-ci-rt with :equal, **Then** returns true (case-insensitive comparison)

---

### User Story 2 - Dispatch Table Registration (Priority: P2)

Compiler developers need string runtime functions registered in `*runtime-function-table*` so the codegen phase automatically dispatches calls to these functions instead of generating inline Wasm.

**Why this priority**: Connects runtime library to compiler. Without registration, the runtime functions exist but are never used during compilation.

**Independent Test**: Can be tested by checking that `(gethash 'string-trim *runtime-function-table*)` returns the function entry with correct arity.

**Acceptance Scenarios**:

1. **Given** string-trim-rt registered in *runtime-function-table*, **When** compiling `(string-trim " " str)`, **Then** compiler generates call to runtime function instead of inline Wasm
2. **Given** all 5 target functions registered, **When** querying *runtime-function-table*, **Then** each function has correct arity and dispatch information
3. **Given** a function with keyword arguments like :start/:end, **When** calling the registered function, **Then** keyword arguments are correctly passed to runtime implementation

---

### User Story 3 - Inline Codegen Removal (Priority: P3)

Compiler developers need the old compile-* functions removed from func-section.lisp to eliminate code duplication and reduce file size by approximately 710 lines (5 functions averaging 142 lines each).

**Why this priority**: Cleanup work. Only safe after runtime implementations are verified working. Reduces maintenance burden and file size.

**Independent Test**: Can be tested by verifying func-section.lisp no longer contains the compile-string-char, compile-string-trim, compile-string-capitalize, compile-string-compare-ci, and compile-nstring-capitalize functions.

**Acceptance Scenarios**:

1. **Given** string-runtime.lisp with working implementations, **When** removing compile-string-char from func-section.lisp, **Then** all existing string-char tests still pass
2. **Given** all 5 compile-* functions removed, **When** running full test suite, **Then** no regressions occur
3. **Given** func-section.lisp after removal, **When** counting lines, **Then** file is reduced by approximately 710 lines

---

### User Story 4 - ANSI Keyword Argument Support (Priority: P2)

Compiler developers need runtime implementations to support all ANSI CL keyword arguments (:start, :end, :test) for each applicable function, ensuring full Common Lisp compliance.

**Why this priority**: Essential for ANSI compatibility. Equal priority with registration since incomplete keyword support would cause silent failures.

**Independent Test**: Can be tested by calling each function with various combinations of :start, :end, and :test keywords and verifying correct behavior.

**Acceptance Scenarios**:

1. **Given** string "Hello World" and :start 6, **When** calling string-capitalize-rt, **Then** returns "Hello World" (only capitalizes from index 6)
2. **Given** string "  hello  " and :end 5, **When** calling string-trim-rt with " ", **Then** returns "hello  " (only trims up to index 5)
3. **Given** two strings with :test #'char-equal, **When** calling string comparison, **Then** uses case-insensitive comparison

---

### Edge Cases

- What happens when :start or :end is beyond string length? Returns subsequence up to actual length per ANSI spec
- How does system handle nil strings? Signals type-error per ANSI spec
- What happens when :start > :end? Signals error per ANSI spec
- How does string-capitalize handle non-alphabetic characters? Leaves them unchanged, treats as word boundaries
- What happens with empty character bag for string-trim? Returns string unchanged

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST create lib/string-runtime.lisp with pure Lisp implementations of target functions
- **FR-002**: System MUST implement string-char-rt supporting index access with bounds checking
- **FR-003**: System MUST implement string-trim-rt, string-left-trim-rt, string-right-trim-rt supporting character-bag parameter and :start/:end keywords
- **FR-004**: System MUST implement string-capitalize-rt supporting :start/:end keywords
- **FR-005**: System MUST implement string-compare-ci-rt supporting case-insensitive comparison with :start1/:end1/:start2/:end2 keywords
- **FR-006**: System MUST implement nstring-capitalize-rt as destructive variant of string-capitalize-rt
- **FR-007**: System MUST register all runtime functions in *runtime-function-table* with correct arities
- **FR-008**: System MUST remove compile-string-char, compile-string-trim, compile-string-capitalize, compile-string-compare-ci, compile-nstring-capitalize from func-section.lisp
- **FR-009**: System MUST maintain backward compatibility with all existing string operation tests
- **FR-010**: Runtime implementations MUST use only Layer 1 primitives (char, schar, length, make-string) for self-hosting compatibility

### Key Entities

- **string-runtime.lisp**: New runtime library file containing Lisp implementations of string functions
- **\*runtime-function-table\***: Hash table mapping function symbols to runtime dispatch entries
- **func-section.lisp**: Codegen file from which compile-* functions will be removed

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: func-section.lisp line count reduced by at least 700 lines
- **SC-002**: All 5 target functions (string-char, string-trim, string-capitalize, string-compare-ci, nstring-capitalize) implemented in string-runtime.lisp
- **SC-003**: All existing string-related unit tests pass without modification
- **SC-004**: Stage 1 compilation continues to succeed with Wasm validation passing
- **SC-005**: Runtime library code is self-documenting with HyperSpec references for each function
- **SC-006**: Each runtime function handles all ANSI-specified keyword arguments

## Assumptions

- The existing *runtime-function-table* dispatch mechanism works correctly (validated in 001-sequence-runtime-migration)
- Layer 1 primitives (char, schar, length, make-string) are available for runtime implementations
- The pattern established in sequence-runtime.lisp is the correct template for string-runtime.lisp
- ANSI CL string function specifications from HyperSpec define the authoritative behavior
