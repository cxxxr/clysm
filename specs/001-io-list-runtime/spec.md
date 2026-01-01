# Feature Specification: I/O and List Operations Runtime Migration

**Feature Branch**: `001-io-list-runtime`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Migrate I/O and list operations from func-section.lisp codegen to the runtime library. I/O functions (princ, print, format, write) should use FFI primitives like %host-write-char and %host-write-string. List operations (assoc, member, find, position) should be implemented using primitives car, cdr, and consp. Remove corresponding compile-* functions from func-section.lisp after migration. Target: reduce func-section.lisp from 18,233 lines to under 11,000 lines while maintaining identical behavior for all migrated functions."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Runtime Library I/O Functions (Priority: P1)

As a Clysm developer, I want I/O functions (princ, print, format, write) to be implemented in the runtime library using FFI primitives, so that the codegen layer is simplified and the functions can be more easily maintained and tested.

**Why this priority**: I/O functions are fundamental to any program's ability to produce output. Moving them to the runtime library reduces codegen complexity while maintaining full functionality. This is the highest impact change for reducing func-section.lisp size.

**Independent Test**: Can be fully tested by compiling a program that uses princ, print, format, and write functions and verifying that output is identical to the current implementation.

**Acceptance Scenarios**:

1. **Given** a program using `(princ "hello")`, **When** compiled with the new runtime, **Then** it outputs "hello" to stdout without quotes.
2. **Given** a program using `(print "hello")`, **When** compiled, **Then** it outputs `"hello"` with preceding newline and trailing space.
3. **Given** a program using `(format t "~A ~D" "test" 42)`, **When** compiled, **Then** it outputs "test 42" to stdout.
4. **Given** a program using `(format nil "~A" x)`, **When** compiled, **Then** it returns a string without writing to stdout.
5. **Given** a program using `(write obj :stream t)`, **When** compiled, **Then** it outputs the printed representation of obj.

---

### User Story 2 - Runtime Library List Search Operations (Priority: P1)

As a Clysm developer, I want list search functions (assoc, member, find, position) to be implemented in the runtime library using car/cdr/consp primitives, so that the compiler's codegen complexity is reduced.

**Why this priority**: List operations are used extensively throughout Lisp programs. Runtime implementations using primitives are easier to maintain and debug than inline codegen.

**Independent Test**: Can be fully tested by compiling programs that use member, assoc, find, and position with various arguments and verifying results match ANSI CL behavior.

**Acceptance Scenarios**:

1. **Given** `(member 'b '(a b c))`, **When** compiled, **Then** it returns `(b c)`.
2. **Given** `(member 'd '(a b c))`, **When** compiled, **Then** it returns `nil`.
3. **Given** `(member 2 '(1 2 3) :test #'<)`, **When** compiled, **Then** it returns `(3)` (first element greater than 2).
4. **Given** `(assoc 'b '((a . 1) (b . 2) (c . 3)))`, **When** compiled, **Then** it returns `(b . 2)`.
5. **Given** `(find #\a "banana")`, **When** compiled, **Then** it returns `#\a`.
6. **Given** `(position #\n "banana")`, **When** compiled, **Then** it returns `2`.
7. **Given** `(position-if #'evenp '(1 3 5 6 7))`, **When** compiled, **Then** it returns `3`.

---

### User Story 3 - Compile-* Function Removal (Priority: P2)

As a Clysm maintainer, I want the compile-* functions for migrated operations removed from func-section.lisp, so that the file size is reduced and there is no code duplication between codegen and runtime.

**Why this priority**: This is the cleanup step after migration. It cannot proceed until the runtime implementations are verified to work correctly.

**Independent Test**: Can be tested by verifying func-section.lisp line count is under 11,000 and all existing tests still pass.

**Acceptance Scenarios**:

1. **Given** I/O runtime migration is complete, **When** compile-princ, compile-print, compile-write, compile-format are removed, **Then** all tests pass.
2. **Given** list runtime migration is complete, **When** compile-assoc, compile-member, compile-find, compile-position are removed, **Then** all tests pass.
3. **Given** all migrations are complete, **When** func-section.lisp line count is measured, **Then** it is under 11,000 lines.

---

### User Story 4 - Variant Functions Migration (Priority: P2)

As a Clysm developer, I want the -if and -if-not variants of list functions to also use the runtime library, so that all related functions are handled consistently.

**Why this priority**: These are closely related to the core list functions and should migrate together to maintain consistency.

**Independent Test**: Can be tested by compiling programs using member-if, member-if-not, assoc-if, find-if, position-if and verifying correct behavior.

**Acceptance Scenarios**:

1. **Given** `(member-if #'evenp '(1 3 5 6 7))`, **When** compiled, **Then** it returns `(6 7)`.
2. **Given** `(member-if-not #'oddp '(1 3 5 6 7))`, **When** compiled, **Then** it returns `(6 7)`.
3. **Given** `(assoc-if #'symbolp '((1 . a) (b . 2)))`, **When** compiled, **Then** it returns `(b . 2)`.
4. **Given** `(find-if #'alpha-char-p "123abc")`, **When** compiled, **Then** it returns `#\a`.
5. **Given** `(position-if-not #'digit-char-p "123abc")`, **When** compiled, **Then** it returns `3`.

---

### Edge Cases

- What happens when format receives an unsupported directive? Signal a condition with informative message.
- How does member handle circular lists? Standard behavior: undefined, implementation may loop forever.
- What happens when position is called on an empty sequence? Returns nil.
- How does find handle :start/:end beyond sequence length? Signal type-error per ANSI CL.
- What happens when assoc receives nil entries in alist? Skip nil entries, only check consp entries.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Runtime library MUST implement princ using %host-write-char and %host-write-string FFI primitives
- **FR-002**: Runtime library MUST implement print with preceding newline and trailing space
- **FR-003**: Runtime library MUST implement write with keyword arguments :stream, :escape, :radix, :base, :circle, :pretty, :level, :length
- **FR-004**: Runtime library MUST implement format with directives ~A, ~S, ~D, ~B, ~O, ~X, ~%, ~&, ~~, ~R, ~C, ~F, ~E, ~G, ~$
- **FR-005**: Runtime library MUST implement member with :test, :test-not, and :key keyword arguments
- **FR-006**: Runtime library MUST implement member-if and member-if-not with :key argument
- **FR-007**: Runtime library MUST implement assoc with :test, :test-not, and :key keyword arguments
- **FR-008**: Runtime library MUST implement assoc-if with :key argument
- **FR-009**: Runtime library MUST implement find with :test, :key, :start, :end, :from-end keyword arguments
- **FR-010**: Runtime library MUST implement find-if and find-if-not with :key, :start, :end, :from-end arguments
- **FR-011**: Runtime library MUST implement position with :test, :key, :start, :end, :from-end keyword arguments
- **FR-012**: Runtime library MUST implement position-if and position-if-not with :key, :start, :end, :from-end arguments
- **FR-013**: Compiler MUST emit calls to runtime functions instead of inline code for migrated functions
- **FR-014**: Corresponding compile-* functions MUST be removed from func-section.lisp after migration is verified
- **FR-015**: All migrated functions MUST produce identical output/results as the current inline codegen implementations

### Key Entities

- **Runtime I/O Module**: Contains princ, print, write, format implementations using FFI primitives
- **Runtime List Module**: Contains member, assoc, find, position and their variants using car/cdr/consp primitives
- **FFI Primitives**: %host-write-char, %host-write-string for I/O operations
- **List Primitives**: car, cdr, consp for list traversal

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: func-section.lisp line count reduced from 18,233 to under 11,000 lines (at least 40% reduction)
- **SC-002**: All existing compiler tests pass after migration
- **SC-003**: No performance degradation for I/O operations (output speed within 10% of current)
- **SC-004**: All ANSI CL conformance tests for migrated functions pass
- **SC-005**: Compilation time for programs using migrated functions is not increased by more than 5%
- **SC-006**: Runtime library additions add fewer than 2,000 lines of code total

## Assumptions

- The existing lib/list-ops.lisp implementations (member*, assoc*, etc.) can serve as a reference or be adapted for the runtime
- FFI primitives %host-write-char and %host-write-string are already defined and functional
- The runtime library is loaded before any user code that uses these functions
- car, cdr, and consp are already available as primitives in the runtime
- The :test-not keyword argument follows ANSI CL deprecation guidance (supported but not recommended)

## Dependencies

- Existing FFI infrastructure (feature 027) must be functional
- lib/list-ops.lisp implementations can be referenced for list operation logic
- streams/ffi-io.lisp FFI definitions for I/O primitives
- Current compile-* implementations in func-section.lisp for behavior reference

## Out of Scope

- Optimizations beyond current behavior (e.g., inlining for simple cases)
- New format directives beyond ANSI CL standard
- Changes to other codegen functions not related to I/O or list operations
- Stream infrastructure changes (only using FFI primitives for output)
