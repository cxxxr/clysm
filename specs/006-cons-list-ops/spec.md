# Feature Specification: Cons Cell and List Operations

**Feature Branch**: `006-cons-list-ops`
**Created**: 2025-12-23
**Status**: Draft
**Input**: User description: "Common Lisp cons cell and list operations - the foundational data structure for Clysm compiler"

## Overview

This feature implements Common Lisp's cons cell (the fundamental pair data structure) and basic list operations. Cons cells are the most critical data structure in Lisp, serving as the foundation for:

- List representation and manipulation
- Macro expansion and backquote processing
- Sequence functions (future feature)
- S-expression representation

The implementation leverages the existing WasmGC `$cons` type (Type 2) already defined in the compiler.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Cons Cell Creation and Access (Priority: P1)

A Lisp programmer can create cons cells using `(cons x y)` and access their components using `(car ...)` and `(cdr ...)`. This is the fundamental building block for all list-based data structures.

**Why this priority**: Without cons/car/cdr, no list operations are possible. This is the absolute minimum viable implementation.

**Independent Test**: Compile and run `(car (cons 1 2))` - should return 1.

**Acceptance Scenarios**:

1. **Given** a Clysm program with `(cons 1 2)`, **When** compiled and executed, **Then** a cons cell is created with car=1 and cdr=2
2. **Given** a cons cell `(cons 1 2)`, **When** `car` is applied, **Then** returns 1
3. **Given** a cons cell `(cons 1 2)`, **When** `cdr` is applied, **Then** returns 2
4. **Given** nested cons cells `(cons (cons 1 2) 3)`, **When** `(car (car ...))` is applied, **Then** returns 1

---

### User Story 2 - NIL Handling with car/cdr (Priority: P1)

Per Common Lisp semantics, `(car nil)` and `(cdr nil)` both return NIL. This is essential for safe list traversal without explicit null checks.

**Why this priority**: NIL handling is required for any practical list processing. Without it, every list operation would need explicit null guards.

**Independent Test**: Compile and run `(car nil)` - should return NIL (not error).

**Acceptance Scenarios**:

1. **Given** NIL, **When** `car` is applied, **Then** returns NIL
2. **Given** NIL, **When** `cdr` is applied, **Then** returns NIL
3. **Given** an empty list traversal `(cdr (cdr (cons 1 nil)))`, **When** executed, **Then** returns NIL

---

### User Story 3 - List Construction with `list` Function (Priority: P1)

A Lisp programmer can create proper lists using `(list a b c ...)` which builds a chain of cons cells terminated by NIL.

**Why this priority**: While cons cells can build lists manually, the `list` function is essential for practical programming.

**Independent Test**: Compile and run `(car (cdr (list 1 2 3)))` - should return 2.

**Acceptance Scenarios**:

1. **Given** `(list)` with no arguments, **When** compiled and executed, **Then** returns NIL
2. **Given** `(list 1)`, **When** compiled and executed, **Then** returns a list with one element
3. **Given** `(list 1 2 3)`, **When** the third cdr is accessed, **Then** returns NIL (proper list termination)

---

### User Story 4 - Type Predicates (Priority: P2)

A Lisp programmer can test the type of values using `consp`, `null`, `atom`, and `listp` predicates.

**Why this priority**: Type predicates enable conditional logic based on data structure types, essential for recursive list processing.

**Independent Test**: Compile and run `(consp (cons 1 2))` - should return T.

**Acceptance Scenarios**:

1. **Given** a cons cell, **When** `consp` is applied, **Then** returns T
2. **Given** NIL, **When** `consp` is applied, **Then** returns NIL
3. **Given** NIL, **When** `null` is applied, **Then** returns T
4. **Given** a cons cell, **When** `null` is applied, **Then** returns NIL
5. **Given** a fixnum, **When** `atom` is applied, **Then** returns T
6. **Given** a cons cell, **When** `atom` is applied, **Then** returns NIL
7. **Given** NIL, **When** `listp` is applied, **Then** returns T (NIL is an empty list)
8. **Given** a cons cell, **When** `listp` is applied, **Then** returns T

---

### User Story 5 - Quoted List Literals (Priority: P2)

A Lisp programmer can write list literals using quote syntax `'(1 2 3)` instead of manual cons construction.

**Why this priority**: Quoted lists are the standard way to express literal data in Lisp code, essential for macro definitions and data specification.

**Independent Test**: Compile and run `(car '(a b c))` - should return the symbol A.

**Acceptance Scenarios**:

1. **Given** `'(1 2 3)`, **When** compiled and executed, **Then** creates a proper list of three elements
2. **Given** `'()`, **When** compiled and executed, **Then** returns NIL
3. **Given** nested quoted list `'((1 2) 3)`, **When** `(car (car ...))` is applied, **Then** returns 1

---

### User Story 6 - Destructive Modification (Priority: P3)

A Lisp programmer can modify existing cons cells using `rplaca` and `rplacd`.

**Why this priority**: Destructive operations enable in-place list modification for performance-critical code. Lower priority as functional style is preferred.

**Independent Test**: Compile and run `(let ((x (cons 1 2))) (rplaca x 10) (car x))` - should return 10.

**Acceptance Scenarios**:

1. **Given** a cons cell, **When** `rplaca` is applied with new value, **Then** car is updated
2. **Given** a cons cell, **When** `rplacd` is applied with new value, **Then** cdr is updated
3. **Given** a cons cell, **When** modified with rplaca, **Then** returns the modified cons cell (not the new value)

---

### User Story 7 - List Accessors (Priority: P3)

A Lisp programmer can use convenience accessors `first` through `tenth`, `rest`, `nth`, and `nthcdr`.

**Why this priority**: These are convenience functions that improve code readability. Can be implemented as simple wrappers.

**Independent Test**: Compile and run `(third '(a b c d))` - should return C.

**Acceptance Scenarios**:

1. **Given** a list, **When** `first` is applied, **Then** returns same as `car`
2. **Given** a list, **When** `rest` is applied, **Then** returns same as `cdr`
3. **Given** a list and index 2, **When** `nth` is applied, **Then** returns third element (0-indexed)
4. **Given** a list and n=2, **When** `nthcdr` is applied, **Then** returns the list after dropping 2 elements

---

### Edge Cases

- What happens when `car` or `cdr` is applied to a non-cons, non-nil value? **Error should be signaled**
- What happens with `(nth -1 list)`? **Undefined behavior per CLHS; implementation may return NIL**
- How are circular lists handled? **Not detected; operations may not terminate (out of scope)**
- What happens with improper lists like `(1 2 . 3)`? **Supported via cons; reader support is separate**

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST support `(cons x y)` to create a new cons cell with car=x and cdr=y
- **FR-002**: System MUST support `(car x)` to retrieve the car of a cons cell, returning NIL if x is NIL
- **FR-003**: System MUST support `(cdr x)` to retrieve the cdr of a cons cell, returning NIL if x is NIL
- **FR-004**: System MUST support `(list &rest args)` to create a proper list from arguments
- **FR-005**: System MUST support `(consp x)` returning T if x is a cons cell, NIL otherwise
- **FR-006**: System MUST support `(null x)` returning T if x is NIL, NIL otherwise
- **FR-007**: System MUST support `(atom x)` returning T if x is not a cons cell, NIL otherwise
- **FR-008**: System MUST support `(listp x)` returning T if x is a cons cell or NIL, NIL otherwise
- **FR-009**: System MUST support quoted list literals `'(...)` compiling to proper list structure
- **FR-010**: System MUST support `(rplaca cons val)` to destructively modify the car
- **FR-011**: System MUST support `(rplacd cons val)` to destructively modify the cdr
- **FR-012**: System MUST support `first` through `tenth` as aliases for successive car/cdr combinations
- **FR-013**: System MUST support `(rest x)` as alias for `(cdr x)`
- **FR-014**: System MUST support `(nth n list)` to access the n-th element (0-indexed)
- **FR-015**: System MUST support `(nthcdr n list)` to return list after n cdr operations

### Key Entities

- **Cons Cell**: The fundamental pair structure with mutable car and cdr fields, represented as WasmGC struct type `$cons` (Type 2)
- **Proper List**: A chain of cons cells where the final cdr is NIL
- **NIL**: The empty list / false value, represented as a singleton struct (per Constitution II, not Wasm null)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 15 functional requirements pass their respective unit tests
- **SC-002**: `cons`, `car`, `cdr` operations execute in constant time O(1)
- **SC-003**: `list` operation executes in linear time O(n) relative to argument count
- **SC-004**: Existing control flow tests (block, tagbody, catch/throw) continue to pass
- **SC-005**: Quoted list literals work correctly with nested structures up to 10 levels deep
- **SC-006**: Memory allocation is handled by WasmGC with no manual memory management required

## Assumptions

- The existing `$cons` GC type (Type 2) in `gc-types.lisp` is correctly defined and usable
- NIL is represented as a singleton struct with unique identity, checked via `ref.eq`
- Symbol representation for quoted symbols is already functional
- The compilation environment and instruction emission patterns are stable

## Out of Scope

- `append`, `reverse`, `mapcar` and other sequence functions (separate feature)
- Dotted list reader syntax support (reader feature)
- Circular list detection
- `setf` expansion for `(setf (car x) v)` (macro system feature)
- Print representation of lists (I/O feature)
