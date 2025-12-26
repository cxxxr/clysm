# Feature Specification: Setf Macros and Generalized References

**Feature Branch**: `028-setf-generalized-refs`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 8H: setf系マクロと汎変数（Generalized References）を実装する。目標はANSI Common Lisp標準のplace操作サポート。setf, psetf, incf, decf, push, pop, pushnew, rotatef, shiftfマクロと、car/cdr/nth/aref/gethash等の標準setf展開子を実装。define-setf-expanderによるユーザー定義place展開もサポートする。これによりANSIテスト通過率を大幅に向上させる。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Place Assignment with setf (Priority: P1)

A Lisp programmer needs to modify values stored in various data structure locations (places) using the standard `setf` macro. This is the most fundamental operation for mutable state in Common Lisp.

**Why this priority**: setf is the foundational macro upon which all other place modification operations depend. Without setf working correctly, no other generalized reference operations can function.

**Independent Test**: Can be fully tested by writing code that uses setf to modify cons cell car/cdr, array elements, and hash table entries, then verifying the modifications persist.

**Acceptance Scenarios**:

1. **Given** a cons cell `(cons 1 2)`, **When** user evaluates `(setf (car x) 10)`, **Then** the car of the cons cell becomes 10 and setf returns 10
2. **Given** a cons cell `(cons 1 2)`, **When** user evaluates `(setf (cdr x) 20)`, **Then** the cdr of the cons cell becomes 20
3. **Given** a list `'(1 2 3)`, **When** user evaluates `(setf (nth 1 x) 99)`, **Then** the second element becomes 99
4. **Given** an array `#(a b c)`, **When** user evaluates `(setf (aref arr 0) 'x)`, **Then** the first element becomes 'x
5. **Given** a hash table, **When** user evaluates `(setf (gethash :key ht) "value")`, **Then** the key-value pair is stored
6. **Given** a symbol with no value, **When** user evaluates `(setf (symbol-value 'x) 42)`, **Then** the symbol's value becomes 42

---

### User Story 2 - Parallel Place Assignment with psetf (Priority: P2)

A Lisp programmer needs to swap or exchange values between multiple places atomically, where all old values are read before any new values are written.

**Why this priority**: psetf enables value swapping without temporary variables and is essential for many algorithms. It depends on setf infrastructure but adds parallel semantics.

**Independent Test**: Can be fully tested by using psetf to swap two variables and verifying both get their new values based on the original values.

**Acceptance Scenarios**:

1. **Given** variables `a=1` and `b=2`, **When** user evaluates `(psetf a b b a)`, **Then** `a` becomes 2 and `b` becomes 1 (swap)
2. **Given** three variables, **When** user evaluates `(psetf x y y z z x)`, **Then** values rotate correctly based on original values
3. **Given** a list and variable, **When** user evaluates `(psetf (car x) y y (car x))`, **Then** the car and variable are swapped

---

### User Story 3 - Numeric Modification with incf/decf (Priority: P2)

A Lisp programmer needs to increment or decrement numeric values stored in places, optionally by a specified delta.

**Why this priority**: incf/decf are among the most commonly used place modification macros, essential for counters, loops, and numeric algorithms.

**Independent Test**: Can be fully tested by using incf/decf on variables and structure slots, verifying values change by the correct delta.

**Acceptance Scenarios**:

1. **Given** a variable `x=10`, **When** user evaluates `(incf x)`, **Then** `x` becomes 11 and incf returns 11
2. **Given** a variable `x=10`, **When** user evaluates `(incf x 5)`, **Then** `x` becomes 15
3. **Given** a variable `x=10`, **When** user evaluates `(decf x)`, **Then** `x` becomes 9
4. **Given** a variable `x=10`, **When** user evaluates `(decf x 3)`, **Then** `x` becomes 7
5. **Given** a cons cell with numeric car, **When** user evaluates `(incf (car x))`, **Then** the car is incremented in place

---

### User Story 4 - List Manipulation with push/pop/pushnew (Priority: P2)

A Lisp programmer needs to treat places holding lists as stacks, pushing and popping elements.

**Why this priority**: push/pop are fundamental list manipulation operations used in nearly every non-trivial Lisp program for building and consuming lists.

**Independent Test**: Can be fully tested by using push/pop on list variables and verifying elements are added to front and removed from front.

**Acceptance Scenarios**:

1. **Given** a variable `x='(b c)`, **When** user evaluates `(push 'a x)`, **Then** `x` becomes `(a b c)` and push returns `(a b c)`
2. **Given** a variable `x='(a b c)`, **When** user evaluates `(pop x)`, **Then** `x` becomes `(b c)` and pop returns `a`
3. **Given** an empty list variable `x=nil`, **When** user evaluates `(push 'a x)`, **Then** `x` becomes `(a)`
4. **Given** a list `'(a b c)`, **When** user evaluates `(pushnew 'a x)`, **Then** `x` remains unchanged (a already exists)
5. **Given** a list `'(a b c)`, **When** user evaluates `(pushnew 'd x)`, **Then** `x` becomes `(d a b c)`
6. **Given** a list and `:test #'equal`, **When** user evaluates `(pushnew '(1 2) x :test #'equal)`, **Then** duplicate check uses equal

---

### User Story 5 - Value Rotation with rotatef/shiftf (Priority: P3)

A Lisp programmer needs to rotate values among multiple places or shift values while capturing the first value.

**Why this priority**: rotatef/shiftf are less commonly used than basic setf but essential for certain algorithms like circular buffers and queue operations.

**Independent Test**: Can be fully tested by using rotatef on three variables and verifying values rotate circularly.

**Acceptance Scenarios**:

1. **Given** variables `a=1, b=2, c=3`, **When** user evaluates `(rotatef a b c)`, **Then** `a=2, b=3, c=1`
2. **Given** two variables `a=1, b=2`, **When** user evaluates `(rotatef a b)`, **Then** `a=2, b=1` (swap)
3. **Given** variables `a=1, b=2, c=3`, **When** user evaluates `(shiftf a b c 4)`, **Then** `a=2, b=3, c=4` and shiftf returns 1
4. **Given** a single variable, **When** user evaluates `(shiftf x 10)`, **Then** old value is returned and `x` becomes 10

---

### User Story 6 - User-Defined Place Expanders (Priority: P3)

A Lisp programmer needs to define custom setf expansions for their own accessor functions using define-setf-expander.

**Why this priority**: Extensibility is important for library authors, but most users rely on built-in setf expanders. This enables CLOS slot accessor setf and other user-defined accessors.

**Independent Test**: Can be fully tested by defining a custom setf expander for a simple accessor and verifying setf works with it.

**Acceptance Scenarios**:

1. **Given** a define-setf-expander for a custom accessor, **When** user uses setf with that accessor, **Then** the custom expansion is invoked
2. **Given** a CLOS slot accessor, **When** user evaluates `(setf (accessor obj) value)`, **Then** the slot is modified
3. **Given** a complex place expression, **When** setf expander returns temps/vals/stores/access/store forms, **Then** correct evaluation order is maintained

---

### Edge Cases

- What happens when setf is used on a read-only place (constant, read-only slot)? System signals an error at compile time or runtime.
- How does system handle setf on an undefined accessor? Signals undefined-function error for missing setf function.
- What happens when incf/decf is used on a non-numeric value? Signals type-error condition.
- How does system handle pop on an empty list (nil)? Returns nil and leaves place as nil.
- What happens when psetf has odd number of arguments? Signals a compile-time error.
- How does system handle nested place forms like `(setf (car (cdr x)) 10)`? Correctly evaluates subforms and modifies innermost place.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement the `setf` macro that modifies generalized places and returns the new value
- **FR-002**: System MUST implement the `psetf` macro that performs parallel assignment to multiple places
- **FR-003**: System MUST implement `incf` macro that increments a numeric place by a delta (default 1)
- **FR-004**: System MUST implement `decf` macro that decrements a numeric place by a delta (default 1)
- **FR-005**: System MUST implement `push` macro that conses a value onto the front of a list place
- **FR-006**: System MUST implement `pop` macro that removes and returns the first element from a list place
- **FR-007**: System MUST implement `pushnew` macro that pushes only if item is not already a member
- **FR-008**: System MUST implement `rotatef` macro that rotates values among places
- **FR-009**: System MUST implement `shiftf` macro that shifts values left, returning the first original value
- **FR-010**: System MUST provide setf expanders for `car`, `cdr`, `first` through `tenth`
- **FR-011**: System MUST provide setf expanders for `nth`, `nthcdr`, `last`
- **FR-012**: System MUST provide setf expander for `aref` (array element access)
- **FR-013**: System MUST provide setf expander for `gethash` (hash table access)
- **FR-014**: System MUST provide setf expander for `symbol-value`, `symbol-function`, `symbol-plist`
- **FR-015**: System MUST implement `define-setf-expander` for user-defined place expansions
- **FR-016**: System MUST implement `get-setf-expansion` function to retrieve expansion for a place form
- **FR-017**: System MUST ensure correct evaluation order: subforms evaluated left-to-right once
- **FR-018**: System MUST support nested place forms like `(setf (car (cdr x)) 10)`
- **FR-019**: System MUST signal appropriate errors for invalid places at compile time when detectable
- **FR-020**: System MUST integrate with CLOS slot accessors for `(setf (accessor obj) value)` forms
- **FR-021**: `pushnew` MUST support `:key`, `:test`, and `:test-not` keyword arguments

### Key Entities

- **Place**: A form that identifies a location where a value can be stored (generalized reference)
- **Setf Expander**: A function that transforms a place form into its accessor and store forms
- **Setf Expansion**: A five-value tuple: (temps vals stores store-form access-form) that defines how to read and write a place

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All ANSI CL test suite cases for setf, psetf, incf, decf, push, pop, pushnew, rotatef, shiftf pass
- **SC-002**: All standard place accessors (car/cdr/nth/aref/gethash/symbol-value) work correctly with setf
- **SC-003**: User-defined setf expanders created with define-setf-expander work correctly
- **SC-004**: Nested place expressions evaluate subforms exactly once in left-to-right order
- **SC-005**: CLOS slot accessors integrate seamlessly with setf for reading and writing slot values
- **SC-006**: ANSI test pass rate increases by at least 5% for place-related tests

## Assumptions

- The existing macro system (Feature 016) is available and functional for defining these macros
- The existing CLOS foundation (Feature 026) provides slot accessor infrastructure
- Array and hash table types are already implemented in the runtime
- The compiler supports compile-time macro expansion and can detect some errors statically
- Standard list functions (cons, car, cdr, member) are available in the runtime

## Dependencies

- Feature 016: Macro System (for defmacro, macroexpand)
- Feature 026: CLOS Foundation (for slot accessor integration)
- Feature 007: Sequence Functions (for nth, nthcdr)
- Existing array and hash table implementations
