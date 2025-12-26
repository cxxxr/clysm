# Feature Specification: ANSI Common Lisp Equality Predicates and Logical Operators

**Feature Branch**: `024-equality-predicates`
**Created**: 2025-12-26
**Status**: Draft
**Input**: User description: "024-equality-predicates: ANSI Common Lisp等価述語と論理演算子を実装する"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Pointer Identity Comparison with eq (Priority: P1)

A Common Lisp developer uses `eq` to check if two objects are the exact same object in memory. This is the fastest equality test and is essential for symbol comparison, nil checks, and identity-based operations.

**Why this priority**: `eq` is the foundation for all other equality predicates and is the most frequently used equality test in Lisp programs. Symbol comparison and nil checks depend entirely on this predicate.

**Independent Test**: Can be fully tested by comparing symbols, nil, and identical object references. Delivers immediate value for symbol-based dispatching and conditional logic.

**Acceptance Scenarios**:

1. **Given** two identical symbols, **When** comparing with `eq`, **Then** returns T
2. **Given** a symbol and nil, **When** comparing with `eq`, **Then** returns NIL
3. **Given** two different symbols, **When** comparing with `eq`, **Then** returns NIL
4. **Given** two fixnums with same value, **When** comparing with `eq`, **Then** returns T (implementation-defined but consistent)
5. **Given** two cons cells with same contents but different allocation, **When** comparing with `eq`, **Then** returns NIL

---

### User Story 2 - Type-Aware Value Comparison with eql (Priority: P1)

A Common Lisp developer uses `eql` to compare objects where type identity matters for numeric comparisons. Two objects are eql if they are `eq`, or if they are numbers of the same type with the same value, or if they are characters that are `char=`.

**Why this priority**: `eql` is the default comparison for many standard functions (hash tables, member, assoc) and is essential for correct numeric behavior where `(eql 1 1.0)` must return NIL.

**Independent Test**: Can be fully tested by comparing numbers of same/different types, characters, and objects. Delivers correct hash table key behavior and list membership tests.

**Acceptance Scenarios**:

1. **Given** two fixnums with same value, **When** comparing with `eql`, **Then** returns T
2. **Given** a fixnum and a float with same numeric value (e.g., 1 and 1.0), **When** comparing with `eql`, **Then** returns NIL
3. **Given** two floats with same value, **When** comparing with `eql`, **Then** returns T
4. **Given** two characters that are char=, **When** comparing with `eql`, **Then** returns T
5. **Given** two ratios with same numerator and denominator, **When** comparing with `eql`, **Then** returns T

---

### User Story 3 - Structural Equality with equal (Priority: P2)

A Common Lisp developer uses `equal` to compare composite data structures by their content. Cons cells are compared element-by-element, strings are compared character-by-character.

**Why this priority**: `equal` enables meaningful comparison of lists and strings, which is essential for any non-trivial Lisp program that manipulates data structures.

**Independent Test**: Can be fully tested by comparing lists, nested lists, strings, and mixed structures. Delivers list and string comparison capability.

**Acceptance Scenarios**:

1. **Given** two cons cells with eql car and cdr, **When** comparing with `equal`, **Then** returns T
2. **Given** two nested lists with same structure and values, **When** comparing with `equal`, **Then** returns T
3. **Given** two strings with same characters, **When** comparing with `equal`, **Then** returns T
4. **Given** two strings with different characters, **When** comparing with `equal`, **Then** returns NIL
5. **Given** objects that are not cons or strings, **When** comparing with `equal`, **Then** falls back to `eql`

---

### User Story 4 - Case-Insensitive and Type-Coercing Comparison with equalp (Priority: P3)

A Common Lisp developer uses `equalp` for the most permissive equality test. Strings are compared case-insensitively, numbers are compared with type coercion.

**Why this priority**: `equalp` provides convenience functions for case-insensitive string matching and flexible numeric comparison, commonly used in user-facing applications.

**Independent Test**: Can be fully tested by comparing strings with different cases, numbers of different types with same mathematical value. Delivers case-insensitive string matching.

**Acceptance Scenarios**:

1. **Given** two strings differing only in case, **When** comparing with `equalp`, **Then** returns T
2. **Given** a fixnum and float with same numeric value (e.g., 1 and 1.0), **When** comparing with `equalp`, **Then** returns T
3. **Given** two characters differing only in case, **When** comparing with `equalp`, **Then** returns T
4. **Given** nested structures with equalp-equivalent elements, **When** comparing with `equalp`, **Then** returns T

---

### User Story 5 - Logical Negation with not (Priority: P1)

A Common Lisp developer uses `not` to invert boolean values. `not` returns T if its argument is NIL, and NIL otherwise. It is functionally identical to `null`.

**Why this priority**: `not` is a fundamental boolean operation required for any conditional logic.

**Independent Test**: Can be fully tested by passing various values including nil, t, and other objects.

**Acceptance Scenarios**:

1. **Given** NIL as argument, **When** calling `not`, **Then** returns T
2. **Given** T as argument, **When** calling `not`, **Then** returns NIL
3. **Given** any non-NIL object, **When** calling `not`, **Then** returns NIL

---

### User Story 6 - Short-Circuit Boolean Evaluation with and/or (Priority: P1)

A Common Lisp developer uses `and` and `or` special forms for short-circuit boolean evaluation. `and` evaluates forms left-to-right, returning NIL at the first NIL form or the value of the last form. `or` returns the first non-NIL value or NIL if all forms evaluate to NIL.

**Why this priority**: `and` and `or` are essential control flow constructs that provide both boolean logic and short-circuit evaluation for efficiency and side-effect control.

**Independent Test**: Can be fully tested by evaluating expressions with various combinations of NIL and non-NIL values, verifying short-circuit behavior.

**Acceptance Scenarios**:

1. **Given** `(and t t t)`, **When** evaluating, **Then** returns T
2. **Given** `(and t nil t)`, **When** evaluating, **Then** returns NIL without evaluating the third form
3. **Given** `(and 1 2 3)`, **When** evaluating, **Then** returns 3 (last value)
4. **Given** `(or nil nil t)`, **When** evaluating, **Then** returns T
5. **Given** `(or nil 5 (error "not reached"))`, **When** evaluating, **Then** returns 5 without evaluating the error form
6. **Given** `(or)`, **When** evaluating, **Then** returns NIL
7. **Given** `(and)`, **When** evaluating, **Then** returns T

---

### Edge Cases

- What happens when comparing circular structures with `equal` or `equalp`? (Assumption: undefined behavior, no infinite loop protection required in initial implementation)
- How does `eq` behave with identical character objects? (ANSI CL: implementation-defined; clysm uses ref.eq)
- How does `eql` handle NaN float values? (ANSI CL: `(eql nan nan)` is unspecified; follow IEEE 754 behavior where NaN != NaN)
- What happens when `and`/`or` receive no arguments? (`(and)` returns T, `(or)` returns NIL per ANSI CL)
- How does `equalp` handle arrays and hash-tables? (Deferred to future implementation; focus on strings, numbers, cons, characters for now)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST implement `eq` predicate that returns T if two arguments are the identical object using Wasm `ref.eq` instruction
- **FR-002**: Compiler MUST implement `eql` predicate that returns T if arguments are `eq`, or are numbers of the same type with same value, or are characters that are `char=`
- **FR-003**: `eql` MUST return NIL when comparing numbers of different types even if mathematically equal (e.g., `(eql 1 1.0)` returns NIL)
- **FR-004**: Compiler MUST implement `equal` predicate that recursively compares cons cells and compares strings character-by-character
- **FR-005**: `equal` MUST fall back to `eql` for non-cons, non-string objects
- **FR-006**: Compiler MUST implement `equalp` predicate that uses case-insensitive character/string comparison and type-coercing numeric comparison
- **FR-007**: `equalp` MUST use `char-equal` for character comparison and `string-equal` for string comparison
- **FR-008**: `equalp` MUST use numeric `=` (type-coercing) for number comparison
- **FR-009**: Compiler MUST implement `not` function that returns T if argument is NIL, NIL otherwise
- **FR-010**: `not` MUST be functionally equivalent to `null`
- **FR-011**: Compiler MUST implement `and` as a special form with short-circuit evaluation
- **FR-012**: `and` MUST evaluate forms left-to-right, returning NIL immediately when any form evaluates to NIL
- **FR-013**: `and` MUST return T when called with no arguments, and return the value of the last form when all forms are non-NIL
- **FR-014**: Compiler MUST implement `or` as a special form with short-circuit evaluation
- **FR-015**: `or` MUST evaluate forms left-to-right, returning the first non-NIL value immediately
- **FR-016**: `or` MUST return NIL when called with no arguments or when all forms evaluate to NIL

### Key Entities

- **Equality Predicate**: A function that compares two objects and returns T or NIL based on equality criteria
- **Special Form**: A syntactic construct handled directly by the compiler (not a function), enabling compile-time transformation for short-circuit evaluation
- **Object Identity**: The property that two references point to the exact same object in memory (tested by `eq`)
- **Structural Equality**: The property that two objects have equivalent content (tested by `equal`)
- **Type-Aware Equality**: Equality that considers the type of objects, not just their value (tested by `eql`)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All ANSI CL test suite tests for `eq` pass (eq.1-5 baseline)
- **SC-002**: All ANSI CL test suite tests for `eql` pass (eql.1-8 baseline)
- **SC-003**: All ANSI CL test suite tests for `equal` pass (equal.1-10 baseline)
- **SC-004**: All ANSI CL test suite tests for `equalp` pass (equalp.1-12 baseline)
- **SC-005**: All ANSI CL test suite tests for `not` pass
- **SC-006**: Short-circuit evaluation of `and`/`or` prevents evaluation of subsequent forms after result is determined (verifiable via side-effect tests)
- **SC-007**: `(and)` returns T and `(or)` returns NIL per ANSI CL specification
- **SC-008**: Generated Wasm modules validate successfully with `wasm-tools validate`

## Assumptions

- Circular structure detection is not required for initial implementation of `equal`/`equalp`
- `equalp` for arrays and hash-tables is deferred to a future feature
- The existing type system (fixnum, float, ratio, character, string, cons, symbol) from prior features is available
- `char-equal` and `string-equal` are already implemented or will be implemented as part of this feature
- The numeric comparison operator `=` with type coercion is already available from feature 010-numeric-tower

## Dependencies

- Feature 008-character-string: Character and string types, `char=`, `char-equal`, `string=`, `string-equal`
- Feature 010-numeric-tower: Numeric types (fixnum, float, ratio) and numeric `=` comparator
- Feature 007-sequence-functions: Cons cell implementation
- Feature 023-type-predicates: Type predicates for runtime type checking
