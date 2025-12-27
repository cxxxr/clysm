# Feature Specification: Type Dispatch Macros

**Feature Branch**: `030-typecase-macros`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 9A: 型システム拡張を実装する。目標はtypecase/etypecase/ctypecase/check-typeマクロの完全実装。コンパイラは892箇所でtypecase系を使用しており、セルフホスティングの最大ブロッカー。typepへの展開によるマクロ実装、etypecaseのtype-errorシグナル、ctypecaseのstore-valueリスタート、check-typeの型検証を含む。既存の023-type-predicates（typep）と014-condition-system（type-error）を活用する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Type Dispatch with typecase (Priority: P1)

A Lisp developer wants to dispatch on the type of a value, executing different code branches based on what type the value is. They use `typecase` to write clean, readable type-based conditional logic.

**Why this priority**: This is the fundamental building block. The Clysm compiler uses typecase 892 times, making it the single largest blocker for self-hosting. Without typecase, the compiler cannot compile itself.

**Independent Test**: Can be fully tested by evaluating typecase forms with different value types and verifying the correct branch executes.

**Acceptance Scenarios**:

1. **Given** a typecase form with integer, string, and symbol clauses, **When** an integer value is passed, **Then** the integer clause body executes and returns its value
2. **Given** a typecase form, **When** the keyform matches multiple type specifiers in a clause, **Then** the first matching clause executes
3. **Given** a typecase form with no matching clauses, **When** evaluated, **Then** NIL is returned
4. **Given** a typecase form with an otherwise clause, **When** no typed clause matches, **Then** the otherwise clause executes
5. **Given** a typecase form with `t` as the last clause key, **When** no typed clause matches, **Then** the `t` clause executes (equivalent to otherwise)

---

### User Story 2 - Exhaustive Type Dispatch with etypecase (Priority: P1)

A Lisp developer wants to ensure all possible types are handled, signaling an error if an unexpected type is encountered. They use `etypecase` for exhaustive type matching with automatic error signaling.

**Why this priority**: etypecase is essential for defensive programming. It catches type mismatches at runtime with clear error messages, which is critical for self-hosting compiler development.

**Independent Test**: Can be fully tested by evaluating etypecase with matching types (success) and non-matching types (type-error signaled).

**Acceptance Scenarios**:

1. **Given** an etypecase form with matching clauses, **When** the keyform matches a clause, **Then** that clause body executes normally
2. **Given** an etypecase form, **When** no clause matches, **Then** a `type-error` is signaled
3. **Given** an etypecase that signals type-error, **When** the condition is caught, **Then** the datum is the tested value and expected-type is a type specifier listing all clause types
4. **Given** an etypecase form, **When** no otherwise or t clause is present, **Then** the macro signals type-error (unlike typecase which returns NIL)

---

### User Story 3 - Correctable Type Dispatch with ctypecase (Priority: P2)

A Lisp developer wants type dispatch with the ability to interactively correct type mismatches. They use `ctypecase` which provides a `store-value` restart allowing the place to be updated with a new value.

**Why this priority**: ctypecase enables interactive debugging and recovery. While less frequently used than typecase/etypecase, it's important for ANSI compliance and the REPL experience.

**Independent Test**: Can be fully tested by evaluating ctypecase, invoking store-value restart with a correct type, and verifying the corrected dispatch occurs.

**Acceptance Scenarios**:

1. **Given** a ctypecase form with matching clauses, **When** the keyform matches a clause, **Then** that clause body executes normally (same as typecase)
2. **Given** a ctypecase form, **When** no clause matches, **Then** a `type-error` is signaled with a `store-value` restart available
3. **Given** a ctypecase with active store-value restart, **When** invoked with a value of correct type, **Then** the place is updated and ctypecase re-evaluates
4. **Given** a ctypecase operating on a place (not just a value), **When** store-value updates the place, **Then** subsequent accesses to that place see the new value

---

### User Story 4 - Type Assertion with check-type (Priority: P1)

A Lisp developer wants to assert that a value is of a specific type, signaling an error with correction opportunity if not. They use `check-type` for input validation and contract enforcement.

**Why this priority**: check-type is heavily used for argument validation in Lisp code. It's essential for robust function implementations and is commonly used in production code.

**Independent Test**: Can be fully tested by calling check-type with correct types (returns NIL, no error) and incorrect types (type-error with store-value restart).

**Acceptance Scenarios**:

1. **Given** a check-type form with a value of the specified type, **When** evaluated, **Then** NIL is returned and no error is signaled
2. **Given** a check-type form with a value not of the specified type, **When** evaluated, **Then** a `type-error` is signaled
3. **Given** a check-type type-error, **When** store-value restart is invoked with correct type, **Then** the place is updated and check-type re-validates
4. **Given** a check-type form with optional type-string, **When** type-error is signaled, **Then** the type-string appears in the error message
5. **Given** a check-type form, **When** successful, **Then** the place value remains unchanged

---

### User Story 5 - Compound Type Specifiers (Priority: P2)

A Lisp developer wants to use compound type specifiers in typecase clauses, such as `(or integer symbol)`, `(and number (satisfies evenp))`, or `(member :a :b :c)`.

**Why this priority**: Compound type specifiers are needed for realistic Lisp code. The compiler uses patterns like `(or null cons)` for list processing.

**Independent Test**: Can be fully tested by evaluating typecase with compound type specifiers and verifying correct dispatch.

**Acceptance Scenarios**:

1. **Given** a clause with `(or integer symbol)` type specifier, **When** an integer or symbol is tested, **Then** the clause matches
2. **Given** a clause with `(and number (satisfies evenp))`, **When** an even number is tested, **Then** the clause matches
3. **Given** a clause with `(member :a :b :c)`, **When** `:b` is tested, **Then** the clause matches
4. **Given** a clause with `(not null)`, **When** a non-nil value is tested, **Then** the clause matches

---

### Edge Cases

- What happens when a clause type specifier is `t`? It matches any value, equivalent to otherwise.
- What happens when the keyform has side effects? The keyform is evaluated exactly once, and its value is cached.
- How does typecase handle NIL as a value? NIL matches `null`, `symbol`, `list`, and their supertypes.
- What happens with overlapping type specifiers? The first matching clause wins; later clauses are not evaluated.
- How does etypecase construct the expected-type for type-error? It creates an `(or ...)` type combining all clause types.
- What happens if check-type's type-string argument is complex? It's evaluated once and used in the error message.

## Requirements *(mandatory)*

### Functional Requirements

#### typecase Macro

- **FR-001**: System MUST provide `typecase` macro that evaluates keyform once and dispatches based on type
- **FR-002**: System MUST expand typecase to nested `(if (typep keyform 'type) ...)` forms
- **FR-003**: System MUST support multiple type specifiers per clause: `((type1 type2) body...)`
- **FR-004**: System MUST support `otherwise` and `t` as catch-all clause keys
- **FR-005**: System MUST return NIL when no clause matches and no otherwise clause exists
- **FR-006**: System MUST evaluate the keyform exactly once, binding to a gensym

#### etypecase Macro

- **FR-007**: System MUST provide `etypecase` macro with exhaustive matching requirement
- **FR-008**: System MUST signal `type-error` when no clause matches
- **FR-009**: System MUST NOT allow `otherwise` or `t` clauses in etypecase
- **FR-010**: System MUST construct expected-type as `(or type1 type2 ...)` for the type-error condition

#### ctypecase Macro

- **FR-011**: System MUST provide `ctypecase` macro operating on a place, not just a value
- **FR-012**: System MUST signal `type-error` with `store-value` restart when no clause matches
- **FR-013**: System MUST re-evaluate the place and re-dispatch after store-value is invoked
- **FR-014**: System MUST NOT allow `otherwise` or `t` clauses in ctypecase

#### check-type Macro

- **FR-015**: System MUST provide `check-type` macro: `(check-type place typespec [type-string])`
- **FR-016**: System MUST return NIL when the place value is of the specified type
- **FR-017**: System MUST signal `type-error` with `store-value` restart when type check fails
- **FR-018**: System MUST re-validate after store-value updates the place
- **FR-019**: System MUST include optional type-string in error message if provided

#### Type Specifier Support

- **FR-020**: System MUST support atomic type specifiers: `integer`, `string`, `symbol`, `cons`, `null`, `list`, `number`, `float`, `ratio`, `character`, `function`
- **FR-021**: System MUST support compound `(or ...)` type specifiers
- **FR-022**: System MUST support compound `(and ...)` type specifiers
- **FR-023**: System MUST support `(not typespec)` type specifiers
- **FR-024**: System MUST support `(member item1 item2 ...)` type specifiers
- **FR-025**: System MUST support `(satisfies predicate)` type specifiers

#### Integration with Existing Systems

- **FR-026**: System MUST use existing `typep` function from 023-type-predicates for type checking
- **FR-027**: System MUST use existing `type-error` condition from 014-condition-system
- **FR-028**: System MUST use existing `store-value` restart infrastructure from 014-condition-system
- **FR-029**: System MUST integrate with macro registry pattern from 016-macro-system

### Key Entities

- **Type Specifier**: A symbol or list describing a type for dispatch (e.g., `integer`, `(or null cons)`)
- **Clause**: A typecase clause containing type specifier(s) and body forms
- **Place**: A setf-able location that ctypecase and check-type can update via store-value
- **Expected Type**: The compound type created from all clause types for error reporting

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 892 typecase usages in the Clysm compiler pass without modification when compiled
- **SC-002**: etypecase signals type-error with correct datum and expected-type in 100% of mismatch cases
- **SC-003**: ctypecase and check-type store-value restarts successfully update places and re-dispatch
- **SC-004**: Compound type specifiers (or, and, not, member, satisfies) dispatch correctly in 100% of test cases
- **SC-005**: Keyform side effects execute exactly once, verified by counter-incrementing tests
- **SC-006**: All four macros expand to valid Wasm code that passes validation
- **SC-007**: Unit tests cover at least 95% of acceptance scenarios across all user stories

## Scope

### In Scope

1. **typecase macro**: Full ANSI CL-compliant implementation
2. **etypecase macro**: Exhaustive dispatch with type-error signaling
3. **ctypecase macro**: Correctable dispatch with store-value restart
4. **check-type macro**: Type assertion with correction capability
5. **Compound type specifiers**: or, and, not, member, satisfies
6. **typep integration**: Use existing type predicate infrastructure
7. **Condition system integration**: Use existing type-error and restart infrastructure

### Out of Scope

- **Full type system**: Complex types like `(array integer 3)`, class types beyond basic predicates
- **CLOS type integration**: Dispatch on user-defined CLOS classes (requires further CLOS work)
- **Compiler type inference**: Type inference or optimization based on typecase clauses
- **Values type specifier**: `(values ...)` type specifier for multiple values

## Assumptions

- The existing `typep` function supports all basic type specifiers needed
- The condition system's `type-error` and `store-value` restart are fully functional
- The macro registry system can register the new macros
- The gensym implementation provides unique symbols for keyform binding

## Dependencies

- **023-type-predicates**: Provides `typep` function for type checking
- **014-condition-system**: Provides `type-error` condition and `store-value` restart
- **016-macro-system**: Provides macro registry and expansion infrastructure
- **028-setf-generalized-refs**: Provides place infrastructure for ctypecase and check-type

## Constraints

- Must expand to typep calls, not inline type checks, for maintainability
- Must not introduce circular dependencies with the compiler's own use of typecase
- Must handle all type specifiers currently used in the 892 compiler typecase usages
