# Feature Specification: LOOP Macro Implementation

**Feature Branch**: `029-loop-macro`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 8I: ANSI Common LispのLOOPマクロを実装する。目標はFull LOOPのサポート。for/as節（算術・リスト・ハッシュテーブル走査）、累積節（collect/sum/count/maximize/minimize）、終了条件（while/until/always/never/thereis）、条件分岐（if/when/unless）、finally節をサポートする。既存のdo/dolist/dotimesマクロを参考に、tagbody/goベースの展開を行う。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Iteration with FOR/AS Clauses (Priority: P1)

As a Lisp programmer, I want to use LOOP's FOR/AS clauses for iteration so that I can iterate over numeric ranges, lists, and hash tables with concise syntax.

**Why this priority**: FOR/AS clauses are the foundation of LOOP. Without them, no iteration is possible. This is the minimal viable LOOP implementation.

**Independent Test**: Can be fully tested by compiling and running LOOP forms with FOR clauses over numeric ranges and lists. Delivers basic iteration capability.

**Acceptance Scenarios**:

1. **Given** a LOOP form with `FOR var FROM start TO end`, **When** compiled and executed, **Then** the loop iterates from start to end inclusive, binding var to each value
2. **Given** a LOOP form with `FOR var IN list`, **When** compiled and executed, **Then** the loop iterates over each element of the list
3. **Given** a LOOP form with `FOR var ON list`, **When** compiled and executed, **Then** the loop iterates over successive cdrs of the list
4. **Given** a LOOP form with `FOR var ACROSS vector`, **When** compiled and executed, **Then** the loop iterates over each element of the vector
5. **Given** a LOOP form with `FOR var BEING THE HASH-KEYS OF hash-table`, **When** compiled and executed, **Then** the loop iterates over all keys in the hash table
6. **Given** a LOOP form with `FOR var = expr THEN step-expr`, **When** compiled and executed, **Then** var is initialized to expr and updated to step-expr each iteration

---

### User Story 2 - Accumulation Clauses (Priority: P1)

As a Lisp programmer, I want to use LOOP's accumulation clauses so that I can collect results, sum values, count items, and find extrema during iteration.

**Why this priority**: Accumulation is the primary way LOOP returns useful values. Without accumulation, LOOP can only perform side effects.

**Independent Test**: Can be fully tested by compiling LOOP forms with COLLECT, SUM, COUNT, MAXIMIZE, MINIMIZE and verifying correct return values.

**Acceptance Scenarios**:

1. **Given** a LOOP form with `COLLECT expr`, **When** compiled and executed, **Then** the loop returns a list of all collected values in order
2. **Given** a LOOP form with `SUM expr`, **When** compiled and executed, **Then** the loop returns the sum of all expressions
3. **Given** a LOOP form with `COUNT expr`, **When** compiled and executed, **Then** the loop returns the count of times expr was non-nil
4. **Given** a LOOP form with `MAXIMIZE expr`, **When** compiled and executed, **Then** the loop returns the maximum value of expr across all iterations
5. **Given** a LOOP form with `MINIMIZE expr`, **When** compiled and executed, **Then** the loop returns the minimum value of expr across all iterations
6. **Given** a LOOP form with `APPEND expr`, **When** compiled and executed, **Then** the loop returns the appended result of all list expressions
7. **Given** a LOOP form with `NCONC expr`, **When** compiled and executed, **Then** the loop returns the nconced result of all list expressions

---

### User Story 3 - Termination and Control Clauses (Priority: P2)

As a Lisp programmer, I want to use LOOP's termination and control clauses so that I can exit loops early, repeat iterations, and express complex loop conditions.

**Why this priority**: Termination clauses provide essential control flow. They are secondary to iteration and accumulation but necessary for real-world use.

**Independent Test**: Can be fully tested by compiling LOOP forms with WHILE, UNTIL, ALWAYS, NEVER, THEREIS, RETURN and verifying correct termination behavior.

**Acceptance Scenarios**:

1. **Given** a LOOP form with `WHILE condition`, **When** condition becomes nil, **Then** the loop terminates immediately
2. **Given** a LOOP form with `UNTIL condition`, **When** condition becomes non-nil, **Then** the loop terminates immediately
3. **Given** a LOOP form with `ALWAYS condition`, **When** any condition is nil, **Then** the loop returns NIL immediately; otherwise returns T after all iterations
4. **Given** a LOOP form with `NEVER condition`, **When** any condition is non-nil, **Then** the loop returns NIL immediately; otherwise returns T after all iterations
5. **Given** a LOOP form with `THEREIS expr`, **When** any expr is non-nil, **Then** the loop returns that value immediately; otherwise returns NIL
6. **Given** a LOOP form with `RETURN expr`, **When** executed, **Then** the loop immediately returns the value of expr
7. **Given** a LOOP form with `LOOP-FINISH`, **When** executed, **Then** the loop terminates normally (executing any FINALLY clause)

---

### User Story 4 - Conditional Clauses (Priority: P2)

As a Lisp programmer, I want to use LOOP's conditional clauses so that I can conditionally execute loop body clauses based on runtime conditions.

**Why this priority**: Conditional clauses enable filtering and conditional accumulation, essential for practical data processing.

**Independent Test**: Can be fully tested by compiling LOOP forms with IF/WHEN/UNLESS and verifying that clauses are conditionally executed.

**Acceptance Scenarios**:

1. **Given** a LOOP form with `IF condition clause`, **When** condition is non-nil, **Then** clause is executed
2. **Given** a LOOP form with `WHEN condition clause`, **When** condition is non-nil, **Then** clause is executed (synonym for IF)
3. **Given** a LOOP form with `UNLESS condition clause`, **When** condition is nil, **Then** clause is executed
4. **Given** a LOOP form with `IF condition clause ELSE other-clause`, **When** condition is nil, **Then** other-clause is executed
5. **Given** a LOOP form with nested conditionals, **When** compiled and executed, **Then** all conditions are evaluated correctly in order

---

### User Story 5 - INITIALLY and FINALLY Clauses (Priority: P2)

As a Lisp programmer, I want to use LOOP's INITIALLY and FINALLY clauses so that I can execute setup and cleanup code before and after the main loop body.

**Why this priority**: These clauses enable resource management and result post-processing, important for robust loop constructs.

**Independent Test**: Can be fully tested by compiling LOOP forms with INITIALLY/FINALLY and verifying execution order.

**Acceptance Scenarios**:

1. **Given** a LOOP form with `INITIALLY forms`, **When** the loop starts, **Then** forms are executed before the first iteration
2. **Given** a LOOP form with `FINALLY forms`, **When** the loop terminates normally, **Then** forms are executed after the last iteration
3. **Given** a LOOP form with FINALLY and early return via RETURN, **When** RETURN is executed, **Then** FINALLY is NOT executed (per ANSI CL spec)
4. **Given** a LOOP form with FINALLY and normal termination, **When** iteration completes, **Then** FINALLY is executed before returning

---

### User Story 6 - Variable Binding Clauses (Priority: P2)

As a Lisp programmer, I want to use LOOP's WITH clause so that I can declare and initialize local variables within the loop scope.

**Why this priority**: WITH clause provides scoped variable declarations, enabling complex loop state management.

**Independent Test**: Can be fully tested by compiling LOOP forms with WITH and verifying variable binding and initialization.

**Acceptance Scenarios**:

1. **Given** a LOOP form with `WITH var = expr`, **When** the loop starts, **Then** var is bound to expr's value for the duration of the loop
2. **Given** a LOOP form with `WITH var`, **When** the loop starts, **Then** var is bound to NIL
3. **Given** a LOOP form with multiple WITH clauses, **When** the loop starts, **Then** all variables are bound in order (sequential binding)
4. **Given** a LOOP form with `WITH var AND var2 = expr2`, **When** the loop starts, **Then** both variables are bound in parallel (AND keyword)

---

### User Story 7 - DO Clause for Side Effects (Priority: P3)

As a Lisp programmer, I want to use LOOP's DO clause so that I can execute arbitrary forms for side effects during each iteration.

**Why this priority**: DO clause is simple but important for loops that perform actions rather than accumulate values.

**Independent Test**: Can be fully tested by compiling LOOP forms with DO and verifying that forms are executed each iteration.

**Acceptance Scenarios**:

1. **Given** a LOOP form with `DO forms`, **When** each iteration runs, **Then** forms are executed in order for side effects
2. **Given** a LOOP form with `DOING forms`, **When** each iteration runs, **Then** forms are executed (synonym for DO)

---

### User Story 8 - Named Loops and RETURN-FROM (Priority: P3)

As a Lisp programmer, I want to name my loops so that I can use RETURN-FROM to exit from nested loops by name.

**Why this priority**: Named loops enable structured control flow in complex nested loop scenarios.

**Independent Test**: Can be fully tested by compiling LOOP forms with NAMED and verifying RETURN-FROM exits correctly.

**Acceptance Scenarios**:

1. **Given** a LOOP form with `NAMED name`, **When** compiled, **Then** the loop establishes a block named 'name'
2. **Given** nested LOOP forms with outer loop `NAMED outer`, **When** inner loop calls `(return-from outer value)`, **Then** the outer loop returns value

---

### Edge Cases

- What happens when LOOP has no clauses? Returns NIL and loops infinitely (simple LOOP).
- What happens when FOR iterates over an empty list? Loop body never executes, returns NIL or accumulated default.
- What happens when COLLECT is used with no iterations? Returns empty list NIL.
- What happens when SUM is used with no iterations? Returns 0.
- What happens when MAXIMIZE/MINIMIZE with no iterations? Returns NIL.
- What happens with conflicting accumulation types (e.g., COLLECT and SUM in same loop)? Compile-time error.
- What happens when HASH-TABLE is modified during iteration? Undefined behavior (per ANSI CL spec).
- What happens with destructuring in FOR clause? Binds nested structure elements to multiple variables.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST implement LOOP as a macro that expands to tagbody/go-based code
- **FR-002**: Compiler MUST support FOR/AS clauses with FROM/TO/BELOW/ABOVE/DOWNTO/DOWNFROM/BY modifiers for arithmetic iteration
- **FR-003**: Compiler MUST support FOR/AS clauses with IN/ON modifiers for list iteration
- **FR-004**: Compiler MUST support FOR/AS clauses with ACROSS modifier for vector/string iteration
- **FR-005**: Compiler MUST support FOR/AS clauses with BEING THE HASH-KEYS/HASH-VALUES OF for hash table iteration
- **FR-006**: Compiler MUST support FOR/AS clauses with = and THEN for general iteration
- **FR-007**: Compiler MUST support COLLECT/COLLECTING accumulation clause returning a list
- **FR-008**: Compiler MUST support SUM/SUMMING accumulation clause returning numeric sum
- **FR-009**: Compiler MUST support COUNT/COUNTING accumulation clause returning count of non-nil values
- **FR-010**: Compiler MUST support MAXIMIZE/MAXIMIZING and MINIMIZE/MINIMIZING accumulation clauses
- **FR-011**: Compiler MUST support APPEND/APPENDING and NCONC/NCONCING accumulation clauses
- **FR-012**: Compiler MUST support WHILE and UNTIL termination clauses
- **FR-013**: Compiler MUST support ALWAYS, NEVER, and THEREIS boolean aggregation clauses
- **FR-014**: Compiler MUST support IF/WHEN/UNLESS conditional clauses with optional ELSE
- **FR-015**: Compiler MUST support INITIALLY and FINALLY clauses
- **FR-016**: Compiler MUST support WITH clause for local variable binding
- **FR-017**: Compiler MUST support DO/DOING clause for side effects
- **FR-018**: Compiler MUST support NAMED clause for establishing named blocks
- **FR-019**: Compiler MUST support RETURN clause for immediate loop exit with value
- **FR-020**: Compiler MUST support LOOP-FINISH for normal termination triggering FINALLY
- **FR-021**: Compiler MUST detect and report conflicting accumulation types at compile time
- **FR-022**: Compiler MUST support INTO keyword for specifying accumulation variable
- **FR-023**: Compiler MUST support multiple FOR clauses executed in parallel (stepped together)
- **FR-024**: Compiler MUST support AND keyword to combine parallel stepping in FOR/WITH clauses
- **FR-025**: Compiler MUST expand LOOP to efficient code reusing existing do/dolist/dotimes patterns

### Key Entities

- **Loop Form**: The complete LOOP expression with all clauses
- **Iteration Clause**: FOR/AS/REPEAT clauses that control iteration
- **Accumulation Clause**: COLLECT/SUM/COUNT/MAXIMIZE/MINIMIZE/APPEND/NCONC clauses that gather results
- **Termination Clause**: WHILE/UNTIL/ALWAYS/NEVER/THEREIS/RETURN clauses that control loop exit
- **Control Clause**: IF/WHEN/UNLESS/DO/INITIALLY/FINALLY clauses that control execution flow
- **Variable Clause**: WITH clause for local variable declarations
- **Loop Expansion**: The tagbody/go-based code generated from LOOP

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 8 user stories pass their acceptance scenarios with 100% success rate
- **SC-002**: LOOP forms compile without errors when given valid ANSI CL LOOP syntax
- **SC-003**: LOOP expansion produces correct results matching SBCL/CCL behavior for all test cases
- **SC-004**: Compile-time errors are reported for invalid LOOP syntax (conflicting accumulations, malformed clauses)
- **SC-005**: At least 50 ANSI CL LOOP test cases pass from the standard test suite
- **SC-006**: LOOP-based code executes correctly after compilation to WasmGC
- **SC-007**: Complex LOOP forms (combining FOR, accumulation, conditionals, FINALLY) produce correct results
- **SC-008**: Nested LOOP forms with NAMED and RETURN-FROM work correctly

## Assumptions

- The existing tagbody/go mechanism in Clysm is sufficient for LOOP expansion
- The existing do/dolist/dotimes macro implementations provide a working pattern to follow
- Hash table iteration can leverage the existing hash-table implementation (if available)
- Vector/string iteration can use existing sequence accessors
- The macro expansion happens at compile time, producing efficient runtime code
- ANSI CL LOOP behavior is the reference standard; SBCL/CCL implementations guide edge case behavior
