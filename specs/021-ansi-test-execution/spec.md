# Feature Specification: ANSI Test Execution

**Feature Branch**: `021-ansi-test-execution`
**Created**: 2025-12-25
**Status**: Draft
**Input**: User description: "ANSIテストスイートの実行を可能にする機能を構築"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Execute Basic Arithmetic Tests (Priority: P1)

A developer wants to verify that basic arithmetic operations work correctly in Clysm. They run the ANSI test suite's "numbers" category and see tests like `(+ 1 2) = 3` actually PASS rather than being skipped or failing due to compilation errors.

**Why this priority**: This validates that the core compilation pipeline works end-to-end. Without arithmetic tests passing, no meaningful compliance measurement is possible.

**Independent Test**: Can be fully tested by running `(run-ansi-tests :category "numbers")` and verifying at least 10% of tests show PASS status.

**Acceptance Scenarios**:

1. **Given** the numbers category tests, **When** the developer runs `(run-ansi-tests :category "numbers")`, **Then** tests like `(+ 1 2)` expecting result `3` show PASS status
2. **Given** a test with fixnum result, **When** the test is executed, **Then** the actual result is compared against expected and correctly marked PASS or FAIL
3. **Given** a test returning T or NIL, **When** the test completes, **Then** boolean values are correctly compared and classified

---

### User Story 2 - Execute Predicate Tests (Priority: P1)

A developer wants to verify that basic type predicates work. Tests like `(null nil) = T` and `(atom 1) = T` should PASS, demonstrating that predicates compile and execute correctly.

**Why this priority**: Predicates are fundamental to Lisp semantics. Passing predicate tests proves the type system is correctly integrated.

**Independent Test**: Can be fully tested by running tests in the data-and-control-flow category and checking that basic predicates pass.

**Acceptance Scenarios**:

1. **Given** predicate tests like `(null nil)`, **When** executed, **Then** the test PASSes when the result matches expected value T
2. **Given** predicate tests like `(atom 1)`, **When** executed, **Then** the test PASSes when the result matches expected value T
3. **Given** predicate tests returning NIL, **When** executed, **Then** the NIL result is correctly detected and compared

---

### User Story 3 - Execute Cons/List Operation Tests (Priority: P1)

A developer wants to verify that basic cons cell operations work. Tests involving `car`, `cdr`, `cons`, and `list` should PASS for simple cases, even if complex nested structures are skipped.

**Why this priority**: Cons cells are the fundamental data structure of Lisp. A working cons implementation is essential for any meaningful Common Lisp compliance.

**Independent Test**: Can be fully tested by running `(run-ansi-tests :category "cons")` and verifying at least 5% of tests pass.

**Acceptance Scenarios**:

1. **Given** cons category tests, **When** the developer runs the category, **Then** at least 5% of tests show PASS status
2. **Given** a test like `(car (cons 1 2))` expecting `1`, **When** executed, **Then** the test PASSes
3. **Given** a test returning a cons cell, **When** executed, **Then** it is marked as SKIP with reason "unverifiable" rather than FAIL

---

### User Story 4 - View Category Pass Rate Summary (Priority: P2)

A developer wants to understand overall compliance progress. After running tests, they see a summary like "cons: 80/1641 (4.9%) passed" showing clear metrics per category.

**Why this priority**: Measuring progress is essential for guiding development efforts, but requires basic test execution to work first.

**Independent Test**: Can be fully tested by running any category and verifying the summary output includes pass count, total count, and percentage.

**Acceptance Scenarios**:

1. **Given** tests have completed, **When** viewing results, **Then** summary shows format "category: PASS/TOTAL (X.X%) passed"
2. **Given** multiple categories are run, **When** all complete, **Then** overall summary aggregates all categories
3. **Given** a category with zero passes, **When** viewing results, **Then** 0% is displayed without errors

---

### User Story 5 - Classify Skip Reasons Accurately (Priority: P2)

A developer wants to understand why tests are skipped. Each skipped test shows a reason like "compile-error", "unsupported-form: FORMAT", or "unverifiable" to help prioritize future work.

**Why this priority**: Understanding skip reasons helps developers identify what features need implementation. Without this, the skip count is not actionable.

**Independent Test**: Can be fully tested by examining skip reasons in results and verifying they match expected categories.

**Acceptance Scenarios**:

1. **Given** a test using FORMAT, **When** executed, **Then** it is skipped with reason containing "unsupported-form: FORMAT"
2. **Given** a test that fails compilation, **When** executed, **Then** it is skipped with reason containing "compile-error"
3. **Given** a test returning a complex object, **When** executed, **Then** it is skipped with reason "unverifiable"

---

### Edge Cases

- What happens when a test form references an undefined symbol?
  - Test is skipped with reason "compile-error: undefined symbol X"
- What happens when wasmtime returns unexpected output format?
  - Output is logged and test marked as SKIP with reason "parse-error"
- What happens when a test has multiple expected values (MV test)?
  - Test is skipped with reason "unverifiable" (only single-value tests are verified currently)
- What happens when expected value is a symbol (not T/NIL)?
  - Test is skipped with reason "unverifiable" (symbol comparison not supported)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST compile simple Lisp expressions to valid Wasm bytecode that wasmtime can execute
- **FR-002**: System MUST correctly return fixnum results from compiled Wasm modules
- **FR-003**: System MUST correctly detect NIL returns using the sentinel value (-2147483648)
- **FR-004**: System MUST correctly detect T returns using the boolean representation
- **FR-005**: System MUST classify tests as PASS when actual result matches expected fixnum value
- **FR-006**: System MUST classify tests as PASS when actual result matches expected boolean (T/NIL)
- **FR-007**: System MUST classify tests as SKIP with reason "unverifiable" when result is not a fixnum or boolean
- **FR-008**: System MUST classify tests as SKIP with reason "compile-error: X" when compilation fails
- **FR-009**: System MUST display category pass rate in format "category: PASS/TOTAL (X.X%)"
- **FR-010**: System MUST complete execution of a single category within 2 minutes
- **FR-011**: System MUST handle tests with multiple expected values by skipping them as "unverifiable"

### Key Entities

- **TestCase**: Single test with name, category, form to evaluate, and expected values
- **TestResult**: Execution outcome with status (PASS/FAIL/SKIP), skip reason, actual value, execution time
- **CategoryResult**: Aggregated statistics for a category including pass/fail/skip counts and pass rate
- **WasmOutput**: Parsed output from wasmtime execution - can be fixnum, boolean, or unparseable

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: At least 10% of tests in the numbers category show PASS status
- **SC-002**: At least 5% of tests in the cons category show PASS status
- **SC-003**: Basic arithmetic tests like `(+ 1 2)`, `(- 5 3)`, `(* 2 3)` all PASS
- **SC-004**: Basic predicate tests like `(null nil)`, `(atom 1)` all PASS
- **SC-005**: Single category test run completes within 2 minutes
- **SC-006**: All skipped tests have documented reasons (no unexplained skips)
- **SC-007**: Pass rate summary displays correctly with format "X/Y (Z.Z%)"

## Assumptions

- The 020-ansi-test infrastructure (loader, runner, classifier) is correctly implemented
- The Clysm compiler can compile basic arithmetic, comparison, and predicate forms
- wasmtime can execute the generated Wasm modules with GC and function-references enabled
- Tests using FORMAT, PRINT, READ, CLOS, and stream I/O will remain skipped
- Fixnum range (-2^31+2 to 2^31-1) is sufficient for most arithmetic test cases
- Symbol comparison (beyond T/NIL) and cons cell comparison are deferred to future features
