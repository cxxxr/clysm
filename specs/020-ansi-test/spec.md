# Feature Specification: ANSI Common Lisp Test Suite Integration

**Feature Branch**: `020-ansi-test`
**Created**: 2025-12-25
**Status**: Draft
**Input**: User description: "ANSI Common Lisp準拠テストスイートの統合機能を開発"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Run Full Test Suite and View Results (Priority: P1)

A developer wants to run the ANSI Common Lisp test suite against the Clysm compiler to understand overall compliance levels. They execute a single command and receive a summary showing pass/fail/skip counts for each test category (cons, numbers, strings, etc.) and the overall compliance percentage.

**Why this priority**: This is the core value proposition - measuring ANSI CL compliance. Without this, the feature has no purpose. Developers need to quickly assess how well Clysm implements the Common Lisp standard.

**Independent Test**: Can be fully tested by running the test suite command and verifying that results are displayed in a readable format with accurate pass/fail/skip counts.

**Acceptance Scenarios**:

1. **Given** the test suite is installed, **When** developer runs the full test suite, **Then** results are displayed showing pass/fail/skip counts per category
2. **Given** all tests have completed, **When** viewing the summary, **Then** overall compliance percentage is calculated and displayed
3. **Given** a test uses unsupported features, **When** that test runs, **Then** it is marked as SKIP with a reason rather than FAIL

---

### User Story 2 - Run Tests for Specific Category (Priority: P1)

A developer working on implementing number operations wants to run only the "numbers" category tests to validate their work. They specify the category name and only relevant tests execute, saving time and providing focused feedback.

**Why this priority**: Essential for iterative development. Developers implementing specific features need fast feedback without waiting for 20,000 tests to complete.

**Independent Test**: Can be fully tested by running tests for a single category and verifying only that category's tests execute.

**Acceptance Scenarios**:

1. **Given** the developer specifies category "cons", **When** tests run, **Then** only cons-related tests execute
2. **Given** a category is specified, **When** tests complete, **Then** results show only that category's pass/fail/skip counts
3. **Given** an invalid category name, **When** tests attempt to run, **Then** a helpful error message lists available categories

---

### User Story 3 - Generate Coverage Report (Priority: P2)

A developer wants to generate a Markdown report documenting the current compliance status. This report can be committed to the repository or shared with stakeholders to track progress over time.

**Why this priority**: Important for documentation and progress tracking, but the core testing functionality (P1) must work first.

**Independent Test**: Can be fully tested by generating a report file and verifying it contains properly formatted Markdown with all category results.

**Acceptance Scenarios**:

1. **Given** tests have completed, **When** report generation is requested, **Then** a Markdown file is created with category-by-category results
2. **Given** a report is generated, **When** viewing the file, **Then** it includes a summary table with pass rates for each category
3. **Given** previous reports exist, **When** comparing reports, **Then** compliance trends are visible (improved/regressed counts)

---

### User Story 4 - Automatic Skip of Unsupported Features (Priority: P2)

A developer running tests doesn't want to see failures for features that Clysm intentionally doesn't support yet. Tests using unsupported forms (like FORMAT or CLOS advanced features) should be automatically detected and skipped with a clear reason.

**Why this priority**: Critical for accurate compliance measurement. Without this, the pass rate would be artificially low and misleading.

**Independent Test**: Can be fully tested by running a test that uses FORMAT and verifying it is skipped rather than failed.

**Acceptance Scenarios**:

1. **Given** a test uses an unsupported form (e.g., FORMAT), **When** the test runs, **Then** it is marked SKIP with reason "unsupported form: FORMAT"
2. **Given** skip reasons are tracked, **When** viewing results, **Then** the most common skip reasons are summarized
3. **Given** a previously unsupported form is now implemented, **When** tests re-run, **Then** those tests now execute instead of skipping

---

### User Story 5 - CI Integration for Regression Detection (Priority: P3)

A CI system runs the test suite on every commit and compares results against a baseline. If the compliance rate drops or previously passing tests now fail, the build is flagged with a warning.

**Why this priority**: Valuable for maintaining quality over time, but requires the basic test execution (P1) and reporting (P2) to work first.

**Independent Test**: Can be fully tested by simulating a regression (breaking a feature) and verifying the comparison tool detects the change.

**Acceptance Scenarios**:

1. **Given** a baseline exists, **When** tests run, **Then** results are compared against baseline
2. **Given** pass rate drops by more than 5%, **When** comparison completes, **Then** a regression warning is issued
3. **Given** new tests start passing, **When** comparison completes, **Then** improvements are highlighted positively

---

### Edge Cases

- What happens when a test hangs or times out?
  - Test is marked SKIP with reason "timeout" after 30 seconds, and execution continues
- What happens when test results are ambiguous (e.g., floating point tolerance)?
  - Integer results are compared exactly; non-integer results are marked as "unverifiable" and skipped
- How does the system handle malformed test files in the external suite?
  - Malformed files are logged as errors and skipped; other tests continue executing
- What happens when the external test suite is updated?
  - The submodule is pinned to a specific commit; updates require explicit action

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST integrate an external ANSI Common Lisp test suite containing approximately 20,000 tests across 28 categories
- **FR-002**: System MUST execute tests by compiling test forms with Clysm and running via the existing execution infrastructure
- **FR-003**: System MUST classify each test result as PASS, FAIL, or SKIP with a reason
- **FR-004**: System MUST calculate and display pass rate per category and overall
- **FR-005**: System MUST automatically detect and skip tests using unsupported language forms
- **FR-006**: System MUST support running tests for a specific category by name
- **FR-007**: System MUST generate Markdown-formatted coverage reports
- **FR-008**: System MUST provide skip reason tracking (unsupported form, unsupported category, timeout, unverifiable result)
- **FR-009**: System MUST handle test timeouts gracefully without blocking execution
- **FR-010**: System MUST compare results against a baseline for regression detection

### Key Entities

- **TestCase**: Represents a single ANSI CL test with name, category, test form, and expected result
- **TestResult**: The outcome of running a TestCase - status (PASS/FAIL/SKIP), reason (for SKIP), actual result, execution time
- **CategoryResult**: Aggregated results for a category - pass/fail/skip counts, pass rate, duration
- **CoverageReport**: Collection of all CategoryResults with overall statistics and timestamp
- **SkipRegistry**: Configuration of which forms, categories, and individual tests to skip

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: cons category achieves at least 40% pass rate on initial implementation
- **SC-002**: numbers category achieves at least 30% pass rate on initial implementation
- **SC-003**: Full test suite (all 28 categories) completes execution within 30 minutes
- **SC-004**: Coverage report generation completes successfully and produces valid Markdown
- **SC-005**: All skipped tests have documented reasons (no unexplained skips)
- **SC-006**: Single category test run completes within 2 minutes
- **SC-007**: Regression detection correctly identifies when pass rate drops by 5% or more

## Assumptions

- The external ANSI CL test suite (pfdietz/ansi-test) is available and stable
- Tests using forms like FORMAT, PRINT, READ, OPEN, DEFGENERIC are expected to be skipped initially
- Integer comparison is sufficient for most test verification; complex object comparison is deferred
- The existing compile-and-run infrastructure is reliable for test execution
- Tests are deterministic and produce consistent results across runs
