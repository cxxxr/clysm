# Data Model: ANSI Common Lisp Test Suite Integration

**Feature**: 020-ansi-test | **Date**: 2025-12-25

## Overview

This document defines the core entities for the ANSI test harness. All entities are implemented as Common Lisp structures (DEFSTRUCT) for performance and simplicity.

---

## Entities

### 1. TestCase

Represents a single ANSI CL test parsed from the test suite.

```lisp
(defstruct test-case
  "A single test case from the ANSI test suite."
  (name nil :type symbol :read-only t)
  (category nil :type string :read-only t)
  (source-file nil :type (or null pathname) :read-only t)
  (form nil :type t :read-only t)
  (expected-values nil :type list :read-only t)
  (metadata nil :type list :read-only t))
```

| Field | Type | Description |
|-------|------|-------------|
| name | symbol | Unique test identifier (e.g., `CONS.1`) |
| category | string | Category name (e.g., "cons", "numbers") |
| source-file | pathname | Path to .lsp file containing the test |
| form | t | S-expression to compile and execute |
| expected-values | list | Expected return values (multiple-value aware) |
| metadata | list | Property list of DEFTEST attributes |

**Invariants**:
- `name` is unique across the entire test suite
- `form` is a valid Lisp S-expression
- `expected-values` is never empty (at least one expected value)

---

### 2. TestResult

The outcome of executing a TestCase.

```lisp
(defstruct test-result
  "Result of running a single test case."
  (test-case nil :type test-case :read-only t)
  (status nil :type (member :pass :fail :skip) :read-only t)
  (actual-values nil :type list)
  (skip-reason nil :type (or null string))
  (error-message nil :type (or null string))
  (execution-time-ms 0 :type (unsigned-byte 32)))
```

| Field | Type | Description |
|-------|------|-------------|
| test-case | test-case | Reference to the executed test |
| status | keyword | :PASS, :FAIL, or :SKIP |
| actual-values | list | Values returned by execution |
| skip-reason | string | Why test was skipped (nil if not skipped) |
| error-message | string | Error details if execution failed |
| execution-time-ms | integer | Execution duration in milliseconds |

**Status Transitions**:
```
TestCase ──compile──> :SKIP (compile-error)
         └──success──> Execute ──timeout──> :SKIP (timeout)
                       └──error──> :FAIL (runtime-error)
                       └──success──> Compare ──match──> :PASS
                                     └──no-match──> :FAIL
                                     └──unverifiable──> :SKIP
```

**Skip Reason Categories**:
- `"unsupported-form: FORMAT"` - Test uses unsupported form
- `"unsupported-category: files"` - Entire category skipped
- `"compile-error: ..."` - Compilation failed
- `"timeout"` - Execution exceeded time limit
- `"unverifiable"` - Result cannot be compared (non-fixnum)

---

### 3. CategoryResult

Aggregated results for a test category.

```lisp
(defstruct category-result
  "Aggregated results for a single category."
  (name nil :type string :read-only t)
  (total-count 0 :type (unsigned-byte 32))
  (pass-count 0 :type (unsigned-byte 32))
  (fail-count 0 :type (unsigned-byte 32))
  (skip-count 0 :type (unsigned-byte 32))
  (duration-ms 0 :type (unsigned-byte 32))
  (results nil :type list))
```

| Field | Type | Description |
|-------|------|-------------|
| name | string | Category name (e.g., "cons") |
| total-count | integer | Total tests in category |
| pass-count | integer | Tests that passed |
| fail-count | integer | Tests that failed |
| skip-count | integer | Tests that were skipped |
| duration-ms | integer | Total execution time |
| results | list | List of test-result structures |

**Derived Metrics**:
```lisp
(defun category-pass-rate (cr)
  "Return pass rate as a float between 0.0 and 1.0."
  (if (zerop (category-result-total-count cr))
      0.0
      (/ (category-result-pass-count cr)
         (category-result-total-count cr))))
```

---

### 4. CoverageReport

Collection of all category results with summary statistics.

```lisp
(defstruct coverage-report
  "Complete test suite execution report."
  (timestamp nil :type string :read-only t)
  (branch nil :type (or null string))
  (commit nil :type (or null string))
  (categories nil :type list)
  (summary nil :type report-summary))

(defstruct report-summary
  "Summary statistics for a coverage report."
  (total-count 0 :type (unsigned-byte 32))
  (pass-count 0 :type (unsigned-byte 32))
  (fail-count 0 :type (unsigned-byte 32))
  (skip-count 0 :type (unsigned-byte 32))
  (duration-ms 0 :type (unsigned-byte 32)))
```

| Field | Type | Description |
|-------|------|-------------|
| timestamp | string | ISO 8601 timestamp |
| branch | string | Git branch name |
| commit | string | Git commit hash |
| categories | list | List of category-result structures |
| summary | report-summary | Aggregated statistics |

---

### 5. SkipRegistry

Configuration for automatic skip detection.

```lisp
(defstruct skip-registry
  "Registry of forms and categories to skip."
  (unsupported-forms nil :type list)
  (unsupported-categories nil :type list)
  (skipped-tests nil :type list)
  (timeout-seconds 30 :type (unsigned-byte 16)))
```

| Field | Type | Description |
|-------|------|-------------|
| unsupported-forms | list | Symbols of unsupported special forms |
| unsupported-categories | list | Category names to skip entirely |
| skipped-tests | list | Specific test names to skip |
| timeout-seconds | integer | Per-test timeout (default: 30) |

**Default Registry**:
```lisp
(defparameter *default-skip-registry*
  (make-skip-registry
   :unsupported-forms
   '(;; I/O
     format print prin1 princ write pprint
     open close with-open-file with-open-stream
     read read-line read-char peek-char
     ;; CLOS
     defgeneric defmethod defclass
     make-instance slot-value slot-boundp
     change-class update-instance-for-redefined-class
     ;; System
     compile-file load require provide
     room describe inspect ed dribble)
   :unsupported-categories
   '("files" "streams" "pathnames" "printer" "reader")
   :skipped-tests nil
   :timeout-seconds 30))
```

---

### 6. BaselineComparison

Result of comparing current run against baseline.

```lisp
(defstruct baseline-comparison
  "Comparison between current results and baseline."
  (baseline-timestamp nil :type (or null string))
  (current-timestamp nil :type string :read-only t)
  (regressions nil :type list)
  (improvements nil :type list)
  (pass-rate-delta 0.0 :type single-float)
  (is-regression-p nil :type boolean))
```

| Field | Type | Description |
|-------|------|-------------|
| baseline-timestamp | string | When baseline was captured |
| current-timestamp | string | Current run timestamp |
| regressions | list | Tests that went from :PASS to :FAIL |
| improvements | list | Tests that went from :FAIL to :PASS |
| pass-rate-delta | float | Change in overall pass rate |
| is-regression-p | boolean | True if delta <= -5% (SC-007) |

---

## Relationships

```
                    ┌──────────────────┐
                    │  SkipRegistry    │
                    │ (configuration)  │
                    └────────┬─────────┘
                             │ checks
                             ▼
┌───────────┐     ┌──────────────────┐     ┌──────────────┐
│ TestCase  │────▶│   TestResult     │────▶│CategoryResult│
│(from file)│ 1:1 │ (execution)      │ N:1 │ (aggregated) │
└───────────┘     └──────────────────┘     └──────┬───────┘
                                                   │ N:1
                                                   ▼
                  ┌──────────────────┐     ┌──────────────┐
                  │BaselineComparison│◀────│CoverageReport│
                  │  (regression)    │     │  (summary)   │
                  └──────────────────┘     └──────────────┘
```

---

## Validation Rules

### TestCase Validation
1. `name` must be a non-nil symbol
2. `category` must match a known category directory
3. `form` must be parseable (not necessarily compilable)
4. `expected-values` must contain at least one value

### TestResult Validation
1. `status` must be one of :PASS, :FAIL, :SKIP
2. If `status` is :SKIP, `skip-reason` must be non-nil
3. If `status` is :PASS, `actual-values` must match `expected-values`
4. `execution-time-ms` must be non-negative

### CategoryResult Validation
1. `total-count` = `pass-count` + `fail-count` + `skip-count`
2. All counts must be non-negative
3. `results` list length must equal `total-count`

### CoverageReport Validation
1. `timestamp` must be valid ISO 8601
2. Summary counts must equal sum of category counts
3. All categories in `categories` list must have valid CategoryResult

---

## State Transitions

### Test Execution State Machine

```
              ┌─────────────┐
              │   PENDING   │
              └──────┬──────┘
                     │ start
                     ▼
              ┌─────────────┐
     skip ┌───│   LOADING   │───┐ parse error
     form │   └──────┬──────┘   │
          ▼          │ parsed   ▼
    ┌──────────┐     │    ┌──────────┐
    │  SKIP    │     │    │  SKIP    │
    │(form)    │     │    │(parse)   │
    └──────────┘     ▼    └──────────┘
              ┌─────────────┐
     compile  │  COMPILING  │ compile
     error ┌──┴──────┬──────┴──┐ error
           ▼         │         ▼
    ┌──────────┐     │  ┌──────────┐
    │  SKIP    │     │  │   FAIL   │
    │(compile) │     │  │(compile) │
    └──────────┘     ▼  └──────────┘
              ┌─────────────┐
     timeout  │  EXECUTING  │ runtime
    ┌─────────┴──────┬──────┴────────┐
    ▼                │               ▼
┌──────────┐         │        ┌──────────┐
│  SKIP    │         │        │   FAIL   │
│(timeout) │         ▼        │(runtime) │
└──────────┘  ┌─────────────┐ └──────────┘
              │  COMPARING  │
       ┌──────┴──────┬──────┴───────┐
       ▼             │              ▼
┌──────────┐         │       ┌──────────┐
│  SKIP    │         │       │   FAIL   │
│(unverify)│         ▼       │(mismatch)│
└──────────┘  ┌─────────────┐└──────────┘
              │    PASS     │
              └─────────────┘
```

---

## Serialization

### JSON Format (for baselines)

```json
{
  "version": "1.0",
  "timestamp": "2025-12-25T10:30:00Z",
  "branch": "main",
  "commit": "abc123def456",
  "summary": {
    "total": 20156,
    "passed": 2847,
    "failed": 1203,
    "skipped": 16106,
    "duration_ms": 923000
  },
  "categories": {
    "cons": {
      "total": 1024,
      "passed": 412,
      "failed": 87,
      "skipped": 525,
      "duration_ms": 45000,
      "passed_tests": ["cons-of-symbols", "cons.order.1"],
      "failed_tests": ["cons.error.1"],
      "skipped_tests": ["cons.print.1"]
    }
  },
  "skip_reasons": {
    "unsupported-form: FORMAT": 8203,
    "unsupported-form: PRINT": 2102,
    "unsupported-category: files": 1234,
    "timeout": 42,
    "unverifiable": 833
  }
}
```

### Markdown Format (for reports)

See research.md Section 6 for detailed report format specification.
