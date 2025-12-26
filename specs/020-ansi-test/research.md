# Research: ANSI Common Lisp Test Suite Integration

**Feature**: 020-ansi-test | **Date**: 2025-12-25

## Executive Summary

This document consolidates research findings for integrating the pfdietz/ansi-test suite into Clysm. The suite provides ~20,000 tests across 33 categories using the RT (Regression Testing) framework. Key decisions cover test format parsing, skip detection strategy, and execution architecture.

---

## 1. ANSI-Test Suite Format

### Decision: Use RT Framework's DEFTEST Format

**Rationale**: The pfdietz/ansi-test suite uses a well-defined DEFTEST macro from the RT framework. Each test has a predictable structure that can be parsed without executing the RT framework itself.

**DEFTEST Syntax**:
```lisp
(deftest <test-name>
  <test-body>
  <expected-result>*)
```

**Examples**:
```lisp
;; Simple test - evaluates (cons 'a 'b), expects (a . b)
(deftest cons-of-symbols
  (cons 'a 'b)
  (a . b))

;; Multiple values test
(deftest cons.order.1
  (let ((i 0))
    (values (cons (incf i) (incf i)) i))
  (1 . 2) 2)

;; Error condition test
(deftest cons.error.1
  (signals-error (cons) program-error)
  t)
```

**Alternatives Considered**:
- Load and execute RT framework → Rejected: Requires implementing RT runtime, heavyweight
- Only parse simple tests → Rejected: Loses coverage of complex multi-value tests
- Convert to Rove format → Rejected: Significant effort, loses upstream compatibility

---

## 2. Test File Organization

### Decision: Parse Tests from Category Directories

**Rationale**: The ansi-test suite organizes tests by ANSI CL specification section. Each category has a dedicated directory with one or more .lsp files.

**Directory Structure** (33 categories):
```
ansi-test/
├── arrays/           # Chapter 15: Arrays
├── characters/       # Chapter 13: Characters
├── cons/             # Chapter 14: Conses
├── conditions/       # Chapter 9: Conditions
├── data-and-control-flow/  # Chapter 5: Data/Control Flow
├── eval-and-compile/ # Chapter 3: Evaluation/Compilation
├── files/            # Chapter 20: Files
├── hash-tables/      # Chapter 18: Hash Tables
├── iteration/        # Chapter 6: Iteration
├── numbers/          # Chapter 12: Numbers
├── objects/          # Chapter 7: Objects (CLOS)
├── packages/         # Chapter 11: Packages
├── pathnames/        # Chapter 19: Pathnames
├── printer/          # Chapter 22: Printer
├── reader/           # Chapter 23: Reader
├── sequences/        # Chapter 17: Sequences
├── streams/          # Chapter 21: Streams
├── strings/          # Chapter 16: Strings
├── structures/       # Chapter 8: Structures
├── symbols/          # Chapter 10: Symbols
├── types-and-classes/  # Chapter 4: Types/Classes
├── environment/      # Chapter 25: Environment
├── system-construction/  # Chapter 24: System Construction
├── auxiliary/        # Test infrastructure
├── beyond-ansi/      # Extension tests
├── bugs/             # Regression tests
├── doc/              # Documentation
├── misc/             # Miscellaneous
├── random/           # Random test generation
├── rctest/           # Additional tests
├── sandbox/          # Experimental
├── rt.lsp            # RT framework
└── doit.lsp          # Test runner
```

**Priority Categories for Initial Implementation**:
1. cons - Core list operations (high likelihood of support)
2. numbers - Numeric tower (partially implemented)
3. sequences - Sequence operations (basic support)
4. symbols - Symbol operations (core infrastructure)
5. characters - Character operations (basic support)

---

## 3. Skip Detection Strategy

### Decision: Multi-Layer Skip Detection with Pre-Scan

**Rationale**: Different skip reasons require different detection approaches. A layered system provides accurate classification and actionable feedback.

**Skip Layers**:

| Layer | When Detected | Examples | Implementation |
|-------|---------------|----------|----------------|
| 1. Form-Level | Pre-execution scan | FORMAT, PRINT, OPEN, DEFGENERIC | AST pattern matching |
| 2. Category-Level | Configuration | pathnames, files, streams | Skip registry file |
| 3. Compile-Time | During compilation | Unimplemented special forms | Catch condition |
| 4. Runtime | During execution | Timeout, unverifiable result | Timeout handler |

**Form-Level Skip List** (initial):
```lisp
;; Unsupported I/O forms
FORMAT PRINT PRIN1 PRINC WRITE PPRINT
OPEN CLOSE WITH-OPEN-FILE WITH-OPEN-STREAM
READ READ-LINE READ-CHAR PEEK-CHAR

;; Unsupported CLOS forms
DEFGENERIC DEFMETHOD DEFCLASS
MAKE-INSTANCE SLOT-VALUE SLOT-BOUNDP
CHANGE-CLASS UPDATE-INSTANCE-FOR-REDEFINED-CLASS

;; Unsupported system forms
COMPILE-FILE LOAD REQUIRE PROVIDE
ROOM DESCRIBE INSPECT ED DRIBBLE
```

**Alternatives Considered**:
- Blacklist individual tests → Rejected: Unmaintainable at scale
- Skip only on compile error → Rejected: Misses pre-known unsupported forms
- Whitelist supported forms → Rejected: Too restrictive, underestimates coverage

---

## 4. Test Execution Architecture

### Decision: Streaming Execution with Per-Test Isolation

**Rationale**: 20,000 tests require streaming execution to manage memory and provide progress feedback. Per-test isolation prevents cascading failures.

**Execution Pipeline**:
```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Loader    │───▶│  Classifier │───▶│   Runner    │───▶│  Reporter   │
│ (parse .lsp)│    │ (skip check)│    │ (compile+   │    │ (aggregate) │
│             │    │             │    │  execute)   │    │             │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
      │                  │                  │                  │
      ▼                  ▼                  ▼                  ▼
 TestCase list     SKIP with reason   PASS/FAIL/SKIP    CategoryResult
```

**Isolation Strategy**:
- Each test compiled to separate Wasm module
- Fresh wasmtime invocation per test
- 30-second timeout per test (configurable)
- Errors caught and recorded, not propagated

**Alternatives Considered**:
- Batch compilation → Rejected: One bad test breaks batch
- Shared Wasm instance → Rejected: State pollution between tests
- Host-side evaluation → Rejected: Not testing Clysm compilation

---

## 5. Result Comparison

### Decision: Type-Aware Equality with Sentinel Detection

**Rationale**: Clysm's Wasm output uses sentinels for non-fixnum values. Comparison must handle this and provide meaningful failure messages.

**Comparison Rules**:

| Clysm Output | Interpretation | Comparison |
|--------------|----------------|------------|
| Integer N | Fixnum value | Exact match with expected |
| -2147483648 | NIL | Match expected NIL |
| -2147483647 | Non-fixnum | SKIP (unverifiable) |
| "true"/"false" | Boolean T/NIL | Map to Lisp values |
| Timeout | No result | SKIP with timeout reason |
| Error exit | Runtime error | FAIL with error message |

**Expected Value Parsing**:
- Quoted atoms: `'foo` → symbol FOO
- Dotted pairs: `(a . b)` → cons cell
- Multiple values: `1 2` → (values 1 2)
- Complex numbers: `#C(1 2)` → (complex 1 2)

**Alternatives Considered**:
- String comparison → Rejected: Loses type information
- Execute expected in host Lisp → Considered: Good for complex values
- Mark all non-fixnum as SKIP → Implemented: Practical for initial version

---

## 6. Report Format

### Decision: Category-Grouped Markdown with Summary Statistics

**Rationale**: Markdown is human-readable, version-controllable, and renders well on GitHub. Category grouping matches ANSI CL specification structure.

**Report Structure**:
```markdown
# Clysm ANSI CL Compliance Report

Generated: 2025-12-25 | Branch: 020-ansi-test | Duration: 15m 23s

## Summary

| Metric | Value |
|--------|-------|
| Total Tests | 20,156 |
| Passed | 2,847 |
| Failed | 1,203 |
| Skipped | 16,106 |
| **Pass Rate** | **14.1%** |

## Categories

### cons (passed: 412/1,024 = 40.2%)

<details>
<summary>Failed tests (87)</summary>

| Test | Expected | Actual | Reason |
|------|----------|--------|--------|
| cons.1 | (A . B) | error | compile-error |
...
</details>

### numbers (passed: 189/2,341 = 8.1%)
...

## Skip Reasons Summary

| Reason | Count | Top Forms |
|--------|-------|-----------|
| unsupported-form | 12,341 | FORMAT (8,203), PRINT (2,102) |
| unsupported-category | 2,890 | files, streams |
| timeout | 42 | |
| unverifiable | 833 | non-fixnum results |
```

**Alternatives Considered**:
- JSON only → Rejected: Not human-readable
- JUnit XML → Rejected: Overkill, not standard in Lisp ecosystem
- Plain text → Rejected: Harder to parse and render

---

## 7. Regression Detection

### Decision: JSON Baseline with Pass/Fail Delta Comparison

**Rationale**: JSON baselines are easy to parse, diff, and version control. Delta comparison highlights regressions and improvements.

**Baseline Format**:
```json
{
  "timestamp": "2025-12-25T10:30:00Z",
  "branch": "main",
  "commit": "abc123",
  "summary": {
    "total": 20156,
    "passed": 2847,
    "failed": 1203,
    "skipped": 16106
  },
  "categories": {
    "cons": {
      "passed": ["cons-of-symbols", "cons.order.1", ...],
      "failed": ["cons.error.1", ...],
      "skipped": [...]
    },
    ...
  }
}
```

**Regression Criteria** (from spec SC-007):
- Pass rate drop ≥ 5% → WARNING
- Any previously passing test now fails → REGRESSION
- New tests passing → IMPROVEMENT

**Alternatives Considered**:
- Store only counts → Rejected: Can't identify specific regressions
- Store full results → Rejected: Too large, slow comparison
- Binary format → Rejected: Not human-inspectable

---

## 8. CI Integration

### Decision: GitHub Actions with Nix Environment

**Rationale**: GitHub Actions is the de facto CI for GitHub projects. Nix provides reproducible environment with all required tools.

**CI Workflow**:
```yaml
name: ANSI Compliance
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          submodules: recursive
      - uses: cachix/install-nix-action@v27
      - run: nix develop --command make ansi-test
      - run: nix develop --command make ansi-compare
      - uses: actions/upload-artifact@v4
        with:
          name: compliance-report
          path: baselines/current.json
```

**Baseline Update Strategy**:
- Automatic on main branch merge
- Manual approval for baseline updates on PRs
- Store baseline in baselines/current.json

---

## 9. Performance Targets

### Decision: Parallel Execution with Configurable Concurrency

**Rationale**: Serial execution of 20,000 tests would take hours. Parallel execution with worker pool achieves 30-minute target.

**Performance Analysis**:
- Average test time: ~100ms (compile + execute)
- Serial execution: 20,000 × 100ms = 33 minutes (exceeds SC-003)
- Parallel (4 workers): ~8 minutes
- Parallel (8 workers): ~4 minutes

**Parallelization Strategy**:
- Use SBCL's `lparallel` or `bordeaux-threads`
- Worker pool with configurable size (default: CPU cores)
- Thread-safe result accumulation
- Per-test file isolation (no shared state)

**Alternatives Considered**:
- Process-level parallelism → Rejected: Higher overhead than threads
- Async/await pattern → Rejected: Not idiomatic in Common Lisp
- No parallelism → Rejected: Fails SC-003 performance target

---

## 10. Dependencies & Integration

### Decision: Minimal New Dependencies

**Rationale**: Clysm already has most required infrastructure. Minimize new dependencies to reduce complexity.

**Existing Dependencies (reused)**:
- alexandria - Utility functions
- uiop - Process execution (wasmtime invocation)
- rove - Test framework (for harness tests)
- clysm/compiler - Wasm compilation

**New Dependencies**:
- cl-json (optional) - JSON serialization for baselines
- lparallel - Parallel execution (if needed)

**No External Dependencies**:
- Test file parsing - Use CL reader
- Markdown generation - String formatting
- Skip detection - Pattern matching on parsed forms

---

## References

- [pfdietz/ansi-test](https://github.com/pfdietz/ansi-test) - Primary test suite
- [sbcl/ansi-test](https://github.com/sbcl/ansi-test) - SBCL fork with notes
- [RT Framework](https://github.com/pfdietz/ansi-test/blob/master/rt.lsp) - Test infrastructure
- [GCL ANSI Test Suite Paper](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=60a297f9c89d1bc6f0ab8bf3dbb8a8a0ee895efa) - Academic reference
