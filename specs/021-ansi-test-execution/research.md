# Research: ANSI Test Execution

**Feature**: 021-ansi-test-execution
**Date**: 2025-12-25

## Executive Summary

Analysis of ANSI test suite patterns reveals that most tests use complex language constructs unsupported by Clysm. However, by expanding the skip registry and improving result comparison, we can achieve measurable pass rates for tests using simple, directly compilable forms.

## Research Questions

### R1: What ANSI test patterns can Clysm currently compile?

**Investigation**: Analyzed sample tests from cons and numbers categories.

**Findings**:

1. **Simple literal forms compile successfully**:
   - `(+ 1 2)` → 3
   - `(car '(a b))` → symbol
   - `(cons 1 2)` → cons cell
   - `(null nil)` → T (true)

2. **Quote forms are mostly supported**:
   - `'(a b c)` → list
   - `'a` → symbol

3. **Many tests use unsupported constructs**:
   - `loop` - Not implemented
   - `let` - Partially implemented
   - `defvar` / `*universe*` - Global variables
   - `macrolet` - Not implemented
   - `signals-error` - ANSI test macro
   - `incf` - Side-effect macro
   - `expand-in-current-env` - ANSI test macro

**Decision**: Focus on tests with simple S-expression patterns that compile directly.

**Rationale**: The compiler supports basic operations; complex control flow is beyond current scope.

**Alternatives considered**:
- Implementing more forms (too much scope creep)
- Modifying tests (violates test integrity)

---

### R2: Which unsupported forms cause the most skips?

**Investigation**: Analyzed current skip-registry and sample test files.

**Findings**:

Current skip-registry includes:
- I/O forms: `format`, `print`, `read`, etc.
- CLOS forms: `defgeneric`, `defmethod`, `defclass`, etc.
- System forms: `compile-file`, `load`, etc.

**Missing from skip-registry (should be added)**:
- `loop` - Used in 60%+ of tests
- `let` / `let*` - Used in 40%+ of tests
- `defvar` / `defparameter` - Used for test fixtures
- `flet` / `labels` - Local functions
- `macrolet` / `symbol-macrolet` - Local macros
- `multiple-value-bind` / `values` - MV handling
- `progn` - Sequencing (partially supported)
- `signals-error` / `handler-case` - Error testing
- `eql` / `eqt` / `equalt` - ANSI test equality macros
- `*universe*` / `*numbers*` - ANSI test globals
- `random` - Not implemented

**Decision**: Expand skip-registry to include common unsupported forms.

**Rationale**: Proper skip classification provides accurate metrics.

---

### R3: How should result comparison work for different types?

**Investigation**: Analyzed classifier.lisp and wasmtime output format.

**Findings**:

Current classifier (`compare-values`) handles:
- NIL sentinel: `-2147483648` (MIN_INT32)
- Non-fixnum sentinel: `-2147483647` (MIN_INT32 + 1)
- Fixnum comparison: exact integer match
- Boolean: "true"/"false" string matching

**Issues identified**:
1. **T vs true**: wasmtime prints `true` for T, but classifier expects `(eq expected t)` + `"true"` match
2. **Symbol expected values**: Most tests expect symbols like `a`, `b`, `c` which can't be verified
3. **Cons expected values**: Tests like `(cons 'a 'b)` expect `(a . b)` which can't be verified
4. **Multiple values**: Tests with MV returns can't be verified

**Decision**:
1. Mark tests expecting symbols (other than T/NIL) as "unverifiable"
2. Mark tests expecting cons cells as "unverifiable"
3. Verify only: fixnums, T, NIL
4. Handle T correctly in boolean comparison

**Rationale**: Accurate classification prevents false failures.

---

### R4: How do we identify compilable tests efficiently?

**Investigation**: Analyzed test form structure.

**Findings**:

**Compilable patterns**:
```lisp
;; Direct evaluation of simple expression
(deftest foo (+ 1 2) 3)           ; ✓ compilable
(deftest bar (null nil) t)         ; ✓ compilable
(deftest baz (car '(a b)) a)       ; △ compiles but result unverifiable

;; With quote forms
(deftest qux (cons 'a 'b) (a . b)) ; △ compiles but result unverifiable
```

**Non-compilable patterns**:
```lisp
(deftest foo (loop ...) nil)       ; ✗ loop unsupported
(deftest bar (let ((x 1)) ...) 2)  ; ✗ let unsupported
(deftest baz (incf x) 1)           ; ✗ incf unsupported
```

**Detection strategy**:
Pre-scan form for unsupported constructs before compilation attempt.

**Decision**: Add `form-complexity-check` function to loader that detects unsupported forms before compilation.

**Rationale**: Fail-fast with proper skip reason rather than compilation error.

---

### R5: What pass rates are realistically achievable?

**Investigation**: Analyzed test categories and form complexity.

**Findings**:

| Category | Total Tests | Simple Forms | Estimated Passable |
|----------|-------------|--------------|-------------------|
| numbers | ~2000 | ~200 (10%) | 100-200 (5-10%) |
| cons | ~1600 | ~80 (5%) | 40-80 (2.5-5%) |
| data-and-control-flow | ~1000 | ~50 (5%) | 25-50 (2.5-5%) |

**Key factors**:
1. Many "simple" tests still expect symbol results (unverifiable)
2. Some tests use ANSI test macros like `eqt`, `equalt`
3. Quote forms with nested structures may fail compilation

**Decision**: Set conservative targets:
- numbers: 10% (with tolerance for fixnum-only tests)
- cons: 5% (limited by unverifiable results)

**Rationale**: Achievable targets provide clear progress measurement.

---

## Implementation Recommendations

### Phase 1: Skip Registry Expansion

Add to `*default-skip-registry*`:
```lisp
;; Control flow (not implemented)
loop for while do do* dotimes dolist
let let* flet labels

;; Macros (not implemented)
macrolet symbol-macrolet

;; Multiple values
multiple-value-bind multiple-value-list values

;; Side effects
incf decf push pop setf setq

;; Error handling
handler-case handler-bind ignore-errors

;; ANSI test framework
signals-error signals-type-error classify-error
eqt equalt notnot expand-in-current-env
*universe* *numbers* *cons-test-*
```

### Phase 2: Classifier Improvements

1. Fix T/NIL comparison:
   - wasmtime outputs `0` for fixnum 0, empty string for NIL-as-integer
   - Handle `"true"` → T matching correctly

2. Add expected-value analysis:
   - If expected is symbol (not T/NIL) → unverifiable
   - If expected is cons → unverifiable
   - If expected is list of symbols → unverifiable

### Phase 3: Form Complexity Analysis

Add to loader.lisp:
```lisp
(defun form-uses-unsupported-p (form registry)
  "Check if form uses any unsupported constructs."
  ;; Already exists: contains-unsupported-form-p
  ;; Extend to detect more patterns
  )
```

### Phase 4: Integration Testing

Run actual ANSI tests and verify:
1. `(run-ansi-tests :category "numbers")` shows measurable passes
2. Skip reasons are accurate and actionable
3. No false failures (tests that should pass but fail)

---

## Decisions Summary

| Decision | Choice | Impact |
|----------|--------|--------|
| Skip registry expansion | Add ~30 common unsupported forms | Higher accuracy in skip classification |
| Result comparison | Fixnum/T/NIL only; symbols → unverifiable | No false failures |
| Form pre-analysis | Detect unsupported forms before compile | Better skip reasons |
| Pass rate targets | 10% numbers, 5% cons | Achievable, measurable goals |
