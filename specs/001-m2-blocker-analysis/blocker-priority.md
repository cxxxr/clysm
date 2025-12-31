# Blocker Priority Analysis: Phase 13D Milestone M2

**Generated**: 2025-12-31
**Baseline**: 13.90% (3,631/26,571 forms)
**Target**: 25%+ (6,643+ forms)

## Summary by Blocker Type

| Priority | Blocker | Count | Expected Impact | Effort |
|----------|---------|-------|-----------------|--------|
| 1 | [DEFMACRO](../../resources/HyperSpec/Body/m_defmac.htm) skip | 646 | +2.4% (count adjustment) | Low |
| 2 | [DEFSTRUCT](../../resources/HyperSpec/Body/m_defstr.htm) expansion | 1,953 | +7.3% | Medium |
| 3 | [DEFINE-CONDITION](../../resources/HyperSpec/Body/m_defi_5.htm) expansion | 302 | +1.1% | Medium |
| 4 | [DEFVAR](../../resources/HyperSpec/Body/m_defvar.htm) completion | 133 | +0.5% | Low |
| 5 | DEFUN body issues | 19,084 | Variable | High (deferred) |

**Total Addressable**: ~11.3% improvement → 25.2% achievable

## Blocker 1: DEFMACRO (646 forms)

**Category**: Compile-time only construct
**Impact**: +2.4% effective coverage (rate recalculation)
**Priority**: HIGH (quick win)

### Analysis

DEFMACRO forms define macros that are expanded at compile time by the host (SBCL).
They should not be compiled to Wasm - they should be skipped.

### Current State

- Forms counted as "failed" in Stage 1 report
- No Wasm code generated (correctly)
- Inflates failure count artificially

### Fix Strategy

Mark DEFMACRO as :skipped in Stage 1 classification:
```lisp
;; In src/clysm/stage1/compiler.lisp
(when (eq operator 'defmacro)
  (return-from classify-form :skipped))
```

### Verification

After fix:
- DEFMACRO removed from top_blockers
- Skipped count increases by ~646
- Coverage calculation uses: compiled / (total - skipped)

---

## Blocker 2: DEFSTRUCT (1,953 forms)

**Category**: Structure definition macro
**Impact**: +7.3% (largest single blocker fix)
**Priority**: HIGH

### Analysis

DEFSTRUCT forms expand to DEFCLASS + accessor functions. Feature 001-defstruct-wasm-compile
implements this expansion, but Stage 1 may not be triggering it correctly.

### Current State

Looking at the report, DEFSTRUCT shows as failed. Need to verify:
1. Is the expansion happening in parse-compound-form?
2. Are the expanded DEFCLASS forms compiling?
3. Are accessor DEFUNs generated and compiling?

### Fix Strategy

1. Verify DEFSTRUCT → DEFCLASS expansion in `src/clysm/compiler/ast.lisp`
2. Ensure expansion runs before Stage 1 classification
3. Validate accessor function generation

### Verification

After fix:
- DEFSTRUCT removed from top_blockers (or count significantly reduced)
- Compiled count increases by up to 1,953

---

## Blocker 3: DEFINE-CONDITION (302 forms)

**Category**: Condition type definition
**Impact**: +1.1%
**Priority**: MEDIUM

### Analysis

DEFINE-CONDITION defines condition types for the Common Lisp condition system.
Like DEFSTRUCT, it should expand to DEFCLASS.

### Current State

Forms fail with: "Unknown compound form: DEFINE-CONDITION"

### Fix Strategy

Add DEFINE-CONDITION case in `parse-compound-form` (src/clysm/compiler/ast.lisp):
```lisp
(define-condition
  (parse-define-condition-as-defclass form env))
```

### Implementation

```lisp
(defun parse-define-condition-as-defclass (form env)
  "Convert DEFINE-CONDITION to DEFCLASS for compilation."
  (destructuring-bind (name parent-types &rest options) (cdr form)
    (let* ((slots (cdr (assoc :slots options)))
           (report (cdr (assoc :report options)))
           (defclass-form `(defclass ,name ,(or parent-types '(condition))
                            ,slots)))
      (parse-compound-form defclass-form env))))
```

### Verification

After fix:
- DEFINE-CONDITION removed from top_blockers
- Condition types compile as DEFCLASS

---

## Blocker 4: DEFVAR (133 forms)

**Category**: Global variable definition
**Impact**: +0.5%
**Priority**: MEDIUM

### Analysis

DEFVAR defines global (special) variables. These need:
1. Global variable declaration in Wasm
2. Initial value compilation
3. Symbol table registration

### Current State

Partial support exists. Some DEFVARs may work, others fail on:
- Complex initial value forms
- Dependency on undefined functions

### Fix Strategy

1. Verify global emission in `src/clysm/compiler/codegen/globals.lisp`
2. Handle NIL initial value case
3. Ensure symbol registration

### Verification

After fix:
- DEFVAR forms compile or are marked correctly

---

## Blocker 5: DEFUN Body Issues (19,084 forms)

**Category**: Function definition failures
**Impact**: Variable (requires analyzing each failure type)
**Priority**: DEFERRED to future milestone

### Analysis

DEFUN failures come from various body issues:

1. **Type mismatch errors** (most common)
   - `type mismatch: expected anyref, found i32`
   - Primitives returning wrong Wasm types

2. **Undefined function errors**
   - Functions used in body but not yet compiled
   - Ordering dependency issues

3. **Complex lambda-list features**
   - &aux, &key with defaults
   - Destructuring patterns

### Observed Failures (Sample)

| Function | Error Type | Cause |
|----------|------------|-------|
| encode-unsigned-leb128 | type mismatch | i32 vs anyref |
| encode-section | undefined function | SECTION-CONTENT |
| validate-section-order | slot type error | Symbol expected |
| encode-vec | undefined function | ENCODE-UNSIGNED-LEB128 |
| encode-name | undefined function | STRING-TO-UTF8-OCTETS |

### Deferred Strategy

These require comprehensive primitive and function ordering fixes:
1. Fix primitive type annotations
2. Implement topological sort for function dependencies
3. Add missing library functions

**Target**: Future milestone (M3+)

---

## Recommended Fix Order

Based on effort/impact ratio:

1. **DEFMACRO skip** - Easiest, immediate coverage improvement
2. **DEFSTRUCT verification** - Highest impact, existing code may work
3. **DEFINE-CONDITION** - Medium effort, leverages CLOS infrastructure
4. **DEFVAR completion** - Low effort, incremental improvement

---

## Success Metrics

| Checkpoint | Expected Coverage | Forms Compiled |
|------------|------------------|----------------|
| Baseline | 13.90% | 3,631 |
| After DEFMACRO skip | 16.3%* | 3,631 (adjusted) |
| After DEFSTRUCT | 23.6% | 5,584 |
| After DEFINE-CONDITION | 24.7% | 5,886 |
| After DEFVAR | 25.2% | 6,019 |

*Coverage formula changes to: compiled / (total - skipped)

---

## Dependencies

```
DEFMACRO skip → (independent)
DEFSTRUCT fix → may unlock DEFUN forms using structures
DEFINE-CONDITION fix → depends on DEFCLASS infrastructure
DEFVAR fix → may unlock DEFUN forms using globals
```

No circular dependencies between fixes. Can be applied incrementally.
