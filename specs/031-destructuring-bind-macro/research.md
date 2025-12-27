# Research: Destructuring-Bind Macro

**Date**: 2025-12-27
**Feature**: 031-destructuring-bind-macro

## ANSI CL Destructuring Lambda-List Specification

### Decision: Follow ANSI CL Section 3.4.4 (Macro Lambda Lists)

**Rationale**: The destructuring lambda-list is defined in CLHS Section 3.4.4. It shares semantics with macro lambda-lists but without `&environment`. Following the standard ensures compatibility with existing Lisp code and the 9 compiler locations that use `destructuring-bind`.

**Alternatives considered**:
- Custom subset of lambda-list keywords → Rejected (breaks ANSI compliance)
- Full macro lambda-list with `&environment` → Deferred (not needed for self-hosting)

### Lambda-List Keyword Semantics

| Keyword | Semantics | Implementation |
|---------|-----------|----------------|
| `&whole` | Binds entire list before destructuring | Bind first, then destructure remainder |
| `&optional` | Optional parameters with defaults | `if (consp rest) ...` tests |
| `&rest` | Captures remaining elements as list | Direct assignment of `cdr` |
| `&body` | Synonym for `&rest` | Identical to `&rest` |
| `&key` | Keyword parameters from plist | `getf`-like plist traversal |
| `&allow-other-keys` | Suppress unknown key errors | Skip key validation |

## Existing Macro Pattern Analysis

### Decision: Use `make-*-expander` factory pattern

**Rationale**: All existing macros in `src/clysm/lib/macros.lisp` follow this pattern:
1. Factory function returns a lambda: `(defun make-FOO-expander () (lambda (form) ...))`
2. Lambda receives the full macro form including the macro name
3. Expansion returns pure Lisp forms (no side effects)
4. Registration via `install-standard-macros`

**Example from existing code**:
```lisp
(defun make-when-expander ()
  (lambda (form)
    (let ((test (second form))
          (body (cddr form)))
      (list 'if test (cons 'progn body) nil))))
```

**Alternatives considered**:
- Direct `defmacro` definition → Rejected (Clysm uses expander registry)
- AST-based expansion → Rejected (other macros use form manipulation)

## Lambda-List Parsing Strategy

### Decision: Two-phase parsing (parse structure, then generate code)

**Rationale**: Lambda-list parsing is complex with interdependent parts. Separating parsing from code generation:
1. Makes testing easier (parse can be unit-tested independently)
2. Enables future reuse for `defmacro` lambda-list processing
3. Matches SBCL/CMUCL internal patterns

**Implementation structure**:
```lisp
(defstruct parsed-lambda-list
  whole-var        ; Symbol or nil
  required-params  ; List of (var) or nested parsed-lambda-list
  optional-params  ; List of (var default supplied-p)
  rest-var         ; Symbol or nil
  key-params       ; List of ((keyword var) default supplied-p)
  allow-other-keys-p)

(defun parse-destructuring-lambda-list (lambda-list) ...)
(defun generate-destructuring-code (parsed-ll form-var) ...)
```

**Alternatives considered**:
- Single-pass expansion → Rejected (hard to maintain, error-prone)
- Full compiler AST transform → Rejected (overkill for macro)

## Error Handling Strategy

### Decision: Signal `program-error` for all malformed inputs

**Rationale**: ANSI CL specifies `program-error` for destructuring mismatches (CLHS 3.5.1.7). Using `program-error` (already in clysm/conditions) for:
- Insufficient list elements
- Excess elements without `&rest`
- Unknown keyword arguments without `&allow-other-keys`

**Code pattern**:
```lisp
;; At expansion time, generate runtime checks:
`(unless (consp ,list-var)
   (error 'program-error
          :format-control "Not enough elements for required parameter ~S"
          :format-arguments (list ',param-name)))
```

**Alternatives considered**:
- `error` with string → Rejected (less structured)
- Custom condition type → Rejected (ANSI specifies program-error)

## Nested Destructuring Strategy

### Decision: Recursive code generation with gensym'd temporaries

**Rationale**: Nested patterns like `((a b) (c d))` require:
1. Binding `(car list)` to a temporary
2. Recursively destructuring that temporary
3. Avoiding variable capture with `gensym`

**Pattern**:
```lisp
;; For pattern ((a b) c)
(let ((#:temp-0 (car list)))
  (let ((a (car #:temp-0))
        (b (cadr #:temp-0)))
    (let ((c (cadr list)))
      ...body...)))
```

**Alternatives considered**:
- Flat binding with complex accessors → Rejected (harder to debug)
- Single `let*` with all bindings → Rejected (scope issues with nested defaults)

## Compiler Self-Hosting Locations

### Analysis of 9 `destructuring-bind` usages:

| File | Line | Pattern Used | Notes |
|------|------|--------------|-------|
| ast.lisp | 361 | `(defclass-sym name supers slots &rest options)` | Basic + &rest |
| compiler.lisp | 543 | `(name kind index)` | Required only |
| compiler.lisp | 1235 | `(name kind index)` | Required only |
| macro.lisp | 401 | `(keyword var default)` | Required only |
| restarts.lisp | 125 | `(name lambda-list &body body-and-options)` | &body |
| restarts.lisp | 217 | `(name function &key ...)` | &key |
| handlers.lisp | 52 | `(type lambda-list &body body)` | &body |
| handlers.lisp | 99 | `(type function)` | Required only |
| sections.lisp | 338 | `(name kind index)` | Required only |

**Required features for self-hosting**:
- Required parameters (all 9 locations)
- `&rest` / `&body` (4 locations)
- `&key` (1 location)
- No `&optional` usage currently (but required for ANSI compliance)
- No `&whole` usage currently (but required for ANSI compliance)

## Implementation Phases

Based on research, recommended implementation order:

1. **Phase 1**: Required parameters + nested patterns (US1)
2. **Phase 2**: `&optional` with defaults and supplied-p (US2)
3. **Phase 3**: `&rest` and `&body` (US2)
4. **Phase 4**: `&key` with defaults and `&allow-other-keys` (US3)
5. **Phase 5**: `&whole` (US4)
6. **Phase 6**: Self-hosting validation (US5)

This order maximizes self-hosting capability early (Phases 1-3 cover 8/9 locations).
