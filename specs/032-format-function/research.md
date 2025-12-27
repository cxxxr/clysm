# Research: FORMAT Function Foundation

**Feature**: 032-format-function
**Date**: 2025-12-27

## Existing Implementation Analysis

### Current State in `src/clysm/streams/format.lisp`

**Implemented Directives**:
- ~A (aesthetic) - calls `princ-to-string`
- ~S (standard) - calls `prin1-to-string`
- ~D (decimal) - integer printing with type check
- ~% (newline) - outputs newline character
- ~~ (tilde) - outputs literal tilde

**Implementation Pattern**:
- `format-directive` struct: type, start, end positions
- `format-string-info` struct: control-string, directives list
- `parse-format-string`: Linear scan, builds directive list
- `format`: Iterates directives, outputs literals between them

**Current Limitations**:
1. No nested directive support (no ~{~}, ~[~])
2. No column tracking (needed for ~&)
3. Uses `cl:error` instead of `format-error` condition
4. Case handling is explicit per-character (not systematic)

---

## Research Findings

### Decision 1: Nested Directive Parsing Strategy

**Decision**: Recursive descent parser with balanced delimiter tracking

**Rationale**:
- ~{format~} and ~[clause~;clause~] require nested parsing
- Linear scan in current implementation cannot handle balanced delimiters
- Recursive descent naturally handles nesting up to arbitrary depth

**Alternatives Considered**:
1. **State machine**: More complex to implement and debug
2. **Regular expressions**: Cannot handle nested structures
3. **Full LALR parser**: Overkill for format string grammar

**Implementation Approach**:
```lisp
;; Parse directive returns (directive . remaining-position)
(defun parse-directive (control-string start)
  "Parse a single directive starting at START position."
  (let ((char (char-upcase (char control-string (1+ start)))))
    (case char
      (#\{ (parse-iteration-directive control-string start))
      (#\[ (parse-conditional-directive control-string start))
      (t   (parse-simple-directive control-string start)))))
```

---

### Decision 2: Column Tracking for ~&

**Decision**: Per-stream column slot with simple heuristic

**Rationale**:
- ~& (fresh-line) must know if at column 0
- Stream objects can carry column state
- Simple rule: column 0 after newline, non-zero otherwise

**Alternatives Considered**:
1. **Global column variable**: Breaks with multiple streams
2. **Track every character**: Performance overhead, complex
3. **No tracking, always newline**: Violates ANSI semantics

**Implementation Approach**:
- Add `column` slot to stream struct (or use existing if present)
- Update column on `write-char`: 0 after newline, else increment
- `fresh-line` checks column = 0 before outputting newline

**Note**: For string output (nil destination), track column in local variable.

---

### Decision 3: Format-Error Condition Type

**Decision**: Define `format-error` as subtype of `simple-error` in clysm/conditions

**Rationale**:
- ANSI CL specifies FORMAT signals format-related errors
- Allows handlers to catch FORMAT-specific errors
- Integrates with existing condition system

**Alternatives Considered**:
1. **Use cl:error directly**: Loses error type specificity
2. **New condition hierarchy**: Overkill for single condition

**Implementation**:
```lisp
(define-condition format-error (simple-error)
  ((control-string :initarg :control-string :reader format-error-control-string)
   (position :initarg :position :reader format-error-position))
  (:report (lambda (condition stream)
             (format stream "Format error in ~S at position ~D: ~A"
                     (format-error-control-string condition)
                     (format-error-position condition)
                     (simple-condition-format-control condition)))))
```

---

### Decision 4: Iteration Directive (~{~}) Implementation

**Decision**: Process list elements with recursive format call

**Rationale**:
- ~{format~} applies format to each list element
- ~^ exits when no more elements remain
- Recursive call reuses existing format infrastructure

**Alternatives Considered**:
1. **Inline expansion**: Code duplication, harder to maintain
2. **New execution context**: Unnecessary complexity

**Implementation Approach**:
```lisp
(defstruct (iteration-directive (:include format-directive))
  (body-directives nil :type list)   ; nested directives
  (body-string "" :type string))     ; substring for ~?

;; During execution:
(defun execute-iteration (directive args stream arg-index)
  (let ((list-arg (nth arg-index args)))
    (unless (listp list-arg)
      (signal-format-error "~{ requires a list argument"))
    (loop for item in list-arg
          for first = t then nil
          do (format-body (iteration-directive-body-directives directive)
                          (list item)
                          stream))))
```

---

### Decision 5: Conditional Directive (~[~]) Implementation

**Decision**: Index-based clause selection with default clause support

**Rationale**:
- ~[clause0~;clause1~;...~] selects by integer index
- ~:[false~;true~] is boolean variant (nil = 0, non-nil = 1)
- ~:; marks default clause (used when index out of range)

**Alternatives Considered**:
1. **Compile to nested IF**: Loses format semantics
2. **Macro expansion**: Not applicable (runtime evaluation)

**Implementation Approach**:
```lisp
(defstruct (conditional-directive (:include format-directive))
  (clauses nil :type list)           ; list of clause-info
  (boolean-p nil :type boolean)      ; ~: modifier present
  (default-index nil :type (or null fixnum)))  ; index of ~:; clause

(defun execute-conditional (directive args stream arg-index)
  (let* ((arg (nth arg-index args))
         (index (if (conditional-directive-boolean-p directive)
                    (if arg 1 0)
                    (unless (integerp arg)
                      (signal-format-error "~[ requires integer argument"))
                    arg))
         (clause (or (nth index (conditional-directive-clauses directive))
                     (and (conditional-directive-default-index directive)
                          (nth (conditional-directive-default-index directive)
                               (conditional-directive-clauses directive))))))
    (when clause
      (format-body (clause-directives clause) args stream))))
```

---

### Decision 6: Recursive Directive (~?) Implementation

**Decision**: Consume two arguments (format-string, args-list), call format recursively

**Rationale**:
- ~? takes next arg as format string, following arg as argument list
- Natural implementation: recursive format call
- Supports composable format strings

**Implementation Approach**:
```lisp
(defun execute-recursive (args stream arg-index)
  (let ((sub-control (nth arg-index args))
        (sub-args (nth (1+ arg-index) args)))
    (unless (stringp sub-control)
      (signal-format-error "~? first argument must be a string"))
    (unless (listp sub-args)
      (signal-format-error "~? second argument must be a list"))
    (apply #'format stream sub-control sub-args))
  ;; Returns number of args consumed: 2
  2)
```

---

## Dependency Analysis

### Required Modules (existing)

| Module | Usage |
|--------|-------|
| `clysm/streams` | Stream types, write-char, write-string |
| `clysm/conditions` | Condition definitions, signaling |
| `clysm/runtime/printer` | prin1*, princ* (already used) |

### No New External Dependencies

All required infrastructure exists in the codebase.

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Nested parsing complexity | Medium | Extensive unit tests for edge cases |
| Column tracking across streams | Low | Simple heuristic, document limitations |
| Compatibility with 93 call sites | High | Audit all format strings in compiler |

---

## Implementation Order

1. **format-error condition** - Foundation for error handling
2. **Column tracking + ~&** - Low complexity, immediate benefit
3. **Parser refactor** - Enable nested directive support
4. **~{~} + ~^** - Iteration (P2 priority)
5. **~[~] + ~:;** - Conditional (P3 priority)
6. **~?** - Recursive (P3 priority)

---

## Verification Plan

1. Unit tests for each new directive in isolation
2. Nested directive tests (iteration containing conditionals)
3. Format-error condition tests (malformed strings)
4. ANSI test suite subset for FORMAT
5. Smoke test: compile Clysm with self-hosted FORMAT
