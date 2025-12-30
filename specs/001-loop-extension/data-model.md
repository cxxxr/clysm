# Data Model: LOOP Macro Extension

**Feature**: 001-loop-extension
**Date**: 2025-12-30

## Entities

This feature extends existing LOOP data structures in `src/clysm/lib/macros.lisp`. No new persistent data; all structures are compile-time only.

### Existing Structures (to be extended)

#### loop-iter-hash

**Purpose**: Represents a hash-table iteration clause in LOOP

**Current Fields** (inferred from `parse-for-hash`):
| Field | Type | Description |
|-------|------|-------------|
| `var` | symbol | Primary iteration variable |
| `clause-type` | keyword | `:hash-keys` or `:hash-values` |
| `hash-form` | form | S-expression yielding hash-table |
| `mode` | keyword | `:keys` or `:values` (internal) |

**New Fields Required**:
| Field | Type | Description |
|-------|------|-------------|
| `using-var` | symbol | nil | Secondary variable from `using` clause |
| `using-type` | symbol | nil | `hash-key` or `hash-value` |
| `entries-var` | symbol | Gensym for collected (key . value) pairs list |
| `iter-var` | symbol | Gensym for current iteration position |

**Validation Rules**:
- `using-type` must be opposite of `mode` (if iterating keys, using provides value, and vice versa)
- `using-var` must be a valid symbol if present

---

#### loop-accumulation-clause

**Purpose**: Represents an accumulation clause (collect, sum, count, etc.)

**Current Fields**:
| Field | Type | Description |
|-------|------|-------------|
| `type` | keyword | `:collect`, `:sum`, `:count`, etc. |
| `expr` | form | Expression to accumulate |
| `into-var` | symbol | nil | Named accumulator (from INTO) |
| `acc-var` | symbol | Actual accumulator var (= into-var or gensym) |

**No structural changes needed** - INTO is already parsed. Verification needed:
- INTO var accessible in body clauses
- INTO var accessible in FINALLY clause
- Multiple INTO vars correctly handled

---

#### loop-context

**Purpose**: Top-level parse context for entire LOOP form

**Relevant Fields** (existing):
| Field | Type | Description |
|-------|------|-------------|
| `with-bindings` | list | List of `(var init-form)` pairs |
| `finally-forms` | list | Forms to execute after loop |
| `iteration-clauses` | list | All FOR/AS clauses |
| `accumulation-clauses` | list | All COLLECT/SUM/etc clauses |

**No changes needed** - all storage already exists.

---

## State Transitions

### Hash-Table Iteration State Machine

```
Initial State: entries-var = (collect-hash-entries hash-form)
               iter-var = entries-var
               var = nil
               using-var = nil (if present)

Loop Entry Check:
  IF (null iter-var) THEN GOTO loop-end

Each Iteration:
  1. (setq var (if keys-mode (caar iter-var) (cdar iter-var)))
  2. (setq using-var (if keys-mode (cdar iter-var) (caar iter-var)))  ; if using-var present
  3. Execute body
  4. (setq iter-var (cdr iter-var))
  5. GOTO Loop Entry Check

loop-end:
  Execute finally-forms
  Return result
```

---

## Code Generation Templates

### Hash-Table Iteration Bindings

```lisp
;; For: (loop for k being the hash-keys of ht using (hash-value v) ...)
;;
;; Generate bindings:
(#:entries-123 (let ((#:acc nil))
                 (maphash (lambda (#:k #:v)
                            (push (cons #:k #:v) #:acc))
                          ht)
                 (nreverse #:acc)))
(#:iter-123 #:entries-123)
(k (if #:iter-123 (caar #:iter-123) nil))
(v (if #:iter-123 (cdar #:iter-123) nil))
```

### Hash-Table Termination Test

```lisp
;; Generate:
(when (null #:iter-123) (go loop-end))
```

### Hash-Table Iteration Step

```lisp
;; Generate stepping:
(setq #:iter-123 (cdr #:iter-123))
(setq k (if #:iter-123 (caar #:iter-123) nil))
(setq v (if #:iter-123 (cdar #:iter-123) nil))
```

---

## Relationships

```
loop-context
├── with-bindings: list of (var init-form)
├── iteration-clauses: list of loop-iter-* structs
│   ├── loop-iter-arithmetic
│   ├── loop-iter-in
│   ├── loop-iter-on
│   ├── loop-iter-across
│   ├── loop-iter-equals
│   └── loop-iter-hash (EXTENDED)
│       ├── var
│       ├── mode (:keys/:values)
│       ├── hash-form
│       ├── using-var (NEW)
│       ├── using-type (NEW)
│       ├── entries-var (NEW)
│       └── iter-var (NEW)
├── accumulation-clauses: list of loop-accumulation-clause
│   ├── type
│   ├── expr
│   ├── into-var
│   └── acc-var
├── body-clauses
├── termination-clauses
├── initially-forms
└── finally-forms
```

---

## Constraints

1. **No duplicate INTO names**: Same variable cannot be used for different accumulation types
   - Enforced by: FR-013 error detection

2. **WITH bindings precede iteration bindings**: FOR variables may depend on WITH values
   - Enforced by: `all-bindings` ordering in `expand-loop`

3. **FINALLY has access to all loop variables**: INTO vars, WITH vars, FOR vars
   - Enforced by: All bindings in outer let*, finally inside block

4. **Hash-table modification during iteration**: Undefined behavior per ANSI CL
   - Not enforced; documented in spec assumptions
