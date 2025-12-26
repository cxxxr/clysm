# Data Model: Equality Predicates and Logical Operators

**Feature**: 024-equality-predicates
**Date**: 2025-12-26

## Type Signatures

### Equality Predicates

| Function | Signature | Return Type | Description |
|----------|-----------|-------------|-------------|
| `eq` | `(x y) → boolean` | T or NIL | Reference identity comparison |
| `eql` | `(x y) → boolean` | T or NIL | Type-aware value equality |
| `equal` | `(x y) → boolean` | T or NIL | Structural equality |
| `equalp` | `(x y) → boolean` | T or NIL | Case-insensitive structural equality |
| `not` | `(x) → boolean` | T or NIL | Logical negation |

### Logical Operators (Special Forms)

| Form | Syntax | Return Type | Description |
|------|--------|-------------|-------------|
| `and` | `(and {form}*)` | Last form value or NIL | Short-circuit conjunction |
| `or` | `(or {form}*)` | First non-NIL value or NIL | Short-circuit disjunction |

## Wasm Type Mappings

### Input Types (anyref)

All equality predicates accept `anyref` parameters:

```wat
(func $eq (param $x anyref) (param $y anyref) (result anyref)
  ;; Implementation
)
```

### Type Constants (from gc-types.lisp)

| Lisp Type | Wasm Type | Type ID | Notes |
|-----------|-----------|---------|-------|
| Fixnum | i31ref | N/A | Immediate value |
| Symbol | (ref $symbol) | 3 | Interned singleton |
| Cons | (ref $cons) | 4 | Pair structure |
| Closure | (ref $closure) | 5 | Function object |
| String | (ref $string) | 10 | Character array |
| Character | i31ref | N/A | Tagged immediate |
| Float | (ref $float) | 16 | f64 boxed |
| Ratio | (ref $ratio) | 15 | num/denom pair |

## Equality Semantics Matrix

### eq (Reference Identity)

| Type A | Type B | Result | Reason |
|--------|--------|--------|--------|
| Symbol | Symbol | T if same | Interned singletons |
| Fixnum | Fixnum | T if same value | i31ref encodes value |
| Character | Character | T if same | i31ref encodes code point |
| Cons | Cons | T if same object | Reference comparison |
| Float | Float | T if same object | Even if same value, different alloc → NIL |
| Any | NIL | T if x is NIL | NIL is singleton |

### eql (Type-Aware Value)

| Type A | Type B | Result | Reason |
|--------|--------|--------|--------|
| Fixnum | Fixnum | T if same value | i31ref value equality |
| Float | Float | T if same f64 | f64.eq comparison |
| Ratio | Ratio | T if same n/d | Compare numerator and denominator |
| Fixnum | Float | NIL | Different types |
| Character | Character | T if char= | Same code point |
| Other | Other | Delegates to eq | Reference identity |

### equal (Structural)

| Type A | Type B | Result | Reason |
|--------|--------|--------|--------|
| Cons | Cons | T if car/cdr equal | Recursive comparison |
| String | String | T if string= | Character-by-character |
| Other | Other | Delegates to eql | Type-aware value |

### equalp (Case-Insensitive Structural)

| Type A | Type B | Result | Reason |
|--------|--------|--------|--------|
| String | String | T if string-equal | Case-insensitive |
| Character | Character | T if char-equal | Case-insensitive |
| Number | Number | T if = | Type-coercing numeric |
| Cons | Cons | T if car/cdr equalp | Recursive case-insensitive |
| Other | Other | Delegates to equal | Structural |

## AST Node Types

### Existing Nodes (from ast.lisp)

```lisp
;; and/or are parsed to nested if forms
(defstruct ast-if
  test    ; Condition expression
  then    ; True branch
  else)   ; False branch (nil for single-branch if)
```

### No New AST Nodes Required

The equality predicates are function calls, not special forms:
- `eq`, `eql`, `equal`, `equalp`, `not` → `ast-call` nodes
- `and`, `or` → Expanded to `ast-if` at parse time

## Compilation Output Structure

### eq Compilation

```wat
;; (eq x y) compiles to:
(local.get $x)
(local.get $y)
(ref.eq)
(if (result anyref)
  (then (ref.i31 (i32.const 1)))   ;; T
  (else (global.get $nil)))         ;; NIL
```

### not Compilation

```wat
;; (not x) compiles to:
(local.get $x)
(global.get $nil)
(ref.eq)
(if (result anyref)
  (then (ref.i31 (i32.const 1)))   ;; T (x was NIL)
  (else (global.get $nil)))         ;; NIL (x was not NIL)
```

### eql Type Dispatch

```wat
;; (eql x y) type dispatch skeleton:
(block $done (result anyref)
  ;; Check if both fixnum
  (local.get $x)
  (ref.test :i31)
  (if
    (then
      (local.get $y)
      (ref.test :i31)
      (if
        (then
          ;; Both fixnum: ref.eq is value comparison
          (local.get $x) (local.get $y) (ref.eq)
          (br_if $done (wrap-boolean ...))
        )
      )
    )
  )
  ;; Check if both float
  ;; Check if both ratio
  ;; Check if both character
  ;; Fall back to ref.eq
  (local.get $x) (local.get $y) (ref.eq)
  (wrap-boolean)
)
```

## Dependencies Graph

```
eq ←───────────┐
               │
eql ───────────┼── equal ──── equalp
  │            │      │          │
  │            │      │          │
  ├── ref.eq   │      ├── string=    ├── string-equal
  ├── ref.test │      │              ├── char-equal
  ├── f64.eq   │      │              └── numeric =
  └── i64.eq   │      │
               │      │
               └──────┴── (recursive calls)
```

## State Transitions

Not applicable - equality predicates are pure functions with no state modification.

## Validation Rules

1. All predicates must return exactly `T` or `NIL` (no other truthy values)
2. `eq` must be reflexive: `(eq x x)` → T for all x
3. `eql` must be reflexive and symmetric
4. `equal` must be reflexive, symmetric, and transitive
5. `equalp` must satisfy all of equal's properties plus case-insensitivity
6. `not` must be idempotent: `(not (not x))` ≠ x in general, but `(not (not t))` → T
