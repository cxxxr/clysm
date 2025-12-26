# Research: Equality Predicates and Logical Operators

**Feature**: 024-equality-predicates
**Date**: 2025-12-26

## 1. Wasm GC Instructions for Equality

### Decision: Use `ref.eq` for identity comparison

**Rationale**: `ref.eq` is the native WasmGC instruction for reference equality. It compares two references and returns 1 (i32) if they point to the same object, 0 otherwise.

**Alternatives Considered**:
- Manual pointer comparison via linear memory: Rejected - violates Constitution Principle I (WasmGC-First)
- Hash-based comparison: Rejected - unnecessary overhead for identity check

### Wasm Instructions Used

| Predicate | Primary Instructions | Notes |
|-----------|---------------------|-------|
| `eq` | `ref.eq` | Returns i32 (0/1), convert to T/NIL |
| `eql` | `ref.eq`, `ref.test`, `ref.cast`, `f64.eq` | Type dispatch + value comparison |
| `equal` | `ref.eq`, `ref.test`, `call $equal-cons`, `call $string=` | Recursive for cons/string |
| `equalp` | `ref.test`, `call $char-equal`, `call $string-equal`, `f64.eq` | Case-insensitive dispatch |
| `not` | `ref.eq` (with NIL singleton) | Check if arg equals NIL |

## 2. Type Dispatch Strategy for eql

### Decision: Explicit type tests with `ref.test` followed by `ref.cast`

**Rationale**: WasmGC requires explicit type testing before downcasting. This matches the pattern established in feature 023-type-predicates.

**Implementation Pattern**:
```wat
;; For eql: check if both are same type, then compare values
(ref.test $fixnum (local.get $a))  ;; Is a fixnum?
(if
  (then
    (ref.test $fixnum (local.get $b))  ;; Is b also fixnum?
    (if
      (then
        (ref.eq (local.get $a) (local.get $b))  ;; i31ref eq is value equality
      )
      (else
        (i32.const 0)  ;; Different types -> NIL
      )
    )
  )
  ...
)
```

**Type Hierarchy for eql**:
1. Fixnum (i31ref): Use `ref.eq` directly (i31 encodes value)
2. Float (struct with f64): Extract f64, use `f64.eq`
3. Ratio (struct with num/denom): Compare numerator and denominator with i64.eq
4. Character (i31ref with tag): Use `ref.eq` directly (encodes code point)
5. All others: Fall back to `ref.eq` (identity)

## 3. Recursive equal Implementation

### Decision: Implement as Wasm function with self-calls

**Rationale**: `equal` requires recursive comparison of cons cells. This must be a Wasm function that can call itself.

**Implementation Strategy**:
1. Export `$equal` as a callable function
2. For cons cells: `(and (equal (car a) (car b)) (equal (cdr a) (cdr b)))`
3. For strings: Use existing `string=` implementation
4. For others: Fall back to `eql`

**Alternatives Considered**:
- Trampoline-based iteration: Rejected - tail-call optimization handles recursion
- Stack-based iteration: Rejected - unnecessary complexity

## 4. equalp Case-Insensitive Comparison

### Decision: Reuse `char-equal` and `string-equal` from feature 008

**Rationale**: The spec explicitly requires `equalp` to use `char-equal` (case-insensitive character comparison) and `string-equal` (case-insensitive string comparison).

**Dependencies Verified**:
- `char-equal`: Implemented in feature 008-character-string
- `string-equal`: Implemented in feature 008-character-string
- Numeric `=`: Implemented in feature 010-numeric-tower (type-coercing)

## 5. and/or Special Form Compilation

### Decision: Compile AST-level expansion (already parsed)

**Rationale**: The parser (`ast.lisp` lines 918-939) already transforms:
- `(and a b c)` → `(if a (if b c NIL) NIL)`
- `(or a b c)` → `(let ((#g1 a)) (if #g1 #g1 (let ((#g2 b)) (if #g2 #g2 c))))`

The compiler only needs to handle the resulting `ast-if` nodes correctly.

**Verification Needed**:
- Confirm `parse-and-form` and `parse-or-form` exist and produce correct AST
- Ensure short-circuit semantics are preserved (no evaluation of skipped forms)

## 6. Return Value Representation

### Decision: T = `(ref.i31 (i32.const 1))`, NIL = symbol singleton

**Rationale**: Consistent with existing type predicates from feature 023.

**Pattern**:
```lisp
(defun wrap-boolean-result (instructions)
  "Wrap i32 (0/1) result as T/NIL"
  `(,@instructions
    (if (result anyref)
      (then (ref.i31 (i32.const 1)))  ;; T
      (else (global.get $nil)))))      ;; NIL singleton
```

## 7. Performance Considerations

### Decision: Inline simple predicates, function call for recursive

| Predicate | Strategy | Reason |
|-----------|----------|--------|
| `eq` | Inline | Single `ref.eq` instruction |
| `eql` | Inline with branches | Type dispatch overhead minimal |
| `equal` | Function call | Recursive; needs call frame |
| `equalp` | Function call | Reuses equal + case-insensitive helpers |
| `not` | Inline | Single `ref.eq` with NIL |
| `and`/`or` | Inline (via if) | Compiled from AST expansion |

## 8. Edge Cases Resolution

| Edge Case | Decision | Source |
|-----------|----------|--------|
| `(eq 'a 'a)` | T | Symbols are interned (same reference) |
| `(eql 1 1)` | T | Fixnums with same value have same i31ref |
| `(eql 1 1.0)` | NIL | Different types |
| `(eql 3.14 3.14)` | T | Same f64 value |
| `(equal '(1 2) '(1 2))` | T | Recursive cons comparison |
| `(equalp "ABC" "abc")` | T | Case-insensitive string-equal |
| `(and)` | T | Per ANSI CL |
| `(or)` | NIL | Per ANSI CL |
| `(eql +nan.0 +nan.0)` | NIL | IEEE 754: NaN != NaN |

## Summary

All technical decisions align with:
- Constitution Principle I (WasmGC-First)
- Established patterns from features 008, 010, 023
- ANSI Common Lisp specification

No NEEDS CLARIFICATION items remain.
