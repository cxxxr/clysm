# Research: Self-Hosting Blockers Resolution

**Feature**: 043-self-hosting-blockers
**Date**: 2025-12-28
**Status**: Complete

## Executive Summary

This document captures research findings for implementing the five feature areas needed to achieve 50%+ Stage 0 compilation rate:

1. **LOOP Macro**: Already comprehensive; minor gaps in error handling
2. **&optional/:key Defaults**: Major gap - no default value support in codegen
3. **Hash Tables**: Not implemented - requires new WasmGC types
4. **List Functions**: Partially implemented - need :test/:key options
5. **Sequence Functions**: Partially implemented - need :test/:key/:start/:end/:from-end

---

## 1. LOOP Macro Analysis

### Current State

**Decision**: LOOP macro is already comprehensive, implemented in `src/clysm/lib/macros.lisp`.

**Rationale**: Existing implementation (lines 630-1460+) supports:
- FOR/AS with IN, ON, ACROSS, =, arithmetic (FROM/TO/BY), BEING hash-keys/values
- Accumulation: COLLECT, SUM, COUNT, MAXIMIZE, MINIMIZE, APPEND, NCONC
- Termination: WHILE, UNTIL, ALWAYS, NEVER, THEREIS, RETURN
- Conditional: IF, WHEN, UNLESS
- Control: DO, DOING, INITIALLY, FINALLY, NAMED

**Alternatives Considered**: None needed - implementation complete.

**Gap Identified**: Only verification needed - ensure all clauses expand correctly and compile.

### Implementation Impact

- No new code needed
- Add integration tests to verify each clause type compiles correctly
- Current blocker is likely in clause *expansion* not *parsing*

---

## 2. Default Parameter Values (&optional/:key)

### Current State

**Gap**: `compile-defun` in `func-section.lisp:3804-3836` treats all parameters as simple symbols.

```lisp
;; Current code - ignores default values
(dolist (param params)
  (env-add-local func-env param))
```

**Decision**: Extend `compile-defun` to handle parameter forms: `(&optional (x default) (y other-default y-p))`.

**Rationale**: ANSI CL requires:
1. Default values evaluated in function scope (not call scope)
2. Default evaluation only when argument not supplied
3. Supplied-p variables for detecting explicit NIL vs. omitted
4. &key similarly with `(&key (name default name-p))`

**Implementation Strategy**:
1. Parse lambda-list in `ast.lisp` to extract structured param info
2. Add AST nodes: `ast-optional-param`, `ast-key-param` with slots for name, default, supplied-p
3. In codegen, generate conditional code:
   - Check if argument count >= param index
   - If supplied: use argument value
   - If not: evaluate default form
   - Set supplied-p variable to T or NIL

### WasmGC Mapping

```wat
;; For (&optional (x 10))
;; Assume $arg_count is available (passed somehow - need design)
local.get $arg_idx
i32.const 0  ;; index of optional param
i32.lt_s
if (result anyref)
  ;; Default: evaluate (10)
  i32.const 10
  ref.i31
else
  ;; Supplied: get argument
  local.get $arg0
end
local.set $x
```

**Challenge**: Clysm currently generates fixed-arity functions. Need to decide:
- Option A: Generate wrapper with fallback to N-ary
- Option B: Pass argument count as implicit first parameter
- Option C: Generate multiple entry points

**Recommended**: Option B (argument count) - matches existing closure `$code_N` pattern.

---

## 3. Hash Tables

### Current State

**Gap**: No WasmGC types or compilation support for hash tables.

**Decision**: Implement hash tables using WasmGC array-of-buckets design.

**Rationale**: WasmGC provides `array` and `struct` types suitable for hash table implementation.

### WasmGC Type Design

```wat
;; Hash table entry (key-value pair)
(type $hash-entry (struct
  (field $key anyref)
  (field $value (mut anyref))
  (field $next (mut (ref null $hash-entry)))))  ;; Chain for collisions

;; Hash table structure
(type $hash-table (struct
  (field $test i32)          ;; 0=eq, 1=eql, 2=equal, 3=equalp
  (field $count (mut i32))   ;; Number of entries
  (field $size i32)          ;; Bucket count
  (field $buckets (ref $bucket-array))))

;; Bucket array
(type $bucket-array (array (mut (ref null $hash-entry))))
```

### Operations

| Operation | Implementation |
|-----------|---------------|
| `make-hash-table` | Allocate struct with empty bucket array |
| `gethash` | Hash key → bucket index → traverse chain → return value + present-p |
| `(setf gethash)` | Hash key → find or create entry → set value |
| `remhash` | Hash key → find entry → unlink from chain |
| `maphash` | Iterate all buckets → call function on each entry |

### Hash Function

For :test #'eql (default):
- Fixnum (i31ref): Use value directly mod bucket count
- Symbol: Use symbol's hash slot
- Other: Fall back to ref identity

---

## 4. List Functions (assoc, rassoc, member, adjoin, union, intersection)

### Current State

**Partial Implementation**: `compile-assoc`, `compile-rassoc`, `compile-member` exist in `func-section.lisp:6558-6800` but only with 2 arguments (no :test/:key options).

**Decision**: Extend existing implementations to support keyword arguments.

**Rationale**: Current implementations use hardcoded EQL comparison. Need to accept and use `:test` and `:key` functions.

### Implementation Strategy

1. **Keyword Argument Parsing**: In codegen, detect keyword arguments and extract :test/:key
2. **Test Function Application**:
   - Default: EQL (existing logic)
   - With :test: Call the test function instead of EQL
   - With :test-not: Invert result (deprecated but required)
3. **Key Function Application**:
   - Apply :key to element before comparison
   - Recursively compile key function call

### Example: Extended member

```lisp
;; Current: (member item list) only
;; Needed: (member item list :test #'equal :key #'car)
```

Codegen changes:
```
;; Instead of hardcoded ref.eq/i31 comparison:
;; 1. Apply key function to element if :key provided
;; 2. Call test function with item and (possibly keyed) element
;; 3. Check if result is non-NIL
```

### Functions to Extend

| Function | Current Args | Needed |
|----------|-------------|--------|
| assoc | (item alist) | + :test :key |
| rassoc | (item alist) | + :test :key |
| member | (item list) | + :test :key |
| adjoin | NEW | (item list &key :test :key) |
| union | NEW | (list1 list2 &key :test :key) |
| intersection | NEW | (list1 list2 &key :test :key) |

---

## 5. Sequence Functions (position, find, remove, substitute)

### Current State

**Partial Implementation**: `compile-find`, `compile-position` exist with 2 arguments only.

**Decision**: Extend to support :test, :key, :start, :end, :from-end options.

**Rationale**: Same as list functions - current implementations use hardcoded comparison.

### Implementation Strategy

1. **Keyword Argument Parsing**: Extract :test, :key, :start, :end, :from-end, :count
2. **Subsequence Handling**:
   - :start N - skip first N elements
   - :end M - stop at element M
   - :from-end - process in reverse (for lists: reverse first)
3. **Count Limiting** (for remove/substitute):
   - :count N - only modify first N matches

### Example: Extended find

```lisp
;; Current: (find item sequence)
;; Needed: (find item sequence :test #'equal :key #'car :start 5 :end 10)
```

### Functions to Extend

| Function | Current | Needed Options |
|----------|---------|---------------|
| position | 2 args | :test :key :start :end :from-end |
| position-if | 2 args | :key :start :end :from-end |
| find | 2 args | :test :key :start :end :from-end |
| find-if | 2 args | :key :start :end :from-end |
| remove | NEW | :test :key :count :start :end :from-end |
| substitute | NEW | :test :key :count :start :end :from-end |

### Vector vs List

Current implementations are list-only. For ANSI compliance:
- Lists: Use current traversal logic
- Vectors: Use array indexing with bounds checking

**Recommendation**: Lists-first for bootstrap (compiler source is list-heavy), vectors as stretch goal.

---

## Implementation Priority

Based on blocker analysis from Feature 038:

| Feature | Impact | Effort | Priority |
|---------|--------|--------|----------|
| &optional/:key defaults | HIGH | MEDIUM | 1 |
| LOOP verification | LOW | LOW | 5 |
| assoc/member/etc :test/:key | MEDIUM | MEDIUM | 2 |
| find/position :test/:key | MEDIUM | MEDIUM | 3 |
| Hash tables | MEDIUM | HIGH | 4 |

**Rationale**: Default parameters block many defun forms. List/sequence keyword args enable pattern matching in compiler. Hash tables are used but less frequently in bootstrap subset.

---

## Test Strategy

### Unit Tests
- Each clause type for LOOP expansion
- Default parameter evaluation scenarios
- Hash table CRUD operations
- List function :test/:key variations
- Sequence function boundary conditions (:start/:end)

### Contract Tests
- Wasm validation for generated code
- Type section includes hash-table types

### Integration Tests
- Bootstrap compilation rate measurement (≥50% target)
- compiler/codegen/ files compilation verification

---

## Dependencies

- Feature 028 (Setf Macros): Required for `(setf gethash)`
- Feature 025 (Multiple Values): Required for `gethash` returning (values value present-p)
- Feature 024 (Equality Predicates): Used for :test #'equal, :test #'equalp

All dependencies are already complete.
