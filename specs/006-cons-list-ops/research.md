# Research: Cons Cell and List Operations

**Feature**: 006-cons-list-ops
**Date**: 2025-12-23
**Phase**: 0 (Research)

## Executive Summary

The Clysm compiler already has the WasmGC infrastructure required for cons cells. The `$cons` type (Type 2) is defined in `gc-types.lisp` with mutable car/cdr fields. This feature needs to add compilation functions for cons/car/cdr/list operations and integrate them into the existing primitive call dispatch.

## Existing Infrastructure Analysis

### $cons Type Definition (Type 2)

**Location**: `src/clysm/compiler/codegen/gc-types.lisp:127-134`

```lisp
(defun make-cons-type ()
  "Create CONS cell type (T035)
   Cons cells have car and cdr fields, both anyref."
  (make-wasm-struct-type
   :name '$cons
   :index +type-cons+
   :fields (list (make-wasm-field :name 'car :type :anyref :mutable t)
                 (make-wasm-field :name 'cdr :type :anyref :mutable t))))
```

**Key Properties**:
- Type index: 2 (constant `+type-cons+`)
- Car field: index 0, mutable anyref
- Cdr field: index 1, mutable anyref
- Both fields are anyref to hold any Lisp object

### Type Index Constants

**Location**: `src/clysm/compiler/codegen/gc-types.lisp:10-27`

```lisp
(defconstant +type-nil+ 0 "Type index for NIL singleton")
(defconstant +type-unbound+ 1 "Type index for UNBOUND sentinel")
(defconstant +type-cons+ 2 "Type index for cons cells")
(defconstant +type-symbol+ 3 "Type index for symbols")
;; ... more types
```

### NIL Representation

Per constitution principle II, NIL is NOT represented as Wasm null. It's a singleton struct of type `$nil` (Type 0). This means:

1. NIL checks use `ref.eq` against the NIL singleton global
2. car/cdr of NIL must return NIL (not trap)
3. A global `$nil` must exist and be accessible

**Verification needed**: Confirm NIL singleton global exists in generated modules.

### Existing Primitive Call Infrastructure

**Location**: `src/clysm/compiler/codegen/func-section.lisp:420-437`

```lisp
(defun compile-primitive-call (op args env)
  "Compile a primitive operation"
  (case op
    (+  (compile-arithmetic-op :i32.add args env 0))
    (-  (if (= 1 (length args))
            (compile-unary-minus (first args) env)
            (compile-arithmetic-op :i32.sub args env nil)))
    (*  (compile-arithmetic-op :i32.mul args env 1))
    (/  (compile-arithmetic-op :i32.div_s args env nil))
    (truncate (compile-truncate args env))
    (<  (compile-comparison-op :i32.lt_s args env))
    (>  (compile-comparison-op :i32.gt_s args env))
    (<= (compile-comparison-op :i32.le_s args env))
    (>= (compile-comparison-op :i32.ge_s args env))
    (=  (compile-comparison-op :i32.eq args env))
    (/= (compile-not-equal args env))))
```

**Integration point**: Add cases for `cons`, `car`, `cdr`, `list`, `consp`, `null`, `atom`, `listp`, `rplaca`, `rplacd`, etc.

### Primitives List

**Needs investigation**: Find where `*primitives*` is defined to ensure new operations are recognized as primitive calls.

## WasmGC Instructions Required

### For cons creation

```wat
;; struct.new creates a new struct instance
struct.new <typeidx>   ;; pops field values, pushes struct ref
```

For `$cons` (Type 2):
```wat
;; Stack: [car_value: anyref, cdr_value: anyref]
struct.new 2
;; Stack: [(ref 2)]
```

### For car/cdr access

```wat
;; struct.get reads a field
struct.get <typeidx> <fieldidx>  ;; pops struct ref, pushes field value

;; For car (field 0):
struct.get 2 0

;; For cdr (field 1):
struct.get 2 1
```

### For rplaca/rplacd

```wat
;; struct.set writes a field
struct.set <typeidx> <fieldidx>  ;; pops struct ref and value, no result

;; For rplaca (field 0):
struct.set 2 0

;; For rplacd (field 1):
struct.set 2 1
```

### For type predicates

```wat
;; ref.test checks if reference is of given type
ref.test (ref <typeidx>)  ;; pops anyref, pushes i32 (0 or 1)

;; For consp:
ref.test (ref 2)  ;; is it a $cons?

;; ref.eq compares two references
ref.eq  ;; pops two refs, pushes i32 (0 or 1)

;; For null:
global.get $nil
ref.eq
```

### For type casting

```wat
;; ref.cast converts reference to specific type (may trap)
ref.cast (ref <typeidx>)  ;; pops anyref, pushes typed ref

;; Safe pattern (test before cast):
local.tee $tmp
ref.test (ref 2)
if (result anyref)
  local.get $tmp
  ref.cast (ref 2)
  struct.get 2 0
else
  ;; handle non-cons case
end
```

## NIL Handling Strategy

### Common Lisp Semantics

- `(car nil)` returns NIL
- `(cdr nil)` returns NIL
- `(consp nil)` returns NIL
- `(null nil)` returns T
- `(atom nil)` returns T (NIL is not a cons)
- `(listp nil)` returns T (NIL is the empty list)

### Implementation Pattern for car/cdr

```lisp
(defun compile-car (args env)
  "Compile (car x) with NIL handling"
  (let ((arg (first args))
        (result '()))
    ;; Compile argument
    (setf result (compile-to-instructions arg env))
    ;; Store in temp local for reuse
    (setf result (append result '((:local.tee $tmp))))
    ;; Check if NIL
    (setf result (append result
      '((:global.get $nil)
        (:ref.eq)
        (:if (:result :anyref)
          ((:global.get $nil))    ;; NIL case: return NIL
          ((:local.get $tmp)
           (:ref.cast (:ref 2))   ;; cast to $cons
           (:struct.get 2 0))))))  ;; get car
    result))
```

### Alternative: Unconditional Cast with NIL Guard

If NIL is checked before calling car/cdr at the Lisp level (using consp), we could optimize away the NIL check. But CL semantics require car/cdr to handle NIL gracefully, so the check is necessary.

## List Construction Strategy

### Right-to-Left Building

`(list a b c)` = `(cons a (cons b (cons c nil)))`

Building right-to-left avoids stack reversal:

```wat
;; Start with NIL
global.get $nil

;; Build (cons c nil)
<compile c>       ;; car value
local.get $list   ;; cdr value (currently nil)
struct.new 2
local.set $list

;; Build (cons b <list>)
<compile b>
local.get $list
struct.new 2
local.set $list

;; Build (cons a <list>)
<compile a>
local.get $list
struct.new 2
;; result on stack
```

### Variadic Handling

`list` accepts any number of arguments. In compile-list:

```lisp
(defun compile-list (args env)
  "Compile (list &rest args)"
  (if (null args)
      ;; (list) -> NIL
      '((:global.get $nil))
      ;; Build cons chain right-to-left
      (let ((result '((:global.get $nil))))  ;; start with NIL
        (dolist (arg (reverse args))
          (setf result
            (append (compile-to-instructions arg env)
                    result
                    '((:struct.new 2)))))
        result)))
```

Wait, the stack ordering needs more thought. Let me reconsider:

For `struct.new 2`, stack order is: `[car, cdr]` (car on bottom, cdr on top)

So for `(list 1 2 3)`:
1. Push NIL (this will be the final cdr)
2. Push 3, swap, struct.new 2 → `(3 . nil)`
3. Push 2, swap, struct.new 2 → `(2 . (3 . nil))`
4. Push 1, swap, struct.new 2 → `(1 . (2 . (3 . nil)))`

But wait, `struct.new` expects `[car, cdr]` so:
- To make `(3 . nil)`: need `[3, nil]` on stack
- After pushing nil, push 3, then need to swap: `[nil] -> [nil, 3] -> [3, nil]` via swap

Actually, Wasm doesn't have a swap instruction. We need local variables:

```wat
global.get $nil   ;; [nil]
local.set $cdr    ;; []

;; Make (cons 3 nil)
i32.const 3
ref.i31           ;; [3]
local.get $cdr    ;; [3, nil]
struct.new 2      ;; [(3 . nil)]
local.set $cdr    ;; []

;; Make (cons 2 <list>)
i32.const 2
ref.i31           ;; [2]
local.get $cdr    ;; [2, (3 . nil)]
struct.new 2      ;; [(2 . (3 . nil))]
local.set $cdr

;; ... and so on
```

This requires a dedicated local variable for the accumulator.

## Existing Compilation Patterns to Follow

### Pattern from compile-arithmetic-op

**Location**: `func-section.lisp:439-469`

Uses `compile-to-instructions` for each argument, then emits the operation.

### Pattern from compile-comparison-op

**Location**: `func-section.lisp:492-509`

Compiles two arguments, emits comparison, then wraps result in boolean conversion.

### Boolean Return Pattern

Predicates need to return T or NIL, not i32. Pattern:

```wat
;; After getting i32 result (0 or 1):
if (result anyref)
  global.get $t
else
  global.get $nil
end
```

This requires `$t` (T symbol) global to exist.

## Open Questions

1. **NIL global**: Verify `$nil` global exists in generated modules
2. **T global**: Verify `$t` global exists for predicate return values
3. **Local variable allocation**: How does compile-to-instructions allocate temp locals?
4. **Primitives registration**: Where is the primitives list defined?

## Recommendations

### Implementation Order

1. **cons** first - fundamental operation, simplest
2. **car/cdr** - need cons working, adds NIL handling
3. **list** - builds on cons
4. **consp/null/atom/listp** - predicates, straightforward
5. **rplaca/rplacd** - destructive ops, last

### Code Location

All implementations should go in `func-section.lisp` alongside existing `compile-*` functions, integrated into `compile-primitive-call`.

### Testing Strategy

Per constitution VII (TDD):
1. Write unit tests that verify generated Wasm instructions
2. Write integration tests that compile and run actual Lisp code
3. Use existing test infrastructure from `tests/integration/`

## Conclusion

The infrastructure for cons cells is already in place. Implementation requires:
1. Adding compilation functions for each operation
2. Integrating into primitive call dispatch
3. Handling NIL correctly per CL semantics
4. Following existing patterns for argument compilation and result handling

No architectural changes needed - this is a straightforward extension of the existing primitive system.
