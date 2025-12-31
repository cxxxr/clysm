# Research: Phase 13D M3 - CLOS Primitives for Wasm

**Date**: 2025-12-31
**Feature**: 001-m3-clos-primitives

## Summary

Research findings for implementing CLOS primitive Wasm codegen. All required opcodes and types already exist in the codebase.

## Findings

### 1. WasmGC Struct Opcodes (Already Implemented)

**Decision**: Use existing opcode definitions from `func-section.lisp`

**Rationale**: The opcodes are already defined and tested in the compiler:
- `(:struct.new . (#xFB #x00))` - Create new struct
- `(:struct.get . (#xFB #x02))` - Read struct field
- `(:struct.set . (#xFB #x05))` - Write struct field

**Source**: `src/clysm/compiler/codegen/func-section.lisp:224-226`

### 2. Reference Type Testing (Already Implemented)

**Decision**: Use existing `ref.test` pattern for `standard-instance-p`

**Rationale**: `ref.test` is extensively used (140+ occurrences) for type checking:
- Pattern: `(:ref.test (:ref ,type-index))`
- Emits to `#xFB #x14` with heaptype argument

**Source**: `src/clysm/compiler/compiler.lisp:1008-1012`

### 3. CLOS Type Indices (Already Defined)

**Decision**: Use existing type indices from `gc-types.lisp`

| Type | Index | Description |
|------|-------|-------------|
| `$instance` | 6 | CLOS instance struct |
| `$standard-class` | 7 | Class metadata struct |
| `$slot-vector` | 21 | Slot value array |

**Source**: `src/clysm/compiler/codegen/gc-types.lisp:16,17,52`

### 4. CLOS Instance Structure

**Decision**: Follow existing $instance structure for slot access

**Structure** (from `make-instance-type`):
```wat
(type $instance (struct
  (field $class (ref $standard-class))  ;; field 0
  (field $slots (ref $slot-vector))))   ;; field 1
```

**Slot Access Pattern**:
1. Get `$slots` field (index 1) from instance
2. Use `array.get` on slot-vector with slot index

**Source**: `src/clysm/compiler/codegen/gc-types.lisp:445-457`

### 5. DEFSTRUCT Macro Expansion (Already Working)

**Decision**: No changes needed to DEFSTRUCT expansion

**Current Expansion** (from `defstruct.lisp`):
- `make-instance*` → Calls `clysm/clos/instance:make-instance*`
- `slot-value*` → Calls `clysm/clos/slot-access:slot-value*`
- `standard-instance-p` → Calls `clysm/clos/mop:standard-instance-p`

The macro expansion is correct - only the compiler codegen for these primitives needs implementation.

**Source**: `src/clysm/lib/defstruct.lisp:227-250`

### 6. Compilation Strategy

**Decision**: Add special-case handlers in `compile-call` for CLOS primitives

**Pattern** (following existing built-ins like `cons`, `car`, `cdr`):
```lisp
(defun compile-call (ast env)
  ...
  (case name
    ...
    ((clysm/clos/slot-access:slot-value*)
     (compile-slot-value-read ast env))
    ((clysm/clos/instance:make-instance*)
     (compile-make-instance ast env))
    ((clysm/clos/mop:standard-instance-p)
     (compile-standard-instance-p ast env))
    ...))
```

**Alternatives Considered**:
- Generic function call: Rejected - too much overhead, prevents inlining
- Macro expansion: Rejected - primitives need direct Wasm emission

### 7. Slot Index Resolution

**Decision**: Static slot index at compile time from literal slot name

**Requirement**: `slot-value*` calls must have literal slot name for compile-time index resolution:
```lisp
;; Compilable (slot name is literal symbol)
(slot-value* instance 'x)
(slot-value* instance ':x)

;; Not directly compilable (dynamic slot name) - falls back to runtime lookup
(slot-value* instance slot-name-var)
```

For DEFSTRUCT-generated accessors, slot names are always literals.

### 8. Array Operations for Slot Access

**Decision**: Use `array.get` and `array.set` opcodes for $slot-vector

Opcodes needed (may need to add if not present):
- `(:array.get . (#xFB #x0B))` - Read array element
- `(:array.set . (#xFB #x0E))` - Write array element

**Note**: Need to verify these are defined in `*wasm-opcodes*`

## Implementation Approach

### Phase 1: Add Opcodes (if missing)
Add `array.get` and `array.set` to `*wasm-opcodes*` if not present.

### Phase 2: slot-value* Compilation
```lisp
;; Input: (slot-value* instance 'slot-name)
;; Output:
;;   <compile instance>
;;   ref.cast (ref $instance)
;;   struct.get $instance 1         ;; get $slots field
;;   i32.const <slot-index>
;;   array.get $slot-vector
```

### Phase 3: (setf slot-value*) Compilation
```lisp
;; Input: (setf (slot-value* instance 'slot-name) value)
;; Output:
;;   <compile instance>
;;   ref.cast (ref $instance)
;;   struct.get $instance 1         ;; get $slots field
;;   i32.const <slot-index>
;;   <compile value>
;;   array.set $slot-vector
;;   <compile value>                 ;; return value
```

### Phase 4: make-instance* Compilation
```lisp
;; Input: (make-instance* 'class-name)
;; Output:
;;   global.get $class-<name>       ;; class metadata global
;;   i32.const <slot-count>
;;   array.new_default $slot-vector ;; create slots array
;;   struct.new $instance           ;; create instance
```

### Phase 5: standard-instance-p Compilation
```lisp
;; Input: (standard-instance-p obj)
;; Output:
;;   <compile obj>
;;   ref.test (ref $instance)       ;; returns i32 (0 or 1)
;;   ;; Convert to Lisp boolean (t/nil)
;;   if (result anyref)
;;     global.get $t
;;   else
;;     global.get $nil
;;   end
```

## Dependencies

- Existing: `gc-types.lisp`, `func-section.lisp`, `compiler.lisp`
- No new external dependencies required
