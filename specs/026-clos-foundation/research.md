# Research: CLOS Foundation for WasmGC

**Date**: 2025-12-27
**Feature**: 026-clos-foundation

## Research Topics

### 1. WasmGC Struct Types for Class Hierarchy

**Decision**: Use WasmGC struct subtyping for class hierarchy representation

**Rationale**:
- WasmGC supports structural subtyping via `sub` declarations
- However, CLOS classes are more dynamic than WasmGC struct subtyping allows
- Better approach: Use a single `$instance` struct type with runtime class pointer

**Alternatives Considered**:
1. **WasmGC struct inheritance**: Generate unique struct type per class with `sub` relationship
   - Rejected: Would require knowing all classes at compile-time; prevents runtime class definition
2. **Tagged union**: Use i32 tag + anyref payload
   - Rejected: Loses type safety, requires casting everywhere

**Final Design**:
```wat
;; All instances share one struct type
(type $instance (struct
  (field $class (ref $standard-class))  ;; Runtime class reference
  (field $slots (ref $slot-vector))      ;; Slot values
))

;; Slot vector is a mutable array
(type $slot-vector (array (mut anyref)))
```

### 2. Class Metadata Representation

**Decision**: Embed class metadata as Wasm globals initialized at module load

**Rationale**:
- Class definitions are typically compile-time constants
- Wasm globals can hold struct references
- Class hierarchy (superclass chain) enables inheritance lookup

**Alternatives Considered**:
1. **Linear memory tables**: Store class metadata in linear memory
   - Rejected: Constitution prohibits linear memory dependency
2. **Encode in function closures**: Each class is a closure returning metadata
   - Rejected: Excessive indirection, slower lookup

**Final Design**:
```wat
(type $standard-class (struct
  (field $name (ref $symbol))                    ;; Class name
  (field $superclass (ref null $standard-class)) ;; Parent (null for root)
  (field $slot-count i32)                        ;; Total slot count
  (field $initargs (ref $keyword-array))         ;; Initarg keywords per slot
  (field $initforms (ref $anyref-array))         ;; Default values (closures)
))

;; Example: point class global
(global $class-point (ref $standard-class)
  (struct.new $standard-class
    (global.get $sym-point)  ;; name
    (ref.null $standard-class) ;; superclass (none)
    (i32.const 2)  ;; 2 slots: x, y
    ...))
```

### 3. Method Dispatch Strategy

**Decision**: Table-based dispatch with class ID indexing

**Rationale**:
- Single-specializer dispatch is the common case (80%+ of methods)
- O(1) lookup via function table indexed by class ID
- Falls back to linear search for multi-specializer methods

**Alternatives Considered**:
1. **Hash table dispatch**: Hash (gf-name, class-id) → method
   - Rejected: Hash computation overhead, memory pressure
2. **Inline caching**: Cache last-used method at call site
   - Rejected: Requires JIT-level code modification; Phase 2 optimization
3. **Class precedence list iteration**: Walk CPL at each dispatch
   - Rejected: O(n) per call, unacceptable for hot paths

**Final Design**:
```
Dispatch algorithm:
1. Get class from instance's $class field
2. Look up class-id (i32 index assigned at class definition)
3. Index into generic function's method table: methods[class-id]
4. If found, call method
5. If null, walk superclass chain and repeat from step 3
6. If no method after exhausting chain, signal no-applicable-method
```

### 4. Accessor Implementation

**Decision**: Accessors are inlined slot access operations (not generic functions)

**Rationale**:
- Accessors are performance-critical (used in every slot access)
- Generic function overhead is unnecessary when only one method exists
- Can upgrade to generic function if subclass overrides accessor

**Alternatives Considered**:
1. **Full generic functions**: Every accessor is a GF with one method
   - Rejected: 10x overhead for simple slot access
2. **Direct struct.get/set**: Bypass function call entirely
   - Rejected: Loses encapsulation, can't override in subclasses

**Final Design**:
```lisp
;; Reader: point-x
(defun point-x (instance)
  (declare (type point instance))
  ;; Compiles to:
  ;; (ref.cast $instance)
  ;; (struct.get $instance $slots)
  ;; (array.get $slot-vector slot-index)
  (slot-value instance 'x))

;; Writer: (setf point-x)
(defun (setf point-x) (new-value instance)
  (declare (type point instance))
  ;; Compiles to:
  ;; (ref.cast $instance)
  ;; (struct.get $instance $slots)
  ;; (array.set $slot-vector slot-index new-value)
  (setf (slot-value instance 'x) new-value))
```

### 5. Slot Initialization Order

**Decision**: Initialize slots in class precedence list order (most specific first)

**Rationale**:
- ANSI CL specifies initforms are evaluated in precedence list order
- Inherited slots initialized by parent class initforms
- Child initforms can shadow parent initforms

**Algorithm**:
```
make-instance(class, initargs):
  1. Allocate slot-vector of size class.slot-count
  2. For each slot in class.all-slots (including inherited):
     a. If initarg in initargs → use provided value
     b. Else if slot has initform → evaluate initform
     c. Else → set to UNBOUND sentinel
  3. Return (struct.new $instance class slots)
```

### 6. Class Finalization and Precedence List

**Decision**: Compute class precedence list at compile-time during class finalization

**Rationale**:
- CPL is static once class is defined
- Needed for method dispatch specificity ordering
- Single inheritance simplifies CPL to linear chain

**Single Inheritance CPL**:
```
CPL(class) = [class, superclass, superclass.superclass, ..., t]
```

### 7. Type Checking in Dispatch

**Decision**: Use `ref.test` instruction for type membership check

**Rationale**:
- WasmGC provides `ref.test` for runtime type testing
- Returns i32 (0 or 1) for use in br_if
- Combined with class pointer comparison for exact match

**Pattern**:
```wat
;; Check if instance's class is or inherits from target-class
(func $instance-of (param $inst (ref $instance)) (param $target (ref $standard-class)) (result i32)
  (local $current (ref null $standard-class))
  (local.set $current (struct.get $instance $class (local.get $inst)))
  (block $found (result i32)
    (loop $check
      ;; Exact class match?
      (br_if $found (i32.const 1)
        (ref.eq (local.get $current) (local.get $target)))
      ;; Move to superclass
      (local.set $current
        (struct.get $standard-class $superclass (local.get $current)))
      ;; If null, not found
      (br_if 1 (ref.is_null (local.get $current)))
      (br $check))
    (i32.const 0)))
```

## Resolved Clarifications

### Q1: Built-in Type Specialization

**Question**: Can methods specialize on built-in types like `fixnum`, `string`, `cons`?

**Decision**: Yes, limited set of built-in types supported for specialization

**Implementation**:
- Built-in types have pseudo-class objects for dispatch
- Use `ref.test` against known type indices (i31ref for fixnum, type 4 for string, etc.)
- Built-in types form a parallel hierarchy rooted at `t`

```
t
├── fixnum (i31ref)
├── cons ($cons)
├── symbol ($symbol)
├── string ($string)
├── instance ($instance) ← user classes
└── ... other built-ins
```

### Q2: Initform Evaluation Timing

**Question**: Are initforms evaluated at class definition or each make-instance call?

**Decision**: Evaluated at each `make-instance` call (per ANSI CL spec)

**Implementation**:
- Store initforms as closures in class metadata
- Call closure during slot initialization
- Closures capture lexical environment at defclass time

### Q3: Class Redefinition

**Question**: What happens when a class is redefined?

**Decision**: Simplified model for Phase 1 - replace class object, existing instances unchanged

**Implementation**:
- New class global overwrites old reference
- Existing instances keep old class pointer (stale but safe)
- Full ANSI class redefinition protocol deferred to future phase

## Summary of Key Decisions

| Topic | Decision | Key Benefit |
|-------|----------|-------------|
| Instance representation | Single $instance type + class pointer | Runtime class flexibility |
| Class metadata | Wasm globals with struct values | Fast O(1) lookup |
| Method dispatch | Class-ID indexed tables + superclass walk | O(1) common case |
| Accessors | Inlined slot access | Performance |
| Slot initialization | CPL order with initarg override | ANSI compliance |
| Type checking | ref.test + class pointer walk | WasmGC native |
