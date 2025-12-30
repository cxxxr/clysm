# Research: make-instance* Primitive Implementation

**Date**: 2025-12-31
**Feature**: 001-make-instance-primitive
**Status**: Complete

## Research Summary

This research documents findings for implementing `make-instance*` as a compiler primitive.

---

## R1: How primitives are registered and dispatched

**Question**: How does the Clysm compiler recognize and dispatch primitive function calls?

**Finding**: Primitives are handled in `compile-call` (func-section.lisp:712-853):
1. A large member list (lines 724-844) contains all primitive symbols
2. When a call matches a primitive, `compile-primitive-call` is invoked
3. `compile-primitive-call` (line 913) uses a `case` dispatch to the specific compiler function

**Decision**: Add `make-instance*` to the primitive list and implement `compile-make-instance*` in the `case` dispatch.

**Rationale**: Follows the established pattern used by 100+ existing primitives.

**Alternatives considered**:
- AST-based dispatch (like `ast-make-instance`): More complex, requires parser changes
- Regular function call: Would require runtime linking, not suitable for self-hosting

---

## R2: WasmGC type structure for instances

**Question**: What WasmGC types are needed to create CLOS instances?

**Finding**: Instance creation requires three types (gc-types.lisp):
- `$instance` (type index 6): `(struct (field $class ref) (field $slots ref))`
- `$standard-class` (type index 7): Class metadata with slot count, initargs, initforms
- `$slot-vector` (type index 21): `(array (mut anyref))` for slot storage

**Decision**: Use existing type definitions; emit `struct.new` and `array.new` instructions.

**Rationale**: Types are already defined and used by existing CLOS infrastructure.

**Alternatives considered**:
- New type for structures only: Unnecessary complexity, structures ARE CLOS instances

---

## R3: How DEFSTRUCT generates make-instance* calls

**Question**: What exact form does DEFSTRUCT expand to when calling make-instance*?

**Finding**: DEFSTRUCT expansion (defstruct.lisp:227, 273) generates:
```lisp
(clysm/clos/instance:make-instance*
  'CLASS-NAME
  :slot1 value1
  :slot2 value2
  ...)
```

**Decision**: Support both package-qualified and unqualified `make-instance*` symbols.

**Rationale**: The primitive list uses symbol-name matching for cross-package compatibility.

**Alternatives considered**:
- Package-specific matching only: Would miss calls from other packages

---

## R4: Existing compile-make-instance implementation

**Question**: What does the existing `compile-make-instance` do and how does it differ?

**Finding**: `compile-make-instance` (func-section.lisp:17120-17150):
- Handles `ast-make-instance` special form (from parser)
- Currently returns `(:ref.null :none)` as placeholder
- Has compile-time class validation
- Does NOT handle function call form from DEFSTRUCT expansion

**Decision**: Implement `compile-make-instance*` as new function, potentially reusing class lookup logic.

**Rationale**: Function call form (`make-instance* 'name ...)`) differs from special form.

**Alternatives considered**:
- Extend compile-make-instance: Would conflate two different call conventions

---

## R5: Runtime semantics of make-instance*

**Question**: What runtime behavior must the generated Wasm implement?

**Finding**: `make-instance*` (clos/instance.lisp:9-23):
1. Resolve class name to class object via `find-class*`
2. Get slot count from class metadata
3. Create slot vector of correct size
4. Initialize slots from initargs (using slot initargs from class)
5. Apply initforms for uninitialized slots
6. Return the instance

**Decision**: For Stage 1 bootstrap, implement simplified version:
- Class lookup via compile-time registry (already exists)
- Slot initialization with default values (nil)
- Full initarg/initform support deferred to Stage 2

**Rationale**: Achieving compilation success is the immediate goal; runtime semantics can be refined.

**Alternatives considered**:
- Full runtime semantics: Too complex for initial implementation, blocks progress

---

## R6: Symbol matching pattern for primitives

**Question**: How do existing primitives handle cross-package symbol matching?

**Finding**: The primitive list uses `:test` with `symbol-name` comparison (line 845-846):
```lisp
:test (lambda (fn sym)
        (string= (symbol-name fn) (symbol-name sym)))
```

This allows `MAKE-INSTANCE*` from any package to match.

**Decision**: Add `make-instance*` to the primitive list; symbol-name matching handles packages.

**Rationale**: Consistent with existing pattern for `%setf-aref`, `gethash`, etc.

**Alternatives considered**:
- Explicit package check: Unnecessary given existing matching mechanism

---

## Conclusion

All research items resolved. No NEEDS CLARIFICATION items remain.

**Implementation approach**:
1. Add `make-instance*` to primitive list (after existing CLOS primitives)
2. Add dispatch case in `compile-primitive-call`
3. Implement `compile-make-instance*` that:
   - Compiles class-name argument (quoted symbol → global lookup)
   - Compiles initarg pairs
   - Emits `struct.new $instance` with class ref and slot vector
4. Validate with DEFSTRUCT compilation tests
