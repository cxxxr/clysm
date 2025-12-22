# Research: Special Variables Compiler Integration

**Date**: 2025-12-22
**Feature**: 002-special-vars-compiler

## Overview

This document consolidates research findings for implementing special variables (dynamic scope) in Clysm. Key decisions are pre-determined by Constitution Principle V (シャローバインディングによる動的スコープ).

---

## Decision 1: Binding Strategy

**Decision**: Shallow Binding

**Rationale** (from Constitution V):
- Wasm does not allow stack walking, requiring explicit binding management
- Shallow binding provides O(1) read performance (direct `$value` field access)
- Shallow binding is the standard approach for modern Lisp implementations (SBCL, CCL)

**Alternatives Considered**:

| Strategy | Read Time | Bind Time | Restore Time | Rejected Because |
|----------|-----------|-----------|--------------|------------------|
| Deep Binding | O(n) | O(1) | O(1) | Read performance unacceptable for frequently accessed variables |
| **Shallow Binding** | O(1) | O(1) | O(1) | **Selected** |
| Association List | O(n) | O(1) | O(1) | Equivalent to deep binding, poor read performance |

---

## Decision 2: Binding Stack Representation

**Decision**: WasmGC struct linked list (global variable)

**Rationale**:
- WasmGC `struct` types integrate with GC for automatic memory management
- Linked list allows O(1) push/pop operations
- Global variable for binding stack top supports single-threaded execution model

**Implementation Pattern**:
```wat
;; Binding frame structure
(type $binding_frame (struct
  (field $symbol (ref $symbol))    ;; The bound symbol
  (field $old_value anyref)        ;; Previous value
  (field $prev (ref null $binding_frame))))  ;; Link to previous frame

;; Global binding stack top
(global $binding_stack (mut (ref null $binding_frame)) (ref.null $binding_frame))
```

**Alternatives Considered**:

| Approach | Pros | Cons | Rejected Because |
|----------|------|------|------------------|
| **Linked List** | GC-managed, O(1) ops | Extra struct allocation | **Selected** - GC overhead acceptable |
| WasmGC Array | Cache-friendly | Fixed size or complex resizing | Unbounded nesting depth requirement |
| Linear Memory | Fast | Not GC-integrated, Constitution violation | MUST NOT use linear memory |

---

## Decision 3: defvar vs defparameter Semantics

**Decision**: Follow ANSI Common Lisp specification exactly

**defvar Behavior**:
1. If symbol is unbound, evaluate init-form and store in `$value`
2. If symbol is already bound, do NOT re-evaluate or change value
3. Mark symbol as special in compilation environment

**defparameter Behavior**:
1. Always evaluate init-form and store in `$value`
2. Mark symbol as special in compilation environment

**Rationale**: Standard compliance ensures expected behavior for Common Lisp programmers.

**Implementation Pattern**:
```lisp
;; defvar codegen pseudocode
(if (unbound-p symbol)
    (setf (symbol-value symbol) init-form-result))

;; defparameter codegen pseudocode
(setf (symbol-value symbol) init-form-result)
```

---

## Decision 4: Special Variable Registry Scope

**Decision**: Compilation-unit scoped registry with global persistence model

**Rationale**:
- Registry must track which symbols are declared special
- Information needed at compile-time for code generation decisions
- Global persistence allows cross-file compilation (future)

**Implementation**:
- Add `*special-variables*` hash-table to compilation environment
- Populated by `defvar`/`defparameter` during AST parsing
- Queried by `compile-var-ref` and `compile-let` to determine binding type

---

## Decision 5: Dynamic Binding in LET

**Decision**: Wrap dynamic bindings with implicit unwind-protect

**Rationale** (from Constitution V.4):
- Binding restoration MUST occur even on non-local exit
- Leverages existing `try_table`/`unwind-protect` implementation
- Ensures exception safety per Constitution IV

**Code Generation Pattern**:
```wat
;; (let ((*special* new-value)) body)
;; Compiles to:
(local.get $symbol)
(struct.get $symbol $value)        ;; Get old value
(global.get $binding_stack)
(struct.new $binding_frame)        ;; Create frame with (symbol, old, prev)
(global.set $binding_stack)        ;; Push frame

(local.get $symbol)
<compile new-value>
(struct.set $symbol $value)        ;; Set new value

(try_table
  <compile body>
  (catch_all
    ;; Restore on exception
    <restore-binding>
    (throw_ref)))

;; Normal exit restoration
<restore-binding>
```

**Restoration Sequence**:
```wat
;; restore-binding helper
(global.get $binding_stack)
(struct.get $binding_frame $old_value)  ;; Get old value
(global.get $binding_stack)
(struct.get $binding_frame $symbol)
(struct.set $symbol $value)              ;; Restore to symbol
(global.get $binding_stack)
(struct.get $binding_frame $prev)
(global.set $binding_stack)              ;; Pop frame
```

---

## Decision 6: Unbound Variable Detection

**Decision**: Use UNBOUND sentinel object (per Constitution II)

**Rationale**:
- Constitution II specifies UNBOUND as internal sentinel
- `(eq value UNBOUND)` check for unbound-variable error
- UNBOUND is not accessible from Lisp level

**Error Handling**:
- On read: Check for UNBOUND, signal `unbound-variable` condition
- Signal mechanism: Use existing exception infrastructure

---

## Decision 7: Symbol-Value Access Code Generation

**Decision**: Direct struct.get for special variables

**Pattern**:
```wat
;; (symbol-value 'foo) or special variable reference
(global.get $symbol-foo)           ;; Get symbol reference
(struct.get $symbol $value)        ;; Get value field (field index 1)
;; Optional: UNBOUND check
(ref.eq UNBOUND)
(if (then (call $signal-unbound-error)))
```

**Rationale**:
- O(1) access as specified by Constitution V
- No stack traversal needed (shallow binding advantage)
- UNBOUND check optional at compile-time (can be elided for known-bound variables)

---

## Best Practices Applied

### From Constitution

1. **TDD (VII)**: Write tests before implementation
   - Unit tests for AST nodes
   - Contract tests for generated Wasm structure
   - Integration tests for end-to-end behavior

2. **Nix-First (VIII)**: Ensure `nix flake check` passes at each step

3. **WasmGC-First (I)**: All structures use WasmGC types, no linear memory

### From Common Lisp Community

1. **Shallow binding precedent**: SBCL, CCL use similar approaches
2. **Trail/stack naming**: Standard terminology in Lisp implementation literature
3. **UNBOUND sentinel**: Standard pattern (SBCL uses similar approach)

---

## Open Questions Resolved

| Question | Resolution |
|----------|------------|
| Shallow vs Deep binding? | Shallow (Constitution V) |
| Stack structure? | WasmGC linked list |
| Thread safety? | Single-threaded (WasmGC limitation) |
| defvar re-initialization? | ANSI CL: No re-init if bound |
| Exception handling? | Leverage existing unwind-protect |

---

## References

- Clysm Constitution v1.0.0, Principle V
- ANSI Common Lisp Specification (defvar, defparameter, special declarations)
- WasmGC Proposal: https://github.com/WebAssembly/gc
- "Lisp in Small Pieces" - Christian Queinnec (binding strategies)
