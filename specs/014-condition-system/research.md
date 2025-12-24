# Research: ANSI Common Lisp Condition System

**Date**: 2025-12-24
**Feature**: 014-condition-system

## Executive Summary

This research documents architectural decisions for implementing the ANSI Common Lisp Condition System in clysm3. The system leverages existing infrastructure (catch/throw, unwind-protect, WasmGC exception handling, special variable shallow binding) while adding condition types, handlers, and restarts.

## Research Topics

### 1. Exception Handling Infrastructure (Existing)

**Decision**: Extend existing `catch`/`throw`/`unwind-protect` infrastructure.

**Rationale**: The current implementation already provides:
- Wasm exception tag `$lisp-throw` (tag index 0) with `(anyref, anyref)` payload
- `try_table` with `catch` and `catch_all_ref` clauses
- `throw` and `throw_ref` instructions for exception propagation
- Compilation environment tracks `catch-tags` and `unwind-stack`

**Existing Code References**:
- `src/clysm/compiler/compiler.lisp:484-497` - Tag section with `$lisp-throw`
- `src/clysm/compiler/codegen/func-section.lisp:2730-2853` - `compile-catch`/`compile-throw`
- `src/clysm/compiler/codegen/func-section.lisp:2873-2940` - `compile-unwind-protect`

**Alternatives Considered**:
- Separate exception mechanism for conditions: Rejected - duplicates infrastructure
- Pure control flow without exceptions: Rejected - violates Constitution IV

### 2. Handler Stack Management

**Decision**: Use shallow binding pattern mirroring special variables.

**Rationale**:
- Constitution V mandates shallow binding for dynamic scope
- Special variables use `*binding-stack*` with push/pop semantics
- Handler stacks can use identical pattern with global handler cluster variable

**Implementation Pattern**:
```lisp
;; Global handler cluster stack (compiled to global variable)
(defvar *handler-clusters* nil)

;; Push cluster at handler-bind/handler-case entry
(push (make-handler-cluster handlers) *handler-clusters*)

;; Pop cluster at exit (via unwind-protect)
(pop *handler-clusters*)
```

**Existing Code Reference**:
- `src/clysm/runtime/special-vars.lisp:1-18` - Shallow binding pattern

**Alternatives Considered**:
- Deep binding (search stack on each access): Rejected - O(n) vs O(1)
- Thread-local storage: Rejected - WasmGC has no threads yet

### 3. Restart Stack Management

**Decision**: Separate stack from handlers, same shallow binding pattern.

**Rationale**:
- Restarts have different lifetime than handlers (persist during handler execution)
- ANSI CL allows restarts to be found by condition association
- Restart stacks need to track restart objects with names, functions, and reports

**Implementation Pattern**:
```lisp
;; Global restart cluster stack
(defvar *restart-clusters* nil)

;; restart-case establishes restarts, returns catch tag
;; invoke-restart uses throw to transfer control
```

**Alternatives Considered**:
- Combined handler/restart stack: Rejected - different semantics
- Restart per handler: Rejected - restarts can exist without handlers

### 4. Condition Class Hierarchy

**Decision**: Use existing CLOS infrastructure with standard condition metaclass.

**Rationale**:
- CLOS is already implemented (`src/clysm/clos/`)
- Conditions are standard CLOS classes with special printing
- Multiple inheritance needed for `simple-error`, `simple-warning`

**WasmGC Representation**:
```wat
;; Condition is a CLOS instance (struct with class pointer + slots)
(type $condition-instance (struct
  (field $class (ref $class))       ;; Condition class
  (field $slots (ref $slot-vector)) ;; Instance slots
))
```

**Existing Code Reference**:
- `src/clysm/clos/instance.lisp` - CLOS instance structure
- `src/clysm/clos/defclass.lisp` - Class definition

**Alternatives Considered**:
- Separate condition structs: Rejected - duplicates CLOS
- Tagged unions: Rejected - loses extensibility

### 5. Handler Dispatch Mechanism

**Decision**: Linear search through handler clusters, type-based matching.

**Rationale**:
- Handler clusters are searched innermost-to-outermost
- Type matching uses `typep` (condition type hierarchy)
- Handler functions are closures that may transfer control or decline

**Dispatch Algorithm**:
```lisp
(defun find-handler (condition)
  "Find applicable handler for condition."
  (dolist (cluster *handler-clusters*)
    (dolist (handler (handler-cluster-handlers cluster))
      (when (typep condition (handler-condition-type handler))
        (return-from find-handler handler))))
  nil)
```

**Alternatives Considered**:
- Hash table dispatch: Rejected - needs exact type, not inheritance
- Caching: Deferred - premature optimization

### 6. Control Transfer for handler-case vs handler-bind

**Decision**:
- `handler-case`: Establish catch tag, handler body throws to it
- `handler-bind`: Handler function is called, may return (decline) or invoke restart

**Rationale**:
- ANSI CL specifies different semantics for these macros
- `handler-case` unwinds before handler execution
- `handler-bind` keeps stack intact during handler execution

**Implementation**:
```lisp
;; handler-case compiles to:
(catch 'handler-case-tag
  (handler-bind ((error-type
                  (lambda (c)
                    (throw 'handler-case-tag (progn ,@handler-body)))))
    ,@protected-forms))

;; handler-bind compiles to:
(let ((*handler-clusters* (cons (make-cluster ...) *handler-clusters*)))
  (unwind-protect
      (progn ,@protected-forms)
    (pop *handler-clusters*)))
```

**Alternatives Considered**:
- Same mechanism for both: Rejected - violates ANSI semantics

### 7. Restart Invocation Mechanism

**Decision**: Use existing `throw` mechanism with restart-specific catch tag.

**Rationale**:
- Restarts need non-local control transfer
- Existing `catch`/`throw` provides this exactly
- Each `restart-case` establishes a unique catch tag

**Implementation**:
```lisp
;; restart-case compiles to:
(let ((restart-tag (gensym "restart")))
  (catch restart-tag
    (let ((*restart-clusters*
           (cons (make-restart-cluster
                  (list (make-restart :name 'use-value
                                      :function (lambda (v) (throw restart-tag v)))))
                 *restart-clusters*)))
      ,@protected-forms)))

;; invoke-restart finds restart and calls its function
(defun invoke-restart (restart-designator &rest args)
  (let ((restart (find-restart restart-designator)))
    (apply (restart-function restart) args)))
```

**Alternatives Considered**:
- Direct goto: Rejected - Wasm has no goto
- Continuation capture: Rejected - complex, no benefit

### 8. Signaling Functions Implementation

**Decision**: Implement `signal`, `warn`, `error`, `cerror` as runtime functions.

**Rationale**:
- These are function calls, not special forms
- They search handler stacks and invoke handlers
- `error` enters debugger (trap) if unhandled

**Behavior Matrix**:

| Function | Handler Found | Handler Declines | No Handler |
|----------|--------------|------------------|------------|
| `signal` | Call handler | Continue search | Return NIL |
| `warn`   | Call handler | Continue search | Print to `*error-output*`, return NIL |
| `error`  | Call handler | Continue search | Enter debugger (trap) |
| `cerror` | Call handler | Continue search | Print continue string, enter debugger |

**Unhandled Error Behavior**:
For WasmGC target without interactive debugger:
1. Print formatted error message to `*error-output*`
2. Invoke top-level `abort` restart if established
3. Execute WebAssembly `unreachable` (trap)

**Alternatives Considered**:
- Special forms: Rejected - standard library functions
- Always trap on error: Rejected - loses `abort` restart capability

### 9. Standard Restarts Implementation

**Decision**: Define standard restarts as symbols with expected behaviors.

**Standard Restart Protocols**:

| Restart | Args | Behavior |
|---------|------|----------|
| `abort` | 0 | Non-local exit to established abort point |
| `continue` | 0 | Continue execution from `cerror` |
| `muffle-warning` | 0 | Suppress warning, return NIL from `warn` |
| `use-value` | 1 | Use provided value instead of erroneous one |
| `store-value` | 1 | Store value for future use |

**Alternatives Considered**:
- Only implement used restarts: Rejected - partial ANSI compliance

### 10. Macro Expansion Strategy

**Decision**: Implement condition system macros as compile-time expansions.

**Macro List**:
- `handler-case` → handler-bind + catch + throw
- `handler-bind` → let + unwind-protect
- `restart-case` → catch + let + restart cluster
- `restart-bind` → let + unwind-protect
- `with-simple-restart` → restart-case with standard expansion

**Rationale**:
- Macros reduce runtime overhead
- Expansions use existing primitives
- Debugging easier with explicit structure

**Alternatives Considered**:
- Runtime interpretation: Rejected - slower, more complex

## Dependencies

### Existing Infrastructure (Required)
1. `catch`/`throw` with WasmGC exception handling ✓
2. `unwind-protect` with cleanup execution ✓
3. Special variable shallow binding ✓
4. CLOS for condition class hierarchy ✓

### New Components (To Implement)
1. Condition type definitions (types.lisp)
2. Handler stack management (handlers.lisp)
3. Restart stack management (restarts.lisp)
4. Signaling functions (signaling.lisp)
5. Standard restarts (standard.lisp)
6. Macro expansions (lib/macros.lisp extension)
7. AST nodes for handler-case, restart-case (ast.lisp extension)
8. Codegen for condition system forms (func-section.lisp extension)

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Handler stack leaks on non-local exit | Medium | High | Use unwind-protect for all stack operations |
| CLOS condition types not fully compatible | Low | Medium | Follow ANSI spec exactly for slots |
| Performance impact on normal code | Low | Low | Handler stacks only checked on signal |
| Recursive error signaling | Medium | High | Unbind handlers during handler execution |

## Conclusions

The condition system can be implemented by:
1. Extending existing `catch`/`throw`/`unwind-protect` infrastructure
2. Adding handler and restart stacks using shallow binding pattern
3. Defining condition classes via CLOS
4. Implementing signaling functions as runtime library
5. Providing macros that expand to existing primitives

All architectural decisions align with the constitution and leverage existing infrastructure.
