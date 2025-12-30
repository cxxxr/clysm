# Research: Phase 13D-4 Global Variable Definitions

**Date**: 2025-12-30
**Researcher**: Explore Agent

## Summary

Research reveals that the Clysm codebase contains **142+ unique global variables** (more than the originally estimated 58). Existing infrastructure for global variable handling is well-established in `func-section.lisp` and `stage0/globals.lisp`, but comprehensive [defvar](../../resources/HyperSpec/Body/m_defpar.htm)/[defparameter](../../resources/HyperSpec/Body/m_defpar.htm) compilation to Wasm is not yet complete.

## Key Decisions

### D1: Global Index Allocation Strategy

**Decision**: Continue using existing allocation starting at index 4

**Rationale**: Indices 0-3 are reserved by the runtime:
| Index | Name | Purpose |
|-------|------|---------|
| 0 | `$nil` | NIL singleton (immutable) |
| 1 | `$unbound` | UNBOUND sentinel (immutable) |
| 2 | `$mv_count` | Multiple values count (mutable, i32) |
| 3 | `$mv_buffer` | Multiple values storage (mutable, ref $mv_array) |
| 4+ | Special variables | User-defined (mutable, ref null any) |

**Alternatives considered**:
- Starting at 0: Rejected - conflicts with reserved runtime globals
- Sparse allocation: Rejected - adds complexity without benefit

### D2: Global Variable Type Strategy

**Decision**: Use `(ref null any)` for all special variables

**Rationale**: Provides maximum flexibility for heterogeneous Lisp values. The type hierarchy uses `anyref` as universal root per Constitution Principle I.

**Alternatives considered**:
- Type-specific globals (i32 for counters, etc.): Rejected - would require complex type tracking and break uniform Lisp semantics
- `(ref any)` non-nullable: Rejected - special variables may be initialized to NIL

### D3: Initialization Strategy

**Decision**: Two-phase initialization pattern

1. **Constant expressions**: Initialize directly in global declaration for literals (integers, strings, NIL)
2. **Deferred initialization**: Use module `$init` function for complex forms (hash-tables, function calls)

**Rationale**: Wasm globals require constant init expressions. Complex Lisp initializers must be evaluated at module instantiation time.

**Alternatives considered**:
- All deferred: Rejected - wasteful for simple constants
- Lazy initialization: Rejected - adds runtime overhead and complexity

### D4: defvar vs defparameter Semantics

**Decision**: Implement ANSI CL semantics exactly

| Form | Behavior |
|------|----------|
| [defvar](../../resources/HyperSpec/Body/m_defpar.htm) | Declare + initialize only if unbound |
| [defparameter](../../resources/HyperSpec/Body/m_defpar.htm) | Declare + always initialize |

**Rationale**: ANSI CL compliance per Constitution Principle IX.

## Global Variable Categories (Complete Census)

### Runtime Registries (~12 vars)
- `*function-registry*` - Compiled function lookup
- `*macro-registry*` - Runtime macro dispatch
- `*global-macro-registry*` - Compile-time macro registry
- `*runtime-macro-registry*` - Evaluator macro registry
- `*gf-registry*` - Generic function registry
- `*dispatch-cache*` - Method dispatch cache
- `*class-registry*` - CLOS class registry
- `*method-combinations*` - Method combination registry
- `*symbol-table*` - Symbol interning
- `*primitives*` - Primitive operator registry
- `*global-functions*` - Stage0 compiled functions
- `*generic-function-registry*` - GF metadata

### I/O Streams (~4 vars)
- `*standard-input*` - stdin reference
- `*standard-output*` - stdout reference
- `*error-output*` - stderr reference
- `*format-column*` - Format position tracking

### Condition System (~2 vars)
- `*handler-clusters*` - Exception handler stack
- `*restart-clusters*` - Restart information stack

### Multiple Values & Binding (~7 vars)
- `*mv-count*` - Return value count
- `*mv-buffer*` - Additional values array
- `*binding-stack*` - Dynamic binding stack
- Index tracking variables for runtime

### Package System (~2 vars)
- `*packages*` - Package registry
- `*current-package*` - Current compilation package

### Compilation State (~15 vars)
- `*compile-env*` - Current compilation environment
- `*compiled-functions*` - List of compiled function defs
- `*compilation-threshold*` - JIT threshold
- Various counters and caches

### JIT & Interpreter (~6 vars)
- `*gc-heap*` - GC heap reference
- `*heap-counter*` - Allocation counter
- `*function-slots*` - Closure environment
- Interpreter state

### FFI & File I/O (~9 vars)
- `*fs-open*`, `*fs-read-all*`, etc. - FFI function names
- Type signature variables

### Additional Categories (~85 vars)
- Macro system variables
- REPL & debugging
- Path & configuration
- Logging & reporting
- Compiler features & metadata
- Testing & validation

## Existing Code Patterns

### Global Allocation (func-section.lisp:7764-7796)
```lisp
(defvar *special-var-globals* (make-hash-table :test 'eq))
(defvar *next-special-global-index* 2)

(defun allocate-special-var-global (name)
  (or (gethash name *special-var-globals*)
      (setf (gethash name *special-var-globals*)
            (prog1 *next-special-global-index*
              (incf *next-special-global-index*)))))
```

### Global Section Generation (stage0/globals.lisp)
```lisp
(defun generate-all-globals ()
  (list
   (encode-global (global-nil-type) +immutable+ (init-nil-global))
   (encode-global (global-unbound-type) +immutable+ (init-unbound-global))
   (encode-global (global-mv-count-type) +mutable+ (init-mv-count-global))
   (encode-global (global-mv-buffer-type) +mutable+ (init-mv-buffer-global))))
```

### Key Constants (stage0/types.lisp)
```lisp
(defconstant +immutable+ #x00)
(defconstant +mutable+ #x01)
(defconstant +wasm-i32+ #x7F)
(defconstant +non-null+ #x64)
(defconstant +gc-prefix+ #xFB)
```

## Integration Points

1. **Compiler reset**: `*special-var-globals*` cleared on each compilation (compiler.lisp:87)
2. **AST parsing**: Pre-allocation during form parsing (compiler.lisp:97-102)
3. **Code generation**: Global index lookup during codegen (func-section.lisp)
4. **Wasm emission**: Global section output (stage0/globals.lisp)

## Best Practices Identified

1. **Hash table test**: Use `'eq` for symbol keys (fast pointer comparison)
2. **Index tracking**: Centralized counter for deterministic allocation
3. **Mutability**: All special variables are mutable for dynamic binding
4. **Type uniformity**: `(ref null any)` avoids type complexity
5. **Constitution compliance**: Shallow binding via symbol `$value` slot

## Gaps to Address

1. **defvar/defparameter form compilation**: Not currently generating global declarations from source forms
2. **Deferred initialization**: `$init` function not generating init code for complex forms
3. **Dynamic binding codegen**: `let`/`let*` not emitting binding stack operations
4. **Global lookup codegen**: Variable references not always resolving to `global.get`

## Files to Modify

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/globals.lisp` | NEW - defvar/defparameter compilation |
| `src/clysm/compiler/codegen/func-section.lisp` | Extend global allocation |
| `src/clysm/stage0/globals.lisp` | Extend global section generation |
| `src/clysm/compiler/ast.lisp` | AST node for global declarations |
| `tests/unit/globals-test.lisp` | NEW - unit tests |
| `tests/contract/wasm-globals-test.lisp` | NEW - Wasm validation tests |
