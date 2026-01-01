# Research: Compiler Internal Function Consolidation

**Date**: 2026-01-01
**Feature**: 001-compiler-internal-consolidation

## Research Questions

### RQ1: Why do ENV-ADD-LOCAL and COMPILE-TO-INSTRUCTIONS appear as blockers?

**Finding**: These functions ARE defined in the host compiler (`func-section.lisp:10-434`), but they cannot be compiled TO Wasm because:

1. **Host-specific constructs**: Uses `defstruct` with `:conc-name`, `assoc`, `push` in ways that require macroexpansion
2. **Circular dependency**: The functions that compile code ARE the code that needs to be compiled
3. **Data structure representation**: `compilation-env` struct uses host Lisp lists, not WasmGC arrays

**Decision**: Create separate Wasm-compilable versions with `-wasm` or `*` suffix that use only primitives already compilable to Wasm.

**Rationale**: This approach allows the host compiler to continue using optimized native versions while the Stage 1 compiler uses simplified Wasm-compatible versions.

**Alternatives Considered**:
- Refactor original functions (rejected: breaks existing compilation pipeline)
- Use FFI for all environment operations (rejected: too much overhead for inner-loop functions)

---

### RQ2: What is the runtime function table pattern?

**Finding**: The pattern in `func-section.lisp:66-148` is proven and working:

```lisp
;; Registration
(defparameter *runtime-function-table* (make-hash-table :test 'eq))

(defun register-runtime-function (symbol runtime-name &optional arity)
  (setf (gethash symbol *runtime-function-table*) (cons runtime-name arity)))

;; Dispatch check in compile-call
(when (runtime-function-p function-symbol)
  (return-from compile-call (compile-runtime-call function-symbol args env)))
```

**CORRECTION (2026-01-01)**: Investigation revealed that:
1. The registration functions (`register-io-runtime-functions`, `register-list-runtime-functions`) are DEFINED but NEVER CALLED
2. The `*runtime-function-table*` is EMPTY at runtime
3. The inline `compile-*` functions are the ONLY working path for these operations
4. The runtime library files (list-runtime.lisp, io-runtime.lisp) are NOT included in Stage 1 output

**Decision**: DEFER runtime migration until:
1. Runtime library files are included in Stage 1 compilation
2. Registration calls are added at load time
3. Runtime functions are exported in the Wasm module

**Rationale**: Enabling runtime dispatch without the runtime library causes compilation failures. The inline functions must remain active until the full migration path is implemented.

**Alternatives Considered**:
- Create new dispatch mechanism (rejected: existing one works when properly initialized)
- Keep inline and runtime in parallel (selected: current state, not redundant as originally thought)

---

### RQ3: How should FFI stubs for package operations be implemented?

**Finding**: The existing FFI infrastructure in `src/clysm/ffi/types.lisp` provides:
- `MarshalType` for type mapping (`:fixnum`, `:string`, `:anyref`, etc.)
- `ForeignFunctionDecl` for import declarations
- Host-shim pattern in `host-shim/` directory

**Decision**: Create package stubs following the io-runtime.lisp pattern:

```lisp
;; package-stubs.lisp
(defun find-package* (name)
  (%host-find-package name))

(defun intern* (name &optional (package *package*))
  (%host-intern name package))

(defun packagep* (obj)
  (%host-packagep obj))
```

**Rationale**: Consistent with existing FFI pattern. Package operations are infrequent; FFI overhead is acceptable.

**Alternatives Considered**:
- Native Wasm package implementation (rejected: scope too large, not needed for Stage 1)
- Compile-time-only package resolution (rejected: some uses are truly runtime)

---

### RQ4: Is dynamic type construction needed?

**Finding**: The existing type indices 0-28 cover all current needs:
- Cons, Symbol, String, Closure, Instance, Standard-class
- Function signatures, binding frames, streams
- Hash tables, multidimensional arrays, etc.

**Decision**: Defer dynamic type construction. Implement `make-wasm-struct-type` as a placeholder that returns existing type indices or signals an error for unknown types.

**Rationale**: Stage 1 compilation doesn't require new types. Adding dynamic type construction increases complexity without immediate benefit.

**Alternatives Considered**:
- Full dynamic type registration (deferred: not needed for 30% compilation target)
- Hard-coded type mapping (selected: simpler, sufficient for current needs)

---

### RQ5: What is the safest migration order?

**Finding**: Based on dependency analysis:

1. ~~**List function inline removal**~~ **(DEFERRED: requires runtime library in Stage 1)**
2. **Package stubs** (isolated FFI calls, low risk)
3. **Environment functions** (medium risk, affects closure compilation)
4. **Type construction** (medium risk, but can be deferred)

**Decision (REVISED 2026-01-01)**: Skip list migration, proceed with package stubs first.

**Rationale**: Package stubs are self-contained FFI wrappers with no dependency on runtime library. Environment functions directly address top blockers (ENV-ADD-LOCAL).

---

## Resolved Unknowns

| Unknown | Resolution | Confidence |
|---------|------------|------------|
| ENV-ADD-LOCAL blocker cause | Create Wasm-compilable version | HIGH |
| COMPILE-TO-INSTRUCTIONS blocker | Create dispatcher stub or simplify | MEDIUM |
| Runtime function pattern | Use existing *runtime-function-table* | HIGH |
| FFI pattern for packages | Follow io-runtime.lisp pattern | HIGH |
| Type construction need | Defer, use existing indices | HIGH |
| Migration order | List → Package → Env → Types | HIGH |

## Best Practices Identified

### From io-runtime.lisp Pattern

1. Define `-rt` suffix functions in `lib/` directory
2. Register in `*runtime-function-table*` with runtime name and optional arity
3. Compile-time check via `runtime-function-p` before inline generation
4. `compile-runtime-call` handles argument compilation and call emission

### From FFI Infrastructure

1. Use `%host-*` prefix for FFI function calls
2. Marshal types explicitly in function signatures
3. Host shim implementation in JavaScript (`host-shim/`)
4. Test FFI calls independently before integration

### From Type System

1. All type indices are constants in `gc-types.lisp`
2. Use `+type-*+` naming convention
3. Type checks via `ref.test`, casts via `ref.cast`
4. Field access via `struct.get` with type and field indices

## Implementation Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Regression in existing forms | LOW | HIGH | Full Stage 1 diff before/after |
| Closure compilation breaks | MEDIUM | HIGH | Unit tests for nested lambdas |
| FFI call failures | LOW | MEDIUM | Host shim testing in Node.js |
| Wasm validation errors | MEDIUM | MEDIUM | Validate after each change |

---

## Critical Discovery: Forward Reference Limitation (2026-01-01)

### Root Cause

The Stage 1 compiler has a **forward reference limitation** that prevents reducing blocker counts even when replacement functions are implemented.

**Technical Details**:
1. `compile-regular-call` (func-section.lisp:8128) uses `env-lookup-function` to resolve function calls
2. Functions must be registered in the compilation environment BEFORE they can be called
3. When compiling file F containing function A that calls function B:
   - If B is defined AFTER A in file F, the call fails with "Undefined function: B"
   - If B is defined in a different file that loads later, same failure

### Affected Blockers

| Blocker | Count | Root Cause |
|---------|-------|------------|
| PACKAGEP* | 25 | Called in reader/package.lisp before definition order allows |
| MAKE-WASM-STRUCT-TYPE | 17 | Defstruct constructor called before available in env |
| ENV-ADD-LOCAL | 137 | Used throughout func-section.lisp in compilation functions |
| COMPILE-TO-INSTRUCTIONS | 39 | Core dispatch function with circular dependencies |

### Implementation Status

Functions implemented but blocked:
- `find-package*`, `intern*`, `packagep*` - src/clysm/lib/package-stubs.lisp
- `type-index-for-name`, `make-wasm-struct-type*` - src/clysm/lib/type-construction.lisp

Host shim created:
- host-shim/package-shim.js

### Required Architectural Changes

To address forward reference limitation:

1. **Two-Pass Compilation**: Pre-register all function names before compiling bodies
2. **Late Binding**: Use runtime dispatch for internal function calls
3. **Dependency-Ordered Loading**: Compile files in topological dependency order
4. **Lazy Resolution**: Defer function lookup to call-time instead of compile-time

### Recommendation

The current blocker analysis approach (defining replacement functions) is **insufficient**.
The compilation rate target (22% → 30%) requires addressing the forward reference limitation first.

**Priority**: Address forward reference limitation before continuing Phase 6-7 implementation.

## References

- [member](resources/HyperSpec/Body/f_mem_m.htm) - ANSI CL member function
- [assoc](resources/HyperSpec/Body/f_assocc.htm) - ANSI CL assoc function
- [find](resources/HyperSpec/Body/f_find_.htm) - ANSI CL find function
- [position](resources/HyperSpec/Body/f_pos_p.htm) - ANSI CL position function
- [find-package](resources/HyperSpec/Body/f_find_p.htm) - ANSI CL find-package function
- [intern](resources/HyperSpec/Body/f_intern.htm) - ANSI CL intern function
- [packagep](resources/HyperSpec/Body/f_pkgp.htm) - ANSI CL packagep predicate
