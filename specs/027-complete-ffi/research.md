# Research: Complete FFI Foundation

**Feature**: 027-complete-ffi
**Date**: 2025-12-27

## Research Topics

### 1. Wasm Exception Handling for FFI Errors

**Context**: FR-005 requires wrapping FFI import calls in try_table/catch to translate host exceptions to Lisp conditions. The current implementation in `import-gen.lisp` has placeholder code.

**Decision**: Use WasmGC exception handling proposal (`try_table` with `catch_all`)

**Rationale**:
- Constitution IV mandates use of Wasm exception handling (try_table/throw/throw_ref)
- `catch_all` catches any exception from host without needing specific tag matching
- Caught exception can be converted to `ffi:ffi-host-error` condition

**Implementation Pattern**:
```wat
;; FFI call with error handling
(block $success (result anyref)
  (try_table (catch_all $on_error)
    ;; marshal arguments
    <marshal-to-wasm instructions>
    ;; call imported function
    (call $imported_func)
    ;; marshal return value
    <marshal-from-wasm instructions>
    (br $success))
  ;; error handler: signal ffi-host-error
  (call $signal-ffi-host-error (i32.const <func-name-idx>))
  (unreachable))
```

**Alternatives Considered**:
- **Manual result codes**: Host returns error codes instead of throwing. Rejected: requires modifying all host functions, doesn't handle unexpected errors.
- **Trap handling**: Let traps propagate. Rejected: crashes entire Wasm module, no recovery possible.

### 2. Compiler Pipeline Integration

**Context**: FFI declarations (from `define-foreign-function`) must be recognized during compilation and generate correct import calls.

**Decision**: Add AST node type for FFI calls and compile them specially in func-section.lisp

**Rationale**:
- Existing compiler already has runtime symbol lookup for FFI (see `emit-import-section-if-needed`)
- Need to recognize calls to FFI-declared functions during AST parsing
- Compile FFI calls directly to Wasm call instructions with marshalling

**Implementation Pattern**:
1. During `parse-expr`, check if function name is in `*ffi-environment*` imports
2. If FFI function: create `ast-ffi-call` node with declaration reference
3. During codegen: emit marshalling + call + unmarshalling using existing FFI generators

**Key Files to Modify**:
- `src/clysm/compiler/ast.lisp`: Add `ast-ffi-call` struct
- `src/clysm/compiler/codegen/func-section.lisp`: Add `compile-ffi-call` function

### 3. Dynamic call-host Implementation

**Context**: FR-006 requires runtime invocation of host functions by name. Currently `call-host` just signals an error.

**Decision**: Implement via host-provided dispatch function that looks up and calls functions by name

**Rationale**:
- Wasm cannot dynamically construct import calls at runtime
- Host environment (JS/wasmtime) must provide dispatch mechanism
- Simplest approach: single `$call_host_dynamic` import that handles lookup

**Implementation Pattern**:
```lisp
;; Lisp side
(ffi:call-host "host.add" 1 2)
;; compiles to:
;; 1. Create argument array (anyref array)
;; 2. Call $call_host_dynamic(name-string, args-array)
;; 3. Unmarshal result

;; Host side (JS)
function call_host_dynamic(name, args) {
  const [module, field] = parseName(name);
  const fn = hostFunctions[module]?.[field];
  if (!fn) throw new Error(`Unknown function: ${name}`);
  return fn(...args);
}
```

**Alternatives Considered**:
- **Function table**: Register all potential host functions in a table. Rejected: requires knowing all functions ahead of time, complex index management.
- **Codegen all variations**: Generate imports for all possible names. Rejected: impossible without knowing names at compile time.

### 4. Re-entrant Callback Support

**Context**: FR-009 requires host code to call back into Lisp during FFI execution (e.g., Lisp calls host, host calls exported Lisp function).

**Decision**: No special handling needed - Wasm naturally supports re-entrant calls through exports

**Rationale**:
- Wasm stack is managed by the runtime, supports nested calls
- Exported functions are regular Wasm functions with proper stack frames
- Each call frame is independent; no shared mutable state conflicts
- Special variables use shallow binding (Constitution V), which handles re-entry via binding stack

**Verification Points**:
- Test depth-3 callbacks (Lisp→Host→Lisp→Host)
- Verify special variable bindings are preserved across re-entrant calls
- Ensure condition handlers work correctly in nested contexts

**Alternatives Considered**:
- **Explicit call stack management**: Track callback depth. Rejected: unnecessary complexity, Wasm handles this.
- **Trampoline**: Queue callbacks for later execution. Rejected: changes semantics, breaks synchronous patterns.

### 5. Type Index Management for FFI

**Context**: FFI function types must be added to the Type section with correct indices.

**Decision**: Use existing `collect-ffi-types` and `emit-ffi-types-to-section` functions, indices start at 23+

**Rationale**:
- compiler.lisp already handles FFI type integration (T058)
- Type indices 0-22 are reserved for built-in types
- FFI types added after regular function types
- `assign-import-indices` already handles index assignment

**Current Type Layout**:
- 0-7: GC types ($nil, $unbound, $cons, $symbol, $string, $closure, reserved×2)
- 8-12: Closure function types ($func_0 through $func_N)
- 13-22: Special types (binding_frame, numeric tower, exception, mv_array)
- 23+: Regular function types, then FFI types

### 6. Marshal Type Edge Cases

**Context**: Edge cases for marshalling identified in spec need specific handling.

**Decision**: Implement runtime type checks with `ffi-type-error` signaling

**Implementation**:
| Edge Case | Handling |
|-----------|----------|
| Fixnum overflow (>31-bit) | Check `(i31-range-p value)` before marshalling; signal `ffi-type-error` |
| nil as string | Marshal as `(ref.null extern)` - valid externref null |
| nil as boolean | Marshal as `(i32.const 0)` - falsy value |
| Invalid externref type | `ref.cast` failure signals `ffi-type-error` |
| Argument count mismatch | Runtime check in call-host; signal error if mismatch |

## Summary of Decisions

| Topic | Decision | Key Rationale |
|-------|----------|---------------|
| Error handling | try_table/catch_all | Constitution IV compliance |
| Pipeline integration | AST node + codegen | Matches existing compiler pattern |
| Dynamic call-host | Host dispatch function | Only viable Wasm approach |
| Re-entrant callbacks | Native Wasm support | No special handling needed |
| Type indices | Start at 23+ | Existing T058 implementation |
| Edge cases | Runtime checks + error signaling | Spec requirements |
