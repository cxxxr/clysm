# Research: FFI Import Architecture

**Feature**: 001-ffi-import-architecture
**Date**: 2025-12-31

## Research Questions

### R1: Current FFI Import Mechanism

**Question**: How does the current FFI import mechanism work, and why are all imports included?

**Finding**: The current implementation has two disconnected paths:

1. `analyze-io-usage` in `io-usage.lisp` detects I/O function usage and returns `uses-io` boolean
2. `compile-to-wasm` passes `uses-io` to `emit-module`
3. **BUG**: `emit-module` accepts `:uses-io` parameter but **never uses it**
4. `emit-import-section-if-needed` always checks `*ffi-environment*` and emits all registered FFI imports

```lisp
;; compiler.lisp:44 - uses-io is passed
(bytes (emit-module module :uses-io uses-io))

;; compiler.lisp:222 - uses-io is IGNORED
(defun emit-module (module &key uses-io)  ; uses-io not referenced
  ...
  (emit-import-section-if-needed buffer functions))  ; Emits ALL FFI imports
```

**Decision**: Fix the disconnect by:
1. Extend analysis to detect all FFI usage (not just I/O)
2. Return list of used FFI functions (not just boolean)
3. Pass used-ffis to emit-module and use it for selective import

### R2: Static vs Dynamic Call Detection

**Question**: How to distinguish static calls from dynamic calls in Common Lisp?

**Finding**: Based on ANSI CL semantics:

| Call Pattern | Classification | Rationale |
|--------------|---------------|-----------|
| `(write-char #\A)` | Static | Function name is literal symbol |
| `(funcall 'write-char #\A)` | Static | Quoted symbol known at compile time |
| `(funcall #'write-char #\A)` | Static | Function reference known at compile time |
| `(funcall (intern "FOO") x)` | Dynamic | Symbol computed at runtime |
| `(funcall var x)` | Dynamic | Function from variable |
| `(apply fn args)` | Dynamic (conservative) | Function may be dynamic |

**Decision**:
- Analyze macro-expanded code (existing behavior)
- Check first argument of [funcall](resources/HyperSpec/Body/f_funcal.htm)/[apply](resources/HyperSpec/Body/f_apply.htm)
- Quote form `'symbol` → static; other expressions → dynamic

### R3: FFI Environment Structure

**Question**: How is `*ffi-environment*` structured and accessed?

**Finding**: From `ffi/macros.lisp`:

```lisp
(defvar *ffi-environment* (make-hash-table :test #'equal)
  "Global FFI environment mapping function names to declarations")

;; Each entry: name -> ffi-declaration struct containing:
;; - name: Lisp function name
;; - foreign-name: Wasm import name
;; - module: Wasm module name (e.g., "clysm:io")
;; - params: Parameter types
;; - results: Return types
```

**Decision**:
- Filter `*ffi-environment*` by used-ffis list when emitting imports
- Add `emit-selected-ffi-imports` function that takes a list of function names

### R4: Dynamic Call Implementation

**Question**: How to implement dynamic call dispatch through host?

**Finding**: WebAssembly imports are resolved at module instantiation time. For dynamic calls:

1. **Single import approach**: One `$dynamic-call` import handles all dynamic calls
2. **Host resolution**: Host maintains function registry, resolves by name at runtime
3. **Type signature**: `(func $dynamic-call (param anyref anyref) (result anyref))`
   - param 1: function name (symbol or string)
   - param 2: arguments (as list)
   - result: return value

**Decision**: Implement single `$dynamic-call` import with host-side registry

### R5: Compilation Mode Implementation

**Question**: How to implement the three compilation modes?

**Finding**: Modes affect import section generation:

| Mode | Static FFI | Dynamic Call Import | Behavior |
|------|-----------|---------------------|----------|
| `:minimal` | Used only | Never | Error on dynamic calls |
| `:full` | Used only | Always | Full flexibility |
| `:auto` (default) | Used only | If detected | Smart selection |

**Decision**:
- Add `:ffi-mode` keyword to `compile-to-wasm`
- FFI analysis returns `(values used-ffis has-dynamic-call-p)`
- `:auto` includes `$dynamic-call` only when `has-dynamic-call-p` is true

### R6: Backward Compatibility

**Question**: How to maintain backward compatibility with existing modules?

**Finding**: Existing modules may have unused FFI imports. Host runtime must:
1. Provide all registered FFI functions during instantiation
2. Not fail on unused imports (Wasm allows this by default)
3. Handle both old (all imports) and new (selective imports) modules

**Decision**: No changes needed to host runtime for backward compatibility. Wasm instantiation naturally handles unused imports if the host provides all functions.

## Alternatives Considered

### A1: Per-function Import Registration

**Rejected**: Register imports per-function call instead of global environment

**Reason**: Would require significant refactoring of compilation pipeline. Current global environment approach is simpler and matches Lisp's dynamic nature.

### A2: Lazy Import Loading

**Rejected**: Load imports on first use at runtime

**Reason**: Wasm requires all imports resolved at instantiation. Cannot add imports after module is instantiated.

### A3: Multiple Dynamic Call Imports

**Rejected**: Separate `$dynamic-call-io`, `$dynamic-call-math`, etc.

**Reason**: One generic `$dynamic-call` is simpler and sufficient. Host can route to appropriate implementation.

## Implementation Order

Based on dependencies:

1. **FFI Usage Analyzer** (`ffi-usage.lisp`)
   - Extend `io-usage.lisp` patterns to all FFI functions
   - Add dynamic call detection
   - Return `(values used-ffis has-dynamic-call-p)`

2. **Compiler Integration** (`compiler.lisp`)
   - Replace `uses-io` with `ffi-analysis` structure
   - Add `:ffi-mode` parameter
   - Pass `used-ffis` to `emit-module`

3. **Selective Import Emission** (`import-gen.lisp`)
   - Add `emit-selected-ffi-imports` function
   - Filter by used-ffis list
   - Conditionally add `$dynamic-call` import

4. **Dynamic Call Host Support** (`runtime.js`)
   - Implement function registry
   - Implement `dynamicCall` function
   - Register all FFI functions

5. **Funcall/Apply Code Generation** (`func-section.lisp`)
   - Detect dynamic funcall/apply
   - Generate `$dynamic-call` invocation
