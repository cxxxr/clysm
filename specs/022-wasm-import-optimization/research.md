# Research: Wasm Import Optimization

**Feature**: 022-wasm-import-optimization
**Date**: 2025-12-25
**Status**: Complete

## Executive Summary

The root cause of unconditional FFI import emission is that the `*ffi-environment*` global is populated when the streams module loads, regardless of whether the compiled code uses I/O. The solution is to add I/O usage analysis to the compilation pipeline and gate import emission on the analysis result.

---

## Research Questions

### RQ1: What I/O operations need to be tracked?

**Findings**:

The following clysm:io FFI imports are registered by the streams module:

| Import | Lisp Functions Using It |
|--------|------------------------|
| `clysm:io::write-char` | `write-char`, `print`, `princ`, `format`, `terpri` |
| `clysm:io::write-string` | `write-string`, `write-line`, `print`, `format` |
| `clysm:io::read-char` | `read-char`, `peek-char`, `read` |
| `clysm:io::read-line` | `read-line` |

**I/O Function Names to Detect** (symbol names, any package):
- Output: `WRITE-CHAR`, `WRITE-STRING`, `WRITE-BYTE`, `WRITE-LINE`, `TERPRI`, `FRESH-LINE`, `PRINT`, `PRIN1`, `PRINC`, `PPRINT`, `FORMAT`, `WRITE`
- Input: `READ-CHAR`, `READ-LINE`, `READ-BYTE`, `PEEK-CHAR`, `READ`

### RQ2: At what point in compilation is I/O usage determinable?

**Findings**:

Compilation pipeline:
```
1. parse-expr (expr → AST)
2. compile-to-module (AST → compiled-module with function list)
3. emit-module (compiled-module → Wasm bytes)
   └── emit-import-section-if-needed (emits FFI imports)
```

**Decision Point**: After step 2, before step 3. The `compiled-module` structure contains all function bodies with their instruction lists. We can analyze these for I/O calls.

**Key Location**: `src/clysm/compiler/compiler.lisp:10-28` (compile-to-wasm function)

### RQ3: What is the current FFI import registration flow?

**Findings**:

1. **Load Time Registration**: When `clysm/streams` loads, it calls `define-foreign-function` for each I/O import
2. **Global Storage**: Imports are stored in `clysm/ffi:*ffi-environment*` hash table
3. **Emission Check**: `emit-import-section-if-needed` (compiler.lisp:205-221) checks:
   - Is `:clysm/ffi` package loaded?
   - Is `*ffi-environment*` bound?
   - Is `(get-ffi-import-count)` > 0?
4. **Problem**: The check is "are there ANY imports registered?" not "does THIS code need imports?"

**Root Cause Confirmed**: The FFI environment is global and populated at module load time, not per-compilation.

---

## Solution Analysis

### Option A: Clear FFI Environment Per-Compilation

**Approach**: Call `(reset-ffi-environment)` at start of `compile-to-wasm`, then selectively re-register needed imports.

**Pros**:
- Minimal code change
- Uses existing infrastructure

**Cons**:
- Breaks if multiple compilations run concurrently
- Requires knowing what imports to register based on code

**Verdict**: Not viable - same detection problem.

### Option B: I/O Usage Analysis (SELECTED)

**Approach**: Add analyzer that scans compiled code for I/O function calls. Only emit imports if I/O is detected.

**Pros**:
- Clean separation of concerns
- No changes to FFI registration flow
- Per-compilation correctness
- Extensible to other conditional imports

**Cons**:
- Slight compilation time increase (negligible)
- New module to maintain

**Verdict**: Best approach - selected.

### Option C: Per-Compilation FFI Environment

**Approach**: Pass FFI environment through compilation, not global.

**Pros**:
- Cleanest architecture
- Thread-safe

**Cons**:
- Large refactoring across many files
- Higher risk of regressions

**Verdict**: Too invasive for this feature scope.

---

## Design Decision Record

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Solution approach | Option B: I/O Usage Analysis | Minimal invasive, correct, extensible |
| Analysis location | After compile-to-module | All function info available |
| Detection method | Scan instruction lists for `:call` to I/O functions | Simple, accurate |
| I/O function list | Hardcoded in analyzer | Known finite set |
| New file | `analyzer/io-usage.lisp` | Follows existing module structure |

---

## Implementation Notes

### Key Files to Modify

1. **src/clysm/compiler/compiler.lisp**
   - `compile-to-wasm`: Add `uses-io` analysis call
   - `emit-module`: Pass `:uses-io` keyword
   - `emit-import-section-if-needed`: Gate on `:uses-io`

2. **NEW: src/clysm/compiler/analyzer/io-usage.lisp**
   - `analyze-io-usage`: Walk function bodies
   - `*io-function-names*`: Detection list
   - `instruction-uses-io-p`: Check single instruction

### Detection Algorithm

```lisp
(defun analyze-io-usage (module)
  "Returns T if module uses I/O functions, NIL otherwise."
  (dolist (func (compiled-module-functions module))
    (dolist (instr (getf func :body))
      (when (io-call-instruction-p instr)
        (return-from analyze-io-usage t))))
  nil)

(defun io-call-instruction-p (instr)
  "Check if instruction is a call to an I/O function."
  (and (consp instr)
       (eq (car instr) :call)
       (io-function-index-p (cadr instr))))
```

### Challenges

1. **Function Index Resolution**: Need to map call target index back to function name
   - Solution: Track function names in compiled-module or use existing env

2. **Indirect Calls**: `call_ref` through closures
   - Solution: Conservative - if any function body has I/O, mark as uses-io
   - Alternatively: Track which functions use I/O and check call graph

3. **Recursive Definitions**: Function A calls B, B uses I/O
   - Solution: Analyze all functions, not just main

---

## References

- `src/clysm/compiler/compiler.lisp` - Main compilation entry point
- `src/clysm/ffi/import-gen.lisp` - FFI import generation
- `src/clysm/streams/ffi-io.lisp` - I/O FFI declarations
- Constitution VII - TDD requirement
