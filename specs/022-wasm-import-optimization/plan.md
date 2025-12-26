# Implementation Plan: Wasm Import Optimization

**Branch**: `022-wasm-import-optimization` | **Date**: 2025-12-25 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/022-wasm-import-optimization/spec.md`

## Summary

Enable Clysm-compiled Wasm modules to run in wasmtime without requiring I/O shims by making FFI import emission conditional. Currently, all compiled modules include `clysm:io` imports unconditionally, causing "unknown import" errors in standard runtimes. This feature modifies the compiler to analyze code for I/O usage and only emit imports when needed.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel, rove (testing), clysm/ffi (existing FFI module)
**Storage**: N/A (compile-time code generation)
**Testing**: rove (unit/contract/integration tests)
**Target Platform**: Wasm (wasmtime, Node.js with host-shim)
**Project Type**: single
**Performance Goals**: Compilation time ≤10% increase; Module size ≤ current for non-I/O code
**Constraints**: Must maintain backward compatibility with existing I/O code
**Scale/Scope**: Affects compiler.lisp, ffi/ module, and ANSI test runner

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | ✅ PASS | No linear memory access; uses GC types |
| II. Lisp Object Representation | ✅ PASS | No changes to object representation |
| III. Function/Closure Strategy | ✅ PASS | No changes to closure handling |
| IV. Wasm Control Flow | ✅ PASS | No changes to exception handling |
| V. Shallow Binding | ✅ PASS | No changes to special variables |
| VI. Tiered Eval/JIT | ✅ PASS | JIT compilation unaffected |
| VII. TDD | ⚠️ REQUIRED | Must write tests first |
| VIII. Nix-First | ✅ PASS | `nix flake check` must pass |

**Gate Status**: PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/022-wasm-import-optimization/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── compiler.lisp       # MODIFY: emit-import-section-if-needed
│   ├── analyzer/           # ADD: io-usage-analyzer.lisp (new)
│   └── codegen/
│       └── func-section.lisp  # READ: understand compilation flow
├── ffi/
│   ├── import-gen.lisp     # MODIFY: conditional import collection
│   └── package.lisp        # READ: exported symbols
└── streams/
    └── ffi-io.lisp         # READ: I/O function definitions

tests/
├── unit/
│   └── compiler/
│       └── io-usage-test.lisp  # ADD: new tests
├── contract/
│   └── compiler/
│       └── import-section-test.lisp  # ADD: new tests
└── integration/
    └── ansi-test/
        └── execution-test.lisp  # MODIFY: verify pass rate
```

**Structure Decision**: Single project; modifications to existing compiler and FFI modules.

## Complexity Tracking

No violations to justify. This feature:
- Adds one new analyzer module (io-usage-analyzer.lisp)
- Modifies existing compiler integration point (emit-import-section-if-needed)
- No new patterns or abstractions beyond existing codebase conventions

---

## Phase 0: Research

### Research Questions

1. **RQ1**: What I/O operations need to be tracked?
   - Which Lisp functions ultimately require FFI I/O imports?
   - How are these functions identified in AST?

2. **RQ2**: At what point in compilation is I/O usage determinable?
   - Before or after macro expansion?
   - Before or after closure conversion?

3. **RQ3**: What is the current FFI import registration flow?
   - How are imports registered in `*ffi-environment*`?
   - When is `emit-import-section-if-needed` called?

### Research Tasks

- [x] RQ1: Examine streams/ffi-io.lisp for I/O function definitions
- [x] RQ2: Trace compilation pipeline in compiler.lisp
- [x] RQ3: Analyze FFI integration in compiler.lisp lines 205-248

### Research Findings

**RQ1 Answer**: The following functions require clysm:io imports:
- `write-char`, `write-string` → `clysm:io::write-char`, `clysm:io::write-string`
- `read-char`, `read-line` → `clysm:io::read-char`, `clysm:io::read-line`
- `print`, `princ`, `format` (via write-char/write-string internally)

**RQ2 Answer**: I/O usage is determinable after AST parsing but before code emission. The AST contains all function call information. The analyzer should run on the parsed AST before `emit-module` is called.

**RQ3 Answer**: FFI imports are registered in `*ffi-environment*` global hash table. The `emit-import-section-if-needed` function checks if the FFI package exists and has registered imports, then emits them. The key insight: **imports are registered statically at load time, not analyzed per-compilation**.

**Critical Finding**: The root cause is that the FFI environment (`*ffi-environment*`) is populated when the streams module loads, regardless of whether the compiled code uses I/O. The fix requires either:
1. **Option A**: Don't load streams module, or clear FFI environment before each compilation
2. **Option B**: Add I/O usage analysis and only emit imports when code actually uses I/O
3. **Option C**: Make FFI environment per-compilation rather than global

**Decision**: Option B is the correct approach. It preserves existing behavior while enabling I/O-free modules.

---

## Phase 1: Design

### Approach

1. **Add I/O Usage Analyzer**: Create `io-usage-analyzer.lisp` that walks the AST and detects I/O function calls
2. **Modify Compiler**: Gate FFI import emission on analysis result
3. **Test Coverage**: Unit tests for analyzer, contract tests for import section, integration tests for execution

### Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Analysis granularity | Per-compilation | Simple, correct, no global state issues |
| Detection method | AST function name matching | Straightforward, maintainable |
| I/O function list | Hardcoded constant | Known set, rarely changes |
| Where to analyze | After `compile-to-module`, before `emit-module` | Minimal invasive change |

### Data Flow

```
Lisp Expr
    ↓
parse-expr → AST
    ↓
compile-to-module → compiled-module
    ↓
**NEW** analyze-io-usage(compiled-module) → uses-io-p
    ↓
emit-module(compiled-module, :uses-io uses-io-p)
    ↓
emit-import-section-if-needed (only if uses-io-p = T)
    ↓
Wasm Bytes
```

### I/O Function Detection

The analyzer tracks calls to these symbols (in any package):
- `write-char`, `write-string`, `write-byte`, `write-line`, `terpri`, `fresh-line`
- `read-char`, `read-line`, `read-byte`, `peek-char`
- `print`, `prin1`, `princ`, `pprint`, `format`, `write`

Detection algorithm:
1. Walk all function bodies in compiled-module
2. Scan instructions for `(:call N)` where N resolves to I/O function
3. Return T if any I/O call found, NIL otherwise

### Module Changes

**src/clysm/compiler/analyzer/io-usage.lisp** (NEW):
```lisp
(defun analyze-io-usage (module) ...)
(defun instruction-uses-io-p (instr) ...)
(defparameter *io-function-names* '("WRITE-CHAR" ...))
```

**src/clysm/compiler/compiler.lisp** (MODIFY):
```lisp
;; In compile-to-wasm:
(let* ((module (compile-to-module expr))
       (uses-io (analyze-io-usage module))
       (bytes (emit-module module :uses-io uses-io)))
  ...)

;; In emit-import-section-if-needed:
(defun emit-import-section-if-needed (buffer &key (uses-io t))
  (when uses-io  ; NEW: only emit if I/O is used
    (let ((ffi-pkg (find-package :clysm/ffi)))
      ...)))
```

### Test Strategy

| Test Type | Count | Purpose |
|-----------|-------|---------|
| Unit tests | 8 | I/O detection accuracy |
| Contract tests | 4 | Import section presence/absence |
| Integration tests | 3 | wasmtime execution, ANSI pass rate |

---

## Artifacts to Generate

- [x] research.md (Phase 0 complete)
- [x] data-model.md (Phase 1 complete)
- [x] contracts/wasm-output.md (Phase 1 complete)
- [x] quickstart.md (Phase 1 complete)

---

## Next Steps

Run `/speckit.tasks` to generate the implementation task list.
