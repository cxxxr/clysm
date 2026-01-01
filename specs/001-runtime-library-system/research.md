# Research: Runtime Library System

**Feature**: 001-runtime-library-system
**Date**: 2026-01-01
**Status**: Complete

## Research Topics

### 1. Primitives Registry Design

**Question**: How should Layer 1 primitives be registered and resolved during compilation?

**Decision**: Use a hash-table based registry similar to `src/clysm/stage0/primitives.lisp`

**Rationale**:
- Existing pattern proven in stage0 interpreter
- O(1) lookup during compilation
- Easy to extend with new primitives
- Clear separation of primitive definition from compilation logic

**Alternatives Considered**:
1. **Property list on symbols**: Rejected - less efficient for frequent lookups
2. **CLOS methods with specializers**: Rejected - over-engineered for this use case
3. **Compile-time macros**: Rejected - less flexible for dynamic registration

**Implementation Notes**:
```lisp
(defvar *primitives-registry* (make-hash-table :test #'eq))

(defstruct primitive
  name           ; Symbol
  wasm-emitter   ; Function: (env args) -> Wasm instructions
  signature      ; (:param types :result type)
  inline-p)      ; Whether to always inline

(defun register-primitive (name emitter &key signature inline-p)
  (setf (gethash name *primitives-registry*)
        (make-primitive :name name
                        :wasm-emitter emitter
                        :signature signature
                        :inline-p inline-p)))
```

### 2. Runtime Library Compilation Integration

**Question**: How should runtime library Lisp source be integrated into the compilation pipeline?

**Decision**: Compile runtime library as a separate phase before user code, with results merged into the final Wasm module

**Rationale**:
- Runtime functions are known at compile time
- Allows caching of compiled runtime
- Function indices stable across compilations
- Existing module structure supports multiple function sources

**Alternatives Considered**:
1. **Compile on demand**: Rejected - unpredictable function indices
2. **Interpret at runtime**: Rejected - violates WasmGC compilation model
3. **Separate Wasm module with linking**: Rejected - increases complexity, linking overhead

**Implementation Notes**:
- Runtime library files loaded via existing reader
- Forms compiled using existing `compile-form` infrastructure
- Function definitions accumulated in compilation context
- Index allocation: primitives < runtime < user code

### 3. I/O Function Migration Strategy

**Question**: What is the optimal order and approach for migrating I/O functions from func-section.lisp?

**Decision**: Bottom-up migration starting with lowest-level functions, preserving behavior through contract tests

**Rationale**:
- Ensures dependencies are available when higher-level functions migrate
- Minimizes risk of behavioral regressions
- Allows incremental validation

**Migration Order**:
1. **Level 1** (FFI wrappers): Already in ffi-io.lisp
   - `%host-write-char`, `%host-write-string`, `%host-read-char`

2. **Level 2** (Basic output):
   - [terpri](resources/HyperSpec/Body/f_terpri.htm) - trivial, no dependencies
   - [write-char](resources/HyperSpec/Body/f_wr_cha.htm) - wraps `%host-write-char`
   - [write-string](resources/HyperSpec/Body/f_wr_stg.htm) - wraps `%host-write-string`

3. **Level 3** (Object printing):
   - [princ](resources/HyperSpec/Body/f_wr_pr.htm) - type dispatch + write-string/write-char
   - [prin1](resources/HyperSpec/Body/f_wr_pr.htm) - princ + escape sequences
   - [print](resources/HyperSpec/Body/f_wr_pr.htm) - prin1 + newline

4. **Level 4** (Advanced):
   - [write](resources/HyperSpec/Body/f_wr_pr.htm) - full printer with keywords
   - [format](resources/HyperSpec/Body/f_format.htm) - format string interpreter

**Estimated Line Reduction**:
| Function | Current LOC (est.) | Migration Complexity |
|----------|-------------------|---------------------|
| terpri | ~50 | Low |
| write-char | ~80 | Low |
| write-string | ~100 | Low |
| princ | ~400 | Medium |
| prin1 | ~600 | Medium |
| print | ~100 | Low |
| write | ~800 | High |
| format | ~2000 | High |
| **Total** | **~4,130** | |

This represents ~22% of func-section.lisp. Additional functions (print-object methods, numeric formatting) could reach the 40% target.

### 4. Circular Dependency Handling

**Question**: How should circular dependencies between runtime functions be handled?

**Decision**: Allow circular dependencies resolved at link time through function index table

**Rationale**:
- Mutual recursion is common in Lisp standard library
- WasmGC supports forward references in function calls
- Existing compilation already handles forward-referenced functions

**Implementation Notes**:
- All runtime function names collected first (declaration pass)
- Function indices allocated before compilation
- Compilation uses indices, not direct function references
- Final Wasm function table links everything

### 5. Performance Validation

**Question**: How to ensure migrated functions maintain performance parity?

**Decision**: Benchmark suite comparing old codegen vs runtime library implementation

**Rationale**:
- Must validate SC-003 (identical behavior)
- Must validate SC-006 (build time increase <20%)
- Provides regression protection

**Benchmark Categories**:
1. **Micro**: Individual function call overhead
2. **Macro**: Real-world I/O patterns (format strings, object printing)
3. **Compile time**: Stage 1 generation timing

**Tools**:
- wasmtime with `--profile` for runtime measurement
- SBCL `time` macro for compile-time measurement
- Existing `tests/integration/stage1-timing-test.lisp` as baseline

## Research Conclusions

All technical questions resolved. No NEEDS CLARIFICATION items remain.

**Key Decisions Summary**:
1. Hash-table primitives registry (proven pattern from stage0)
2. Pre-compile runtime library, merge into Wasm module
3. Bottom-up I/O migration starting with FFI wrappers
4. Circular dependencies allowed via forward references
5. Benchmark suite for performance validation

**Ready for Phase 1: Design & Contracts**
