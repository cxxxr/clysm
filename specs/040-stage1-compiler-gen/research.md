# Research: Stage 1 Compiler Generation (Phase 13D-7)

**Date**: 2025-12-30
**Branch**: `040-stage1-compiler-gen`

## Executive Summary

This research validates the feasibility of achieving 80%+ compilation rate for Stage 1 generation, leveraging Phase 13D-1~6 implementations. The existing infrastructure (`src/clysm/stage1/`) provides a solid foundation, with blocker analysis capabilities already in place.

## Research Tasks

### 1. Current Compilation Rate Assessment

**Decision**: Current compilation rate is approximately 24.6% (per CLAUDE.md).

**Rationale**: The baseline measurement provides a clear target gap (24.6% → 80%+). The existing `blocker.lisp` analysis tool can identify specific operators blocking compilation.

**Alternatives Considered**:
- Manual code review: Too time-consuming for 45,000 lines
- Sample-based estimation: Less accurate than full compilation attempt

### 2. Phase 13D Feature Integration Status

**Decision**: All Phase 13D-1~6 features are implemented and available for Stage 1 generation.

**Implemented Features**:

| Feature | HyperSpec | Implementation Status |
|---------|-----------|----------------------|
| [aref](resources/HyperSpec/Body/f_aref.htm) | Array accessor | Phase 13D-1 (001-ansi-array-primitives) |
| [svref](resources/HyperSpec/Body/f_svref.htm) | Simple-vector accessor | Phase 13D-1 |
| [coerce](resources/HyperSpec/Body/f_coerce.htm) | Type coercion | Phase 13D-2 |
| [subseq](resources/HyperSpec/Body/f_subseq.htm) | Subsequence | Phase 13D-2 |
| [concatenate](resources/HyperSpec/Body/f_concat.htm) | Sequence concatenation | Phase 13D-2 |
| [handler-case](resources/HyperSpec/Body/m_hand_1.htm) | Exception handling | Phase 13D-6 |
| [values](resources/HyperSpec/Body/f_values.htm) | Multiple values | Phase 13D-6 |
| [the](resources/HyperSpec/Body/s_the.htm) | Type declaration | Phase 13D-6 |
| [labels](resources/HyperSpec/Body/s_flet_.htm) | Local functions | Phase 13D-6 |
| [loop](resources/HyperSpec/Body/m_loop.htm) | Iteration macro | Phase 13D-5 |

**Rationale**: These features address the most common compilation blockers identified in Phase 13D analysis.

### 3. Blocker Analysis Approach

**Decision**: Use existing `clysm/stage1:analyze-blockers` to identify top blockers.

**Rationale**: The blocker analysis infrastructure (blocker.lisp) provides:
- Automatic aggregation of failures by operator
- Impact percentage calculation
- Priority estimation (HIGH/MEDIUM/LOW)
- Recommendations for fixes

**Key Blockers (Historical)**:
1. `defstruct` - Structure definitions
2. `loop` - Complex iteration (now addressed by Phase 13D-5)
3. `format` - Format strings with complex directives
4. `declare` - Type/optimization declarations
5. `define-condition` - Condition type definitions

### 4. Wasm Module Aggregation Strategy

**Decision**: Combine all compiled forms into a single Wasm module with proper section ordering.

**Rationale**:
- WasmGC requires sections in specific order (type, import, func, table, memory, global, export, elem, code)
- The existing `accumulate-wasm-bytes` function handles byte concatenation
- Validation via `wasm-tools validate` ensures structural correctness

**Alternatives Considered**:
- Separate modules per source file: Increases complexity, requires dynamic linking
- Incremental module: Wasm doesn't support incremental construction

### 5. Error Recovery Strategy

**Decision**: Continue-on-failure with detailed logging.

**Rationale**:
- Partial compilation is better than total failure
- Logging enables post-hoc blocker analysis
- Matches existing `compile-all-forms` behavior in generator.lisp

**Implementation**:
```lisp
(handler-case
    (compile-form-to-wasm form)
  (error (e)
    (log-compilation-failure form e)
    (values nil nil (format nil "~A" e))))
```

### 6. Build Script Design

**Decision**: Create `build/stage1-complete.lisp` as canonical entry point.

**Rationale**:
- FR-010 specifies `sbcl --load build/stage1-complete.lisp`
- Existing `stage1-gen.lisp` provides template
- Separates concern from ASDF system definition

**Interface**:
- Input: None (uses default paths)
- Output: `dist/clysm-stage1.wasm`, `dist/stage1-report.json`
- Exit codes: 0 (success), 1 (fatal error)

## Best Practices Applied

### Common Lisp Build Patterns

1. **ASDF System Loading**: Use `asdf:load-system` for dependency resolution
2. **Pathnames**: Use `merge-pathnames` with system root for portability
3. **Error Handling**: `handler-case` for graceful degradation
4. **Exit Codes**: Standard Unix convention (0=success, non-zero=failure)

### WasmGC Patterns

1. **Section Order**: Maintain type < func < code section ordering
2. **Validation**: Always run `wasm-tools validate` before accepting output
3. **Type Indices**: Use consistent type indices per CLAUDE.md table

## Open Questions Resolved

| Question | Resolution |
|----------|------------|
| How to measure compilation rate? | Count compiled/total forms, report percentage |
| What if rate < 80%? | Feature incomplete; iterate on blockers |
| How to handle circular dependencies? | ASDF topological sort handles ordering |
| Where to output binary? | `dist/clysm-stage1.wasm` per convention |

## Recommendations

1. **Run blocker analysis first** to identify remaining high-impact blockers
2. **Fix any remaining defstruct/format issues** before full compilation
3. **Monitor memory usage** during compilation of large codebase
4. **Use verbose mode** during development for debugging
