# Research: Compile-Time Directive Processing

**Date**: 2025-12-30
**Feature**: 001-compile-time-directives

## Research Questions

### RQ-1: Where should directive detection occur in the compilation pipeline?

**Decision**: Before AST generation, in a new `compile-toplevel-form` function called from `compile-to-module`

**Rationale**:
- The spec requires FR-005: "compiler MUST NOT generate AST nodes"
- Current pipeline: `compile-to-module` → `parse-expr` → AST → codegen
- Inserting detection after parse would require AST cleanup
- Inserting before parse allows early return with nil

**Alternatives Considered**:
1. **In parse-expr**: Rejected - would still create partial AST before detection
2. **In parse-compound-form**: Rejected - same issue, also mixes concerns
3. **Post-compilation filtering**: Rejected - wasteful and violates FR-005

### RQ-2: How should directives be evaluated at compile-time?

**Decision**: Use host Lisp's `eval` directly, with `*package*` binding for proper context

**Rationale**:
- Host SBCL already implements [in-package](resources/HyperSpec/Body/m_in_pkg.htm), [defpackage](resources/HyperSpec/Body/m_defpkg.htm), [declaim](resources/HyperSpec/Body/m_declai.htm), [proclaim](resources/HyperSpec/Body/f_procla.htm) correctly
- No need to reimplement ANSI CL semantics
- Compile-time evaluation affects host environment which is the goal
- `*package*` changes from `in-package` persist for subsequent forms

**Alternatives Considered**:
1. **Custom interpreter**: Rejected - duplicates ANSI CL functionality, error-prone
2. **Macro expansion only**: Rejected - doesn't handle all cases (proclaim is a function)

### RQ-3: What should directive processing return?

**Decision**: Return `nil` to indicate no AST/Wasm generation needed

**Rationale**:
- FR-009 specifies "nil or an empty progn"
- `nil` is simpler and semantically cleaner
- Calling code checks return value: if nil, skip codegen

**Alternatives Considered**:
1. **Empty progn AST node**: Rejected - still creates AST node, violates spirit of FR-005
2. **Special sentinel value**: Rejected - adds complexity, nil is idiomatic

### RQ-4: How should errors be handled?

**Decision**: Let host errors propagate with additional compile-time context

**Rationale**:
- FR-008: "Errors MUST be signaled as compile-time errors with clear messages"
- Host already signals proper conditions (e.g., `package-error` for undefined package)
- Wrap in `handler-case` to add source location context

**Alternatives Considered**:
1. **Silent failure**: Rejected - violates FR-008, hides bugs
2. **Custom condition hierarchy**: Rejected - overengineering for simple feature

### RQ-5: Which declarations should declaim/proclaim support?

**Decision**: All ANSI CL declarations supported by host SBCL

**Rationale**:
- Host evaluation means automatic support for: [optimize](resources/HyperSpec/Body/d_optimi.htm), [type](resources/HyperSpec/Body/d_type.htm), [special](resources/HyperSpec/Body/d_specia.htm), [ftype](resources/HyperSpec/Body/d_ftype.htm), [inline](resources/HyperSpec/Body/d_inline.htm), [notinline](resources/HyperSpec/Body/d_inline.htm)
- Future declarations automatically supported when SBCL adds them
- Clysm compiler can query `*compilation-environment*` for optimization settings

**Alternatives Considered**:
1. **Subset only (optimize, type, special)**: Rejected - arbitrary limitation, less useful
2. **Custom declaration storage**: Rejected - duplicates host environment

## Integration Points

### Existing Code Locations

| Component | File | Lines | Integration Method |
|-----------|------|-------|-------------------|
| Main entry | `src/clysm/compiler/compiler.lisp` | 65-135 | Call `compile-toplevel-form` from loop in `compile-to-module` |
| Form parsing | `src/clysm/compiler/ast.lisp` | 729-833 | No changes needed - directive forms never reach parser |
| Environment | `src/clysm/compiler/env.lisp` | All | May store declaration info if needed for optimization |

### New Code Locations

| Component | File | Purpose |
|-----------|------|---------|
| Directive handler | `src/clysm/compiler/directive.lisp` | `compile-toplevel-form`, `directive-form-p` predicates |
| Unit tests | `tests/unit/directive-test.lisp` | Test each directive type |
| Contract tests | `tests/contract/directive-output-test.lisp` | Verify no Wasm output |

## Dependencies

### Host Dependencies (already present)
- SBCL 2.4+ with standard package/declaration support
- `*package*` special variable for current package context

### No New Dependencies Required
- Feature uses only standard Common Lisp functionality
- No external libraries needed

## Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Host package mutation affects parallel compilation | Low | Medium | Document single-threaded compilation assumption |
| declaim optimization settings not used by codegen | Medium | Low | Document as future enhancement; current goal is error elimination |
| Forward package references in same file | Low | High | Error handling per ANSI CL (package must exist before use) |
