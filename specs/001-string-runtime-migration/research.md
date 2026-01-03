# Research: String Runtime Migration

**Feature**: 001-string-runtime-migration
**Date**: 2026-01-03

## Executive Summary

All technical unknowns have been resolved through codebase analysis. The migration follows established patterns from `001-sequence-runtime-migration` with no architectural changes required.

## Research Findings

### 1. Runtime Function Dispatch Pattern

**Decision**: Use existing `*runtime-function-table*` mechanism

**Rationale**:
- Pattern validated in 001-sequence-runtime-migration (12 functions migrated successfully)
- O(1) lookup via hash table
- Automatic arity validation
- No changes to compiler dispatch logic needed

**Alternatives Considered**:
- Inline macro expansion: Rejected - doesn't reduce func-section.lisp complexity
- New dispatch table: Rejected - existing mechanism works well

**Implementation Reference**:
```lisp
;; From func-section.lisp:69-88
(defparameter *runtime-function-table* (make-hash-table :test 'eq))

(defun register-runtime-function (symbol runtime-name arity)
  (setf (gethash symbol *runtime-function-table*)
        (cons runtime-name arity)))
```

### 2. Runtime Library File Pattern

**Decision**: Follow sequence-runtime.lisp structure exactly

**Rationale**:
- Consistent with existing codebase conventions
- HyperSpec references in header comments
- Layer 1 primitive restriction documented
- `-rt` suffix convention established

**File Header Template**:
```lisp
;;;; string-runtime.lisp - Runtime library string functions
;;;; Feature: 001-string-runtime-migration
;;;;
;;;; Implements string operations (char, trim, capitalize, compare)
;;;; using Layer 1 primitives only.
;;;;
;;;; HyperSpec references:
;;;;   [char](resources/HyperSpec/Body/f_char_.htm)
;;;;   [string-trim](resources/HyperSpec/Body/f_stg_tr.htm)
;;;;   [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm)
;;;;   [string-equal](resources/HyperSpec/Body/f_stgeq_.htm)
;;;;   [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm)

(in-package #:clysm)
```

### 3. Layer 1 Primitives Available

**Decision**: Use schar, char, length, make-string, char-code, code-char

**Rationale**:
- These primitives are already compiled to Wasm instructions
- Required for self-hosting compatibility (runtime must compile to Wasm)
- Matches sequence-runtime.lisp approach (uses car, cdr, cons, funcall)

**Available String Primitives**:
| Primitive | Wasm Implementation | Notes |
|-----------|---------------------|-------|
| [schar](resources/HyperSpec/Body/f_char_.htm) | array.get_u | Simple string char access |
| [char](resources/HyperSpec/Body/f_char_.htm) | UTF-8 decode | General string char access |
| [length](resources/HyperSpec/Body/f_length.htm) | array.len | String byte length |
| [make-string](resources/HyperSpec/Body/f_mk_stg.htm) | array.new | Create new string |
| [char-code](resources/HyperSpec/Body/f_char_c.htm) | i31.get_s | Character to integer |
| [code-char](resources/HyperSpec/Body/f_code_c.htm) | ref.i31 | Integer to character |

### 4. UTF-8 Handling Strategy

**Decision**: Runtime handles UTF-8 character iteration in Lisp

**Rationale**:
- Existing compile-string-char handles UTF-8 in Wasm (~195 lines)
- Same logic in Lisp is clearer and more maintainable
- Character indexing (not byte indexing) per ANSI spec

**Implementation Approach**:
```lisp
(defun string-char-rt (string index)
  "Return character at INDEX in STRING (character index, not byte).
   See [char](resources/HyperSpec/Body/f_char_.htm)."
  (let ((byte-idx 0)
        (char-count 0)
        (len (length string)))
    ;; Iterate to find byte position of index-th character
    (loop while (< byte-idx len)
          for byte = (schar string byte-idx)
          when (not (utf8-continuation-byte-p byte))
            do (when (= char-count index)
                 (return (decode-utf8-char string byte-idx)))
               (incf char-count)
          do (incf byte-idx))
    (error "Index ~D out of bounds for string of length ~D"
           index char-count)))
```

### 5. ANSI Keyword Argument Support

**Decision**: Full keyword support per ANSI spec for applicable functions

**Function Keyword Requirements**:
| Function | Required Keywords | Notes |
|----------|-------------------|-------|
| char/schar | None | Simple index access |
| [string-trim](resources/HyperSpec/Body/f_stg_tr.htm) | :start :end | Bound the trim region |
| [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm) | :start :end | Bound the capitalize region |
| [string-equal](resources/HyperSpec/Body/f_stgeq_.htm) | :start1 :end1 :start2 :end2 | Compare substrings |
| [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm) | :start :end | Bound the destructive region |

### 6. Testing Strategy

**Decision**: Three-tier testing per existing patterns

**Test Structure**:
1. **Unit tests** (`tests/unit/string-runtime-test.lisp`):
   - Test each runtime function in isolation
   - Edge cases: empty strings, single chars, Unicode
   - Keyword argument combinations

2. **Contract tests** (`tests/contract/string-runtime/`):
   - Verify Wasm output structure
   - Check runtime dispatch generates correct call instructions

3. **Integration tests** (existing Stage 1 compilation):
   - Ensure Stage 1 still compiles and validates
   - No regression in string operations

## Risks and Mitigations

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| UTF-8 edge cases | Medium | High | Comprehensive Unicode test suite |
| Performance regression | Low | Medium | Benchmark before/after if needed |
| Missing helper functions | Low | Low | Helper functions migrated with primary |

## Conclusion

All technical decisions align with established patterns. No architectural changes required. Implementation can proceed directly following the TDD workflow.
