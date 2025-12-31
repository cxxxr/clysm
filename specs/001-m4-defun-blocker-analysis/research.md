# Research: Phase 13D M4 - DEFUN Blocker Analysis

**Date**: 2025-12-31
**Feature**: 001-m4-defun-blocker-analysis

## Research Questions

### Q1: What are the main DEFUN error patterns?

**Decision**: Classify errors by error type/message prefix

**Rationale**: Based on analysis of `build/analyze-defun-failures.lisp` and current Stage 1 report, the most actionable classification groups errors by:
1. Compilation errors (compile-time failures before Wasm generation)
2. Validation errors (wasm-tools validate failures after Wasm generation)

**Findings from current codebase**:
- Current Stage 1 report (`dist/stage1-report.json`) shows 18,997 DEFUN failures
- DEFUN accounts for 90%+ of all failures
- Existing analysis script samples first 10 failures with detailed error messages

**Alternatives considered**:
- By AST node type: Too fine-grained, would create 100+ categories
- By source module: Useful for progress tracking but not for fixing patterns
- Hierarchical (error type → node → module): Overkill for current scale

### Q2: What lambda-list features are missing from codegen?

**Decision**: &aux codegen exists but may have edge case issues; &allow-other-keys needs runtime handling

**Rationale**: Code review of `src/clysm/compiler/codegen/func-section.lisp:8902-8916` shows:
- &aux parameters ARE handled in `generate-parameter-defaults`
- &aux init forms ARE compiled via `compile-to-instructions`
- &allow-other-keys flag IS parsed in ast.lisp but runtime enforcement is unclear

**Findings**:
```lisp
;; From ast.lisp:177-185 - Already supports:
(defstruct (ast-parsed-lambda-list ...)
  (required nil :type list)
  (optional nil :type list)
  (rest nil :type (or null ast-param-info))
  (keys nil :type list)
  (allow-other-keys nil :type boolean)  ;; Already parsed
  (aux nil :type list))                  ;; Already parsed
```

**Potential issues to investigate**:
1. &aux init form compilation may fail for complex forms (e.g., calls to undefined functions)
2. &allow-other-keys may not be enforced during keyword argument validation
3. Nested destructuring in lambda-lists is NOT supported

**Alternatives considered**: N/A - this is a gap analysis, not a design choice

### Q3: What makes backend/ and reader/ modules compile successfully?

**Decision**: These modules use simpler DEFUN patterns; target them as validation checkpoints

**Rationale**: From Stage 1 report module statistics:
| Module | Total | Compiled | Failed | Rate |
|--------|-------|----------|--------|------|
| backend/leb128.lisp | 1243 | 273 | 932 | 22.0% |
| backend/sections.lisp | 1237 | 273 | 926 | 22.1% |
| reader/tokenizer.lisp | 1197 | 256 | 903 | 21.4% |
| reader/reader.lisp | 1122 | 245 | 841 | 21.8% |

**Key insight**: These modules primarily contain:
- Pure computational functions (LEB128 encoding, tokenization)
- Simple control flow (conditionals, loops)
- Minimal CLOS usage

If DEFUN-related issues are fixed, these modules should reach near 100% compilation.

**Alternatives considered**: N/A - analysis finding

### Q4: What error logging infrastructure exists?

**Decision**: Extend existing Stage 1 report with detailed error_patterns section

**Rationale**: Current infrastructure:
- `build/stage1-complete.lisp` generates `dist/stage1-report.json`
- Report contains per-module statistics but not per-error-pattern breakdowns
- `build/analyze-defun-failures.lisp` exists as a prototype for detailed analysis

**Implementation approach**:
1. Create `src/clysm/stage0/error-analysis.lisp` for reusable error classification
2. Enhance `stage1-complete.lisp` to collect and classify errors
3. Output `dist/defun-errors.json` with detailed error log
4. Add `error_patterns` section to `stage1-report.json`

**Alternatives considered**:
- Logging to stdout: Loses structured data
- Separate logging library: Unnecessary complexity for build-time analysis

## Technical Decisions

### Error Pattern Classification Algorithm

```text
Pattern extraction:
1. Capture full error message from handler-case
2. Extract pattern key:
   - For compile errors: First line of condition message
   - For validation errors: wasm-tools error prefix (e.g., "type mismatch", "unknown type")
3. Normalize pattern:
   - Remove line numbers and file paths
   - Replace specific names with placeholders (e.g., "function FOO" → "function <NAME>")
4. Aggregate by pattern key, maintain:
   - Count
   - First 3 examples (function name + source location)
   - Affected modules set
```

### Report Output Format

**Decision**: JSON format for both detailed log and summary report

```json
{
  "error_patterns": [
    {
      "pattern_id": "P001",
      "pattern": "unsupported lambda-list keyword: &WHOLE",
      "count": 1234,
      "percentage": 6.5,
      "examples": [
        {"function": "MAKE-FOO", "module": "src/clysm/clos/mop.lisp"}
      ]
    }
  ]
}
```

### &aux Parameter Handling

**Decision**: Debug existing implementation, add test coverage

Current implementation at `func-section.lisp:8902-8916`:
```lisp
(dolist (p aux)
  (let ((name (clysm/compiler/ast:ast-param-info-name p))
        (init-form (clysm/compiler/ast:ast-param-info-default-form p)))
    ...))
```

Suspected issues:
1. `parse-expr` may fail on complex init forms
2. Local index allocation may not include aux parameters correctly
3. Init form evaluation may reference undefined symbols

### &allow-other-keys Runtime

**Decision**: Generate runtime check skip when flag is true

Current parsing preserves the flag (`ast-parsed-lambda-list-allow-other-keys`) but codegen at lines 8865-8900 for keyword handling doesn't use it.

Implementation: When `allow-other-keys` is true, skip the "unknown keyword" error generation.

## Dependencies

| Dependency | Required Version | Purpose |
|------------|------------------|---------|
| SBCL | 2.4+ | Host compiler |
| wasm-tools | latest | Wasm validation |
| alexandria | any | Utility functions |
| rove | any | Test framework |

All dependencies are already available via `nix develop`.

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Error patterns are too diverse | Medium | Medium | Start with top 10 patterns (80% coverage) |
| &aux codegen requires deep changes | Low | High | Existing code handles it; likely edge cases only |
| Module 100% target too aggressive | Medium | Low | Accept "intentionally skipped" forms |
| Report format changes break CI | Low | Medium | Version the report format |

## Next Steps

1. ✅ Research complete
2. → Phase 1: Data model for error log entries and patterns
3. → Phase 1: Contract for report JSON schema
4. → Implementation: Error logging infrastructure
5. → Implementation: Lambda-list codegen fixes
6. → Validation: Run Stage 1 with enhanced logging
