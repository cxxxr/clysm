# Research: Compile-Time Directive Skip Integration

**Feature**: 002-compile-time-directives
**Date**: 2025-12-31

## Research Questions

### RQ-1: How does `test-form-compilation` currently handle nil returns?

**Finding**: The function treats nil bytes as validation failure.

```lisp
;; src/clysm/stage1/generator.lisp:20-29
(defun test-form-compilation (sexp &key (validate t))
  (handler-case
      (let ((bytes (clysm:compile-to-wasm sexp)))
        (if (and validate (not (validate-wasm-bytes bytes)))
            (values nil nil)
            (values t bytes)))
    (error () (values nil nil))))
```

When `compile-to-wasm` returns `nil` for a directive:
1. `bytes` is `nil`
2. `validate-wasm-bytes` is called with `nil`
3. Validation fails (nil is not valid Wasm bytes)
4. Returns `(values nil nil)` - classified as failure

**Decision**: Add explicit check for `nil` bytes before validation
**Rationale**: Distinguishes intentional nil (directive) from actual failure
**Alternatives**:
- Return special value from compile-to-wasm - rejected (changes API)
- Pass directive info separately - rejected (more complex)

---

### RQ-2: How does `classify-forms` track statistics?

**Finding**: Uses three counters: `total`, `compiled`, `failed`.

```lisp
;; src/clysm/stage1/generator.lisp:31-73
(defun classify-forms (forms &key (progress-callback nil) (validate t))
  (let ((total (length forms))
        (compiled 0)
        (failed 0)
        ...)
    (loop for form in forms
          ...
          (if success-p
              (incf compiled)
              (incf failed))
          ...)
    (values ... (list :compiled compiled :failed failed :total total))))
```

**Decision**: Add `skipped` counter alongside existing counters
**Rationale**: Minimal change, maintains backwards compatibility for compiled/failed
**Alternatives**:
- Remove failed count for directives - rejected (loses accurate failure tracking)
- Use separate data structure - rejected (unnecessary complexity)

---

### RQ-3: What is the current Stage 1 report JSON structure?

**Finding**: Report already has a `skipped` field (value: 48).

```json
{
  "summary": {
    "total_forms": 26353,
    "compiled": 3585,
    "failed": 22720,
    "skipped": 48,
    "coverage_pct": 13.60,
    "top_blockers": [...]
  },
  "modules": [...]
}
```

**Decision**: Merge directive skips into existing `skipped` field
**Rationale**: The existing skipped count (48) is for other reasons. Combining all skips into one field maintains simplicity.
**Alternatives**:
- Add new `directive_skipped` field - considered but adds complexity
- Replace existing skipped with directive-only - rejected (loses other skip info)

---

### RQ-4: What operators should be recognized as directives?

**Finding**: Per ANSI CL and existing `directive.lisp`:

| Operator | HyperSpec | Purpose |
|----------|-----------|---------|
| [in-package](resources/HyperSpec/Body/m_in_pkg.htm) | Macro | Set current package |
| [defpackage](resources/HyperSpec/Body/m_defpkg.htm) | Macro | Define new package |
| [declaim](resources/HyperSpec/Body/m_declai.htm) | Macro | File-scope declarations |
| [proclaim](resources/HyperSpec/Body/f_procla.htm) | Function | Global declarations |

**Decision**: Handle all four operators as directives
**Rationale**: Consistent with ANSI CL semantics - all affect compile-time environment only
**Alternatives**: None - these are the standard compile-time directives

---

### RQ-5: How does `test-form-compilation` distinguish nil from error?

**Finding**: Currently it doesn't - both return `(values nil nil)`.

Need to add return value distinction:
- `(values nil nil)` - compilation error
- `(values :skipped nil)` - directive (nil return from compile-to-wasm)
- `(values t bytes)` - successful compilation

**Decision**: Use three-state return: nil/error, :skipped, t/success
**Rationale**: Minimal API change, explicit state distinction
**Alternatives**:
- Multiple values with reason - rejected (more complex consumer code)
- Exception for skipped - rejected (skipping is not exceptional)

---

## Implementation Approach

Based on research findings:

1. **Modify `test-form-compilation`** to detect nil bytes and return `:skipped`
2. **Modify `classify-forms`** to handle `:skipped` state and track count
3. **Modify `generate-progress-report`** to include combined skipped count
4. **Modify `top_blockers` generation** to exclude skipped operators
5. **Update coverage calculation** to use `compiled / (total - skipped) * 100`

## Dependencies

- Existing `directive.lisp` infrastructure (no changes needed)
- `clysm:compile-to-wasm` (no changes needed)
- Report consumers should handle unchanged field names gracefully

## Risks

| Risk | Mitigation |
|------|------------|
| Existing tooling depends on exact report format | Only adding to skipped count, not changing structure |
| Coverage calculation confusion | Document both raw and adjusted percentages |
| Other nil-returning forms mistakenly skipped | Only directive detection triggers nil; other forms throw errors |
