# Quickstart: Compile-Time Directive Skip Integration

**Feature**: 002-compile-time-directives
**Date**: 2025-12-31

## Prerequisites

```bash
# Enter Nix development shell
nix develop

# Verify tools
sbcl --version    # SBCL 2.4+
wasm-tools --version
```

## Understanding the Problem

Before this fix, directive forms caused false failures:

```bash
# Run Stage 1 generation
sbcl --load build/stage1-complete.lisp

# Check report - DEFPACKAGE shows 284 failures
cat dist/stage1-report.json | jq '.summary.top_blockers[] | select(.operator=="DEFPACKAGE")'
# {"operator": "DEFPACKAGE", "count": 284, "priority": "HIGH"}
```

## Key Files to Modify

| File | Purpose |
|------|---------|
| `src/clysm/stage1/generator.lisp` | test-form-compilation, classify-forms |

## Implementation Steps

### Step 1: Modify test-form-compilation

```lisp
;; Change return values to distinguish skipped from failed
(defun test-form-compilation (sexp &key (validate t))
  (handler-case
      (let ((bytes (clysm:compile-to-wasm sexp)))
        (cond
          ;; NEW: Directive returned nil - skip, don't fail
          ((null bytes)
           (values :skipped nil))
          ;; Existing: validation failed
          ((and validate (not (validate-wasm-bytes bytes)))
           (values nil nil))
          ;; Existing: success
          (t
           (values t bytes))))
    (error () (values nil nil))))
```

### Step 2: Modify classify-forms

```lisp
;; Add skipped counter and handle :skipped return
(defun classify-forms (forms &key (progress-callback nil) (validate t))
  (let ((total (length forms))
        (compiled 0)
        (failed 0)
        (skipped 0)  ; NEW
        ...)
    (loop for form in forms
          ...
          (case success-p
            (:skipped (incf skipped))  ; NEW
            ((t) (incf compiled) ...)
            ((nil) (incf failed) ...))
          ...)
    (values ... (list :compiled compiled
                      :failed failed
                      :skipped skipped  ; NEW
                      :total total))))
```

### Step 3: Update Report Generation

Ensure `skipped` count flows through to `dist/stage1-report.json`.

## Verification

```bash
# Run Stage 1 generation
sbcl --load build/stage1-complete.lisp

# Verify DEFPACKAGE is no longer a blocker
cat dist/stage1-report.json | jq '.summary.top_blockers[] | select(.operator=="DEFPACKAGE")'
# Should return empty (no match)

# Verify skipped count increased
cat dist/stage1-report.json | jq '.summary.skipped'
# Should be >= 284 (was 48)

# Verify coverage improved
cat dist/stage1-report.json | jq '.summary.coverage_pct'
# Should be >= 18.0 (was 13.6)

# Validate Wasm output
wasm-tools validate dist/clysm-stage1.wasm
```

## Running Tests

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific Stage 1 integration test
tests/integration/bootstrap/test-fixpoint.sh
```

## Success Criteria Checklist

- [ ] DEFPACKAGE not in top_blockers
- [ ] IN-PACKAGE not in top_blockers
- [ ] DECLAIM not in top_blockers
- [ ] skipped count >= 284
- [ ] coverage_pct >= 18.0
- [ ] wasm-tools validate passes
- [ ] All existing tests pass

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Directive still showing as failed | Check if `compile-to-wasm` returns nil for the form |
| Coverage not improved | Verify skipped is excluded from denominator |
| Wasm validation fails | Unrelated to directives - check other forms |
