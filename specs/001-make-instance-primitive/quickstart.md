# Quickstart: make-instance* Primitive

**Feature**: 001-make-instance-primitive
**Time**: ~30 minutes implementation

## What This Feature Does

Adds `make-instance*` as a recognized compiler primitive, enabling DEFSTRUCT-generated constructor code to compile to WebAssembly.

## Quick Verification

After implementation, run:

```bash
# Full Stage 1 build (includes validation)
sbcl --load build/stage1-complete.lisp

# Check report for DEFSTRUCT failures
grep -i "make-instance" dist/stage1-report.json
# Expected: No matches (all failures resolved)

# Validate generated Wasm
wasm-tools validate dist/clysm-stage1.wasm
# Expected: Exit code 0
```

## Files Changed

| File | Change |
|------|--------|
| `src/clysm/compiler/codegen/func-section.lisp` | Add primitive + compiler function |

## Implementation Steps

1. **Add to primitive list** (~line 844):
   ```lisp
   ;; CLOS instance creation (001-make-instance-primitive)
   make-instance*
   ```

2. **Add case dispatch** (in `compile-primitive-call`):
   ```lisp
   ((string= op-name "MAKE-INSTANCE*") (compile-make-instance* args env))
   ```

3. **Implement compiler function**:
   ```lisp
   (defun compile-make-instance* (args env)
     ;; See contracts/compile-make-instance-star.md
     ...)
   ```

## Testing

```lisp
;; Test 1: Simple structure
(defstruct point x y)
;; Should compile without errors

;; Test 2: Constructor call
(make-point :x 1 :y 2)
;; Should generate valid Wasm
```

## Success Criteria

- [x] `(defstruct point x y)` compiles successfully
- [x] `(make-point :x 1 :y 2)` compiles successfully (make-instance* recognized)
- [ ] Stage 1 compilation rate ≥ 30% (NOT MET: 13.76% - see notes)
- [x] wasm-tools validate passes

### Notes

The 30% compilation rate target was not achieved because:
1. make-instance* IS working correctly (no specific errors in grep)
2. DEFSTRUCT forms fail for other reasons within their expanded code (accessor functions, predicates, etc.)
3. The actual impact is unblocking the make-instance* primitive, but full DEFSTRUCT compilation requires additional work on accessors and other generated functions

Current compilation rate: 13.76% (3585/26053 forms)
