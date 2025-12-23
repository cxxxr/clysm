# Quickstart: Tagbody/Go Implementation

**Feature Branch**: `004-tagbody-go`
**Date**: 2025-12-23

## Prerequisites

- Nix with flakes enabled
- Access to clysm3 repository on `004-tagbody-go` branch

## Development Environment

```bash
# Enter development shell
cd /path/to/clysm3
nix develop

# Run tests
rove clysm.asd
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/func-section.lisp` | Main implementation location |
| `tests/unit/tagbody-test.lisp` | Unit tests for strategy detection |
| `tests/integration/control-flow-test.lisp` | Integration tests for tagbody/go |

## Implementation Order

### Phase 1: Data Structures
1. Add `tagbody-context` struct to `func-section.lisp`
2. Extend `compilation-env` with `tagbody-context` slot
3. Update `copy-compilation-env` for nesting support

### Phase 2: Strategy Analysis
1. Implement `collect-go-targets`
2. Implement `all-goes-are-backward-p`
3. Implement `analyze-tagbody-strategy`

### Phase 3: Sequential Strategy
1. Implement `compile-tagbody-sequential`
2. Test: tagbody without go statements

### Phase 4: Simple Loop Strategy
1. Implement `compile-tagbody-simple-loop`
2. Implement `compile-go-simple`
3. Test: single tag backward loop

### Phase 5: Dispatch Strategy
1. Implement `compile-tagbody-dispatch`
2. Implement `compile-tagbody-segment`
3. Implement `compile-go-dispatch`
4. Test: forward jumps, complex patterns

### Phase 6: Integration
1. Update `compile-tagbody` to dispatch by strategy
2. Update `compile-go` to use context
3. Full integration tests

## Quick Test Commands

```bash
# Run all tests
rove clysm.asd

# Run specific test file (if supported)
sbcl --load run-test.lisp --eval '(rove:run-test :tagbody-test)'

# Compile and validate a test Wasm module
sbcl --load clysm.asd --eval '(clysm:compile-to-wasm "(tagbody LOOP (go LOOP))" :output "test.wasm")'
wasm-tools validate test.wasm
```

## Debugging Tips

### Inspect Generated WAT
```lisp
(clysm:compile-to-wat '(let ((x 0))
                         (tagbody
                          LOOP
                            (setq x (+ x 1))
                            (if (< x 5) (go LOOP)))
                         x))
```

### Verify Strategy Detection
```lisp
(analyze-tagbody-strategy
  '((LOOP . ((go-form) (condition-form)))))
;; => :simple-loop
```

### Check br_table Indices
For N segments, verify:
- br_table has N+1 entries (N segments + 1 exit)
- Depth for segment i = N - i
- Exit depth = 0

## Common Issues

| Issue | Solution |
|-------|----------|
| "go outside tagbody" | Ensure tagbody-context is in env |
| Stack overflow in loop | Check br depth targets loop, not exit |
| Wrong segment executed | Verify br_table index mapping |
| Infinite loop | Check termination condition compiles correctly |
