# Quickstart: Wasm Local Instruction Binding

**Feature**: 001-wasm-local-binding
**Date**: 2026-01-01

## Prerequisites

- SBCL 2.4+ installed
- Nix environment configured (`nix develop`)
- wasm-tools available for validation

## Development Workflow

### 1. Enter Development Environment

```bash
cd /home/user/src/clysm-workbench/clysm3
nix develop
```

### 2. Run Unit Tests (TDD)

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific test file (after implementation)
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(rove:run #p\"tests/unit/local-instruction-test.lisp\")"
```

### 3. Generate Stage 1 and Check Coverage

```bash
# Generate Stage 1 with report
sbcl --load build/stage1-complete.lisp

# Check coverage in report
jq '.summary.coverage_pct' dist/stage1-report.json
# Expected: >= 25 (was 19.00)

# Check specific error patterns eliminated
jq '.error_patterns[] | select(.pattern_id == "P221")' dist/stage1-report.json
# Expected: count = 0 (was 40)
```

### 4. Validate Generated Wasm

```bash
wasm-tools validate dist/clysm-stage1.wasm
echo $?  # Should be 0
```

## Verification Commands

### Check LOCAL.SET/LOCAL.TEE Binding

```lisp
;; In SBCL REPL
(ql:quickload :clysm)

;; Test that :local.set compiles correctly in a simple function
(clysm:compile-to-wasm
  '(defun test-local-set (x)
     (let ((y x))
       (setq y (1+ y))
       y)))

;; Should not error with "Unbound variable: LOCAL.SET"
```

### Check ADVANCE-TOKEN Export

```lisp
;; Verify export
(find-symbol "ADVANCE-TOKEN" :clysm)
;; Should return: CLYSM:ADVANCE-TOKEN, :EXTERNAL

;; Verify runtime registration
(clysm/compiler/codegen/func-section::runtime-function-p 'clysm:advance-token)
;; Should return: (:$ADVANCE-TOKEN-RT . 1)
```

### Check EMIT-MODULE-HEADER Export

```lisp
;; Verify export
(find-symbol "EMIT-MODULE-HEADER" :clysm)
;; Should return: CLYSM:EMIT-MODULE-HEADER, :EXTERNAL

;; Verify function works
(clysm:emit-module-header)
;; Should return: #(0 97 115 109 1 0 0 0)
```

### Check Error Pattern Counts

```bash
# Before fix - baseline
jq '[.error_patterns[] | select(.pattern_id | test("P221|P987|P027|P143|P943")) | .count] | add' dist/stage1-report.json
# Expected before: 101

# After fix
jq '[.error_patterns[] | select(.pattern_id | test("P221|P987|P027|P143|P943")) | .count] | add' dist/stage1-report.json
# Expected after: 0
```

## File Locations

| Purpose | Path |
|---------|------|
| LOCAL.SET/LOCAL.TEE fix | `src/clysm/compiler/codegen/func-section.lisp` |
| ADVANCE-TOKEN source | `src/clysm/reader/parser.lisp:34` |
| EMIT-MODULE-HEADER source | `src/clysm/backend/wasm-emit.lisp:10` |
| Package exports | `src/clysm/package.lisp` |
| Runtime function table | `src/clysm/compiler/codegen/func-section.lisp:69` |
| Stage 1 output | `dist/clysm-stage1.wasm` |
| Coverage report | `dist/stage1-report.json` |

## Success Criteria Verification

| Criterion | Command | Expected |
|-----------|---------|----------|
| SC-001: Coverage ≥ 25% | `jq '.summary.coverage_pct' dist/stage1-report.json` | ≥ 25.00 |
| SC-002: P221 = 0 | `jq '.error_patterns[] \| select(.pattern_id=="P221") \| .count' dist/stage1-report.json` | 0 |
| SC-003: P987 = 0 | `jq '.error_patterns[] \| select(.pattern_id=="P987") \| .count' dist/stage1-report.json` | 0 |
| SC-004: P027 = 0 | `jq '.error_patterns[] \| select(.pattern_id=="P027") \| .count' dist/stage1-report.json` | 0 |
| SC-005: P143 = 0 | `jq '.error_patterns[] \| select(.pattern_id=="P143") \| .count' dist/stage1-report.json` | 0 |
| SC-006: P943 = 0 | `jq '.error_patterns[] \| select(.pattern_id=="P943") \| .count' dist/stage1-report.json` | 0 |
| SC-007: Wasm valid | `wasm-tools validate dist/clysm-stage1.wasm; echo $?` | 0 |
| SC-008: Tests pass | `sbcl --eval "(asdf:test-system :clysm)"` | All pass |

## Troubleshooting

### "Unbound variable: LOCAL.SET" persists

1. Check backquote expressions in func-section.lisp
2. Ensure `:local.set` is properly quoted in list construction
3. Look for macro expansions that might unquote keywords

### ADVANCE-TOKEN not found

1. Verify export in package.lisp
2. Check runtime-function-table registration
3. Ensure package is reloaded after changes

### Wasm validation fails

1. Run `wasm-tools print dist/clysm-stage1.wasm` to inspect
2. Check for malformed local variable indices
3. Verify LEB128 encoding is correct
