# Quickstart: Lexical Environment Function Export

**Feature**: 001-lexenv-function-export

## Prerequisites

- SBCL 2.4+
- wasm-tools installed
- clysm system loaded

## Quick Verification

### 1. Verify Function Exports

```lisp
;; In SBCL REPL
(ql:quickload :clysm)

;; Check exports are accessible
(find-symbol "ENV-ADD-LOCAL" :clysm)      ; => CLYSM:ENV-ADD-LOCAL, :EXTERNAL
(find-symbol "LOOP-KEYWORD-EQ" :clysm)    ; => CLYSM:LOOP-KEYWORD-EQ, :EXTERNAL
(find-symbol "NUMERIC-LITERAL-P" :clysm)  ; => CLYSM:NUMERIC-LITERAL-P, :EXTERNAL
```

### 2. Verify Runtime Table Registration

```lisp
;; Check runtime function table
(clysm/compiler/codegen/func-section::runtime-function-p 'clysm:env-add-local)
; => (:$ENV-ADD-LOCAL-RT . NIL)  ; NIL = variadic (has optional type parameter)

(clysm/compiler/codegen/func-section::runtime-function-p 'clysm:loop-keyword-eq)
; => (:$LOOP-KEYWORD-EQ-RT . 2)

(clysm/compiler/codegen/func-section::runtime-function-p 'clysm:numeric-literal-p)
; => (:$NUMERIC-LITERAL-P-RT . 1)
```

### 3. Regenerate Stage 1 and Verify

```bash
# Generate Stage 1
sbcl --load build/stage1-complete.lisp

# Validate Wasm output
wasm-tools validate dist/clysm-stage1.wasm
echo $?  # Should be 0

# Check report for error reduction
jq '.error_patterns[] | select(.pattern | contains("ENV-ADD-LOCAL"))' dist/stage1-report.json
# Should return empty or show 0 count
```

### 4. Run Tests

```bash
sbcl --eval "(asdf:test-system :clysm)"
```

## Success Criteria Checklist

- [ ] `clysm:env-add-local` is externally accessible
- [ ] `clysm:loop-keyword-eq` is externally accessible
- [ ] `clysm:numeric-literal-p` is externally accessible
- [ ] Runtime table entries exist for all 3 functions
- [ ] Stage 1 report shows 0 ENV-ADD-LOCAL errors
- [ ] Stage 1 report shows 0 LOOP-KEYWORD-EQ errors
- [ ] Stage 1 report shows 0 NUMERIC-LITERAL-P errors
- [ ] `wasm-tools validate` passes
- [ ] All existing tests pass

## Troubleshooting

### Symbol Not Found

```lisp
;; If symbol not found, check package definition order
(describe (find-package :clysm))
;; Ensure :import-from comes before :export
```

### Runtime Table Entry Missing

```lisp
;; Verify registration function was called
(clysm/compiler/codegen/func-section::register-lexenv-runtime-functions)
```

### Stage 1 Errors Persist

```bash
# Check error pattern details
jq '.error_patterns[] | select(.count > 0)' dist/stage1-report.json
```
