# Quickstart: ANSI Test Suite

**Feature**: 020-ansi-test | **Date**: 2025-12-25

## Prerequisites

```bash
# Enter Nix development shell
nix develop

# Verify tools are available
which wasmtime wasm-tools
sbcl --version
```

## Basic Usage

### Run All Tests

```lisp
;; In SBCL REPL
(asdf:load-system :clysm/ansi-test)
(clysm/ansi-test:run-ansi-tests)
```

### Run Specific Category

```lisp
;; Run only cons tests
(clysm/ansi-test:run-ansi-tests :category "cons")

;; Run only numbers tests
(clysm/ansi-test:run-ansi-tests :category "numbers")
```

### List Available Categories

```lisp
(clysm/ansi-test:list-categories)
;; => (("cons" :count 1024 :status :active)
;;     ("numbers" :count 2341 :status :active)
;;     ...)
```

## Parallel Execution

```lisp
;; Run with 4 parallel workers (faster)
(clysm/ansi-test:run-ansi-tests :parallel 4)
```

## Generate Report

```lisp
;; Generate Markdown report
(clysm/ansi-test:run-ansi-tests
  :report-path #p"coverage-report.md")
```

## Regression Detection

```lisp
;; Compare against baseline
(clysm/ansi-test:compare-baseline)

;; Update baseline (after improvements)
(clysm/ansi-test:update-baseline :force t)
```

## Shell Script (CI)

```bash
# Run all tests
./scripts/ansi-test.sh run

# Run specific category
./scripts/ansi-test.sh run --category cons

# Compare and fail on regression
./scripts/ansi-test.sh compare

# Update baseline
./scripts/ansi-test.sh update --force
```

## Expected Initial Results

| Category | Expected Pass Rate | Notes |
|----------|-------------------|-------|
| cons | ~40% | Core list operations |
| numbers | ~30% | Numeric tower (partial) |
| symbols | ~50% | Symbol operations |
| characters | ~60% | Character predicates |

## Troubleshooting

### Test hangs
```lisp
;; Increase timeout
(clysm/ansi-test:run-ansi-tests :timeout 60)
```

### Too many skips
```lisp
;; Check skip reasons
(clysm/ansi-test:run-ansi-tests :category "cons")
;; Look at skip reason summary in output
```

### Invalid Wasm
```bash
# Validate a single expression
sbcl --eval "(asdf:load-system :clysm)" \
     --eval "(clysm/tests:validate-wasm
               (clysm/compiler:compile-to-wasm '(cons 1 2)))"
```
