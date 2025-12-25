# Quickstart: ANSI Test Execution

**Feature**: 021-ansi-test-execution
**Date**: 2025-12-25

## Prerequisites

1. Enter Nix development shell:
   ```bash
   nix develop
   ```

2. Load the system:
   ```lisp
   (ql:quickload :clysm/ansi-test)
   ```

## Basic Usage

### Run a Single Category

```lisp
;; Run numbers category
(clysm/ansi-test:run-ansi-tests :category "numbers")

;; Expected output:
;; Category: numbers............... [~200/2014] ~10.0%
;; Summary: ~200/2014 (~10.0%) | ~100 failed | ~1714 skipped
;; Duration: 1m 30s
```

### Run Cons Category

```lisp
;; Run cons category
(clysm/ansi-test:run-ansi-tests :category "cons")

;; Expected output:
;; Category: cons............... [~80/1641] ~5.0%
;; Summary: ~80/1641 (~5.0%) | ~60 failed | ~1501 skipped
;; Duration: 2m 0s
```

### Generate Report

```lisp
(clysm/ansi-test:run-ansi-tests
  :category "numbers"
  :report-path #p"reports/numbers-compliance.md")
```

## Interpreting Results

### Status Meanings

| Status | Meaning | Action |
|--------|---------|--------|
| PASS | Test compiled, ran, result matched | None - working correctly |
| FAIL | Compiled but result didn't match | Investigate implementation bug |
| SKIP | Couldn't verify | Check skip reason for next steps |

### Common Skip Reasons

| Skip Reason | Meaning |
|-------------|---------|
| `unsupported-form: loop` | Test uses `loop` macro (not implemented) |
| `unsupported-form: let` | Test uses local bindings (not implemented) |
| `compile-error: ...` | Clysm compiler couldn't compile the form |
| `unverifiable: expected symbol A` | Test expects symbol; can only verify fixnum/T/NIL |
| `runtime-error: timeout` | Wasm execution exceeded 30 seconds |

## Target Pass Rates

| Category | Target | Notes |
|----------|--------|-------|
| numbers | ≥10% | Basic arithmetic tests |
| cons | ≥5% | Limited by unverifiable symbol results |

## Shell Script

```bash
# Run via shell script
./scripts/ansi-test.sh --category numbers
./scripts/ansi-test.sh --category cons --report reports/cons.md
```

## Troubleshooting

### All tests skip with "compile-error"

Check if the Clysm compiler is loaded:
```lisp
(find-package :clysm/compiler)
```

### Low pass rate

Many tests use complex forms. Check skip reasons:
```lisp
(let ((report (run-ansi-tests :category "cons")))
  (mapcar #'test-result-skip-reason
          (remove-if-not (lambda (r) (eq :skip (test-result-status r)))
                         (category-result-results (first (coverage-report-categories report))))))
```

### Timeout errors

Increase timeout:
```lisp
(run-ansi-tests :category "numbers" :timeout 60)
```
