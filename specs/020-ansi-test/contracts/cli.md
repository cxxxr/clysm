# CLI Contract: ANSI Test Runner

**Feature**: 020-ansi-test | **Version**: 1.0.0 | **Date**: 2025-12-25

## Overview

This document specifies the command-line interface for the ANSI Common Lisp test runner. The interface is implemented as Lisp functions callable from the REPL and optionally wrapped with a shell script for CI integration.

---

## Commands

### 1. run-ansi-tests

Run the ANSI test suite with optional filters.

**Signature**:
```lisp
(run-ansi-tests &key category timeout parallel report-path)
```

**Parameters**:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| category | string/nil | nil | Run only this category (nil = all) |
| timeout | integer | 30 | Per-test timeout in seconds |
| parallel | integer | 1 | Number of parallel workers |
| report-path | pathname/nil | nil | Path for Markdown report output |

**Return Value**:
```lisp
;; Returns coverage-report structure
(values coverage-report     ; Full results
        overall-pass-rate)  ; Float 0.0-1.0
```

**Exit Codes** (when wrapped in shell):
- 0: All applicable tests passed
- 1: Some tests failed (not skipped)
- 2: Error in test harness itself

**Examples**:
```lisp
;; Run all tests
(run-ansi-tests)

;; Run only cons category
(run-ansi-tests :category "cons")

;; Run with 4 parallel workers and generate report
(run-ansi-tests :parallel 4
                :report-path #p"./coverage-report.md")

;; Run with custom timeout
(run-ansi-tests :timeout 60)
```

**Output Format** (to *standard-output*):
```
Running ANSI test suite...
Category: cons [412/1024] ████████████░░░░░░░░ 40.2%
Category: numbers [189/2341] ██░░░░░░░░░░░░░░░░░░ 8.1%
...
Summary: 2847/20156 (14.1%) | 1203 failed | 16106 skipped
Duration: 15m 23s
```

---

### 2. list-categories

List all available test categories.

**Signature**:
```lisp
(list-categories &key include-skipped)
```

**Parameters**:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| include-skipped | boolean | nil | Include categories marked as skipped |

**Return Value**:
```lisp
;; Returns list of category info
(("cons" :count 1024 :status :active)
 ("numbers" :count 2341 :status :active)
 ("files" :count 456 :status :skipped)
 ...)
```

**Example**:
```lisp
;; List active categories
(list-categories)
;; => (("cons" :count 1024 :status :active) ...)

;; Include skipped categories
(list-categories :include-skipped t)
;; => (... ("files" :count 456 :status :skipped) ...)
```

---

### 3. compare-baseline

Compare current results against stored baseline.

**Signature**:
```lisp
(compare-baseline &key baseline-path current-report)
```

**Parameters**:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| baseline-path | pathname | "baselines/current.json" | Path to baseline file |
| current-report | coverage-report/nil | nil | Use existing report (or run tests) |

**Return Value**:
```lisp
;; Returns baseline-comparison structure
(values baseline-comparison  ; Comparison details
        is-regression-p)     ; T if pass rate dropped >= 5%
```

**Output Format**:
```
Comparing against baseline from 2025-12-24...

Pass rate: 14.1% -> 14.3% (+0.2%) ✓

Improvements (5):
  + cons.tail.1: FAIL -> PASS
  + cons.tail.2: FAIL -> PASS
  ...

Regressions (0): None

Status: OK (no regression detected)
```

**Exit Codes** (when wrapped):
- 0: No regression (delta > -5%)
- 1: Regression detected (delta <= -5%)
- 2: Baseline file not found or invalid

---

### 4. update-baseline

Save current results as the new baseline.

**Signature**:
```lisp
(update-baseline &key report baseline-path force)
```

**Parameters**:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| report | coverage-report/nil | nil | Use existing report (or run tests) |
| baseline-path | pathname | "baselines/current.json" | Output path |
| force | boolean | nil | Overwrite without confirmation |

**Return Value**:
```lisp
;; Returns pathname of saved baseline
#p"baselines/current.json"
```

**Confirmation Prompt** (if not :force):
```
Current baseline: 2847 passed (14.1%)
New results:      2852 passed (14.2%)
Delta: +5 tests (+0.1%)

Save as new baseline? [y/N]
```

---

### 5. generate-report

Generate a Markdown compliance report.

**Signature**:
```lisp
(generate-report report &key output-path include-details)
```

**Parameters**:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| report | coverage-report | required | Report data |
| output-path | pathname | stdout | Output file path |
| include-details | boolean | t | Include per-test details |

**Return Value**:
```lisp
;; Returns pathname or :stdout
#p"coverage-report.md"
```

---

## Shell Wrapper

For CI integration, a shell wrapper is provided:

```bash
#!/usr/bin/env bash
# ansi-test.sh - ANSI CL compliance test runner

set -euo pipefail

usage() {
    echo "Usage: $0 [command] [options]"
    echo ""
    echo "Commands:"
    echo "  run       Run the test suite"
    echo "  compare   Compare against baseline"
    echo "  update    Update baseline"
    echo "  list      List categories"
    echo ""
    echo "Options:"
    echo "  -c, --category NAME   Run specific category"
    echo "  -p, --parallel N      Parallel workers (default: 1)"
    echo "  -t, --timeout SECS    Per-test timeout (default: 30)"
    echo "  -r, --report PATH     Output report to file"
    echo "  -b, --baseline PATH   Baseline file path"
    echo "  -f, --force           Force overwrite without confirmation"
}

# Implementation delegates to Lisp functions via:
# sbcl --noinform --disable-debugger --eval "(asdf:load-system :clysm/ansi-test)" \
#      --eval "(clysm/ansi-test:run-ansi-tests ...)"
```

---

## Error Handling

### Error Conditions

| Condition | Type | Recovery |
|-----------|------|----------|
| Category not found | `category-not-found-error` | List valid categories |
| Baseline not found | `file-error` | Create new baseline with :force |
| Parse error in test file | `parse-error` | Skip file, log error |
| Compilation error | `compilation-error` | Mark test as SKIP |
| Runtime timeout | `timeout-error` | Mark test as SKIP |

### Condition Definitions

```lisp
(define-condition category-not-found-error (error)
  ((category :initarg :category :reader error-category)
   (available :initarg :available :reader available-categories))
  (:report (lambda (c s)
             (format s "Category ~S not found. Available: ~{~A~^, ~}"
                     (error-category c)
                     (available-categories c)))))
```

---

## Progress Reporting

### Interactive Mode

When running interactively (TTY detected), display real-time progress:

```
[cons      ] ████████████░░░░░░░░  412/1024 (40.2%) 12.3s
[numbers   ] ██░░░░░░░░░░░░░░░░░░  189/2341 ( 8.1%) 34.5s
[sequences ] ▓▓▓▓▓▓▓▓▓▓▓▓░░░░░░░░  156/500  (31.2%) 8.2s (running)
```

### Non-Interactive Mode

When output is piped or in CI, use simple line-by-line output:

```
[cons] 412/1024 (40.2%) - 12.3s
[numbers] 189/2341 (8.1%) - 34.5s
...
```

---

## Configuration File

Optional configuration via `ansi-test.conf.lisp`:

```lisp
;; ansi-test.conf.lisp
(:parallel 4
 :timeout 30
 :baseline-path "baselines/current.json"
 :report-path "reports/latest.md"
 :skip-forms (format print open)
 :skip-categories ("files" "streams" "pathnames"))
```

Loaded automatically if present in project root or specified via `--config`.

---

## Integration Points

### ASDF System

```lisp
;; In clysm.asd
(defsystem "clysm/ansi-test"
  :depends-on ("clysm" "alexandria" "uiop")
  :pathname "src/clysm/ansi-test/"
  :components ((:file "package")
               (:file "skip-registry" :depends-on ("package"))
               (:file "loader" :depends-on ("package"))
               (:file "classifier" :depends-on ("package"))
               (:file "runner" :depends-on ("loader" "classifier"))
               (:file "reporter" :depends-on ("runner"))
               (:file "baseline" :depends-on ("reporter"))
               (:file "cli" :depends-on ("baseline"))))
```

### Nix Integration

```nix
# In flake.nix
{
  inputs.ansi-test = {
    url = "github:pfdietz/ansi-test";
    flake = false;
  };

  outputs = { self, nixpkgs, ansi-test, ... }: {
    packages.x86_64-linux.ansi-test = ...;
    checks.x86_64-linux.ansi-compliance = ...;
  };
}
```
