# CLI Contract: ANSI Test Execution

**Feature**: 021-ansi-test-execution
**Date**: 2025-12-25

## Overview

This document describes the CLI interface for running ANSI tests. The interface is inherited from 020-ansi-test with no changes to the function signatures; only the behavior (pass/skip classification) is improved.

## Primary Entry Point

### `run-ansi-tests`

```lisp
(run-ansi-tests &key category timeout parallel report-path)
  → (values coverage-report overall-pass-rate)
```

**Parameters**:

| Name | Type | Default | Description |
|------|------|---------|-------------|
| `:category` | `(or null string)` | `nil` | Run only this category; nil = all |
| `:timeout` | `(unsigned-byte 16)` | `30` | Per-test timeout in seconds |
| `:parallel` | `(unsigned-byte 8)` | `1` | Number of parallel workers (reserved) |
| `:report-path` | `(or null pathname)` | `nil` | Path for Markdown report |

**Return values**:

1. `coverage-report` - Full report structure with all category results
2. `overall-pass-rate` - Float between 0.0 and 1.0

**Console output format**:

```
Category: cons....... [80/1641] 4.9%
Category: numbers.... [200/2014] 9.9%
Summary: 280/3655 (7.7%) | 150 failed | 3225 skipped
Duration: 1m 45s
```

### Example Usage

```lisp
;; Run numbers category
(run-ansi-tests :category "numbers")
;; → Category: numbers... [~200/2014] ~10.0%
;; → Summary: ~200/2014 (~10.0%) | ~100 failed | ~1714 skipped

;; Run cons category with report
(run-ansi-tests :category "cons" :report-path #p"cons-report.md")
;; → Writes Markdown report to cons-report.md
```

## Support Functions

### `list-categories`

```lisp
(list-categories &key root include-skipped)
  → list of (name :count N :status :active/:skipped)
```

Lists available test categories.

### `run-category`

```lisp
(run-category category-name &key timeout registry progress-callback)
  → category-result
```

Runs all tests in a single category.

### `execute-single-test`

```lisp
(execute-single-test test-case &key timeout registry)
  → test-result
```

Executes a single test and returns result.

## Skip Reason Format

Skip reasons are returned as strings in the `test-result-skip-reason` slot:

```
unsupported-category: <name>
unsupported-form: <form-name>
compile-error: <message>
unverifiable: <reason>
runtime-error: <message>
parse-error: <message>
```

## Result Summary Format

The summary line follows this format:

```
Summary: PASS/TOTAL (RATE%) | FAIL failed | SKIP skipped
```

Where:
- `PASS` = number of tests with `:pass` status
- `TOTAL` = total tests attempted
- `RATE` = `(* 100 (/ PASS TOTAL))` formatted to one decimal
- `FAIL` = number of tests with `:fail` status
- `SKIP` = number of tests with `:skip` status

## Error Handling

| Error | Behavior |
|-------|----------|
| Invalid category | Signal `category-not-found-error` with available list |
| Compilation error | Mark as SKIP with `compile-error:` reason |
| Wasm timeout | Mark as SKIP with `runtime-error: timeout` |
| Unknown output | Mark as SKIP with `parse-error:` reason |

## Compatibility

This interface is 100% backward-compatible with 020-ansi-test. All existing code using `run-ansi-tests` will continue to work; only the classification accuracy is improved.
