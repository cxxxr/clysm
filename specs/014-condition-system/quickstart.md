# Quickstart: ANSI Common Lisp Condition System

**Feature**: 014-condition-system
**Date**: 2025-12-24

## Overview

The ANSI Common Lisp Condition System provides structured error handling through three main concepts:

1. **Conditions**: Objects representing exceptional situations
2. **Handlers**: Code that responds to conditions
3. **Restarts**: Named recovery points for continuing from errors

## Basic Usage

### Signaling Errors

```lisp
;; Signal a simple error (will trap if unhandled)
(error "Something went wrong: ~A" some-value)

;; Signal a specific error type
(error 'type-error :datum value :expected-type 'integer)

;; Signal a warning (non-fatal)
(warn "This might be a problem: ~A" suspicious-value)
```

### Handling Errors (handler-case)

```lisp
;; Catch and handle specific error types
(handler-case
    (risky-operation)
  (type-error (c)
    (format t "Got type error: ~A~%" c)
    (fallback-value))
  (error (c)
    (format t "Got some error: ~A~%" c)
    (default-value)))
```

### Providing Recovery Options (restart-case)

```lisp
;; Provide restarts for callers to use
(defun read-data (source)
  (restart-case
      (parse-data source)
    (use-default ()
      :report "Use default data"
      *default-data*)
    (use-value (new-value)
      :report "Provide alternate data"
      :interactive (lambda () (list (prompt-for-data)))
      new-value)))
```

### Invoking Restarts

```lisp
;; From a handler, invoke a restart
(handler-bind
    ((parse-error
      (lambda (c)
        (invoke-restart 'use-default))))
  (read-data source))
```

## Common Patterns

### Pattern 1: Error with Recovery

```lisp
(defun safe-divide (a b)
  (restart-case
      (if (zerop b)
          (error 'division-by-zero :operand b)
          (/ a b))
    (use-value (value)
      :report "Provide a result to use"
      value)
    (return-zero ()
      :report "Return zero"
      0)))

;; Caller can handle:
(handler-bind
    ((division-by-zero
      (lambda (c)
        (invoke-restart 'return-zero))))
  (safe-divide 10 0))
;; => 0
```

### Pattern 2: Warning with Muffling

```lisp
(defun check-value (v)
  (when (> v 100)
    (warn "Value ~A exceeds recommended maximum" v))
  v)

;; Suppress the warning
(handler-bind
    ((warning #'muffle-warning))
  (check-value 200))
;; => 200 (no warning printed)
```

### Pattern 3: Continuable Errors

```lisp
(defun validate-input (input)
  (unless (valid-p input)
    (cerror "Continue with invalid input"
            "Input ~A is not valid" input))
  input)

;; Caller can continue anyway
(handler-bind
    ((error (lambda (c)
              (invoke-restart 'continue))))
  (validate-input bad-input))
```

### Pattern 4: Logging without Handling

```lisp
;; handler-bind allows inspection without consuming
(handler-bind
    ((error (lambda (c)
              (log-error c)
              ;; Return normally = decline to handle
              nil)))
  (handler-case
      (risky-operation)
    (error (c)
      (format t "Handled: ~A~%" c))))
```

## Condition Hierarchy

```text
condition
├── serious-condition
│   └── error
│       ├── simple-error
│       ├── type-error
│       ├── cell-error
│       │   ├── unbound-variable
│       │   └── undefined-function
│       └── control-error
└── warning
    └── simple-warning
```

## Standard Restarts

| Restart | Purpose |
|---------|---------|
| `abort` | Abort operation, return to top level |
| `continue` | Continue from continuable error |
| `muffle-warning` | Suppress warning output |
| `use-value` | Use a substitute value |
| `store-value` | Store value for future use |

## Key Functions

| Function | Purpose |
|----------|---------|
| `error` | Signal fatal error |
| `warn` | Signal warning |
| `signal` | Signal condition (may be ignored) |
| `cerror` | Signal continuable error |
| `find-restart` | Find restart by name |
| `invoke-restart` | Transfer to restart |
| `compute-restarts` | List available restarts |

## Implementation Notes

### clysm3 Specifics

- Conditions are CLOS instances using WasmGC structs
- Handler stacks use shallow binding (like special variables)
- Control transfer uses existing `catch`/`throw` mechanism
- Unhandled `error` calls trap (WebAssembly `unreachable`)

### Testing

```lisp
;; Run condition system tests
(asdf:test-system :clysm/tests)

;; Or specific tests
(rove:run-test 'clysm/tests/unit/handler-test)
(rove:run-test 'clysm/tests/integration/condition-test)
```

## References

- ANSI Common Lisp Standard (Chapter 9: Conditions)
- Constitution: Section IV (Wasm Control Flow), Section V (Dynamic Scope)
- Research: `specs/014-condition-system/research.md`
