# Contract: compile-to-wasm with FFI Mode

**Module**: `clysm/compiler`
**Date**: 2025-12-31

## Function Signature

```lisp
(defun compile-to-wasm (expr &key output extra-exports ffi-mode) ...)
```

**Purpose**: Compile Lisp expression to Wasm binary with configurable FFI import behavior.

**Parameters**:
- `expr`: Lisp expression to compile
- `output`: Optional pathname for file output
- `extra-exports`: Additional exports for Stage 1 generation
- `ffi-mode`: FFI import mode (`:minimal`, `:full`, `:auto`; default `:auto`)

**Returns**:
- Byte vector if no output specified
- Pathname if output specified
- `nil` for compile-time directives

## Contract by FFI Mode

### Mode: `:minimal`

| Input | Behavior |
|-------|----------|
| Pure arithmetic | No imports, runs in wasmtime standalone |
| Static FFI calls | Only used FFI functions imported |
| Dynamic funcall | **ERROR**: `dynamic-call-in-minimal-mode` |

### Mode: `:full`

| Input | Behavior |
|-------|----------|
| Pure arithmetic | Includes `$dynamic-call` import |
| Static FFI calls | Used FFI + `$dynamic-call` imported |
| Dynamic funcall | Works via `$dynamic-call` |

### Mode: `:auto` (default)

| Input | Behavior |
|-------|----------|
| Pure arithmetic | No imports |
| Static FFI calls | Only used FFI imported |
| Dynamic funcall | Used FFI + `$dynamic-call` imported |

## Test Cases

```lisp
;; T001: Pure arithmetic, standalone execution
(let ((wasm (compile-to-wasm '(+ 1 2))))
  (assert (not (has-import-section-p wasm)))
  (assert (= 3 (wasmtime-run wasm))))

;; T002: Static FFI, selective import
(let ((wasm (compile-to-wasm '(sin 1.0))))
  (assert (has-import-p wasm "clysm:math" "sin"))
  (assert (not (has-import-p wasm "clysm:io" "write-char"))))

;; T003: Minimal mode error on dynamic call
(handler-case
    (compile-to-wasm '(funcall (intern "FOO") x) :ffi-mode :minimal)
  (dynamic-call-in-minimal-mode (e)
    (assert e)))

;; T004: Auto mode includes dynamic-call when needed
(let ((wasm (compile-to-wasm '(funcall (intern "FOO") x) :ffi-mode :auto)))
  (assert (has-import-p wasm "clysm:runtime" "dynamic-call")))

;; T005: Full mode always includes dynamic-call
(let ((wasm (compile-to-wasm '(+ 1 2) :ffi-mode :full)))
  (assert (has-import-p wasm "clysm:runtime" "dynamic-call")))
```

## Error Conditions

### dynamic-call-in-minimal-mode

```lisp
(define-condition dynamic-call-in-minimal-mode (error)
  ((form :initarg :form :reader error-form))
  (:report (lambda (condition stream)
             (format stream "Dynamic call detected in :minimal mode: ~S"
                     (error-form condition)))))
```

Signaled when:
- `ffi-mode` is `:minimal`
- Code contains dynamic funcall/apply pattern

## Integration Points

1. **Stage 1 Generation**: Uses `:ffi-mode :auto` by default
2. **REPL**: Can specify mode per compilation
3. **Batch Compilation**: Mode can be set via command-line flag
