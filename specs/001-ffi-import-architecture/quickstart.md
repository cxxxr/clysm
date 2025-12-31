# Quickstart: FFI Import Architecture

**Feature**: 001-ffi-import-architecture
**Date**: 2025-12-31

## Overview

This feature enables Clysm-compiled Wasm modules to run standalone (without host imports) for pure computations, and selectively imports only the FFI functions actually used.

## Usage

### Basic Compilation (Auto Mode)

```lisp
;; Pure arithmetic - runs in wasmtime without host
(compile-to-wasm '(+ 1 2))
;; => Wasm with no import section

;; Using FFI - only imports what's used
(compile-to-wasm '(sin 1.0))
;; => Wasm with import for clysm:math/sin only

;; Dynamic call - auto-includes dynamic-call support
(compile-to-wasm '(funcall (get-handler request) data))
;; => Wasm with $dynamic-call import
```

### Explicit Mode Selection

```lisp
;; Minimal: strict mode, no dynamic calls allowed
(compile-to-wasm expr :ffi-mode :minimal)

;; Full: always include dynamic-call support
(compile-to-wasm expr :ffi-mode :full)

;; Auto (default): smart detection
(compile-to-wasm expr :ffi-mode :auto)
```

### Running Standalone

```bash
# Pure computation - runs directly
wasmtime dist/add.wasm
# => 3

# With FFI - needs host shim
node host-shim/run.js dist/with-io.wasm
```

## Development Workflow

### 1. Check if Module Needs Host

```lisp
;; Analyze FFI usage
(let ((analysis (analyze-ffi-usage (macroexpand-all expr))))
  (format t "Used FFIs: ~A~%" (ffi-analysis-used-ffis analysis))
  (format t "Needs dynamic call: ~A~%" (ffi-analysis-has-dynamic-call-p analysis)))
```

### 2. Verify Import Section

```bash
# View import section
wasm-tools print output.wasm | grep import

# Validate structure
wasm-tools validate output.wasm
```

### 3. Test Modes

```lisp
;; Development: use minimal for cleaner errors
(compile-to-wasm expr :ffi-mode :minimal)

;; Production: use auto for flexibility
(compile-to-wasm expr :ffi-mode :auto)
```

## Error Handling

### Dynamic Call in Minimal Mode

```lisp
(handler-case
    (compile-to-wasm '(funcall var x) :ffi-mode :minimal)
  (dynamic-call-in-minimal-mode (e)
    (format t "Cannot use dynamic calls in minimal mode: ~A~%" e)))
```

### Unknown FFI at Runtime

```javascript
// Host error for unregistered dynamic call
try {
  result = dynamicCall(funcName, args);
} catch (e) {
  // "Unknown function: NONEXISTENT"
}
```

## Testing

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run FFI usage analyzer tests
sbcl --eval "(asdf:load-system :clysm/tests)" --eval "(rove:run :clysm/tests/unit/ffi-usage-test)"

# Run import section contract tests
sbcl --eval "(asdf:load-system :clysm/tests)" --eval "(rove:run :clysm/tests/contract/import-section)"

# Validate Wasm output
wasm-tools validate --features gc,exceptions dist/clysm-stage1.wasm
```

## Advanced Usage

### Analyzing Code Before Compilation

```lisp
;; Check what FFI functions will be imported
(let* ((expr '(let ((x (sin 1.0))) (print x)))
       (analysis (analyze-ffi-usage expr)))
  (format t "Static FFI calls: ~A~%" (ffi-analysis-static-funcalls analysis))
  (format t "Dynamic calls: ~A~%" (ffi-analysis-has-dynamic-call-p analysis))
  (format t "Dynamic sites: ~A~%" (ffi-analysis-dynamic-sites analysis)))
```

### Static vs Dynamic Detection

```lisp
;; Static call (quoted function) - detected at compile time
(funcall 'sin 1.0)         ; => imports clysm:math/sin

;; Static call (function reference) - detected at compile time
(funcall #'write-char #\A) ; => imports clysm:io/write-char

;; Dynamic call (computed function) - requires $dynamic-call import
(funcall (get-handler type) data)  ; => imports $dynamic-call
(apply fn args)                     ; => imports $dynamic-call
```

### Conditional Dynamic Call Handling

```lisp
;; Handler for minimal mode restriction
(defun compile-safely (expr)
  (handler-case
      (compile-to-wasm expr :ffi-mode :minimal)
    (dynamic-call-in-minimal-mode (c)
      (format *error-output* "Warning: ~A~%Falling back to :auto mode~%"
              (dynamic-call-in-minimal-mode-sites c))
      (compile-to-wasm expr :ffi-mode :auto))))
```

## Migration from Feature 022

Feature 022 (`uses-io` flag) is superseded by this feature. Changes:

| Old (022) | New (001-ffi-import-architecture) |
|-----------|-----------------------------------|
| `uses-io` boolean | `ffi-analysis` struct |
| I/O only detection | All FFI detection |
| No dynamic call support | Full dynamic call support |
| Flag ignored in emit | Flag used for selective emit |

Code using `uses-io` continues to work (backward compatible), but new code should use `ffi-mode` parameter.
