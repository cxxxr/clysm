# REPL API Contract: compile-file

**Date**: 2025-12-27
**Feature**: 041-dev-workflow

## Overview

The `compile-file` function provides REPL integration for compiling individual source files to WebAssembly.

## Function Signature

```lisp
(compile-file pathname &key output-file verbose force)
  => (values primary-value warnings-p failure-p)
```

## Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `pathname` | pathname-designator | (required) | Path to source file |
| `output-file` | pathname-designator | derived | Output Wasm file path |
| `verbose` | boolean | nil | Print compilation progress |
| `force` | boolean | nil | Ignore cache and recompile |

## Return Values

| Value | Type | Description |
|-------|------|-------------|
| `primary-value` | pathname or nil | Output file path if successful |
| `warnings-p` | boolean | T if warnings were generated |
| `failure-p` | boolean | T if compilation failed |

## Conditions

### file-error

Signaled when the source file cannot be read.

```lisp
(define-condition file-error (error)
  ((pathname :initarg :pathname :reader file-error-pathname)))
```

**Restarts**:
- `use-value`: Provide an alternative pathname
- `abort`: Abort compilation

### compile-error

Signaled when compilation fails.

```lisp
(define-condition compile-error (error)
  ((pathname :initarg :pathname :reader compile-error-pathname)
   (line :initarg :line :reader compile-error-line)
   (message :initarg :message :reader compile-error-message)
   (form :initarg :form :reader compile-error-form)))
```

**Restarts**:
- `continue`: Skip this form and continue
- `abort`: Abort compilation
- `use-value`: Use a replacement form

### compile-warning

Signaled for non-fatal issues.

```lisp
(define-condition compile-warning (warning)
  ((pathname :initarg :pathname :reader compile-warning-pathname)
   (line :initarg :line :reader compile-warning-line)
   (message :initarg :message :reader compile-warning-message)))
```

**Restarts**:
- `muffle-warning`: Suppress the warning

## Examples

### Basic usage

```lisp
;; Compile a single file
CL-USER> (compile-file "src/clysm/backend/leb128.lisp")
#P"src/clysm/backend/leb128.wasm"
NIL
NIL

;; With explicit output path
CL-USER> (compile-file "src/clysm/backend/leb128.lisp"
                       :output-file "dist/leb128.wasm")
#P"dist/leb128.wasm"
NIL
NIL
```

### Verbose output

```lisp
CL-USER> (compile-file "src/clysm/backend/leb128.lisp" :verbose t)
; Compiling src/clysm/backend/leb128.lisp
; Reading source file... 45 forms
; Compiling forms...
;   [1/45] defpackage CLYSM/BACKEND/LEB128 ... OK
;   [2/45] in-package ... skipped
;   [3/45] defun encode-uleb128 ... OK
;   ...
; Compilation complete: 43/45 forms compiled (2 skipped)
; Output: src/clysm/backend/leb128.wasm (1234 bytes)
#P"src/clysm/backend/leb128.wasm"
NIL
NIL
```

### Error handling

```lisp
;; File not found
CL-USER> (compile-file "nonexistent.lisp")
; Error: File not found: nonexistent.lisp
; Restarts:
;   0: [USE-VALUE] Use a different file
;   1: [ABORT] Abort compilation
;; User selects 0 and provides "correct.lisp"
#P"correct.wasm"
NIL
NIL

;; Compilation error
CL-USER> (compile-file "broken.lisp")
; Error: broken.lisp:15:3: Unknown variable BAR
; in form: (defun foo () bar)
; Restarts:
;   0: [CONTINUE] Skip this form and continue
;   1: [USE-VALUE] Use a replacement form
;   2: [ABORT] Abort compilation
;; User selects 0 to continue
#P"broken.wasm"
NIL
T  ; failure-p is T because errors occurred
```

### Force recompilation

```lisp
;; Ignore cache and recompile
CL-USER> (compile-file "src/clysm/backend/leb128.lisp" :force t)
; Forcing recompilation (cache ignored)
; ...
#P"src/clysm/backend/leb128.wasm"
NIL
NIL
```

### With condition handlers

```lisp
;; Automatically continue on errors
(handler-bind ((compile-error
                 (lambda (c)
                   (format t "~&; Skipping error: ~a~%" c)
                   (invoke-restart 'continue))))
  (compile-file "broken.lisp"))

;; Collect all warnings
(let ((warnings nil))
  (handler-bind ((compile-warning
                   (lambda (w)
                     (push (compile-warning-message w) warnings)
                     (muffle-warning))))
    (compile-file "source.lisp"))
  (format t "~&; ~d warnings collected~%" (length warnings)))
```

## Output File Derivation

When `output-file` is not specified:

1. Replace `.lisp` extension with `.wasm`
2. If no `.lisp` extension, append `.wasm`

```lisp
"foo.lisp"        → "foo.wasm"
"bar"             → "bar.wasm"
"path/to/baz.lsp" → "path/to/baz.wasm"
```

## Interaction with Compilation Cache

1. **Cache Check**: If `force` is nil, check cache for existing compilation
2. **Cache Hit**: If mtime and hash match, return cached result immediately
3. **Cache Miss**: Compile and update cache
4. **Cache Update**: After successful compilation, store result

```lisp
;; Cache hit example
CL-USER> (compile-file "unchanged.lisp")
; Using cached compilation (file unchanged)
#P"unchanged.wasm"
NIL
NIL
```

## Module Loading

`compile-file` compiles but does not load the module. To compile and load:

```lisp
;; Compile then load
(load (compile-file "module.lisp"))

;; Or use compile-and-load helper (if implemented)
(compile-and-load "module.lisp")
```

## Implementation Notes

1. **Reader Integration**: Uses existing `clysm/stage1:read-source-forms`
2. **Compiler Backend**: Delegates to `clysm:compile-to-wasm`
3. **Error Recovery**: Continues on form errors, collects all for report
4. **Progress Tracking**: Uses `clysm/stage1:progress-report` infrastructure
5. **Cache Management**: Integrates with `.clysm-cache/` system

## ANSI CL Compatibility

This function follows ANSI CL `compile-file` semantics where practical:
- Same return value convention (pathname, warnings-p, failure-p)
- Same condition types (file-error, compile-error, compile-warning)
- Same restart names (continue, abort, muffle-warning)

Differences from ANSI CL:
- Output is WebAssembly, not FASL
- No `:print`, `:external-format` parameters (UTF-8 only)
- No `*compile-file-pathname*` / `*compile-file-truename*` (not implemented)
