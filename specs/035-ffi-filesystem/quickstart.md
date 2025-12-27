# Quickstart: FFI Filesystem Access

**Feature**: 035-ffi-filesystem
**Date**: 2025-12-27

## Overview

This feature provides file I/O operations for Clysm-compiled WebAssembly modules. It works on both wasmtime (via WASI Preview2) and browser environments (via Virtual FS shim).

## Quick Examples

### Reading a File

```lisp
;; Simple one-liner
(read-file-contents "config.txt")
;; => "key=value\nother=setting\n"

;; With explicit stream
(with-open-file (s "data.txt" :direction :input)
  (read-file-contents s))
```

### Writing a File

```lisp
;; Simple one-liner
(write-file-contents "output.txt" "Hello, World!")

;; With explicit stream
(with-open-file (s "log.txt" :direction :output)
  (write-file-contents s "Log entry 1\n")
  (write-file-contents s "Log entry 2\n"))
```

### Safe Resource Management

```lisp
;; File is always closed, even if error occurs
(with-open-file (s "important.txt" :direction :output)
  (write-file-contents s "Critical data")
  (when some-condition
    (error "Something went wrong"))
  ;; File is closed before error propagates
  )
```

## API Reference

### Functions

| Function | Description |
|----------|-------------|
| `(open-file pathname &key direction if-exists if-does-not-exist)` | Open file, return stream |
| `(close-file stream)` | Close file stream |
| `(read-file-contents pathname-or-stream)` | Read entire file as string |
| `(write-file-contents pathname-or-stream contents)` | Write string to file |

### Macros

| Macro | Description |
|-------|-------------|
| `(with-open-file (var pathname &key ...) body...)` | Open, execute, close |

### Keyword Arguments

| Keyword | Values | Default | Description |
|---------|--------|---------|-------------|
| `:direction` | `:input`, `:output` | `:input` | Read or write mode |
| `:if-exists` | `:supersede`, `:error` | `:supersede` | Handle existing files (output only) |
| `:if-does-not-exist` | `:error`, `:create` | `:error` (input), `:create` (output) | Handle missing files |

## Error Handling

```lisp
;; Catch file errors
(handler-case
    (read-file-contents "missing.txt")
  (file-error (e)
    (format t "Cannot read ~A~%" (file-error-pathname e))))

;; Or use with-open-file + handler-case
(handler-case
    (with-open-file (s "data.txt" :direction :input)
      (process (read-file-contents s)))
  (file-error (e)
    (log-error e)
    (use-default-value)))
```

## Host Environment Setup

### wasmtime (WASI Preview2)

```bash
# Run with filesystem access
wasmtime --dir=. my-program.wasm
```

### Browser (Virtual FS)

```javascript
import { getImports } from './fs-shim.js';
import { VirtualFS } from './virtual-fs.js';

// Pre-load files
const vfs = new VirtualFS();
vfs.writeFile('config.txt', 'key=value');

// Instantiate with filesystem imports
const imports = getImports({ backend: 'virtual', fs: vfs });
const instance = await WebAssembly.instantiate(wasmModule, imports);
```

## Common Patterns

### Configuration Loading

```lisp
(defun load-config (path)
  "Load configuration from file, return alist."
  (handler-case
      (parse-config (read-file-contents path))
    (file-error ()
      ;; Return default config if file missing
      '((debug . nil) (log-level . "info")))))
```

### Log File Writing

```lisp
(defun append-log (path message)
  "Append message to log file."
  (let ((existing (handler-case
                      (read-file-contents path)
                    (file-error () ""))))
    (write-file-contents path
                         (concatenate 'string existing message "\n"))))
```

### File Processing Pipeline

```lisp
(defun process-files (input-dir output-dir)
  "Process all .txt files from input to output."
  (dolist (file (list-files input-dir "*.txt"))
    (let* ((content (read-file-contents file))
           (result (transform content))
           (output-path (merge-pathnames output-dir (file-namestring file))))
      (write-file-contents output-path result))))
```

## Limitations

- **Text only**: This version supports UTF-8 text files only. Binary I/O is planned for a future feature.
- **Whole-file**: Operations read/write entire files. No seek or partial read support.
- **No append mode**: To append, read existing content first, concatenate, then write.
- **Path handling**: Paths are passed directly to host; relative path resolution depends on environment.

## Testing

```bash
# Run tests
nix develop
rove tests/unit/filesystem/*.lisp
rove tests/contract/filesystem-wasm-test.lisp
rove tests/integration/filesystem-test.lisp
```

## Dependencies

This feature requires:
- Feature 027 (Complete FFI) - for `define-foreign-function`
- Feature 014 (Condition System) - for `file-error`, `handler-case`
- Feature 028 (Setf Macros) - for `unwind-protect`
