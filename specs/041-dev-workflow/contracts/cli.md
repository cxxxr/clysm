# CLI Contract: clysm compile

**Date**: 2025-12-27
**Feature**: 041-dev-workflow

## Overview

Command-line interface for compiling Clysm source files to WebAssembly.

## Synopsis

```bash
clysm compile <patterns...> -o <output> [options]
```

## Commands

### compile

Compile Lisp source files to a WebAssembly binary.

**Arguments**:
| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `patterns` | string[] | Yes | Glob patterns for source files (e.g., `src/**/*.lisp`) |

**Options**:
| Option | Short | Type | Default | Description |
|--------|-------|------|---------|-------------|
| `--output` | `-o` | path | (required) | Output Wasm file path |
| `--verbose` | `-v` | flag | false | Show detailed compilation output |
| `--force` | `-f` | flag | false | Force full recompilation (ignore cache) |
| `--cache-dir` | | path | `.clysm-cache/` | Cache directory location |
| `--no-progress` | | flag | false | Disable progress indicator |
| `--continue` | `-c` | flag | true | Continue on compilation errors |
| `--help` | `-h` | flag | | Show help message |
| `--version` | `-V` | flag | | Show version information |

## Exit Codes

| Code | Meaning | Description |
|------|---------|-------------|
| 0 | Success | All files compiled successfully |
| 1 | Partial Success | Some files compiled; others had errors |
| 2 | Failure | Compilation failed; no output produced |
| 3 | Invalid Arguments | Invalid command-line arguments |
| 4 | Missing Files | No files matched input patterns |
| 5 | Output Error | Could not write output file |

## Output Formats

### Standard Output (default)

```
Compiling 45 modules...
[1/45] src/clysm/backend/leb128.lisp ... OK (23ms)
[2/45] src/clysm/backend/sections.lisp ... OK (45ms)
...
[45/45] src/clysm/lib/macros.lisp ... OK (89ms)

Compilation complete: 45/45 modules, 0 errors
Output: dist/clysm.wasm (45678 bytes)
```

### Verbose Output (`--verbose`)

```
Compiling 45 modules...
[1/45] src/clysm/backend/leb128.lisp
  Reading source file... 234 forms
  Compiling forms... 230/234 (4 skipped)
  Generated 1234 bytes in 23ms
  Dependencies: (none)
...
```

### Error Output (stderr)

```
error: src/clysm/compiler/ast.lisp:123:45
  Unknown variable: FOO
  in form: (defun bar () foo)

error: src/clysm/compiler/env.lisp:89:12
  Unsupported feature: LOOP
  in form: (loop for x in list collect x)

Compilation failed: 43/45 modules, 2 errors
```

### JSON Output (`--json`)

```json
{
  "status": "partial",
  "modules": {
    "total": 45,
    "compiled": 43,
    "failed": 2,
    "skipped": 0
  },
  "errors": [
    {
      "path": "src/clysm/compiler/ast.lisp",
      "line": 123,
      "column": 45,
      "message": "Unknown variable: FOO",
      "form": "(defun bar () foo)"
    }
  ],
  "output": null,
  "duration_ms": 4567
}
```

## Examples

### Basic compilation

```bash
# Compile all Lisp files to output.wasm
clysm compile 'src/**/*.lisp' -o output.wasm
```

### Incremental compilation

```bash
# First build (full)
clysm compile 'src/**/*.lisp' -o dist/clysm.wasm

# Edit a file...

# Second build (incremental - only changed files)
clysm compile 'src/**/*.lisp' -o dist/clysm.wasm
# Output: "1 module changed, recompiling..."
```

### Force rebuild

```bash
# Force full recompilation
clysm compile 'src/**/*.lisp' -o output.wasm --force
```

### Verbose mode

```bash
# See detailed progress
clysm compile 'src/**/*.lisp' -o output.wasm --verbose
```

### Multiple patterns

```bash
# Compile from multiple directories
clysm compile 'src/core/**/*.lisp' 'src/lib/**/*.lisp' -o output.wasm
```

### Custom cache location

```bash
# Use different cache directory
clysm compile 'src/**/*.lisp' -o output.wasm --cache-dir /tmp/clysm-cache
```

## Error Handling

### Invalid patterns

```bash
$ clysm compile 'nonexistent/**/*.lisp' -o output.wasm
error: No files matched pattern: nonexistent/**/*.lisp
exit code: 4
```

### Missing output flag

```bash
$ clysm compile 'src/**/*.lisp'
error: Missing required option: --output
usage: clysm compile <patterns...> -o <output> [options]
exit code: 3
```

### Compilation errors

```bash
$ clysm compile 'src/**/*.lisp' -o output.wasm
error: src/clysm/compiler/ast.lisp:123:45
  Unknown variable: FOO

Compilation failed with 1 error.
exit code: 2
```

### Partial success (with --continue)

```bash
$ clysm compile 'src/**/*.lisp' -o output.wasm --continue
error: src/clysm/compiler/ast.lisp:123:45
  Unknown variable: FOO

Partial compilation: 44/45 modules successful
Output: output.wasm (44123 bytes, excluding failed modules)
exit code: 1
```

## FFI Requirements

For Stage 1+ execution, the following FFI functions must be available:

| Function | Signature | Description |
|----------|-----------|-------------|
| `fs.glob` | `(pattern: string) → string[]` | Expand glob pattern |
| `fs.read` | `(path: string) → string` | Read file contents |
| `fs.write` | `(path: string, data: bytes) → void` | Write file |
| `fs.mkdir` | `(path: string) → void` | Create directory |
| `fs.exists` | `(path: string) → bool` | Check if path exists |
| `fs.mtime` | `(path: string) → int` | Get modification time |
| `process.args` | `() → string[]` | Get command-line arguments |
| `process.exit` | `(code: int) → never` | Exit with code |
| `console.log` | `(msg: string) → void` | Print to stdout |
| `console.error` | `(msg: string) → void` | Print to stderr |

## Implementation Notes

1. **Glob Expansion**: Patterns are expanded by host (Node.js) before processing
2. **Progress Display**: Uses ANSI escape codes for in-place updates
3. **Cache Invalidation**: On version change, full recompile is triggered
4. **Signal Handling**: SIGINT triggers graceful shutdown with partial output
