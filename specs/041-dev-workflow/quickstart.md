# Quickstart: Clysm Development Workflow

**Date**: 2025-12-27
**Feature**: 041-dev-workflow

## Prerequisites

- Nix with flakes enabled (recommended)
- Or: SBCL 2.4+, wasmtime, Node.js 18+, wasm-tools

## Installation

### Option 1: Nix (Recommended)

```bash
# Enter development environment
nix develop

# All tools are now available
clysm --version
wasmtime --version
wasm-tools --version
```

### Option 2: Manual

```bash
# Install SBCL
# macOS: brew install sbcl
# Ubuntu: apt install sbcl

# Install wasmtime
curl https://wasmtime.dev/install.sh -sSf | bash

# Install Node.js 18+
# See https://nodejs.org/

# Install wasm-tools
cargo install wasm-tools
```

## Quick Start

### 1. Compile Clysm

```bash
# Compile all Clysm source files
clysm compile 'src/**/*.lisp' -o dist/clysm.wasm

# Output:
# Compiling 45 modules...
# [45/45] src/clysm/lib/macros.lisp ... OK
# Compilation complete: 45/45 modules, 0 errors
# Output: dist/clysm.wasm (123456 bytes)
```

### 2. Verify the output

```bash
# Validate Wasm structure
wasm-tools validate dist/clysm.wasm

# View Wasm text format
wasm-tools print dist/clysm.wasm | head -50
```

### 3. Run with wasmtime

```bash
# Execute compiled Wasm
wasmtime run dist/clysm.wasm --invoke main
```

## Common Tasks

### Incremental compilation

After making changes, just rerun the compile command:

```bash
# Edit a file...
vim src/clysm/compiler/ast.lisp

# Recompile (only changed files)
clysm compile 'src/**/*.lisp' -o dist/clysm.wasm
# Output: "1 module changed, recompiling..."
```

### Force full rebuild

```bash
clysm compile 'src/**/*.lisp' -o dist/clysm.wasm --force
```

### Verbose output

```bash
clysm compile 'src/**/*.lisp' -o dist/clysm.wasm --verbose
```

### Compile specific modules

```bash
# Just the backend
clysm compile 'src/clysm/backend/**/*.lisp' -o dist/backend.wasm

# Multiple patterns
clysm compile 'src/clysm/backend/*.lisp' 'src/clysm/compiler/*.lisp' -o dist/partial.wasm
```

## REPL Usage

### Start the REPL

```bash
# Using SBCL
sbcl --load build/load.lisp
```

### Compile a file from REPL

```lisp
;; Basic compilation
(compile-file "src/clysm/backend/leb128.lisp")

;; With verbose output
(compile-file "src/clysm/backend/leb128.lisp" :verbose t)

;; Force recompilation
(compile-file "src/clysm/backend/leb128.lisp" :force t)

;; Custom output path
(compile-file "src/clysm/backend/leb128.lisp"
              :output-file "dist/leb128.wasm")
```

### Compile and load

```lisp
;; Compile and immediately load the module
(load (compile-file "my-module.lisp"))
```

## Self-Hosting (Advanced)

Once Stage 1 is working, you can compile without SBCL:

```bash
# Use Stage 1 to compile (no SBCL needed)
./bin/clysm-stage1 compile 'src/**/*.lisp' -o dist/clysm-new.wasm

# Verify fixed-point (Stage 1 == Stage 2)
./scripts/verify-fixpoint.sh
```

## Troubleshooting

### "No files matched pattern"

Check your glob pattern:
```bash
# List matching files first
ls -la src/**/*.lisp

# Use quotes to prevent shell expansion
clysm compile 'src/**/*.lisp' -o output.wasm
```

### Compilation errors

View detailed error information:
```bash
clysm compile 'src/**/*.lisp' -o output.wasm --verbose 2>&1 | less
```

### Cache issues

Clear the cache and rebuild:
```bash
rm -rf .clysm-cache/
clysm compile 'src/**/*.lisp' -o output.wasm
```

### Wasm validation errors

Check the output with wasm-tools:
```bash
wasm-tools validate output.wasm 2>&1
wasm-tools print output.wasm > output.wat
```

## Project Structure

```
.
├── src/clysm/           # Source files
│   ├── backend/         # Wasm binary generation
│   ├── compiler/        # AST and codegen
│   ├── runtime/         # Runtime support
│   └── ...
├── dist/                # Compiled outputs
│   └── clysm.wasm       # Main binary
├── .clysm-cache/        # Compilation cache
│   └── compilation-cache.sexp
└── tests/               # Test files
```

## Next Steps

- Read the [CLI Contract](contracts/cli.md) for full option documentation
- Read the [compile-file Contract](contracts/compile-file.md) for REPL details
- Read the [Data Model](data-model.md) for internal structure
- Run tests: `nix flake check` or `sbcl --load tests/run.lisp`
