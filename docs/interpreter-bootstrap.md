# Interpreter Bootstrap Strategy

**Feature 044: Interpreter Bootstrap Strategy**

This document describes the interpreter-based bootstrap strategy for Clysm's self-hosting capability.

## Overview

The interpreter bootstrap strategy provides an alternative path to self-hosting that reduces dependency on SBCL during the bootstrap process. Instead of relying solely on SBCL to compile the compiler, this approach uses the Tier 1 interpreter to load and run the compiler source code.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Standard Bootstrap                        │
│  SBCL ──compile──> Stage 0 ──wasmtime──> Stage 1 ──> Stage 2│
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                 Interpreter Bootstrap                        │
│  SBCL+Interp ──interpret──> Stage 0 ──wasmtime──> Stage 1 ──> Stage 2│
│       ↑                          ↑                           │
│       │                          │                           │
│    Tier 1                   Valid Wasm                       │
│  Interpreter                 Binary                          │
└─────────────────────────────────────────────────────────────┘
```

## Key Components

### 1. Tier 1 Interpreter (`src/clysm/eval/interpreter.lisp`)

The interpreter provides:
- Full Common Lisp special forms support
- Lambda-list parsing with &optional, &rest, &key, &aux
- Macro definition and expansion
- Condition system integration
- File loading and package switching

### 2. Bootstrap Infrastructure (`src/clysm/bootstrap/`)

- **package.lisp**: Package definition with exports
- **interpreter-stage0.lisp**: Stage 0 generation via interpreter
- **fixpoint.lisp**: Fixed-point verification integration

### 3. Scripts

- **scripts/gen-stage0-interp.sh**: Generate Stage 0 via interpreter
- **scripts/verify-fixpoint-interp.sh**: Interpreter-based fixpoint verification
- **scripts/bootstrap-without-sbcl.sh**: Full SBCL-free workflow
- **scripts/run-tests-via-interpreter.sh**: Run tests via interpreter

## Usage

### Generate Stage 0 via Interpreter

```bash
# Using the shell script
./scripts/gen-stage0-interp.sh --verbose

# Or directly via SBCL
sbcl --load build/bootstrap-interp.lisp
```

### Verify Fixed-Point

```bash
# Interpreter-based verification
./scripts/verify-fixpoint-interp.sh

# Or use the standard script with --interpreter flag
./scripts/verify-fixpoint.sh --interpreter
```

### Check SBCL-Free Workflow

```bash
# Requires pre-existing Stage 0
./scripts/bootstrap-without-sbcl.sh --verbose
```

## API Reference

### Bootstrap Result Struct

```lisp
(clysm/interpreter-bootstrap:make-bootstrap-result
  :success t
  :wasm-bytes <vector>
  :modules-loaded 45
  :forms-compiled 218
  :errors nil
  :elapsed-time 12.5)
```

### Main Functions

| Function | Description |
|----------|-------------|
| `generate-stage0-via-interpreter` | Generate Stage 0 Wasm binary |
| `verify-fixpoint-interpreter` | Run fixed-point verification |
| `run-full-bootstrap` | Execute full Stage 0 → 1 → 2 chain |
| `form-compilable-p` | Check if a form is compilable |
| `validate-stage0-binary` | Validate binary with wasm-tools |

### Progress Callback

```lisp
(let ((clysm/interpreter-bootstrap:*bootstrap-progress-callback*
       (lambda (phase module-count form-count)
         (format t "Phase: ~A, Modules: ~D, Forms: ~D~%"
                 phase module-count form-count))))
  (clysm/interpreter-bootstrap:generate-stage0-via-interpreter))
```

## Exit Codes

| Code | Status | Description |
|------|--------|-------------|
| 0 | ACHIEVED | Stage 1 == Stage 2 (fixed-point verified) |
| 1 | NOT_ACHIEVED | Binaries differ |
| 2 | COMPILATION_ERROR | Stage generation failed |
| 3 | MISSING_DEPENDENCY | wasmtime/wasm-tools/Stage 0 missing |
| 77 | SKIP | Known limitation (Stage 0 lacks exports) |

## Known Limitations

### Current Compilation Rate

The interpreter-based Stage 0 currently compiles approximately 23% of forms. This is due to:

1. **Internal compiler functions**: Functions like `env-add-local`, `compile-to-instructions` are internal to the compiler and cannot be expressed as standalone Wasm primitives.

2. **CL subset limitations**: Some features used by the compiler source (complex `loop`, `format`, etc.) are not fully supported.

### SBCL-Free Workflow Status

The current interpreter-generated Stage 0:
- ✅ Generates valid Wasm binary
- ✅ Passes wasm-tools validation
- ❌ Does not export `compile_all` (minimal binary)
- ❌ Cannot run on wasmtime as a compiler

### Resolution Path

To achieve full SBCL-free self-hosting:

1. **Extend CL subset**: Add support for more CL features used by compiler source
2. **Rewrite source**: Rewrite Clysm source using only the blessed subset
3. **Hybrid approach**: Use interpreter for initial load, host CL for compilation

## Testing

### Run Interpreter Tests

```bash
# Run all interpreter tests
./scripts/run-tests-via-interpreter.sh

# Run with verbose output
./scripts/run-tests-via-interpreter.sh --verbose

# Run specific test pattern
./scripts/run-tests-via-interpreter.sh defun
```

### Test Coverage

- **Unit tests**: `tests/unit/interpreter/*.lisp` (100 tests)
- **Contract tests**: `tests/contract/interpreter-*.lisp` (7 tests)
- **Integration tests**: `tests/integration/stage0-*.lisp`, `tests/integration/sbcl-free-test.lisp`

## Development Workflow

### Adding New Interpreter Features

1. Write tests in `tests/unit/interpreter/`
2. Implement in `src/clysm/eval/interpreter*.lisp`
3. Export from `src/clysm/package.lisp`
4. Run tests: `./scripts/run-tests-via-interpreter.sh`

### Debugging Bootstrap Issues

1. Enable verbose mode: `--verbose` flag
2. Check progress callback phases
3. Review error list in bootstrap-result
4. Use `form-compilable-p` to check individual forms

## Related Documentation

- [CLAUDE.md](../CLAUDE.md) - Project development guidelines
- [Feature 043: Self-Hosting Blockers](../specs/043-self-hosting-blockers/) - Related blockers
- [Feature 040: Fixed-Point Verification](../specs/040-fixed-point-verification/) - Verification infrastructure
