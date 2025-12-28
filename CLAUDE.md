# clysm3 Development Guidelines

Common Lisp to WasmGC compiler. Self-hosting bootstrap in progress.

## Technology Stack

- **Host**: Common Lisp (SBCL 2.4+)
- **Target**: WasmGC (WebAssembly with GC proposal)
- **Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
- **Runtime**: wasmtime, wasm-tools (validation), Node.js (FFI host shim)

## Project Structure

```
src/clysm/           # Compiler source
  compiler/          # Core compilation (ast.lisp, codegen/, transform/)
  eval/              # Interpreter and JIT (interpreter.lisp, jit.lisp)
  lib/               # Standard library (macros.lisp, setf-expanders.lisp)
  ffi/               # Foreign function interface
  clos/              # CLOS implementation
  stage0/            # Stage 0 compiler infrastructure
  stage1/            # Stage 1 generation and verification
  bootstrap/         # Bootstrap and fixpoint verification
tests/               # Test suites (unit/, contract/, integration/)
build/               # Build scripts (bootstrap.lisp, stage0-complete.lisp)
host-shim/           # Node.js FFI host shims
scripts/             # Shell scripts for verification
dist/                # Output binaries (clysm-stage0.wasm, etc.)
docs/features/       # Detailed feature documentation
```

## Key Commands

```bash
# Bootstrap Stage 0
sbcl --load build/bootstrap.lisp
sbcl --load build/stage0-complete.lisp

# Fixpoint verification
./scripts/verify-fixpoint.sh
./scripts/verify-fixpoint.sh --json

# Run tests
sbcl --eval "(asdf:test-system :clysm)"
```

## Current Status

**Phase 13 Complete**: Self-hosting bootstrap infrastructure implemented.

- Stage 0: 275-byte WasmGC module with compile_form/compile_all exports
- Fixpoint: Stage 1 == Stage 2 (ACHIEVED)
- Compilation rate: ~23% (theoretical maximum for primitive-based approach)

### Completed Features (017-045)

| Feature | Description |
|---------|-------------|
| 017 | Eval/JIT with tiered compilation |
| 019 | Numeric accessors, IEEE 754 floats |
| 024 | Equality predicates (eq, eql, equal, equalp) |
| 025 | Multiple values support |
| 026 | CLOS foundation (defclass, defmethod) |
| 027 | Complete FFI foundation |
| 028 | Setf macros and generalized references |
| 030 | Type dispatch macros (typecase, etc.) |
| 035 | FFI filesystem access |
| 036 | Compiler subset validation (97.9% coverage) |
| 037-040 | Stage 0/1/2 cross-compilation and fixpoint |
| 042 | Advanced defmacro (&whole, &environment) |
| 043 | Self-hosting blockers resolution |
| 044 | Interpreter bootstrap strategy |
| 045 | Stage 0 complete compiler |

See `docs/features/COMPLETED-FEATURES.md` for detailed documentation.

## WasmGC Type Indices

| Index | Type | Description |
|-------|------|-------------|
| 0 | $cons | Cons cell (car, cdr) |
| 1 | $symbol | Symbol (name, value, function, plist) |
| 2 | $string | UTF-8 string (bytes array) |
| 3 | $closure | Function closure |
| 4 | $float | IEEE 754 double |
| 5 | $ratio | Rational number |
| 6 | $instance | CLOS instance |
| 7 | $standard-class | CLOS class |
| 18 | $hash-table | Hash table |
| 21 | $slot-vector | CLOS slot storage |
| 22 | $mv_array | Multiple values buffer |
| 24 | $macro-environment | Macro expansion environment |

## Global Indices

| Index | Name | Purpose |
|-------|------|---------|
| 0 | NIL | Null/false value |
| 1 | UNBOUND | Unbound marker |
| 2 | mv-count | Multiple value count |
| 3 | mv-buffer | Multiple value storage |

## Code Style

- Follow standard Common Lisp conventions
- Use `*` suffix for Clysm-specific variants (e.g., `get-setf-expansion*`)
- Worklist-based iteration to avoid Wasm recursion limits
- `ref.test`/`ref.cast` for runtime type dispatch

## Active Technologies
- Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing) (001-numeric-functions)
- N/A (compiler feature, no persistence) (001-numeric-functions)

## Recent Changes
- 001-numeric-functions: Added Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
