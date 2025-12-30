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

**Phase 13 Infrastructure Complete**: Bootstrap pipeline established, but true self-hosting not yet achieved.

### What Works

- **SBCL Host Compiler**: `clysm:compile-to-wasm` successfully compiles Lisp → Wasm
- **Compilation Rate**: ~23% of compiler forms compile successfully
- **Wasm Validation**: Generated Wasm passes `wasm-tools validate`

### What Doesn't Work Yet

- **Stage 0**: 275-byte module with stub functions (`compile_form` returns `ref.null extern`)
- **Stage 1/2**: Cannot be generated from Stage 0 (stubs produce empty modules)
- **Fixpoint**: Not achievable until Stage 0 has actual compiler logic

### Bootstrap Pipeline (Current State)

```
SBCL + Clysm Host Compiler → Wasm bytecode (works)
                    ↓
Stage 0 (275 bytes, stubs only) → Stage 1 (empty, 17 bytes)
                                          ↓
                               Fixpoint not meaningful (empty == empty)
```

### Next Steps for True Self-Hosting (Phase 13D)

**Goal**: SBCL上のClysmでClysm自身をWasmにコンパイル

1. **コンパイル率向上** (23% → 80%+)
   - `defstruct`, `loop`, `handler-case` 等の完全サポート
   - コンパイラが使用する全機能をWasmにコンパイル可能にする

2. **Stage 1生成**: SBCL + ClysmでClysm全体をWasmにコンパイル
3. **Stage 1実行**: Node.js + host-shimでStage 1を実行
4. **固定点達成**: Stage 1でClysm自身をコンパイル → Stage 2 == Stage 1

### Completed Features (017-045, 001-ansi-array-primitives)

| Feature | Description |
|---------|-------------|
| 001-ansi-array-primitives | ANSI CL array/sequence primitives (aref, svref, schar, elt, coerce, setf forms) |
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
| 037-040 | Bootstrap infrastructure (Stage 0 stubs, verification scripts) |
| 042 | Advanced defmacro (&whole, &environment) |
| 043 | Self-hosting blockers analysis (~23% compilation rate) |
| 044 | Interpreter bootstrap strategy (design only) |
| 045 | Stage 0 module structure (stubs, not functional) |

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
- Common Lisp (SBCL 2.4+) for host, WasmGC for target + alexandria, babel (UTF-8), wasmtime, wasm-tools (001-true-self-hosting)
- N/A (in-memory compilation only) (001-true-self-hosting)
- Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, babel (UTF-8), wasmtime, wasm-tools (001-ansi-numeric-functions)
- Common Lisp (SBCL 2.4+) for host compiler + alexandria, babel (UTF-8), wasmtime, wasm-tools (001-ansi-array-primitives)
- N/A (in-memory compilation) (001-ansi-array-primitives)
- Common Lisp (SBCL 2.4+) + alexandria, babel (UTF-8) (001-compile-time-directives)
- N/A (in-memory compilation, Wasm binary output) (001-global-variable-defs)

## Recent Changes
- 001-numeric-functions: Added Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
