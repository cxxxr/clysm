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
# Stage 1 Generation (Phase 13D-7)
sbcl --load build/stage1-complete.lisp           # Generate dist/clysm-stage1.wasm
sbcl --load build/stage1-complete.lisp --verbose # With detailed progress
wasm-tools validate dist/clysm-stage1.wasm       # Validate output

# Stage 2 Generation (Phase 13D-9)
./scripts/run-stage2-gen.sh                      # Generate dist/clysm-stage2.wasm
./scripts/run-stage2-gen.sh --json               # JSON output for CI

# Bootstrap Stage 0
sbcl --load build/bootstrap.lisp
sbcl --load build/stage0-complete.lisp

# Fixpoint verification
./scripts/verify-fixpoint.sh                     # Compare Stage 1 and Stage 2
./scripts/verify-fixpoint.sh --json              # JSON output with diff details
./scripts/verify-fixpoint.sh --skip-generate     # Compare existing binaries only

# Run tests
sbcl --eval "(asdf:test-system :clysm)"
node tests/contract/fixpoint/test-exports.js    # Test Stage 1 exports
tests/integration/bootstrap/test-fixpoint.sh    # Integration test
```

## Current Status

**Phase 13 Infrastructure Complete**: Bootstrap pipeline established, but true self-hosting not yet achieved.

### What Works

- **SBCL Host Compiler**: `clysm:compile-to-wasm` successfully compiles Lisp → Wasm
- **Stage 1 Generation**: `sbcl --load build/stage1-complete.lisp` produces valid 24.5KB Wasm
- **Compilation Rate**: 14.2% of compiler forms compile AND validate successfully (164/1157)
- **Wasm Validation**: Generated Wasm passes `wasm-tools validate` with exit code 0
- **Control Structures**: `values`, `the`, `labels`, `handler-case` fully supported
- **Progress Reporting**: `dist/stage1-report.json` with per-module statistics and blocker analysis

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

1. **コンパイル率向上** (24.6% → 80%+)
   - `defstruct`, `loop` 等の完全サポート
   - `handler-case`, `values`, `the`, `labels` は実装済み (Phase 13D-6)
   - コンパイラが使用する全機能をWasmにコンパイル可能にする

2. **Stage 1生成**: SBCL + ClysmでClysm全体をWasmにコンパイル
3. **Stage 1実行**: Node.js + host-shimでStage 1を実行
4. **固定点達成**: Stage 1でClysm自身をコンパイル → Stage 2 == Stage 1

### Completed Features (017-045, 001-ansi-array-primitives, 001-defstruct-wasm-compile, 002-numeric-functions)

| Feature | Description |
|---------|-------------|
| 001-ansi-array-primitives | ANSI CL array/sequence primitives (aref, svref, schar, elt, coerce, setf forms) |
| 001-defstruct-wasm-compile | DEFSTRUCT macro → DEFCLASS expansion (:conc-name, :include, :predicate, :copier, :constructor, :read-only) |
| 002-numeric-functions | ANSI CL numeric functions: trig (sin, cos, tan, asin, acos, atan), hyperbolic (sinh, cosh, tanh, asinh, acosh, atanh), bit ops (ash, logand, logior, logxor, lognot, logcount, integer-length), math (sqrt, exp, log, expt, abs, signum), conversion (float, rational), parse-integer |
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
| 040-stage1 | Stage 1 compiler generation (24.5KB valid Wasm, 14.2% compilation rate) |
| 042 | Advanced defmacro (&whole, &environment) |
| 043 | Self-hosting blockers analysis (~23% compilation rate) |
| 044 | Interpreter bootstrap strategy (design only) |
| 045 | Stage 0 module structure (stubs, not functional) |
| 13D-9 | Bootstrap fixpoint: Stage 1 exports, Stage 2 generation, fixpoint verification |

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
- Common Lisp (SBCL 2.4+) host compiler + alexandria, babel (UTF-8), trivial-gray-streams (001-control-structure-extension)
- Common Lisp (SBCL 2.4+) + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing) (040-stage1-compiler-gen)
- N/A (in-memory compilation, Wasm binary output to `dist/`) (040-stage1-compiler-gen)
- JavaScript (Node.js 20+ with WasmGC support) + Node.js fs module, existing host-shim (io-shim.js, fs-shim.js), wasm-tools (validation) (001-stage1-runtime)
- N/A (file I/O via FFI functions to host filesystem) (001-stage1-runtime)
- Common Lisp (SBCL 2.4+) for host compiler, JavaScript (Node.js 20+) for runtime + alexandria, babel (UTF-8), wasm-tools, wasmtime (001-bootstrap-fixpoint)
- N/A (in-memory compilation, Wasm binary output to dist/) (001-bootstrap-fixpoint)
- Common Lisp (SBCL 2.4+) for host compiler + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing) (001-defstruct-wasm-compile)
- Common Lisp (SBCL 2.4+) host compiler, WasmGC target + alexandria, babel (UTF-8), existing bit operations (ash, logand, logior, logxor) (001-numeric-predicates)

## Recent Changes
- 001-numeric-functions: Added Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
