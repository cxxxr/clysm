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

### Completed Features (017-045, 001-ansi-array-primitives, 001-ansi-array-ops, 001-ansi-char-functions, 001-ansi-string-trim, 001-defstruct-wasm-compile, 002-numeric-functions, 001-numeric-format, 001-arithmetic-primitives, 002-primitive-dispatch-table, 001-equality-type-dispatch)

| Feature | Description |
|---------|-------------|
| 001-equality-type-dispatch | Consolidated [eq](resources/HyperSpec/Body/f_eq.htm), [eql](resources/HyperSpec/Body/f_eql.htm), [equal](resources/HyperSpec/Body/f_equal.htm), [equalp](resources/HyperSpec/Body/f_equalp.htm) into unified type-dispatch infrastructure. compile-equality-predicate master function with :eq/:eql/:equal/:equalp levels. Stage 1 compiles, Wasm validates. |
| 002-primitive-dispatch-table | Hash-table driven primitive dispatch replacing 538-line case statement. 239 primitives registered with O(1) lookup. Extensible via register-primitive-compiler API. |
| 001-arithmetic-primitives | Arithmetic primitives [1-](resources/HyperSpec/Body/f_1pl_1_.htm) and [1+](resources/HyperSpec/Body/f_1pl_1_.htm) for recursive/iterative algorithms |
| 001-ansi-array-primitives | ANSI CL array/sequence primitives (aref, svref, schar, elt, coerce, setf forms) |
| 001-ansi-char-functions | ANSI CL character functions: graphic-char-p, standard-char-p, both-case-p, char-name, name-char, digit-char, char-int |
| 001-ansi-string-trim | ANSI CL string trim functions: string-trim, string-left-trim, string-right-trim, nstring-upcase, nstring-downcase, nstring-capitalize with :start/:end support |
| 001-ansi-array-ops | ANSI CL array operations: array-rank, array-dimension, array-dimensions, array-total-size, array-row-major-index, row-major-aref, (setf row-major-aref), adjustable-array-p, adjust-array |
| 001-defstruct-wasm-compile | DEFSTRUCT macro → DEFCLASS expansion (:conc-name, :include, :predicate, :copier, :constructor, :read-only) |
| 001-numeric-format | ANSI CL numeric conversion/formatting: rationalize (float→ratio with continued fraction), write-to-string (:base 2-36 for integers, ratio/float support) |
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
| 28 | $mdarray | Multidimensional array wrapper (dimensions, storage, adjustable) |

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
- Common Lisp (SBCL 2.4+) for host compiler + alexandria, babel (UTF-8), trivial-gray-streams, existing clysm compiler infrastructure (001-ansi-list-ops)
- Common Lisp (SBCL 2.4+) host compiler, WasmGC target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing), existing clysm compiler infrastructure (001-ansi-sequence-functions)
- N/A (in-memory compilation, no persistence) (001-ansi-sequence-functions)
- Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, babel (UTF-8), trivial-gray-streams; existing array infrastructure from `001-ansi-array-primitives` (001-ansi-array-ops)
- Common Lisp (SBCL 2.4+) for host compiler + alexandria, babel (UTF-8), trivial-gray-streams, existing Clysm compiler infrastructure (001-ansi-string-trim)
- Common Lisp (SBCL 2.4+) for host compiler + alexandria, babel (UTF-8), existing clysm compiler infrastructure (001-char-literal-compile)
- Common Lisp (SBCL 2.4+) host compiler, WasmGC target + alexandria, babel (UTF-8), existing FFI infrastructure (feature 027), existing format implementation (001-io-print-primitives)
- Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, babel (UTF-8), existing clysm compiler infrastructure (001-division-rounding-primitives)
- Common Lisp (SBCL 2.4+) + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing), wasm-tools (validation) (001-m2-blocker-analysis)
- Common Lisp (SBCL 2.4+) for host compiler + alexandria, babel (UTF-8), existing Clysm compiler infrastructure (001-m3-clos-primitives)
- N/A (in-memory compilation, JSON reports to dist/) (001-m4-defun-blocker-analysis)
- Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target runtime + alexandria, babel (UTF-8), trivial-gray-streams; existing FFI infrastructure (feature 027) (001-io-list-runtime)
- Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, trivial-gray-streams, wasm-tools (validation) (001-type-package-export)
- Common Lisp (SBCL 2.4+) + alexandria, babel (UTF-8), existing clysm compiler infrastructure (001-ast-function-export)
- Common Lisp (SBCL 2.4+) + alexandria (hash-table utilities) (001-primitive-dispatch-table)
- N/A (in-memory dispatch tables) (002-primitive-dispatch-table)
- Common Lisp (SBCL 2.4+) + alexandria (hash-table utilities), existing clysm compiler infrastructure (001-cxr-compiler-macro)
- Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, existing primitive-dispatch.lisp infrastructure (001-equality-type-dispatch)

## Recent Changes
- 001-cxr-compiler-macro: Consolidated 12 compile-cXXr functions ([caar](resources/HyperSpec/Body/f_car_c.htm), [cadr](resources/HyperSpec/Body/f_car_c.htm), [cdar](resources/HyperSpec/Body/f_car_c.htm), [cddr](resources/HyperSpec/Body/f_car_c.htm), caaar through cdddr) into define-cxr-compiler macro. Added ops-to-expansion helper for docstring generation. Macro validates operation strings (a/d characters only). 12 five-line defuns replaced by 12 single-line macro invocations. Unit tests in tests/unit/cxr-macro-test.lisp (6 tests pass). Stage 1 compilation and Wasm validation pass.
- 001-wasm-local-binding: Fixed Wasm local variable instruction binding issues. Added keyword literal handling to AST parser (keywordp check in parse-expr) and compile-literal (:keyword case). Exported ADVANCE-TOKEN, CURRENT-TOKEN, MAKE-PARSER-STATE from clysm/reader/parser and re-exported from clysm package. Exported EMIT-MODULE-HEADER from clysm/backend/wasm-emit and re-exported from clysm package. Registered parser functions (advance-token, current-token, make-parser-state) and backend function (emit-module-header) in *runtime-function-table*. Target error patterns (P221/LOCAL.SET, P987/LOCAL.TEE, P027/ADVANCE-TOKEN, P143/EMIT-MODULE-HEADER, P943/AST-TAGBODY) eliminated. Unit tests in tests/unit/local-instruction-test.lisp, advance-token-export-test.lisp, emit-header-export-test.lisp, ast-tagbody-test.lisp. Wasm validation passes.
- 001-ast-function-export: Exported 8 AST manipulation functions (compile-to-instructions, make-wasm-struct-type, wasm-struct-type-p, wasm-struct-type-fields, make-ast-literal, ast-literal-value, ast-literal-p, get-numeric-value) to clysm package. Registered all functions in *runtime-function-table* with correct arities. Added get-numeric-value export from clysm/compiler/ast. Unit tests in tests/unit/ast-export-test.lisp (32 assertions pass). Target error patterns (P944, P321, P543, P106) eliminated. Stage 1 coverage: 19%, Wasm validation passes.
- 001-type-package-export: Implemented compiler enhancement for type constant and package primitive export. Added DEFCONSTANT handling with *constant-bindings* registry and compile-time constant folding (i32.const emission). Pre-populated 28 type constants (+type-nil+ through +type-mdarray+) for constant folding. Registered SYMBOL-PACKAGE* in runtime function table. Added unit tests in tests/unit/constant-export-test.lisp and tests/unit/package-primitive-test.lisp. P846 error pattern eliminated (0 errors). Wasm validation passes.
- 001-lexenv-function-export: Exported 3 lexical environment functions (env-add-local, loop-keyword-eq, numeric-literal-p) to clysm package. Registered functions in *runtime-function-table* for Wasm dispatch (env-add-local with variadic arity for optional type param). Eliminated ENV-ADD-LOCAL, LOOP-KEYWORD-EQ, and NUMERIC-LITERAL-P undefined function error patterns. Added tests in tests/unit/lexenv-export-test.lisp. Wasm validation passes.
- 001-internal-function-export: Exported 9 internal compiler functions (lexical-env-parent, lexical-env-bindings, make-lexical-env, compile-to-instructions, make-wasm-struct-type, wasm-struct-type-p, wasm-struct-type-fields, ast-literal-value, ast-literal-p) to clysm package. Added quasiquote expansion at AST parsing level to eliminate P464 errors. Registered package functions (packagep*, find-package*, intern*) as runtime functions. P464 (QUASIQUOTE) error pattern completely eliminated (16 → 0). Stage 1 coverage: 21.43%, Wasm validation passes.
- 001-internal-function-consolidation: Exported 2 internal compiler functions (compile-unary-math-ffi, compile-cxr-chain) to clysm package. Removed dead code from func-section.lisp for I/O, list, and sequence functions now dispatched via *runtime-function-table*. File reduced from 18,351 to 16,366 lines (-1,985 lines). Updated dispatch table comments. Stage 1 Wasm validation passes.
- 001-sequence-runtime-migration: Migrated sequence functions ([remove](resources/HyperSpec/Body/f_rm_rm.htm), [count](resources/HyperSpec/Body/f_countc.htm), [substitute](resources/HyperSpec/Body/f_sbs_s.htm), [delete](resources/HyperSpec/Body/f_rm_rm.htm) families - 12 functions) from inline Wasm codegen to Lisp runtime library. All ANSI keyword arguments supported (:key, :test, :start, :end, :count, :from-end). 454 lines of maintainable Lisp in sequence-runtime.lisp. 49 tests pass. Stage 1 coverage: 14.2% → 22.15%.
- 001-io-list-runtime (Phase 13D-1f): Runtime library migration infrastructure. Added runtime function dispatch mechanism (*runtime-function-table*, compile-runtime-call). Implemented I/O runtime functions ([princ](resources/HyperSpec/Body/f_wr_pr.htm), [prin1](resources/HyperSpec/Body/f_wr_pr.htm), [print](resources/HyperSpec/Body/f_wr_pr.htm), [write](resources/HyperSpec/Body/f_wr_pr.htm), [format](resources/HyperSpec/Body/f_format.htm), [terpri](resources/HyperSpec/Body/f_terpri.htm)) in io-runtime.lisp. Implemented list runtime functions ([member](resources/HyperSpec/Body/f_mem_m.htm), [assoc](resources/HyperSpec/Body/f_assocc.htm), [rassoc](resources/HyperSpec/Body/f_rassoc.htm), [find](resources/HyperSpec/Body/f_find_.htm), [position](resources/HyperSpec/Body/f_pos_p.htm)) in list-runtime.lisp. Runtime library total: 602 lines.
- 001-m2-blocker-analysis (Phase 13D M2): Blocker analysis and DEFMACRO skip. Analyzed compilation blockers (DEFSTRUCT 1953, DEFMACRO 646, DEFINE-CONDITION 302, DEFVAR 133). Implemented [DEFMACRO](resources/HyperSpec/Body/m_defmac.htm) skip in directive.lisp. Coverage: 13.90% → 14.26%. Remaining blockers require CLOS primitives (slot-value*, make-instance*) - deferred to M3.
- 001-division-rounding-primitives (Phase 13D-1e): Implemented ANSI CL division/rounding functions ([floor](resources/HyperSpec/Body/f_floorc.htm), [ceiling](resources/HyperSpec/Body/f_floorc.htm), [round](resources/HyperSpec/Body/f_floorc.htm), [ffloor](resources/HyperSpec/Body/f_floorc.htm), [fceiling](resources/HyperSpec/Body/f_floorc.htm), [fround](resources/HyperSpec/Body/f_floorc.htm)) as WasmGC primitives. Returns 2 values (quotient/remainder) via mv-count/mv-buffer. Supports both integer and float arguments, single/two-argument forms. Added f64.ceil opcode (0x9C) to emitter.
- 002-compile-time-directives (Phase 13D-3): Implemented compile-time directive skip integration. Directives ([in-package](resources/HyperSpec/Body/m_in_pkg.htm), [defpackage](resources/HyperSpec/Body/m_defpkg.htm), [declaim](resources/HyperSpec/Body/m_declai.htm), [proclaim](resources/HyperSpec/Body/f_procla.htm)) now return :skipped instead of failing. Stage 1 report shows 458 skipped forms with DEFPACKAGE/IN-PACKAGE/DECLAIM removed from top_blockers. Coverage calculation uses adjusted formula: compiled / (total - skipped).
- 001-io-print-primitives (Phase 13D-1d): Implemented I/O print primitives for compilation. Added print, prin1, princ, terpri, write functions as FFI-backed stubs. Implemented format function with basic directives (~A, ~S, ~D, ~%, ~&, ~~). Resolves DEFUN compilation failures where print-related forms caused errors.
- 001-ansi-array-ops (Phase 15C): Implemented 9 ANSI CL array operations: array-rank, array-dimension, array-dimensions, array-total-size, array-row-major-index, row-major-aref, (setf row-major-aref), adjustable-array-p, adjust-array. Added $mdarray type (index 28) for multidimensional array support.
- 001-numeric-functions: Added Common Lisp (SBCL 2.4+) for host compiler, WasmGC for target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
