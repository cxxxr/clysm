# clysm3 Development Guidelines

Auto-generated from all feature plans. Last updated: 2025-12-21

## Active Technologies
- Common Lisp (SBCL 2.4+) + alexandria, babel, trivial-gray-streams, rove (testing) (002-special-vars-compiler)
- N/A (compiler generates Wasm binaries) (002-special-vars-compiler)
- Common Lisp (SBCL 2.4+) - コンパイラ本体 + alexandria, babel, trivial-gray-streams, rove (testing) (007-sequence-functions)
- N/A (コンパイラはWasmバイナリを生成) (007-sequence-functions)
- N/A（コンパイラはWasmバイナリを生成） (008-character-string)
- Common Lisp (SBCL 2.4+) for compiler, WasmGC for output + alexandria, babel, trivial-gray-streams, rove (testing) (010-numeric-tower)
- Common Lisp (SBCL 2.4+) - コンパイラ実装言語 + alexandria, babel, trivial-gray-streams, rove (testing) (013-package-system)
- N/A（インメモリハッシュテーブル） (013-package-system)
- Common Lisp (SBCL 2.4+) - compiler implementation language + alexandria, babel, trivial-gray-streams, rove (testing) (014-condition-system)
- N/A (in-memory condition/handler/restart stacks) (014-condition-system)
- Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target + alexandria, babel (UTF-8), trivial-gray-streams, rove (testing); existing FFI foundation (012), condition system (014), special variables (002), character/string types (008) (015-ffi-stream-io)
- N/A (in-memory streams to host file descriptors) (015-ffi-stream-io)
- Common Lisp (SBCL 2.4+) - compiler implementation + alexandria, babel, trivial-gray-streams, rove (testing) (016-macro-system)
- N/A (compile-time only, in-memory macro registry) (016-macro-system)
- N/A (in-memory data structures for function slots, invocation counters) (017-eval-jit-compile)
- Common Lisp (SBCL 2.4+) + alexandria, babel, trivial-gray-streams, rove (testing), clysm/ffi (012), clysm/conditions (014) (018-fix-ffi-streams)
- Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target + alexandria, babel, trivial-gray-streams, rove (testing); existing numeric tower (010) (019-numeric-accessors)
- N/A (compile-time code generation) (019-numeric-accessors)

- Common Lisp (SBCL 2.4+) - コンパイラ本体、WAT/Wasm - 出力 (001-clysm-compiler)

## Project Structure

```text
src/
tests/
```

## Commands

# Add commands for Common Lisp (SBCL 2.4+) - コンパイラ本体、WAT/Wasm - 出力

## Code Style

Common Lisp (SBCL 2.4+) - コンパイラ本体、WAT/Wasm - 出力: Follow standard conventions

## Recent Changes
- 019-numeric-accessors: Added Common Lisp (SBCL 2.4+) - compiler implementation; WasmGC - output target + alexandria, babel, trivial-gray-streams, rove (testing); existing numeric tower (010)
- 018-fix-ffi-streams: Added Common Lisp (SBCL 2.4+) + alexandria, babel, trivial-gray-streams, rove (testing), clysm/ffi (012), clysm/conditions (014)
- 017-eval-jit-compile: Added Common Lisp (SBCL 2.4+) - compiler implementation + alexandria, babel, trivial-gray-streams, rove (testing)


<!-- MANUAL ADDITIONS START -->
## Feature 017: Eval/JIT Compile System - COMPLETE

**Status**: All 54 tasks completed (2025-12-24)

### Implemented Components
- `src/clysm/eval/compile.lisp`: Tiered compilation with graceful degradation
- `src/clysm/eval/jit.lisp`: JIT infrastructure with runtime imports
- `src/clysm/eval/interpreter.lisp`: Tier 1 S-expression interpreter

### Key Features
1. **compile* function**: `(compile nil '(lambda ...))` returns callable function
2. **Tiered execution**: Tier 1 interpreter + Tier 2 JIT compilation
3. **Hot spot detection**: Automatic tier promotion after threshold (default: 10)
4. **Graceful degradation**: Falls back to Tier 1 if JIT fails
5. **Hot-patching**: Named functions can be recompiled at runtime
6. **Runtime imports**: 40+ standard functions available for JIT modules

### Test Coverage
- Unit tests: 15+ tests for compile*, tier management, struct
- Contract tests: 9 tests for Wasm generation and module linking
- Integration tests: 13 tests for tier promotion, hot-patching, special forms

## Feature 019: Numeric Accessors and Float Special Values - COMPLETE

**Status**: All 38 tasks completed (2025-12-25)

### Implemented Components
- `src/clysm/compiler/codegen/func-section.lisp`: numerator/denominator accessors, float comparisons
- `src/clysm/compiler/ast.lisp`: IEEE 754 float traps masking for constant folding

### Key Features
1. **numerator/denominator accessors**: ANSI CL accessor functions for ratios and integers
   - `(numerator 2/3)` → 2, `(denominator 2/3)` → 3
   - `(numerator 5)` → 5, `(denominator 5)` → 1
2. **IEEE 754 special values**: +Infinity, -Infinity, NaN from float division
   - `(/ 1.0 0.0)` → +Infinity, `(/ -1.0 0.0)` → -Infinity
3. **Float-aware comparisons**: =, <, >, <=, >=, /= handle floats with f64 instructions
4. **Double-float precision**: Full 64-bit IEEE 754 precision preserved in compilation

### Technical Details
- Runtime type dispatch using `ref.test` and `ref.cast` for ratio/float detection
- Wasm `f64.eq/lt/gt/le/ge` for IEEE 754-compliant float comparisons
- `sb-int:with-float-traps-masked` for safe constant folding of special values

### Known Limitation
- Test harness requires FFI shim (host-shim/) for wasmtime execution
- All Wasm modules validate correctly with `wasm-tools validate`
<!-- MANUAL ADDITIONS END -->
