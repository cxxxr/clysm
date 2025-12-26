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
- Common Lisp (SBCL 2.4+) - test harness implementation + alexandria, rove (test framework), uiop (process execution), wasmtime (Wasm runtime), wasm-tools (validation) (020-ansi-test)
- N/A (in-memory test execution; optional JSON/Markdown for reports and baselines) (020-ansi-test)
- Common Lisp (SBCL 2.4+) - test harness modification + Existing 020-ansi-test (loader, runner, classifier), clysm/compiler (021-ansi-test-execution)
- N/A (in-memory test execution) (021-ansi-test-execution)
- Common Lisp (SBCL 2.4+) + alexandria, babel, rove (testing), clysm/ffi (existing FFI module) (022-wasm-import-optimization)
- Common Lisp (SBCL 2.4+) for compiler implementation + alexandria, babel, trivial-gray-streams, rove (testing) (024-equality-predicates)
- N/A (compile-time only) (024-equality-predicates)
- Common Lisp (SBCL 2.4+) for compiler; WasmGC for output + alexandria, babel, trivial-gray-streams, rove (testing); existing clysm/compiler, clysm/runtime modules (025-multiple-values)
- N/A (in-memory globals within Wasm module) (025-multiple-values)

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
- 025-multiple-values: Added Common Lisp (SBCL 2.4+) for compiler; WasmGC for output + alexandria, babel, trivial-gray-streams, rove (testing)
- 025-multiple-values: Added Common Lisp (SBCL 2.4+) for compiler; WasmGC for output + alexandria, babel, trivial-gray-streams, rove (testing); existing clysm/compiler, clysm/runtime modules
- 024-equality-predicates: Added Common Lisp (SBCL 2.4+) for compiler implementation + alexandria, babel, trivial-gray-streams, rove (testing)


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

## Feature 024: Equality Predicates and Logical Operators - COMPLETE

**Status**: All 103 tasks completed (2025-12-26)

### Implemented Components
- `src/clysm/compiler/codegen/func-section.lisp`: eq, eql, equal, equalp, not predicates
- `src/clysm/compiler/compiler.lisp`: i64.eq, i64.ne, f64.convert_i32_s instructions

### Key Features
1. **eq predicate**: Pointer identity using Wasm `ref.eq`
   - Handles null (NIL) specially with `ref.is_null`
   - `(eq 'a 'a)` → T, `(eq (cons 1 2) (cons 1 2))` → NIL
2. **eql predicate**: Type-aware value equality
   - i31ref comparison for fixnums/characters
   - f64.eq for floats, ref.eq for ratio numerator/denominator
   - `(eql 3.14 3.14)` → T, `(eql 1 1.0)` → NIL
3. **equal predicate**: Structural equality with worklist-based recursion
   - Byte-by-byte string comparison
   - Recursive cons cell comparison without Wasm-level recursion
   - `(equal '(1 2 3) '(1 2 3))` → T
4. **equalp predicate**: Case-insensitive structural equality
   - Case-insensitive string/character comparison
   - Numeric type coercion (fixnum ↔ float)
   - `(equalp "Hello" "HELLO")` → T, `(equalp 3 3.0)` → T
5. **not predicate**: Logical negation
   - `(not nil)` → T, `(not t)` → NIL
6. **and/or special forms**: Already implemented in ast.lisp as nested if expansion

### Technical Details
- Worklist-based algorithm for recursive cons comparison (avoids Wasm recursion limits)
- `ref.test` and `ref.cast` for runtime type dispatch
- Boolean result wrapping: `(:i32.const 1) :ref.i31` for T, `(:ref.null :none)` for NIL
- Added i64.eq/i64.ne opcodes for ratio comparison
- Added f64.convert_i32_s for numeric coercion in equalp

### Test Coverage
- Unit tests: equality-predicates-test.lisp (eq, eql, equal, equalp, not)
- Unit tests: logical-operators-test.lisp (and, or)
- Contract tests: equality-wasm-test.lisp (Wasm validation)
- Integration tests: equality-ansi-test.lisp (ANSI CL compliance)

## Feature 025: Multiple Values Support - COMPLETE

**Status**: All 82 tasks completed (2025-12-26)

### Implemented Components
- `src/clysm/compiler/ast.lisp`: AST structs and parsers for mv forms
- `src/clysm/compiler/codegen/func-section.lisp`: Compilation for all mv forms
- `src/clysm/compiler/compiler.lisp`: array.get instruction for mv-buffer access
- `src/clysm/package.lisp`: Exported AST symbols
- `src/clysm/runtime/multi-value.lisp`: mv-count and mv-buffer globals

### Key Features
1. **values form**: Return multiple values from a form
   - `(values)` → NIL with mv-count=0
   - `(values 1 2 3)` → 1 (primary), with 2,3 in mv-buffer
2. **multiple-value-bind**: Bind variables to multiple values
   - `(multiple-value-bind (a b c) (values 1 2) c)` → NIL (fewer values)
   - Extra values are ignored
3. **multiple-value-list**: Collect all values as a list
   - `(multiple-value-list (values 1 2 3))` → (1 2 3)
4. **nth-value**: Access specific value by index
   - `(nth-value 1 (values 10 20 30))` → 20
   - Out-of-range returns NIL
5. **values-list**: Spread list as multiple values
   - `(values-list '(1 2 3))` → (values 1 2 3)
6. **multiple-value-prog1**: Preserve first form's values
   - `(multiple-value-prog1 (values 1 2) 999)` → 1
7. **multiple-value-call**: Pass all values to function
   - `(multiple-value-call #'+ (values 1 2 3))` → 6

### Technical Details
- Global indices: 0=NIL, 1=UNBOUND, 2=mv-count, 3=mv-buffer (array of anyref)
- Type section: 22=$mv_array (array of anyref)
- Primary value returned on Wasm stack; secondary values in mv-buffer global
- Void block/loop pattern `(:block)` `(:loop)` for iteration without result type
- Local variable types: `:i32` for counters/indices, `:anyref` for values
- Required `(:ref.cast :i31)` before `:i31.get_s` when casting from anyref

### Test Coverage
- Unit tests: multiple-values-test.lisp (26 tests for all user stories)
- Tests validated: values, mvb, mvl, nth-value, values-list, mvp1, mvc
<!-- MANUAL ADDITIONS END -->
