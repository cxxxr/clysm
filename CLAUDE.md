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
- N/A (in-memory class/instance registry) (026-clos-foundation)
- Common Lisp (SBCL 2.4+) + alexandria, babel, trivial-gray-streams, rove (testing), existing clysm/ffi, clysm/compiler modules (027-complete-ffi)
- N/A (in-memory compile-time registries) (027-complete-ffi)
- Common Lisp (SBCL 2.4+) for compiler; WasmGC for output + clysm/compiler, clysm/lib/macros, clysm/clos (existing modules) (028-setf-generalized-refs)
- N/A (compile-time registry for setf expanders) (028-setf-generalized-refs)
- Common Lisp (SBCL 2.4+) for compiler; WasmGC for output + clysm/compiler, clysm/lib/macros (existing tagbody/go infrastructure) (029-loop-macro)
- N/A (compile-time macro expansion only) (029-loop-macro)

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
- 029-loop-macro: Added Common Lisp (SBCL 2.4+) for compiler; WasmGC for output + clysm/compiler, clysm/lib/macros (existing tagbody/go infrastructure)
- 028-setf-generalized-refs: Added Common Lisp (SBCL 2.4+) for compiler; WasmGC for output + clysm/compiler, clysm/lib/macros, clysm/clos (existing modules)
- 027-complete-ffi: Added Common Lisp (SBCL 2.4+) + alexandria, babel, trivial-gray-streams, rove (testing), existing clysm/ffi, clysm/compiler modules


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

## Feature 026: CLOS Foundation - COMPLETE

**Status**: All 8 phases completed (2025-12-27)

### Implemented Components
- `src/clysm/compiler/ast.lisp`: AST structs for defclass, make-instance, defmethod
- `src/clysm/compiler/codegen/func-section.lisp`: Compile-time class and GF registries
- `src/clysm/compiler/codegen/gc-types.lisp`: WasmGC CLOS type definitions
- `src/clysm/package.lisp`: Exported CLOS symbols

### Key Features
1. **defclass**: Define CLOS classes with slot options
   - `:initarg` - keyword argument for initialization
   - `:accessor` - reader/writer function name
   - `:initform` - default value expression
   - Single inheritance supported (multiple inheritance rejected)
2. **make-instance**: Create class instances
   - `(make-instance 'point :x 3 :y 4)` creates point instance
   - Initargs matched to slot indices at compile time
3. **Slot accessors**: Reader/writer generation
   - Accessors recorded in compile-time slot-info
   - Slot indices computed for array access
4. **defmethod**: Define methods with type specialization
   - `(defmethod speak ((a animal)) "sound")` specializes on class
   - Qualifiers: :before, :after, :around for method combination
5. **Generic function dispatch**: Method selection by argument class
   - compute-applicable-methods finds matching methods
   - sort-methods orders by specificity
6. **Single inheritance**: Child classes inherit parent slots
   - Slots prepended from parent, indices recomputed
   - Child slots shadow parent slots with same name

### WasmGC Types
- Type index 6: `$instance` struct (class ref, slot-vector ref)
- Type index 7: `$standard-class` struct (name, superclass, slot_count, etc.)
- Type index 21: `$slot-vector` array (mutable anyref elements)
- Type index 22: `$keyword-array` array (symbol refs for initargs)
- Type index 23: `$closure-array` array (nullable closure refs for initforms)

### Compile-Time Registries
- `*class-registry*`: Maps class names to class-info structs
- `*generic-function-registry*`: Maps GF names to gf-info structs
- `class-info`: name, superclass, slots, class-id, finalized-p
- `slot-info`: name, initarg, accessor, initform, initform-p, index
- `gf-info`: name, methods (list of method-info), lambda-list
- `method-info`: specializers, qualifier, lambda-list, body

### Test Coverage
- Unit tests: tests/unit/clos/*.lisp (defclass-test, make-instance-test, accessor-test, defmethod-test)
- Contract tests: tests/contract/clos-wasm-test.lisp (WasmGC type validation)
- Integration tests: tests/integration/clos-test.lisp (end-to-end CLOS usage)

## Feature 027: Complete FFI Foundation - COMPLETE

**Status**: All 66 tasks completed (2025-12-27)

### Implemented Components
- `src/clysm/compiler/ast.lisp`: ast-ffi-call, ast-call-host AST nodes
- `src/clysm/compiler/codegen/func-section.lisp`: compile-ffi-call, compile-call-host
- `src/clysm/ffi/marshalling.lisp`: Lisp↔Wasm type marshalling
- `src/clysm/ffi/import-gen.lisp`: Wasm import section generation
- `src/clysm/ffi/export-gen.lisp`: Wasm export section generation
- `src/clysm/runtime/ffi-dispatch.lisp`: Dynamic call-host infrastructure
- `host-shim/ffi-test-host.js`: JavaScript test host with mock functions

### Key Features
1. **define-foreign-function**: Declare host function imports
   - `(ffi:define-foreign-function console-log "host.log" (:string) :void)`
   - Marshal types: :fixnum, :float, :string, :boolean, :anyref, :void
2. **export-function**: Export Lisp functions to host
   - `(ffi:export-function my-add "add" (:fixnum :fixnum) :fixnum)`
   - Auto-generated wrapper with marshalling
3. **ffi:call-host**: Dynamic host function invocation
   - `(ffi:call-host "host.random")` calls by string name
   - Arguments packed into externref array
4. **Error handling**: ffi-host-error and ffi-type-error conditions
   - try_table/catch_all for host exceptions
   - Proper condition hierarchy (subtypes of ERROR)
5. **Callback support**: Re-entrant host→Lisp calls
   - Export wrappers are re-entrant safe
   - Special variable bindings preserved
   - Condition handlers work across boundaries

### Type Marshalling
| Lisp Type | Wasm Type | Host Type |
|-----------|-----------|-----------|
| :fixnum | i31ref | i32 |
| :float | (ref $float) | f64 |
| :string | (ref $string) | externref |
| :boolean | anyref | i32 (0/1) |
| :anyref | anyref | externref |
| :void | - | - |

### Technical Details
- WasmGC-First: No linear memory, all types as GC references
- Type indices: FFI imports start at 23+ (after built-in types)
- i31ref range: -2^30 to 2^30-1 (31-bit signed)
- Nil handling: null externref for strings, i32 0 for booleans

### Test Coverage
- Unit tests: tests/unit/ffi/*.lisp (define-foreign-function, marshal, call-host, callback)
- Contract tests: tests/contract/ffi-*.lisp (Wasm validation)
- Integration tests: tests/integration/ffi-*.lisp (end-to-end FFI)

## Feature 028: Setf Macros and Generalized References - COMPLETE

**Status**: All 8 phases completed (2025-12-27)

### Implemented Components
- `src/clysm/lib/setf-expanders.lisp`: Setf expander registry and standard expanders
- `src/clysm/lib/macros.lisp`: Setf macro expanders (setf, psetf, incf, decf, push, pop, pushnew, rotatef, shiftf)
- `src/clysm/clos/slot-access.lisp`: CLOS slot accessor setf expander generation

### Key Features
1. **setf macro**: Modify place values with proper setf expansion protocol
   - `(setf x 10)` → assigns 10 to x
   - `(setf (car list) 1)` → modifies car of list
   - Multiple pairs supported: `(setf a 1 b 2 c 3)`
2. **psetf macro**: Parallel assignment (values evaluated before any assignment)
   - `(psetf a b b a)` → swaps a and b atomically
3. **incf/decf macros**: In-place numeric modification
   - `(incf x)` → increment x by 1
   - `(decf x 5)` → decrement x by 5
4. **push/pop/pushnew macros**: Stack-like list operations
   - `(push item list)` → prepend item to list
   - `(pop list)` → remove and return first element
   - `(pushnew item list :test #'equal)` → push if not member
5. **rotatef/shiftf macros**: Multi-place value exchange
   - `(rotatef a b c)` → rotate values: a←b, b←c, c←a
   - `(shiftf a b c new)` → shift values: return a, a←b, b←c, c←new
6. **define-setf-expander*/defsetf***: User-defined setf expanders
   - Full five-value expansion protocol support
   - Short form: `(defsetf* accessor setter)`
   - Long form: `(defsetf* accessor (args) (store) body)`

**Note**: Clysm uses `*` suffix (e.g., `get-setf-expansion*`, `define-setf-expander*`, `defsetf*`) to avoid conflicts with CL package symbols.

### Standard Setf Expanders
- **Cons accessors**: car, cdr, first, rest, second through tenth, nth
- **Array access**: aref
- **Hash tables**: gethash
- **Symbol accessors**: symbol-value, symbol-function, symbol-plist
- **CLOS slot accessors**: Auto-generated for :accessor slot options

### Five-Value Expansion Protocol
Each setf expander returns five values:
1. `temps` - temporary variable symbols
2. `vals` - forms to evaluate for temps
3. `stores` - store variable symbols (usually 1)
4. `store-form` - form that stores the value
5. `access-form` - form that reads current value

### Error Conditions
- `undefined-setf-expander`: No expander for accessor
- `invalid-place`: Invalid place form
- `constant-modification-error`: Attempting to modify nil, t, or keywords
- `odd-argument-count`: setf/psetf with odd number of arguments

### Test Coverage
- Unit tests: tests/unit/setf-test.lisp, tests/unit/setf-expander-test.lisp
- Contract tests: tests/contract/setf-wasm-test.lisp (Wasm validation)
- Integration tests: tests/integration/setf-ansi-test.lisp (ANSI compliance)
<!-- MANUAL ADDITIONS END -->
