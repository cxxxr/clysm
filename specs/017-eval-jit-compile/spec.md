# Feature Specification: Eval/JIT Compile System

**Feature Branch**: `017-eval-jit-compile`
**Created**: 2025-12-24
**Status**: Draft
**Input**: User description: "Phase 6 Eval/JIT完成: compile関数とTier切り替えを実装する。目標は(compile nil '(lambda ...))がコンパイル済み関数を返し、Tier 1インタプリタとTier 2 JITの動的切り替えが正常動作すること。既存のeval/interpreter.lisp(Tier 1)、eval/jit.lisp(Tier 2基盤)、compile-to-wasm関数を活用し、ホットスポット検出による自動Tier昇格と動的モジュールリンクを実装する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Anonymous Lambda (Priority: P1)

A language developer needs to compile anonymous lambda expressions at runtime to obtain callable compiled functions, enabling dynamic code generation and optimization scenarios.

**Why this priority**: This is the fundamental compile operation that all other features depend on. Without the ability to compile lambdas, no higher-level compilation features can work.

**Independent Test**: Can be fully tested by calling `(compile nil '(lambda (x) (+ x 1)))` and verifying the returned function accepts arguments and returns correct results.

**Acceptance Scenarios**:

1. **Given** a valid lambda expression `'(lambda (x) (+ x 1))`, **When** `(compile nil <lambda>)` is called, **Then** a callable function is returned that correctly computes `(+ x 1)` for any input x.
2. **Given** a lambda with multiple parameters `'(lambda (a b) (* a b))`, **When** compiled and invoked with arguments, **Then** returns the product of the two arguments.
3. **Given** a lambda with no parameters `'(lambda () 42)`, **When** compiled and called, **Then** returns 42.
4. **Given** a lambda with complex body `'(lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))`, **When** compiled, **Then** returns a function that correctly executes all special forms.

---

### User Story 2 - Tier 1 Interpreted Execution (Priority: P1)

A developer needs fast startup execution of code through the Tier 1 interpreter, accepting lower peak performance in exchange for zero compilation overhead.

**Why this priority**: Tier 1 execution is essential for immediate code execution without JIT overhead. This is the default execution mode for all newly compiled code.

**Independent Test**: Can be fully tested by executing code and verifying it runs correctly through the interpreter without triggering JIT compilation.

**Acceptance Scenarios**:

1. **Given** a function executed fewer than threshold times, **When** invoked, **Then** executes through Tier 1 interpreter.
2. **Given** a newly compiled function, **When** first invoked, **Then** runs in Tier 1 with minimal latency.
3. **Given** Tier 1 execution, **When** any supported special form is used, **Then** executes correctly (if, let, let*, lambda, progn, setq, block, return-from, tagbody, go, flet, labels).

---

### User Story 3 - Automatic Tier Promotion via Hot Spot Detection (Priority: P2)

The runtime automatically identifies frequently-executed functions (hot spots) and promotes them to Tier 2 JIT compilation for improved performance, without requiring explicit user action.

**Why this priority**: This enables transparent performance optimization. Users get improved performance for hot code paths automatically.

**Independent Test**: Can be tested by executing a function beyond the threshold count and verifying it transitions to Tier 2 execution.

**Acceptance Scenarios**:

1. **Given** a function invoked more than threshold times (default 10), **When** threshold is exceeded, **Then** function is automatically promoted to Tier 2.
2. **Given** hot spot promotion, **When** the same function is called again, **Then** executes through the JIT-compiled Wasm code.
3. **Given** multiple functions, **When** each exceeds threshold independently, **Then** each is promoted individually without affecting others.
4. **Given** a configurable threshold, **When** threshold is modified, **Then** new threshold applies to subsequent invocations.

---

### User Story 4 - Compile Named Functions (Priority: P2)

A developer needs to compile and register named functions that can be referenced globally, enabling module-level function definitions and symbol-based function lookup.

**Why this priority**: Named function compilation is required for defun-like semantics and global function registration.

**Independent Test**: Can be tested by compiling a named function and calling it by name from another context.

**Acceptance Scenarios**:

1. **Given** a function name and lambda `(compile 'my-fn '(lambda (x) x))`, **When** compiled, **Then** function is registered in the function slot for symbol 'my-fn.
2. **Given** a named compiled function, **When** called via `(funcall 'my-fn arg)`, **Then** executes correctly.
3. **Given** recompilation of same name, **When** `(compile 'my-fn '(lambda (x) (+ x 1)))` is called, **Then** symbol's function slot is updated (hot-patched).

---

### User Story 5 - Dynamic Module Linking (Priority: P3)

JIT-compiled Wasm modules can link to runtime-provided imports (standard library functions, runtime services) at instantiation time.

**Why this priority**: Module linking enables JIT-compiled code to call into the runtime for operations that cannot be inlined.

**Independent Test**: Can be tested by JIT-compiling a function that uses runtime imports (like cons, car, cdr) and verifying correct operation.

**Acceptance Scenarios**:

1. **Given** JIT-compiled code calling a runtime import, **When** executed, **Then** correctly invokes the registered import function.
2. **Given** multiple Wasm modules, **When** each instantiated, **Then** each receives proper import bindings.
3. **Given** new runtime imports registered, **When** new modules are instantiated, **Then** can access newly registered imports.

---

### Edge Cases

- What happens when compiling invalid syntax (e.g., `(compile nil '(not-a-lambda))`)?
- How does the system handle compilation of closures that capture variables from an outer scope?
- What happens when Tier 2 compilation fails (e.g., unsupported construct)?
- How are recursive functions handled during tier promotion?
- What happens when a function is hot-patched while it's currently executing?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement `(compile name definition)` where name is nil or a symbol, and definition is a lambda expression, returning a callable function.
- **FR-002**: System MUST execute all newly compiled functions through Tier 1 interpreter by default.
- **FR-003**: System MUST track invocation counts per function for hot spot detection.
- **FR-004**: System MUST automatically promote functions to Tier 2 when invocation count exceeds configurable threshold.
- **FR-005**: System MUST provide a way to configure the hot spot threshold (default: 10 invocations).
- **FR-006**: System MUST hot-patch symbol function slots when promoting to Tier 2, ensuring subsequent calls use JIT-compiled code.
- **FR-007**: System MUST support all special forms in Tier 1: quote, if, progn, let, let*, lambda, setq, block, return-from, tagbody, go, flet, labels, funcall.
- **FR-008**: System MUST generate valid Wasm binaries for Tier 2 compilation using existing compile-to-wasm infrastructure.
- **FR-009**: System MUST validate Wasm binaries before instantiation.
- **FR-010**: System MUST link runtime imports to JIT-compiled modules at instantiation time.
- **FR-011**: System MUST share GC heap between host and JIT-compiled code.
- **FR-012**: System MUST signal an error when attempting to compile non-lambda expressions.
- **FR-013**: System MUST handle closures by capturing the appropriate environment at compile time.
- **FR-014**: System MUST support explicit tier selection via `compile-with-tier`.

### Key Entities

- **Compiled Function**: A callable object produced by compile, initially backed by interpreter, potentially promoted to Wasm.
- **Invocation Counter**: Tracks call counts per function for hot spot detection.
- **Wasm Instance**: An instantiated Wasm module with exports and bound imports.
- **Function Slot**: A mutable binding from symbol to function, enabling hot-patching.
- **Runtime Import**: A host function exposed to JIT-compiled Wasm modules.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `(compile nil '(lambda (x) (+ x 1)))` returns a callable function that computes `(+ 5 1)` as 6.
- **SC-002**: A function invoked 11+ times executes through Tier 2 on subsequent calls.
- **SC-003**: Tier 1 interpreted execution supports all listed special forms without error.
- **SC-004**: Named function compilation registers the function and allows invocation by symbol.
- **SC-005**: Hot-patching a function while in use does not cause runtime errors.
- **SC-006**: 100% of valid lambda expressions compile successfully.
- **SC-007**: Invalid compile inputs (non-lambdas) produce clear error messages.
- **SC-008**: JIT-compiled functions produce identical results to Tier 1 interpreted functions for equivalent inputs.

## Assumptions

- Existing `compile-to-wasm` function generates valid Wasm binaries for supported constructs.
- The Tier 1 interpreter (eval/interpreter.lisp) is fully functional for all listed special forms.
- Wasm validation and instantiation infrastructure (eval/jit.lisp) is operational.
- The runtime is single-threaded, simplifying hot-patching semantics.
- Functions not supported by Tier 2 compilation remain in Tier 1 indefinitely.
