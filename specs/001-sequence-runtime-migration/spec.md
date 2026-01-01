# Feature Specification: Sequence Runtime Migration

**Feature Branch**: `001-sequence-runtime-migration`
**Created**: 2026-01-01
**Status**: Draft
**Input**: User description: "Build a runtime library migration system for Clysm (WebAssembly GC Lisp compiler) that moves sequence operations from inline Wasm codegen to Lisp-implemented runtime functions."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compiler User Compiles Code with Sequence Functions (Priority: P1)

A developer writes Common Lisp code that uses sequence operations like `remove`, `count`, `substitute`, or `delete`. When compiling this code with Clysm, the compiler generates calls to runtime library functions instead of inline Wasm bytecode. The compiled code behaves identically to before migration, with full ANSI CL compatibility.

**Why this priority**: This is the core functionality - ensuring compiled code works correctly with the migrated runtime functions. Without this, nothing else matters.

**Independent Test**: Can be tested by compiling a Lisp form using any migrated function and verifying correct output matches SBCL's behavior.

**Acceptance Scenarios**:

1. **Given** a Lisp form `(remove 3 '(1 2 3 4 3 5))`, **When** compiled and executed, **Then** the result is `(1 2 4 5)`
2. **Given** a Lisp form `(count 'a '(a b a c a))`, **When** compiled and executed, **Then** the result is `3`
3. **Given** a Lisp form `(substitute 'x 'a '(a b a c))`, **When** compiled and executed, **Then** the result is `(x b x c)`
4. **Given** a Lisp form `(delete nil '(1 nil 2 nil 3))`, **When** compiled and executed, **Then** the original list is modified and `(1 2 3)` is returned
5. **Given** a Lisp form `(remove-if-not #'numberp '(1 a 2 b 3))`, **When** compiled and executed, **Then** the result is `(1 2 3)`

---

### User Story 2 - Compiler User Uses ANSI Keyword Arguments (Priority: P1)

A developer uses advanced sequence operations with `:key`, `:test`, `:start`, and `:end` keyword arguments per ANSI CL specification. The runtime functions correctly process these arguments to filter, count, or transform specific portions of sequences.

**Why this priority**: ANSI compatibility is a core requirement. Many real-world programs depend on these keyword arguments.

**Independent Test**: Can be tested by compiling forms with various keyword argument combinations and comparing results to SBCL.

**Acceptance Scenarios**:

1. **Given** `(remove 3 '((1 . a) (2 . b) (3 . c)) :key #'car)`, **When** compiled and executed, **Then** the result is `((1 . a) (2 . b))`
2. **Given** `(count "foo" '("FOO" "bar" "Foo") :test #'string-equal)`, **When** compiled and executed, **Then** the result is `2`
3. **Given** `(substitute 0 nil '(1 nil 2 nil 3) :start 1 :end 3)`, **When** compiled and executed, **Then** the result is `(1 0 2 nil 3)`
4. **Given** `(remove-if #'evenp '(1 2 3 4 5 6) :start 2)`, **When** compiled and executed, **Then** the result is `(1 2 3 5)`

---

### User Story 3 - Compiler Developer Reduces Codebase Size (Priority: P2)

A compiler maintainer migrates sequence operations from inline Wasm codegen in `func-section.lisp` to Lisp-implemented runtime functions. This reduces the main codegen file size, improving maintainability and reducing complexity. The 12 targeted functions represent ~640 lines of inline codegen (~3.5% reduction); additional migrations can be pursued in follow-up features.

**Why this priority**: While not user-facing, this is a key success metric that improves long-term maintainability. Secondary to correctness.

**Independent Test**: Can be verified by measuring line count of `func-section.lisp` before and after migration.

**Acceptance Scenarios**:

1. **Given** the current `func-section.lisp` with ~18,300 lines, **When** all 12 targeted functions are migrated, **Then** the file is reduced by at least 600 lines (~3.5% reduction)
2. **Given** migrated runtime functions, **When** reviewing code, **Then** each function is implemented using only Layer 1 primitives (car, cdr, cons, type predicates)

---

### User Story 4 - Compiler Developer Improves Compilation Rate (Priority: P2)

The Stage 1 compilation rate increases because runtime functions are simpler to compile than inline Wasm generation code. This brings the project closer to the self-hosting goal.

**Why this priority**: Supports the broader bootstrap goal but is an indirect benefit of migration.

**Independent Test**: Can be measured by running Stage 1 generation and checking the compilation rate in the report.

**Acceptance Scenarios**:

1. **Given** current Stage 1 compilation rate of ~14%, **When** all migrations are complete, **Then** the compilation rate increases to 35% or higher
2. **Given** the runtime library, **When** compiling runtime functions themselves, **Then** they compile successfully (they use only primitives)

---

### Edge Cases

**Note**: This feature implements lists-only support. String and vector sequence support is deferred to a follow-up feature.

- What happens when `:start` is greater than list length? System signals an error per ANSI CL specification.
- What happens when `:end` is less than `:start`? System signals an error per ANSI CL specification.
- What happens with empty lists? Operations return nil.
- How does system handle circular lists? Functions process elements until stack limits or detect cycles per implementation-defined behavior.
- What happens when `:key` function signals an error? Error propagates to caller as expected.

## Requirements *(mandatory)*

### Functional Requirements

#### Core Migration Infrastructure

- **FR-001**: System MUST provide a `*runtime-function-table*` dispatch mechanism that maps function names to runtime implementations
- **FR-002**: System MUST support `compile-runtime-call` to generate calls to runtime library functions
- **FR-003**: Runtime functions MUST be implemented using only Layer 1 primitives: `car`, `cdr`, `cons`, `null`, `listp`, `atom`, `consp`, `eq`, `eql`, `funcall`

#### Remove Family ([remove](resources/HyperSpec/Body/f_rm_rm.htm))

- **FR-010**: System MUST implement [remove](resources/HyperSpec/Body/f_rm_rm.htm) that returns a copy of sequence without elements matching item
- **FR-011**: System MUST implement [remove-if](resources/HyperSpec/Body/f_rm_rm.htm) that returns a copy without elements satisfying predicate
- **FR-012**: System MUST implement [remove-if-not](resources/HyperSpec/Body/f_rm_rm.htm) that returns a copy keeping only elements satisfying predicate
- **FR-013**: All remove functions MUST support `:key`, `:test`, `:start`, `:end`, `:count`, `:from-end` keyword arguments

#### Count Family ([count](resources/HyperSpec/Body/f_countc.htm))

- **FR-020**: System MUST implement [count](resources/HyperSpec/Body/f_countc.htm) that returns the number of elements matching item
- **FR-021**: System MUST implement [count-if](resources/HyperSpec/Body/f_countc.htm) that returns count of elements satisfying predicate
- **FR-022**: System MUST implement [count-if-not](resources/HyperSpec/Body/f_countc.htm) that returns count of elements not satisfying predicate
- **FR-023**: All count functions MUST support `:key`, `:test`, `:start`, `:end`, `:from-end` keyword arguments

#### Substitute Family ([substitute](resources/HyperSpec/Body/f_sbs_s.htm))

- **FR-030**: System MUST implement [substitute](resources/HyperSpec/Body/f_sbs_s.htm) that returns sequence with matching elements replaced
- **FR-031**: System MUST implement [substitute-if](resources/HyperSpec/Body/f_sbs_s.htm) that replaces elements satisfying predicate
- **FR-032**: System MUST implement [substitute-if-not](resources/HyperSpec/Body/f_sbs_s.htm) that replaces elements not satisfying predicate
- **FR-033**: All substitute functions MUST support `:key`, `:test`, `:start`, `:end`, `:count`, `:from-end` keyword arguments

#### Delete Family ([delete](resources/HyperSpec/Body/f_rm_rm.htm) - Destructive Variants)

- **FR-040**: System MUST implement [delete](resources/HyperSpec/Body/f_rm_rm.htm) as destructive version of `remove`
- **FR-041**: System MUST implement [delete-if](resources/HyperSpec/Body/f_rm_rm.htm) as destructive version of `remove-if`
- **FR-042**: System MUST implement [delete-if-not](resources/HyperSpec/Body/f_rm_rm.htm) as destructive version of `remove-if-not`
- **FR-043**: All delete functions MUST support same keyword arguments as remove family
- **FR-044**: Delete functions MAY modify the original list structure for efficiency

#### Backward Compatibility

- **FR-050**: Migrated functions MUST produce identical results to previous inline Wasm implementations
- **FR-051**: Existing compiled code MUST continue to work without recompilation
- **FR-052**: Function signatures MUST remain unchanged at the call site

#### Integration

- **FR-060**: Runtime library functions MUST be registered in `*runtime-function-table*`
- **FR-061**: Compiler MUST generate runtime calls for all migrated functions
- **FR-062**: Runtime functions MUST be loadable via the existing ASDF system definition

### Key Entities

- **Runtime Function Table**: A dispatch table mapping function symbols to their Lisp implementations
- **Layer 1 Primitives**: The minimal set of operations that compile directly to Wasm (car, cdr, cons, predicates)
- **Sequence**: Lists, strings, and vectors that are inputs to sequence operations
- **Keyword Arguments**: Optional parameters (:key, :test, :start, :end, :count, :from-end) controlling function behavior

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `func-section.lisp` line count is reduced by at least 600 lines (~3.5% reduction for 12 functions)
- **SC-002**: All 12 migrated functions (remove/count/substitute/delete families) pass ANSI CL compatibility tests
- **SC-003**: Stage 1 compilation rate increases from current ~14% to 35% or higher
- **SC-004**: All existing sequence operation tests continue to pass with identical results
- **SC-005**: Runtime library code compiles successfully via Stage 1 (uses only primitives)
- **SC-006**: No performance regression greater than 10% for sequence operations on typical workloads

## Assumptions

- The existing `*runtime-function-table*` mechanism from `001-io-list-runtime` is functional and can be extended
- Layer 1 primitives (car, cdr, cons, type predicates) are fully working in the Wasm runtime
- Existing test infrastructure can validate ANSI CL compatibility
- The compiler can already generate function calls to runtime library entries
- String and vector sequence support can follow list implementation patterns
