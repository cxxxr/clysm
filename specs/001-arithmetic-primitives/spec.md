# Feature Specification: Arithmetic Primitives 1- and 1+

**Feature Branch**: `001-arithmetic-primitives`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 13D-1b: 算術プリミティブ追加を実装する。目標はDEFUN本体のコンパイル成功率向上。func-section.lisp:722-839のプリミティブリストに 1- と 1+ を追加し、compile-1- と compile-1+ 関数を実装する。1- は (- x 1) と等価、1+ は (+ x 1) と等価。テストケース: (defun fact (n) (if (<= n 1) 1 (* n (fact (1- n))))) のコンパイルが成功すること。期待する効果: コンパイル率13.63%→30%への向上。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Recursive Functions Using 1- (Priority: P1)

As a Clysm compiler developer, I want the compiler to recognize and compile the `1-` primitive so that recursive functions like factorial can be compiled successfully.

**Why this priority**: The `1-` primitive is commonly used in recursive algorithms (factorial, Fibonacci, tree traversals) and is blocking a significant portion of DEFUN body compilations. This is the primary driver for improving compilation rate.

**Independent Test**: Can be fully tested by compiling `(defun fact (n) (if (<= n 1) 1 (* n (fact (1- n)))))` and verifying the generated Wasm is valid.

**Acceptance Scenarios**:

1. **Given** the Clysm compiler with `1-` support, **When** I compile `(1- 5)`, **Then** the compiler generates Wasm equivalent to `(- 5 1)` producing 4.
2. **Given** a factorial function definition using `1-`, **When** I compile the function, **Then** the compilation succeeds without errors.
3. **Given** compiled Wasm code for `(1- x)`, **When** validated with wasm-tools, **Then** validation passes.

---

### User Story 2 - Compile Incrementing Patterns Using 1+ (Priority: P1)

As a Clysm compiler developer, I want the compiler to recognize and compile the `1+` primitive so that loop increment patterns and iterative algorithms can be compiled successfully.

**Why this priority**: The `1+` primitive is commonly used in iterative loops (DOTIMES, DO, iteration counters) and complements `1-` for complete arithmetic increment/decrement support.

**Independent Test**: Can be fully tested by compiling `(defun next (n) (1+ n))` and verifying the generated Wasm is valid.

**Acceptance Scenarios**:

1. **Given** the Clysm compiler with `1+` support, **When** I compile `(1+ 5)`, **Then** the compiler generates Wasm equivalent to `(+ 5 1)` producing 6.
2. **Given** a function definition using `1+`, **When** I compile the function, **Then** the compilation succeeds without errors.
3. **Given** compiled Wasm code for `(1+ x)`, **When** validated with wasm-tools, **Then** validation passes.

---

### User Story 3 - Improved Compilation Success Rate (Priority: P2)

As a Clysm compiler developer, I want the DEFUN body compilation success rate to improve from 13.63% toward 30% so that more of the compiler's own code can be compiled to Wasm.

**Why this priority**: This is the measurable outcome that validates the feature's impact on the self-hosting bootstrap goal.

**Independent Test**: Can be verified by running Stage 1 generation and comparing the compilation statistics before and after.

**Acceptance Scenarios**:

1. **Given** the Clysm compiler with `1-` and `1+` support, **When** I run Stage 1 generation, **Then** the DEFUN body compilation success rate increases from the baseline.
2. **Given** the Stage 1 report, **When** I analyze the blocker statistics, **Then** `1-` and `1+` are no longer listed as compilation blockers.

---

### Edge Cases

- What happens when `1-` or `1+` is called with non-numeric arguments?
  - The compiler generates code that assumes numeric input; runtime type errors will occur for non-numeric inputs (consistent with existing arithmetic primitives).
- What happens when `1-` or `1+` is called with wrong arity (0 or 2+ arguments)?
  - The compiler should signal an error during compilation, as these are unary-only operators.
- How does the system handle `1-` and `1+` with floats vs integers?
  - Both integer and float operands are supported; the result type matches the input type (integer + 1 = integer, float + 1.0 = float).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST add `1-` and `1+` to the primitive operators list in `func-section.lisp` (lines 722-843).
- **FR-002**: System MUST implement `compile-1-` function that generates Wasm code equivalent to `(- arg 1)`.
- **FR-003**: System MUST implement `compile-1+` function that generates Wasm code equivalent to `(+ arg 1)`.
- **FR-004**: `compile-1-` and `compile-1+` MUST follow the existing primitive compilation pattern used by other arithmetic operators in `primitives.lisp`.
- **FR-005**: System MUST handle both integer and float operands correctly (integer → integer result, float → float result).
- **FR-006**: System MUST validate arity at compile time (exactly 1 argument required).
- **FR-007**: Generated Wasm code MUST pass `wasm-tools validate`.

### Key Entities

- **Primitive Operator**: A built-in function recognized by the compiler with specialized Wasm code generation (e.g., `+`, `-`, `*`, `/`).
- **Compile Function**: A function in `primitives.lisp` that takes arguments and environment, returning Wasm bytecode (e.g., `compile-+`, `compile--`).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: The factorial function `(defun fact (n) (if (<= n 1) 1 (* n (fact (1- n)))))` compiles successfully without errors.
- **SC-002**: All generated Wasm code using `1-` or `1+` passes `wasm-tools validate` with exit code 0.
- **SC-003**: DEFUN body compilation success rate increases from the 13.63% baseline (specific target improvement depends on how many blocked functions use these primitives).
- **SC-004**: `1-` and `1+` no longer appear in the Stage 1 compilation blocker report.

## Assumptions

- The existing arithmetic primitive compilation infrastructure (`compile-+`, `compile--`) provides a proven pattern to follow.
- The Wasm type system can handle the numeric type dispatch (i32 vs f64) using existing mechanisms.
- No changes to the Wasm type indices or global indices are required.
- The implementation follows the same structure as other unary primitives (e.g., `not`, `null`, `atom`).
