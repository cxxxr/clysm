# Feature Specification: Numeric Accessors and Float Special Values

**Feature Branch**: `019-numeric-accessors`
**Created**: 2025-12-24
**Status**: Draft
**Input**: User description: "ANSI CL数値アクセサとFloat特殊値対応を実装する。目標は(1) numerator/denominator関数によるRatioコンポーネントアクセス、(2) 浮動小数点特殊値(NaN, +Infinity, -Infinity)の正しい生成と比較、(3) double-float精度の保持。既存の010-numeric-towerを拡張し、テスト失敗を解消する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Ratio Component Access (Priority: P1)

A Lisp programmer needs to extract the numerator and denominator from a ratio value using the standard ANSI Common Lisp accessor functions. This is essential for implementing custom arithmetic algorithms and inspecting rational number structure.

**Why this priority**: The `numerator` and `denominator` functions are fundamental ANSI CL accessors. Existing tests in `tests/integration/ratio-test.lisp` (lines 116-124) require these functions to work correctly. This is a basic accessor pattern used throughout Lisp numeric code.

**Independent Test**: Can be fully tested by compiling `(numerator 1/3)` → 1 and `(denominator 1/3)` → 3, verifying correct extraction from the Ratio WasmGC struct.

**Acceptance Scenarios**:

1. **Given** a ratio 1/3, **When** `(numerator 1/3)` is called, **Then** the result is the integer 1
2. **Given** a ratio 1/3, **When** `(denominator 1/3)` is called, **Then** the result is the integer 3
3. **Given** an integer 5, **When** `(numerator 5)` is called, **Then** the result is 5 (integer is its own numerator)
4. **Given** an integer 5, **When** `(denominator 5)` is called, **Then** the result is 1 (integer has denominator 1)
5. **Given** a negative ratio -3/4, **When** `(numerator -3/4)` is called, **Then** the result is -3 (sign on numerator per CL spec)
6. **Given** a reduced ratio from (/ 6 9), **When** `(numerator (/ 6 9))` is called, **Then** the result is 2 (from reduced 2/3)

---

### User Story 2 - Float Special Value Generation (Priority: P1)

A Lisp programmer writes code that intentionally produces IEEE 754 special values (positive infinity, negative infinity, NaN) through standard arithmetic operations. These values must be correctly generated and distinguishable.

**Why this priority**: IEEE 754 compliance is critical for numerical computing. Existing tests in `tests/integration/float-test.lisp` (lines 77-95) test these special values. Scientific and financial applications depend on correct infinity/NaN handling.

**Independent Test**: Can be fully tested by compiling `(/ 1.0 0.0)` → +Infinity, `(/ -1.0 0.0)` → -Infinity, and arithmetic producing NaN.

**Acceptance Scenarios**:

1. **Given** `(/ 1.0 0.0)`, **When** compiled and executed, **Then** the result is positive infinity (+Inf)
2. **Given** `(/ -1.0 0.0)`, **When** compiled and executed, **Then** the result is negative infinity (-Inf)
3. **Given** `(- (/ 1.0 0.0) (/ 1.0 0.0))`, **When** compiled and executed, **Then** the result is NaN (Infinity - Infinity)
4. **Given** `(sqrt -1.0)`, **When** compiled and executed for real numbers, **Then** the result is NaN (no complex coercion)
5. **Given** `(* 0.0 (/ 1.0 0.0))`, **When** compiled and executed, **Then** the result is NaN (0 × Infinity)

---

### User Story 3 - Float Special Value Comparison (Priority: P1)

A Lisp programmer needs to correctly compare float special values. NaN comparisons must follow IEEE 754 semantics where NaN is not equal to anything, including itself.

**Why this priority**: Incorrect NaN comparison is a common source of subtle bugs. Test at line 92-95 of float-test.lisp specifically tests `(= nan nan)` returning NIL. This is essential for robust numerical code.

**Independent Test**: Can be fully tested by verifying `(let ((nan (- (/ 1.0 0.0) (/ 1.0 0.0)))) (= nan nan))` returns NIL.

**Acceptance Scenarios**:

1. **Given** two NaN values, **When** compared with `=`, **Then** the result is NIL (NaN ≠ NaN)
2. **Given** a NaN value, **When** compared with `<`, `>`, `<=`, `>=` to any value, **Then** the result is NIL
3. **Given** positive infinity values, **When** compared with `=`, **Then** the result is T (+Inf = +Inf)
4. **Given** positive and negative infinity, **When** compared with `=`, **Then** the result is NIL
5. **Given** positive infinity and any finite float, **When** compared with `>`, **Then** positive infinity is greater

---

### User Story 4 - Double-Float Precision Preservation (Priority: P2)

A Lisp programmer uses double-float literals (e.g., 1.0d0) for high-precision scientific computations. The system must preserve full 64-bit IEEE 754 double precision throughout compilation and execution.

**Why this priority**: Precision loss causes silent computational errors. Tests at lines 104-114 of float-test.lisp verify double precision. Scientific computing requires consistent precision behavior.

**Independent Test**: Can be fully tested by compiling `(/ 1.0d0 3.0d0)` and verifying the result matches 0.3333333333333333d0 within 1.0d-15 tolerance.

**Acceptance Scenarios**:

1. **Given** `1.0d0`, **When** compiled, **Then** it is stored as 64-bit IEEE 754 double
2. **Given** `(+ 1.0d0 0.0d0)`, **When** compiled and executed, **Then** result maintains double precision
3. **Given** `(/ 1.0d0 3.0d0)`, **When** compared to 0.3333333333333333d0, **Then** difference is < 1.0d-15
4. **Given** very small double `1.0d-100`, **When** multiplied by `1.0d-100`, **Then** result is valid (not underflowed to 0)
5. **Given** very large double `1.0d100`, **When** multiplied by `1.0d100`, **Then** result is valid (handled correctly)

---

### Edge Cases

- What happens when `numerator` or `denominator` is called on a float? (Should signal type error)
- How is -0.0 (negative zero) compared with 0.0? (Should be equal per IEEE 754)
- What happens when comparing infinity with bignum? (Infinity should be greater than any finite bignum)
- How does the system handle float-to-ratio conversion for special values? (Should signal error for NaN/Infinity)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement `numerator` function that returns the numerator of a ratio
- **FR-002**: System MUST implement `denominator` function that returns the denominator of a ratio
- **FR-003**: `numerator` and `denominator` MUST accept integers (returning the integer and 1 respectively)
- **FR-004**: `numerator` MUST return the reduced numerator (after GCD simplification)
- **FR-005**: Negative ratios MUST have the sign on the numerator, not the denominator
- **FR-006**: System MUST generate positive infinity (+Inf) from `(/ 1.0 0.0)`
- **FR-007**: System MUST generate negative infinity (-Inf) from `(/ -1.0 0.0)`
- **FR-008**: System MUST generate NaN from Infinity - Infinity and similar indeterminate operations
- **FR-009**: NaN comparison with `=` MUST return NIL, including self-comparison
- **FR-010**: NaN comparison with `<`, `>`, `<=`, `>=` MUST return NIL
- **FR-011**: Infinity values MUST compare correctly with `=` (equal to same-sign infinity only)
- **FR-012**: Infinity MUST be greater than any finite number in comparison
- **FR-013**: System MUST preserve 64-bit double precision for `d0` suffix literals
- **FR-014**: Arithmetic on double-floats MUST use 64-bit precision throughout
- **FR-015**: System MUST correctly handle negative zero (-0.0) per IEEE 754

### Key Entities

- **Ratio** (existing): WasmGC struct with NUMERATOR and DENOMINATOR fields (anyref type)
- **Float** (existing): WasmGC struct with VALUE field (f64 type)
- **Special Float Values**: +Infinity, -Infinity, NaN (IEEE 754 bit patterns in f64)
- **Numeric Tower Hierarchy**: Accessors work on rational numbers (ratio, integer)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All tests in `tests/integration/ratio-test.lisp` pass, including numerator/denominator tests
- **SC-002**: All tests in `tests/integration/float-test.lisp` pass, including special value tests
- **SC-003**: `(numerator 1/3)` returns 1 and `(denominator 1/3)` returns 3
- **SC-004**: `(= nan nan)` returns NIL for any NaN value
- **SC-005**: `(/ 1.0 0.0)` produces a value greater than any finite float
- **SC-006**: Double-float arithmetic maintains precision within 1.0d-15 relative error
- **SC-007**: All existing numeric tower tests (010-numeric-tower) continue to pass
- **SC-008**: Generated Wasm code executes successfully in wasmtime

## Assumptions

- The Ratio WasmGC struct already has NUMERATOR and DENOMINATOR fields of type anyref (verified in existing tests)
- The Float WasmGC struct uses f64 which natively supports IEEE 754 special values
- Wasmtime correctly implements IEEE 754 semantics for f64 operations
- The compiler already has infrastructure for calling Wasm struct field accessors
- Constant folding in the compiler should handle special float value propagation correctly

## Dependencies

- **010-numeric-tower**: This feature extends the numeric tower implementation (Ratio and Float types)
- **clysm/compiler/codegen/gc-types**: WasmGC type definitions for Ratio and Float
- **clysm/compiler/codegen/func-section**: Function code generation for numeric operations
