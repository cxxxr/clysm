# Feature Specification: IEEE 754 Bit Extraction

**Feature Branch**: `033-ieee754-bit-extraction`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 10A: IEEE 754ビット抽出を実装する。目標はSBCL固有関数(sb-kernel:single-float-bits, sb-kernel:double-float-bits, sb-int:with-float-traps-masked)の排除。FFI経由でホストJavaScript/wasmtimeのDataView APIを使用し、浮動小数点数とビットパターン間の変換を実装。特殊値(+Infinity, -Infinity, NaN, subnormal)を正しく処理すること。compiler/compiler.lisp:1118-1135を書き換え、ポータブルな実装に置換する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Float Constants Portably (Priority: P1)

The Clysm compiler must be able to emit IEEE 754 floating-point constants in Wasm binary format without relying on SBCL-specific internal functions. When a user compiles Lisp code containing float literals, the compiler should correctly encode them as Wasm f32/f64 constants, regardless of which Common Lisp implementation is running the compiler.

**Why this priority**: This is the core functionality required for the compiler to work on non-SBCL implementations. Without this, the compiler cannot produce valid Wasm output on other CLs.

**Independent Test**: Can be fully tested by compiling a Lisp program with various float literals and validating the generated Wasm binary encodes them correctly.

**Acceptance Scenarios**:

1. **Given** a Lisp source with float literal `3.14d0`, **When** compiled to Wasm, **Then** the binary contains the correct 64-bit IEEE 754 encoding `0x40091EB851EB851F`.
2. **Given** a Lisp source with float literal `2.5f0`, **When** compiled to Wasm, **Then** the binary contains the correct 32-bit IEEE 754 encoding `0x40200000`.
3. **Given** a Lisp source compiled on SBCL, **When** the same source is compiled on another CL implementation, **Then** identical Wasm binaries are produced.

---

### User Story 2 - Handle Special Float Values (Priority: P1)

The compiler must correctly handle IEEE 754 special values including positive infinity, negative infinity, NaN (Not-a-Number), and subnormal (denormalized) numbers. These values must be encoded correctly in Wasm output and handled safely during constant folding at compile time.

**Why this priority**: Special values are commonly produced by division operations (e.g., `(/ 1.0 0.0)`) and are essential for IEEE 754 compliance. Incorrect handling could cause compiler crashes or produce incorrect results.

**Independent Test**: Can be tested by compiling expressions that produce special values and verifying the Wasm binary contains the correct bit patterns.

**Acceptance Scenarios**:

1. **Given** the expression `(/ 1.0d0 0.0d0)` is constant-folded, **When** the result is encoded in Wasm, **Then** the binary contains positive infinity (`0x7FF0000000000000`).
2. **Given** the expression `(/ -1.0d0 0.0d0)` is constant-folded, **When** the result is encoded in Wasm, **Then** the binary contains negative infinity (`0xFFF0000000000000`).
3. **Given** the expression `(/ 0.0d0 0.0d0)` is constant-folded, **When** the result is encoded in Wasm, **Then** the binary contains a valid NaN bit pattern (exponent all 1s, non-zero mantissa).
4. **Given** a subnormal float value `5.0d-324`, **When** encoded in Wasm, **Then** the binary contains the correct subnormal representation (exponent = 0, non-zero mantissa).

---

### User Story 3 - Safe Compile-Time Arithmetic (Priority: P2)

The compiler must safely perform constant folding on arithmetic expressions without triggering floating-point exceptions or traps. Operations that produce special values should complete successfully and store the correct result.

**Why this priority**: Constant folding improves performance and is used extensively. It must work correctly without relying on SBCL's trap masking mechanism.

**Independent Test**: Can be tested by compiling expressions with division by zero and verifying compilation succeeds and produces correct results.

**Acceptance Scenarios**:

1. **Given** the expression `(+ (/ 1.0 0.0) 1.0)` at compile time, **When** constant folding is attempted, **Then** folding succeeds without error and produces infinity.
2. **Given** the expression `(* 0.0 (/ 1.0 0.0))` at compile time, **When** constant folding is attempted, **Then** folding succeeds and produces NaN.

---

### User Story 4 - Portable Test Harness (Priority: P2)

The test harness must be able to verify float operations and special values without relying on SBCL-specific functions. Tests should produce consistent results across different CL implementations.

**Why this priority**: Tests ensure correctness. If tests themselves are SBCL-specific, portability cannot be verified.

**Independent Test**: Can be tested by running the existing float-related tests on a non-SBCL implementation (e.g., CCL or ECL).

**Acceptance Scenarios**:

1. **Given** the test harness function `compile-and-run-numeric`, **When** evaluating `(/ 1.0 0.0)`, **Then** the result is correctly identified as positive infinity on any CL implementation.
2. **Given** float operation tests, **When** run on SBCL and another CL implementation, **Then** all tests pass on both.

---

### Edge Cases

- What happens when encoding the smallest positive subnormal float (`2^-1074` for double)?
- How does the system handle quiet NaN vs signaling NaN bit patterns?
- What happens when converting between single and double precision special values?
- How are negative zero (`-0.0`) and positive zero (`+0.0`) distinguished?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST emit correct 32-bit IEEE 754 binary representation for single-float values.
- **FR-002**: Compiler MUST emit correct 64-bit IEEE 754 binary representation for double-float values.
- **FR-003**: Compiler MUST correctly encode positive infinity (`+Inf`) with bit pattern `0x7F800000` (f32) / `0x7FF0000000000000` (f64).
- **FR-004**: Compiler MUST correctly encode negative infinity (`-Inf`) with bit pattern `0xFF800000` (f32) / `0xFFF0000000000000` (f64).
- **FR-005**: Compiler MUST correctly encode NaN with exponent bits all 1s and non-zero mantissa.
- **FR-006**: Compiler MUST correctly encode subnormal numbers (exponent = 0, non-zero mantissa).
- **FR-007**: Compiler MUST correctly encode negative zero with sign bit = 1 and all other bits = 0.
- **FR-008**: Compiler MUST NOT use `sb-kernel:single-float-bits` or `sb-kernel:double-float-bits`.
- **FR-009**: Compiler MUST NOT use `sb-int:with-float-traps-masked`.
- **FR-010**: Constant folding MUST NOT crash or signal floating-point exceptions for any valid float arithmetic.
- **FR-011**: Generated Wasm binaries MUST be identical regardless of which CL implementation runs the compiler.
- **FR-012**: Test harness MUST work correctly on non-SBCL implementations.

### Key Entities

- **Float Encoding**: The mapping from a floating-point value to its IEEE 754 binary representation (sign bit, exponent, mantissa).
- **Special Values**: +Infinity, -Infinity, NaN (quiet/signaling), subnormal numbers, negative zero.
- **Constant Folding**: Compile-time evaluation of arithmetic expressions that may produce special values.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing float-related unit tests pass after the change.
- **SC-002**: Compiler produces bit-identical Wasm output for float constants compared to the current SBCL-based implementation.
- **SC-003**: No occurrences of `sb-kernel:single-float-bits`, `sb-kernel:double-float-bits`, or `sb-int:with-float-traps-masked` remain in the compiler source.
- **SC-004**: Compilation of float-heavy code completes successfully without floating-point exceptions.
- **SC-005**: All IEEE 754 special values (+Inf, -Inf, NaN, subnormal, -0.0) are correctly encoded in Wasm output.

## Assumptions

- The host environment provides a way to convert floats to bit patterns (JavaScript DataView API for wasmtime/browser, or portable CL implementation).
- The clysm3 project already has FFI infrastructure (`host-shim/`) that can be extended for float bit extraction.
- Single-float is 32-bit IEEE 754 and double-float is 64-bit IEEE 754 on all target platforms.
- Little-endian byte order is used for Wasm float encoding (per Wasm specification).

## Dependencies

- Existing FFI infrastructure (Feature 027: Complete FFI Foundation).
- Existing float type support (Feature 010: Numeric Tower).
- Host-shim JavaScript files (`host-shim/io-shim.js`, `host-shim/run-wasm.js`).

## Out of Scope

- Changes to Wasm runtime float semantics (this feature is compiler-side only).
- Support for 128-bit or other non-standard float formats.
- Optimization of float compilation performance (functional correctness is the goal).
