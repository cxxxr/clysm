# Feature Specification: I/O Print Primitives

**Feature Branch**: `001-io-print-primitives`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 13D-1d: I/O関数プリミティブ (print/format) を実装する。目標はDEFUNコンパイル失敗の根本原因解決とコンパイル率55%達成。具体的には: (1) print/prin1/princ/terpriをFFI呼び出しスタブとして追加、(2) format関数の基本ディレクティブ (~A, ~S, ~D, ~%, ~&, ~~) を実装、(3) write関数の基本実装。検証: (format nil "~A" 42) => "42"、printを含むDEFUN本体がコンパイル成功すること。"

## Overview

This feature adds I/O print primitive support to the Clysm compiler, enabling Common Lisp I/O functions to be compiled to WebAssembly. This resolves a key blocker for DEFUN compilation failures where print-related forms cause compilation to fail. The primary goal is to increase the overall compilation success rate from the current level toward 55%.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Print Output Compilation (Priority: P1)

As a compiler user, I want to compile Lisp code containing `print`, `prin1`, `princ`, and `terpri` calls so that I/O operations in my code don't cause compilation failures.

**Why this priority**: Print functions are fundamental I/O operations used throughout the codebase. Without them, many DEFUN bodies fail to compile, blocking progress toward self-hosting.

**Independent Test**: Compile a simple function containing `(print x)` and verify the output Wasm is valid.

**Acceptance Scenarios**:

1. **Given** source code `(defun greet (x) (print x) x)`, **When** compiled to Wasm, **Then** the compilation succeeds and output Wasm validates without errors
2. **Given** source code `(princ "hello")`, **When** compiled to Wasm, **Then** the compilation succeeds
3. **Given** source code `(prin1 'symbol)`, **When** compiled to Wasm, **Then** the compilation succeeds
4. **Given** source code `(terpri)`, **When** compiled to Wasm, **Then** the compilation succeeds and a newline operation is represented

---

### User Story 2 - Format String Compilation (Priority: P1)

As a compiler user, I want to compile Lisp code containing `format` calls with basic directives so that string formatting operations compile successfully.

**Why this priority**: `format` is extensively used in the compiler codebase for string construction and error messages. Supporting basic directives unblocks significant portions of code.

**Independent Test**: Compile `(format nil "~A" 42)` and verify it produces valid Wasm that can be executed.

**Acceptance Scenarios**:

1. **Given** source code `(format nil "~A" 42)`, **When** compiled and executed, **Then** the result is the string "42"
2. **Given** source code `(format nil "~S" "hello")`, **When** compiled and executed, **Then** the result includes quotation marks: "\"hello\""
3. **Given** source code `(format nil "~D" 255)`, **When** compiled and executed, **Then** the result is "255"
4. **Given** source code `(format nil "Line1~%Line2")`, **When** compiled and executed, **Then** the result contains a newline between "Line1" and "Line2"
5. **Given** source code `(format nil "~&Start")`, **When** compiled and executed, **Then** the result handles fresh-line correctly
6. **Given** source code `(format nil "100~~")`, **When** compiled and executed, **Then** the result is "100~"

---

### User Story 3 - Write Function Compilation (Priority: P2)

As a compiler user, I want to compile Lisp code containing `write` calls so that lower-level output operations are available.

**Why this priority**: `write` provides the foundation for customized output. While less commonly used directly than `print` or `format`, it's needed for complete I/O support.

**Independent Test**: Compile a function using `write` and verify Wasm validity.

**Acceptance Scenarios**:

1. **Given** source code `(write obj)`, **When** compiled to Wasm, **Then** the compilation succeeds
2. **Given** source code `(write obj :stream nil)`, **When** compiled to Wasm, **Then** returns the printed representation as a string

---

### User Story 4 - Format to Stream Output (Priority: P3)

As a compiler user, I want `format` to work with stream destinations (t for standard output) so that console output is possible.

**Why this priority**: While `format nil` (string output) is most critical for compilation, stream output to standard output enables debugging and user interaction.

**Independent Test**: Compile `(format t "Hello~%")` and verify it outputs to standard output when executed.

**Acceptance Scenarios**:

1. **Given** source code `(format t "Hello")`, **When** compiled and executed in a runtime environment, **Then** "Hello" appears on standard output
2. **Given** source code `(format t "Value: ~A~%" x)`, **When** compiled and executed, **Then** the formatted string with x's value appears on standard output followed by newline

---

### Edge Cases

- What happens when `format` receives a nil argument for `~A` directive? Expected: prints "NIL"
- What happens when `format` receives too few arguments for directives? Expected: compilation succeeds; runtime error or undefined behavior
- How does `~%` behave when output destination is a string (nil)? Expected: inserts newline character into string
- What happens when an unknown directive is used? Expected: compilation fails with clear error message

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST recognize and compile [print](resources/HyperSpec/Body/f_wr_pr.htm) function calls to valid Wasm output
- **FR-002**: Compiler MUST recognize and compile [prin1](resources/HyperSpec/Body/f_wr_pr.htm) function calls to valid Wasm output
- **FR-003**: Compiler MUST recognize and compile [princ](resources/HyperSpec/Body/f_wr_pr.htm) function calls to valid Wasm output
- **FR-004**: Compiler MUST recognize and compile [terpri](resources/HyperSpec/Body/f_terpri.htm) function calls to valid Wasm output
- **FR-005**: Compiler MUST recognize and compile [format](resources/HyperSpec/Body/f_format.htm) function calls with supported directives to valid Wasm output
- **FR-006**: Compiler MUST support the [~A](resources/HyperSpec/Body/22_cca.htm) (aesthetic) format directive for human-readable output
- **FR-007**: Compiler MUST support the [~S](resources/HyperSpec/Body/22_ccb.htm) (standard) format directive for machine-readable output with escape characters
- **FR-008**: Compiler MUST support the [~D](resources/HyperSpec/Body/22_cba.htm) (decimal) format directive for integer output
- **FR-009**: Compiler MUST support the [~%](resources/HyperSpec/Body/22_cea.htm) (newline) format directive
- **FR-010**: Compiler MUST support the [~&](resources/HyperSpec/Body/22_ceb.htm) (fresh-line) format directive
- **FR-011**: Compiler MUST support the [~~](resources/HyperSpec/Body/22_cfa.htm) (tilde escape) format directive
- **FR-012**: Compiler MUST recognize and compile [write](resources/HyperSpec/Body/f_wr_pr.htm) function calls to valid Wasm output
- **FR-013**: When [format](resources/HyperSpec/Body/f_format.htm) destination is `nil`, the result MUST be a string containing the formatted output
- **FR-014**: When [format](resources/HyperSpec/Body/f_format.htm) destination is `t`, output MUST be directed to standard output via FFI
- **FR-015**: Generated Wasm MUST pass validation with wasm-tools validate

### Key Entities

- **Format Directive**: A control sequence in format strings (e.g., ~A, ~S, ~D) that specifies how arguments should be formatted
- **Stream Destination**: The target for output operations; `nil` means return string, `t` means standard output
- **Print Readability**: Distinction between aesthetic (~A, princ) and readable (~S, prin1) output representations

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All DEFUN bodies containing print/prin1/princ/terpri compile successfully where they previously failed
- **SC-002**: `(format nil "~A" 42)` compiles and when executed returns exactly "42"
- **SC-003**: Format directives ~A, ~S, ~D, ~%, ~&, ~~ all compile without errors
- **SC-004**: Overall Clysm compiler compilation rate increases from ~25% toward 55% target (measured by `dist/stage1-report.json`)
- **SC-005**: All generated Wasm modules pass `wasm-tools validate` with exit code 0
- **SC-006**: No regression in existing compilation capabilities (current passing tests continue to pass)

## Assumptions

- FFI host environment (Node.js) provides I/O capabilities that can be called from Wasm
- Existing FFI infrastructure from feature 027 can be extended for print operations
- String handling infrastructure from feature 016B (string trim) is available
- Integer-to-string conversion from feature 001-numeric-format (write-to-string) can be reused
