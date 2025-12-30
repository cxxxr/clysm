# Feature Specification: Character Literal Compilation Support

**Feature Branch**: `001-char-literal-compile`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 13D-1a: 文字リテラル対応を実装する。目標はDEFUN本体のコンパイル成功率向上。func-section.lisp:509-525のcompile-quoted-element関数にcharacterpの分岐を追加し、文字リテラル（#\Space, #\Tab, #\Newline等）をi31refとしてコンパイル可能にする。テストケース: (member char '(#\Space #\Tab))のコンパイルが成功すること。期待する効果: コンパイル率14%→20%への向上。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Quoted Character Literals (Priority: P1)

As a Clysm compiler developer, I need the compiler to handle character literals in quoted expressions so that functions using character comparison patterns can be compiled successfully.

**Why this priority**: This is the core functionality that directly addresses the compilation failure. Many Common Lisp functions use character literal patterns like `'(#\Space #\Tab #\Newline)` for whitespace handling, and these currently cause compilation to fail.

**Independent Test**: Can be fully tested by compiling a form containing `(member char '(#\Space #\Tab))` and verifying the generated Wasm is valid.

**Acceptance Scenarios**:

1. **Given** a quoted list containing character literals like `'(#\Space #\Tab)`, **When** the compiler processes this expression, **Then** valid Wasm instructions are generated for each character literal.

2. **Given** a function body containing `(member x '(#\a #\b #\c))`, **When** compiling the entire defun, **Then** compilation succeeds without errors.

3. **Given** a standalone quoted character literal `'#\Newline`, **When** the compiler processes this expression, **Then** it produces a valid i31ref representation.

---

### User Story 2 - Support All Standard Character Literals (Priority: P2)

As a Clysm compiler developer, I need the compiler to handle all standard Common Lisp character literals including named characters and arbitrary character codes.

**Why this priority**: While the basic feature enables whitespace character handling, full support for all character types ensures compatibility with more complex codebases.

**Independent Test**: Can be fully tested by compiling forms with various character literals: printable characters, named characters (#\Space, #\Tab, #\Newline, #\Return), and arbitrary characters.

**Acceptance Scenarios**:

1. **Given** printable ASCII character literals like `#\a`, `#\Z`, `#\!`, **When** compiling quoted expressions containing them, **Then** compilation succeeds with correct character code values.

2. **Given** named character literals like `#\Space` (32), `#\Tab` (9), `#\Newline` (10), `#\Return` (13), **When** compiling quoted expressions containing them, **Then** compilation succeeds with correct character code values.

3. **Given** Unicode character literals, **When** compiling quoted expressions containing them, **Then** compilation succeeds with the correct Unicode code point values.

---

### Edge Cases

- What happens when the character code exceeds i31ref range (>1073741823)?
  - **Assumption**: All valid Unicode code points (0-1114111) fit within i31ref range, so this is not a practical concern for characters.

- How does system handle invalid or malformed character literals?
  - These are rejected at read time by the Lisp reader before reaching the compiler, so no special handling needed.

- How are character literals in nested quoted structures handled?
  - The recursive nature of `compile-quoted-list` already handles nesting; adding character support to `compile-quoted-element` covers all cases.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST recognize character objects in quoted expressions via the `characterp` type predicate.
- **FR-002**: Compiler MUST convert character literals to their integer code point using `char-code`.
- **FR-003**: Compiler MUST encode character code points as i31ref values (same encoding as fixnum integers).
- **FR-004**: Compiler MUST handle all standard Common Lisp named characters (#\Space, #\Tab, #\Newline, #\Return, #\Page, #\Rubout, #\Linefeed, #\Backspace).
- **FR-005**: Compiler MUST handle arbitrary printable character literals (#\a through #\z, #\A through #\Z, digits, punctuation).
- **FR-006**: Compiler MUST handle Unicode characters whose code points fit in i31ref range.
- **FR-007**: Generated Wasm MUST pass validation by wasm-tools.

### Key Entities

- **Character Literal**: A Common Lisp character object appearing in quoted expressions, represented by reader syntax like `#\Space` or `#\a`.
- **i31ref**: WebAssembly GC reference type for 31-bit integers, used to efficiently represent fixnums and character codes.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Compilation of `(member char '(#\Space #\Tab))` completes successfully (no errors thrown).
- **SC-002**: Generated Wasm from forms containing character literals passes `wasm-tools validate`.
- **SC-003**: Overall DEFUN body compilation success rate increases from 14% to at least 20% (verified by running Stage 1 generation).
- **SC-004**: All existing compiler tests continue to pass (no regressions).
- **SC-005**: Character literal compilation produces correct Wasm instruction sequence: `i32.const <char-code>` followed by `ref.i31`.

## Assumptions

- Character codes in Common Lisp are non-negative integers representing Unicode code points.
- All Unicode code points (0 to 1114111) fit within the i31ref range (0 to 1073741823).
- The existing `compile-quoted-element` function structure supports adding new type branches.
- Character equality at runtime will be handled by comparing i31ref values directly (code point comparison).
