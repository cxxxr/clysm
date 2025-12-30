# Feature Specification: Phase 16A - ANSI Character Functions

**Feature Branch**: `001-ansi-char-functions`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 16A: 文字関数 (Character Functions) を実装する。目標はANSI CL文字関数の完全実装とcharactersカテゴリ80%+準拠率。graphic-char-p（印字可能文字判定）、standard-char-p（標準文字判定）、both-case-p（大小文字両方持つか）、char-name/name-char（文字と名前の相互変換）、digit-char（数値から数字文字への変換）、char-int（文字から整数）を実装する。既存のchar-upcase, char-downcase, alpha-char-p, digit-char-pは実装済みなので、それらと整合性を持たせる。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Character Type Classification (Priority: P1)

Developers need to classify characters by their type to implement text processing logic. The character classification predicates (`graphic-char-p`, `standard-char-p`, `both-case-p`) enable developers to write portable code that handles different character categories correctly.

**Why this priority**: Character classification is fundamental for text processing, input validation, and reader implementation. These predicates are frequently used in combination with existing functions.

**Independent Test**: Can be fully tested by calling each predicate with various character inputs and verifying the returned boolean values against ANSI CL specification.

**Acceptance Scenarios**:

1. **Given** a printable character like `#\A` or `#\Space`, **When** `graphic-char-p` is called, **Then** it returns `T`.
2. **Given** a non-printable control character like `#\Null` (U+0000), **When** `graphic-char-p` is called, **Then** it returns `NIL`.
3. **Given** a standard ASCII letter `#\a`, **When** `standard-char-p` is called, **Then** it returns `T`.
4. **Given** an extended Unicode character like `#\U+3042` (hiragana 'a'), **When** `standard-char-p` is called, **Then** it returns `NIL`.
5. **Given** a letter with both cases `#\A`, **When** `both-case-p` is called, **Then** it returns `T`.
6. **Given** a digit `#\5` or punctuation `#\!`, **When** `both-case-p` is called, **Then** it returns `NIL`.

---

### User Story 2 - Character Name Conversion (Priority: P2)

Developers need to convert between characters and their symbolic names for human-readable output, debugging, and reader/printer implementation. The `char-name` and `name-char` functions provide bidirectional conversion.

**Why this priority**: Name conversion is essential for implementing the Lisp reader/printer and for debugging character-related issues. It enables portable representation of non-printable characters.

**Independent Test**: Can be fully tested by converting characters to names and back, verifying round-trip consistency and standard name recognition.

**Acceptance Scenarios**:

1. **Given** a named character `#\Space`, **When** `char-name` is called, **Then** it returns `"Space"`.
2. **Given** a newline character `#\Newline`, **When** `char-name` is called, **Then** it returns `"Newline"`.
3. **Given** a regular printable character `#\A`, **When** `char-name` is called, **Then** it returns `NIL` (no special name).
4. **Given** a valid name string `"Space"`, **When** `name-char` is called, **Then** it returns `#\Space`.
5. **Given** a name in any case `"NEWLINE"`, `"newline"`, or `"Newline"`, **When** `name-char` is called, **Then** it returns `#\Newline` (case-insensitive).
6. **Given** an invalid name string `"InvalidName"`, **When** `name-char` is called, **Then** it returns `NIL`.

---

### User Story 3 - Digit-Character Conversion (Priority: P2)

Developers need to convert numeric values to their corresponding digit characters for number formatting and output generation. The `digit-char` function converts integer weights to digit characters in any radix.

**Why this priority**: Essential for implementing numeric output formatting, base conversion, and the `format` directive. Works as the inverse of the existing `digit-char-p`.

**Independent Test**: Can be fully tested by converting weights 0-35 to characters in various radices and verifying the output characters.

**Acceptance Scenarios**:

1. **Given** weight 5 and radix 10, **When** `digit-char` is called, **Then** it returns `#\5`.
2. **Given** weight 10 and radix 16, **When** `digit-char` is called, **Then** it returns `#\A`.
3. **Given** weight 15 and radix 16, **When** `digit-char` is called, **Then** it returns `#\F`.
4. **Given** weight 35 and radix 36, **When** `digit-char` is called, **Then** it returns `#\Z`.
5. **Given** weight 10 and radix 10 (weight >= radix), **When** `digit-char` is called, **Then** it returns `NIL`.
6. **Given** negative weight -1, **When** `digit-char` is called, **Then** it returns `NIL`.

---

### User Story 4 - Character Integer Conversion (Priority: P3)

Developers need to obtain the integer encoding of characters for low-level character manipulation, hashing, and comparison operations. The `char-int` function returns the implementation-defined integer encoding.

**Why this priority**: Lower priority as `char-code` already provides similar functionality, but `char-int` is required for ANSI compliance and may include additional character attributes.

**Independent Test**: Can be fully tested by converting characters to integers and verifying uniqueness and consistency properties.

**Acceptance Scenarios**:

1. **Given** character `#\A`, **When** `char-int` is called, **Then** it returns an integer (65 for ASCII/UTF-8 encoding).
2. **Given** character `#\Space`, **When** `char-int` is called, **Then** it returns an integer (32 for ASCII/UTF-8 encoding).
3. **Given** two equal characters, **When** `char-int` is called on both, **Then** both return the same integer.
4. **Given** two different characters, **When** `char-int` is called on both, **Then** they return different integers.

---

### Edge Cases

- What happens when `graphic-char-p` receives a control character (U+0000-U+001F)?
  - Returns `NIL` for all control characters except `#\Space` (U+0020).
- How does `char-name` handle Unicode characters beyond ASCII?
  - Returns `NIL` for characters without standard names; implementation supports only ANSI-required names.
- What happens when `digit-char` receives a radix outside 2-36?
  - Signals a type error per ANSI CL specification.
- How does `name-char` handle names with different capitalizations?
  - Case-insensitive per ANSI CL specification.
- What happens when `both-case-p` receives a non-alphabetic character?
  - Returns `NIL` as non-alphabetic characters don't have case variants.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement `graphic-char-p` that returns `T` for printable characters (code points 32-126, excluding control characters) and `NIL` otherwise.
- **FR-002**: System MUST implement `standard-char-p` that returns `T` for the 96 standard characters (A-Z, a-z, 0-9, space, newline, and the 13 standard punctuation: `!$"'(),.:;?<space><newline>`) and `NIL` otherwise.
- **FR-003**: System MUST implement `both-case-p` that returns `T` for characters that have both uppercase and lowercase variants (A-Z and a-z), `NIL` otherwise.
- **FR-004**: System MUST implement `char-name` that returns a string name for named characters (`Space`, `Newline`, `Tab`, `Return`, `Page`, `Backspace`, `Rubout`, `Linefeed`, `Null`) and `NIL` for unnamed characters.
- **FR-005**: System MUST implement `name-char` that returns the character for a given name string (case-insensitive) or `NIL` if no such character exists.
- **FR-006**: System MUST implement `digit-char` that takes weight and optional radix (default 10, range 2-36) and returns the character for that digit weight, or `NIL` if weight >= radix or weight < 0.
- **FR-007**: System MUST implement `char-int` that returns an integer representation unique for each character (equivalent to `char-code` in this implementation).
- **FR-008**: All new functions MUST be compiled to WasmGC bytecode consistent with existing character functions (`char-upcase`, `char-downcase`, `alpha-char-p`, `digit-char-p`).
- **FR-009**: All new functions MUST support Unicode characters represented as i32 code points in WasmGC.

### Key Entities

- **Character**: Represented as i32 Unicode code point in WasmGC. Characters are immediate values, not boxed objects.
- **Character Name**: String representation of special characters (e.g., "Space", "Newline"). Names are case-insensitive for lookup.
- **Radix**: Integer 2-36 specifying the numeric base for digit operations.
- **Weight**: Non-negative integer 0-35 representing a digit's numeric value in a given radix.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 6 new character functions compile to valid WasmGC bytecode that passes validation.
- **SC-002**: Characters category compliance rate reaches 80% or higher (measured by ANSI CL character function coverage).
- **SC-003**: All acceptance scenarios pass automated testing.
- **SC-004**: Round-trip conversion `(name-char (char-name x))` returns the original character for all named characters.
- **SC-005**: Round-trip conversion `(digit-char (digit-char-p x))` returns the original character for all digit characters in the same radix.
- **SC-006**: New functions integrate seamlessly with existing character functions without regression in existing tests.

## Assumptions

- Unicode code points are represented as i32 values in WasmGC (consistent with existing implementation).
- The 96 standard characters are the ASCII printable characters (32-126) plus newline.
- Character names follow ANSI CL conventions: Space, Newline, Tab, Return, Page, Backspace, Rubout (DEL), Linefeed, Null.
- `char-int` is equivalent to `char-code` in this implementation (no additional character attributes).
- Radix validation follows ANSI CL: must be integer 2-36, signal type error otherwise.
