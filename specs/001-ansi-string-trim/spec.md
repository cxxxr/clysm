# Feature Specification: ANSI String Trim Functions (Phase 16B)

**Feature Branch**: `001-ansi-string-trim`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 16B: 文字列トリム関数を実装する。目標は文字列操作関数の完成。string-trim, string-left-trim, string-right-trimで文字列の端から指定文字集合を除去。nstring-upcase, nstring-downcase, nstring-capitalizeで破壊的なケース変換を実装。ANSI CL仕様に準拠し、:start/:end キーワード引数をサポートすること。検証基準: (string-trim \" \" \" test \") => \"test\"、strings カテゴリ 70%+パス。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - String Trimming (Priority: P1)

As a Clysm compiler user, I want to trim whitespace and other characters from strings so that I can clean and normalize input data.

**Why this priority**: String trimming is the most commonly used string manipulation operation and directly enables the primary validation criterion. This is essential for parsing user input, cleaning data, and string normalization.

**Independent Test**: Can be fully tested by calling `(string-trim " " " test ")` and verifying the result is `"test"`. Delivers value immediately for any code that needs to clean string boundaries.

**Acceptance Scenarios**:

1. **Given** a string with leading and trailing spaces `" test "`, **When** `string-trim` is called with space as the character bag, **Then** the result is `"test"` with no leading or trailing spaces.
2. **Given** a string with multiple trim characters `"...hello..."`, **When** `string-trim` is called with `"."` as the character bag, **Then** the result is `"hello"`.
3. **Given** a string with characters to trim only at the start `"  hello"`, **When** `string-left-trim` is called with space as the character bag, **Then** the result is `"hello"` with leading spaces removed.
4. **Given** a string with characters to trim only at the end `"hello  "`, **When** `string-right-trim` is called with space as the character bag, **Then** the result is `"hello"` with trailing spaces removed.
5. **Given** a string and `:start`/`:end` arguments, **When** any trim function is called with these bounds, **Then** trimming only occurs within the specified substring region.

---

### User Story 2 - Destructive Case Conversion (Priority: P2)

As a Clysm compiler user, I want to perform in-place case conversion on strings so that I can efficiently transform case without allocating new strings.

**Why this priority**: Destructive operations are important for performance-critical code and complete the ANSI CL string function set. Required for self-hosting compiler optimizations.

**Independent Test**: Can be fully tested by creating a string, calling `nstring-upcase` on it, and verifying the original string is modified in place to uppercase. Delivers value for any code needing efficient case transformation.

**Acceptance Scenarios**:

1. **Given** a string `"hello"`, **When** `nstring-upcase` is called, **Then** the original string is modified to `"HELLO"` and the same string object is returned.
2. **Given** a string `"HELLO"`, **When** `nstring-downcase` is called, **Then** the original string is modified to `"hello"` and the same string object is returned.
3. **Given** a string `"hello world"`, **When** `nstring-capitalize` is called, **Then** the original string is modified to `"Hello World"` and the same string object is returned.
4. **Given** a string and `:start`/`:end` keyword arguments, **When** any nstring function is called with these bounds, **Then** only the specified substring region is converted.
5. **Given** a string with mixed case `"HeLLo"`, **When** `nstring-capitalize` is called, **Then** the result is `"Hello"` (first letter uppercase, rest lowercase for each word).

---

### User Story 3 - ANSI CL Compliance (Priority: P3)

As a Clysm compiler developer, I want string functions to comply with ANSI CL specifications so that code written for standard Common Lisp works correctly.

**Why this priority**: ANSI compliance ensures compatibility with existing Lisp code and libraries, enabling broader ecosystem support.

**Independent Test**: Can be fully tested by running ANSI CL conformance tests for string functions and verifying expected behavior matches the standard.

**Acceptance Scenarios**:

1. **Given** a character bag specified as a sequence (string or list), **When** trim functions are called, **Then** all characters in the bag are recognized and trimmed.
2. **Given** the strings category test suite, **When** tests are executed, **Then** at least 70% of tests pass.
3. **Given** nil or empty character bag, **When** trim functions are called, **Then** the original string is returned unchanged.
4. **Given** invalid type arguments, **When** functions are called, **Then** appropriate type errors are signaled.

---

### Edge Cases

- What happens when the string is empty? The empty string is returned unchanged.
- What happens when the character bag contains all characters in the string? An empty string is returned.
- What happens when `:start` equals `:end`? The original string is returned unchanged (no characters in the range to process).
- What happens when `:start` or `:end` is out of bounds? A type error or bounds error is signaled.
- What happens with non-ASCII/Unicode characters? Characters are compared by character code; Unicode characters are handled correctly.
- What happens when nstring functions are called on literal strings? The behavior is undefined per ANSI CL (implementation may modify or error).
- How does `nstring-capitalize` handle non-alphabetic word boundaries? Non-alphabetic characters act as word separators; characters following them are treated as word starts.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement [string-trim](resources/HyperSpec/Body/f_stg_tr.htm) that removes characters from both ends of a string based on a character bag.
- **FR-002**: System MUST implement [string-left-trim](resources/HyperSpec/Body/f_stg_tr.htm) that removes characters from the start of a string based on a character bag.
- **FR-003**: System MUST implement [string-right-trim](resources/HyperSpec/Body/f_stg_tr.htm) that removes characters from the end of a string based on a character bag.
- **FR-004**: All trim functions MUST accept a character bag as the first argument (string or sequence of characters).
- **FR-005**: All trim functions MUST accept `:start` keyword argument specifying the beginning index of the substring to process.
- **FR-006**: All trim functions MUST accept `:end` keyword argument specifying the end index of the substring to process (exclusive).
- **FR-007**: Trim functions MUST return a new string (non-destructive).
- **FR-008**: System MUST implement [nstring-upcase](resources/HyperSpec/Body/f_stg_up.htm) that converts a string to uppercase in place.
- **FR-009**: System MUST implement [nstring-downcase](resources/HyperSpec/Body/f_stg_up.htm) that converts a string to lowercase in place.
- **FR-010**: System MUST implement [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm) that capitalizes each word in place (first character uppercase, rest lowercase).
- **FR-011**: All nstring functions MUST accept `:start` keyword argument specifying the beginning index.
- **FR-012**: All nstring functions MUST accept `:end` keyword argument specifying the end index.
- **FR-013**: All nstring functions MUST modify the original string and return it (destructive).
- **FR-014**: System MUST signal type errors when arguments are of incorrect type.
- **FR-015**: System MUST handle bounding index designators correctly (nil for `:end` means end of string).

### Key Entities

- **Character Bag**: A sequence (string, list, or vector) of characters to be used for trimming operations. Any character present in the bag is eligible for removal from string boundaries.
- **Bounding Index Designator**: A non-negative integer or nil indicating a position in the string. Used by `:start` (defaults to 0) and `:end` (defaults to nil, meaning string length) keyword arguments.
- **String**: The target string to be modified or from which a trimmed copy is created.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `(string-trim " " " test ")` returns `"test"` as specified in the validation criterion.
- **SC-002**: The strings category test suite achieves at least 70% pass rate.
- **SC-003**: All six functions (`string-trim`, `string-left-trim`, `string-right-trim`, `nstring-upcase`, `nstring-downcase`, `nstring-capitalize`) compile to valid WasmGC code.
- **SC-004**: Destructive nstring functions demonstrably modify the original string object rather than creating new strings.
- **SC-005**: All functions correctly handle `:start` and `:end` keyword arguments across the full range of valid inputs.

## Assumptions

- The existing string infrastructure (from Phase 16A character functions) provides basic string creation, access, and UTF-8 encoding.
- Character case conversion (char-upcase, char-downcase) is already available from the character functions implementation.
- The `$string` WasmGC type (index 4) is already defined with UTF-8 byte storage.
- Standard word boundaries for capitalization follow ANSI CL convention: any non-alphanumeric character acts as a word separator.
- Performance optimization is secondary to correctness; these functions may be implemented with straightforward algorithms initially.

## Scope Boundaries

**In Scope**:
- The six specified string functions with full `:start`/`:end` support
- ANSI CL compliant behavior for all edge cases
- Integration with existing Clysm string infrastructure
- Unit tests for each function

**Out of Scope**:
- Non-standard string functions (e.g., string-split, string-join)
- Locale-aware case conversion
- Regular expression-based trimming
- String interning or symbol name modifications
