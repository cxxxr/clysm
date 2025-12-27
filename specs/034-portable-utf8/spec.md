# Feature Specification: Portable UTF-8 Encoding

**Feature Branch**: `034-portable-utf8`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 10B: UTF-8エンコーディング (Babel代替) を実装する。目標はbabel:string-to-octets/babel:octets-to-string の依存を排除し、ポータブルなCL実装を提供すること。string-to-utf8-octets, utf8-octets-to-string関数をsrc/clysm/lib/以下に実装。4バイトUTF-8（サロゲートペア）対応必須。不正シーケンスはdecoding-error条件をシグナル。コンパイラ内の6箇所のbabel呼び出しを置換し、`grep -r 'babel:' src/`の結果が0件になること。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Encode Lisp Strings to UTF-8 Bytes (Priority: P1)

As a Clysm compiler developer, I want to encode Common Lisp strings to UTF-8 byte sequences so that the compiler can generate valid Wasm binary format strings without depending on the Babel library.

**Why this priority**: This is the core functionality needed by all 6 existing call sites in the compiler. Without this, the compiler cannot generate Wasm binaries.

**Independent Test**: Can be tested by encoding various strings (ASCII, Japanese, emoji) and verifying the resulting byte sequences match expected UTF-8 encoding.

**Acceptance Scenarios**:

1. **Given** an ASCII string "hello", **When** `string-to-utf8-octets` is called, **Then** returns `#(104 101 108 108 111)` (5 bytes)
2. **Given** a Japanese string "日本語", **When** `string-to-utf8-octets` is called, **Then** returns a 9-byte vector (3 bytes per character)
3. **Given** a 4-byte emoji "🎉" (U+1F389), **When** `string-to-utf8-octets` is called, **Then** returns `#(240 159 142 137)` (4 bytes)
4. **Given** an empty string "", **When** `string-to-utf8-octets` is called, **Then** returns an empty octet vector `#()`

---

### User Story 2 - Decode UTF-8 Bytes to Lisp Strings (Priority: P2)

As a Clysm compiler developer, I want to decode UTF-8 byte sequences back to Common Lisp strings so that error messages and debugging output can properly handle UTF-8 encoded data.

**Why this priority**: Required for bidirectional UTF-8 handling, though currently less used than encoding in the compiler.

**Independent Test**: Can be tested by decoding known UTF-8 byte sequences and verifying the resulting strings match expected characters.

**Acceptance Scenarios**:

1. **Given** UTF-8 bytes `#(104 101 108 108 111)`, **When** `utf8-octets-to-string` is called, **Then** returns "hello"
2. **Given** UTF-8 bytes for "日本語" (9 bytes), **When** `utf8-octets-to-string` is called, **Then** returns "日本語"
3. **Given** UTF-8 bytes `#(240 159 142 137)`, **When** `utf8-octets-to-string` is called, **Then** returns "🎉"
4. **Given** an empty byte vector `#()`, **When** `utf8-octets-to-string` is called, **Then** returns ""

---

### User Story 3 - Handle Invalid UTF-8 Sequences Gracefully (Priority: P2)

As a Clysm compiler developer, I want invalid UTF-8 sequences to signal a `decoding-error` condition so that encoding errors are detected early and can be handled appropriately.

**Why this priority**: Error handling is essential for robustness, especially when processing external data.

**Independent Test**: Can be tested by providing malformed UTF-8 byte sequences and verifying the correct condition is signaled.

**Acceptance Scenarios**:

1. **Given** an invalid lead byte `#(255 0)`, **When** `utf8-octets-to-string` is called, **Then** a `decoding-error` condition is signaled
2. **Given** a truncated 2-byte sequence `#(194)` (missing continuation), **When** `utf8-octets-to-string` is called, **Then** a `decoding-error` condition is signaled
3. **Given** an unexpected continuation byte `#(128)` at start, **When** `utf8-octets-to-string` is called, **Then** a `decoding-error` condition is signaled
4. **Given** overlong encoding `#(192 128)` (should be single byte), **When** `utf8-octets-to-string` is called, **Then** a `decoding-error` condition is signaled

---

### User Story 4 - Replace All Babel Usages in Compiler (Priority: P1)

As a Clysm project maintainer, I want all 6 `babel:string-to-octets` calls in the compiler replaced with the portable implementation so that the Babel dependency can be removed and the compiler works across SBCL, CCL, and ECL.

**Why this priority**: This is the primary goal - eliminating the Babel dependency for cross-implementation portability.

**Independent Test**: Can be verified by running `grep -r 'babel:' src/` and confirming zero matches, then running the existing test suite to ensure no regressions.

**Acceptance Scenarios**:

1. **Given** the current codebase with 6 Babel usages, **When** the migration is complete, **Then** `grep -r 'babel:' src/` returns no matches
2. **Given** the replaced implementation, **When** the compiler test suite runs, **Then** all existing tests pass
3. **Given** the replaced implementation, **When** Wasm binaries are generated, **Then** they validate correctly with `wasm-tools validate`

---

### Edge Cases

- What happens when a string contains surrogate pairs (U+D800-U+DFFF)? These are invalid in UTF-8 and should signal a `decoding-error` when encountered in byte sequences. Encoding from strings should never produce surrogates as Common Lisp uses Unicode code points directly.
- What happens with the maximum valid code point (U+10FFFF)? Should encode to 4 bytes correctly.
- What happens with code points above U+10FFFF? These are invalid and should be rejected.
- How does system handle very long strings (100KB+)? Should encode efficiently without stack overflow.
- What happens with NUL characters (U+0000)? Should encode as single byte `#(0)` per UTF-8 specification.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a `string-to-utf8-octets` function that converts a Common Lisp string to a vector of unsigned bytes representing UTF-8 encoding
- **FR-002**: System MUST provide a `utf8-octets-to-string` function that converts a vector of unsigned bytes (UTF-8) back to a Common Lisp string
- **FR-003**: Both functions MUST handle the full Unicode range (U+0000 to U+10FFFF) including 4-byte sequences
- **FR-004**: The `utf8-octets-to-string` function MUST signal a `decoding-error` condition when encountering invalid UTF-8 sequences
- **FR-005**: Invalid UTF-8 sequences include: invalid lead bytes (0xC0-0xC1, 0xF5-0xFF), missing/extra continuation bytes, overlong encodings, and surrogate code points (U+D800-U+DFFF)
- **FR-006**: Both functions MUST be implemented using only portable Common Lisp (no SBCL-specific internals)
- **FR-007**: The implementation MUST be placed in `src/clysm/lib/utf8.lisp`
- **FR-008**: All 6 Babel call sites in the compiler MUST be replaced with the portable implementation
- **FR-009**: The `decoding-error` condition MUST include the byte position and the invalid byte sequence in its report

### Key Entities

- **UTF-8 Octet Vector**: A simple-vector or array of `(unsigned-byte 8)` elements representing encoded bytes
- **decoding-error**: A condition type (subtype of `error`) signaled when UTF-8 decoding fails, containing position and invalid bytes information

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Running `grep -r 'babel:' src/` returns zero matches after migration
- **SC-002**: All existing compiler tests pass without modification after migration
- **SC-003**: The encoding function correctly handles all Unicode code points from U+0000 to U+10FFFF
- **SC-004**: The decoding function detects and signals errors for all categories of invalid UTF-8 sequences
- **SC-005**: The implementation works correctly on SBCL, CCL, and ECL (portable Common Lisp only)
- **SC-006**: Performance is comparable to Babel for typical compiler workloads (string lengths under 1000 characters)

## Assumptions

- The Babel library's UTF-8 encoding behavior is the reference standard for the replacement implementation
- The existing test suite provides sufficient coverage to detect regressions from the migration
- Only `babel:string-to-octets` is currently used (6 call sites); `babel:octets-to-string` is provided for completeness and future use
- The `decoding-error` condition will be defined in the clysm/conditions package or co-located with the UTF-8 implementation
- No other Babel functionality (other encodings like Latin-1, UTF-16) is required
