# Feature Specification: Fix FFI Streams Module

**Feature Branch**: `018-fix-ffi-streams`
**Created**: 2025-12-24
**Status**: Draft
**Input**: User description: "Phase 8D代替を完成させる: 015-ffi-stream-ioで実装されたstreamsモジュール（format, write-char, read-char等）が'pre-existing issues'で無効化されている。この問題を特定・修正し、以下を達成する: 1) streamsモジュールの有効化 2) 全テストスイートのパス 3) ANSI CL準拠のformat/write/read動作確認。既存のFFI基盤(012)と条件システム(014)を活用する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Re-enable Streams Module (Priority: P1)

As a clysm developer, I want the streams module (implemented in 015-ffi-stream-io) to be enabled and functional so that compiled programs can perform I/O operations.

**Why this priority**: The streams module is currently disabled in the ASDF system definition (clysm.asd lines 115-125). Without enabling it, no I/O functionality is available to compiled programs. This is the foundational step for all other scenarios.

**Independent Test**: Can be fully tested by loading the clysm system and verifying that the streams module loads without errors and exports the expected symbols.

**Acceptance Scenarios**:

1. **Given** the clysm system definition with streams module commented out, **When** the module is uncommented and loaded, **Then** the system loads successfully without compilation errors.
2. **Given** a loaded clysm system with streams enabled, **When** checking exported symbols, **Then** all documented stream functions (write-char, write-string, read-char, read-line, format, streamp) are available.
3. **Given** a loaded clysm system with streams enabled, **When** accessing *standard-input*, *standard-output*, *error-output*, **Then** all three standard streams are initialized as stream objects.

---

### User Story 2 - Fix Pre-existing Issues (Priority: P1)

As a clysm developer, I want all pre-existing issues that caused the streams module to be disabled to be identified and fixed so that the module operates correctly.

**Why this priority**: The streams module was disabled due to unspecified "pre-existing issues". These issues must be identified through investigation (compilation errors, test failures, runtime errors) and resolved before the module can function correctly.

**Independent Test**: Can be fully tested by running the full test suite and verifying zero failures related to stream functionality.

**Acceptance Scenarios**:

1. **Given** streams module re-enabled in ASDF, **When** compiling the system, **Then** no compilation errors or warnings related to streams module occur.
2. **Given** streams module enabled with tests re-enabled, **When** running `(asdf:test-system :clysm)`, **Then** all stream-related tests pass.
3. **Given** any identified pre-existing issues, **When** fixes are applied, **Then** the root cause is documented in commit messages.

---

### User Story 3 - Pass All Test Suites (Priority: P2)

As a clysm developer, I want all test suites to pass after streams module is enabled so that I have confidence the fix is complete and no regressions were introduced.

**Why this priority**: Passing all tests verifies the fix is complete. Stream tests (currently commented out in unit tests and streams test module) must be re-enabled and pass alongside existing tests.

**Independent Test**: Can be fully tested by running `(asdf:test-system :clysm)` and verifying 100% pass rate.

**Acceptance Scenarios**:

1. **Given** streams module and all stream tests enabled, **When** running the full test suite, **Then** all tests pass (including stream-types-test, stream-write-test, stream-read-test, stream-format-test).
2. **Given** the full test suite, **When** running tests, **Then** no existing tests regress (tests that passed before the fix continue to pass).
3. **Given** edge case tests for I/O errors, **When** running tests, **Then** type-error conditions are correctly signaled for invalid inputs.

---

### User Story 4 - ANSI CL Compliance Verification (Priority: P2)

As a clysm developer, I want format, write, and read functions to behave according to ANSI Common Lisp specification so that programs relying on standard behavior work correctly.

**Why this priority**: ANSI compliance ensures predictable behavior and compatibility with existing Common Lisp code. This verifies the implementation matches the specification.

**Independent Test**: Can be fully tested by verifying format directive output matches ANSI CL specification examples.

**Acceptance Scenarios**:

1. **Given** `(format nil "~A" "test")`, **When** executed, **Then** returns "test" (aesthetic printing without quotes).
2. **Given** `(format nil "~S" "test")`, **When** executed, **Then** returns "\"test\"" (standard printing with quotes).
3. **Given** `(format nil "~D" 42)`, **When** executed, **Then** returns "42" (decimal integer printing).
4. **Given** `(format nil "~%")`, **When** executed, **Then** returns a string containing newline character.
5. **Given** `(format nil "~~")`, **When** executed, **Then** returns "~" (literal tilde).
6. **Given** `(write-char #\A)`, **When** executed, **Then** character A is output to standard-output.
7. **Given** `(streamp *standard-output*)`, **When** executed, **Then** returns T.

---

### User Story 5 - FFI Integration Verification (Priority: P3)

As a clysm developer, I want stream I/O operations to correctly use the FFI foundation (012-ffi-foundation) so that I/O calls reach the host environment properly.

**Why this priority**: Streams module depends on FFI for host I/O. Verifying FFI integration confirms the architectural design is correctly implemented.

**Independent Test**: Can be fully tested by compiling a program that writes to stdout and verifying output appears in host environment.

**Acceptance Scenarios**:

1. **Given** a compiled clysm program calling write-char, **When** run on wasmtime, **Then** the character appears in host stdout.
2. **Given** FFI imports defined in ffi-io.lisp, **When** streams module loads, **Then** define-foreign-function macro correctly generates import specifications.
3. **Given** a stream operation with incorrect argument type, **When** executed, **Then** type-error condition is signaled using the condition system (014-condition-system).

---

### Edge Cases

- What happens when write-char receives a non-character? System signals type-error.
- What happens when write-string receives a non-string? System signals type-error.
- What happens when format receives unknown directive? System signals error during format string parsing.
- What happens when format has too few arguments for directives? System signals error.
- How does the system handle circular references in princ-to-string/prin1-to-string? Implementation may recurse; reasonable behavior expected (either prints or signals error).

## Requirements *(mandatory)*

### Functional Requirements

**Module Enablement**:

- **FR-001**: Streams module MUST be enabled in clysm.asd by uncommenting lines 115-125.
- **FR-002**: Stream tests MUST be enabled in clysm.asd by uncommenting stream test components.
- **FR-003**: System MUST compile without errors after streams module is enabled.

**Issue Resolution**:

- **FR-004**: All pre-existing issues causing module disable MUST be identified and documented.
- **FR-005**: All identified issues MUST be fixed with appropriate code changes.
- **FR-006**: Fixes MUST NOT introduce regressions in existing functionality.

**Test Suite**:

- **FR-007**: All unit tests in stream-types-test, stream-write-test, stream-read-test, stream-format-test MUST pass.
- **FR-008**: All integration tests from tests/streams/stream-test.lisp MUST pass.
- **FR-009**: Full test suite `(asdf:test-system :clysm)` MUST pass with zero failures.

**ANSI CL Compliance**:

- **FR-010**: format function MUST support ~A directive (aesthetic printing).
- **FR-011**: format function MUST support ~S directive (standard printing).
- **FR-012**: format function MUST support ~D directive (decimal integer printing).
- **FR-013**: format function MUST support ~% directive (newline output).
- **FR-014**: format function MUST support ~~ directive (literal tilde).
- **FR-015**: write-char MUST output single character to stream.
- **FR-016**: write-string MUST output string to stream.
- **FR-017**: read-char MUST read single character from stream.
- **FR-018**: read-line MUST read line from stream.

**Integration Requirements**:

- **FR-019**: Stream operations MUST use FFI foundation (012-ffi-foundation) for host I/O calls.
- **FR-020**: Error conditions MUST use condition system (014-condition-system) for signaling.
- **FR-021**: Standard streams (*standard-input*, *standard-output*, *error-output*) MUST be properly initialized.

### Key Entities

- **Streams Module**: The clysm/streams package containing all stream I/O functionality (files: package.lisp, types.lisp, ffi-io.lisp, write.lisp, read.lisp, format.lisp).
- **Pre-existing Issues**: Unspecified problems that caused the module to be disabled; must be identified through investigation.
- **Stream Tests**: Test files in tests/unit/ (stream-*-test.lisp) and tests/streams/ (stream-test.lisp).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Streams module is uncommented and loads successfully in clysm.asd.
- **SC-002**: Full test suite passes with 100% success rate for all enabled tests.
- **SC-003**: All five format directives (~A, ~S, ~D, ~%, ~~) produce ANSI CL compliant output.
- **SC-004**: write-char and write-string functions work correctly with character and string arguments.
- **SC-005**: Standard stream variables (*standard-input*, *standard-output*, *error-output*) are accessible and return valid stream objects.
- **SC-006**: Type errors are properly signaled for invalid argument types to I/O functions.
- **SC-007**: Pre-existing issues are documented in commit messages with root cause analysis.

## Assumptions

- The streams module implementation (015-ffi-stream-io) is fundamentally correct and issues are fixable with targeted changes.
- Pre-existing issues are likely related to: package conflicts, missing dependencies, incorrect FFI macro expansion, or test setup issues.
- The FFI foundation (012-ffi-foundation) is complete and functioning correctly.
- The condition system (014-condition-system) is complete and can be used for error signaling.
- Investigation will reveal the specific issues through compilation attempts and test execution.

## Dependencies

- **012-ffi-foundation**: Required for FFI call mechanism to host I/O functions.
- **014-condition-system**: Required for signaling type-error and stream-error conditions.
- **002-special-vars-compiler**: Required for dynamic special variable binding (*standard-input*, etc.).
- **008-character-string**: Required for character and string data types.
