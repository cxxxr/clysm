# Feature Specification: FFI-based Stream I/O

**Feature Branch**: `015-ffi-stream-io`
**Created**: 2025-12-24
**Status**: Draft
**Input**: User description: "Phase 8D代替: FFI経由ストリームI/Oを実装する。目標は線形メモリを使用せず、FFI基盤経由でホスト環境のI/O機能を呼び出すこと。write-char, write-string, read-char, read-lineをFFI経由で実装し、format基本ディレクティブ（~A, ~S, ~D, ~%, ~~）をサポート。$stream WasmGC構造体をホストファイルハンドルに対応させ、*standard-input*, *standard-output*, *error-output*スペシャル変数を提供する。wasmtime + ホストJavaScript/WASI shimで動作確認を行う。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Character Output (Priority: P1)

As a developer using clysm3, I want to write characters and strings to standard output so that my compiled programs can display text to users.

**Why this priority**: Output is the most fundamental I/O operation. Without the ability to output text, programs cannot communicate results to users. This is the foundation for all other I/O features.

**Independent Test**: Can be fully tested by compiling and running a program that calls `write-char` and `write-string` and verifying the correct text appears in the host environment's output.

**Acceptance Scenarios**:

1. **Given** a compiled clysm3 program with `(write-char #\H *standard-output*)`, **When** the program runs in wasmtime, **Then** the character "H" appears in the host's standard output.
2. **Given** a compiled clysm3 program with `(write-string "Hello, World!")`, **When** the program runs, **Then** "Hello, World!" appears in the host's standard output.
3. **Given** a compiled clysm3 program that writes to `*error-output*`, **When** the program runs, **Then** the text appears in the host's standard error stream.

---

### User Story 2 - Basic Character Input (Priority: P2)

As a developer using clysm3, I want to read characters and lines from standard input so that my compiled programs can accept user input.

**Why this priority**: Input enables interactive programs. While output alone provides value (logging, one-way communication), input makes programs interactive and dramatically expands use cases.

**Independent Test**: Can be fully tested by compiling a program that calls `read-char` or `read-line` and verifying it correctly receives input from the host environment.

**Acceptance Scenarios**:

1. **Given** a compiled clysm3 program with `(read-char *standard-input*)`, **When** the user types "A" and presses enter, **Then** the function returns the character #\A.
2. **Given** a compiled clysm3 program with `(read-line *standard-input*)`, **When** the user types "Hello World" and presses enter, **Then** the function returns the string "Hello World".
3. **Given** a program reading from an empty input stream, **When** `read-char` is called, **Then** the function returns nil or signals end-of-file appropriately.

---

### User Story 3 - Format Function Support (Priority: P3)

As a developer using clysm3, I want to use the `format` function with basic directives so that I can easily create formatted output without manual string concatenation.

**Why this priority**: `format` is the standard Common Lisp way to produce formatted output. Supporting it makes the compiler significantly more practical for real-world use, but it builds upon basic I/O functionality.

**Independent Test**: Can be fully tested by compiling programs that use `format` with each supported directive and verifying the output matches expected formatted text.

**Acceptance Scenarios**:

1. **Given** a program with `(format t "~A" "test")`, **When** executed, **Then** "test" appears in standard output (aesthetic printing).
2. **Given** a program with `(format t "~S" "test")`, **When** executed, **Then** "\"test\"" appears (standard printing with quotes).
3. **Given** a program with `(format t "~D" 42)`, **When** executed, **Then** "42" appears (decimal printing).
4. **Given** a program with `(format t "~%")`, **When** executed, **Then** a newline character is output.
5. **Given** a program with `(format t "~~")`, **When** executed, **Then** a single tilde "~" appears.
6. **Given** a program with `(format nil "~A" "test")`, **When** executed, **Then** the string "test" is returned (no output to stream).

---

### User Story 4 - Stream Object Management (Priority: P4)

As a developer using clysm3, I want stream objects represented as first-class values so that I can pass streams to functions and store them in variables.

**Why this priority**: This enables more advanced I/O patterns like redirecting output, passing streams to functions, and building abstractions over I/O. Essential for building reusable libraries.

**Independent Test**: Can be fully tested by verifying that stream objects can be stored in variables, passed to functions, and used interchangeably with the standard streams.

**Acceptance Scenarios**:

1. **Given** a program binding `*standard-output*` to a variable, **When** that variable is passed to `write-string`, **Then** output appears correctly.
2. **Given** a program calling `(streamp *standard-output*)`, **When** executed, **Then** it returns true.
3. **Given** a program dynamically rebinding `*standard-output*`, **When** functions write to `*standard-output*` inside that binding, **Then** output goes to the rebound stream.

---

### Edge Cases

- What happens when writing a non-character value to `write-char`? System signals a type error.
- What happens when `write-string` receives a non-string? System signals a type error.
- How does `read-line` handle very long input lines? Lines up to reasonable host buffer limits are supported; extremely long lines may be truncated or signal an error.
- What happens when reading from a closed stream? System signals an error condition.
- How are Unicode characters handled? Full Unicode support via UTF-8 encoding at the FFI boundary.

## Requirements *(mandatory)*

### Functional Requirements

**Output Operations**:

- **FR-001**: System MUST provide `write-char` function that outputs a single character to a stream via FFI.
- **FR-002**: System MUST provide `write-string` function that outputs a string to a stream via FFI.
- **FR-003**: Both output functions MUST support optional stream argument (default: `*standard-output*`).

**Input Operations**:

- **FR-004**: System MUST provide `read-char` function that reads a single character from a stream via FFI.
- **FR-005**: System MUST provide `read-line` function that reads a line (up to newline) from a stream via FFI.
- **FR-006**: Both input functions MUST support optional stream argument (default: `*standard-input*`).
- **FR-007**: Input functions MUST handle end-of-file conditions appropriately.

**Format Function**:

- **FR-008**: System MUST provide `format` function supporting destination argument (t, nil, or stream).
- **FR-009**: `format` MUST support `~A` directive for aesthetic (human-readable) printing.
- **FR-010**: `format` MUST support `~S` directive for standard (machine-readable) printing.
- **FR-011**: `format` MUST support `~D` directive for decimal integer printing.
- **FR-012**: `format` MUST support `~%` directive for newline output.
- **FR-013**: `format` MUST support `~~` directive for literal tilde output.

**Stream Infrastructure**:

- **FR-014**: System MUST represent streams as WasmGC struct types that map to host file handles.
- **FR-015**: System MUST provide `*standard-input*` special variable bound to host stdin.
- **FR-016**: System MUST provide `*standard-output*` special variable bound to host stdout.
- **FR-017**: System MUST provide `*error-output*` special variable bound to host stderr.
- **FR-018**: Stream special variables MUST be dynamically rebindable.
- **FR-019**: System MUST NOT use WebAssembly linear memory for I/O data transfer.

**FFI Integration**:

- **FR-020**: All I/O operations MUST use the existing FFI foundation (from 012-ffi-foundation).
- **FR-021**: System MUST work with wasmtime runtime using host JavaScript/WASI shim.
- **FR-022**: Character encoding at FFI boundary MUST be UTF-8.

### Key Entities

- **Stream**: Represents an I/O channel; contains a host handle reference and stream direction (input/output/bidirectional).
- **Host Handle**: Opaque reference to host environment file descriptor (stdin=0, stdout=1, stderr=2 by convention).
- **Format Directive**: Control sequence in format string that determines how arguments are formatted.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Programs using `write-char` and `write-string` correctly output text visible in the host environment.
- **SC-002**: Programs using `read-char` and `read-line` correctly receive input from the host environment.
- **SC-003**: All five format directives (~A, ~S, ~D, ~%, ~~) produce correct output as defined by ANSI Common Lisp specification.
- **SC-004**: Standard stream special variables (`*standard-input*`, `*standard-output*`, `*error-output*`) are accessible and usable in compiled programs.
- **SC-005**: Dynamically rebinding stream variables correctly redirects I/O operations.
- **SC-006**: No WebAssembly linear memory is used for I/O data; all data passes through WasmGC types and FFI.
- **SC-007**: Compiled programs run successfully on wasmtime with the provided host shim.
- **SC-008**: Type errors are signaled when I/O functions receive arguments of incorrect type.

## Assumptions

- The FFI foundation (012-ffi-foundation) is complete and provides the necessary infrastructure for calling host functions.
- The condition system (014-condition-system) is available for signaling type errors and I/O errors.
- The host environment provides basic file descriptor I/O (read, write) that can be exposed via FFI.
- Unicode support follows UTF-8 encoding, which is the standard for WebAssembly string interchange.
- The wasmtime host shim will be provided as part of this feature's test infrastructure.

## Dependencies

- **012-ffi-foundation**: Required for FFI call mechanism to host functions.
- **014-condition-system**: Required for signaling I/O errors and type errors.
- **002-special-vars-compiler**: Required for implementing dynamic special variables.
- **008-character-string**: Required for character and string data types.
