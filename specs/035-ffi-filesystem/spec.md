# Feature Specification: FFI Filesystem Access

**Feature Branch**: `035-ffi-filesystem`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 10C: ファイルシステムアクセスを実装する。目標はwasmtime/ブラウザ環境でのファイルI/O。FFI経由でopen-file, close-file, read-file-contents, write-file-contentsを実装し、with-open-fileマクロでunwind-protectによる安全なリソース管理を提供する。ホスト環境別バックエンド（wasmtime: WASI Preview2、ブラウザ: Virtual FS）に対応する統一インターフェースを設計する。"

## Background: Dependencies and Context

This feature builds upon:
- **Feature 027 (Complete FFI)**: Provides `define-foreign-function`, `export-function`, type marshalling, and error handling infrastructure
- **Feature 014 (Condition System)**: Provides `signal`, `error`, `handler-case`, and condition hierarchy
- **Feature 028 (Setf Macros)**: Provides `unwind-protect` for safe resource cleanup
- **Feature 008 (Character/String)**: Provides string representation for filenames and file contents

The host environments provide:
- **wasmtime**: WASI Preview2 filesystem API (fd_read, fd_write, path_open, etc.)
- **browser**: Virtual filesystem shim (IndexedDB, in-memory, or pre-loaded file tree)

## User Scenarios & Testing

### User Story 1 - Read File Contents (Priority: P1)

A Lisp programmer reads the entire contents of a file by specifying a filename and receives the contents as a string.

**Why this priority**: Reading files is the most fundamental I/O operation. Configuration loading, data processing, and any file-based workflow requires this capability.

**Independent Test**: Can be fully tested by creating a test file with known contents, calling `read-file-contents` with the filename, and verifying the returned string matches the file contents.

**Acceptance Scenarios**:

1. **Given** a file "test.txt" containing "Hello, World!", **When** calling `(read-file-contents "test.txt")`, **Then** the function returns the string "Hello, World!".

2. **Given** a file with UTF-8 encoded text including non-ASCII characters, **When** calling `read-file-contents`, **Then** the string correctly preserves all characters.

3. **Given** a non-existent file path, **When** calling `read-file-contents`, **Then** the system signals a `file-error` condition with the filename and reason.

---

### User Story 2 - Write File Contents (Priority: P1)

A Lisp programmer writes a string to a file, creating the file if it doesn't exist or overwriting if it does.

**Why this priority**: Writing files is equally fundamental as reading. Log output, generated code, processed data all require file writing.

**Independent Test**: Can be fully tested by writing a string to a file, then reading it back and verifying the contents match.

**Acceptance Scenarios**:

1. **Given** a string "Test output", **When** calling `(write-file-contents "output.txt" "Test output")`, **Then** the file is created with the specified contents.

2. **Given** an existing file "output.txt", **When** calling `write-file-contents` with new contents, **Then** the file is overwritten with the new contents.

3. **Given** a path in a non-existent directory, **When** calling `write-file-contents`, **Then** the system signals a `file-error` condition.

4. **Given** UTF-8 encoded content, **When** writing to file and reading back, **Then** all characters are preserved correctly.

---

### User Story 3 - Safe File Handling with with-open-file (Priority: P2)

A Lisp programmer uses the `with-open-file` macro to ensure files are properly closed even when errors occur during processing.

**Why this priority**: Resource leaks from unclosed files cause system instability. Safe resource management is essential for robust programs, though manual open/close covers basic cases.

**Independent Test**: Can be tested by using `with-open-file`, throwing an error within the body, and verifying the file is still properly closed afterward.

**Acceptance Scenarios**:

1. **Given** a file opened with `with-open-file`, **When** the body completes normally, **Then** the file is automatically closed.

2. **Given** a file opened with `with-open-file`, **When** an error is signaled in the body, **Then** the file is still closed via `unwind-protect` before the error propagates.

3. **Given** `(with-open-file (stream "file.txt" :direction :input) (read-line stream))`, **When** executed, **Then** the result is the first line of the file.

4. **Given** `(with-open-file (stream "out.txt" :direction :output) (write-line "test" stream))`, **When** executed, **Then** the file contains "test" followed by a newline.

---

### User Story 4 - Open and Close File Handles (Priority: P2)

A Lisp programmer explicitly opens a file, performs multiple operations, and closes it for fine-grained control over file lifetime.

**Why this priority**: While `with-open-file` is preferred, explicit handle management is needed for long-lived file resources and advanced I/O patterns.

**Independent Test**: Can be tested by opening a file, verifying the handle is valid, performing operations, closing the handle, and verifying operations on the closed handle signal an error.

**Acceptance Scenarios**:

1. **Given** a call `(open-file "test.txt" :direction :input)`, **When** the file exists, **Then** a valid file stream object is returned.

2. **Given** a valid file stream, **When** calling `(close-file stream)`, **Then** the stream is closed and subsequent operations signal an error.

3. **Given** a closed file stream, **When** attempting to read from it, **Then** the system signals a `file-error` indicating the stream is closed.

---

### User Story 5 - Cross-Platform File Access (Priority: P3)

A Lisp programmer writes file I/O code that works identically on wasmtime (WASI) and browser (Virtual FS) environments.

**Why this priority**: Portability is important for targeting multiple platforms, but many use cases target a single environment.

**Independent Test**: Can be tested by running the same file operations in both wasmtime and browser environments and verifying identical behavior.

**Acceptance Scenarios**:

1. **Given** file I/O code using the unified interface, **When** executed in wasmtime, **Then** it operates on the WASI filesystem.

2. **Given** the same file I/O code, **When** executed in browser, **Then** it operates on the virtual filesystem.

3. **Given** a file error, **When** it occurs in either environment, **Then** the same `file-error` condition type is signaled with environment-specific details.

---

### Edge Cases

- What happens when reading an empty file? `read-file-contents` returns an empty string `""`.
- What happens when writing an empty string to a file? The file is created/truncated to zero bytes.
- How does the system handle binary files? This specification covers text files only; binary I/O is deferred to a future feature.
- What happens with very large files? The system attempts to read/write the entire contents; memory limits may cause errors.
- How are relative paths resolved? Paths are passed directly to the host environment for resolution.
- What happens if a file is modified by another process during read? Behavior is undefined; the host environment handles consistency.

## Requirements

### Functional Requirements

- **FR-001**: System MUST provide `open-file` function to obtain a file stream handle from a filename.
- **FR-002**: System MUST provide `close-file` function to release a file stream handle.
- **FR-003**: System MUST provide `read-file-contents` function to read entire file contents as a string.
- **FR-004**: System MUST provide `write-file-contents` function to write a string to a file.
- **FR-005**: System MUST provide `with-open-file` macro that uses `unwind-protect` to ensure file closure.
- **FR-006**: System MUST signal `file-error` condition when file operations fail.
- **FR-007**: System MUST support `:direction` parameter with values `:input` and `:output` for `open-file` and `with-open-file`.
- **FR-008**: System MUST implement file operations via FFI to host environment.
- **FR-009**: System MUST provide identical interface for both wasmtime (WASI) and browser (Virtual FS) backends.
- **FR-010**: System MUST properly encode/decode UTF-8 when transferring text between Lisp strings and host filesystem.
- **FR-011**: System MUST reject operations on closed file streams with appropriate error.
- **FR-012**: System MUST support `:if-exists` parameter for output files with values `:supersede` (default) and `:error`.
- **FR-013**: System MUST support `:if-does-not-exist` parameter with values `:error` (default for input) and `:create` (default for output).

### Key Entities

- **File Stream**: Represents an open file handle with direction (input/output) and state (open/closed).
- **file-error**: Condition signaled for filesystem-related errors, containing filename and reason.
- **Host Backend**: The underlying filesystem implementation (WASI Preview2 or Virtual FS).

## Success Criteria

### Measurable Outcomes

- **SC-001**: File read operations correctly retrieve 100% of file contents for files up to 1MB.
- **SC-002**: File write operations correctly persist all written data as verified by subsequent reads.
- **SC-003**: `with-open-file` closes files in 100% of cases, including error scenarios.
- **SC-004**: All file operations work identically on both wasmtime and browser environments.
- **SC-005**: UTF-8 text with characters from multiple scripts (Latin, CJK, emoji) round-trips correctly.
- **SC-006**: Invalid file operations (non-existent file, closed stream) signal appropriate conditions.

## Assumptions

- Host environment (wasmtime/browser) provides filesystem API via FFI imports.
- WASI Preview2 is available in wasmtime for filesystem operations.
- Browser environment provides a JavaScript shim implementing the same FFI interface.
- Text files use UTF-8 encoding; no support for other encodings in this feature.
- The existing FFI infrastructure (Feature 027) is functional for import declarations.
- The condition system (Feature 014) is functional for signaling file errors.
- The `unwind-protect` special form (Feature 028) is functional for cleanup.
