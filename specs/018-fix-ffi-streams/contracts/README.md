# Contracts: Fix FFI Streams Module

**Feature**: 018-fix-ffi-streams

## Status: Not Applicable

This is a **fix task** for an existing implementation. No new API contracts are being designed.

The original API contracts were defined in 015-ffi-stream-io and remain unchanged:

### Existing API (clysm/streams package)

| Function | Signature | Description |
|----------|-----------|-------------|
| write-char | (char &optional stream) → char | Write character to stream |
| write-string | (string &optional stream &key start end) → string | Write string to stream |
| read-char | (&optional stream eof-error-p eof-value recursive-p) → char | Read character from stream |
| read-line | (&optional stream eof-error-p eof-value recursive-p) → (values string boolean) | Read line from stream |
| format | (destination control-string &rest args) → string-or-nil | Formatted output |
| streamp | (object) → boolean | Stream type predicate |
| input-stream-p | (stream) → boolean | Input capability predicate |
| output-stream-p | (stream) → boolean | Output capability predicate |

### FFI Imports (host functions)

| Import | Module | Parameters | Returns |
|--------|--------|------------|---------|
| %host-write-char | clysm:io | (fd: i32, codepoint: i32) | void |
| %host-write-string | clysm:io | (fd: i32, string: anyref) | void |
| %host-read-char | clysm:io | (fd: i32) | i32 (-1 for EOF) |
| %host-read-line | clysm:io | (fd: i32) | anyref (null for EOF) |

See `src/clysm/streams/ffi-io.lisp` for FFI definitions.
