# Research: FFI-based Stream I/O

**Date**: 2025-12-24
**Branch**: `015-ffi-stream-io`

## Overview

Research findings for implementing stream I/O via FFI without linear memory.

---

## 1. WasmGC String Passing to Host Functions

### Decision: Use externref with WasmGC string arrays

**Rationale**: WasmGC strings are arrays of i8 (`(array (mut i8))`). To pass these to host functions, we use `externref` which allows the host to receive an opaque reference to the GC-managed array.

**Approach**:
1. Wasm module exports `$string` type as WasmGC array
2. FFI call passes `(ref $string)` as `externref` to host
3. Host accesses array elements via WebAssembly GC API
4. For wasmtime with JS shim, the shim converts the GC array to a JS string

**Alternatives Considered**:
- Linear memory + pointer: Rejected (violates FR-019, constitution principle I)
- Component Model strings: Deferred (WASI Preview 2 not yet stable for all hosts)

---

## 2. Host Shim Architecture (wasmtime + JavaScript)

### Decision: JavaScript host shim with fd-based I/O

**Rationale**: wasmtime supports WASI and custom imports. A JavaScript host shim provides the most flexibility for testing and allows direct integration with Node.js stdio.

**Shim API Design**:
```javascript
// Host imports provided to Wasm module
const hostImports = {
  "clysm:io": {
    // Write a single character (Unicode codepoint) to fd
    "write-char": (fd, codepoint) => { /* ... */ },

    // Write a WasmGC string array to fd
    "write-string": (fd, stringRef) => { /* ... */ },

    // Read a single character from fd, returns codepoint or -1 for EOF
    "read-char": (fd) => { /* ... */ },

    // Read a line from fd, returns WasmGC string ref or null for EOF
    "read-line": (fd) => { /* ... */ }
  }
};
```

**File Descriptor Convention**:
- fd 0: stdin (`*standard-input*`)
- fd 1: stdout (`*standard-output*`)
- fd 2: stderr (`*error-output*`)

**Alternatives Considered**:
- WASI wasi_snapshot_preview1: More standard but doesn't expose GC-friendly APIs
- Pure Rust host: Less flexible for rapid iteration; may use for production

---

## 3. ANSI Common Lisp EOF Handling

### Decision: ANSI CL compliant with eof-error-p and eof-value parameters

**Rationale**: Following the ANSI Common Lisp specification provides predictable behavior and compatibility with existing Lisp code.

**Signature**:
```lisp
(read-char &optional input-stream eof-error-p eof-value recursive-p)
(read-line &optional input-stream eof-error-p eof-value recursive-p)
```

**Behavior**:
- When `eof-error-p` is true (default): Signal `end-of-file` condition on EOF
- When `eof-error-p` is nil: Return `eof-value` on EOF
- `recursive-p` is ignored for this implementation (no reader integration)

**Implementation Notes**:
- Host `read-char` returns -1 for EOF; Wasm wrapper checks and handles
- Host `read-line` returns null for EOF; Wasm wrapper checks and handles
- `end-of-file` is a stream-error subclass (from 014-condition-system)

---

## 4. Format Function Implementation

### Decision: Compile-time format string parsing with runtime dispatch

**Rationale**: Parsing format strings at compile time enables early error detection and avoids runtime string parsing overhead.

**Architecture**:
1. `format` macro parses format string at compile time
2. Generates sequence of output calls for literal text and directive handlers
3. Each directive (`~A`, `~S`, `~D`, `~%`, `~~`) has a dedicated handler function

**Directive Implementations**:

| Directive | Handler | Description |
|-----------|---------|-------------|
| `~A` | `format-aesthetic` | `princ` output (no quotes for strings) |
| `~S` | `format-standard` | `prin1` output (readable, with quotes) |
| `~D` | `format-decimal` | Integer in decimal notation |
| `~%` | `format-newline` | Write newline character |
| `~~` | `format-tilde` | Write literal tilde |

**Destination Handling**:
- `t`: Output to `*standard-output*`, return NIL
- `nil`: Collect output to string, return string
- stream: Output to stream, return NIL

**Alternatives Considered**:
- Runtime format string parsing: Rejected (slower, no compile-time error checking)
- Full FORMAT implementation: Deferred (only basic directives for MVP)

---

## 5. Stream Type Design

### Decision: Minimal $stream struct with host handle and direction

**Rationale**: Streams are lightweight wrappers around host file descriptors. Keeping the struct minimal reduces GC pressure and simplifies FFI.

**WasmGC Type Definition**:
```wat
(type $stream (struct
  (field $fd i32)              ;; Host file descriptor
  (field $direction i32)       ;; 0=input, 1=output, 2=bidirectional
  (field $element-type anyref) ;; :character or :binary (future)
))
```

**Type Index**: 19 (following existing numeric tower types at 14-18)

**Direction Values**:
- 0: Input stream (read-only)
- 1: Output stream (write-only)
- 2: Bidirectional (unused for standard streams, future extension)

---

## 6. Special Variable Initialization

### Decision: Initialize standard streams during runtime startup

**Rationale**: Standard stream symbols must be bound before any user code executes.

**Initialization Sequence**:
1. Wasm module start function or export `_start`
2. Create $stream structs for fd 0, 1, 2
3. Bind `*standard-input*`, `*standard-output*`, `*error-output*` symbols

**Symbol Structure**:
- Uses existing $symbol type from gc-types.lisp
- Value slot holds stream reference
- Bound via shallow binding mechanism (002-special-vars-compiler)

---

## 7. Type Checking Strategy

### Decision: Runtime type checks with condition signaling

**Rationale**: I/O operations must validate argument types to provide clear error messages.

**Checks Required**:
- `write-char`: Argument must be character (i31ref with character tag or char struct)
- `write-string`: Argument must be string (ref $string)
- All: Stream argument must be stream (ref $stream)

**Error Signaling**:
- Use `type-error` condition from 014-condition-system
- Slot `datum`: The invalid argument
- Slot `expected-type`: `'character`, `'string`, or `'stream`

---

## Summary

| Topic | Decision | Rationale |
|-------|----------|-----------|
| String passing | externref + WasmGC array | Constitution compliant, no linear memory |
| Host shim | JavaScript with fd-based API | Flexible, Node.js compatible |
| EOF handling | ANSI CL compliant (eof-error-p) | Standard behavior, predictable API |
| Format | Compile-time parsing | Performance, early error detection |
| Stream type | Minimal struct (fd, direction) | Low overhead, simple FFI |
| Special vars | Runtime init at start | Symbols bound before user code |
| Type checking | Runtime with conditions | Clear errors, ANSI compliant |
