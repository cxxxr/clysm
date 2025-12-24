# Contract: Host I/O FFI Interface

**Date**: 2025-12-24
**Branch**: `015-ffi-stream-io`

## Overview

Defines the FFI contract between compiled Wasm modules and the host environment for I/O operations.

---

## 1. Import Module

**Module Name**: `clysm:io`

All I/O functions are imported from this module.

---

## 2. Host Functions

### write-char

Writes a single Unicode character to a file descriptor.

**Signature**:
```wat
(import "clysm:io" "write-char" (func $host-write-char (param i32 i32)))
```

**Parameters**:
| Index | Type | Description |
|-------|------|-------------|
| 0 | i32 | File descriptor (0=stdin, 1=stdout, 2=stderr) |
| 1 | i32 | Unicode codepoint (0-1114111) |

**Returns**: None (void)

**Preconditions**:
- fd must be valid and writable (1 or 2 for standard streams)
- codepoint must be valid Unicode (0 ≤ cp ≤ 0x10FFFF, excluding surrogates)

**Postconditions**:
- Character is written to the host's output buffer
- May be buffered until newline or explicit flush

**Errors**:
- Invalid fd: Host implementation dependent (may trap or ignore)
- Invalid codepoint: Host should handle gracefully (replacement char or error)

---

### write-string

Writes a WasmGC string (array of i8) to a file descriptor.

**Signature**:
```wat
(import "clysm:io" "write-string" (func $host-write-string (param i32 externref)))
```

**Parameters**:
| Index | Type | Description |
|-------|------|-------------|
| 0 | i32 | File descriptor |
| 1 | externref | Reference to WasmGC string array (ref $string) |

**Returns**: None (void)

**Preconditions**:
- fd must be valid and writable
- externref must be a valid WasmGC string array or null

**Postconditions**:
- String bytes are written as UTF-8 to the host's output
- Null reference writes nothing (no error)

**Errors**:
- Invalid fd: Host implementation dependent
- Malformed UTF-8: Host should handle (replacement chars or error)

---

### read-char

Reads a single Unicode character from a file descriptor.

**Signature**:
```wat
(import "clysm:io" "read-char" (func $host-read-char (param i32) (result i32)))
```

**Parameters**:
| Index | Type | Description |
|-------|------|-------------|
| 0 | i32 | File descriptor (0 for stdin) |

**Returns**:
| Type | Description |
|------|-------------|
| i32 | Unicode codepoint, or -1 for EOF |

**Preconditions**:
- fd must be valid and readable (0 for standard input)

**Postconditions**:
- Returns next available Unicode codepoint
- Returns -1 when end of input reached
- Advances input position by consumed bytes

**Errors**:
- Invalid fd: Returns -1
- Malformed UTF-8 in input: Host implementation dependent

**EOF Convention**:
- Return value of -1 indicates end-of-file
- Lisp wrapper checks for -1 and handles eof-error-p/eof-value

---

### read-line

Reads a line of text (up to newline) from a file descriptor.

**Signature**:
```wat
(import "clysm:io" "read-line" (func $host-read-line (param i32) (result externref)))
```

**Parameters**:
| Index | Type | Description |
|-------|------|-------------|
| 0 | i32 | File descriptor (0 for stdin) |

**Returns**:
| Type | Description |
|------|-------------|
| externref | WasmGC string array, or null for EOF |

**Preconditions**:
- fd must be valid and readable

**Postconditions**:
- Returns string containing characters up to (but not including) newline
- Consumes the newline character from input
- Returns null when end of input reached with no data
- Returns partial line at EOF if data exists without trailing newline

**Errors**:
- Invalid fd: Returns null
- Memory allocation failure: Host implementation dependent

**EOF Convention**:
- null return indicates end-of-file
- Lisp wrapper checks for null and handles eof-error-p/eof-value

---

## 3. Host Implementation Requirements

### JavaScript/Node.js Shim

```javascript
// io-shim.js
module.exports = {
  "clysm:io": {
    "write-char": (fd, codepoint) => {
      const char = String.fromCodePoint(codepoint);
      if (fd === 1) process.stdout.write(char);
      else if (fd === 2) process.stderr.write(char);
    },

    "write-string": (fd, stringRef) => {
      // stringRef is a WebAssembly.Array (i8)
      if (stringRef === null) return;
      const bytes = new Uint8Array(stringRef.length);
      for (let i = 0; i < stringRef.length; i++) {
        bytes[i] = stringRef.get(i);
      }
      const text = new TextDecoder('utf-8').decode(bytes);
      if (fd === 1) process.stdout.write(text);
      else if (fd === 2) process.stderr.write(text);
    },

    "read-char": (fd) => {
      // Synchronous read not directly supported in Node.js
      // Implementation requires readline or async patterns
      // Return -1 for EOF
    },

    "read-line": (fd) => {
      // Returns WasmGC string array or null
      // Implementation requires GC array creation via module exports
    }
  }
};
```

### wasmtime CLI (WASI Alternative)

For wasmtime without JavaScript, use WASI fd_read/fd_write as underlying implementation, with a thin wrapper that handles WasmGC type conversion.

---

## 4. Type Mapping Summary

| Wasm Type | JavaScript Type | Description |
|-----------|-----------------|-------------|
| i32 (fd) | number | File descriptor |
| i32 (codepoint) | number | Unicode codepoint |
| i32 (-1) | number | EOF sentinel |
| externref (string) | WebAssembly.Array | GC-managed i8 array |
| externref (null) | null | EOF or empty |
