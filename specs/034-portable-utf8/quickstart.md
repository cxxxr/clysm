# Quickstart: Portable UTF-8 Encoding

**Feature**: 034-portable-utf8
**Date**: 2025-12-27

## Overview

This feature implements portable UTF-8 encoding/decoding functions to replace the Babel library dependency, enabling the Clysm compiler to run on SBCL, CCL, and ECL.

## API Reference

### string-to-utf8-octets

```lisp
(string-to-utf8-octets string) => octets
```

Converts a Common Lisp string to a UTF-8 encoded byte vector.

**Parameters**:
- `string` - A Common Lisp string

**Returns**:
- `octets` - A `(simple-array (unsigned-byte 8) (*))` containing the UTF-8 encoded bytes

**Examples**:
```lisp
;; ASCII
(string-to-utf8-octets "hello")
;; => #(104 101 108 108 111)

;; Japanese
(string-to-utf8-octets "日本語")
;; => #(230 151 165 230 156 172 232 170 158)

;; Emoji (4-byte UTF-8)
(string-to-utf8-octets "🎉")
;; => #(240 159 142 137)

;; Empty string
(string-to-utf8-octets "")
;; => #()
```

### utf8-octets-to-string

```lisp
(utf8-octets-to-string octets) => string
```

Converts a UTF-8 encoded byte vector to a Common Lisp string.

**Parameters**:
- `octets` - A vector of `(unsigned-byte 8)` containing UTF-8 encoded bytes

**Returns**:
- `string` - A Common Lisp string

**Signals**:
- `decoding-error` - If the input contains invalid UTF-8 sequences

**Examples**:
```lisp
;; ASCII
(utf8-octets-to-string #(104 101 108 108 111))
;; => "hello"

;; Round-trip
(utf8-octets-to-string (string-to-utf8-octets "日本語"))
;; => "日本語"

;; Invalid UTF-8 handling
(handler-case
    (utf8-octets-to-string #(255 0))
  (decoding-error (e)
    (format nil "Error at position ~D" (decoding-error-position e))))
;; => "Error at position 0"
```

### decoding-error Condition

```lisp
(define-condition decoding-error (error)
  ((position :initarg :position :reader decoding-error-position)
   (invalid-bytes :initarg :invalid-bytes :reader decoding-error-invalid-bytes)))
```

Signaled when `utf8-octets-to-string` encounters invalid UTF-8.

**Slots**:
- `position` - Byte offset where the error was detected
- `invalid-bytes` - The invalid byte sequence

## Migration Guide

### Before (with Babel)

```lisp
(babel:string-to-octets name :encoding :utf-8)
```

### After (portable)

```lisp
(string-to-utf8-octets name)
```

### Search and Replace Pattern

```bash
# Find all usages
grep -rn 'babel:string-to-octets' src/

# The replacement pattern
# FROM: (babel:string-to-octets EXPR :encoding :utf-8)
# TO:   (string-to-utf8-octets EXPR)
```

## Testing

### Run UTF-8 Tests

```bash
# Enter Nix development environment
nix develop

# Run unit tests
sbcl --load tests/main.lisp --eval '(rove:run :clysm/tests.utf8)'

# Run all tests to verify migration
sbcl --load tests/main.lisp --eval '(rove:run :clysm/tests)'
```

### Verify Migration Complete

```bash
# Should return no matches
grep -r 'babel:' src/
echo $?  # Should be 1 (no matches)
```

### Verify Wasm Output

```bash
# Generate a Wasm module with strings
sbcl --load src/clysm/clysm.lisp --eval '(clysm:compile-to-file "(defun hello () \"Hello, 世界!\")" "test.wasm")'

# Validate the output
wasm-tools validate test.wasm
```

## UTF-8 Byte Patterns Reference

| Code Point Range | Byte 1 | Byte 2 | Byte 3 | Byte 4 |
|------------------|--------|--------|--------|--------|
| U+0000 - U+007F | 0xxxxxxx | - | - | - |
| U+0080 - U+07FF | 110xxxxx | 10xxxxxx | - | - |
| U+0800 - U+FFFF | 1110xxxx | 10xxxxxx | 10xxxxxx | - |
| U+10000 - U+10FFFF | 11110xxx | 10xxxxxx | 10xxxxxx | 10xxxxxx |

## Error Categories

| Error | Cause | Example Bytes |
|-------|-------|---------------|
| Invalid lead byte | 0xC0, 0xC1, 0xF5-0xFF | `#(255 0)` |
| Unexpected continuation | 0x80-0xBF at start | `#(128)` |
| Missing continuation | Non-continuation after lead | `#(194)` (truncated) |
| Overlong encoding | Too many bytes for value | `#(192 128)` for NUL |
| Surrogate code point | U+D800-U+DFFF decoded | `#(237 160 128)` |
