# Data Model: Portable UTF-8 Encoding

**Feature**: 034-portable-utf8
**Date**: 2025-12-27

## Entities

### UTF-8 Octet Vector

A sequence of bytes representing UTF-8 encoded text.

**Type**: `(simple-array (unsigned-byte 8) (*))`

**Constraints**:
- Each element is an unsigned byte (0-255)
- Valid UTF-8 sequences only (for encoding output)
- May contain any bytes (for decoding input; validation occurs during decode)

**Examples**:
```lisp
;; ASCII "hello"
#(104 101 108 108 111)

;; Japanese "日本語" (3 characters × 3 bytes each)
#(230 151 165 230 156 172 232 170 158)

;; Emoji "🎉" (U+1F389, 4 bytes)
#(240 159 142 137)
```

### decoding-error Condition

Signaled when `utf8-octets-to-string` encounters invalid UTF-8 sequences.

**Superclass**: `error`

**Slots**:

| Slot | Type | Description |
|------|------|-------------|
| `position` | `(integer 0 *)` | Byte offset where error was detected |
| `invalid-bytes` | `(simple-array (unsigned-byte 8) (*))` | The problematic byte sequence (1-4 bytes) |

**Accessors**:
- `decoding-error-position` (condition) → integer
- `decoding-error-invalid-bytes` (condition) → vector

**Report Format**:
```text
Invalid UTF-8 sequence at position N: #(byte1 byte2 ...)
```

## State Transitions

N/A - Both functions are pure transformations with no state.

## Relationships

```text
┌──────────────────┐     string-to-utf8-octets     ┌─────────────────────┐
│  Common Lisp     │ ─────────────────────────────>│  UTF-8 Octet Vector │
│  String          │                               │  (byte array)       │
└──────────────────┘                               └─────────────────────┘
        ^                                                   │
        │                                                   │
        │           utf8-octets-to-string                   │
        └───────────────────────────────────────────────────┘
                           │
                           │ (on invalid input)
                           v
                  ┌─────────────────┐
                  │ decoding-error  │
                  │ condition       │
                  └─────────────────┘
```

## Validation Rules

### Encoding (string-to-utf8-octets)

No validation needed - Common Lisp strings contain valid Unicode code points by definition.

### Decoding (utf8-octets-to-string)

| Rule | Invalid Input | Error |
|------|---------------|-------|
| R1 | Lead byte 0xC0 or 0xC1 | Overlong encoding (should be 1-byte ASCII) |
| R2 | Lead byte 0xF5-0xFF | Code point would exceed U+10FFFF |
| R3 | Continuation byte (0x80-0xBF) at start | Unexpected continuation |
| R4 | Non-continuation after multi-byte lead | Missing continuation |
| R5 | Decoded code point U+D800-U+DFFF | Invalid surrogate code point |
| R6 | 3-byte encoding for < U+0800 | Overlong encoding |
| R7 | 4-byte encoding for < U+10000 | Overlong encoding |
| R8 | Truncated sequence at end | Incomplete multi-byte sequence |

## Integration Points

### Call Sites to Modify

| File | Line | Current Usage | Replacement |
|------|------|---------------|-------------|
| `compiler.lisp` | 545 | `(babel:string-to-octets name :encoding :utf-8)` | `(string-to-utf8-octets name)` |
| `func-section.lisp` | 418 | `(babel:string-to-octets string :encoding :utf-8)` | `(string-to-utf8-octets string)` |
| `import-gen.lisp` | 89 | `(babel:string-to-octets (wi-module-name import) :encoding :utf-8)` | `(string-to-utf8-octets (wi-module-name import))` |
| `import-gen.lisp` | 93 | `(babel:string-to-octets (wi-field-name import) :encoding :utf-8)` | `(string-to-utf8-octets (wi-field-name import))` |
| `export-gen.lisp` | 51 | `(babel:string-to-octets string :encoding :utf-8)` | `(string-to-utf8-octets string)` |
| `sections.lisp` | 74 | `(babel:string-to-octets name :encoding :utf-8)` | `(string-to-utf8-octets name)` |
