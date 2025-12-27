# Research: Portable UTF-8 Encoding

**Feature**: 034-portable-utf8
**Date**: 2025-12-27

## UTF-8 Encoding Algorithm

### Decision: Standard UTF-8 encoding using `char-code`

**Rationale**: Common Lisp's `char-code` returns the Unicode code point of a character, which is the standard input for UTF-8 encoding. All target implementations (SBCL, CCL, ECL) use Unicode code points.

**Algorithm**:
```text
For each character in string:
  code-point = (char-code char)
  if code-point < #x80:           ; 1-byte (ASCII)
    emit code-point
  else if code-point < #x800:     ; 2-byte
    emit (logior #xC0 (ash code-point -6))
    emit (logior #x80 (logand code-point #x3F))
  else if code-point < #x10000:   ; 3-byte
    emit (logior #xE0 (ash code-point -12))
    emit (logior #x80 (logand (ash code-point -6) #x3F))
    emit (logior #x80 (logand code-point #x3F))
  else:                            ; 4-byte (code-point < #x110000)
    emit (logior #xF0 (ash code-point -18))
    emit (logior #x80 (logand (ash code-point -12) #x3F))
    emit (logior #x80 (logand (ash code-point -6) #x3F))
    emit (logior #x80 (logand code-point #x3F))
```

**Alternatives considered**:
- SBCL-specific `sb-ext:string-to-octets`: Not portable to CCL/ECL
- FFI to libiconv: Adds native dependency, violates portability goal
- Flexi-streams library: Another dependency; goal is to reduce dependencies

## UTF-8 Decoding Algorithm

### Decision: State-machine based decoder with validation

**Rationale**: UTF-8 decoding requires careful validation to detect all error categories. A state-machine approach handles all edge cases cleanly.

**Validation Rules**:
| Error Category | Detection |
|----------------|-----------|
| Invalid lead byte (0xC0-0xC1) | Overlong 2-byte for ASCII |
| Invalid lead byte (0xF5-0xFF) | Code points > U+10FFFF |
| Missing continuation | Expected 10xxxxxx, got other |
| Unexpected continuation | 10xxxxxx at sequence start |
| Overlong encoding | Minimum bytes not used |
| Surrogate code point | U+D800-U+DFFF decoded |

**Algorithm**:
```text
while bytes remain:
  lead = next byte
  if lead < #x80:                 ; 1-byte
    code-point = lead
  else if lead < #xC0:            ; Unexpected continuation
    signal decoding-error
  else if lead < #xC2:            ; Overlong (0xC0, 0xC1)
    signal decoding-error
  else if lead < #xE0:            ; 2-byte
    code-point = decode-2-byte(lead, next)
  else if lead < #xF0:            ; 3-byte
    code-point = decode-3-byte(lead, next, next)
    if #xD800 <= code-point <= #xDFFF:  ; Surrogate
      signal decoding-error
  else if lead < #xF5:            ; 4-byte
    code-point = decode-4-byte(lead, next, next, next)
  else:                           ; Invalid lead (0xF5-0xFF)
    signal decoding-error

  validate-not-overlong(code-point, byte-count)
  emit (code-char code-point)
```

**Alternatives considered**:
- Permissive decoding (replace errors with U+FFFD): Spec requires signaling error
- Babel-compatible error handling: Babel signals `babel-encodings:character-decoding-error`; we use our own `decoding-error`

## Condition Type Design

### Decision: `decoding-error` as subtype of `error`

**Rationale**: Follows existing clysm condition patterns. The condition should provide enough context for debugging.

**Condition Slots**:
- `position` (integer): Byte offset where error occurred
- `invalid-bytes` (vector): The problematic byte sequence (1-4 bytes)
- `expected` (string, optional): Description of what was expected

**Alternatives considered**:
- Subtype of `parse-error`: Not applicable; UTF-8 is not parsing in the traditional sense
- Subtype of `stream-error`: No stream involved in our API

## Return Type Design

### Decision: `(simple-array (unsigned-byte 8) (*))`

**Rationale**: Matches Babel's return type. A specialized array allows efficient byte-level operations and is compatible with existing code that expects byte vectors.

**API Signature**:
```lisp
(defun string-to-utf8-octets (string)
  "Convert STRING to a UTF-8 encoded byte vector.
   Returns (simple-array (unsigned-byte 8) (*))."
  ...)

(defun utf8-octets-to-string (octets)
  "Convert UTF-8 encoded OCTETS to a string.
   Signals DECODING-ERROR if OCTETS contains invalid UTF-8."
  ...)
```

**Alternatives considered**:
- Keyword `:encoding` parameter: Unnecessary since we only support UTF-8
- `:start`/`:end` parameters: Not used by any call site; YAGNI

## Performance Considerations

### Decision: Pre-calculate output size for encoding

**Rationale**: Avoid multiple array resizing operations by calculating the exact output size first.

**Strategy**:
```text
Phase 1: Calculate total byte count
  For each char: add 1/2/3/4 based on code-point range
Phase 2: Allocate result array with exact size
Phase 3: Fill array (no resizing needed)
```

**Alternatives considered**:
- Adjustable array with `vector-push-extend`: Simpler but slower due to resizing
- Over-allocate 4x then shrink: Wastes memory for ASCII-heavy strings

## Package Integration

### Decision: Define in clysm package, export functions

**Rationale**: Functions are used by core compiler code which is in clysm package. Defining a separate package adds unnecessary complexity.

**Export list additions**:
- `string-to-utf8-octets`
- `utf8-octets-to-string`
- `decoding-error` (condition type)
- `decoding-error-position` (accessor)
- `decoding-error-invalid-bytes` (accessor)

**Alternatives considered**:
- Separate `clysm.utf8` package: Over-engineering for 2 functions
- Internal only (no export): utf8-octets-to-string may be useful for future features
