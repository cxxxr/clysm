# Contract: Stream API

**Date**: 2025-12-24
**Branch**: `015-ffi-stream-io`

## Overview

Defines the Lisp-level API contract for stream I/O operations.

---

## 1. Output Functions

### write-char

Writes a character to a stream.

**Signature**:
```lisp
(write-char character &optional output-stream) => character
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| character | character | required | The character to write |
| output-stream | stream | *standard-output* | Target stream |

**Returns**: The character argument

**Preconditions**:
- character must be a character type
- output-stream must be an output stream

**Postconditions**:
- Character is written to the stream

**Errors**:
- `type-error` if character is not a character
- `type-error` if output-stream is not a stream
- `type-error` if output-stream is not an output stream

**Example**:
```lisp
(write-char #\H)           ; Writes "H" to *standard-output*
(write-char #\! *error-output*)  ; Writes "!" to stderr
```

---

### write-string

Writes a string to a stream.

**Signature**:
```lisp
(write-string string &optional output-stream &key start end) => string
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| string | string | required | The string to write |
| output-stream | stream | *standard-output* | Target stream |
| start | fixnum | 0 | Start index (inclusive) |
| end | fixnum | nil | End index (exclusive), nil means end of string |

**Returns**: The string argument

**Preconditions**:
- string must be a string type
- output-stream must be an output stream
- start and end must be valid bounding indices

**Postconditions**:
- Characters from start to end are written to the stream

**Errors**:
- `type-error` if string is not a string
- `type-error` if output-stream is not an output stream
- `error` if start/end are invalid indices

**Example**:
```lisp
(write-string "Hello, World!")        ; Full string
(write-string "Hello, World!" nil :start 0 :end 5)  ; "Hello"
```

---

## 2. Input Functions

### read-char

Reads a character from a stream.

**Signature**:
```lisp
(read-char &optional input-stream eof-error-p eof-value recursive-p) => character or eof-value
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| input-stream | stream | *standard-input* | Source stream |
| eof-error-p | generalized-boolean | t | Signal error on EOF? |
| eof-value | t | nil | Value to return on EOF |
| recursive-p | generalized-boolean | nil | Ignored (reader integration) |

**Returns**: Character or eof-value

**Preconditions**:
- input-stream must be an input stream

**Postconditions**:
- If character available: returns character, advances stream position
- If EOF and eof-error-p: signals end-of-file
- If EOF and not eof-error-p: returns eof-value

**Errors**:
- `type-error` if input-stream is not an input stream
- `end-of-file` if EOF reached and eof-error-p is true

**Example**:
```lisp
(read-char)                    ; Read from *standard-input*, error on EOF
(read-char nil nil :eof)       ; Return :eof instead of signaling
```

---

### read-line

Reads a line of text from a stream.

**Signature**:
```lisp
(read-line &optional input-stream eof-error-p eof-value recursive-p) => string, missing-newline-p
```

**Parameters**:
| Name | Type | Default | Description |
|------|------|---------|-------------|
| input-stream | stream | *standard-input* | Source stream |
| eof-error-p | generalized-boolean | t | Signal error on EOF? |
| eof-value | t | nil | Value to return on EOF |
| recursive-p | generalized-boolean | nil | Ignored |

**Returns**: Two values:
1. String (line content, without newline) or eof-value
2. Boolean (true if line was terminated by EOF instead of newline)

**Preconditions**:
- input-stream must be an input stream

**Postconditions**:
- If line available: returns (line, missing-newline-p)
- If EOF and eof-error-p: signals end-of-file
- If EOF and not eof-error-p: returns (eof-value, t)

**Errors**:
- `type-error` if input-stream is not an input stream
- `end-of-file` if EOF reached with no data and eof-error-p is true

**Example**:
```lisp
(read-line)                    ; => "user input", NIL
(read-line nil nil "")         ; => "" on EOF instead of error
```

---

## 3. Format Function

### format

Produces formatted output.

**Signature**:
```lisp
(format destination control-string &rest args) => result
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| destination | (or null (eql t) stream) | Output target |
| control-string | string | Format string with directives |
| args | &rest t | Arguments for directives |

**Returns**:
- If destination is nil: formatted string
- If destination is t or stream: NIL

**Destination Semantics**:
| Destination | Behavior |
|-------------|----------|
| nil | Return formatted string |
| t | Write to *standard-output*, return NIL |
| stream | Write to stream, return NIL |

**Supported Directives**:

| Directive | Description | Consumes Arg |
|-----------|-------------|--------------|
| ~A | Aesthetic (princ) output | Yes |
| ~S | Standard (prin1) output | Yes |
| ~D | Decimal integer | Yes |
| ~% | Newline | No |
| ~~ | Literal tilde | No |

**Errors**:
- `error` if unsupported directive encountered
- `error` if insufficient arguments for directives
- `type-error` if ~D argument is not an integer

**Example**:
```lisp
(format t "Hello, ~A!~%" "World")  ; Prints "Hello, World!\n"
(format nil "~D + ~D = ~D" 1 2 3)  ; Returns "1 + 2 = 3"
(format t "~S" "test")              ; Prints "\"test\""
```

---

## 4. Predicate Functions

### streamp

Tests if an object is a stream.

**Signature**:
```lisp
(streamp object) => generalized-boolean
```

**Returns**: True if object is a stream, NIL otherwise

---

### input-stream-p

Tests if a stream is an input stream.

**Signature**:
```lisp
(input-stream-p stream) => generalized-boolean
```

**Preconditions**: stream must be a stream

**Returns**: True if stream accepts input operations

---

### output-stream-p

Tests if a stream is an output stream.

**Signature**:
```lisp
(output-stream-p stream) => generalized-boolean
```

**Preconditions**: stream must be a stream

**Returns**: True if stream accepts output operations

---

## 5. Special Variables

### *standard-input*

**Type**: stream (input)
**Initial Value**: Stream bound to host stdin (fd 0)
**Rebindable**: Yes (dynamically)

### *standard-output*

**Type**: stream (output)
**Initial Value**: Stream bound to host stdout (fd 1)
**Rebindable**: Yes (dynamically)

### *error-output*

**Type**: stream (output)
**Initial Value**: Stream bound to host stderr (fd 2)
**Rebindable**: Yes (dynamically)
