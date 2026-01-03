# Data Model: String Runtime Migration

**Feature**: 001-string-runtime-migration
**Date**: 2026-01-03

## Overview

This feature migrates string functions from inline Wasm codegen to a Lisp runtime library. The "data model" consists of function signatures with their parameter types and return values.

## Runtime Function Signatures

### string-char-rt

```lisp
(defun string-char-rt (string index)
  "Return character at INDEX in STRING.
   See [char](resources/HyperSpec/Body/f_char_.htm)."
  ...)
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| string | string | Source string |
| index | fixnum | Character index (0-based) |

**Return**: character

**Dispatch Registration**:
```lisp
(register-runtime-function 'char :$string-char-rt 2)
(register-runtime-function 'schar :$string-char-rt 2)
```

---

### string-trim-rt

```lisp
(defun string-trim-rt (character-bag string start end)
  "Return STRING with characters in CHARACTER-BAG trimmed from both ends.
   See [string-trim](resources/HyperSpec/Body/f_stg_tr.htm)."
  ...)
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| character-bag | string | Characters to trim |
| string | string | Source string |
| start | (or fixnum null) | Start index (default 0) |
| end | (or fixnum null) | End index (default length) |

**Return**: string (newly allocated)

**Dispatch Registration**:
```lisp
(register-runtime-function 'string-trim :$string-trim-rt nil)  ; variadic for keywords
```

---

### string-left-trim-rt

```lisp
(defun string-left-trim-rt (character-bag string start end)
  "Return STRING with characters in CHARACTER-BAG trimmed from left.
   See [string-left-trim](resources/HyperSpec/Body/f_stg_tr.htm)."
  ...)
```

**Parameters**: Same as string-trim-rt

**Return**: string (newly allocated)

---

### string-right-trim-rt

```lisp
(defun string-right-trim-rt (character-bag string start end)
  "Return STRING with characters in CHARACTER-BAG trimmed from right.
   See [string-right-trim](resources/HyperSpec/Body/f_stg_tr.htm)."
  ...)
```

**Parameters**: Same as string-trim-rt

**Return**: string (newly allocated)

---

### string-capitalize-rt

```lisp
(defun string-capitalize-rt (string start end)
  "Return STRING with first char of each word uppercased, rest lowercased.
   See [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm)."
  ...)
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| string | string | Source string |
| start | (or fixnum null) | Start index (default 0) |
| end | (or fixnum null) | End index (default length) |

**Return**: string (newly allocated)

**Dispatch Registration**:
```lisp
(register-runtime-function 'string-capitalize :$string-capitalize-rt nil)
```

---

### nstring-capitalize-rt

```lisp
(defun nstring-capitalize-rt (string start end)
  "Destructively capitalize STRING (first char up, rest down per word).
   See [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm)."
  ...)
```

**Parameters**: Same as string-capitalize-rt

**Return**: string (same object, modified)

**Dispatch Registration**:
```lisp
(register-runtime-function 'nstring-capitalize :$nstring-capitalize-rt nil)
```

---

### string-compare-ci-rt

```lisp
(defun string-compare-ci-rt (string1 string2 start1 end1 start2 end2 comparison)
  "Case-insensitive string comparison.
   COMPARISON is one of :equal, :not-equal, :lt, :gt, :le, :ge.
   See [string-equal](resources/HyperSpec/Body/f_stgeq_.htm)."
  ...)
```

**Parameters**:
| Name | Type | Description |
|------|------|-------------|
| string1 | string | First string |
| string2 | string | Second string |
| start1 | (or fixnum null) | Start index for string1 |
| end1 | (or fixnum null) | End index for string1 |
| start2 | (or fixnum null) | Start index for string2 |
| end2 | (or fixnum null) | End index for string2 |
| comparison | keyword | Comparison type |

**Return**:
- For :equal: T or NIL
- For :not-equal, :lt, :gt, :le, :ge: mismatch index (fixnum) or NIL

**Dispatch Registration**:
```lisp
(register-runtime-function 'string-equal :$string-equal-rt nil)
(register-runtime-function 'string-not-equal :$string-not-equal-rt nil)
(register-runtime-function 'string-lessp :$string-lessp-rt nil)
(register-runtime-function 'string-greaterp :$string-greaterp-rt nil)
(register-runtime-function 'string-not-lessp :$string-not-lessp-rt nil)
(register-runtime-function 'string-not-greaterp :$string-not-greaterp-rt nil)
```

## Helper Functions

### utf8-continuation-byte-p

```lisp
(defun utf8-continuation-byte-p (byte)
  "Return T if BYTE is a UTF-8 continuation byte (10xxxxxx)."
  (= (logand byte #xC0) #x80))
```

### decode-utf8-char

```lisp
(defun decode-utf8-char (string byte-index)
  "Decode UTF-8 character at BYTE-INDEX in STRING.
   Returns the character and the number of bytes consumed."
  ...)
```

### char-in-bag-p

```lisp
(defun char-in-bag-p (char character-bag)
  "Return T if CHAR is in CHARACTER-BAG string."
  ...)
```

### alpha-char-p-ascii

```lisp
(defun alpha-char-p-ascii (char)
  "Return T if CHAR is ASCII alphabetic (A-Z, a-z)."
  ...)
```

## Type Relationships

```
                    string-runtime.lisp
                           │
        ┌─────────────────┼─────────────────┐
        │                 │                 │
   string-char-rt    string-trim-rt    string-capitalize-rt
        │                 │                 │
        └─────────────────┴─────────────────┘
                          │
                   Helper Functions
                 (utf8, char-in-bag, etc.)
```

## State Transitions

**Non-destructive functions** (string-trim-rt, string-capitalize-rt, string-char-rt):
- Input string → unchanged
- Return value → new string or character

**Destructive functions** (nstring-capitalize-rt):
- Input string → modified in-place
- Return value → same string object

## Validation Rules

1. **Index bounds**: 0 ≤ start ≤ end ≤ length
2. **NIL defaults**: start defaults to 0, end defaults to length
3. **Character bag**: Must be a string (designator coerced by caller)
4. **UTF-8**: Invalid sequences treated as single bytes per ANSI spec
