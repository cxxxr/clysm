# Data Model: FFI-based Stream I/O

**Date**: 2025-12-24
**Branch**: `015-ffi-stream-io`

## Overview

Data structures for FFI-based stream I/O, including WasmGC types, FFI declarations, and runtime structures.

---

## 1. WasmGC Stream Type

### $stream Struct

WasmGC struct representing an I/O stream bound to a host file descriptor.

```wat
;; Type index: 19 (following numeric tower at 14-18)
(type $stream (struct
  (field $fd i32)              ;; Host file descriptor (0=stdin, 1=stdout, 2=stderr)
  (field $direction i32)       ;; Direction: 0=input, 1=output, 2=bidirectional
  (field $element-type anyref) ;; Element type symbol (:character or :binary)
))
```

**Fields**:

| Field | Type | Mutable | Description |
|-------|------|---------|-------------|
| $fd | i32 | No | Host file descriptor number |
| $direction | i32 | No | Stream direction (0=in, 1=out, 2=io) |
| $element-type | anyref | No | Element type (symbol ref, typically :character) |

**Type Index Constant**:
```lisp
(defconstant +type-stream+ 19 "Type index for stream struct")
```

---

## 2. Stream Direction Enumeration

```lisp
(deftype stream-direction ()
  "Direction of a stream."
  '(member :input :output :io))

(defconstant +direction-input+ 0)
(defconstant +direction-output+ 1)
(defconstant +direction-io+ 2)
```

---

## 3. FFI Import Declarations

### Host I/O Function Imports

```lisp
(defstruct (io-import-decl (:include foreign-function-decl))
  "FFI import for host I/O operations."
  ;; Inherits: lisp-name, module-name, field-name, param-types, return-type, type-index
  )
```

**Required Imports**:

| Lisp Name | Host Module | Host Field | Params | Return |
|-----------|-------------|------------|--------|--------|
| `%host-write-char` | clysm:io | write-char | (i32 i32) | void |
| `%host-write-string` | clysm:io | write-string | (i32 externref) | void |
| `%host-read-char` | clysm:io | read-char | (i32) | i32 |
| `%host-read-line` | clysm:io | read-line | (i32) | externref |

**Parameter Semantics**:
- First i32: File descriptor
- Second i32 (write-char): Unicode codepoint
- externref: WasmGC string reference
- Return i32 (read-char): Codepoint or -1 for EOF
- Return externref (read-line): String or null for EOF

---

## 4. Compiler-Side Structures

### StreamInfo

Compile-time representation of stream-related information.

```lisp
(defstruct (stream-info (:conc-name si-))
  "Compile-time information about a stream expression."
  (known-stream-p nil :type boolean)   ; Is this a known stream?
  (known-direction nil :type (or null stream-direction))
  (constant-fd nil :type (or null fixnum)))
```

### FormatDirective

Parsed format directive for compile-time processing.

```lisp
(defstruct (format-directive (:conc-name fd-))
  "Parsed format directive from format string."
  (character nil :type character)      ; The directive char (A, S, D, %, ~)
  (position nil :type fixnum)          ; Position in format string
  (arg-index nil :type (or null fixnum)) ; Which arg this consumes (nil for ~%, ~~)
  (colonp nil :type boolean)           ; Has colon modifier?
  (atp nil :type boolean))             ; Has at-sign modifier?
```

### FormatStringInfo

Complete parsed format string.

```lisp
(defstruct (format-string-info (:conc-name fsi-))
  "Parsed format string for compile-time processing."
  (literal-segments nil :type list)    ; List of literal string segments
  (directives nil :type list)          ; List of format-directive structs
  (arg-count nil :type fixnum))        ; Number of arguments consumed
```

---

## 5. Runtime Condition Types

### end-of-file Condition

```lisp
(defclass end-of-file (stream-error)
  ()
  (:documentation "Condition signaled when reading past end of stream."))

(defclass stream-error (error)
  ((stream :initarg :stream
           :reader stream-error-stream
           :documentation "The stream where the error occurred"))
  (:documentation "Base class for stream-related errors."))
```

**Hierarchy**:
```
condition
└── serious-condition
    └── error
        └── stream-error
            └── end-of-file
```

---

## 6. Standard Stream Symbols

Symbols for standard streams, bound during runtime initialization.

```lisp
;; Symbol definitions (actual binding happens at runtime)
(defvar *standard-input* nil
  "The default input stream.")

(defvar *standard-output* nil
  "The default output stream.")

(defvar *error-output* nil
  "The stream for error messages.")
```

**Initialization (WAT pseudo-code)**:
```wat
(func $init-standard-streams
  ;; Create stdin stream
  (global.set $*standard-input*
    (struct.new $stream
      (i32.const 0)          ;; fd = 0
      (i32.const 0)          ;; direction = input
      (global.get $keyword-character)))

  ;; Create stdout stream
  (global.set $*standard-output*
    (struct.new $stream
      (i32.const 1)          ;; fd = 1
      (i32.const 1)          ;; direction = output
      (global.get $keyword-character)))

  ;; Create stderr stream
  (global.set $*error-output*
    (struct.new $stream
      (i32.const 2)          ;; fd = 2
      (i32.const 1)          ;; direction = output
      (global.get $keyword-character))))
```

---

## 7. Entity Relationships

```
┌─────────────────────┐
│  Symbol ($symbol)   │
├─────────────────────┤
│ name: $string       │
│ value: anyref ──────┼───────┐
│ function: anyref    │       │
│ plist: anyref       │       │
└─────────────────────┘       │
                              ▼
          ┌───────────────────────────────┐
          │         $stream               │
          ├───────────────────────────────┤
          │ fd: i32 ───────────────────────┼──▶ Host file descriptor
          │ direction: i32                │
          │ element-type: anyref          │
          └───────────────────────────────┘

┌─────────────────────┐
│ ForeignFunctionDecl │
├─────────────────────┤
│ lisp-name: SYMBOL   │
│ module-name: STRING │◀── "clysm:io"
│ field-name: STRING  │◀── "write-char", "read-line", etc.
│ param-types: LIST   │
│ return-type: KEYWORD│
└─────────────────────┘

┌─────────────────────┐
│  FormatStringInfo   │
├─────────────────────┤
│ literal-segments ───┼──▶ ("Hello, " " world!")
│ directives ─────────┼──▶ (FormatDirective...)
│ arg-count: FIXNUM   │
└─────────────────────┘
          │
          ▼
┌─────────────────────┐
│  FormatDirective    │
├─────────────────────┤
│ character: CHAR     │◀── #\A, #\S, #\D, #\%, #\~
│ position: FIXNUM    │
│ arg-index: FIXNUM   │
│ colonp: BOOLEAN     │
│ atp: BOOLEAN        │
└─────────────────────┘
```

---

## 8. Validation Rules

### Stream Validation

```lisp
(defun valid-stream-p (object)
  "Check if OBJECT is a valid stream."
  (and (typep object 'stream)  ; ref.test $stream
       (let ((fd (stream-fd object)))
         (and (integerp fd)
              (>= fd 0)))))

(defun input-stream-p (stream)
  "Check if STREAM is an input stream."
  (member (stream-direction stream) '(:input :io)))

(defun output-stream-p (stream)
  "Check if STREAM is an output stream."
  (member (stream-direction stream) '(:output :io)))
```

### Format String Validation

```lisp
(defun valid-format-directive-p (char)
  "Check if CHAR is a supported format directive."
  (member char '(#\A #\S #\D #\% #\~) :test #'char-equal))
```

---

## 9. State Transitions

### Stream Lifecycle

```
[Created] ─── struct.new $stream ───▶ [Active]
                                          │
                                          │ (streams are never explicitly closed
                                          │  in this implementation)
                                          ▼
                                      [Active]
```

Note: Standard streams (fd 0, 1, 2) remain active for the lifetime of the Wasm module. Stream closing is not implemented in this feature (future extension).

### Format Processing

```
[Format String] ─── parse-format-string ───▶ [FormatStringInfo]
                                                    │
                                     compile-format │
                                                    ▼
                                          [Wasm IR sequence]
                                                    │
                                           emit    │
                                                    ▼
                                            [Wasm bytecode]
```
