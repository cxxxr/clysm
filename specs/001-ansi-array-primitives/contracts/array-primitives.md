# API Contracts: Array Primitives

**Feature**: 001-ansi-array-primitives
**Date**: 2025-12-29

## Overview

This document defines the compilation contracts for array/sequence primitives. Each primitive compiles to specific WasmGC instructions.

## aref

**ANSI CL**: [aref](../../../resources/HyperSpec/Body/f_aref.htm)

### Signature

```lisp
(aref array &rest subscripts) => element
```

### Compilation Contract

**Input AST**: `(ast-call :operator 'aref :args (array-expr index-expr))`

**Output Instructions**:
```wat
;; For single-dimensional simple-vector
<compile array-expr>           ;; Stack: [arrayref]
(:ref.cast (:ref 22))          ;; Ensure type 22
<compile index-expr>           ;; Stack: [arrayref, anyref]
(:ref.cast :i31)               ;; Ensure fixnum
:i31.get_s                     ;; Stack: [arrayref, i32]
(:array.get 22)                ;; Stack: [anyref]
```

### Error Cases

| Condition | Signal |
|-----------|--------|
| `array` not a vector | `(error 'type-error :datum array :expected-type 'vector)` |
| `index` not an integer | `(error 'type-error :datum index :expected-type 'integer)` |
| `index` out of bounds | Wasm trap (array bounds check) |

### Test Cases

```lisp
;; Basic access
(aref #(1 2 3) 0) => 1
(aref #(1 2 3) 1) => 2
(aref #(1 2 3) 2) => 3

;; Nested values
(aref #(#(a b) #(c d)) 0) => #(a b)

;; Edge cases
(aref #() 0) => error (bounds)
(aref #(1) -1) => error (bounds)
```

---

## svref

**ANSI CL**: [svref](../../../resources/HyperSpec/Body/f_svref.htm)

### Signature

```lisp
(svref simple-vector index) => element
```

### Compilation Contract

**Input AST**: `(ast-call :operator 'svref :args (vec-expr index-expr))`

**Output Instructions**:
```wat
;; Identical to aref for simple-vectors
<compile vec-expr>
(:ref.cast (:ref 22))
<compile index-expr>
(:ref.cast :i31)
:i31.get_s
(:array.get 22)
```

### Notes

- `svref` is semantically identical to `aref` for simple-vectors
- May share implementation with `aref`

---

## schar

**ANSI CL**: [schar](../../../resources/HyperSpec/Body/f_schar.htm)

### Signature

```lisp
(schar simple-string index) => character
```

### Compilation Contract

**Input AST**: `(ast-call :operator 'schar :args (str-expr index-expr))`

**Output Instructions**:
```wat
<compile str-expr>             ;; Stack: [stringref]
(:ref.cast (:ref 4))           ;; Ensure type 4 ($string)
<compile index-expr>           ;; Stack: [stringref, anyref]
(:ref.cast :i31)
:i31.get_s                     ;; Stack: [stringref, i32]
(:array.get_u 4)               ;; Stack: [i32] (byte value)
:ref.i31                       ;; Stack: [i31ref] (character)
```

### Notes

- Returns character as i31ref containing code point
- **Phase 13D-1 Limitation**: Assumes ASCII (1 byte = 1 character)
- UTF-8 multi-byte support deferred to future phase

### Test Cases

```lisp
(schar "hello" 0) => #\h
(schar "hello" 4) => #\o
(char-code (schar "ABC" 1)) => 66
```

---

## elt

**ANSI CL**: [elt](../../../resources/HyperSpec/Body/f_elt.htm)

### Signature

```lisp
(elt sequence index) => element
```

### Compilation Contract

**Input AST**: `(ast-call :operator 'elt :args (seq-expr index-expr))`

**Output Instructions** (type dispatch):
```wat
<compile seq-expr>
(:local.tee $seq)              ;; Save sequence

;; Test for simple-vector (type 22)
(:local.get $seq)
(:ref.test (:ref 22))
(:if (:result :anyref))
  ;; Vector path
  (:local.get $seq)
  (:ref.cast (:ref 22))
  <compile index-expr>
  (:ref.cast :i31)
  :i31.get_s
  (:array.get 22)
:else
  ;; Test for string (type 4)
  (:local.get $seq)
  (:ref.test (:ref 4))
  (:if (:result :anyref))
    ;; String path
    (:local.get $seq)
    (:ref.cast (:ref 4))
    <compile index-expr>
    (:ref.cast :i31)
    :i31.get_s
    (:array.get_u 4)
    :ref.i31
  :else
    ;; List path - call runtime nth
    (:local.get $seq)
    <compile index-expr>
    (:call $nth)
  :end
:end
```

### Dependencies

- Runtime function `$nth` for list access

### Test Cases

```lisp
;; Vector
(elt #(a b c) 1) => b

;; String
(elt "hello" 0) => #\h

;; List
(elt '(a b c) 2) => c
```

---

## coerce

**ANSI CL**: [coerce](../../../resources/HyperSpec/Body/f_coerce.htm)

### Signature

```lisp
(coerce object result-type) => result
```

### Compilation Contract

**Compile-time expansion** when `result-type` is a constant:

```lisp
;; (coerce x 'vector) expands to:
(if (simple-vector-p x)
    x
    (%list-to-vector x))

;; (coerce x 'list) expands to:
(if (listp x)
    x
    (%vector-to-list x))

;; (coerce x 'string) from character list:
(%chars-to-string x)
```

### Runtime Helpers

```lisp
;; %list-to-vector: Traverse list, build array
;; %vector-to-list: Iterate array, cons in reverse, nreverse
;; %chars-to-string: Extract code points, build string
```

### Test Cases

```lisp
(coerce '(1 2 3) 'vector) => #(1 2 3)
(coerce #(a b c) 'list) => (a b c)
(coerce '(#\h #\i) 'string) => "hi"
(coerce "hello" 'list) => (#\h #\e #\l #\l #\o)
```

---

## Setf Forms

### (setf aref)

**Expansion** (via setf-expanders.lisp):
```lisp
(setf (aref arr i) val)
→ (%setf-aref arr val i)
```

**%setf-aref Compilation**:
```wat
<compile arr>
(:ref.cast (:ref 22))
<compile i>
(:ref.cast :i31)
:i31.get_s
<compile val>
(:array.set 22)
<compile val>                   ;; Return stored value
```

### (setf svref)

Identical to `(setf aref)` - share implementation.

### (setf schar)

```wat
<compile str>
(:ref.cast (:ref 4))
<compile i>
(:ref.cast :i31)
:i31.get_s
<compile char>                  ;; Character value
(:ref.cast :i31)
:i31.get_s                      ;; Extract code point
(:i32.and (:i32.const 255))     ;; Mask to byte
(:array.set 4)
<compile char>                  ;; Return stored value
```

### (setf elt)

Type dispatch similar to `elt` read, using appropriate `array.set` or `rplaca` for lists.
