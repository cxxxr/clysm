# Runtime API Contracts

**Date**: 2026-01-01
**Feature**: 001-io-list-runtime

## Overview

This document defines the API contracts for runtime library functions. These contracts specify the expected behavior, parameter validation, and return values.

---

## I/O Functions

### princ

```lisp
(princ object &optional output-stream) → object
```

**Parameters**:
- `object`: Any Lisp object
- `output-stream`: Stream designator (default: `*standard-output*`)

**Behavior**:
1. Write aesthetic (human-readable) representation to stream
2. No escape characters for strings (no quotes)
3. Symbols printed in lowercase without package prefix (if accessible)

**Return**: The original object

**HyperSpec**: [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm)

---

### prin1

```lisp
(prin1 object &optional output-stream) → object
```

**Parameters**:
- `object`: Any Lisp object
- `output-stream`: Stream designator (default: `*standard-output*`)

**Behavior**:
1. Write standard (machine-readable) representation to stream
2. Strings printed with quotes
3. Symbols printed with package prefix if necessary

**Return**: The original object

**HyperSpec**: [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm)

---

### print

```lisp
(print object &optional output-stream) → object
```

**Parameters**:
- `object`: Any Lisp object
- `output-stream`: Stream designator (default: `*standard-output*`)

**Behavior**:
1. Output newline
2. Output object as by `prin1`
3. Output space

**Return**: The original object

**HyperSpec**: [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm)

---

### write

```lisp
(write object &key stream escape radix base circle pretty level length) → object
```

**Parameters**:
- `object`: Any Lisp object
- `:stream`: Stream designator (default: `*standard-output*`)
- `:escape`: Boolean, whether to use escape syntax (default: `*print-escape*`)
- `:radix`: Boolean, print radix for integers (default: `*print-radix*`)
- `:base`: Integer 2-36, number base (default: `*print-base*`)
- `:circle`: Boolean, detect circular structures (default: `*print-circle*`)
- `:pretty`: Boolean, use pretty-printing (default: `*print-pretty*`)
- `:level`: Integer or nil, depth limit (default: `*print-level*`)
- `:length`: Integer or nil, length limit (default: `*print-length*`)

**Behavior**:
1. Bind printer control variables
2. Write object to stream

**Return**: The original object

**HyperSpec**: [f_wr_pr.htm](resources/HyperSpec/Body/f_wr_pr.htm)

---

### format

```lisp
(format destination control-string &rest args) → result
```

**Parameters**:
- `destination`: nil (return string), t (stdout), or stream
- `control-string`: Format control string
- `args`: Format arguments

**Supported Directives**:
| Directive | Description |
|-----------|-------------|
| ~A | Aesthetic (princ) |
| ~S | Standard (prin1) |
| ~D | Decimal integer |
| ~B | Binary integer |
| ~O | Octal integer |
| ~X | Hexadecimal integer |
| ~% | Newline |
| ~& | Fresh line |
| ~~ | Literal tilde |
| ~R | Radix output |
| ~C | Character output |
| ~F | Fixed-format float |
| ~E | Exponential float |
| ~G | General float |
| ~$ | Monetary format |

**Return**:
- If destination is nil: formatted string
- Otherwise: nil

**HyperSpec**: [f_format.htm](resources/HyperSpec/Body/f_format.htm)

---

## List Functions

### member

```lisp
(member item list &key test test-not key) → tail
```

**Parameters**:
- `item`: Object to search for
- `list`: List to search
- `:test`: Equality test function (default: `#'eql`)
- `:test-not`: Negated test function (deprecated, mutually exclusive with :test)
- `:key`: Key extraction function (default: `#'identity`)

**Behavior**:
1. Iterate through list
2. Apply `:key` to each element
3. Test with `:test` against `item`
4. Return tail starting from matching element

**Return**: Tail of list starting at match, or nil

**HyperSpec**: [f_mem_m.htm](resources/HyperSpec/Body/f_mem_m.htm)

---

### member-if / member-if-not

```lisp
(member-if predicate list &key key) → tail
(member-if-not predicate list &key key) → tail
```

**Parameters**:
- `predicate`: Function of one argument
- `list`: List to search
- `:key`: Key extraction function (default: `#'identity`)

**Return**: Tail of list starting at element satisfying (or not satisfying) predicate

---

### assoc

```lisp
(assoc item alist &key test test-not key) → entry
```

**Parameters**:
- `item`: Key to search for
- `alist`: Association list
- `:test`: Equality test function (default: `#'eql`)
- `:test-not`: Negated test function (deprecated)
- `:key`: Key extraction function applied to car of entries (default: `#'identity`)

**Behavior**:
1. Iterate through alist
2. Skip non-cons entries (per ANSI CL)
3. Apply `:key` to car of each entry
4. Test with `:test` against `item`

**Return**: First matching cons cell, or nil

**HyperSpec**: [f_assocc.htm](resources/HyperSpec/Body/f_assocc.htm)

---

### find / find-if / find-if-not

```lisp
(find item sequence &key test key start end from-end) → element
(find-if predicate sequence &key key start end from-end) → element
(find-if-not predicate sequence &key key start end from-end) → element
```

**Parameters**:
- `item` or `predicate`: Search target or test function
- `sequence`: List or vector
- `:test`: Equality test (default: `#'eql`)
- `:key`: Key extraction function (default: `#'identity`)
- `:start`: Starting index (default: 0)
- `:end`: Ending index (default: sequence length)
- `:from-end`: Search from end (default: nil)

**Return**: Matching element, or nil

**HyperSpec**: [f_find_.htm](resources/HyperSpec/Body/f_find_.htm)

---

### position / position-if / position-if-not

```lisp
(position item sequence &key test key start end from-end) → index
(position-if predicate sequence &key key start end from-end) → index
(position-if-not predicate sequence &key key start end from-end) → index
```

**Parameters**: Same as find

**Return**: Index of matching element (relative to sequence start), or nil

**HyperSpec**: [f_pos_p.htm](resources/HyperSpec/Body/f_pos_p.htm)

---

## Error Conditions

| Condition | When Raised |
|-----------|-------------|
| `type-error` | Non-list passed to list function |
| `type-error` | :start/:end beyond sequence bounds |
| `simple-error` | Unsupported format directive |
| `wrong-number-of-arguments` | Insufficient format arguments |
