# Data Model: ANSI String Trim Functions (Phase 16B)

**Date**: 2025-12-31
**Feature**: `001-ansi-string-trim`

## Overview

This feature adds six string functions that operate on the existing `$string` WasmGC type. No new types are introduced.

## Existing Types Used

### $string (Type Index 4)

**Definition** (from `gc-types.lisp`):
```wat
(type $string (array (mut i8)))
```

**Properties**:
- Element type: i8 (single byte)
- Mutable: yes (enables nstring destructive operations)
- Encoding: UTF-8 bytes
- Access: `array.get_u`/`array.set`

### Character Representation

**Type**: i31ref (unboxed fixnum)
**Value**: Unicode code point
**Access**: Extract with `i31.get_u`

## Function Signatures

### Trim Functions (Non-Destructive)

| Function | ANSI CL Signature | Return Type |
|----------|-------------------|-------------|
| [string-trim](resources/HyperSpec/Body/f_stg_tr.htm) | `(string-trim character-bag string &key :start :end)` | string |
| [string-left-trim](resources/HyperSpec/Body/f_stg_tr.htm) | `(string-left-trim character-bag string &key :start :end)` | string |
| [string-right-trim](resources/HyperSpec/Body/f_stg_tr.htm) | `(string-right-trim character-bag string &key :start :end)` | string |

**Character Bag**: Sequence of characters (string, list, or vector)
**:start**: Non-negative integer, defaults to 0
**:end**: Non-negative integer or nil (defaults to string length)

### Case Conversion Functions (Destructive)

| Function | ANSI CL Signature | Return Type |
|----------|-------------------|-------------|
| [nstring-upcase](resources/HyperSpec/Body/f_stg_up.htm) | `(nstring-upcase string &key :start :end)` | string (same object) |
| [nstring-downcase](resources/HyperSpec/Body/f_stg_up.htm) | `(nstring-downcase string &key :start :end)` | string (same object) |
| [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm) | `(nstring-capitalize string &key :start :end)` | string (same object) |

## Data Flow

### Trim Function Flow

```
Input: character-bag (sequence), string ($string), :start (i31ref), :end (i31ref/nil)
                |
                v
    +------------------------+
    | Validate bounding      |
    | indices                |
    +------------------------+
                |
                v
    +------------------------+
    | Scan from left:        |
    | find first non-trim    |
    | character              |
    +------------------------+
                |
                v
    +------------------------+
    | Scan from right:       |
    | find last non-trim     |
    | character              |
    +------------------------+
                |
                v
    +------------------------+
    | Create new string      |
    | (subseq equivalent)    |
    +------------------------+
                |
                v
Output: new $string (trimmed copy)
```

### Destructive Case Conversion Flow

```
Input: string ($string), :start (i31ref), :end (i31ref/nil)
                |
                v
    +------------------------+
    | Validate bounding      |
    | indices                |
    +------------------------+
                |
                v
    +------------------------+
    | Loop from start to end:|
    |  - Read byte           |
    |  - Convert case        |
    |  - Write byte in-place |
    +------------------------+
                |
                v
Output: same $string (modified in-place)
```

## Validation Rules

### Bounding Index Constraints

| Parameter | Type | Valid Range | Default |
|-----------|------|-------------|---------|
| :start | fixnum | 0 ≤ start ≤ length | 0 |
| :end | fixnum or nil | start ≤ end ≤ length | nil (= length) |

### Error Conditions

| Condition | Error Type | Per FR |
|-----------|------------|--------|
| :start < 0 | type-error | FR-014 |
| :end < :start | type-error | FR-014 |
| :start > length | type-error | FR-014 |
| :end > length | type-error | FR-014 |
| Non-string argument | type-error | FR-014 |
| Non-sequence character-bag | type-error | FR-014 |

## State Transitions

### Trim Functions (Immutable)
- Input string: unchanged
- Output: new string object

### nstring Functions (Mutable)
- Input string: modified in-place
- Output: same string object reference

```
Before nstring-upcase: "hello" at address 0x1000
After nstring-upcase:  "HELLO" at address 0x1000 (same object)
```

## Relationships to Other Entities

```
$string <--- uses --- string-trim, string-left-trim, string-right-trim
   ^
   |
   +--- mutates --- nstring-upcase, nstring-downcase, nstring-capitalize

character-bag (sequence)
   |
   +--- contains --- characters (i31ref code points)
```
