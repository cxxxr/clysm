# Data Model: Sequence and Array Runtime Migration

**Feature**: 001-sequence-array-runtime
**Date**: 2026-01-04

## Entities

### 1. Sequence Types (Input)

The runtime functions operate on three distinct sequence types:

| Type | WasmGC Representation | Type Index | Detection |
|------|----------------------|------------|-----------|
| String | `$string` (array of i8) | 2 | `stringp` |
| List | `$cons` cells | 0 | `listp` |
| Vector | `$mv_array` (array of anyref) | 22 | `vectorp` |

### 2. Runtime Function Table Entry

```lisp
;; Structure: symbol -> (runtime-name . arity)
;; Storage: *runtime-function-table* hash table

;; Entries for this feature:
'subseq       -> (:$subseq-rt . nil)        ; variadic
'adjust-array -> (:$adjust-array-rt . nil)  ; variadic
```

### 3. UTF-8 Character Representation

For string subseq operations, character positions map to byte positions:

| Byte Pattern | Character Bytes | Detection |
|--------------|-----------------|-----------|
| 0xxxxxxx | 1 (ASCII) | `(< byte #x80)` |
| 110xxxxx | 2 | `(< byte #xE0)` |
| 1110xxxx | 3 | `(< byte #xF0)` |
| 11110xxx | 4 | default |

Continuation bytes (10xxxxxx) are skipped when counting characters.

## Function Signatures

### subseq-rt

```lisp
(defun subseq-rt (sequence start end)
  "Return subsequence from START to END.
   See [subseq](resources/HyperSpec/Body/f_subseq.htm).

   Arguments:
     SEQUENCE - string, list, or vector
     START    - non-negative integer (character index for strings)
     END      - nil or non-negative integer (nil = sequence length)

   Returns:
     New sequence of same type as input

   Errors:
     - START or END out of bounds
     - END < START
     - SEQUENCE is not a sequence type"
  ...)
```

### adjust-array-rt

```lisp
(defun adjust-array-rt (array new-dimensions initial-element initial-element-p)
  "Return array with new dimensions.
   See [adjust-array](resources/HyperSpec/Body/f_adjust.htm).

   Arguments:
     ARRAY             - 1D array (simple-vector)
     NEW-DIMENSIONS    - integer or list containing single integer
     INITIAL-ELEMENT   - value for new slots (if array grows)
     INITIAL-ELEMENT-P - t if :initial-element was provided

   Returns:
     New array with requested dimensions

   Errors:
     - ARRAY is not a 1D array
     - NEW-DIMENSIONS is invalid"
  ...)
```

## State Transitions

No persistent state. Both functions are pure transformations:

```
Input Sequence → [subseq-rt] → Output Sequence (new allocation)
Input Array    → [adjust-array-rt] → Output Array (new allocation)
```

## Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                    Compiler Dispatch                         │
│                                                              │
│  (subseq ...)  ──────► *runtime-function-table*             │
│                              │                               │
│                              ▼                               │
│                    (runtime-function-p 'subseq)              │
│                              │                               │
│                              ▼                               │
│                    compile-runtime-call                      │
│                              │                               │
│                              ▼                               │
│                    (:call :$subseq-rt)                       │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                    Runtime Execution                         │
│                                                              │
│  subseq-rt(sequence, start, end)                            │
│       │                                                      │
│       ├── stringp? ──► string-subseq (UTF-8 aware)          │
│       │                                                      │
│       ├── listp? ────► list-subseq (nthcdr + copy)          │
│       │                                                      │
│       └── vectorp? ──► vector-subseq (aref + make-array)    │
└─────────────────────────────────────────────────────────────┘
```

## Validation Rules

| Field | Rule | Error Message |
|-------|------|---------------|
| sequence | Must be string, list, or vector | "Not a sequence: ~A" |
| start | 0 ≤ start ≤ length | "Start ~A out of bounds for length ~A" |
| end | start ≤ end ≤ length (if provided) | "End ~A out of bounds or less than start ~A" |
| array | Must be 1D array | "Only 1D arrays supported" |
| new-dimensions | Positive integer or list of one | "Invalid dimensions: ~A" |
