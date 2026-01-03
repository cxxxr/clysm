# Runtime Function Contracts

**Feature**: 001-string-runtime-migration
**Date**: 2026-01-03

## Contract Overview

This document defines the behavioral contracts for runtime string functions. Each contract specifies:
- Function signature (inputs/outputs)
- Preconditions (caller guarantees)
- Postconditions (function guarantees)
- Error conditions

## Function Contracts

### string-char-rt

**Signature**: `(string-char-rt string index) → character`

**Preconditions**:
- `string` is a valid string object
- `index` is a non-negative fixnum

**Postconditions**:
- Returns the character at position `index` (0-indexed)
- Original string is unchanged

**Error Conditions**:
- `index` ≥ character length: signals error

**ANSI Reference**: [char](resources/HyperSpec/Body/f_char_.htm)

---

### string-trim-rt

**Signature**: `(string-trim-rt character-bag string start end) → string`

**Preconditions**:
- `character-bag` is a valid string
- `string` is a valid string
- `start` is nil or non-negative fixnum
- `end` is nil or non-negative fixnum
- If both specified: start ≤ end

**Postconditions**:
- Returns new string with leading/trailing characters from bag removed
- Only characters within [start, end) range are considered
- Original string is unchanged

**Error Conditions**:
- start > end: signals error
- start or end > length: signals error

**ANSI Reference**: [string-trim](resources/HyperSpec/Body/f_stg_tr.htm)

---

### string-left-trim-rt

**Signature**: `(string-left-trim-rt character-bag string start end) → string`

**Preconditions**: Same as string-trim-rt

**Postconditions**:
- Returns new string with leading characters from bag removed
- Only characters within [start, end) range are considered
- Original string is unchanged

**ANSI Reference**: [string-left-trim](resources/HyperSpec/Body/f_stg_tr.htm)

---

### string-right-trim-rt

**Signature**: `(string-right-trim-rt character-bag string start end) → string`

**Preconditions**: Same as string-trim-rt

**Postconditions**:
- Returns new string with trailing characters from bag removed
- Only characters within [start, end) range are considered
- Original string is unchanged

**ANSI Reference**: [string-right-trim](resources/HyperSpec/Body/f_stg_tr.htm)

---

### string-capitalize-rt

**Signature**: `(string-capitalize-rt string start end) → string`

**Preconditions**:
- `string` is a valid string
- `start` is nil or non-negative fixnum
- `end` is nil or non-negative fixnum
- If both specified: start ≤ end

**Postconditions**:
- Returns new string with each word capitalized
- Word = sequence of alphabetic characters
- First alphabetic char of word → uppercase
- Subsequent alphabetic chars → lowercase
- Non-alphabetic characters are word boundaries, unchanged
- Only characters within [start, end) are modified
- Original string is unchanged

**Error Conditions**:
- start > end: signals error
- start or end > length: signals error

**ANSI Reference**: [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm)

---

### nstring-capitalize-rt

**Signature**: `(nstring-capitalize-rt string start end) → string`

**Preconditions**: Same as string-capitalize-rt

**Postconditions**:
- Modifies string in-place with each word capitalized
- Returns the same string object (not a copy)
- Word boundary rules same as string-capitalize-rt
- Only characters within [start, end) are modified

**ANSI Reference**: [nstring-capitalize](resources/HyperSpec/Body/f_stg_up.htm)

---

### string-compare-ci-rt

**Signature**: `(string-compare-ci-rt string1 string2 start1 end1 start2 end2 comparison) → (or fixnum boolean)`

**Preconditions**:
- `string1`, `string2` are valid strings
- `start1`, `end1`, `start2`, `end2` are nil or non-negative fixnums
- `comparison` is one of: :equal, :not-equal, :lt, :gt, :le, :ge

**Postconditions**:
- Compares substrings case-insensitively
- For :equal → T if equal, NIL otherwise
- For others → mismatch index (character position) or NIL

**Error Conditions**:
- Invalid comparison keyword: signals error
- start > end for either string: signals error

**ANSI Reference**: [string-equal](resources/HyperSpec/Body/f_stgeq_.htm)

## Dispatch Table Contract

The following entries must be added to `*runtime-function-table*`:

| Symbol | Runtime Name | Arity |
|--------|--------------|-------|
| char | :$string-char-rt | 2 |
| schar | :$string-char-rt | 2 |
| string-trim | :$string-trim-rt | nil |
| string-left-trim | :$string-left-trim-rt | nil |
| string-right-trim | :$string-right-trim-rt | nil |
| string-capitalize | :$string-capitalize-rt | nil |
| nstring-capitalize | :$nstring-capitalize-rt | nil |
| string-equal | :$string-equal-rt | nil |
| string-not-equal | :$string-not-equal-rt | nil |
| string-lessp | :$string-lessp-rt | nil |
| string-greaterp | :$string-greaterp-rt | nil |
| string-not-lessp | :$string-not-lessp-rt | nil |
| string-not-greaterp | :$string-not-greaterp-rt | nil |

**Note**: Arity `nil` indicates variadic functions (keyword arguments handled).

## Backward Compatibility Contract

1. All existing tests in `tests/unit/` must pass without modification
2. Stage 1 compilation must succeed
3. `wasm-tools validate dist/clysm-stage1.wasm` must pass
4. Behavior must match ANSI CL specification exactly
