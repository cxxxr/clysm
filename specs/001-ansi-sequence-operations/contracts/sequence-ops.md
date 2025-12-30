# Contract: Sequence Operations Wasm Output

**Branch**: `001-ansi-sequence-operations` | **Date**: 2025-12-29

## Overview

This document defines the expected Wasm instruction patterns for compiled sequence operations. These contracts serve as test specifications for contract tests.

## subseq Contract

### Signature

```lisp
(subseq sequence start &optional end) → sequence
```

### Wasm Output Pattern (Vector Case)

```wat
;; Input: sequence on stack, start and end as locals
;; Output: new vector on stack

;; 1. Bounds validation
local.get $seq
array.len
local.get $end
i32.gt_u
if
  ;; Signal bounding-indices-bad-error
  ...
end

;; 2. Calculate length
local.get $end
local.get $start
i32.sub
local.set $len

;; 3. Create new array
local.get $len
array.new_default $anyref-array

;; 4. Copy elements
local.get $new-array     ;; dest
i32.const 0              ;; dest offset
local.get $seq           ;; src
local.get $start         ;; src offset
local.get $len           ;; length
array.copy $anyref-array $anyref-array
```

### Contract Test Assertions

1. **CT-SUBSEQ-001**: `(subseq #(1 2 3 4 5) 1 3)` compiles to Wasm containing `array.copy`
2. **CT-SUBSEQ-002**: Generated Wasm passes `wasm-tools validate`
3. **CT-SUBSEQ-003**: Output contains bounds check instructions (`array.len`, `i32.gt_u`)

## concatenate Contract

### Signature

```lisp
(concatenate result-type &rest sequences) → sequence
```

### Wasm Output Pattern (String Case)

```wat
;; Input: result-type symbol, sequence refs on stack
;; Output: new string on stack

;; 1. Calculate total length
i32.const 0
local.set $total-len
;; For each input sequence:
local.get $seq-n
array.len
local.get $total-len
i32.add
local.set $total-len

;; 2. Create result array
local.get $total-len
array.new_default $string

;; 3. Copy each sequence
local.get $result
i32.const 0              ;; initial offset
;; For each input:
local.get $result
local.get $offset
local.get $seq-n
i32.const 0
local.get $seq-n
array.len
array.copy $string $string
;; Update offset
```

### Contract Test Assertions

1. **CT-CONCAT-001**: `(concatenate 'string "foo" "bar")` compiles to Wasm containing `array.copy`
2. **CT-CONCAT-002**: Generated Wasm passes `wasm-tools validate`
3. **CT-CONCAT-003**: Multiple `array.copy` instructions present for multiple inputs

## make-string Contract

### Signature

```lisp
(make-string size &key initial-element element-type) → string
```

### Wasm Output Pattern

```wat
;; Input: size (i32), initial-element (optional, default #\Null)
;; Output: new string on stack

;; 1. Get size
local.get $size

;; 2. Get initial byte value (UTF-8 encoding of character)
local.get $initial-element  ;; or i32.const 0 for default

;; 3. Create string with initial value
array.new $string
```

### Contract Test Assertions

1. **CT-MKSTR-001**: `(make-string 5)` compiles to Wasm containing `array.new`
2. **CT-MKSTR-002**: `(make-string 3 :initial-element #\x)` uses the character byte value
3. **CT-MKSTR-003**: Generated Wasm passes `wasm-tools validate`

## make-array Extensions Contract

### Signature (Extensions)

```lisp
(make-array dimensions &key initial-element initial-contents element-type) → array
```

### Wasm Output Pattern (:initial-element)

```wat
;; Input: size (i32), initial-element value
;; Output: new array on stack

local.get $size
local.get $initial-element
array.new $anyref-array
```

### Wasm Output Pattern (:initial-contents)

```wat
;; Input: size (i32), contents as list/vector
;; Output: new array on stack

;; Push each element onto stack
local.get $elem-0
local.get $elem-1
...
local.get $elem-n

;; Create array from stack values
array.new_fixed $anyref-array <n>
```

### Contract Test Assertions

1. **CT-MKARR-001**: `(make-array 3 :initial-element nil)` compiles to `array.new`
2. **CT-MKARR-002**: `(make-array 3 :initial-contents '(a b c))` compiles to `array.new_fixed`
3. **CT-MKARR-003**: Generated Wasm passes `wasm-tools validate`

## copy-seq Contract

### Signature

```lisp
(copy-seq sequence) → sequence
```

### Wasm Output Pattern

```wat
;; Equivalent to (subseq sequence 0)

;; 1. Get length
local.get $seq
array.len
local.set $len

;; 2. Create new array
local.get $len
array.new_default $type

;; 3. Copy all elements
local.get $new-array
i32.const 0
local.get $seq
i32.const 0
local.get $len
array.copy $type $type
```

### Contract Test Assertions

1. **CT-CPSEQ-001**: `(copy-seq #(1 2 3))` compiles to Wasm containing `array.copy`
2. **CT-CPSEQ-002**: Source array is fully copied (length check)
3. **CT-CPSEQ-003**: Generated Wasm passes `wasm-tools validate`

## Common Validation

All compiled sequence operations MUST:

1. Pass `wasm-tools validate` without errors
2. Use correct type indices from `gc-types.lisp`
3. Include appropriate bounds checking for indexed access
4. Follow existing codegen patterns from Phase 13D-1

## Type Dispatch Pattern

For polymorphic operations (subseq, copy-seq, elt on sequence):

```wat
;; Type dispatch using ref.test
local.get $seq
ref.test (ref $string)
if
  ;; String case
else
  local.get $seq
  ref.test (ref $anyref-array)
  if
    ;; Vector case
  else
    ;; List case (iterate cons cells)
  end
end
```
