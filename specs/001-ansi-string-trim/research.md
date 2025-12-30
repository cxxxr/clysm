# Research: ANSI String Trim Functions (Phase 16B)

**Date**: 2025-12-31
**Feature**: `001-ansi-string-trim`

## Research Questions

1. How should character bags be handled for trim functions?
2. What are the implementation patterns for trim algorithms?
3. How do destructive nstring functions differ from non-destructive versions?
4. What existing patterns can be reused from `func-section.lisp`?

---

## 1. Character Bag Handling

### Decision: Inline byte-by-byte membership check

### Rationale
ANSI CL specifies that the character bag argument for [string-trim](resources/HyperSpec/Body/f_stg_tr.htm) can be:
- A string (most common case)
- A list of characters
- A vector of characters

For WasmGC implementation, all cases reduce to iterating the character bag and comparing bytes.

### Implementation Pattern
```wat
;; For each byte in string to potentially trim:
;;   For each character in character-bag:
;;     If byte == char-code(character), mark as trim candidate
;;     Use nested loop or early-exit block pattern
```

### Alternatives Considered
| Alternative | Rejected Because |
|-------------|------------------|
| Hash set lookup | Overhead not justified for typical small character bags |
| Bitmap for ASCII | Only covers ASCII; doesn't handle Unicode character bags |
| Pre-sorted binary search | Sorting overhead exceeds linear scan for small bags |

---

## 2. Trim Algorithm Implementation

### Decision: Two-pointer approach with subseq

### Rationale
The standard trim algorithm:
1. Scan from start to find first non-trim character → `left-index`
2. Scan from end to find last non-trim character → `right-index`
3. Extract substring from `left-index` to `right-index + 1`

This matches existing `subseq` usage patterns in Clysm and requires only O(n) character bag checks per trimmed character.

### Implementation Pattern
```lisp
;; string-trim pseudo-algorithm
(let* ((len (length string))
       (start (or explicit-start 0))
       (end (or explicit-end len))
       (left (find-first-non-trim-from-left string char-bag start end))
       (right (find-first-non-trim-from-right string char-bag start end)))
  (if (> left right)
      ""  ; All characters were trimmed
      (subseq string left (1+ right))))
```

### Wasm Loop Pattern (from existing string-upcase)
```wat
(block $break
  (loop $continue
    ;; ... check byte membership in char-bag ...
    (br_if $break (not-in-bag))
    (local.set $idx (i32.add (local.get $idx) (i32.const 1)))
    (br_if $continue (i32.lt_u (local.get $idx) (local.get $end)))
  )
)
```

---

## 3. Destructive vs Non-Destructive Functions

### Decision: nstring functions modify in-place using array.set

### Rationale
ANSI CL [nstring-upcase](resources/HyperSpec/Body/f_stg_up.htm) et al. are defined as:
- Modify the original string (destructive)
- Return the same string object

The `$string` type is defined as mutable (`mutable: t` in gc-types.lisp), so `array.set` can be used for in-place modification.

### Key Difference from Non-Destructive
| Aspect | Non-destructive (string-upcase) | Destructive (nstring-upcase) |
|--------|--------------------------------|------------------------------|
| Array allocation | `array.new_default` + copy | None |
| Modification target | New array | Original array |
| Return value | New string object | Same string object |
| Wasm instruction | `array.set` on new | `array.set` on original |

### Implementation Pattern for nstring-upcase
```wat
;; Loop from start to end
(loop $continue
  (local.set $byte (array.get_u $string (local.get $str) (local.get $idx)))
  ;; Convert case
  (if (i32.and (i32.ge_u (local.get $byte) (i32.const 97))
               (i32.le_u (local.get $byte) (i32.const 122)))
    (then
      (array.set $string (local.get $str) (local.get $idx)
                 (i32.sub (local.get $byte) (i32.const 32)))))
  ;; Increment and continue
  (local.set $idx (i32.add (local.get $idx) (i32.const 1)))
  (br_if $continue (i32.lt_u (local.get $idx) (local.get $end)))
)
;; Return original string
(local.get $str)
```

---

## 4. Existing Patterns to Reuse

### From `func-section.lisp`

| Pattern | Location | Reuse For |
|---------|----------|-----------|
| Keyword argument parsing | `compile-make-string` (lines 15069-15138) | `:start`/`:end` extraction |
| String loop iteration | `compile-string-upcase` (lines 15177-15249) | Byte scanning |
| Case conversion logic | `compile-char-upcase` (lines 13405-13430) | nstring-capitalize word boundary detection |
| Array creation | `compile-make-string` | Trim result string allocation |
| Bounding index validation | `validate-bounding-indices` in sequences-util.lisp | `:start`/`:end` bounds checking |

### Dispatcher Registration Pattern
New functions must be registered in `compile-call`:
```lisp
;; In compile-call dispatcher (around line 1132)
((member fn '(string-trim string-left-trim string-right-trim))
 (compile-string-trim form env))
((member fn '(nstring-upcase nstring-downcase nstring-capitalize))
 (compile-nstring-case form env))
```

---

## 5. Unicode Considerations

### Decision: ASCII-only optimization with UTF-8 pass-through

### Rationale
- Current Clysm string functions (string-upcase, etc.) only convert ASCII a-z/A-Z
- Character bags are typically ASCII whitespace or punctuation
- Non-ASCII bytes (>127) in UTF-8 encoding pass through unchanged
- This matches SBCL and most CL implementations

### Edge Cases
- Multi-byte UTF-8 characters: Trim by byte, not character (preserves UTF-8 validity)
- Character bag with non-ASCII: Compare full character codes
- Mixed ASCII/non-ASCII trim target: Only ASCII bytes matching bag are trimmed

---

## Summary of Decisions

| Topic | Decision |
|-------|----------|
| Character bag handling | Inline byte-by-byte membership check |
| Trim algorithm | Two-pointer with subseq |
| nstring implementation | In-place array.set modification |
| Keyword args | Reuse existing parsing patterns |
| Unicode | ASCII optimization, UTF-8 pass-through |

All decisions align with existing Clysm patterns and ANSI CL specification requirements.
