# Quickstart: ANSI Sequence Generic Functions

**Feature**: 001-ansi-sequence-functions (Phase 15B)
**Date**: 2025-12-31

## Prerequisites

- SBCL 2.4+ with clysm loaded
- Development environment via `nix develop`
- Phase 15A complete (list-ops.lisp implemented)

## Getting Started

### 1. Enter Development Environment

```bash
cd /home/user/src/clysm-workbench/clysm3
nix develop
```

### 2. Load Clysm System

```lisp
(asdf:load-system :clysm)
```

### 3. Implementation Files

New files to create:
- `src/clysm/lib/sequences.lisp` - Main implementation
- `src/clysm/lib/sequences-util.lisp` - Shared utilities (optional)

### 4. Quick Test Pattern

For each function, follow TDD cycle:

```lisp
;; 1. Write test first
(deftest test-count-basic
  (testing "count returns correct count"
    (ok (= 3 (count* 1 '(1 2 1 3 1))))))

;; 2. Run test (expect failure)
(asdf:test-system :clysm)

;; 3. Implement function
(defun count* (item sequence &key (test #'eql) (key #'identity)
                                  (start 0) end from-end)
  ...)

;; 4. Run test (expect pass)
(asdf:test-system :clysm)
```

## Implementation Order

Recommended order based on dependencies:

1. **Utility functions** (bounds checking)
2. **Count family** (simplest, establishes patterns)
3. **Find/Position family** (similar structure)
4. **Mismatch/Search** (two-sequence operations)
5. **Substitute/NSubstitute** (mutation patterns)
6. **Remove-duplicates/Delete-duplicates** (complex logic)
7. **Fill/Replace** (bulk operations)

## Key Patterns

### Pattern 1: Sequence Type Dispatch

```lisp
(defun count* (item sequence &key ...)
  (etypecase sequence
    (list   (count-list item sequence ...))
    (vector (count-vector item sequence ...))
    (string (count-string item sequence ...))))
```

### Pattern 2: Bounds Validation

```lisp
(let* ((len (length sequence))
       (bounds (validate-bounding-indices start end len))
       (real-start (car bounds))
       (real-end (cdr bounds)))
  ...)
```

### Pattern 3: :from-end Handling

```lisp
;; Collect matches, select based on from-end
(let ((matches (collect-matches ...)))
  (if from-end
      (last-n matches count)
      (first-n matches count)))
```

## Running Tests

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific sequence tests
sbcl --eval "(rove:run :clysm/tests/unit/sequences)"

# Validate Wasm output
wasm-tools validate dist/clysm-stage1.wasm
```

## Useful REPL Commands

```lisp
;; Test a single function interactively
(count* 'a '(a b a c a))   ; => 3
(find* 'b '(a b c d))      ; => B
(position* 'c '(a b c d))  ; => 2

;; Test with keywords
(count* 1 '(1 2 3) :start 1)           ; => 0
(find* "B" '("a" "b") :test #'string-equal)  ; => "b"

;; Test :from-end
(position* 'a '(a b a c) :from-end t)  ; => 2
```

## HyperSpec Quick Reference

| Function | HyperSpec |
|----------|-----------|
| count | [f_countc.htm](../../resources/HyperSpec/Body/f_countc.htm) |
| find | [f_find_.htm](../../resources/HyperSpec/Body/f_find_.htm) |
| position | [f_pos_p.htm](../../resources/HyperSpec/Body/f_pos_p.htm) |
| mismatch | [f_mismat.htm](../../resources/HyperSpec/Body/f_mismat.htm) |
| search | [f_search.htm](../../resources/HyperSpec/Body/f_search.htm) |
| substitute | [f_substc.htm](../../resources/HyperSpec/Body/f_substc.htm) |
| remove-duplicates | [f_rm_dup.htm](../../resources/HyperSpec/Body/f_rm_dup.htm) |
| fill | [f_fill.htm](../../resources/HyperSpec/Body/f_fill.htm) |
| replace | [f_replac.htm](../../resources/HyperSpec/Body/f_replac.htm) |

## Common Issues

### Issue: :test and :test-not both supplied
**Solution**: Signal an error per ANSI spec

### Issue: Invalid bounding indices
**Solution**: Use `validate-bounding-indices` helper

### Issue: NIL vs empty list confusion
**Solution**: Remember NIL is a singleton, not null (Constitution II)

## Next Steps

After implementation:
1. Run full test suite
2. Measure ANSI compliance rate (target: 60%+)
3. Update CLAUDE.md with completed features
