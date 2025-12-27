# Quickstart: LOOP Macro Implementation

**Feature**: 029-loop-macro
**Date**: 2025-12-27

## Development Setup

```bash
# Enter development environment
nix develop

# Run existing tests to verify baseline
rove clysm-tests.asd
```

## Key Files

| Purpose | File |
|---------|------|
| LOOP expander implementation | `src/clysm/lib/macros.lisp` |
| Existing iteration macros (reference) | `src/clysm/lib/macros.lisp:120-300` |
| Tagbody compilation | `src/clysm/compiler/codegen/func-section.lisp:4160-4341` |
| ANSI LOOP tests | `ansi-test/iteration/loop*.lsp` |

## Implementation Order (TDD)

### Phase 1: Core Iteration (P1)

1. **FOR arithmetic** - `(loop for i from 1 to 10 do ...)`
   ```lisp
   ;; Test first
   (deftest loop-for-arithmetic
     (ok (equal (loop for i from 1 to 3 collect i)
                '(1 2 3))))
   ```

2. **FOR IN list** - `(loop for x in list do ...)`
   ```lisp
   (deftest loop-for-in
     (ok (equal (loop for x in '(a b c) collect x)
                '(a b c))))
   ```

3. **COLLECT** - accumulate results
   ```lisp
   (deftest loop-collect
     (ok (equal (loop for i from 1 to 3 collect (* i i))
                '(1 4 9))))
   ```

### Phase 2: More Iteration & Accumulation (P1)

4. **FOR ON list** - iterate over tails
5. **FOR ACROSS vector** - iterate over vector elements
6. **SUM/COUNT** - numeric accumulation
7. **MAXIMIZE/MINIMIZE** - extrema

### Phase 3: Control Flow (P2)

8. **WHILE/UNTIL** - termination conditions
9. **IF/WHEN/UNLESS** - conditional clauses
10. **RETURN** - early exit
11. **WITH** - local variables
12. **INITIALLY/FINALLY** - prologue/epilogue

### Phase 4: Advanced (P2-P3)

13. **ALWAYS/NEVER/THEREIS** - boolean aggregation
14. **NAMED** - named loops
15. **Multiple FOR clauses** - parallel stepping
16. **LOOP-FINISH** - normal termination

## Expansion Pattern

All LOOP forms expand to this structure:

```lisp
;; Input
(loop for i from 1 to 3 collect (* i i))

;; Expansion
(let ((#:acc-head (cons nil nil))
      (#:acc-tail #:acc-head)
      (#:i 1)
      (#:limit 3))
  (block nil
    (tagbody
     #:loop-start
     (when (> #:i #:limit) (go #:loop-end))
     (let ((#:val (* #:i #:i)))
       (rplacd #:acc-tail (cons #:val nil))
       (setq #:acc-tail (cdr #:acc-tail)))
     (setq #:i (+ #:i 1))
     (go #:loop-start)
     #:loop-end)
    (cdr #:acc-head)))
```

## Testing Strategy

```bash
# Unit tests (clause parsing, expansion)
rove tests/unit/loop-test.lisp

# Contract tests (Wasm validation)
rove tests/contract/loop-wasm-test.lisp

# Integration tests (end-to-end)
rove tests/integration/loop-ansi-test.lisp

# All tests
nix flake check
```

## Common Patterns

### Adding a New Clause Type

1. Define structure in data model section of macros.lisp
2. Add parser case in `parse-loop-clause`
3. Add generator in `generate-clause-code`
4. Add tests for the clause

### Debugging Expansion

```lisp
;; See expanded form
(macroexpand-1 '(loop for i from 1 to 3 collect i))
```

## Error Messages

Use clear, actionable messages:

```lisp
(error "LOOP: conflicting accumulation types COLLECT and SUM. ~
        Use INTO to specify separate accumulators.")

(error "LOOP: FOR ~S requires a numeric FROM value, got ~S"
       var from-form)
```

## Performance Notes

- Use tail-pointer for COLLECT (O(n) not O(n^2))
- Inline termination tests for :simple-loop strategy
- Parallel stepping uses psetq (single traversal)
