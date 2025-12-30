# Research: Character Literal Compilation Support

**Branch**: `001-char-literal-compile` | **Date**: 2025-12-31

## Research Questions

### Q1: How are character codes represented in Common Lisp?

**Decision**: Use [char-code](resources/HyperSpec/Body/f_char_c.htm) to extract Unicode code points from character objects.

**Rationale**:
- `char-code` returns a non-negative integer representing the character's code point
- ANSI CL guarantees `char-code` returns values in range `[0, char-code-limit)`
- SBCL's `char-code-limit` is 1114112 (Unicode range + 1)

**Alternatives considered**:
- `sxhash` on characters: Rejected because it doesn't preserve identity semantics (different characters could hash to same value)
- Custom encoding: Rejected because `char-code` is the standard, portable approach

### Q2: What is the i31ref encoding pattern used for integers?

**Decision**: Follow the existing integer pattern in `compile-quoted-element`:
```lisp
(list (list :i32.const <value>) :ref.i31)
```

**Rationale**:
- Consistent with existing fixnum handling (line 516 of func-section.lisp)
- Character codes are always non-negative, so no sign handling needed
- All Unicode code points (0-1114111) fit within i31ref range (max 1073741823)

**Alternatives considered**:
- Separate character type: Rejected to avoid adding Wasm type complexity; i31ref is sufficient
- Heap-allocated character objects: Rejected because characters are immutable values like fixnums

### Q3: Do we need range checking for character codes?

**Decision**: No range check required.

**Rationale**:
- Unicode code points max at 1114111
- i31ref positive range max at 1073741823
- 1114111 < 1073741823, so all valid character codes fit
- The Lisp reader already validates character literals before they reach the compiler

**Alternatives considered**:
- Defensive range check: Could add but unnecessary overhead for impossible case

### Q4: Where should the characterp branch be inserted?

**Decision**: Insert between the `complexp` branch (line 520) and the `listp` branch (line 521).

**Rationale**:
- Keeps numeric types together (integerp, ratio, floatp, complexp)
- Characters are scalar values like numbers, logically grouped with them
- Maintains alphabetical ordering of type predicates is not required; logical grouping preferred

**Alternatives considered**:
- After symbolp: Rejected because characters are values, not identifiers
- At the beginning: Rejected because null check must remain first for efficiency

## Implementation Pattern

Based on research, the implementation is:

```lisp
;; Add this branch after ((complexp elem) ...)
((characterp elem)
 (list (list :i32.const (char-code elem)) :ref.i31))
```

## Test Strategy

Per Constitution Principle VII (TDD), tests must be written first:

1. **Unit tests** (compile-quoted-element function)
   - Test `#\Space` returns `((:i32.const 32) :ref.i31)`
   - Test `#\Tab` returns `((:i32.const 9) :ref.i31)`
   - Test `#\Newline` returns `((:i32.const 10) :ref.i31)`
   - Test `#\a` returns `((:i32.const 97) :ref.i31)`
   - Test `#\A` returns `((:i32.const 65) :ref.i31)`

2. **Contract tests** (Wasm output validation)
   - Compile `'(#\Space #\Tab)` and validate with wasm-tools
   - Compile `(member x '(#\Space #\Tab #\Newline))` and validate

3. **Integration tests** (Stage 1 generation)
   - Run `sbcl --load build/stage1-complete.lisp`
   - Verify compilation rate improvement

## References

- [characterp](resources/HyperSpec/Body/f_chp.htm) - Character type predicate
- [char-code](resources/HyperSpec/Body/f_char_c.htm) - Character to integer conversion
- [char-code-limit](resources/HyperSpec/Body/v_char_c.htm) - Maximum character code + 1
- Existing pattern: `src/clysm/compiler/codegen/func-section.lisp:513-517`
