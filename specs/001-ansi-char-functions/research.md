# Research: Phase 16A - ANSI Character Functions

**Date**: 2025-12-31
**Feature**: 001-ansi-char-functions

## Research Questions

### Q1: Character Representation in WasmGC

**Decision**: Characters are represented as i31ref containing Unicode code points (i32).

**Rationale**:
- Consistent with existing implementation (`char-upcase`, `char-downcase`, etc.)
- i31ref provides efficient unboxed storage for values up to 2^30
- Unicode code points fit within i31 range (max U+10FFFF = 1,114,111)

**Alternatives Considered**:
- Boxed character struct: Rejected due to allocation overhead
- Linear memory strings: Rejected per Constitution Principle I (WasmGC-First)

### Q2: Standard Character Set Definition

**Decision**: The 96 standard characters per ANSI CL [13.1.7](../../resources/HyperSpec/Body/13_ag.htm):
- 26 uppercase letters (A-Z): code points 65-90
- 26 lowercase letters (a-z): code points 97-122
- 10 decimal digits (0-9): code points 48-57
- Space (32) and Newline (10)
- 32 semi-standard characters including punctuation

**Rationale**:
- ANSI CL specifies exactly 96 standard characters
- Implementation uses efficient range checks for letter/digit subsets
- Punctuation handled via lookup table or inline checks

**Alternatives Considered**:
- Extended Unicode support: Deferred to future phase (bootstrap focuses on ASCII)

### Q3: Character Name Mapping

**Decision**: Support 9 standard character names per ANSI CL:

| Name | Code Point | Alternate Names |
|------|------------|-----------------|
| Space | 32 | SP |
| Newline | 10 | NL, LF |
| Tab | 9 | HT |
| Return | 13 | CR |
| Page | 12 | FF |
| Backspace | 8 | BS |
| Rubout | 127 | DEL, Delete |
| Linefeed | 10 | (same as Newline) |
| Null | 0 | NUL |

**Rationale**:
- ANSI CL requires these names (implementation may add more)
- Case-insensitive lookup per specification
- Linefeed and Newline map to same code point (implementation-defined choice)

**Alternatives Considered**:
- Unicode character names (e.g., "LATIN CAPITAL LETTER A"): Deferred to future phase
- Numeric names (e.g., "U+0041"): Could add as extension

### Q4: digit-char Output Case

**Decision**: `digit-char` returns uppercase letters for radix > 10.

**Rationale**:
- ANSI CL specifies uppercase for radix > 10: `(digit-char 10 16)` → `#\A`
- Consistent with `format ~R` uppercase output
- Matches acceptance scenarios in spec

**Alternatives Considered**:
- Lowercase output: Would violate ANSI CL specification

### Q5: char-int vs char-code Relationship

**Decision**: `char-int` is equivalent to `char-code` in this implementation.

**Rationale**:
- ANSI CL allows `char-int` to include implementation-specific attributes
- This implementation has no character attributes beyond code point
- Simplifies implementation to identity function

**Alternatives Considered**:
- Include font/bit attributes: Not applicable to WasmGC target

### Q6: Error Handling for Invalid Inputs

**Decision**: Follow ANSI CL error signaling:
- `digit-char` with radix outside 2-36: Signal type error
- Predicate functions with non-character: Undefined behavior (may trap)

**Rationale**:
- ANSI CL specifies type errors for invalid radix
- Character predicates assume character input (type checking optional)

**Alternatives Considered**:
- Return NIL for invalid inputs: Would mask programming errors

## Best Practices Applied

### WasmGC Code Generation Patterns

From analysis of existing `compile-char-upcase`, `compile-alpha-char-p`, `compile-digit-char-p`:

1. **Character extraction**: `(:ref.cast :i31) :i31.get_s` converts i31ref to i32
2. **Range checks**: Use `i32.ge_s` / `i32.le_s` with constants
3. **Boolean logic**: Use `i32.mul` for AND, `i32.or` for OR
4. **Branching**: `(:if (:result :anyref)) ... :else ... :end`
5. **Return values**:
   - T as `(:i32.const 1) :ref.i31`
   - NIL as `(:ref.null :none)`
   - Character as `:ref.i31` wrapping i32

### String Operations for char-name/name-char

`char-name` and `name-char` require string operations:
- `char-name`: Build $string struct at runtime
- `name-char`: Compare input string against known names

Implementation options:
1. **Compile-time dispatch**: Generate inline comparisons for each name
2. **Runtime lookup table**: Store names in data section

**Decision**: Compile-time dispatch for efficiency (9 names is small)

## Dependencies and Integration

### Existing Functions to Leverage

| Function | Use Case |
|----------|----------|
| `compile-to-instructions` | Compile subexpressions |
| `env-add-local` | Create local variables |
| `get-nil-global` | Return NIL reference |
| `get-t-global` | Return T reference |
| `compile-string-literal` | Create string for char-name |

### Registration Requirements

Add to `*builtin-function-compilers*` alist:
- `graphic-char-p` → `compile-graphic-char-p`
- `standard-char-p` → `compile-standard-char-p`
- `both-case-p` → `compile-both-case-p`
- `char-name` → `compile-char-name`
- `name-char` → `compile-name-char`
- `digit-char` → `compile-digit-char`
- `char-int` → `compile-char-int`

## NEEDS CLARIFICATION Resolution

All technical context items resolved. No outstanding clarifications needed.
