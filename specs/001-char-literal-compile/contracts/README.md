# Contracts: Character Literal Compilation Support

**Branch**: `001-char-literal-compile` | **Date**: 2025-12-31

## No External API Contracts

This feature modifies an internal compiler function (`compile-quoted-element`) and does not expose any new external APIs. Therefore, no OpenAPI/GraphQL schemas are required.

## Internal Function Contract

### compile-quoted-element

**Location**: `src/clysm/compiler/codegen/func-section.lisp`

**Extended Input Contract**:
| Input Type | Condition | Output |
|------------|-----------|--------|
| character | `(characterp elem)` | `((:i32.const <char-code>) :ref.i31)` |

**Character Code Mapping**:
- Input: Any valid Common Lisp character object
- Output: List of Wasm instructions encoding the character's code point as i31ref

**Example**:
```lisp
;; Input
#\Space

;; Output
((:I32.CONST 32) :REF.I31)
```

## Wasm Output Contract

Generated Wasm for character literals must:
1. Use `i32.const` with the character's Unicode code point
2. Follow immediately with `ref.i31` instruction
3. Pass `wasm-tools validate` without errors
