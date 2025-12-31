# Research: Division/Rounding Function Primitives

**Created**: 2025-12-31
**Purpose**: Document research findings for implementing floor/ceiling/round primitives

## 1. ANSI CL Rounding Function Semantics

### Reference
- [floor, ceiling, truncate, round](resources/HyperSpec/Body/f_floorc.htm)
- [ffloor, fceiling, ftruncate, fround](resources/HyperSpec/Body/f_floorc.htm)

### Semantics

All functions accept 1 or 2 arguments: `(op number &optional divisor)`
- If divisor omitted, defaults to 1
- Returns 2 values: quotient and remainder
- Invariant: `number = quotient * divisor + remainder`

| Function | Rounding Direction | Quotient Type | Remainder Sign |
|----------|-------------------|---------------|----------------|
| floor | Toward -∞ | integer | Same as divisor |
| ceiling | Toward +∞ | integer | Opposite to divisor |
| truncate | Toward 0 | integer | Same as number |
| round | Nearest (banker's) | integer | Varies |
| ffloor | Toward -∞ | float | Same as divisor |
| fceiling | Toward +∞ | float | Opposite to divisor |
| fround | Nearest (banker's) | float | Varies |

### Decision: Follow ANSI CL exactly
**Rationale**: ANSI CL is the target specification; deviating would break existing code.

## 2. Wasm F64 Rounding Instructions

### Available Instructions

| Wasm Instruction | Opcode | Behavior |
|-----------------|--------|----------|
| `f64.floor` | 0x9B | Round toward -∞ |
| `f64.ceil` | 0x9C | Round toward +∞ |
| `f64.trunc` | 0x9D | Round toward 0 |
| `f64.nearest` | 0x9E | Round to nearest (ties to even) |

### Mapping to CL Functions

| CL Function | Wasm Instruction |
|-------------|-----------------|
| floor | f64.floor |
| ceiling | f64.ceil |
| round | f64.nearest |
| truncate | f64.trunc (already implemented) |

### Decision: Use Wasm native instructions directly
**Rationale**: Wasm instructions implement IEEE 754 semantics which aligns with ANSI CL requirements. Direct use avoids unnecessary computation.

## 3. Existing Truncate Implementation Pattern

### Location
`src/clysm/compiler/codegen/func-section.lisp:3485-3495`

### Current Implementation

```lisp
(defun compile-truncate (args env)
  "Compile truncate division."
  (when (< (length args) 2)
    (error "truncate requires two arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.div_s
     :ref.i31)))
```

### Limitations of Current Pattern
1. Only handles integer arguments (uses i31ref)
2. Only returns single value (quotient)
3. No remainder calculation
4. No single-argument form support

### Decision: Extend pattern with float handling and multiple values
**Rationale**: The existing pattern provides a foundation but needs significant extension for full ANSI CL compliance.

## 4. Multiple Values Mechanism

### Global Variables (from codebase analysis)

| Global Index | Name | Type | Purpose |
|-------------|------|------|---------|
| 2 | $mv_count | i32 | Number of values returned |
| 3 | $mv_buffer | ref $mv_array | Array for secondary values |

### Pattern for Returning 2 Values

From `compile-values` analysis:
1. Set `$mv_count` to 2
2. Store secondary value (remainder) in `$mv_buffer[0]`
3. Return primary value (quotient) on stack

```wasm
;; Set mv-count = 2
(i32.const 2)
(global.set $mv_count)
;; Store remainder in mv-buffer[0]
(global.get $mv_buffer)
(i32.const 0)
<remainder>
(array.set $mv_array)
;; Return quotient on stack
<quotient>
```

### Decision: Use existing multiple values infrastructure
**Rationale**: The mv-count/mv-buffer mechanism is already established and tested (Feature 025).

## 5. Type Handling Strategy

### Argument Types

The functions must handle:
1. **Both integers**: Use i32.div_s + compute remainder via multiplication
2. **Either or both floats**: Convert to f64, use f64 instructions
3. **Single argument**: Treat as `(op number 1)`

### Return Type Rules (per ANSI CL)

| Function Family | Quotient Type | Remainder Type |
|----------------|---------------|----------------|
| floor/ceiling/round | Always integer | Same as arguments |
| ffloor/fceiling/fround | Always float | Same as arguments |

### Decision: Type dispatch at compile time when possible, runtime otherwise
**Rationale**: Compile-time dispatch avoids runtime overhead for statically typed cases. Runtime dispatch needed for polymorphic cases.

**Alternatives Considered**:
- Always use f64: Rejected because integer operations are faster and more precise
- Only support f64: Rejected because it violates ANSI CL type preservation rules

## 6. Implementation Strategy

### Phase 1: Integer-only floor/ceiling/round (P1)

Extend `compile-truncate` pattern:
1. Add floor/ceiling/round cases to builtin dispatcher
2. Implement integer division with correct rounding
3. Compute remainder: `remainder = dividend - quotient * divisor`
4. Return 2 values using mv mechanism

### Phase 2: Float support (P1)

1. Detect float arguments (via type inference or runtime check)
2. Convert to f64 if needed
3. Apply f64.floor/f64.ceil/f64.nearest
4. Convert quotient back to integer (for floor/ceiling/round)
5. Compute remainder in f64

### Phase 3: Single-argument forms (P2)

1. Check argument count in compile functions
2. If single argument: synthesize divisor = 1
3. Apply two-argument logic

### Phase 4: f-variants (P2)

1. Same as Phase 2 but keep quotient as float
2. Package as $float struct for Lisp representation

## 7. Test Strategy

### Unit Tests (per Constitution VII)

1. **Integer cases**: `(floor 7 2)` → 3, 1
2. **Negative integers**: `(floor -7 2)` → -4, 1
3. **Float cases**: `(floor 7.5 2.0)` → 3, 1.5
4. **Single-arg**: `(floor 3.7)` → 3, 0.7
5. **Edge cases**: Division by zero, NaN, infinity

### Contract Tests

1. Generated Wasm passes `wasm-tools validate`
2. Output matches expected instruction sequence

## Summary of Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| CL Semantics | Follow exactly | Target specification compliance |
| Wasm Instructions | Use native f64.floor/ceil/nearest | IEEE 754 alignment, performance |
| Multiple Values | Use existing mv mechanism | Reuse tested infrastructure |
| Type Strategy | Compile-time dispatch preferred | Performance, type safety |
| Implementation Order | Integer → Float → Single-arg → f-variants | Incremental complexity |
