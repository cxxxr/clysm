# Research: Arithmetic Primitives 1- and 1+

**Date**: 2025-12-31
**Feature**: 001-arithmetic-primitives

## Summary

Research completed to understand the existing primitive compilation pattern in Clysm. All NEEDS CLARIFICATION items resolved.

## Findings

### 1. Primitive Registration Pattern

**Decision**: Add `1-` and `1+` to the primitive list at func-section.lisp:725 (arithmetic operators section)

**Rationale**: The primitive list is organized by category. Arithmetic operators (`+`, `-`, `*`, `/`, `truncate`, `mod`, `rem`) are at lines 725-726. Adding `1-` and `1+` in this section maintains logical grouping.

**Alternatives considered**:
- Add at end of list: Rejected - breaks categorical organization
- Add in separate section: Rejected - unnecessary complexity for standard arithmetic

### 2. Compile Function Dispatch Pattern

**Decision**: Add cases to `compile-primitive-call` case statement at func-section.lisp:931

**Rationale**: All primitives dispatch through a single `case` statement in `compile-primitive-call`. The pattern is:
```lisp
(case op
  (+ (compile-arithmetic-op :i32.add args env 0))
  (- (if (= 1 (length args))
         (compile-unary-minus (first args) env)
         (compile-arithmetic-op :i32.sub args env nil)))
  ...
  (1- (compile-1- args env))
  (1+ (compile-1+ args env))
  ...)
```

**Alternatives considered**:
- Expand inline to `(- x 1)`: Rejected - would require modifying the expression before compilation, violating the direct compilation pattern

### 3. Wasm Instruction Sequence

**Decision**: Use i31ref arithmetic pattern from `compile-unary-minus`

**Rationale**: The existing pattern is proven and correct:
1. Compile argument to stack (produces anyref)
2. Cast to i31ref: `(:ref.cast :i31)`
3. Extract signed i32: `:i31.get_s`
4. Push constant 1: `(:i32.const 1)`
5. Perform operation: `:i32.sub` or `:i32.add`
6. Wrap back to i31ref: `:ref.i31`

**Alternatives considered**:
- Direct i32 operations without boxing: Rejected - violates WasmGC-First principle (all values are anyref)
- Float support: Deferred - current pattern handles fixnums only; float support would require type dispatch

### 4. Arity Validation

**Decision**: Validate exactly 1 argument at compile time

**Rationale**: ANSI CL specifies `1-` and `1+` as unary operators. Compile-time validation follows the pattern of `compile-lognot`:
```lisp
(when (/= (length args) 1)
  (error "1- requires exactly 1 argument"))
```

**Alternatives considered**:
- Runtime validation: Rejected - compile-time error is clearer and faster

### 5. HyperSpec Reference

**Decision**: Both functions are documented in the same HyperSpec page

**Location**: [resources/HyperSpec/Body/f_1pl_1_.htm](resources/HyperSpec/Body/f_1pl_1_.htm)

**Key specification points**:
- `(1+ number)` ≡ `(+ number 1)`
- `(1- number)` ≡ `(- number 1)`
- Return type: number

## No Unresolved Items

All technical questions have been resolved through code analysis. No external research required.
