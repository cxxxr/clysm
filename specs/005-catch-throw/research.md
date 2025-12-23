# Research: Catch/Throw Dynamic Exception Handling

**Feature**: 005-catch-throw
**Date**: 2025-12-23

## Decision Summary

**Chosen Approach**: Use Wasm Exception Handling proposal (`try_table`/`throw`/`throw_ref`)

## Background

Common Lisp `catch/throw` provides dynamic non-local exit:
- `(catch 'tag body...)` - establishes a catch point with runtime-evaluated tag
- `(throw 'tag value)` - transfers control to nearest matching catch, returning value
- Tags are compared using `eq` (symbol identity)
- Throws propagate across function boundaries (cross-function unwinding)

## Options Analyzed

### Option 1: Wasm Exception Handling Proposal (SELECTED)

**How it works:**
- Use `try_table` to establish exception handlers
- Use `throw` to throw exceptions with a tag
- Use `exnref` type to hold caught exception references
- Exception tag contains (catch-tag-symbol, value) pair

**Pros:**
- Native Wasm mechanism designed for this use case
- Efficient stack unwinding handled by runtime
- Proper integration with host exceptions (JavaScript)
- Cross-function throws work automatically

**Cons:**
- Requires enabling `-W exceptions=y` in wasmtime
- Relatively new proposal (Phase 4, shipping in browsers)

**Code pattern:**
```wat
;; Define exception tag
(tag $lisp-throw (param anyref anyref))  ; (tag-symbol, value)

;; Catch form
(block $catch-exit (result anyref)
  (block $handler (result exnref)
    (try_table (catch $lisp-throw $handler)
      ;; body code
      (br $catch-exit)  ; normal exit
    )
  )
  ;; handler: exnref on stack
  ;; Extract tag and value, compare tag, rethrow or return value
)

;; Throw form
(throw $lisp-throw tag-symbol value)
```

### Option 2: Simulated with Locals and br

**How it works:**
- Store exception state in locals/globals
- Use existing block/br for control flow
- Requires explicit checking after each call

**Pros:**
- No dependency on new Wasm features
- Works with any Wasm runtime

**Cons:**
- Significant performance overhead (check after every call)
- Complex code generation
- Cross-function throws require global state and manual unwinding
- Breaks tail call optimization

**Rejected because:** Performance overhead is unacceptable for exception handling that should be rare-path. Cross-function throws become extremely complex.

### Option 3: CPS Transform

**How it works:**
- Transform code to continuation-passing style
- Pass exception continuations explicitly

**Pros:**
- Pure functional approach
- Works anywhere

**Cons:**
- Completely changes code generation model
- Massive complexity increase
- Destroys all other optimizations
- Not practical for a production compiler

**Rejected because:** Would require rewriting entire compiler.

## Decision: Use Wasm Exception Handling

**Rationale:**
1. **Wasmtime support confirmed**: Version 39.0.1 has `-W exceptions=y` flag
2. **Proper semantics**: Native exception handling matches Common Lisp catch/throw perfectly
3. **Cross-function works**: No special handling needed for throws across call boundaries
4. **unwind-protect integration**: `try_table` can catch exceptions, run cleanup, rethrow
5. **Future-proof**: This is the standardized Wasm exception handling approach

**Implementation impact:**
- Add `--wasm exceptions` flag to test helper's wasmtime invocation
- Define single exception tag `$lisp-throw` in module
- Generate `try_table` for `catch` forms
- Generate `throw` for `throw` forms
- Unwind-protect uses catch-all to run cleanup then rethrow

## Runtime Requirements

1. **Wasmtime flags**: Add `--wasm exceptions` to `run-wasm-bytes` in tests/helpers.lisp
2. **Exception tag**: Single tag `$lisp-throw` takes `(anyref anyref)` - catch tag symbol and value
3. **Tag matching**: Extract tag from exnref, compare with `eq`, rethrow if mismatch

## References

- [Wasm Exception Handling Proposal](https://github.com/WebAssembly/exception-handling)
- [Wasmtime EH Support PR](https://github.com/bytecodealliance/wasmtime/pull/11326)
- [try_table specification](https://github.com/WebAssembly/exception-handling/blob/main/proposals/exception-handling/Exceptions.md)
