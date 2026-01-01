# Research: Wasm Local Instruction Binding

**Feature**: 001-wasm-local-binding
**Date**: 2026-01-01
**Status**: Complete

## Executive Summary

This research documents the root causes of 101 Stage 1 compilation errors across 5 error patterns. All issues are solvable through targeted fixes in the codegen layer and package exports.

## Research Tasks

### R1: LOCAL.SET/LOCAL.TEE Unbound Variable Analysis

**Decision**: The `:local.set` and `:local.tee` keywords need explicit handling in the self-compilation path.

**Rationale**:
- The opcode table at `func-section.lisp:361-367` correctly maps keywords to opcodes:
  ```lisp
  (:local.get . #x20)
  (:local.set . #x21)
  (:local.tee . #x22)
  ```
- The emitter at `compiler.lisp:969-974` handles these instructions correctly
- Error occurs during self-compilation when func-section.lisp code containing `:local.set` literals is compiled
- Backquote expressions like `(list (list :local.set temp-local))` at line 1637 become unquoted during macro expansion

**Alternatives Considered**:
1. **Constant binding approach**: Define constants `+local-set+` etc. - Rejected: Would require extensive code changes
2. **Reader macro approach**: Create reader syntax for instructions - Rejected: Overcomplicates the solution
3. **Proper quoting in backquote**: Ensure `:local.set` remains a keyword - Selected: Minimal change, follows Lisp conventions

**Implementation**: Add handling in the AST literal processing to recognize and preserve Wasm instruction keywords during compilation.

### R2: ADVANCE-TOKEN Function Export

**Decision**: Export ADVANCE-TOKEN from clysm package and register in *runtime-function-table*.

**Rationale**:
- Function exists at `src/clysm/reader/parser.lisp:34`
  ```lisp
  (defun advance-token (state)
    "Consume current token and return it."
    ...)
  ```
- Function takes 1 argument (parser state)
- Currently only visible in internal clysm/reader package
- 22 compilation failures (P027) reference this function

**Alternatives Considered**:
1. **Inline codegen for ADVANCE-TOKEN**: Generate Wasm directly - Rejected: Complex state manipulation
2. **Runtime dispatch**: Register in *runtime-function-table* - Selected: Consistent with existing patterns
3. **Package re-export only**: Just export without runtime registration - Rejected: Would fail during Wasm dispatch

**Implementation**:
1. Add to clysm package exports in package.lisp
2. Register in *runtime-function-table* with arity 1

### R3: EMIT-MODULE-HEADER Function Export

**Decision**: Re-export EMIT-MODULE-HEADER from main clysm package.

**Rationale**:
- Function exists at `src/clysm/backend/wasm-emit.lisp:10`
  ```lisp
  (defun emit-module-header ()
    "Emit the Wasm module header: magic number and version.
     Returns an 8-byte vector."
    ...)
  ```
- Already exported from `clysm/backend/wasm-emit` package (package.lisp:72)
- 10 compilation failures (P143) reference this function
- Function takes 0 arguments

**Alternatives Considered**:
1. **Use fully-qualified name**: clysm/backend/wasm-emit:emit-module-header - Rejected: Inconsistent with other exports
2. **Re-export from clysm**: Add to main package exports - Selected: Consistent API

**Implementation**:
1. Add to clysm package exports in package.lisp
2. Register in *runtime-function-table* with arity 0

### R4: AST-TAGBODY Serialization

**Decision**: Add AST-TAGBODY handling in the literal/structure compilation path.

**Rationale**:
- Structure defined at `src/clysm/compiler/ast.lisp:360`
  ```lisp
  (defstruct (ast-tagbody (:include ast-node) (:conc-name ast-tagbody-))
    "Tagbody node for goto-based control flow."
    (tags nil :type list)
    (segments nil :type list))
  ```
- Compilation handler exists at `func-section.lisp:442-443`
  ```lisp
  (clysm/compiler/ast:ast-tagbody
   (compile-tagbody ast env))
  ```
- 9 errors (P943) show raw #S(...) structure appearing in output
- Issue: AST-TAGBODY structures appearing as literals (quoted) are not being handled

**Alternatives Considered**:
1. **Error on quoted TAGBODY AST**: Signal compilation error - Rejected: Would break valid code patterns
2. **Transform during macro expansion**: Convert earlier in pipeline - Rejected: May lose source information
3. **Handle in literal compilation**: Add case to literal handler - Selected: Targeted fix

**Implementation**: Add AST-TAGBODY case to the literal/structure handling in codegen.

### R5: *runtime-function-table* Pattern Analysis

**Decision**: Follow established pattern for new function registrations.

**Rationale**:
- Table defined at `func-section.lisp:69`
- Registration pattern:
  ```lisp
  (register-runtime-function 'symbol :$runtime-name-rt arity)
  ```
- Existing registrations for I/O, list, sequence, package functions
- Functions are called via `:call` to runtime function index

**Pattern for new registrations**:
```lisp
;; Parser functions (001-wasm-local-binding)
(register-runtime-function 'clysm:advance-token :$advance-token-rt 1)

;; Backend functions (001-wasm-local-binding)
(register-runtime-function 'clysm:emit-module-header :$emit-module-header-rt 0)
```

## Summary of Decisions

| Area | Decision | Rationale |
|------|----------|-----------|
| LOCAL.SET/LOCAL.TEE | Proper keyword quoting in codegen | Minimal change, correct Lisp semantics |
| ADVANCE-TOKEN | Export + runtime table registration | Consistent with existing patterns |
| EMIT-MODULE-HEADER | Re-export + runtime table registration | Already exported from sub-package |
| AST-TAGBODY | Add literal compilation handler | Targeted fix for structure serialization |

## Codebase Locations

| Component | File | Line |
|-----------|------|------|
| Opcode table | src/clysm/compiler/codegen/func-section.lisp | 361-367 |
| Instruction emitter | src/clysm/compiler/compiler.lisp | 969-974 |
| ADVANCE-TOKEN | src/clysm/reader/parser.lisp | 34 |
| EMIT-MODULE-HEADER | src/clysm/backend/wasm-emit.lisp | 10 |
| AST-TAGBODY struct | src/clysm/compiler/ast.lisp | 360 |
| TAGBODY compilation | src/clysm/compiler/codegen/func-section.lisp | 442-443, 9611-9615 |
| Runtime function table | src/clysm/compiler/codegen/func-section.lisp | 69-88 |
| Package exports | src/clysm/package.lisp | (multiple sections) |

## Expected Impact

Fixing all 5 error patterns should eliminate 101 compilation failures:
- P221 (LOCAL.SET): 40 → 0
- P987 (LOCAL.TEE): 20 → 0
- P027 (ADVANCE-TOKEN): 22 → 0
- P143 (EMIT-MODULE-HEADER): 10 → 0
- P943 (AST-TAGBODY): 9 → 0

Current coverage: 19.00% (4580/24107 adjusted)
Expected coverage: ~25%+ (assuming 101 additional successes)
