# Research: CXR Compiler Macro Consolidation

**Branch**: `001-cxr-compiler-macro`
**Date**: 2026-01-03

## Overview

This feature requires minimal research - it is a straightforward code deduplication refactoring with no unknowns in the Technical Context.

## Decisions

### D1: Macro Design Pattern

**Decision**: Use `defmacro` with compile-time validation

**Rationale**:
- Standard Common Lisp macro pattern
- Validation at macroexpansion time catches errors early
- No runtime overhead
- Consistent with existing clysm codebase patterns

**Alternatives Considered**:
- Reader macro: Rejected - overkill for 12 usages
- Inline defun generator: Rejected - less idiomatic than defmacro

### D2: Operation String Format

**Decision**: Keep existing string format ("aa", "da", "dda")

**Rationale**:
- Already proven in compile-cxr-chain implementation
- Compact and readable
- No need to change working code

**Alternatives Considered**:
- Keyword list ((:car :car)): Rejected - more verbose, no benefit
- Reverse order string: Rejected - would require changes to compile-cxr-chain

### D3: Docstring Generation

**Decision**: Generate descriptive docstrings at macroexpansion time

**Rationale**:
- Maintains parity with existing hand-written docstrings
- Helps IDE/documentation tools
- Minimal implementation effort

**Alternatives Considered**:
- No docstrings: Rejected - loses documentation
- Static docstrings: Rejected - would require 12 separate strings anyway

### D4: Validation Approach

**Decision**: Use `assert` with restartable conditions

**Rationale**:
- Standard Common Lisp error handling
- Provides clear error messages
- Allows interactive recovery in REPL

**Alternatives Considered**:
- Silent failure: Rejected - would hide bugs
- Hard error without restart: Rejected - less flexible

## Technology Validation

| Technology | Version | Status | Notes |
|------------|---------|--------|-------|
| SBCL | 2.4+ | Available | Host compiler |
| defmacro | CL Standard | Available | Core feature |
| check-type | CL Standard | Available | Type validation |
| assert | CL Standard | Available | Condition validation |

## No Unknowns Remaining

All NEEDS CLARIFICATION markers from Technical Context have been resolved or were not applicable. The implementation can proceed directly to Phase 1 design artifacts.
