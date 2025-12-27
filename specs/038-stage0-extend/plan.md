# Implementation Plan: Stage 0 Capability Extension

**Branch**: `038-stage0-extend` | **Date**: 2025-12-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/038-stage0-extend/spec.md`

## Summary

Extend Stage 0 bootstrap capabilities to increase compilation rate from 1.6% (14/849 forms) to 50%+ (427+ forms). Key additions:
1. **defconstant/defparameter**: Compile to Wasm globals with constant folding
2. **define-condition**: Expand to defclass forms before compilation
3. **declare/proclaim**: Skip without compilation failure
4. **defstruct**: Expand to constructor/accessor functions
5. **Error reporting**: Operator-grouped failure statistics

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compilation; WasmGC for target output
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing); existing clysm/compiler, clysm/validation, clysm/clos modules
**Storage**: N/A (file-based: source files → single .wasm binary)
**Testing**: rove for unit/contract/integration tests; wasm-tools validate for Wasm verification
**Target Platform**: WasmGC (WebAssembly with Garbage Collection proposal)
**Project Type**: Single project (Common Lisp compiler with Wasm output)
**Performance Goals**: Bootstrap completion in reasonable time (no specific target; build tool, not runtime)
**Constraints**: Output must pass `wasm-tools validate`; must use WasmGC types only (no linear memory)
**Scale/Scope**: 849 forms across 41 modules; target 50%+ compilation rate

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First | ✅ PASS | All new types (globals for constants) use GC types |
| II. Lisp Object Representation | ✅ PASS | NIL/UNBOUND handling preserved; constants use existing type representations |
| III. Function/Closure Strategy | ✅ PASS | defstruct expansion generates standard defuns |
| IV. Wasm Control Flow | ✅ PASS | No new control flow patterns required |
| V. Shallow Binding | ✅ PASS | defparameter continues to use special variable mechanism |
| VI. Tiered Eval/JIT | N/A | Bootstrap is compile-time only |
| VII. TDD | ✅ REQUIRED | All features must have tests first |
| VIII. Nix-First | ✅ PASS | `nix flake check` must pass after implementation |

**Gate Status**: ✅ PASSED - No violations detected.

## Project Structure

### Documentation (this feature)

```text
specs/038-stage0-extend/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (internal APIs)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                    # Add ast-defconstant struct and parser
│   └── codegen/
│       └── func-section.lisp       # Add compile-defconstant, skip declare
├── lib/
│   └── defstruct-expand.lisp       # NEW: defstruct expansion logic
└── conditions/
    └── types.lisp                  # Already has define-condition (check expansion)

build/
├── bootstrap.lisp                  # Extend filter and expansion logic
└── defstruct-expander.lisp         # NEW: Bootstrap-specific defstruct expansion

tests/
├── unit/
│   ├── defconstant-test.lisp       # NEW: Unit tests for defconstant
│   ├── defstruct-expand-test.lisp  # NEW: Unit tests for defstruct expansion
│   └── declare-skip-test.lisp      # NEW: Unit tests for declare handling
├── contract/
│   └── stage0-extend-test.lisp     # NEW: Wasm validation tests
└── integration/
    └── stage0-compile-rate-test.lisp # NEW: Compilation rate verification
```

**Structure Decision**: Single project structure. All new code integrates with existing src/clysm/ modules.

## Complexity Tracking

No violations - complexity tracking not required.

## Implementation Phases

### Phase 1: Core Infrastructure (P1 - defconstant/defparameter)

**Goal**: Support constant definitions with compile-time evaluation

1. Add `ast-defconstant` struct to ast.lisp (mirrors ast-defparameter)
2. Add `parse-defconstant-form` function
3. Add `compile-defconstant` to func-section.lisp
4. Implement constant folding for arithmetic expressions
5. Add constant registry to bootstrap.lisp for cross-reference resolution

### Phase 2: Declaration Handling (P2 - declare/proclaim)

**Goal**: Skip declarations without compilation failure

1. Modify AST parser to recognize and skip declare forms in function/let bodies
2. Add proclaim to bootstrap filter as skippable
3. Update bootstrap.lisp `compilable-form-p` to include forms with declarations

### Phase 3: Condition Expansion (P2 - define-condition)

**Goal**: Expand define-condition to defclass

1. Add `expand-define-condition` function to bootstrap.lisp
2. Map condition parent types to existing class hierarchy
3. Handle :report option (register or skip for basic support)

### Phase 4: defstruct Expansion (P3)

**Goal**: Expand defstruct to constructor/accessor functions

1. Create `expand-defstruct` function
2. Generate `make-NAME` constructor (using list/vector backend)
3. Generate `NAME-SLOT` accessor functions
4. Handle slot defaults and :constructor option

### Phase 5: Error Reporting (P3)

**Goal**: Operator-grouped failure statistics

1. Extend `compile-result` struct with per-operator tracking
2. Add failure categorization by operator name
3. Generate percentage-based progress output

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Constant folding complexity | Limit to arithmetic + known constants; error on complex expressions |
| defstruct semantics incompatibility | Use simple list backend; document limitations |
| 50% target not achievable | Progressive measurement; identify additional blockers |
| Wasm validation failures | Run wasm-tools validate after each phase |

## Dependencies

- Feature 026 (CLOS Foundation): Provides defclass compilation for condition expansion
- Feature 037 (Cross-Compile Stage 0): Base bootstrap infrastructure
- Existing ast.lisp, func-section.lisp: Extension points for new forms

## Success Metrics

- [ ] SC-001: 50%+ forms compile (427+ of 849)
- [ ] SC-002: wasm-tools validate passes
- [ ] SC-003: All defconstant/defparameter forms compile
- [ ] SC-004: All define-condition forms compile
- [ ] SC-005: Functions with declare compile
- [ ] SC-006: Operator-grouped error report
- [ ] SC-007: Percentage progress display
