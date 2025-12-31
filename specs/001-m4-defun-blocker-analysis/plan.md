# Implementation Plan: Phase 13D M4 - DEFUN Blocker Analysis and Resolution

**Branch**: `001-m4-defun-blocker-analysis` | **Date**: 2025-12-31 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-m4-defun-blocker-analysis/spec.md`

## Summary

Implement detailed error logging and pattern classification for DEFUN compilation failures, add missing lambda-list features (&aux codegen, &allow-other-keys runtime), and achieve 100% compilation for backend/ and reader/ modules. Target: compilation rate ≥35%, DEFUN errors ≤15,000.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, rove (testing)
**Storage**: N/A (in-memory compilation, JSON reports to dist/)
**Testing**: rove test framework, wasm-tools validate
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Project Type**: Single project (compiler)
**Performance Goals**: Compilation rate ≥35% (current: 22.21%)
**Constraints**: DEFUN errors ≤15,000 (current: 18,997)
**Scale/Scope**: 26,671 total forms, 18,997 DEFUN failures to analyze

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First Type System | ✅ PASS | All lambda-list codegen uses WasmGC types |
| II. Lisp Object Representation | ✅ PASS | No changes to NIL/UNBOUND representation |
| III. Function/Closure Strategy | ✅ PASS | &aux initializations use closure environment |
| IV. Wasm Control Flow | ✅ PASS | No changes to tail-call/exception handling |
| V. Shallow Binding | ✅ PASS | No changes to dynamic scope implementation |
| VI. Tiered Eval/JIT | ✅ PASS | No changes to eval/compile model |
| VII. TDD (Non-negotiable) | ✅ PASS | Contract tests for error logging, unit tests for lambda-list |
| VIII. Nix-First Workflow | ✅ PASS | Uses existing nix develop environment |
| IX. ANSI CL Spec References | ✅ PASS | [defun](resources/HyperSpec/Body/m_defun.htm), [lambda-list](resources/HyperSpec/Body/03_da.htm) |

## Project Structure

### Documentation (this feature)

```text
specs/001-m4-defun-blocker-analysis/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   ├── ast.lisp                    # Lambda-list parsing (already has &aux, &allow-other-keys)
│   ├── directive.lisp              # Compile-time directives
│   └── codegen/
│       └── func-section.lisp       # DEFUN codegen (needs &aux codegen fix)
├── stage0/
│   └── error-analysis.lisp         # NEW: Error logging/classification
├── backend/                         # Target: 100% compilation
│   ├── leb128.lisp
│   ├── sections.lisp
│   ├── wasm-emit.lisp
│   └── wat-print.lisp
└── reader/                          # Target: 100% compilation
    ├── package.lisp
    ├── reader.lisp
    ├── tokenizer.lisp
    └── parser.lisp

tests/
├── contract/
│   └── error-analysis/              # NEW: Error logging contracts
└── unit/
    └── lambda-list/                 # NEW: &aux, &allow-other-keys tests

build/
└── stage1-complete.lisp             # Enhanced with error logging

dist/
├── stage1-report.json               # Enhanced with error_patterns section
└── defun-errors.json                # NEW: Detailed DEFUN error log
```

**Structure Decision**: Single project layout following existing Clysm structure. New files for error analysis in stage0/, tests in appropriate subdirectories.

## Complexity Tracking

> No Constitution violations requiring justification.

| Area | Complexity | Justification |
|------|------------|---------------|
| Error classification | Low | Simple string pattern matching on error messages |
| &aux codegen | Medium | Extends existing optional/key parameter handling |
| &allow-other-keys | Low | Flag already parsed, needs runtime handling |
