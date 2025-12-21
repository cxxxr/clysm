# Quickstart: Clysm Development

**Date**: 2025-12-22
**Plan Reference**: [plan.md](./plan.md)
**Status**: Phase 1-2 complete, Phase 3 in progress

## Prerequisites

- Nix Package Manager (with flakes enabled)
- Git
- (Optional) direnv for automatic environment activation

## Getting Started

### 1. Clone and Enter Development Environment

```bash
git clone <repository-url> clysm
cd clysm

# Option A: Using direnv (recommended)
direnv allow

# Option B: Manual nix develop
nix develop
```

This will provide:
- SBCL (Steel Bank Common Lisp)
- wasm-tools
- wasmtime
- wabt (wat2wasm)

### 2. Verify Environment

```bash
sbcl --version
# => SBCL 2.4.x

wasm-tools --version
# => wasm-tools 1.x.x

wasmtime --version
# => wasmtime-cli 27.x.x
```

### 3. Load the Project

```bash
sbcl --load clysm.asd
```

```lisp
;; In SBCL REPL
(ql:quickload :clysm)
```

### 4. Run Tests

```bash
sbcl --eval "(asdf:test-system :clysm)" --quit
```

Or in REPL:

```lisp
(asdf:test-system :clysm)
```

### 5. Compile a Simple Expression

```lisp
;; Load Clysm
(ql:quickload :clysm)
(in-package :clysm)

;; Compile a simple expression
(compile-to-wasm '(+ 1 2) :output "output.wasm")
```

### 6. Validate and Run

```bash
# Validate the generated Wasm
wasm-tools validate output.wasm

# Run with wasmtime
wasmtime output.wasm
# => 3
```

---

## Project Structure

```
clysm/
├── flake.nix              # Nix development environment
├── clysm.asd              # ASDF system definition
├── src/clysm/
│   ├── backend/           # Wasm binary emission
│   ├── compiler/          # Compiler core
│   ├── reader/            # S-expression reader
│   ├── runtime/           # Runtime support
│   ├── eval/              # Dynamic evaluation
│   └── clos/              # Object system
├── tests/
│   ├── unit/              # Unit tests
│   ├── contract/          # Wasm validation tests
│   └── integration/       # End-to-end tests
└── specs/                 # Design documents
```

---

## Development Workflow

### TDD Cycle (Required)

1. **Write test first**
```lisp
(deftest test-fixnum-add
  (ok (= 3 (compile-and-run '(+ 1 2)))))
```

2. **Verify test fails**
```lisp
(rove:run :clysm/tests/arithmetic)
;; => FAIL
```

3. **Implement feature**

4. **Verify test passes**
```lisp
(rove:run :clysm/tests/arithmetic)
;; => PASS
```

5. **Refactor if needed**

6. **Run full test suite**
```bash
nix flake check
```

### Nix Flake Check (Required before commit)

```bash
nix flake check
```

This runs:
- All tests
- Wasm validation
- Lint checks

---

## Common Tasks

### Generate Empty Wasm Module

```lisp
(emit-empty-module :output "empty.wasm")
```

```bash
wasm-tools validate empty.wasm
wasm-tools print empty.wasm
```

### Debug WAT Output

```lisp
(compile-to-wat '(+ 1 2))
;; => "(module ...)"
```

### Run Specific Test

```lisp
(rove:run :clysm/tests/leb128)
```

### Profile Generated Wasm

```bash
wasmtime run --profile=perfmap benchmark.wasm
```

---

## Troubleshooting

### "Module failed validation"

```bash
wasm-tools validate -v output.wasm
```

Check:
- Section order
- LEB128 encoding
- Type definitions

### "Undefined type"

Ensure all required types are defined in Type Section:
- `$nil`
- `$unbound`
- `$cons`
- `$symbol`

### "Stack overflow in recursion"

Verify tail call optimization:
```lisp
;; Should use return_call
(defun fact (n acc)
  (if (= n 0) acc (fact (1- n) (* n acc))))
```

---

## Key Commands Reference

| Task | Command |
|------|---------|
| Enter dev shell | `nix develop` |
| Run all tests | `nix flake check` |
| Load project | `(ql:quickload :clysm)` |
| Run tests | `(asdf:test-system :clysm)` |
| Compile to Wasm | `(compile-to-wasm expr :output "out.wasm")` |
| Validate Wasm | `wasm-tools validate out.wasm` |
| Run Wasm | `wasmtime out.wasm` |
| Print WAT | `wasm-tools print out.wasm` |

---

## Next Steps

See [tasks.md](./tasks.md) for the current implementation progress.

Current focus:
1. **Phase 3**: Complete US2 (reproducible development environment)
2. **Phase 4**: Implement US1 MVP - compile `(+ 1 2)` to Wasm

Completed:
- Phase 1: Project setup (Nix Flakes, ASDF, directory structure)
- Phase 2: Wasm backend foundation (LEB128, sections, empty module generation)
