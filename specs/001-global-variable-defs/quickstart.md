# Quickstart: Phase 13D-4 Global Variable Definitions

## Prerequisites

- SBCL 2.4+ installed
- Nix with flakes enabled
- wasm-tools in PATH

## Development Setup

```bash
# Enter development shell
nix develop

# Load the compiler
sbcl --load build/bootstrap.lisp

# Run tests
sbcl --eval "(asdf:test-system :clysm)"
```

## Testing Global Variable Compilation

### Unit Test

```lisp
;; In SBCL REPL after loading clysm
(in-package :clysm)

;; Test simple defvar
(compile-to-wasm '(defvar *test-counter* 0))
;; Should return Wasm bytes with global declaration

;; Test defparameter
(compile-to-wasm '(defparameter *test-name* "default"))
;; Should return Wasm bytes with string-initialized global

;; Test defvar without init
(compile-to-wasm '(defvar *test-unset*))
;; Should return Wasm bytes with UNBOUND initialization
```

### Contract Test (Wasm Validation)

```bash
# Compile a test file
sbcl --eval '(clysm:compile-file "test-globals.lisp" :output "test.wasm")'

# Validate the output
wasm-tools validate test.wasm

# Print WAT for inspection
wasm-tools print test.wasm
```

### Integration Test

```lisp
;; Test dynamic binding
(compile-to-wasm
  '(progn
     (defvar *dynamic* 1)
     (let ((*dynamic* 2))
       *dynamic*)))
;; Should compile and validate
```

## Key Files

| File | Purpose |
|------|---------|
| `src/clysm/compiler/codegen/globals.lisp` | Global compilation |
| `src/clysm/stage0/globals.lisp` | Wasm section generation |
| `tests/unit/globals-test.lisp` | Unit tests |
| `tests/contract/wasm-globals-test.lisp` | Validation tests |

## Common Tasks

### Add Support for New Global Category

1. Update `*special-var-globals*` allocation in `func-section.lisp`
2. Add init-form analysis in `globals.lisp`
3. Write unit test in `globals-test.lisp`
4. Run validation: `wasm-tools validate`

### Debug Global Initialization

```lisp
;; Check allocated indices
(maphash (lambda (k v) (format t "~A -> ~A~%" k v))
         *special-var-globals*)

;; Trace global emission
(trace emit-global-entry)
(compile-to-wasm '(defvar *debug* nil))
```

### Verify Wasm Output

```bash
# Disassemble globals section
wasm-tools print output.wasm | grep -A5 "(global"

# Check section structure
wasm-tools dump output.wasm --section global
```

## Success Criteria Verification

| Criterion | Verification Command |
|-----------|---------------------|
| SC-001: 58 globals compile | `(compile-compiler-globals)` |
| SC-002: Wasm validates | `wasm-tools validate output.wasm` |
| SC-003: Correct init values | `(run-global-init-tests)` |
| SC-004: Dynamic binding works | `(run-binding-tests)` |
| SC-005: setq works | `(run-setq-tests)` |
| SC-006: Compilation rate up | `(measure-compilation-rate)` |

## Troubleshooting

### "Global index out of range"
- Check that reserved indices 0-3 are not being reallocated
- Verify `*next-special-global-index*` starts at 4

### "Invalid init expression"
- Ensure init expression ends with `#x0B` (end opcode)
- Check that complex inits use deferred pattern

### "Type mismatch in global"
- All special variables must use `(ref null any)` type
- Verify type encoding: `#x63 #x6F` for ref null any

## References

- [defvar/defparameter](../../resources/HyperSpec/Body/m_defpar.htm) - ANSI CL specification
- [Wasm Global Section](https://webassembly.github.io/spec/core/binary/modules.html#global-section) - Binary format
- [WasmGC Types](https://github.com/AntoinePrv/gc/blob/main/proposals/gc/Overview.md) - GC proposal
