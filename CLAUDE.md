# cl-wasm

Common Lisp to WebAssembly compiler using WasmGC.

## Development

### Requirements
- SBCL
- Quicklisp with: alexandria, serapeum, trivia, flexi-streams, fiveam

### Loading
```lisp
(push #p"/path/to/cl-wasm/" asdf:*central-registry*)
(ql:quickload :cl-wasm)
```

### Running Tests
```lisp
(ql:quickload :cl-wasm/tests)
(cl-wasm/tests:run-tests)
```

### Basic Usage
```lisp
;; Compile an expression to WASM module
(let* ((module (cl-wasm/compiler:compile-module '((+ 1 2))))
       (bytes (cl-wasm/wasm:encode-module module)))
  (cl-wasm/wasm:save-module module "output.wasm"))
```

## Architecture

```
Source (.lisp) -> Reader -> Macro Expand (SBCL) -> AST -> IR -> Codegen -> WASM
```

### Packages
- `cl-wasm/utils` - LEB128 encoding, byte buffers
- `cl-wasm/wasm` - WASM types, instructions, encoder
- `cl-wasm/reader` - S-expression reader
- `cl-wasm/ast` - AST node definitions
- `cl-wasm/ir` - Intermediate representation
- `cl-wasm/compiler` - Compilation and code generation
- `cl-wasm/runtime` - Runtime type definitions
- `cl-wasm/stdlib` - Standard library primitives

## Code Style
- Follow standard Common Lisp conventions
- Use defstruct for data types
- Export public API from package.lisp
- Write tests with FiveAM

## Current Status: Phase 1 Complete
- Basic arithmetic (+, -, *, /)
- Comparisons (<, >, <=, >=, =, /=)
- Control flow (if)
- Local bindings (let)
- Function definitions (defun)
