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

## Current Status: Phase 2 In Progress

### Phase 1 (Complete)
- Basic arithmetic (+, -, *, /, mod, rem)
- Comparisons (<, >, <=, >=, =, /=)
- Boolean (not, null)
- Bitwise (logand, logior, logxor, ash)
- Control flow (if)
- Local bindings (let)
- Function definitions (defun)

### Phase 2 (In Progress)
- Memory and heap management
- List primitives (cons, car, cdr, consp, atom, rplaca, rplacd)
- List constructors (list, list*)
- quote for literals (numbers, lists)
- let* (sequential bindings)
- Conditionals (when, unless, cond, and, or)
- Recursive function calls
- Multiple function definitions with mutual calls

### Remaining
- Closures with environment capture
- block/return-from
- tagbody/go
- Symbols
