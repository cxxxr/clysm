# clysm

Common Lisp to WebAssembly compiler using WasmGC.

## Development

### Requirements
- SBCL
- Quicklisp with: alexandria, serapeum, trivia, fiveam

### Loading
```lisp
(push #p"/path/to/clysm/" asdf:*central-registry*)
(ql:quickload :clysm)
```

### Running Tests
```lisp
(ql:quickload :clysm/tests)
(clysm/tests:run-tests)
```

### Basic Usage
```lisp
;; Compile an expression to WASM module
(let* ((module (clysm/compiler:compile-module '((+ 1 2))))
       (bytes (clysm/wasm:encode-module module)))
  (clysm/wasm:save-module module "output.wasm"))
```

## Architecture

```
Source (.lisp) -> Reader -> Macro Expand (SBCL) -> AST -> IR -> Codegen -> WASM
```

### Packages
- `clysm/utils` - LEB128 encoding, byte buffers
- `clysm/wasm` - WASM types, instructions, encoder
- `clysm/reader` - S-expression reader
- `clysm/ast` - AST node definitions
- `clysm/ir` - Intermediate representation
- `clysm/compiler` - Compilation and code generation
- `clysm/runtime` - Runtime type definitions
- `clysm/stdlib` - Standard library primitives

## Code Style
- Follow standard Common Lisp conventions
- Use defstruct for data types
- Export public API from package.lisp
- Write tests with FiveAM

## Current Status: Bootstrap In Progress

### Phase 1 (Complete)
- Arithmetic: +, -, *, /, mod, rem, 1+, 1-
- Comparisons: <, >, <=, >=, =, /= (multi-arg)
- Boolean: not, null, zerop, plusp, minusp
- Bitwise: logand, logior, logxor, ash
- Control: if, when, unless, cond, and, or, case, ecase
- Bindings: let, let*, setq, setf
- Functions: defun, lambda, funcall

### Phase 2 (Complete)
- Lists: cons, car, cdr, list, list*, first-fourth, nth, nthcdr
- Destructive: rplaca, rplacd
- Predicates: eq, eql, consp, atom, listp, numberp, symbolp
- List ops: append, reverse, nreverse, member, assoc, last, length, butlast, copy-list
- Higher-order: mapcar, mapc, reduce

### Phase 2.5 (Complete)
- Math: abs, max, min, evenp, oddp, gcd, lcm
- Rounding: floor, ceiling, truncate, round
- Multiple values: values, multiple-value-bind
- Destructuring: destructuring-bind
- Local functions: labels
- Control: block, return-from, tagbody, go
- Loops: dotimes, dolist, loop (via SBCL)

### Bootstrap (In Progress)
- Structs: defstruct (constructor, accessors, predicate, setf)
- Globals: defparameter, defconstant, defvar
- Hash tables: make-hash-table, gethash, sethash, remhash
- Macros: defmacro (host expansion), backquote
- Strings: string=, string-downcase, string-upcase, string-append, schar
- Control: catch, throw, unwind-protect
- Format: ~A, ~%, ~~
- Reader: S-expression parser (Phase S1 almost complete)

### Remaining
- Runtime symbol intern
- Full loop implementation (self-hosted)
- Arrays and vectors
- CLOS
- Condition system
