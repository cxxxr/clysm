# Clysm

Common Lisp to WebAssembly compiler using WasmGC.

Clysm compiles Common Lisp code to WebAssembly, leveraging WasmGC for garbage collection and targeting browser execution.

## Installation

```lisp
(push #p"/path/to/clysm/" asdf:*central-registry*)
(ql:quickload :clysm)
```

## Usage

```lisp
;; Compile an expression to WASM module
(let ((module (clysm/compiler:compile-module '((+ 1 2)))))
  (clysm/wasm:save-module module "output.wasm"))
```

### Example: Factorial

```lisp
;; Define and compile a factorial function
(let ((module (clysm/compiler:compile-module
               '((defun factorial (n)
                   (if (<= n 1)
                       1
                       (* n (factorial (- n 1)))))
                 (factorial 10)))))
  (clysm/wasm:save-module module "factorial.wasm"))
```

## Running Tests

```lisp
(ql:quickload :clysm/tests)
(clysm/tests:run-tests)
```

## License

MIT
