# Quickstart: Clysm Development

**Date**: 2025-12-22
**Plan Reference**: [plan.md](./plan.md)
**Status**: All phases complete (Phase 1-11). 29/30 tests pass.

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
# Run all tests via Nix (recommended)
nix flake check

# Or run tests in SBCL
sbcl --eval "(asdf:test-system :clysm)" --quit
```

Or in REPL:

```lisp
(asdf:test-system :clysm)
;; => 29/30 tests pass (1 expected failure for MVP limitations)
```

---

## Working Examples

### Basic Arithmetic (Phase 4)

```lisp
(ql:quickload :clysm)
(in-package :clysm)

;; Compile and run arithmetic
(compile-to-wasm '(+ 1 2) :output "/tmp/add.wasm")
```

```bash
wasm-tools validate /tmp/add.wasm && echo "Valid!"
wasmtime /tmp/add.wasm
# => 3
```

### Conditionals

```lisp
;; if expression
(compile-to-wasm '(if (> 10 5) 1 0) :output "/tmp/cond.wasm")
```

### Variable Binding

```lisp
;; let bindings
(compile-to-wasm '(let ((x 10) (y 20)) (+ x y)) :output "/tmp/let.wasm")

;; let* (sequential binding)
(compile-to-wasm '(let* ((x 10) (y (* x 2))) y) :output "/tmp/letstar.wasm")
```

### Functions (defun)

```lisp
;; Define and call a function
(compile-to-wasm
 '(progn
   (defun square (x) (* x x))
   (square 7))
 :output "/tmp/square.wasm")
```

```bash
wasmtime /tmp/square.wasm
# => 49
```

### Closures and Lambda (Phase 5)

```lisp
;; Lambda with funcall
(compile-to-wasm
 '(funcall (lambda (x) (+ x 1)) 10)
 :output "/tmp/lambda.wasm")
```

```bash
wasmtime /tmp/lambda.wasm
# => 11
```

```lisp
;; Closure capturing variables
(compile-to-wasm
 '(let ((n 5))
   (funcall (lambda (x) (+ x n)) 10))
 :output "/tmp/closure.wasm")
```

### Tail Call Optimization (Phase 5)

```lisp
;; Factorial with tail recursion (no stack overflow)
(compile-to-wasm
 '(progn
   (defun fact-iter (n acc)
     (if (= n 0)
         acc
         (fact-iter (- n 1) (* n acc))))
   (defun fact (n)
     (fact-iter n 1))
   (fact 10))
 :output "/tmp/fact.wasm")
```

```bash
wasmtime /tmp/fact.wasm
# => 3628800
```

### Block and Return-from (Phase 6)

```lisp
;; Early return from block
(compile-to-wasm
 '(block foo
   (+ 1 (return-from foo 42) 3))
 :output "/tmp/block.wasm")
```

```bash
wasmtime /tmp/block.wasm
# => 42
```

### Reader and Interpreter (Phase 7)

```lisp
;; Use the Clysm reader
(clysm/reader:read-from-string* "(+ 1 2 3)")
;; => (+ 1 2 3)

;; Use the interpreter
(clysm/eval/interpreter:interpret '(+ 1 2))
;; => 3

;; REPL evaluation
(clysm/repl:compile-and-eval "(let ((x 10)) (* x x))")
;; => 100
```

### Macros (Phase 8)

```lisp
;; Standard macros are available
(clysm/eval/interpreter:interpret '(when t 1 2 3))
;; => 3

(clysm/eval/interpreter:interpret '(unless nil 42))
;; => 42

(clysm/eval/interpreter:interpret '(cond ((= 1 2) 'no) (t 'yes)))
;; => YES
```

### CLOS (Phase 10)

```lisp
;; Define a class
(clysm/clos/defclass:define-class*
 '(defclass point ()
   ((x :initarg :x :accessor point-x)
    (y :initarg :y :accessor point-y))))

;; Create an instance
(defvar *p* (clysm/clos/instance:make-instance* 'point :x 3 :y 4))

;; Access slots
(clysm/clos/slot-access:slot-value* *p* 'x)
;; => 3

;; Modify slots
(clysm/clos/slot-access:set-slot-value* *p* 'x 10)
(clysm/clos/slot-access:slot-value* *p* 'x)
;; => 10
```

### Generic Functions and Methods

```lisp
;; Define classes
(clysm/clos/defclass:define-class*
 '(defclass animal () ()))
(clysm/clos/defclass:define-class*
 '(defclass dog (animal) ()))

;; Define generic function
(defvar *speak-gf* (clysm/clos/generic:defgeneric* 'speak '(obj)))

;; Add methods
(clysm/clos/defmethod:add-method*
 *speak-gf*
 (clysm/clos/defmethod:make-method*
  :specializers '(animal)
  :function (lambda (obj) (declare (ignore obj)) "...")))

(clysm/clos/defmethod:add-method*
 *speak-gf*
 (clysm/clos/defmethod:make-method*
  :specializers '(dog)
  :function (lambda (obj) (declare (ignore obj)) "woof")))

;; Dispatch
(clysm/clos/dispatch:dispatch *speak-gf*
 (clysm/clos/instance:make-instance* 'dog))
;; => "woof"
```

### Method Combination

```lisp
;; :before, :after, and :around methods
(clysm/clos/defclass:define-class*
 '(defclass greeter () ()))

(defvar *greet-gf* (clysm/clos/generic:defgeneric* 'greet '(obj)))

;; Primary method
(clysm/clos/defmethod:add-method*
 *greet-gf*
 (clysm/clos/defmethod:make-method*
  :specializers '(greeter)
  :function (lambda (obj) (declare (ignore obj)) "Hello!")))

;; :before method
(clysm/clos/defmethod:add-method*
 *greet-gf*
 (clysm/clos/defmethod:make-method*
  :specializers '(greeter)
  :qualifier :before
  :function (lambda (obj) (declare (ignore obj)) (format t "Preparing..."))))

;; call-next-method
(clysm/clos/defclass:define-class*
 '(defclass child (greeter) ()))

(clysm/clos/defmethod:add-method*
 *greet-gf*
 (clysm/clos/defmethod:make-method*
  :specializers '(child)
  :function (lambda (obj)
              (declare (ignore obj))
              (format nil "Child says: ~A" (clysm/clos/combination:call-next-method*)))))
```

---

## Project Structure

```
clysm/
├── flake.nix              # Nix development environment
├── clysm.asd              # ASDF system definition
├── src/clysm/
│   ├── package.lisp       # All package definitions
│   ├── backend/           # Wasm binary emission
│   │   ├── leb128.lisp    # LEB128 encoding
│   │   ├── sections.lisp  # Wasm section handling
│   │   ├── wasm-emit.lisp # Binary emission
│   │   └── wat-print.lisp # WAT text output
│   ├── compiler/          # Compiler core
│   │   ├── ast.lisp       # AST node definitions
│   │   ├── env.lisp       # Lexical environments
│   │   ├── analyzer/      # Static analysis
│   │   ├── transform/     # AST transformations
│   │   ├── codegen/       # Wasm code generation
│   │   └── compiler.lisp  # Main entry points
│   ├── reader/            # S-expression reader
│   │   ├── tokenizer.lisp
│   │   ├── parser.lisp
│   │   ├── package.lisp
│   │   └── reader.lisp
│   ├── runtime/           # Runtime support
│   │   ├── objects.lisp
│   │   ├── special-vars.lisp
│   │   ├── multi-value.lisp
│   │   └── printer.lisp
│   ├── eval/              # Dynamic evaluation
│   │   ├── interpreter.lisp
│   │   ├── jit.lisp
│   │   ├── eval.lisp
│   │   └── compile.lisp
│   ├── clos/              # Object system
│   │   ├── mop.lisp       # Metaobject protocol
│   │   ├── defclass.lisp
│   │   ├── instance.lisp
│   │   ├── slot-access.lisp
│   │   ├── generic.lisp
│   │   ├── defmethod.lisp
│   │   ├── combination.lisp
│   │   ├── dispatch.lisp
│   │   └── method-combination.lisp
│   ├── lib/               # Standard library
│   │   └── macros.lisp
│   └── repl.lisp          # REPL interface
├── tests/
│   ├── package.lisp
│   ├── helpers.lisp
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
(rove:run :clysm/tests/unit/arithmetic)
;; => FAIL
```

3. **Implement feature**

4. **Verify test passes**
```lisp
(rove:run :clysm/tests/unit/arithmetic)
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
- System loading verification

---

## Common Tasks

### Generate Empty Wasm Module

```lisp
(clysm:emit-empty-module :output "/tmp/empty.wasm")
```

```bash
wasm-tools validate /tmp/empty.wasm
wasm-tools print /tmp/empty.wasm
```

### Debug WAT Output

```lisp
(clysm:compile-to-wat '(+ 1 2))
;; => "(module ...)"
```

### Run Specific Test Suite

```lisp
;; Run unit tests for CLOS
(rove:run :clysm/tests/unit/clos)

;; Run integration tests for closures
(rove:run :clysm/tests/integration/closure)

;; Run all tests
(asdf:test-system :clysm)
```

### Profile Generated Wasm

```bash
wasmtime run --profile=perfmap /tmp/benchmark.wasm
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

Ensure all required GC types are defined in Type Section:
- `$nil` (type 0)
- `$unbound` (type 1)
- `$cons` (type 2)
- `$symbol` (type 3)
- `$string` (type 4)
- `$closure` (type 5)

### "Stack overflow in recursion"

Verify tail call optimization is working:
```lisp
;; This should use return_call, not call
(defun fact-iter (n acc)
  (if (= n 0) acc (fact-iter (- n 1) (* n acc))))
```

### "No applicable method"

Ensure:
1. Class is defined and finalized
2. Generic function exists
3. Method specializers match the instance's class

---

## Key Commands Reference

| Task | Command |
|------|---------|
| Enter dev shell | `nix develop` |
| Run all tests | `nix flake check` |
| Load project | `(ql:quickload :clysm)` |
| Run tests | `(asdf:test-system :clysm)` |
| Compile to Wasm | `(compile-to-wasm expr :output "out.wasm")` |
| Compile to WAT | `(compile-to-wat expr)` |
| Validate Wasm | `wasm-tools validate out.wasm` |
| Run Wasm | `wasmtime out.wasm` |
| Print WAT | `wasm-tools print out.wasm` |
| Interpret expr | `(clysm/eval/interpreter:interpret expr)` |
| REPL eval | `(clysm/repl:compile-and-eval "(+ 1 2)")` |

---

## Feature Status

| Feature | Status | Notes |
|---------|--------|-------|
| Arithmetic (+, -, *, /) | ✅ Complete | i31ref representation |
| Comparisons (<, >, =) | ✅ Complete | |
| Conditionals (if) | ✅ Complete | |
| let/let* binding | ✅ Complete | |
| defun/functions | ✅ Complete | |
| Lambda/closures | ✅ Complete | Environment capture |
| Tail call optimization | ✅ Complete | return_call |
| block/return-from | ✅ Complete | Using Wasm block/br |
| tagbody/go | ⚠️ MVP | Sequential only |
| catch/throw | ⚠️ MVP | Needs Wasm exceptions |
| unwind-protect | ⚠️ MVP | Normal exit only |
| Reader | ✅ Complete | |
| Packages | ✅ Complete | CL, KEYWORD |
| Macros | ✅ Complete | Host SBCL expansion |
| Interpreter | ✅ Complete | |
| JIT stub | ✅ Complete | |
| CLOS defclass | ✅ Complete | |
| make-instance | ✅ Complete | |
| slot-value | ✅ Complete | |
| Generic functions | ✅ Complete | |
| Method dispatch | ✅ Complete | |
| Method combination | ✅ Complete | before/after/around |
| call-next-method | ✅ Complete | |

---

## Next Steps

See [tasks.md](./tasks.md) for remaining edge case tasks (T256-T264).

Completed:
- Phase 1: Project setup
- Phase 2: Wasm backend foundation
- Phase 3: Reproducible environment (US2)
- Phase 4: Basic compilation (US1) - **MVP**
- Phase 5: Closures and TCO (US3)
- Phase 6: Control flow (US4)
- Phase 7: Reader and REPL (US5)
- Phase 8: Macros (US6)
- Phase 9: Eval/JIT infrastructure
- Phase 10: CLOS (US7)
- Phase 11: Polish (in progress)
