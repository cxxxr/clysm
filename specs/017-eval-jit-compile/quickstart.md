# Quickstart: Eval/JIT Compile System

## Prerequisites

```bash
# Enter development environment
nix develop

# Load the system
sbcl --load clysm.asd --eval '(asdf:load-system :clysm)'
```

## Basic Usage

### Compiling Anonymous Lambdas

```lisp
;; Compile and call an anonymous function
(let ((add-one (clysm/eval/compile:compile* nil '(lambda (x) (+ x 1)))))
  (funcall add-one 5))  ; => 6
```

### Compiling Named Functions

```lisp
;; Compile and register a named function
(clysm/eval/compile:compile* 'my-double '(lambda (x) (* x 2)))

;; Call via function slot
(funcall (clysm/eval/jit:get-function-slot 'my-double) 21)  ; => 42
```

### Explicit Tier Selection

```lisp
;; Force Tier 1 (interpreter)
(clysm/eval/compile:compile-with-tier :tier-1 '(lambda (x) x))

;; Force Tier 2 (JIT to Wasm)
(clysm/eval/compile:compile-with-tier :tier-2 '(lambda (x) x))
```

### Configuring Hot Spot Threshold

```lisp
;; Lower threshold for faster promotion (testing)
(setf clysm/eval/compile:*compilation-threshold* 5)

;; Higher threshold for production
(setf clysm/eval/compile:*compilation-threshold* 100)
```

## Testing

```bash
# Run all tests
nix flake check

# Run specific test file
sbcl --load clysm.asd \
     --eval '(asdf:test-system :clysm/tests)' \
     --quit
```

### Key Test Files

- `tests/unit/interpreter-test.lisp` - Tier 1 interpreter tests
- `tests/integration/jit-test.lisp` - JIT compilation tests
- `tests/integration/eval-test.lisp` - eval* function tests

## Tier Promotion Observation

```lisp
;; Create a function and observe tier promotion
(let ((fib (clysm/eval/compile:compile* 'fib
             '(lambda (n)
                (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))))
  ;; First 10 calls: Tier 1
  (dotimes (i 11)
    (funcall fib 10))
  ;; 11th call triggers Tier 2 promotion
  ;; Subsequent calls use JIT-compiled code
  (funcall fib 20))
```

## Common Patterns

### Error Handling for Invalid Input

```lisp
;; Non-lambda expressions signal an error
(handler-case
    (clysm/eval/compile:compile* nil '(not-a-lambda))
  (error (e)
    (format t "Compile error: ~A~%" e)))
```

### Checking Function Tier

```lisp
;; Check invocation count
(gethash 'my-fn clysm/eval/compile:*invocation-counts*)

;; Check if promotion should happen
(clysm/eval/compile:should-promote-to-tier-2-p 'my-fn)
```

## Debugging

```lisp
;; Trace tier switching
(trace clysm/eval/compile:promote-to-tier-2)
(trace clysm/eval/jit:jit-compile)

;; Inspect function slots
(maphash (lambda (k v) (format t "~S -> ~S~%" k v))
         clysm/eval/jit:*function-slots*)
```
