# Quickstart: Runtime Library System

**Feature**: 001-runtime-library-system
**Date**: 2026-01-01

## Prerequisites

- SBCL 2.4+
- Nix (for development environment)
- wasm-tools, wasmtime (via nix develop)

## Setup

```bash
# Enter development environment
cd /home/user/src/clysm-workbench/clysm3
nix develop

# Load system
sbcl --eval "(asdf:load-system :clysm)"
```

## Using the Primitives Registry

### Registering a Primitive

```lisp
(in-package #:clysm/compiler/codegen)

;; Register a new primitive
(register-primitive 'my-primitive
  (lambda (env args)
    ;; Return Wasm instructions
    `((local.get ,(first args))
      (i32.const 1)
      (i32.add)))
  :signature '(:params (:i32) :result :i32)
  :category :arithmetic)
```

### Checking Primitive Availability

```lisp
;; Check if a primitive exists
(primitive-p 'cons)     ; => T
(primitive-p 'unknown)  ; => NIL

;; Get primitive definition
(get-primitive 'cons)   ; => #<PRIMITIVE cons>
```

## Adding Runtime Library Functions

### Creating a New Runtime Function

```lisp
;; In src/clysm/runtime/io.lisp

(defun my-print (object)
  "Print OBJECT to standard output."
  (princ object)
  (terpri)
  object)
```

### Runtime Library Constraints

1. Only use Layer 1 primitives or other runtime functions
2. No direct Wasm instructions
3. No host-specific code (use FFI primitives)

### Bad Example (Don't Do This)

```lisp
;; WRONG: Uses undefined primitive
(defun bad-fn (x)
  (%undefined-primitive x))  ; Error: undefined

;; WRONG: Uses host-specific function
(defun bad-fn-2 ()
  (sb-ext:process-wait))     ; Error: not available in Wasm
```

## Migrating a Function from func-section.lisp

### Step 1: Identify Dependencies

```lisp
;; Check what princ uses
;; In func-section.lisp, find compile-princ and note:
;; - %host-write-string (primitive)
;; - %host-write-char (primitive)
;; - type predicates (stringp, consp, etc.)
```

### Step 2: Implement in Runtime Library

```lisp
;; In src/clysm/runtime/io.lisp
(defun princ* (object &optional (stream *standard-output*))
  "Print OBJECT without escape characters."
  (cond
    ((stringp object)
     (%host-write-string (stream-fd stream) object))
    ((characterp object)
     (%host-write-char (stream-fd stream) (char-code object)))
    ((consp object)
     (%host-write-char (stream-fd stream) 40)  ; #\(
     (princ* (car object) stream)
     (loop for tail = (cdr object) then (cdr tail)
           while (consp tail)
           do (%host-write-char (stream-fd stream) 32)  ; #\Space
              (princ* (car tail) stream))
     (%host-write-char (stream-fd stream) 41)) ; #\)
    (t
     ;; Handle other types...
     ))
  object)
```

### Step 3: Write Tests

```lisp
;; In tests/unit/runtime/io-test.lisp
(deftest princ-string ()
  (testing "princ outputs string without quotes"
    (ok (string= "hello"
                 (with-output-to-string (s)
                   (princ* "hello" s))))))
```

### Step 4: Remove from func-section.lisp

```lisp
;; Delete compile-princ and related codegen
;; Update function dispatch to use runtime function
```

## Running Tests

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run specific runtime tests
sbcl --eval "(asdf:test-system :clysm/tests)" \
     --eval "(rove:run :clysm/tests/unit/runtime)"

# Validate generated Wasm
wasm-tools validate dist/clysm-stage1.wasm
```

## Measuring Migration Progress

```bash
# Count lines in func-section.lisp
wc -l src/clysm/compiler/codegen/func-section.lisp

# Target: < 11,000 lines (40% reduction from 18,229)
```

## Common Issues

### "Undefined primitive" Error

```lisp
;; Error: The primitive %FOO is not registered
;; Solution: Register the primitive first
(register-primitive '%foo ...)
```

### "Circular dependency" Warning

```lisp
;; Warning: Circular dependency detected: A -> B -> A
;; This is OK - functions resolve at link time
;; Ensure both functions are in the same runtime module
```

### Build Time Increase

```lisp
;; If build time increases >20%, check:
;; 1. Runtime library size (too many functions?)
;; 2. Compile caching working?
;; 3. Unnecessary recompilations?
```

## Next Steps

1. Run `/speckit.tasks` to generate task breakdown
2. Start with P1 (primitives registry) implementation
3. Migrate functions bottom-up per research.md
