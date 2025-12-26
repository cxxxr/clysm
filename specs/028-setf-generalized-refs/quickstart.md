# Quickstart: Setf Macros and Generalized References

**Feature**: 028-setf-generalized-refs
**Date**: 2025-12-27

## Prerequisites

- Clysm compiler with macro system (Feature 016)
- CLOS foundation (Feature 026) for slot accessor integration
- Rove test framework

## Development Setup

```bash
# Enter development environment
cd /home/user/src/clysm-workbench/clysm3
nix develop

# Run existing tests to ensure baseline
sbcl --load tests/main.lisp --eval '(rove:run :clysm/tests)'
```

## Key Files to Modify/Create

### New Files

1. **src/clysm/lib/setf-expanders.lisp** - Setf expander registry and standard expanders
2. **tests/unit/setf-test.lisp** - Unit tests for setf macros
3. **tests/unit/setf-expander-test.lisp** - Unit tests for expander protocol
4. **tests/contract/setf-wasm-test.lisp** - Wasm validation tests
5. **tests/integration/setf-ansi-test.lisp** - ANSI CL compliance tests

### Modified Files

1. **src/clysm/lib/macros.lisp** - Add setf, psetf, incf, decf, push, pop, pushnew, rotatef, shiftf
2. **src/clysm/package.lisp** - Export new symbols
3. **src/clysm/compiler/transform/macro.lisp** - Integrate setf expander registry

## TDD Workflow

### Step 1: Unit Test for setf Expander Registry

```lisp
;; tests/unit/setf-expander-test.lisp
(deftest test-setf-expander-registry
  (let ((registry (make-setf-expander-registry)))
    ;; Test registration
    (register-setf-expander registry 'car
      (lambda (form env)
        (declare (ignore env))
        (let ((temp (gensym))
              (store (gensym)))
          (values (list temp)
                  (list (second form))
                  (list store)
                  `(progn (rplaca ,temp ,store) ,store)
                  `(car ,temp)))))
    ;; Verify retrieval
    (ok (get-setf-expander registry 'car))))
```

### Step 2: Unit Test for Simple setf

```lisp
;; tests/unit/setf-test.lisp
(deftest test-setf-variable
  ;; setf of simple variable should expand to setq
  (let ((expansion (macroexpand-1 '(setf x 10) *env*)))
    (ok (equal expansion '(setq x 10)))))

(deftest test-setf-car
  ;; setf of car should expand using expander
  (let ((expansion (macroexpand-1 '(setf (car x) 10) *env*)))
    ;; Should have let binding and rplaca
    (ok (eq (first expansion) 'let))
    (ok (member 'rplaca (flatten expansion)))))
```

### Step 3: Contract Test for Wasm Generation

```lisp
;; tests/contract/setf-wasm-test.lisp
(deftest test-setf-car-wasm
  ;; Compile (setf (car x) 10) and validate Wasm
  (let ((wasm (compile-to-wasm '(lambda (x) (setf (car x) 10)))))
    (ok (wasm-valid-p wasm))
    ;; Should contain struct.set instruction
    (ok (contains-instruction-p wasm 'struct.set))))
```

### Step 4: Integration Test

```lisp
;; tests/integration/setf-ansi-test.lisp
(deftest test-setf-returns-new-value
  (let ((result (run-wasm '(let ((x (cons 1 2)))
                             (setf (car x) 10)))))
    (ok (= result 10))))

(deftest test-setf-modifies-place
  (let ((result (run-wasm '(let ((x (cons 1 2)))
                             (setf (car x) 10)
                             (car x)))))
    (ok (= result 10))))
```

## Implementation Order

1. **Setf Expander Registry** (data structure)
2. **get-setf-expansion** (protocol function)
3. **Standard expanders** (car, cdr, aref, gethash, symbol-value)
4. **setf macro** (uses expanders)
5. **psetf macro** (parallel assignment)
6. **incf/decf macros** (numeric helpers)
7. **push/pop/pushnew macros** (list helpers)
8. **rotatef/shiftf macros** (rotation helpers)
9. **define-setf-expander** (user extensibility)
10. **defsetf** (simplified interface)

## Common Patterns

### Registering a Standard Expander

```lisp
(defun make-car-setf-expander ()
  "Setf expander for CAR."
  (lambda (form env)
    (declare (ignore env))
    (let ((cons-form (second form))
          (cons-temp (gensym "CONS-"))
          (store-var (gensym "NEW-")))
      (values
       (list cons-temp)           ; temps
       (list cons-form)           ; vals
       (list store-var)           ; stores
       `(progn (rplaca ,cons-temp ,store-var) ,store-var)  ; store-form
       `(car ,cons-temp)))))      ; access-form
```

### Implementing a setf-based Macro

```lisp
(defun make-incf-expander ()
  "Macro expander for INCF."
  (lambda (form)
    (let ((place (second form))
          (delta (or (third form) 1)))
      `(setf ,place (+ ,place ,delta)))))
```

## Debugging Tips

1. **Trace macro expansion**: Use `macroexpand-1` and `macroexpand` to see intermediate forms
2. **Check Wasm output**: Use `wasm-tools print output.wasm` to verify generated instructions
3. **Test evaluation order**: Create forms with side effects to verify single evaluation

## Running Tests

```bash
# Run all setf-related tests
sbcl --load tests/main.lisp --eval '(rove:run :clysm/tests/setf)'

# Run specific test file
sbcl --load tests/unit/setf-test.lisp --eval '(rove:run :clysm/tests/unit/setf)'

# Validate Wasm output
wasm-tools validate output.wasm
```
