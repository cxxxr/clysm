# Quickstart: DEFSTRUCT Wasm Compilation

**Feature**: 001-defstruct-wasm-compile
**Date**: 2025-12-30

## Prerequisites

- SBCL 2.4+
- Nix devShell: `nix develop`
- wasm-tools: `wasm-tools --version`

## Usage

### Basic Structure Definition

```lisp
;; Define a structure
(defstruct point x y)

;; Create instances
(make-point :x 10 :y 20)

;; Access slots
(point-x p)  ; => 10
(point-y p)  ; => 20

;; Modify slots
(setf (point-x p) 30)

;; Type check
(point-p p)  ; => T
```

### Compile to Wasm

```lisp
;; Compile structure definition
(clysm:compile-to-wasm '(defstruct point x y))

;; Or compile file containing defstruct
(clysm:compile-file "my-structures.lisp")
```

### With Options

```lisp
;; Custom accessor prefix
(defstruct (point (:conc-name p-)) x y)
(p-x p)  ; => access x slot

;; Structure inheritance
(defstruct (colored-point (:include point)) color)
(colored-point-color cp)  ; => access color
(point-x cp)              ; => inherited accessor works

;; Custom predicate/copier names
(defstruct (node (:predicate nodep) (:copier clone-node))
  left right)
```

## Testing

```bash
# Run unit tests
sbcl --eval "(asdf:test-system :clysm)" --quit

# Validate generated Wasm
wasm-tools validate dist/clysm-stage1.wasm

# Check compilation rate
sbcl --load build/stage1-complete.lisp
# Look for "Compilation rate: XX%" in output
```

## File Locations

| File | Purpose |
|------|---------|
| `src/clysm/lib/defstruct.lisp` | Macro implementation |
| `tests/unit/defstruct-test.lisp` | Unit tests |
| `tests/contract/defstruct-wasm-test.lisp` | Wasm validation |

## Common Issues

### "Undefined structure type"

When using `:include`, the parent structure must be defined first:

```lisp
;; WRONG: child defined before parent
(defstruct (child (:include parent)) c)
(defstruct parent a b)

;; CORRECT: parent first
(defstruct parent a b)
(defstruct (child (:include parent)) c)
```

### "Slot name conflict"

Child structures cannot redefine parent slot names:

```lisp
;; ERROR: 'x' already defined in parent
(defstruct parent x)
(defstruct (child (:include parent)) x)  ; error
```

## Success Criteria Verification

```bash
# Check compilation rate (target: 25%+)
grep "Compilation rate" dist/stage1-report.json

# Check binary size (target: 50KB+)
ls -la dist/clysm-stage1.wasm

# Validate Wasm
wasm-tools validate dist/clysm-stage1.wasm && echo "PASS"
```
