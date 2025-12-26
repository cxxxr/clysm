# Research: Setf Macros and Generalized References

**Feature**: 028-setf-generalized-refs
**Date**: 2025-12-27

## Overview

This document consolidates research findings for implementing ANSI Common Lisp's generalized reference (place) system.

---

## 1. Setf Expansion Protocol

### Decision
Implement the five-value setf expansion protocol per ANSI CL specification.

### Rationale
The five-value tuple (temps vals stores store-form access-form) provides:
- Correct left-to-right evaluation order
- Single evaluation of subforms
- Support for multiple store values
- Clean separation between accessor and mutator logic

### Alternatives Considered
1. **Simple setter function lookup**: Rejected - doesn't handle complex places like `(car (cdr x))`
2. **Compiler-only expansion**: Rejected - users need `get-setf-expansion` for macros

### Implementation Details
```lisp
;; get-setf-expansion returns:
;;   temps   - list of temporary variable symbols
;;   vals    - list of value forms for temps
;;   stores  - list of store variable symbols (usually 1)
;;   store-form - form that stores the value and returns it
;;   access-form - form that reads the current value
```

---

## 2. Setf Expander Registry

### Decision
Create a parallel registry to the macro registry, mapping accessor names to setf expander functions.

### Rationale
- Separates concerns from general macros
- Allows `(setf accessor)` function name lookup
- Supports both define-setf-expander and defsetf patterns

### Implementation Details
```lisp
(defstruct setf-expander-registry
  (table (make-hash-table :test 'eq) :type hash-table))

(defun register-setf-expander (registry name expander-fn)
  "EXPANDER-FN: (form env) -> (temps vals stores store-form access-form)"
  (setf (gethash name (setf-expander-registry-table registry)) expander-fn))
```

---

## 3. Standard Setf Expanders

### Decision
Implement built-in setf expanders for the most common accessors, mapped to existing Wasm primitives.

### Mapping to Wasm Operations

| Accessor | Setter Primitive | Wasm Codegen |
|----------|-----------------|--------------|
| car | rplaca | struct.set $cons 0 |
| cdr | rplacd | struct.set $cons 1 |
| first-tenth | nth-based | navigate + struct.set |
| nth | (setf nth) | nthcdr + rplaca |
| aref | (setf aref) | array.set |
| gethash | (setf gethash) | hash-table-set call |
| symbol-value | set-symbol-value | struct.set $symbol $value |
| symbol-function | set-symbol-function | struct.set $symbol $function |
| symbol-plist | set-symbol-plist | struct.set $symbol $plist |

### Rationale
These cover 95%+ of common setf use cases in typical Lisp code.

---

## 4. Macro Implementation Strategy

### Decision
Implement setf-related macros as standard Clysm macros that expand to primitive operations.

### Expansion Patterns

```lisp
;; setf - single place
(setf (car x) 10)
=> (let ((#:G1 x))
     (rplaca #:G1 10)
     10)

;; setf - multiple pairs
(setf (car x) 1 (cdr x) 2)
=> (progn (setf (car x) 1) (setf (cdr x) 2))

;; psetf - parallel assignment
(psetf a b b a)
=> (let ((#:temp-a b)
         (#:temp-b a))
     (setq a #:temp-a)
     (setq b #:temp-b)
     nil)

;; incf
(incf x)
=> (setf x (+ x 1))

;; incf with delta
(incf x 5)
=> (setf x (+ x 5))

;; push
(push item place)
=> (setf place (cons item place))

;; pop
(pop place)
=> (prog1 (car place) (setf place (cdr place)))

;; pushnew
(pushnew item place :test test)
=> (if (member item place :test test)
       place
       (setf place (cons item place)))

;; rotatef
(rotatef a b c)
=> (psetf a b b c c a)  ; but with correct old-value semantics

;; shiftf
(shiftf a b c new)
=> (prog1 a (psetf a b b c c new))
```

---

## 5. Evaluation Order Guarantee

### Decision
Strict left-to-right evaluation of all subforms, each evaluated exactly once.

### Rationale
ANSI CL requires this for correct semantics. Critical for side-effecting subforms.

### Implementation
The get-setf-expansion protocol with temps/vals ensures this by:
1. Generating temp bindings for all subforms
2. Binding temps left-to-right before any mutations
3. Store form uses only temp variables

### Example
```lisp
(setf (aref a (incf i)) (* i 10))
;; i is incremented once, not twice
;; expansion binds temps for a, (incf i), (* i 10) in order
```

---

## 6. define-setf-expander Implementation

### Decision
Implement define-setf-expander as a defmacro that registers an expander function.

### Rationale
User-defined setf expanders need full control over expansion, which the five-value protocol provides.

### Signature
```lisp
(define-setf-expander accessor-name (lambda-list)
  &body body)
;; body should return (values temps vals stores store-form access-form)
```

### Compile-time vs Runtime
- Expander functions execute at compile time (host SBCL)
- Generated forms are compiled to Wasm
- Registry is part of compile-time environment

---

## 7. Integration with CLOS Slot Accessors

### Decision
Leverage existing slot-access.lisp infrastructure; generate setf expanders for accessor definitions.

### Rationale
Feature 026 already implements `(setf slot-value*)`. Slot accessors just need registration.

### Implementation
When defclass generates an accessor, also register a setf expander:
```lisp
;; For :accessor slot-name
(register-setf-expander registry 'accessor-name
  (lambda (form env)
    (let ((instance-form (second form)))
      (values ...))))
```

---

## 8. Simple Variable Setf

### Decision
`(setf var value)` for simple variables expands to `(setq var value)`.

### Rationale
Variables are the trivial case; no expansion protocol needed.

### Implementation
In the setf macro:
```lisp
(defun simple-variable-p (place)
  (and (symbolp place)
       (not (constantp place))))

;; In setf expansion:
(if (simple-variable-p place)
    `(setq ,place ,value)
    (expand-using-protocol place value))
```

---

## 9. Error Handling

### Decision
Signal specific conditions for setf errors at compile time when possible.

### Conditions
| Condition | When |
|-----------|------|
| `undefined-setf-expander` | No expander for accessor |
| `invalid-place` | setf on a constant or read-only form |
| `odd-argument-count` | psetf/setf with odd args |
| `type-error` | incf/decf on non-numeric (runtime) |

---

## 10. defsetf Support

### Decision
Implement defsetf as syntactic sugar over define-setf-expander.

### Rationale
defsetf is simpler for common cases and part of ANSI CL.

### Two Forms
```lisp
;; Short form: (defsetf accessor setter)
(defsetf car rplaca)

;; Long form: (defsetf accessor lambda-list (store-var) body)
(defsetf subseq (sequence start &optional end) (new-sequence)
  `(progn (replace ,sequence ,new-sequence :start1 ,start :end1 ,end)
          ,new-sequence))
```

---

## Summary

All research items resolved. No NEEDS CLARIFICATION remaining.

| Topic | Decision | Impact |
|-------|----------|--------|
| Expansion Protocol | Five-value tuple | Core architecture |
| Registry | Parallel to macro registry | Moderate |
| Standard Expanders | Map to Wasm primitives | High (covers most use) |
| Macro Strategy | Expand to primitives | Standard approach |
| Evaluation Order | Temps/vals pattern | Required for correctness |
| define-setf-expander | Register expander fn | User extensibility |
| CLOS Integration | Leverage slot-access | Minimal new code |
| Variable Setf | Expand to setq | Trivial case |
| Error Handling | Compile-time conditions | Developer experience |
| defsetf | Sugar over define-setf-expander | ANSI compliance |
