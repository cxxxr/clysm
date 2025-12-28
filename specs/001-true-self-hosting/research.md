# Research: Phase 13D - True Self-Hosting Achievement

**Feature**: 001-true-self-hosting
**Date**: 2025-12-28
**Status**: Complete

## Research Tasks

This document consolidates findings for all technical decisions required for implementing true self-hosting.

---

## 1. Interpreter Architecture for WasmGC

### Decision: Recursive Evaluator with Worklist Fallback

### Rationale
WasmGC supports recursive function calls with tail-call optimization. For the minimal bootstrap interpreter, a simple recursive evaluator is sufficient. The worklist pattern (mentioned in edge cases) is only needed if recursion depth becomes problematic.

### Alternatives Considered
| Alternative | Pros | Cons | Why Rejected |
|-------------|------|------|--------------|
| CPS Transform | No stack growth | Complex, larger code | Overkill for bootstrap |
| Worklist/Loop | Explicit control | More code, less readable | Only needed for deep AST |
| Recursive | Simple, natural | Stack limits | Acceptable for bootstrap forms |

### Implementation Notes
- Evaluator takes `(form, env)` and returns value
- Environment is an association list of `(symbol . value)` pairs
- Use WasmGC `struct` for env entries, cons for the list

---

## 2. S-expression Input Strategy

### Decision: Host Parses String → Pre-constructed AST

### Rationale
The spec requires `compile_form` to accept S-expression input. For bootstrap simplicity, the host (SBCL) parses the string and constructs WasmGC objects that Stage 0 receives as `externref`. This leverages the existing reader infrastructure and avoids implementing a full reader in minimal Wasm.

### Alternatives Considered
| Alternative | Pros | Cons | Why Rejected |
|-------------|------|------|--------------|
| Full Wasm reader | Self-contained | Complex (~500+ LOC) | Adds too much scope |
| Pre-parsed AST | Simple, leverages host | Depends on host shim | Acceptable for bootstrap |
| Tokenized stream | Partial parsing | Still needs Wasm parser | Middle ground, more work |

### Implementation Notes
- Host shim (`host-shim/stage1-host.js`) provides `parse_sexpr(string) -> externref`
- Returns WasmGC cons cells, symbols, and fixnums
- Stage 0 imports this as FFI function

---

## 3. Wasm Binary Output Strategy

### Decision: Direct Binary Emission (Existing Infrastructure)

### Rationale
The project already has `src/clysm/stage0/output.lisp` and `codegen.lisp` for generating Wasm binary. The interpreter evaluates forms and produces IR that the existing codegen transforms to binary.

### Implementation Notes
- Existing `emit-wasm-binary` handles section ordering
- Type section uses pre-defined indices from Constitution
- Export section for `compile_form`, `compile_all`

---

## 4. Symbol Interning Strategy

### Decision: String-based Equality (No Package System)

### Rationale
The Out of Scope section explicitly excludes "Package system (single namespace for bootstrap)". A simple hash table mapping symbol-name strings to symbol structs is sufficient.

### Implementation Notes
```lisp
;; Minimal symbol table
(defvar *symbol-table* (make-hash-table :test 'equal))

(defun intern-symbol (name)
  (or (gethash name *symbol-table*)
      (setf (gethash name *symbol-table*)
            (make-wasm-symbol name))))
```

---

## 5. Fixed-Point Source Definition

### Decision: Minimal Compiler as S-expression List

### Rationale
For Stage 1 == Stage 2, both stages must compile the same source to produce identical output. The "source" is a list of forms that define the minimal compiler itself.

### Source Forms (Bootstrap Subset)
```lisp
;; Minimal forms that Stage 0 must compile
(progn
  ;; Type constructors
  (defun make-fixnum (n) n)
  (defun make-cons (car cdr) (cons car cdr))
  (defun make-symbol (name) ...)

  ;; Primitives
  (defun prim-car (x) (car x))
  (defun prim-cdr (x) (cdr x))
  (defun prim-add (a b) (+ a b))
  (defun prim-sub (a b) (- a b))
  (defun prim-mul (a b) (* a b))
  (defun prim-div (a b) (/ a b))
  (defun prim-lt (a b) (< a b))
  (defun prim-gt (a b) (> a b))
  (defun prim-eq (a b) (eq a b))
  (defun prim-num-eq (a b) (= a b))

  ;; Evaluator
  (defun eval-form (form env)
    (if (fixnump form)
        form
        (if (symbolp form)
            (lookup env form)
            (if (consp form)
                (eval-compound form env)
                form))))

  ;; compile_form export
  (defun compile-form (sexpr)
    (let ((ir (eval-form sexpr nil)))
      (emit-wasm ir)))

  ;; compile_all export
  (defun compile-all (forms)
    (emit-wasm-module
     (mapcar (lambda (f) (eval-form f nil)) forms))))
```

### Estimated Size
- ~15-20 defun forms
- ~50-100 lines of S-expressions
- Expected Stage 1 size: 2-5 KB (meeting 1KB requirement)

---

## 6. WasmGC Type Layout Review

### Decision: Use Existing Type Indices from CLAUDE.md

### Type Mapping
| Index | Type | Bootstrap Usage |
|-------|------|-----------------|
| 0 | $cons | Cons cells, lists, environments |
| 1 | $symbol | Symbols with name string |
| 2 | $string | Symbol names |
| 3 | $closure | Lambda/defun implementations |
| 4 | $float | Not used in bootstrap |
| 21 | $slot-vector | Not used in bootstrap |

### Implementation Notes
```wat
;; Type definitions (already in Stage 0)
(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))))

(type $symbol (struct
  (field $name (ref $string))
  (field $value (mut anyref))
  (field $function (mut anyref))
  (field $plist (mut anyref))
  (field $package (mut anyref))))

;; Fixnum: i31ref (no type definition needed)
```

---

## 7. Environment Representation

### Decision: Association List of Cons Cells

### Rationale
For lexical binding in let/let*, a simple alist `((sym1 . val1) (sym2 . val2) ...)` suffices. Lookup is O(n) but acceptable for bootstrap scope.

### Implementation
```lisp
(defun extend-env (env bindings)
  "Add bindings to environment. Each binding is (symbol . value)."
  (if (null bindings)
      env
      (cons (car bindings)
            (extend-env env (cdr bindings)))))

(defun lookup (env symbol)
  "Find symbol's value in environment."
  (if (null env)
      (error "Unbound variable")
      (if (eq (caar env) symbol)
          (cdar env)
          (lookup (cdr env) symbol))))
```

---

## 8. Primitive Dispatch

### Decision: Symbol-based Case Dispatch

### Rationale
The evaluator checks if a form's head is a known primitive symbol and dispatches accordingly.

### Implementation Pattern
```lisp
(defun eval-compound (form env)
  (let ((head (car form)))
    (cond
      ((eq head 'quote) (cadr form))
      ((eq head 'if) (eval-if form env))
      ((eq head 'let) (eval-let form env))
      ((eq head 'let*) (eval-let* form env))
      ((eq head 'lambda) (make-closure form env))
      ((eq head 'defun) (eval-defun form env))
      ((eq head '+) (apply-arith #'+ (cdr form) env))
      ((eq head '-) (apply-arith #'- (cdr form) env))
      ((eq head '*) (apply-arith #'* (cdr form) env))
      ((eq head '/) (apply-arith #'/ (cdr form) env))
      ((eq head '<) (apply-cmp #'< (cdr form) env))
      ((eq head '>) (apply-cmp #'> (cdr form) env))
      ((eq head '=) (apply-cmp #'= (cdr form) env))
      ((eq head 'eq) (apply-eq (cdr form) env))
      ((eq head 'car) (car (eval-form (cadr form) env)))
      ((eq head 'cdr) (cdr (eval-form (cadr form) env)))
      ((eq head 'cons) (cons (eval-form (cadr form) env)
                             (eval-form (caddr form) env)))
      (t (apply-function head (cdr form) env)))))
```

---

## Summary

All research tasks complete. Key findings:

1. **Interpreter**: Recursive evaluator is sufficient for bootstrap
2. **Input**: Host parses strings, Stage 0 receives WasmGC objects
3. **Output**: Use existing codegen infrastructure
4. **Symbols**: String-based equality in single namespace
5. **Fixed-Point**: ~20 defun forms defining minimal compiler
6. **Types**: Reuse existing WasmGC type indices
7. **Environment**: Simple association list
8. **Dispatch**: Symbol-based case matching

**Next Steps**: Proceed to Phase 1 (data-model.md, contracts/).
