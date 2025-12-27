# Data Model: LOOP Macro

**Feature**: 029-loop-macro
**Date**: 2025-12-27

## Overview

LOOP is a compile-time macro. This document defines the internal data structures used during macro expansion, not runtime objects. All structures are ephemeral and exist only during compilation.

## Core Entities

### 1. loop-context

Holds the complete state of a LOOP form during parsing and expansion.

```lisp
(defstruct loop-context
  "Complete LOOP parsing and expansion context"
  (name nil :type (or null symbol))           ; NAMED loop name
  (iteration-clauses nil :type list)          ; List of loop-iteration-clause
  (accumulation-clauses nil :type list)       ; List of loop-accumulation-clause
  (termination-clauses nil :type list)        ; List of loop-termination-clause
  (body-clauses nil :type list)               ; DO/conditional clauses
  (initially-forms nil :type list)            ; INITIALLY forms
  (finally-forms nil :type list)              ; FINALLY forms
  (with-bindings nil :type list)              ; ((var init) ...) from WITH
  (result-form nil :type t)                   ; Final return expression
  (gensym-counter 0 :type fixnum))            ; For unique variable generation
```

**Relationships**:
- Contains 0+ `loop-iteration-clause`
- Contains 0+ `loop-accumulation-clause`
- Contains 0+ `loop-termination-clause`
- Used by `expand-loop` to generate tagbody form

---

### 2. loop-iteration-clause

Represents a single FOR/AS iteration specification.

```lisp
(defstruct loop-iteration-clause
  "Base structure for iteration clauses"
  (var nil :type symbol)                      ; Loop variable
  (clause-type nil :type keyword))            ; :arithmetic, :in, :on, :across, :hash-keys, :hash-values, :equals

;; Subtypes by clause-type:

(defstruct (loop-iter-arithmetic (:include loop-iteration-clause))
  "FOR var FROM x TO y BY z"
  (from nil :type t)                          ; Start expression
  (to nil :type t)                            ; End expression (inclusive)
  (below nil :type t)                         ; End expression (exclusive)
  (above nil :type t)                         ; End expression (exclusive descending)
  (downto nil :type t)                        ; End expression (inclusive descending)
  (downfrom nil :type t)                      ; Start value for descending
  (upfrom nil :type t)                        ; Start value for ascending
  (by nil :type t))                           ; Step expression (default 1)

(defstruct (loop-iter-in (:include loop-iteration-clause))
  "FOR var IN list [BY step-fn]"
  (list-form nil :type t)                     ; List expression
  (step-fn nil :type t))                      ; Step function (default #'cdr)

(defstruct (loop-iter-on (:include loop-iteration-clause))
  "FOR var ON list [BY step-fn]"
  (list-form nil :type t)
  (step-fn nil :type t))

(defstruct (loop-iter-across (:include loop-iteration-clause))
  "FOR var ACROSS vector"
  (vector-form nil :type t)                   ; Vector expression
  (index-var nil :type symbol))               ; Generated index variable

(defstruct (loop-iter-hash (:include loop-iteration-clause))
  "FOR var BEING THE HASH-KEYS/VALUES OF hash-table"
  (hash-form nil :type t)                     ; Hash table expression
  (value-var nil :type symbol)                ; Optional USING (HASH-VALUE v)
  (key-var nil :type symbol)                  ; Optional USING (HASH-KEY k)
  (mode :keys :type keyword))                 ; :keys or :values

(defstruct (loop-iter-equals (:include loop-iteration-clause))
  "FOR var = init-form [THEN step-form]"
  (init-form nil :type t)                     ; Initial value
  (then-form nil :type t))                    ; Step expression (nil = no stepping)
```

**State Transitions** (for expansion):
```
parse → validate → generate-init → generate-step → generate-test
```

---

### 3. loop-accumulation-clause

Represents a value accumulation operation.

```lisp
(defstruct loop-accumulation-clause
  "Accumulation clause"
  (type nil :type keyword)                    ; :collect, :sum, :count, :maximize, :minimize, :append, :nconc
  (expr nil :type t)                          ; Expression to accumulate
  (into-var nil :type symbol)                 ; Optional INTO variable
  (acc-var nil :type symbol))                 ; Generated accumulator variable

;; Type-specific behavior:
;; :collect  -> (nconc acc (list expr))
;; :sum      -> (+ acc expr)
;; :count    -> (+ acc 1) when expr non-nil
;; :maximize -> (if (or (null acc) (> expr acc)) expr acc)
;; :minimize -> (if (or (null acc) (< expr acc)) expr acc)
;; :append   -> (append acc expr)
;; :nconc    -> (nconc acc expr)
```

**Validation Rules**:
- All accumulations without INTO must be same type (error otherwise)
- WITH INTO, different types allowed (user manages variables)
- COLLECT/APPEND/NCONC → list result
- SUM/COUNT → numeric result
- MAXIMIZE/MINIMIZE → numeric or nil

---

### 4. loop-termination-clause

Represents termination conditions and early exit.

```lisp
(defstruct loop-termination-clause
  "Termination or boolean aggregation clause"
  (type nil :type keyword)                    ; :while, :until, :always, :never, :thereis, :return
  (expr nil :type t))                         ; Condition or return expression

;; Type behavior:
;; :while   -> continue while expr is true
;; :until   -> continue until expr is true
;; :always  -> return NIL immediately if expr is NIL; T if all pass
;; :never   -> return NIL immediately if expr is non-NIL; T if all pass
;; :thereis -> return expr immediately if non-NIL; NIL otherwise
;; :return  -> return expr immediately, skip FINALLY
```

---

### 5. loop-conditional-clause

Represents IF/WHEN/UNLESS conditionals.

```lisp
(defstruct loop-conditional-clause
  "Conditional execution"
  (type nil :type keyword)                    ; :if, :when, :unless
  (condition nil :type t)                     ; Test expression
  (then-clauses nil :type list)               ; Clauses when true (or false for :unless)
  (else-clauses nil :type list))              ; Optional ELSE clauses
```

**Scope**: Conditionals scope over following clauses until:
- Another conditional at same level
- END keyword
- End of LOOP clauses

---

## Expansion Phases

### Phase 1: Parsing
```
(loop ...) → loop-context with parsed clauses
```

### Phase 2: Validation
```
loop-context → check accumulation conflicts, type compatibility
```

### Phase 3: Variable Generation
```
loop-context → generate gensyms for accumulators, indices, list-tails
```

### Phase 4: Code Generation
```
loop-context → tagbody/block form
```

Output structure:
```lisp
(let ((acc-vars...) (iter-vars...) (with-vars...))
  (block loop-name
    (tagbody
      ;; INITIALLY
      initially-forms...

      loop-start
      ;; Termination tests
      (if termination-test (go loop-end))

      ;; Body with conditionals
      body-forms...

      ;; Accumulation updates
      accumulation-updates...

      ;; Iteration stepping
      (psetq iter-var1 step1 iter-var2 step2 ...)
      (go loop-start)

      loop-end)
    ;; FINALLY
    finally-forms...
    ;; Result
    result-form))
```

---

## Keyword Mapping

| LOOP Keyword | Structure | Field |
|--------------|-----------|-------|
| FOR/AS | loop-iteration-clause | clause-type determines subtype |
| FROM | loop-iter-arithmetic | from |
| TO | loop-iter-arithmetic | to |
| BELOW | loop-iter-arithmetic | below |
| ABOVE | loop-iter-arithmetic | above |
| DOWNTO | loop-iter-arithmetic | downto |
| DOWNFROM | loop-iter-arithmetic | downfrom |
| UPFROM | loop-iter-arithmetic | upfrom |
| BY | loop-iter-arithmetic/in/on | by/step-fn |
| IN | loop-iter-in | list-form |
| ON | loop-iter-on | list-form |
| ACROSS | loop-iter-across | vector-form |
| BEING | loop-iter-hash | (triggers hash iteration) |
| HASH-KEYS | loop-iter-hash | mode = :keys |
| HASH-VALUES | loop-iter-hash | mode = :values |
| = | loop-iter-equals | init-form |
| THEN | loop-iter-equals | then-form |
| COLLECT/ING | loop-accumulation-clause | type = :collect |
| SUM/MING | loop-accumulation-clause | type = :sum |
| COUNT/ING | loop-accumulation-clause | type = :count |
| MAXIMIZE/ING | loop-accumulation-clause | type = :maximize |
| MINIMIZE/ING | loop-accumulation-clause | type = :minimize |
| APPEND/ING | loop-accumulation-clause | type = :append |
| NCONC/ING | loop-accumulation-clause | type = :nconc |
| INTO | loop-accumulation-clause | into-var |
| WHILE | loop-termination-clause | type = :while |
| UNTIL | loop-termination-clause | type = :until |
| ALWAYS | loop-termination-clause | type = :always |
| NEVER | loop-termination-clause | type = :never |
| THEREIS | loop-termination-clause | type = :thereis |
| RETURN | loop-termination-clause | type = :return |
| IF/WHEN | loop-conditional-clause | type = :if/:when |
| UNLESS | loop-conditional-clause | type = :unless |
| ELSE | loop-conditional-clause | else-clauses |
| END | (scope terminator) | - |
| AND | (clause combiner) | - |
| DO/DOING | loop-context | body-clauses |
| INITIALLY | loop-context | initially-forms |
| FINALLY | loop-context | finally-forms |
| WITH | loop-context | with-bindings |
| NAMED | loop-context | name |
| LOOP-FINISH | (special form) | jumps to loop-end |
