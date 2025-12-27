# Research: Interpreter Bootstrap Strategy

**Feature**: 044-interpreter-bootstrap
**Date**: 2025-12-28

## Research Tasks

1. Compiler source CL feature requirements (from blessed subset)
2. Existing interpreter architecture analysis
3. Macro system implementation patterns
4. LOOP macro implementation strategy
5. Condition system (handler-case/restart-case) patterns

---

## 1. Compiler Source CL Feature Requirements

### Decision: Use Blessed Subset as Implementation Target

The blessed subset (documented in `docs/blessed-subset.lisp`) defines exactly which CL features the interpreter must support.

### Rationale

Feature 036 performed comprehensive analysis of all compiler source files and identified the exact CL symbols used. This is the authoritative list.

### Feature Categories Required

**Special Forms (17):**
- BLOCK, CATCH, FLET, GO, IF, LABELS, LAMBDA, LET, LET*, MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, QUOTE, RETURN-FROM, SETQ, TAGBODY, THROW, UNWIND-PROTECT

**Macros (39):**
- Core: AND, CASE, COND, ECASE, ETYPECASE, NOT, OR, WHEN, UNLESS, PROG1, PROGN, RETURN, SETF
- Definition: DEFUN, DEFMACRO, DEFVAR, DEFPARAMETER, DEFCONSTANT, DEFSTRUCT, DEFCLASS, DEFMETHOD, DEFINE-CONDITION
- Iteration: LOOP, DO, DOLIST
- Multiple Values: MULTIPLE-VALUE-BIND, MULTIPLE-VALUE-LIST, NTH-VALUE, VALUES, VALUES-LIST
- Others: DESTRUCTURING-BIND, PUSH, POP, INCF, DECF, TYPECASE, WITH-OPEN-FILE, WITH-OUTPUT-TO-STRING, WITH-SIMPLE-RESTART, PRINT-UNREADABLE-OBJECT

**Functions (100+):**
- Arithmetic: +, -, *, /, 1+, 1-, ABS, MOD, REM, FLOOR, CEILING, TRUNCATE, ROUND, GCD, LCM, EXPT, SQRT
- Comparison: <, <=, =, >, >=, /=, EQ, EQL, EQUAL
- List: CAR, CDR, CONS, LIST, APPEND, REVERSE, NTH, NTHCDR, FIRST, REST, SECOND, THIRD, FOURTH
- Sequence: LENGTH, SUBSEQ, FIND, FIND-IF, POSITION, REMOVE, REMOVE-IF, MAPCAR, REDUCE, COUNT-IF-NOT
- Hash: MAKE-HASH-TABLE, GETHASH, CLRHASH, REMHASH, MAPHASH, HASH-TABLE-COUNT, HASH-TABLE-P
- String: CONCATENATE, STRING=, STRING-UPCASE, STRING-DOWNCASE, CHAR
- Type: TYPEP, TYPE-OF, COERCE, INTEGERP, FLOATP, STRINGP, SYMBOLP, CONSP, LISTP, FUNCTIONP

### Alternatives Considered

1. **Full ANSI CL**: Too broad; would implement unused features
2. **Minimal subset**: Risk missing features; would require iteration
3. **Blessed subset (chosen)**: Exactly what compiler needs; verified by Feature 036

---

## 2. Existing Interpreter Architecture Analysis

### Decision: Extend Current Environment-Chain Architecture

The existing interpreter uses a parent-chain environment model that is suitable for extension.

### Current Architecture (interpreter.lisp:400 LOC)

```lisp
;; Environment structure
(defstruct interpreter-env
  (bindings (make-hash-table :test 'eq))  ; symbol → value
  (parent nil))                            ; parent env or nil

;; Closure structure
(defstruct interpreted-closure
  (params nil)   ; lambda-list
  (body nil)     ; list of forms
  (env nil))     ; captured environment

;; Core dispatch
(defun interpret-form (form env)
  (cond
    ((self-evaluating-p form) form)
    ((symbolp form) (env-lookup env form))
    ((consp form) (interpret-list form env))
    (t (error "Cannot interpret: ~S" form))))
```

### Extensions Required

| Component | Current | Extended |
|-----------|---------|----------|
| Special forms | 13 | 17+ (add CATCH, THROW, UNWIND-PROTECT, etc.) |
| Built-ins | 30 | 100+ (add hash, string, type functions) |
| Macro support | None | Full defmacro with &whole, &environment |
| Lambda-list | Simple positional | &optional, &rest, &key, &aux, supplied-p |
| File loading | None | interpret-file with in-package handling |

### Rationale

- Parent-chain lookup is O(depth) but depth is typically <10; acceptable
- Hash-table per env frame allows O(1) binding lookup within frame
- Interpreted closures capture env reference; standard lexical scoping pattern
- No need for a fundamentally different architecture

### Alternatives Considered

1. **Flat environment with de Bruijn indices**: More complex; not needed for bootstrap
2. **Compile-on-load**: Defeats purpose of interpreter bootstrap
3. **Extend existing (chosen)**: Minimal risk; proven architecture

---

## 3. Macro System Implementation Patterns

### Decision: Separate Macro Registry with Interpreter-Based Expansion

Macros are stored in a separate registry from functions and expanded during interpretation.

### Implementation Pattern

```lisp
;; Macro registry (global, separate from function namespace)
(defvar *macro-registry* (make-hash-table :test 'eq))

;; Macro expander structure
(defstruct macro-expander
  (name nil :type symbol)
  (lambda-list nil :type list)
  (body nil :type list)
  (env nil :type (or null interpreter-env)))

;; interpret-defmacro: Register macro expander
(defun interpret-defmacro (name lambda-list body env)
  (let ((expander (make-macro-expander
                   :name name
                   :lambda-list lambda-list
                   :body body
                   :env env)))
    (setf (gethash name *macro-registry*) expander)
    name))

;; Macro expansion before interpretation
(defun maybe-macroexpand (form env)
  (if (and (consp form) (symbolp (car form)))
      (let ((expander (gethash (car form) *macro-registry*)))
        (if expander
            (let ((expanded (apply-macro-expander expander (cdr form) form env)))
              (maybe-macroexpand expanded env))  ; recursive expansion
            form))
      form))
```

### &whole and &environment Support

From Feature 042 patterns:
- `&whole form-var`: Binds the entire macro call form
- `&environment env-var`: Binds current lexical environment for nested expansion

```lisp
(defun parse-macro-lambda-list (lambda-list)
  "Returns (values whole-var env-var required optional rest keys body-var)"
  ;; Extract &whole if first element
  ;; Extract &environment from anywhere
  ;; Parse remaining as standard lambda-list with &body
  ...)
```

### Rationale

- Separate registry avoids namespace collision with functions
- Expansion before interpretation allows standard CL macro semantics
- Recursive expansion handles nested macros correctly
- Feature 042's patterns already tested for Clysm's macro system

### Alternatives Considered

1. **Host CL's macro-function**: Would bypass interpreter; breaks self-hosting
2. **Compile macros to Wasm**: Unnecessary complexity for bootstrap
3. **Interpreter macros (chosen)**: Self-contained; matches spec requirements

---

## 4. LOOP Macro Implementation Strategy

### Decision: Implement LOOP as Macro Expander to TAGBODY/GO

The LOOP macro expands to primitive control flow already supported by the interpreter.

### Expansion Pattern

```lisp
;; Source
(loop for i from 1 to 5 collect (* i i))

;; Expands to
(let ((#:result nil) (#:i 1))
  (tagbody
   #:start
     (when (> #:i 5) (go #:end))
     (push (* #:i #:i) #:result)
     (setq #:i (1+ #:i))
     (go #:start)
   #:end)
  (nreverse #:result))
```

### Supported Clauses (from blessed-subset.lisp partial notes)

- `:for var from/to/by`: Numeric iteration
- `:collect form`: Accumulate results
- `:do form`: Execute side effects
- `:return form`: Early exit
- `:while test`: Conditional continuation
- `:until test`: Conditional termination
- `:initially form`: Setup before loop
- `:finally form`: Cleanup after loop

### Implementation

```lisp
(defun expand-loop (clauses)
  "Expand LOOP clauses to TAGBODY/GO/LET form."
  (let ((bindings nil)
        (prologue nil)
        (body nil)
        (epilogue nil)
        (result-var (gensym "RESULT"))
        (start-tag (gensym "START"))
        (end-tag (gensym "END")))
    ;; Parse clauses and build components
    (loop for clause in (parse-loop-clauses clauses)
          do (case (clause-type clause)
               (:for (push (make-for-binding clause) bindings)
                     (push (make-for-step clause) body))
               (:collect (push `(push ,(clause-form clause) ,result-var) body))
               (:do (push (clause-form clause) body))
               ...))
    ;; Assemble expansion
    `(let (,@bindings (,result-var nil))
       ,@prologue
       (tagbody
        ,start-tag
          ,@body
          (go ,start-tag)
        ,end-tag)
       ,@epilogue
       (nreverse ,result-var))))
```

### Rationale

- TAGBODY/GO already implemented in interpreter
- Macro expansion reuses existing control flow
- Only need to implement clause parsing, not new special forms

### Alternatives Considered

1. **Special form for LOOP**: More complex; not needed
2. **Use host CL's LOOP**: Breaks self-hosting isolation
3. **Macro expansion (chosen)**: Leverages existing infrastructure

---

## 5. Condition System (handler-case/restart-case) Patterns

### Decision: Use Host CL's Condition System via Delegation

The interpreter delegates condition handling to host SBCL's condition system.

### Implementation Pattern

```lisp
;; handler-case interpretation
(defun interpret-handler-case (protected-form handlers env)
  "Interpret (handler-case form (type (var) handler-body)*)"
  (let ((translated-handlers
         (mapcar (lambda (h)
                   (destructuring-bind (type (var) &body body) h
                     `(,type (,var)
                        ;; Interpret handler body in current env
                        (interpret-form '(progn ,@body)
                                        (extend-env ,env (list (cons ',var ,var)))))))
                 handlers)))
    ;; Use host CL's handler-case
    (eval `(handler-case
               (interpret-form ',protected-form ,env)
             ,@translated-handlers))))

;; restart-case interpretation (similar pattern)
(defun interpret-restart-case (form restarts env)
  (let ((translated-restarts
         (mapcar (lambda (r)
                   (destructuring-bind (name lambda-list &body body) r
                     `(,name ,lambda-list
                        (interpret-form '(progn ,@body) ,env))))
                 restarts)))
    (eval `(restart-case
               (interpret-form ',form ,env)
             ,@translated-restarts))))
```

### Rationale

- Host SBCL provides full ANSI-compliant condition system
- Delegation is simpler than reimplementing condition stack
- Interpreted code seamlessly integrates with host conditions
- Self-hosting requirement is evaluation, not condition system reimplementation

### Caveats

- Interpreted conditions use host types, not Clysm's define-condition
- Acceptable for bootstrap; full isolation not required

### Alternatives Considered

1. **Full condition system reimplementation**: Overkill for bootstrap
2. **No condition support**: Would break compiler source (uses handler-case extensively)
3. **Host delegation (chosen)**: Pragmatic; meets spec requirements

---

## Summary

| Research Area | Decision | Key Pattern |
|---------------|----------|-------------|
| Feature Requirements | Blessed subset | 17 special forms, 39 macros, 100+ functions |
| Architecture | Extend current | Parent-chain env, hash-table bindings |
| Macro System | Separate registry | Expand before interpret, &whole/&environment |
| LOOP Macro | Macro expansion | Expand to TAGBODY/GO |
| Condition System | Host delegation | Use SBCL's handler-case/restart-case |

All NEEDS CLARIFICATION items from Technical Context have been resolved.
