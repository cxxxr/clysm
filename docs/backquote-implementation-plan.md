# SBCL-Independent Backquote Implementation Plan

## Current State Analysis

### Current Implementation

The clysm compiler currently handles backquotes through a two-layer approach:

1. **SBCL-Specific Layer** (compiler.lisp):
   - Uses `#+sbcl` conditionals to detect `sb-impl::comma` objects
   - Converts `sb-int:quasiquote` to `(backquote ...)` form
   - Normalizes SBCL comma objects to `(unquote ...)` / `(unquote-splicing ...)` forms

2. **Portable Expansion Layer** (macroexpand.lisp):
   - `expand-backquote` function handles the intermediate representation
   - Works with `(unquote x)` and `(splice-unquote x)` forms
   - Generates list-building code: `list`, `list*`, `cons`, `append`

### Problem

When self-hosting, we won't have SBCL's reader producing `sb-impl::comma` objects. We need:
1. A custom reader that produces our intermediate representation
2. Or a way to read backquoted forms without depending on SBCL internals

## Implementation Strategy

### Phase 1: Define Portable Intermediate Representation (IR)

Use symbolic forms that our reader can produce and our expander can consume:

```lisp
;; Backquote forms (produced by reader, consumed by expand-macros)
(quasiquote <form>)      ; `form
(unquote <form>)         ; ,form
(unquote-splicing <form>) ; ,@form

;; Note: We rename 'backquote' to 'quasiquote' for clarity
;; and 'splice-unquote' to 'unquote-splicing' for CLtL2 compatibility
```

### Phase 2: Update expand-backquote for Dual-Mode Operation

Modify `expand-backquote` and `expand-backquote-list` in macroexpand.lisp:

```lisp
(defun expand-backquote (form)
  "Expand a backquoted form into list-building code."
  (cond
    ;; SBCL COMMA object - convert to unquote/unquote-splicing
    ;; Only active during bootstrap on SBCL
    #+sbcl
    ((and (not *self-hosting-mode*)
          (typep form 'sb-impl::comma))
     (let ((expr (sb-impl::comma-expr form))
           (kind (sb-impl::comma-kind form)))
       (if (= kind 0)
           (expand-backquote `(unquote ,expr))
           (expand-backquote `(unquote-splicing ,expr)))))

    ;; Portable unquote form
    ((and (consp form) (eq (car form) 'unquote))
     (second form))

    ;; Portable unquote-splicing at top level is error
    ((and (consp form) (eq (car form) 'unquote-splicing))
     (error ",@ not inside list"))

    ;; Atom - quote non-self-evaluating atoms
    ((atom form)
     (if (self-evaluating-p form)
         form
         `(quote ,form)))

    ;; List - process recursively
    (t
     (expand-backquote-list form))))
```

### Phase 3: Create Custom Reader for Self-Hosting

Add backquote reader macros to `src/reader/reader.lisp`:

```lisp
;;; Backquote Reader

(defun read-quasiquote (stream char)
  "Read ` (backquote) syntax."
  (declare (ignore char))
  (list 'quasiquote (read stream t nil t)))

(defun read-unquote (stream char)
  "Read , (unquote) or ,@ (unquote-splicing) syntax."
  (declare (ignore char))
  (let ((next (peek-char nil stream t nil t)))
    (if (char= next #\@)
        (progn
          (read-char stream t nil t)
          (list 'unquote-splicing (read stream t nil t)))
        (list 'unquote (read stream t nil t)))))

(defun install-quasiquote-reader ()
  "Install reader macros for quasiquote syntax."
  (set-macro-character #\` #'read-quasiquote)
  (set-macro-character #\, #'read-unquote))
```

### Phase 4: Update Compiler Pipeline

Modify `expand-macros` in compiler.lisp to handle both modes:

```lisp
(defun expand-macros (form)
  "Recursively expand all macros in FORM."
  (cond
    ;; Bootstrap mode: Handle SBCL comma objects
    #+sbcl
    ((and (not *self-hosting-mode*)
          (typep form 'sb-impl::comma))
     (let ((expr (sb-impl::comma-expr form))
           (kind (sb-impl::comma-kind form)))
       (expand-macros
        (if (= kind 0)
            `(unquote ,expr)
            `(unquote-splicing ,expr)))))

    ;; Atoms
    ((atom form) form)

    ;; Quote - don't expand inside
    ((eq (car form) 'quote) form)

    ;; Quasiquote (our IR) - expand to list-building code
    ((eq (car form) 'quasiquote)
     (expand-macros (expand-backquote (second form))))

    ;; Bootstrap: SBCL's quasiquote syntax
    #+sbcl
    ((and (not *self-hosting-mode*)
          (eq (car form) 'sb-int:quasiquote))
     (expand-macros `(quasiquote ,(second form))))

    ;; Rest of expand-macros...
    ...))
```

## Implementation Files

### Files to Modify

1. **src/compiler/macroexpand.lisp**
   - Rename `splice-unquote` → `unquote-splicing` throughout
   - Add `self-evaluating-p` helper
   - Update `expand-backquote` for dual-mode
   - Update `expand-backquote-list` for dual-mode

2. **src/compiler/compiler.lisp**
   - Update `expand-macros` to handle `quasiquote` form
   - Wrap SBCL-specific code in `(not *self-hosting-mode*)` checks
   - Update `normalize-sbcl-internals` with same guards

3. **src/reader/reader.lisp** (new or extend)
   - Add `read-quasiquote` function
   - Add `read-unquote` function
   - Add `install-quasiquote-reader` function

### Files to Create

4. **src/reader/quasiquote.lisp** (optional, can be in reader.lisp)
   - Dedicated quasiquote reader implementation

## Migration Strategy

### Step 1: Backward-Compatible Changes

1. Add the `unquote-splicing` symbol as alias for current `splice-unquote`
2. Add `quasiquote` as alias for current `backquote` handling
3. Add `self-evaluating-p` helper function
4. Test that existing code still works

### Step 2: Reader Implementation

1. Implement `read-quasiquote` and `read-unquote`
2. Test with simple cases: `` `(a ,b ,@c) ``
3. Test with nested cases: `` `(a `(b ,,c)) ``

### Step 3: Dual-Mode Expansion

1. Add `*self-hosting-mode*` guards to SBCL-specific code
2. Update expand-backquote for dual-mode
3. Test both SBCL mode (bootstrap) and self-hosting mode

### Step 4: Integration Testing

1. Compile the compiler with backquoted forms
2. Verify macros using backquote expand correctly
3. Test nested backquotes work properly

## Edge Cases to Handle

1. **Nested backquotes**: `` `(a `(b ,,c ,@,@d)) ``
   - Each nesting level decreases/increases quote depth
   - Need proper depth tracking

2. **Unquote in non-list context**: `` `,x `` → just returns `x`

3. **Improper lists**: `` `(a . ,b) ``
   - Need to handle dotted pairs

4. **Unquote-splicing rules**:
   - Only valid inside lists
   - Error at top level or in non-list context

5. **Self-evaluating forms**:
   - Numbers, strings, characters, keywords: don't quote
   - NIL and T: don't quote
   - Other symbols: quote

## Self-Evaluating Predicate

```lisp
(defun self-evaluating-p (form)
  "Return T if FORM evaluates to itself."
  (or (null form)
      (eq form t)
      (numberp form)
      (stringp form)
      (characterp form)
      (keywordp form)
      (and (consp form)
           (eq (car form) 'quote))))
```

## Testing Plan

### Unit Tests

```lisp
;; Basic unquote
(test-backquote `(a ,b c) where b=2)
;; → (list 'a 2 'c)

;; Splice
(test-backquote `(a ,@b c) where b='(1 2))
;; → (append (list 'a) '(1 2) (list 'c))

;; Nested (most complex case)
(test-backquote `(a `(b ,,c)) where c='x)
;; → (list 'a (list 'quasiquote (list 'b (list 'unquote 'x))))
```

### Integration Tests

1. Compile defmacro forms that use backquote
2. Verify macro expansion produces correct code
3. Run compiled code and check results

## Timeline

This is a focused change with clear boundaries:
1. Add helper functions and aliases (minimal risk)
2. Implement reader macros (isolated, testable)
3. Update expand-backquote (core logic, needs careful testing)
4. Update compiler pipeline (integration)

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Break existing macros | Keep SBCL fallback during bootstrap |
| Nested backquote complexity | Follow CLtL2 specification exactly |
| Reader macro conflicts | Only install in self-hosting mode |
| Performance regression | Benchmark before/after |
