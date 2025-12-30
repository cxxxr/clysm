# Quickstart: LOOP Macro Extension

**Feature**: 001-loop-extension
**Date**: 2025-12-30

## Overview

This feature extends Clysm's LOOP macro to support hash-table iteration, WITH clause local variables, FINALLY cleanup, and INTO named accumulators.

## Prerequisites

- SBCL 2.4+ installed
- Nix with flakes enabled
- Project cloned and `nix develop` shell active

## Development Setup

```bash
# Enter development environment
nix develop

# Run existing tests to verify baseline
sbcl --eval "(asdf:test-system :clysm)"
```

## Implementation Guide

### Step 1: Extend loop-iter-hash Structure

Location: `src/clysm/lib/macros.lisp` (around line 620)

Find the `defstruct loop-iter-hash` definition and add new fields:

```lisp
(defstruct (loop-iter-hash (:include loop-iteration-clause))
  "Hash-table iteration clause."
  hash-form                    ; existing
  mode                         ; existing: :keys or :values
  using-var                    ; NEW: secondary variable
  using-type                   ; NEW: hash-key or hash-value
  entries-var                  ; NEW: gensym for collected entries
  iter-var)                    ; NEW: gensym for iteration position
```

### Step 2: Update parse-for-hash

Location: `src/clysm/lib/macros.lisp`, function `parse-for-hash` (line 961)

Add USING clause parsing after hash-form extraction:

```lisp
;; After: (let ((hash-form (first rest2)) (final-rest (rest rest2)))
;; Add:
(let ((using-var nil)
      (using-type nil))
  ;; Check for USING clause
  (when (loop-keyword-eq (first final-rest) 'using)
    (let ((using-spec (second final-rest)))
      (setf using-type (if (loop-keyword-eq (first using-spec) 'hash-value)
                           :hash-value
                           :hash-key))
      (setf using-var (second using-spec))
      (setf final-rest (cddr final-rest))))
  ;; Create struct with new fields
  ...)
```

### Step 3: Implement Hash-Table Code Generation

Add cases to these functions in `src/clysm/lib/macros.lisp`:

#### generate-iteration-bindings (line 1189)

```lisp
;; Add case for loop-iter-hash-p
((loop-iter-hash-p clause)
 (let* ((hash-form (loop-iter-hash-hash-form clause))
        (entries-var (or (loop-iter-hash-entries-var clause)
                         (gensym "HT-ENTRIES-")))
        (iter-var (or (loop-iter-hash-iter-var clause)
                      (gensym "HT-ITER-")))
        (var (loop-iteration-clause-var clause))
        (mode (loop-iter-hash-mode clause))
        (using-var (loop-iter-hash-using-var clause)))
   ;; Store gensyms back for other generators
   (setf (loop-iter-hash-entries-var clause) entries-var)
   (setf (loop-iter-hash-iter-var clause) iter-var)
   ;; Collect entries upfront
   (push `(,entries-var (let ((#:acc nil))
                          (maphash (lambda (#:k #:v)
                                     (push (cons #:k #:v) #:acc))
                                   ,hash-form)
                          (nreverse #:acc)))
         bindings)
   (push `(,iter-var ,entries-var) bindings)
   ;; Primary variable
   (push `(,var (if ,iter-var
                    (,(if (eq mode :keys) 'caar 'cdar) ,iter-var)
                    nil))
         bindings)
   ;; Secondary variable (if USING specified)
   (when using-var
     (push `(,using-var (if ,iter-var
                            (,(if (eq mode :keys) 'cdar 'caar) ,iter-var)
                            nil))
           bindings))))
```

#### generate-termination-tests (line 1280)

```lisp
;; Add case for loop-iter-hash-p
((loop-iter-hash-p clause)
 (let ((iter-var (loop-iter-hash-iter-var clause)))
   (push `(when (null ,iter-var) (go ,loop-end)) tests)))
```

#### generate-iteration-steps (line 1428)

```lisp
;; Add case for loop-iter-hash-p
((loop-iter-hash-p clause)
 (let* ((iter-var (loop-iter-hash-iter-var clause))
        (var (loop-iteration-clause-var clause))
        (mode (loop-iter-hash-mode clause))
        (using-var (loop-iter-hash-using-var clause)))
   ;; Step iterator
   (push iter-var step-pairs)
   (push `(cdr ,iter-var) step-pairs)
   ;; Update primary var
   (push var step-pairs)
   (push `(if ,iter-var
              (,(if (eq mode :keys) 'caar 'cdar) ,iter-var)
              nil)
         step-pairs)
   ;; Update secondary var
   (when using-var
     (push using-var step-pairs)
     (push `(if ,iter-var
                (,(if (eq mode :keys) 'cdar 'caar) ,iter-var)
                nil)
           step-pairs))))
```

### Step 4: Write Tests (TDD)

Create `tests/unit/loop-extension/hash-iteration-test.lisp`:

```lisp
(defpackage #:clysm/tests/loop-extension/hash-iteration
  (:use #:cl #:rove))

(in-package #:clysm/tests/loop-extension/hash-iteration)

(deftest hash-keys-basic
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (let ((keys (loop for k being the hash-keys of ht collect k)))
      (ok (= 2 (length keys)))
      (ok (member :a keys))
      (ok (member :b keys)))))

(deftest hash-keys-with-using
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (let ((pairs (loop for k being the hash-keys of ht
                       using (hash-value v)
                       collect (cons k v))))
      (ok (= 2 (length pairs)))
      (ok (member '(:a . 1) pairs :test #'equal))
      (ok (member '(:b . 2) pairs :test #'equal)))))

(deftest empty-hash-table
  (let ((ht (make-hash-table)))
    (ok (null (loop for k being the hash-keys of ht collect k)))))
```

## Testing

```bash
# Run all tests
sbcl --eval "(asdf:test-system :clysm)"

# Run only loop extension tests
sbcl --eval "(asdf:test-system :clysm/tests/loop-extension)"

# Validate generated Wasm
wasm-tools validate dist/output.wasm
```

## Verification

### Success Criteria Checklist

- [ ] ANSI CL iteration tests 50%+ pass rate
- [ ] All 40 hash-table patterns in compiler compile successfully
- [ ] No regression in existing LOOP functionality
- [ ] Wasm validation passes
- [ ] Runtime behavior matches SBCL

### Manual Verification

```lisp
;; In SBCL REPL, test macro expansion:
(macroexpand-1 '(loop for k being the hash-keys of ht collect k))

;; Compare with Clysm expansion:
(clysm/lib/macros::make-loop-expander) ; get expander
;; Then manually invoke on same form
```

## Common Issues

1. **"Unknown clause type"**: Ensure `loop-iter-hash-p` predicate exists
2. **"Undefined variable in FINALLY"**: Check binding order in `all-bindings`
3. **"Hash-table iteration returns NIL"**: Verify `entries-var` is populated

## References

- [LOOP macro](resources/HyperSpec/Body/m_loop.htm)
- [maphash](resources/HyperSpec/Body/f_maphas.htm)
- [with-hash-table-iterator](resources/HyperSpec/Body/m_w_hash.htm)
