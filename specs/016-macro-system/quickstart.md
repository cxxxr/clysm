# Quickstart: Macro System

## Defining Macros

Use `defmacro` to define compile-time macros:

```lisp
(defmacro my-when (test &body body)
  "Execute BODY only when TEST is true."
  `(if ,test
       (progn ,@body)
       nil))
```

## Using Macros

Macros are called like functions but expand at compile time:

```lisp
(my-when (> x 0)
  (print "positive")
  x)

;; Expands to:
;; (if (> x 0)
;;     (progn (print "positive") x)
;;     nil)
```

## Backquote Syntax

Use backquote for code templates:

| Syntax | Meaning |
|--------|---------|
| `` `form `` | Quote the form (template) |
| `,expr` | Evaluate and insert expr |
| `,@list` | Splice list elements |

Example:
```lisp
(let ((x 1)
      (xs '(2 3)))
  `(a ,x ,@xs d))
;; => (a 1 2 3 d)
```

## Lambda List Keywords

Macro lambda lists support:

| Keyword | Purpose |
|---------|---------|
| `&optional` | Optional parameters with defaults |
| `&rest` | Collect remaining args as list |
| `&body` | Like &rest, hints at body forms |
| `&key` | Keyword parameters |

Example with &key:
```lisp
(defmacro with-options (name &key (timeout 30) verbose)
  `(let ((*timeout* ,timeout)
         (*verbose* ,verbose))
     ,name))

(with-options (do-work) :timeout 60 :verbose t)
```

## Standard Macros

### Control Flow
- `(when test &body body)` - execute body if test is true
- `(unless test &body body)` - execute body if test is false
- `(cond &rest clauses)` - multi-branch conditional
- `(case keyform &rest clauses)` - key-based dispatch

### Boolean
- `(and &rest forms)` - short-circuit AND
- `(or &rest forms)` - short-circuit OR

### Iteration
- `(dolist (var list [result]) &body body)` - iterate over list
- `(dotimes (var count [result]) &body body)` - iterate n times
- `(do vars (end-test &rest result) &body body)` - general loop

### Sequencing
- `(prog1 first &body rest)` - return first, execute rest
- `(prog2 first second &body rest)` - return second

## Macro Expansion Inspection

Debug macros with expansion functions:

```lisp
;; Single expansion step
(macroexpand-1 '(when t 1 2 3))
;; => (if t (progn 1 2 3) nil)

;; Full expansion (recursive)
(macroexpand '(when (and a b) 1))
;; => (if (if a (and b) nil) (progn 1) nil)
```

## Common Patterns

### Gensym for Hygiene

Avoid variable capture with gensym:

```lisp
(defmacro with-timing (&body body)
  (let ((start (gensym "START-")))
    `(let ((,start (get-internal-real-time)))
       ,@body
       (- (get-internal-real-time) ,start))))
```

### Recursive Macros

Macros can expand to other macro calls:

```lisp
(defmacro my-and (&rest forms)
  (cond
    ((null forms) t)
    ((null (rest forms)) (first forms))
    (t `(if ,(first forms)
            (my-and ,@(rest forms))
            nil))))
```

## Errors

The compiler reports:
- **Undefined macro**: Using a macro before definition
- **Expansion depth exceeded**: Infinite expansion loop (limit: 1000)
- **Wrong arguments**: Macro called with incorrect argument count

## Testing Macros

Write tests for macro expansion:

```lisp
(deftest when-expansion
  (ok (equal (macroexpand-1 '(when t 1 2 3))
             '(if t (progn 1 2 3) nil))))

(deftest when-execution
  (ok (= 3 (when t 1 2 3)))
  (ok (null (when nil 1 2 3))))
```
