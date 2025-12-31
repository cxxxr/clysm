# Contract: FFI Usage Analyzer

**Module**: `clysm/compiler/analyzer/ffi-usage`
**Date**: 2025-12-31

## Functions

### analyze-ffi-usage

```lisp
(defun analyze-ffi-usage (form) ...)
```

**Purpose**: Analyze macro-expanded Lisp form to detect FFI function usage and dynamic call patterns.

**Input**:
- `form`: Macro-expanded S-expression

**Output**: `ffi-analysis` structure

**Contract**:

| Condition | Guarantee |
|-----------|-----------|
| Pure arithmetic `(+ 1 2)` | `used-ffis` is empty, `has-dynamic-call-p` is nil |
| Direct FFI `(write-char #\A)` | `used-ffis` contains `WRITE-CHAR` |
| Quoted funcall `(funcall 'sin 1.0)` | `used-ffis` contains `SIN`, `has-dynamic-call-p` is nil |
| Dynamic funcall `(funcall (intern "X") y)` | `has-dynamic-call-p` is t |
| Nested forms | All FFI calls detected recursively |

### detect-dynamic-call-p

```lisp
(defun detect-dynamic-call-p (form) ...)
```

**Purpose**: Check if a funcall/apply form uses dynamic function resolution.

**Input**:
- `form`: A `(funcall ...)` or `(apply ...)` form

**Output**: Boolean

**Contract**:

| Input | Output |
|-------|--------|
| `(funcall 'sym ...)` | `nil` (static) |
| `(funcall #'sym ...)` | `nil` (static) |
| `(funcall var ...)` | `t` (dynamic) |
| `(funcall (intern "X") ...)` | `t` (dynamic) |
| `(apply 'sym ...)` | `nil` (static) |
| `(apply fn args)` | `t` (dynamic) |

### get-ffi-function-names

```lisp
(defun get-ffi-function-names () ...)
```

**Purpose**: Return list of all registered FFI function names.

**Output**: List of symbols

**Contract**:
- Returns symbols from `*ffi-environment*` keys
- Empty list if no FFI registered
- Does not modify `*ffi-environment*`

## Test Cases

```lisp
;; T001: Pure computation has no FFI
(let ((result (analyze-ffi-usage '(+ 1 2))))
  (assert (null (ffi-analysis-used-ffis result)))
  (assert (null (ffi-analysis-has-dynamic-call-p result))))

;; T002: Direct FFI call detected
(let ((result (analyze-ffi-usage '(write-char #\A))))
  (assert (member 'write-char (ffi-analysis-used-ffis result))))

;; T003: Quoted funcall is static
(let ((result (analyze-ffi-usage '(funcall 'sin 1.0))))
  (assert (member 'sin (ffi-analysis-used-ffis result)))
  (assert (null (ffi-analysis-has-dynamic-call-p result))))

;; T004: Dynamic funcall detected
(let ((result (analyze-ffi-usage '(funcall (intern "FOO") x))))
  (assert (ffi-analysis-has-dynamic-call-p result)))

;; T005: Nested detection
(let ((result (analyze-ffi-usage '(progn (sin 1.0) (cos 2.0)))))
  (assert (member 'sin (ffi-analysis-used-ffis result)))
  (assert (member 'cos (ffi-analysis-used-ffis result))))
```
