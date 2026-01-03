# Data Model: Primitive Dispatch Table

**Branch**: `001-primitive-dispatch-table`
**Date**: 2026-01-03

## Entities

### 1. Primitive Entry

Represents a registered primitive compiler.

```lisp
(defstruct primitive-entry
  "Entry in the primitive dispatch table."
  (compiler-fn nil :type (or function null))
  (arity nil :type (or fixnum null))
  (flags nil :type list))
```

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| `compiler-fn` | `function` | Compiler function `(op args env) -> instructions` | Required, non-nil |
| `arity` | `fixnum` or `nil` | Expected argument count | `nil` = variadic |
| `flags` | `plist` | Optional metadata for future extensions | Empty by default |

**Example flags** (for future use):
- `:inline-hint t` - Prefer inline expansion
- `:pure t` - No side effects
- `:foldable t` - Can be constant-folded

### 2. Primitive Symbol Table

Global hash table for symbol-based lookup.

```lisp
(defparameter *primitive-symbol-table*
  (make-hash-table :test 'eq :size 300)
  "Maps Lisp symbols to primitive-entry structures.
   Uses EQ test for O(1) symbol identity lookup.")
```

| Key Type | Value Type | Test | Size Hint |
|----------|------------|------|-----------|
| `symbol` | `primitive-entry` | `eq` | 300 |

### 3. Primitive String Table

Global hash table for string-based lookup.

```lisp
(defparameter *primitive-string-table*
  (make-hash-table :test 'equal :size 50)
  "Maps string names to primitive-entry structures.
   Uses EQUAL test for string comparison.
   Used for cross-package symbols like %SETF-AREF.")
```

| Key Type | Value Type | Test | Size Hint |
|----------|------------|------|-----------|
| `string` | `primitive-entry` | `equal` | 50 |

## Relationships

```
+-------------------+       +-------------------+
| Symbol            |       | String            |
| (e.g., 'cons)     |       | (e.g., "%SETF-AREF") |
+-------------------+       +-------------------+
         |                           |
         | gethash (eq)              | gethash (equal)
         v                           v
+-------------------+       +-------------------+
| *primitive-       |       | *primitive-       |
|  symbol-table*    |       |  string-table*    |
+-------------------+       +-------------------+
         |                           |
         +-----------+---------------+
                     |
                     v
          +-------------------+
          | primitive-entry   |
          |-------------------|
          | compiler-fn       |
          | arity             |
          | flags             |
          +-------------------+
                     |
                     | funcall
                     v
          +-------------------+
          | compile-* fn      |
          | (e.g., compile-+) |
          +-------------------+
```

## State Transitions

### Registration Lifecycle

```
                    +-------------+
                    | Unregistered |
                    +-------------+
                          |
                          | register-primitive-compiler
                          v
                    +-------------+
                    | Registered   |
                    +-------------+
                          |
                          | register-primitive-compiler (same key)
                          v
                    +-------------+
                    | Re-registered |<-----+
                    +-------------+       |
                          |               |
                          +---------------+
```

### Lookup Lifecycle

```
compile-primitive-call(op, args, env)
           |
           v
    +------+------+
    | symbol?     |
    +------+------+
      yes  |  no
           |   |
           v   v
    +------+------+------+
    | lookup in          |
    | *primitive-        |
    | symbol-table*      |
    +------+------+------+
      found| not found
           |   |
           v   v
    +------+------+------+
    | lookup in          |
    | *primitive-        |
    | string-table*      |
    | (by symbol-name)   |
    +------+------+------+
      found| not found
           |   |
           v   v
    +------+------+------+
    | dispatch to        | --> fallback to
    | entry.compiler-fn  |     default compilation
    +------+------+------+
```

## Validation Rules

### Registration Validation

1. **compiler-fn**: Must be a callable function
   ```lisp
   (check-type compiler-fn function)
   ```

2. **arity**: If provided, must be non-negative
   ```lisp
   (check-type arity (or null (integer 0 *)))
   ```

3. **symbol**: Must be a symbol for symbol-table registration
   ```lisp
   (check-type symbol symbol)
   ```

4. **string-name**: If provided, must be a non-empty string
   ```lisp
   (check-type string-name (or null (and string (not (eql "")))))
   ```

### Lookup Validation

1. **op**: Must be a symbol
   ```lisp
   (check-type op symbol)
   ```

2. **args**: Must be a list
   ```lisp
   (check-type args list)
   ```

3. **env**: Must be a valid compilation environment
   ```lisp
   (check-type env compilation-env)
   ```

## Data Volume Estimates

| Table | Current Size | Expected Max | Growth Rate |
|-------|-------------|--------------|-------------|
| Symbol table | 0 → 248 | 300 | Low (feature additions) |
| String table | 0 → 18 | 50 | Very low |

## Invariants

1. **No duplicate keys**: Each symbol/string appears at most once per table
2. **Entry integrity**: Every entry has a non-nil `compiler-fn`
3. **Table consistency**: Tables are always in valid state (no partial updates)
4. **Immutable after load**: Tables populated at load time, rarely modified at runtime
