# API Contract: Primitive Dispatch

**Module**: `clysm/compiler/codegen/primitive-dispatch`
**Version**: 1.0.0
**Date**: 2026-01-03

## Public API

### Registration Functions

#### register-primitive-compiler

Registers a primitive compiler function.

```lisp
(register-primitive-compiler symbol compiler-fn &key arity flags string-name)
```

**Parameters**:
| Name | Type | Required | Description |
|------|------|----------|-------------|
| `symbol` | `symbol` | Yes | The primitive operation symbol |
| `compiler-fn` | `function` | Yes | Compiler function `(op args env) -> list` |
| `arity` | `(or fixnum null)` | No | Expected argument count, `nil` for variadic |
| `flags` | `list` | No | Property list of optional metadata |
| `string-name` | `(or string null)` | No | If provided, also register with this string key |

**Returns**: `primitive-entry` - The registered entry

**Side Effects**: Modifies `*primitive-symbol-table*` and optionally `*primitive-string-table*`

**Signals**:
- `type-error` if `symbol` is not a symbol
- `type-error` if `compiler-fn` is not a function
- `type-error` if `arity` is provided but not a non-negative integer

**Example**:
```lisp
;; Register a simple primitive
(register-primitive-compiler 'cons #'compile-cons :arity 2)

;; Register with string lookup
(register-primitive-compiler 'slot-value* #'compile-slot-value*
                             :string-name "SLOT-VALUE*")
```

---

#### unregister-primitive-compiler

Removes a primitive compiler registration.

```lisp
(unregister-primitive-compiler symbol &key string-name)
```

**Parameters**:
| Name | Type | Required | Description |
|------|------|----------|-------------|
| `symbol` | `symbol` | Yes | The primitive operation symbol |
| `string-name` | `(or string null)` | No | If provided, also remove from string table |

**Returns**: `boolean` - `t` if entry was found and removed, `nil` otherwise

**Side Effects**: Modifies `*primitive-symbol-table*` and optionally `*primitive-string-table*`

---

### Query Functions

#### primitive-compiler-entry

Looks up a primitive compiler entry.

```lisp
(primitive-compiler-entry op)
```

**Parameters**:
| Name | Type | Required | Description |
|------|------|----------|-------------|
| `op` | `symbol` | Yes | The operation symbol to look up |

**Returns**: `(or primitive-entry null)` - The entry if found, `nil` otherwise

**Lookup Order**:
1. Symbol lookup in `*primitive-symbol-table*` using `eq`
2. If not found, string lookup in `*primitive-string-table*` using `(symbol-name op)`

**Example**:
```lisp
(primitive-compiler-entry 'cons)
;; => #S(PRIMITIVE-ENTRY :COMPILER-FN #<FUNCTION COMPILE-CONS> :ARITY 2 :FLAGS NIL)

(primitive-compiler-entry 'unknown-op)
;; => NIL
```

---

#### primitive-registered-p

Checks if a primitive is registered.

```lisp
(primitive-registered-p op)
```

**Parameters**:
| Name | Type | Required | Description |
|------|------|----------|-------------|
| `op` | `symbol` | Yes | The operation symbol to check |

**Returns**: `boolean` - `t` if registered, `nil` otherwise

---

#### list-registered-primitives

Lists all registered primitive symbols.

```lisp
(list-registered-primitives &key table)
```

**Parameters**:
| Name | Type | Required | Description |
|------|------|----------|-------------|
| `table` | `(member :symbol :string :all)` | No | Which table(s) to list, default `:all` |

**Returns**: `list` - List of registered keys (symbols or strings)

---

### Dispatch Function

#### dispatch-primitive

Main dispatch function (internal, called by compile-primitive-call).

```lisp
(dispatch-primitive op args env)
```

**Parameters**:
| Name | Type | Required | Description |
|------|------|----------|-------------|
| `op` | `symbol` | Yes | The primitive operation |
| `args` | `list` | Yes | Argument forms |
| `env` | `compilation-env` | Yes | Compilation environment |

**Returns**: `(or list null)` - Instruction list if dispatched, `nil` if not found

**Behavior**:
1. Look up `op` in symbol table
2. If not found, look up `(symbol-name op)` in string table
3. If found, call `(funcall (primitive-entry-compiler-fn entry) op args env)`
4. Return result or `nil`

---

## Internal API

These functions are not exported but documented for maintainability.

### clear-primitive-tables

Clears all registrations (for testing).

```lisp
(clear-primitive-tables)
```

---

### describe-primitive

Prints information about a registered primitive (for debugging).

```lisp
(describe-primitive op &optional (stream *standard-output*))
```

---

## Constants

```lisp
(defconstant +primitive-symbol-table-size+ 300
  "Initial size for symbol dispatch table.")

(defconstant +primitive-string-table-size+ 50
  "Initial size for string dispatch table.")
```

---

## Error Conditions

### primitive-registration-error

Signaled when registration fails validation.

```lisp
(define-condition primitive-registration-error (error)
  ((symbol :initarg :symbol :reader registration-error-symbol)
   (reason :initarg :reason :reader registration-error-reason)))
```

---

## Thread Safety

Current implementation is **not thread-safe**. Tables are modified at load time and assumed read-only during compilation. If parallel compilation is needed, use `sb-ext:with-locked-hash-table` or redesign with immutable snapshots.

---

## Versioning

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-03 | Initial API design |
