# FFI API Contract

**Feature**: 027-complete-ffi
**Date**: 2025-12-27

## Lisp API

### ffi:define-foreign-function

Declare a host function for import.

```lisp
(ffi:define-foreign-function lisp-name host-name param-types return-type)
```

**Parameters**:
- `lisp-name` (symbol): Name to bind in Lisp
- `host-name` (string): "module.field" naming the host function
- `param-types` (list): List of marshal type keywords
- `return-type` (keyword): Marshal type or `:void`

**Effects**:
- Registers declaration in `*ffi-environment*`
- Defines a Lisp function stub (for interpreter mode errors)

**Example**:
```lisp
(ffi:define-foreign-function console-log "host.log" (:string) :void)
(ffi:define-foreign-function host-add "host.add" (:fixnum :fixnum) :fixnum)
```

**Generated Wasm** (Import Section):
```wat
(import "host" "log" (func $console-log (param externref)))
(import "host" "add" (func $host-add (param i32 i32) (result i32)))
```

---

### ffi:export-function

Export a Lisp function for host invocation.

```lisp
(ffi:export-function lisp-name &key as signature)
```

**Parameters**:
- `lisp-name` (symbol): Lisp function to export
- `:as` (string, optional): Export name (defaults to lowercase lisp-name)
- `:signature` (list): `((param-types...) return-type)`

**Effects**:
- Registers export declaration in `*ffi-environment*`
- Causes wrapper function generation during compilation

**Example**:
```lisp
(ffi:export-function my-add :as "add" :signature ((:fixnum :fixnum) :fixnum))
```

**Generated Wasm** (Export Section):
```wat
(export "add" (func $export-wrapper-my-add))
```

---

### ffi:call-host

Dynamically invoke a host function by name.

```lisp
(ffi:call-host function-name &rest args) → result
```

**Parameters**:
- `function-name` (string): "module.field" naming the host function
- `args`: Arguments to pass (marshalled based on runtime type)

**Returns**: Unmarshalled return value from host

**Signals**:
- `ffi:ffi-host-error`: If function not found or host throws
- `ffi:ffi-type-error`: If argument marshalling fails

**Example**:
```lisp
(ffi:call-host "host.random")        ; → 0.42
(ffi:call-host "host.add" 1 2)       ; → 3
(ffi:call-host "unknown.fn")         ; signals ffi-host-error
```

---

## Conditions

### ffi:ffi-host-error

Signaled when a host function call fails.

**Slots**:
- `function-name` (string): Name of the failed function
- `message` (string): Error message from host

**Example Handler**:
```lisp
(handler-case
    (ffi:call-host "host.dangerous")
  (ffi:ffi-host-error (e)
    (format t "FFI failed: ~A" (ffi:ffi-host-error-message e))))
```

---

### ffi:ffi-type-error

Signaled when marshalling fails.

**Slots**:
- `expected-type` (keyword): Expected marshal type
- `actual-value`: Value that couldn't be marshalled

**Example Handler**:
```lisp
(handler-case
    (my-ffi-fn 12345678901234567890) ; bignum, not fixnum
  (ffi:ffi-type-error (e)
    (format t "Type error: expected ~A" (ffi:ffi-type-error-expected-type e))))
```

---

## Wasm Import/Export Interface

### Required Host Imports (for dynamic call-host)

```wat
;; Module: "ffi"
;; Function: "call_host_dynamic"
;; Signature: (func (param externref externref) (result anyref))
;; Parameters:
;;   - function-name: externref (string)
;;   - args-array: externref (array of anyref)
;; Returns: anyref (result value)

(import "ffi" "call_host_dynamic"
  (func $call_host_dynamic (param externref externref) (result anyref)))
```

### Export Wrapper Pattern

For each exported function, generate wrapper:

```wat
;; Wrapper for: (ffi:export-function my-add :signature ((:fixnum :fixnum) :fixnum))
(func $export-wrapper-my-add (param $p0 i32) (param $p1 i32) (result i32)
  ;; Unmarshal params: i32 → i31ref
  (local.get $p0)
  (ref.i31)
  (local.get $p1)
  (ref.i31)
  ;; Call Lisp function
  (call $my-add)
  ;; Marshal result: i31ref → i32
  (ref.cast i31)
  (i31.get_s))

(export "add" (func $export-wrapper-my-add))
```

---

## Marshal Type Mappings

| Marshal Type | Lisp → Wasm | Wasm → Host | Host → Wasm | Wasm → Lisp |
|--------------|-------------|-------------|-------------|-------------|
| `:fixnum` | push value | `i31.get_s` | `ref.i31` | on stack |
| `:float` | `struct.get $float 0` | f64 | `struct.new $float` | on stack |
| `:string` | `extern.convert_any` | externref | `any.convert_extern` | `ref.cast $string` |
| `:boolean` | `ref.is_null` + `i32.eqz` | i32 | `if`/`select` | anyref |
| `:anyref` | (passthrough) | externref | (passthrough) | (passthrough) |
| `:void` | N/A | N/A | N/A | nil |
