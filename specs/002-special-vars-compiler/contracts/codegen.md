# Code Generation Contracts

**Feature**: 002-special-vars-compiler

## Contract: compile-defvar

**Input**: `ast-defvar` node

**Output**: Wasm instructions

### Scenarios

#### Without Init-Form: `(defvar *x*)`
```wat
;; No code generated - symbol already has UNBOUND in $value
;; Only side effect: symbol registered as special
(nop)
```

#### With Init-Form: `(defvar *x* 10)`
```wat
;; Check if unbound, only then initialize
(global.get $symbol-*x*)
(struct.get $symbol $value)
(global.get $UNBOUND)
(ref.eq)
(if
  (then
    (global.get $symbol-*x*)
    (i32.const 10)
    (ref.i31)
    (struct.set $symbol $value)))
```

---

## Contract: compile-defparameter

**Input**: `ast-defparameter` node

**Output**: Wasm instructions

### Scenarios

#### `(defparameter *x* 10)`
```wat
;; Always set the value
(global.get $symbol-*x*)
<compile init-form>
(struct.set $symbol $value)
```

---

## Contract: compile-var-ref (special variable)

**Input**: `ast-var-ref` where `name` is special

**Output**: Wasm instructions

### Scenarios

#### `*x*` (special variable reference)
```wat
;; Direct value field access (O(1))
(global.get $symbol-*x*)
(struct.get $symbol $value)
;; Optional UNBOUND check
```

---

## Contract: compile-setq (special variable)

**Input**: `ast-setq` where `name` is special

**Output**: Wasm instructions

### Scenarios

#### `(setq *x* 10)` (special variable assignment)
```wat
(global.get $symbol-*x*)
<compile value>
(struct.set $symbol $value)
;; Return the value (tee pattern)
```

---

## Contract: compile-let (with special bindings)

**Input**: `ast-let` where some bindings are special variables

**Output**: Wasm instructions with dynamic binding protocol

### Scenarios

#### `(let ((*x* 20)) body)` - Single special binding
```wat
;; 1. Save old value to binding stack
(global.get $symbol-*x*)           ;; symbol ref for frame
(global.get $symbol-*x*)
(struct.get $symbol $value)        ;; old value
(global.get $binding_stack)        ;; prev frame
(struct.new $binding_frame)        ;; create frame
(global.set $binding_stack)        ;; push frame

;; 2. Set new value
(global.get $symbol-*x*)
<compile new-value-20>
(struct.set $symbol $value)

;; 3. Execute body with exception handling
(block $normal (result anyref)
  (try_table (result anyref) (catch_all $cleanup)
    <compile body>
    (br $normal))

  ;; $cleanup: restore and rethrow
  (call $restore-binding)
  (throw_ref))

;; 4. Normal exit: restore binding
(call $restore-binding)
```

#### Mixed lexical and special: `(let ((y 1) (*x* 2)) body)`
```wat
;; Lexical binding: standard local.set
<compile 1>
(local.set $y-idx)

;; Special binding: dynamic protocol
<special binding code for *x*>

;; Body
<compile body>

;; Restore only special bindings
<restore-binding for *x*>
```

---

## Contract: restore-binding (helper)

**Input**: None (operates on global `$binding_stack`)

**Output**: None (side effect: restores top binding)

### Wasm Pattern
```wat
(func $restore-binding
  ;; Get top frame
  (global.get $binding_stack)
  (local.tee $frame)

  ;; Restore value: frame.old_value -> frame.symbol.$value
  (struct.get $binding_frame $old_value)
  (local.get $frame)
  (struct.get $binding_frame $symbol)
  (struct.set $symbol $value)

  ;; Pop frame: frame.prev -> $binding_stack
  (local.get $frame)
  (struct.get $binding_frame $prev)
  (global.set $binding_stack))
```

---

## Contract: UNBOUND Check

**Input**: Value from symbol's `$value` field

**Output**: Error if unbound, value otherwise

### Wasm Pattern (optional, configurable)
```wat
;; After (struct.get $symbol $value)
(local.tee $temp)
(global.get $UNBOUND)
(ref.eq)
(if
  (then
    ;; Signal unbound-variable error
    (global.get $symbol-*x*)
    (call $signal-unbound-variable)
    (unreachable)))
(local.get $temp)
```

---

## Instruction Count Summary

| Operation | Approx Instructions |
|-----------|---------------------|
| Special var read | 2-8 (with/without UNBOUND check) |
| Special var write | 3 |
| Dynamic bind entry | ~12 |
| Dynamic bind exit | ~8 |
| defvar (unbound) | 0 |
| defvar (init) | ~10 |
| defparameter | ~5 |
