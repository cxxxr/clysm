# Internal API Contract: compile* Function

**Package**: `clysm/eval/compile`

## Primary Interface

### compile* (name definition) → function

Compile a lambda expression and return a callable function.

**Parameters**:
- `name`: `(or null symbol)` - Function name (NIL for anonymous)
- `definition`: `cons` - Lambda expression `(lambda (params...) body...)`

**Returns**: `function` - A callable function

**Side Effects**:
- If `name` is a symbol, registers in `*function-slots*`

**Errors**:
- `error` if `definition` is not a valid lambda expression

**Examples**:
```lisp
;; Anonymous function
(compile* nil '(lambda (x) (+ x 1)))
=> #<FUNCTION>

;; Named function
(compile* 'my-add '(lambda (a b) (+ a b)))
=> #<FUNCTION>  ; Also registered in *function-slots*
```

---

### compile-with-tier (tier definition) → function

Compile at a specific tier without automatic promotion.

**Parameters**:
- `tier`: `(member :tier-1 :tier-2)` - Target compilation tier
- `definition`: `cons` - Lambda expression

**Returns**: `function` - A callable function

**Errors**:
- `error` if tier is invalid
- `error` if `:tier-2` compilation fails for unsupported construct

---

## Tier Management Interface

### *compilation-threshold* → fixnum

Global variable controlling when tier promotion occurs.

**Default**: 10

---

### record-invocation (symbol) → fixnum

Increment and return the invocation count for a symbol.

**Parameters**:
- `symbol`: `symbol` - Function name

**Returns**: `fixnum` - Updated count

---

### should-promote-to-tier-2-p (symbol) → boolean

Check if a function should be promoted based on invocation count.

**Parameters**:
- `symbol`: `symbol` - Function name

**Returns**: `boolean` - T if count exceeds threshold

---

### promote-to-tier-2 (symbol definition) → function

Promote a function to Tier 2 and hot-patch the function slot.

**Parameters**:
- `symbol`: `symbol` - Function name
- `definition`: `cons` - Original lambda expression

**Returns**: `function` - JIT-compiled function

**Side Effects**:
- Updates `*function-slots*` with new function
- Updates function's implementation slot

---

## JIT Interface (clysm/eval/jit)

### jit-compile (expr) → function

JIT compile an expression to Wasm and return a callable function.

**Parameters**:
- `expr`: `t` - Lisp expression (typically lambda)

**Returns**: `function` - Callable function backed by Wasm

---

### hotpatch-function (symbol new-function) → t

Replace a symbol's function slot with a new function.

**Parameters**:
- `symbol`: `symbol` - Function name
- `new-function`: `function` - New implementation

**Returns**: `t`

---

### register-runtime-import (name function) → t

Register a runtime import for JIT-compiled modules.

**Parameters**:
- `name`: `string` - Import name (Wasm convention)
- `function`: `function` - Host function

---

### get-runtime-import (name) → (or function null)

Retrieve a registered runtime import.

**Parameters**:
- `name`: `string` - Import name

**Returns**: Function or NIL if not found
