# Internal Contract: compile-make-instance*

**Type**: Compiler Function
**Module**: clysm/compiler/codegen/func-section

## Signature

```lisp
(defun compile-make-instance* (args env) ...)
```

## Parameters

| Name | Type | Description |
|------|------|-------------|
| args | list | Arguments to make-instance*: (class-name &rest initargs) |
| env | cenv | Compilation environment |

## Returns

| Type | Description |
|------|-------------|
| list | List of Wasm instructions creating an instance |

## Behavior

1. **Extract class name** from first argument (quoted symbol)
2. **Validate class** exists in compile-time registry
3. **Compile initarg pairs** (keyword + value expressions)
4. **Emit Wasm instructions**:
   - Look up or create class reference
   - Create slot vector of appropriate size
   - Initialize slots from initargs or nil
   - Create $instance struct

## Generated Wasm Pattern

```wat
;; Simplified output for (make-instance* 'point :x 1 :y 2)
(global.get $class_point)           ;; Get class reference
(i32.const 2)                        ;; Slot count
(array.new $slot-vector)             ;; Create slot array
;; ... slot initialization ...
(struct.new $instance)               ;; Create instance
```

## Error Conditions

| Condition | Error Message |
|-----------|---------------|
| Unknown class | "Cannot make instance of undefined class: ~S" |
| Invalid class name | "Class name must be a symbol: ~S" |

## Dependencies

- `find-compile-time-class`: Class lookup
- `compile-to-instructions`: Argument compilation
- `+type-instance+`: Instance type index
- `+type-slot-vector+`: Slot vector type index
