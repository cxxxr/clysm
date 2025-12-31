# Contract: Selective Import Section

**Module**: `clysm/ffi/import-gen`
**Date**: 2025-12-31

## Functions

### emit-selected-ffi-imports

```lisp
(defun emit-selected-ffi-imports (buffer used-ffis &key include-dynamic-call) ...)
```

**Purpose**: Emit Wasm import section containing only the specified FFI functions.

**Input**:
- `buffer`: Byte vector for output
- `used-ffis`: List of FFI function names (symbols) to include
- `include-dynamic-call`: Boolean, whether to add `$dynamic-call` import

**Output**: Modified buffer with import section bytes

**Contract**:

| Condition | Guarantee |
|-----------|-----------|
| `used-ffis` is empty, no dynamic-call | No import section emitted |
| `used-ffis` contains N functions | Exactly N imports in section |
| `include-dynamic-call` is t | Additional `$dynamic-call` import |
| Function not in `*ffi-environment*` | Skipped silently (graceful handling) |

### Import Section Format

```text
Section ID: 0x02
Content:
  count: LEB128 (number of imports)
  For each import:
    module: length-prefixed UTF-8 string
    name: length-prefixed UTF-8 string
    kind: 0x00 (function)
    type-idx: LEB128 (function type index)
```

## Test Cases

```lisp
;; T001: No FFI, no imports
(let ((buffer (make-adjustable-byte-vector)))
  (emit-selected-ffi-imports buffer nil :include-dynamic-call nil)
  (assert (zerop (length buffer))))

;; T002: Single FFI import
(let ((buffer (make-adjustable-byte-vector)))
  (emit-selected-ffi-imports buffer '(sin) :include-dynamic-call nil)
  (assert (= (aref buffer 0) #x02))  ; Import section ID
  (assert (import-count buffer) 1))

;; T003: Multiple FFI imports
(let ((buffer (make-adjustable-byte-vector)))
  (emit-selected-ffi-imports buffer '(sin cos) :include-dynamic-call nil)
  (assert (= (import-count buffer) 2)))

;; T004: Dynamic call import added
(let ((buffer (make-adjustable-byte-vector)))
  (emit-selected-ffi-imports buffer '(sin) :include-dynamic-call t)
  (assert (= (import-count buffer) 2))
  (assert (has-import-named buffer "dynamic-call")))

;; T005: Unknown FFI function skipped
(let ((buffer (make-adjustable-byte-vector)))
  (emit-selected-ffi-imports buffer '(nonexistent-ffi) :include-dynamic-call nil)
  (assert (zerop (import-count buffer))))
```

## Wasm Validation

Generated import section must pass `wasm-tools validate`:

```bash
wasm-tools validate output.wasm
# Exit code 0 for valid import section structure
```

Validation checks:
- Section ID is 0x02
- LEB128 encoding is correct
- String lengths match content
- Type indices reference valid types
