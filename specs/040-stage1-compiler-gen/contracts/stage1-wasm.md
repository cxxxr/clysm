# Contract: Stage 1 Wasm Binary Structure

**Date**: 2025-12-30
**Branch**: `040-stage1-compiler-gen`
**Type**: Binary Output Contract

## Overview

This contract defines the structure and validation criteria for the Stage 1 Wasm binary output (`dist/clysm-stage1.wasm`).

## Binary Constraints

### Size Requirements

| Constraint | Value | Rationale |
|------------|-------|-----------|
| Minimum size | 100 KB (102,400 bytes) | Ensures non-trivial compiler content |
| Maximum size | None | No upper limit specified |

### Validation

The binary MUST pass `wasm-tools validate` with exit code 0.

```bash
wasm-tools validate dist/clysm-stage1.wasm
echo $?  # Must be 0
```

## Wasm Module Structure

### Required Sections

Per WebAssembly specification, sections must appear in ascending ID order:

| ID | Section | Required | Purpose |
|----|---------|----------|---------|
| 1 | Type | YES | WasmGC type definitions |
| 2 | Import | NO | External function imports |
| 3 | Function | YES | Function signatures |
| 4 | Table | NO | Indirect call tables |
| 5 | Memory | NO | Linear memory (avoided per Constitution) |
| 6 | Global | YES | NIL, UNBOUND, mv-count, mv-buffer |
| 7 | Export | YES | Public API functions |
| 8 | Start | NO | Module initialization |
| 9 | Element | NO | Table initialization |
| 10 | Code | YES | Function bodies |
| 11 | Data | NO | Memory initialization |

### Required Type Definitions

Per CLAUDE.md type index table:

| Index | Type | Description |
|-------|------|-------------|
| 0 | $cons | Cons cell (car, cdr) |
| 1 | $symbol | Symbol (name, value, function, plist) |
| 2 | $string | UTF-8 string (bytes array) |
| 3 | $closure | Function closure |
| 4 | $float | IEEE 754 double |
| 5 | $ratio | Rational number |
| 6 | $instance | CLOS instance |
| 7 | $standard-class | CLOS class |
| 18 | $hash-table | Hash table |
| 21 | $slot-vector | CLOS slot storage |
| 22 | $mv_array | Multiple values buffer |
| 24 | $macro-environment | Macro expansion environment |

### Required Globals

Per CLAUDE.md global index table:

| Index | Name | Purpose |
|-------|------|---------|
| 0 | NIL | Null/false value |
| 1 | UNBOUND | Unbound marker |
| 2 | mv-count | Multiple value count |
| 3 | mv-buffer | Multiple value storage |

## Expected Exports

The Stage 1 binary SHOULD export compiler entry points:

| Export Name | Description |
|-------------|-------------|
| `compile_form` | Compile a single S-expression |
| `read_form` | Read/parse S-expression |
| `emit_wasm` | Generate Wasm bytes |

Note: Exact export names may vary based on implementation.

## Verification Tests

### Test 1: File Existence

```lisp
(deftest stage1-file-exists ()
  (ok (probe-file "dist/clysm-stage1.wasm")))
```

### Test 2: Minimum Size

```lisp
(deftest stage1-minimum-size ()
  (let ((size (with-open-file (s "dist/clysm-stage1.wasm"
                                  :element-type '(unsigned-byte 8))
                (file-length s))))
    (ok (>= size 102400)
        (format nil "Size ~D bytes >= 100KB" size))))
```

### Test 3: Wasm Validation

```lisp
(deftest stage1-wasm-valid ()
  (multiple-value-bind (output error-output status)
      (uiop:run-program '("wasm-tools" "validate" "dist/clysm-stage1.wasm")
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore output))
    (ok (zerop status)
        (format nil "wasm-tools validate exit code: ~D~%~A" status error-output))))
```

### Test 4: Magic Number

```lisp
(deftest stage1-wasm-magic ()
  (with-open-file (s "dist/clysm-stage1.wasm"
                      :element-type '(unsigned-byte 8))
    (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence magic s)
      (ok (equalp magic #(#x00 #x61 #x73 #x6D))  ; "\0asm"
          "Valid Wasm magic number"))))
```

### Test 5: Wasm Version

```lisp
(deftest stage1-wasm-version ()
  (with-open-file (s "dist/clysm-stage1.wasm"
                      :element-type '(unsigned-byte 8))
    (file-position s 4)  ; Skip magic
    (let ((version (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence version s)
      (ok (equalp version #(#x01 #x00 #x00 #x00))  ; Version 1
          "Valid Wasm version"))))
```

## Error Conditions

### Invalid Module

If `wasm-tools validate` fails, the contract is violated. Common causes:

1. Section order incorrect
2. Type mismatch in function signatures
3. Invalid LEB128 encoding
4. Missing required imports

### Size Too Small

If file size < 100KB, compilation rate is likely too low. Check:

1. Compilation rate in `dist/stage1-report.json`
2. Top blockers from blocker analysis
3. Phase 13D feature integration

## Relationship to Other Artifacts

```
build/stage1-complete.lisp
         │
         │ generates
         ▼
dist/clysm-stage1.wasm  ◀── This Contract
         │
         │ validates via
         ▼
wasm-tools validate
         │
         │ reports to
         ▼
dist/stage1-report.json
```
