# Contract: Print Primitives Compilation

**Feature**: 001-io-print-primitives
**Date**: 2025-12-31

## Overview

This contract defines the expected behavior when compiling print primitive function calls to WasmGC.

## Contract: compile-print

**Input**: `(print object &optional stream)`

**Expected Wasm Output Structure**:

```wat
;; Pseudocode for compiled (print x) where x is local $0

;; 1. Get stream fd (default: *standard-output* = 1)
(i32.const 1)                    ;; fd = 1 (stdout)

;; 2. Write newline
(i32.const 10)                   ;; newline character
(call $import:write-char)        ;; FFI call

;; 3. Write object (prin1 semantics)
(local.get $0)                   ;; object
(call $prin1-to-string)          ;; convert to string with escape
(call $import:write-string)      ;; FFI call

;; 4. Write space
(i32.const 1)                    ;; fd
(i32.const 32)                   ;; space character
(call $import:write-char)        ;; FFI call

;; 5. Return object
(local.get $0)
```

**Validation Criteria**:
- Generated module must include `clysm:io.write-char` import
- Generated module must include `clysm:io.write-string` import
- Function must return the object (anyref)

---

## Contract: compile-prin1

**Input**: `(prin1 object &optional stream)`

**Expected Wasm Output Structure**:

```wat
;; 1. Get stream fd
(i32.const 1)                    ;; fd = 1 (stdout)

;; 2. Write object with escape
(local.get $0)                   ;; object
(call $prin1-to-string)          ;; convert with escape=t
(call $import:write-string)      ;; FFI call

;; 3. Return object
(local.get $0)
```

---

## Contract: compile-princ

**Input**: `(princ object &optional stream)`

**Expected Wasm Output Structure**:

```wat
;; 1. Get stream fd
(i32.const 1)                    ;; fd = 1 (stdout)

;; 2. Write object without escape
(local.get $0)                   ;; object
(call $princ-to-string)          ;; convert with escape=nil
(call $import:write-string)      ;; FFI call

;; 3. Return object
(local.get $0)
```

---

## Contract: compile-terpri

**Input**: `(terpri &optional stream)`

**Expected Wasm Output Structure**:

```wat
;; 1. Get stream fd
(i32.const 1)                    ;; fd = 1 (stdout)

;; 2. Write newline
(i32.const 10)                   ;; newline character
(call $import:write-char)        ;; FFI call

;; 3. Return NIL
(global.get $nil)
```

---

## Contract: compile-format-nil

**Input**: `(format nil "~A ~D" x y)` where format string is constant

**Expected Wasm Output Structure**:

```wat
;; 1. Create string builder (or use with-output-to-string pattern)
(call $make-string-output-stream)
(local.set $stream)

;; 2. For each directive:
;;    ~A with x:
(local.get $stream)
(local.get $x)
(call $princ-to-string)
(call $string-builder-append)

;;    literal " ":
(local.get $stream)
(i32.const 32)                   ;; space
(call $string-builder-append-char)

;;    ~D with y:
(local.get $stream)
(local.get $y)
(call $write-to-string-decimal)
(call $string-builder-append)

;; 3. Get result string
(local.get $stream)
(call $get-output-stream-string)
```

**Validation Criteria**:
- Result must be a `(ref $string)` type
- No FFI I/O calls when destination is nil (string output)

---

## Contract: compile-format-t

**Input**: `(format t "Hello~%")`

**Expected Wasm Output Structure**:

```wat
;; 1. Write "Hello" to stdout
(i32.const 1)                    ;; fd = stdout
(string.const "Hello")           ;; or equivalent string creation
(call $import:write-string)

;; 2. Write newline
(i32.const 1)                    ;; fd
(i32.const 10)                   ;; newline
(call $import:write-char)

;; 3. Return NIL (format t returns nil)
(global.get $nil)
```

---

## Import Requirements

All modules containing print primitives MUST import these FFI functions:

```wat
(import "clysm:io" "write-char" (func $import:write-char (param i32 i32)))
(import "clysm:io" "write-string" (func $import:write-string (param i32 externref)))
```

## Wasm Validation

All generated modules MUST pass:

```bash
wasm-tools validate output.wasm
```

Exit code MUST be 0.
