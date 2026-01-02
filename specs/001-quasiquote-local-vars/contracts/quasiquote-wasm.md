# Wasm Contract: Quasiquote Local Variable Compilation

**Feature**: 001-quasiquote-local-vars
**Date**: 2026-01-01

## Contract Overview

This document defines the Wasm output contracts for quasiquote compilation. Tests verify these contracts by inspecting generated Wasm binary/text output.

---

## Contract 1: Simple Unquote Variable Reference

**Input**:
```lisp
(defun test-simple-unquote (x)
  `(result ,x))
```

**Required Wasm Output Patterns**:

1. **Local.get instruction present** for parameter `x`:
   ```wat
   (local.get $x)  ;; or (local.get 0) if first param
   ```

2. **Symbol reference for 'result'**:
   ```wat
   (i32.const <hash>)  ;; Symbol hash for RESULT
   (ref.i31)
   ```

3. **Cons cell construction** (type index 0):
   ```wat
   (struct.new 0)  ;; $cons type
   ```

**Validation**:
```bash
wasm-tools validate output.wasm && echo "PASS"
wasm-tools print output.wasm | grep -c "local.get" # >= 1
wasm-tools print output.wasm | grep -c "struct.new 0" # >= 2 (for 2-element list)
```

---

## Contract 2: Multiple Unquoted Variables

**Input**:
```lisp
(defun test-multi-unquote (a b c)
  `(,a ,b ,c))
```

**Required Wasm Output Patterns**:

1. **Three local.get instructions** for a, b, c:
   ```wat
   (local.get 0)  ;; a
   (local.get 1)  ;; b
   (local.get 2)  ;; c
   ```

2. **Three cons cell constructions**:
   ```wat
   (struct.new 0)  ;; (c . nil)
   (struct.new 0)  ;; (b . (c . nil))
   (struct.new 0)  ;; (a . (b . (c . nil)))
   ```

**Validation**:
```bash
wasm-tools print output.wasm | grep -c "local.get" # >= 3
wasm-tools print output.wasm | grep -c "struct.new 0" # >= 3
```

---

## Contract 3: Unquote-Splicing

**Input**:
```lisp
(defun test-splice (items)
  `(prefix ,@items suffix))
```

**Required Wasm Output Patterns**:

1. **Local.get for items parameter**:
   ```wat
   (local.get 0)  ;; items
   ```

2. **Call to append function**:
   ```wat
   (call $append)  ;; or call_indirect if via table
   ```

3. **List construction for static parts**:
   ```wat
   ;; (list 'prefix) and (list 'suffix) parts
   (struct.new 0)
   ```

**Validation**:
```bash
wasm-tools print output.wasm | grep -E "(call|call_indirect)" # append call present
```

---

## Contract 4: Mixed Quoted and Unquoted

**Input**:
```lisp
(defun test-mixed (cond then else)
  `(if ,cond ,then ,else))
```

**Required Wasm Output Patterns**:

1. **Symbol reference for 'if'** (NOT a local.get):
   ```wat
   (i32.const <if-hash>)
   (ref.i31)
   ```

2. **Three local.get instructions** for cond, then, else:
   ```wat
   (local.get 0)  ;; cond
   (local.get 1)  ;; then
   (local.get 2)  ;; else
   ```

3. **Four cons cells** for 4-element list:
   ```wat
   (struct.new 0)  ;; 4 times
   ```

**Validation**:
```bash
wasm-tools print output.wasm | grep -c "local.get" # >= 3
wasm-tools print output.wasm | grep -c "struct.new 0" # >= 4
wasm-tools print output.wasm | grep -c "i32.const" # >= 1 (for 'if symbol)
```

---

## Contract 5: Nested Let with Quasiquote

**Input**:
```lisp
(defun test-nested-let ()
  (let ((x 1))
    (let ((y 2))
      `(,x ,y))))
```

**Required Wasm Output Patterns**:

1. **Two local.get instructions** at correct indices:
   ```wat
   (local.get <x-index>)
   (local.get <y-index>)
   ```

2. **Local indices differ** (x and y are separate locals)

**Validation**:
```bash
# Extract local.get indices and verify they differ
wasm-tools print output.wasm | grep "local.get" | sort -u | wc -l # >= 2
```

---

## Contract 6: Error Conditions

### 6a. Unquote Outside Quasiquote

**Input**:
```lisp
(defun test-error ()
  ,x)  ;; Error: unquote outside quasiquote
```

**Expected**: Compile-time error, no Wasm output

**Validation**:
```lisp
(handler-case
    (clysm:compile-to-wasm '(defun test-error () ,x))
  (error (c)
    (assert (search "outside" (princ-to-string c)))))
```

### 6b. Undefined Variable in Unquote

**Input**:
```lisp
(defun test-unbound ()
  `(,undefined-var))
```

**Expected**: Compile-time error "Unbound variable: UNDEFINED-VAR"

**Validation**:
```lisp
(handler-case
    (clysm:compile-to-wasm '(defun test-unbound () `(,undefined-var)))
  (error (c)
    (assert (search "Unbound variable" (princ-to-string c)))))
```

---

## Runtime Correctness Contract

**Test Pattern**: Compile, instantiate, call, compare to SBCL reference

```lisp
;; Reference: SBCL evaluation
(let ((x 42))
  `(value ,x))
;; => (VALUE 42)

;; Compiled Wasm must produce equivalent structure
;; Verify via FFI call and result inspection
```

**Validation Strategy**:
1. Compile test function to Wasm
2. Instantiate with Node.js runtime
3. Call function with test values
4. Compare list structure to expected
