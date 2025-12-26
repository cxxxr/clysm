# Data Model: Wasm Import Optimization

**Feature**: 022-wasm-import-optimization
**Date**: 2025-12-25
**Status**: Draft

## Overview

This feature introduces minimal data model changes - primarily a new analysis result that gates import emission.

---

## Existing Entities (Reference)

### compiled-module

**Location**: `src/clysm/compiler/compiler.lisp:40-46`

```lisp
(defstruct (compiled-module (:conc-name compiled-module-))
  (types nil :type list)
  (functions nil :type list)     ; List of function plists
  (globals nil :type list)
  (exports nil :type list)
  (main-func-idx nil :type (or null fixnum)))
```

**Function plist format**:
```lisp
(:name $main
 :params ((arg1 :anyref) ...)
 :result :i32
 :locals ((local1 :anyref) ...)
 :body (instruction...))
```

### ffi-environment

**Location**: `src/clysm/ffi/types.lisp`

```lisp
(defstruct ffi-environment
  (imports (make-hash-table :test 'equal))  ; name → foreign-function-decl
  (exports '())                              ; list of export-decl
  (next-import-func-index 0)
  (type-cache (make-hash-table :test 'equal)))
```

---

## New Entities

### io-usage-result

**Purpose**: Result of I/O usage analysis for a compiled module

**Type**: Simple boolean (T or NIL)

**Usage**:
```lisp
(let ((uses-io (analyze-io-usage module)))
  (emit-module module :uses-io uses-io))
```

No struct needed - the analysis returns a boolean that is passed through the compilation pipeline.

---

## Modified Function Signatures

### compile-to-wasm (modified)

**Before**:
```lisp
(defun compile-to-wasm (expr &key output) ...)
```

**After** (internal change only - external API unchanged):
```lisp
(defun compile-to-wasm (expr &key output)
  (let* ((module (compile-to-module expr))
         (uses-io (analyze-io-usage module))  ; NEW
         (bytes (emit-module module :uses-io uses-io)))  ; NEW
    ...))
```

### emit-module (modified)

**Before**:
```lisp
(defun emit-module (module) ...)
```

**After**:
```lisp
(defun emit-module (module &key (uses-io t)) ...)
```

**Parameter**:
- `uses-io`: Boolean indicating whether the module uses I/O functions
  - Default `t` for backward compatibility
  - When `nil`, import section is not emitted

### emit-import-section-if-needed (modified)

**Before**:
```lisp
(defun emit-import-section-if-needed (buffer) ...)
```

**After**:
```lisp
(defun emit-import-section-if-needed (buffer &key (uses-io t)) ...)
```

---

## Constants

### *io-function-names*

**Location**: `src/clysm/compiler/analyzer/io-usage.lisp`

**Type**: List of strings

**Value**:
```lisp
(defparameter *io-function-names*
  '("WRITE-CHAR" "WRITE-STRING" "WRITE-BYTE" "WRITE-LINE"
    "TERPRI" "FRESH-LINE"
    "PRINT" "PRIN1" "PRINC" "PPRINT" "FORMAT" "WRITE"
    "READ-CHAR" "READ-LINE" "READ-BYTE" "PEEK-CHAR" "READ")
  "List of I/O function names that require FFI imports.")
```

---

## State Transitions

This feature does not introduce new state machines. The compilation pipeline remains linear:

```
Expression → AST → compiled-module → (uses-io analysis) → Wasm bytes
```

The only change is the insertion of the `uses-io` analysis step that produces a boolean used to gate import emission.

---

## Validation Rules

1. **uses-io must be boolean**: Analysis always returns T or NIL
2. **Backward compatibility**: Default `uses-io = t` ensures existing code works unchanged
3. **Conservative analysis**: If uncertain, return T (emit imports)
