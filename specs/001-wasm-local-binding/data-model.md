# Data Model: Wasm Local Instruction Binding

**Feature**: 001-wasm-local-binding
**Date**: 2026-01-01

## Overview

This document describes the key data structures and registries involved in the Wasm local instruction binding feature.

## Entities

### 1. Wasm Instruction Opcode Table

**Location**: `src/clysm/compiler/codegen/func-section.lisp:361-400`

**Structure**: Alist mapping keyword symbols to opcode bytes

```lisp
;; Relevant entries for this feature
(:local.get . #x20)   ; Get local variable
(:local.set . #x21)   ; Set local variable (pop value from stack)
(:local.tee . #x22)   ; Set local and keep value on stack
```

**Relationships**:
- Used by instruction emitter in `compiler.lisp:969-974`
- Referenced during code generation in `func-section.lisp`

### 2. Runtime Function Table (*runtime-function-table*)

**Location**: `src/clysm/compiler/codegen/func-section.lisp:69`

**Structure**: Hash table mapping symbols to (runtime-name . arity) pairs

```lisp
(defparameter *runtime-function-table* (make-hash-table :test 'eq))

;; Entry structure
symbol -> (runtime-keyword . arity-or-nil)

;; Example entries
'clysm:princ -> (:$princ-rt . 1)
'clysm:format -> (:$format-rt . nil)  ; variadic
```

**Operations**:
- `register-runtime-function (symbol runtime-name &optional arity)` - Add entry
- `runtime-function-p (symbol)` - Check if function is registered
- `compile-runtime-call (function args env)` - Compile call to runtime

**New Entries** (this feature):
```lisp
'clysm:advance-token -> (:$advance-token-rt . 1)
'clysm:emit-module-header -> (:$emit-module-header-rt . 0)
```

### 3. AST-TAGBODY Structure

**Location**: `src/clysm/compiler/ast.lisp:360`

**Structure**: DEFSTRUCT inheriting from AST-NODE

```lisp
(defstruct (ast-tagbody (:include ast-node) (:conc-name ast-tagbody-))
  "Tagbody node for goto-based control flow."
  (tags nil :type list)      ; List of tag symbols in order
  (segments nil :type list)) ; Alist of (tag . forms)
```

**Fields**:
| Field | Type | Description |
|-------|------|-------------|
| tags | list | Ordered list of tag symbols |
| segments | alist | (tag . forms) mapping each tag to its body forms |
| source-location | (inherited) | Source location for debugging |

**Relationships**:
- Created by `parse-tagbody-form` in `ast.lisp:1541`
- Compiled by `compile-tagbody` in `func-section.lisp:9611`
- References [tagbody](resources/HyperSpec/Body/s_tagbod.htm) ANSI CL spec

### 4. Parser State

**Location**: `src/clysm/reader/parser.lisp`

**Structure**: Used by ADVANCE-TOKEN function

```lisp
;; Parser state contains:
;; - tokens: list of tokens from lexer
;; - position: current index in token list

(defun current-token (state)
  (nth (parser-state-position state) (parser-state-tokens state)))

(defun advance-token (state)
  "Consume current token and return it."
  ...)
```

**Arity**: 1 (takes parser-state, returns token)

### 5. Module Header

**Location**: `src/clysm/backend/wasm-emit.lisp:10`

**Structure**: 8-byte vector

```lisp
(defconstant +wasm-magic+ #(#x00 #x61 #x73 #x6D))  ; "\0asm"
(defconstant +wasm-version+ 1)

(defun emit-module-header ()
  "Emit the Wasm module header: magic number and version.
   Returns an 8-byte vector."
  ...)
```

**Return Value**: Vector of 8 bytes (magic + version as u32 little-endian)

**Arity**: 0 (no arguments)

## State Transitions

### Compilation Error Resolution Flow

```
Stage 1 Generation
      │
      ├─► LOCAL.SET/LOCAL.TEE in code
      │         │
      │         ▼ (before fix)
      │   Error: "Unbound variable: LOCAL.SET"
      │         │
      │         ▼ (after fix)
      │   Proper keyword handling → Wasm opcode emitted
      │
      ├─► ADVANCE-TOKEN call
      │         │
      │         ▼ (before fix)
      │   Error: "Undefined function: ADVANCE-TOKEN"
      │         │
      │         ▼ (after fix)
      │   Runtime table lookup → :call $advance-token-rt
      │
      └─► AST-TAGBODY literal
                │
                ▼ (before fix)
          Error: "#S(AST-TAGBODY ...)" in output
                │
                ▼ (after fix)
          Structure handled → Control flow compiled
```

## Validation Rules

### Opcode Emission

- LOCAL.SET (0x21) must be followed by LEB128-encoded local index
- LOCAL.TEE (0x22) must be followed by LEB128-encoded local index
- Generated Wasm must pass `wasm-tools validate`

### Function Registration

- Runtime function symbols must be exported from clysm package
- Arity must match actual function parameter count
- Runtime name must be unique keyword (convention: `:$name-rt`)

### AST-TAGBODY

- Tags must be unique within a single tagbody
- Segments must cover all code between tags
- GO targets must reference valid tags in lexical scope
