# Data Model: Catch/Throw Dynamic Exception Handling

**Feature**: 005-catch-throw
**Date**: 2025-12-23

## Core Entities

### 1. Exception Tag (Wasm-level)

**Purpose**: Single Wasm exception tag for all Common Lisp throws

```wat
;; Module-level declaration
(tag $lisp-throw (param anyref anyref))
;; Parameters: (catch-tag-symbol value)
```

**Design decisions:**
- Single tag for all Lisp throws (runtime tag matching via `eq`)
- Two anyref parameters: the catch tag symbol and the thrown value
- Defined once per module in type section

### 2. Catch Context (Compiler Environment)

**Purpose**: Track active catch points during compilation for cross-function throw support

```lisp
;; Existing field in compilation-env
(catch-tags nil :type list)  ; ((local-idx . block-depth) ...)
```

**Fields:**
- `local-idx`: Local variable holding the evaluated catch tag
- `block-depth`: Nesting depth for calculating br targets

**Usage:**
- Push entry when entering `catch` body
- Pop when exiting `catch`
- Used by nested throws to find matching catch (compile-time optimization for same-function throws)

### 3. Catch Frame (Runtime)

**Purpose**: Conceptual entity representing an active catch point at runtime

**Wasm representation:**
```wat
;; Generated structure for each catch form
(block $catch-exit (result anyref)
  (block $handler (result exnref)
    (try_table (catch $lisp-throw $handler)
      ;; tag-local = evaluate-tag-expr
      ;; body-code
      (br $catch-exit)
    )
  )
  ;; Exception handling code
  ;; $exn = exnref on stack
  ;; Extract (thrown-tag, value) from exception
  ;; if (eq thrown-tag tag-local) -> return value
  ;; else -> throw_ref $exn
)
```

**Components:**
- Tag local: Stores evaluated catch tag for comparison
- Handler block: Receives exnref when exception is caught
- Exit block: For normal completion bypassing handler

### 4. Throw Value (Runtime)

**Purpose**: The pair of (tag, value) thrown during non-local exit

**Representation:**
- Created by `throw` instruction
- Carried as exception payload
- Extracted in catch handler for tag comparison

## Data Flow

```
throw 'foo 42
    ↓
Creates exception: ($lisp-throw 'foo-symbol 42-value)
    ↓
Unwinds stack until try_table catches it
    ↓
Handler receives exnref
    ↓
Extracts (thrown-tag, value)
    ↓
Compares thrown-tag with catch-tag using eq
    ↓
Match: return value    |    No match: throw_ref exn
```

## Wasm Type Requirements

| Entity | Wasm Type | Notes |
|--------|-----------|-------|
| Catch tag symbol | anyref | Symbol representation |
| Thrown value | anyref | Any Lisp value |
| Exception reference | exnref | Handle for caught exception |
| Exception tag | (tag $lisp-throw (param anyref anyref)) | Module-level |

## Integration Points

### With unwind-protect

```wat
;; unwind-protect during throw
(block $exit (result anyref)
  (block $handler (result exnref)
    (try_table (catch_all $handler)
      ;; protected form
      (local.set $result)
      (br $exit)
    )
  )
  ;; cleanup forms here
  ;; then rethrow
  (throw_ref)  ; rethrow caught exception
)
(local.get $result)
```

### With block/return-from

- `return-from` uses existing block/br mechanism (local, lexical)
- `throw` uses exception handling (dynamic, cross-function)
- Both can coexist; `throw` takes precedence for dynamic exits
