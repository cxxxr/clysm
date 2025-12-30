# Data Model: Character Literal Compilation Support

**Branch**: `001-char-literal-compile` | **Date**: 2025-12-31

## Overview

This feature extends compile-time handling only. No new runtime data structures are introduced.

## Compile-Time Entities

### Character Literal (Input)

A Common Lisp character object recognized by `characterp`.

| Property | Type | Description |
|----------|------|-------------|
| char-code | integer [0, 1114111] | Unicode code point |

**Source**: Lisp reader produces character objects from syntax like `#\Space`, `#\a`, `#\Tab`

### i31ref Encoding (Output)

WebAssembly GC reference type for 31-bit signed integers.

| Property | Type | Description |
|----------|------|-------------|
| value | i32 | Character code as 32-bit integer |
| encoding | ref.i31 | WasmGC reference wrapper |

**Wasm Instructions**:
```wat
i32.const <char-code>  ;; Push character code as i32
ref.i31                ;; Convert to i31ref
```

## Type Mapping

| Common Lisp | Wasm Representation | Notes |
|-------------|---------------------|-------|
| character | i31ref | Same as fixnum encoding |
| `#\Space` | i31ref(32) | ASCII/Unicode 32 |
| `#\Tab` | i31ref(9) | ASCII/Unicode 9 |
| `#\Newline` | i31ref(10) | ASCII/Unicode 10 |
| `#\Return` | i31ref(13) | ASCII/Unicode 13 |

## Runtime Behavior

Character values at runtime are indistinguishable from fixnums in their Wasm representation. Character-specific operations (like [char=](resources/HyperSpec/Body/f_chareq.htm)) will need type tags or context to differentiate, but that is outside the scope of this feature.

## No New Entities

This feature:
- Does NOT add new Wasm types
- Does NOT modify existing type indices
- Does NOT require schema migrations
- Does NOT introduce persistent state
