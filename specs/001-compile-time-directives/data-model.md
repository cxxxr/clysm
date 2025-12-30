# Data Model: Compile-Time Directive Processing

**Date**: 2025-12-30
**Feature**: 001-compile-time-directives

## Overview

This feature operates on compile-time state rather than persistent data. The "data model" describes the conceptual entities and their relationships during compilation.

## Entities

### Directive Form

A toplevel S-expression that affects the compilation environment without producing runtime code.

| Attribute | Type | Description |
|-----------|------|-------------|
| operator | symbol | One of: `in-package`, `defpackage`, `declaim`, `proclaim` |
| arguments | list | Form-specific arguments per ANSI CL |
| source-location | (file line column) | For error reporting |

**Lifecycle**:
1. Read from source file
2. Detected by `directive-form-p` predicate
3. Evaluated in host environment
4. Discarded (no AST generated)

### Compile-Time Environment

The mutable state during file compilation.

| Attribute | Type | Description | Modified By |
|-----------|------|-------------|-------------|
| current-package | package | Active package for symbol interning | [in-package](resources/HyperSpec/Body/m_in_pkg.htm) |
| declarations | plist | Optimization settings, type declarations | [declaim](resources/HyperSpec/Body/m_declai.htm), [proclaim](resources/HyperSpec/Body/f_procla.htm) |
| defined-packages | list | Packages created during compilation | [defpackage](resources/HyperSpec/Body/m_defpkg.htm) |

**Notes**:
- `current-package` is bound via `*package*` special variable
- `declarations` stored in host's global proclamation database
- `defined-packages` tracked by host's package system

### Toplevel Form (Existing)

Any S-expression at the top level of a source file.

| Variant | Detection | Action |
|---------|-----------|--------|
| Directive Form | `directive-form-p` returns T | Eval and skip AST |
| Definition Form | operator is `defun`, `defvar`, etc. | Parse and compile |
| Expression Form | All others | Parse and compile |

## Relationships

```
┌─────────────────┐
│  Source File    │
└────────┬────────┘
         │ contains
         ▼
┌─────────────────┐     ┌───────────────────────┐
│  Toplevel Form  │────▶│ Compile-Time Env      │
└────────┬────────┘     │  - *package*          │
         │              │  - proclamations      │
         │              │  - package registry   │
    ┌────┴────┐         └───────────────────────┘
    │         │                    ▲
    ▼         ▼                    │ modifies
┌────────┐  ┌────────────┐         │
│Directive│  │ Definition │         │
│  Form  │───────────────┼─────────┘
└────────┘  └────────────┘
    │              │
    ▼              ▼
  (nil)     ┌──────────┐
            │ AST Node │
            └────┬─────┘
                 │
                 ▼
            ┌──────────┐
            │Wasm Code │
            └──────────┘
```

## State Transitions

### Package Context (`*package*`)

```
Initial: CL-USER
         │
         ▼ (in-package :clysm)
       CLYSM
         │
         ▼ (in-package :clysm/compiler)
    CLYSM/COMPILER
         │
        ...
```

### Compilation Processing

```
Form Read ──▶ directive-form-p? ──┬──▶ [Yes] ──▶ eval ──▶ nil (skip)
                                  │
                                  └──▶ [No] ──▶ parse-expr ──▶ compile ──▶ Wasm
```

## Validation Rules

### In-Package

- Package MUST exist (signals `package-error` otherwise)
- Argument MUST be string designator (symbol or string)

### Defpackage

- Package name MUST be unique or redefine existing
- `:use` packages MUST exist
- `:import-from` symbols MUST be external in source package
- `:shadow` and `:shadowing-import-from` resolve conflicts

### Declaim / Proclaim

- Declaration identifiers MUST be valid (`optimize`, `type`, `special`, `ftype`, `inline`, `notinline`, etc.)
- Type specifiers MUST be valid type designators
- Optimization qualities: `speed`, `safety`, `debug`, `space`, `compilation-speed` (values 0-3)

## No Persistent Storage

This feature does not create or modify persistent data. All state changes:
- Affect the host Lisp environment during compilation
- Are automatically saved if host state is serialized (FASL)
- Do not affect the generated Wasm module directly
