# Data Model: Type Constant and Package Primitive Export

**Feature Branch**: `001-type-package-export`
**Date**: 2026-01-01

## Entities

### 1. Constant Binding

Represents a compile-time constant defined via [defconstant](resources/HyperSpec/Body/m_defcon.htm).

| Field | Type | Description |
|-------|------|-------------|
| name | symbol | Constant symbol (e.g., +TYPE-CONS+) |
| value | integer | Evaluated constant value (e.g., 2) |
| doc | string | Optional documentation string |

**Lifecycle**:
1. Created: When DEFCONSTANT form is processed in directive.lisp
2. Read: When symbol is referenced during compilation
3. Never deleted (persists for compilation session)

**Storage**: In-memory hash-table `*constant-bindings*`

---

### 2. Type Index Constant

Represents a WasmGC type section index used for runtime type dispatch.

| Constant Name | Value | Description |
|---------------|-------|-------------|
| +TYPE-NIL+ | 0 | NIL singleton struct |
| +TYPE-UNBOUND+ | 1 | UNBOUND sentinel |
| +TYPE-CONS+ | 2 | Cons cell struct |
| +TYPE-SYMBOL+ | 3 | Symbol struct |
| +TYPE-STRING+ | 4 | String (i8 array) |
| +TYPE-CLOSURE+ | 5 | Closure struct |
| +TYPE-INSTANCE+ | 6 | CLOS instance |
| +TYPE-STANDARD-CLASS+ | 7 | CLOS class metadata |
| +TYPE-FUNC-0+ | 8 | 0-arg function type |
| +TYPE-FUNC-1+ | 9 | 1-arg function type |
| +TYPE-FUNC-2+ | 10 | 2-arg function type |
| +TYPE-FUNC-3+ | 11 | 3-arg function type |
| +TYPE-FUNC-N+ | 12 | N-arg function type |
| +TYPE-BINDING-FRAME+ | 13 | Dynamic binding frame |
| +TYPE-BIGNUM+ | 14 | Arbitrary-precision integer |
| +TYPE-RATIO+ | 15 | Exact rational |
| +TYPE-FLOAT+ | 16 | IEEE 754 double |
| +TYPE-COMPLEX+ | 17 | Complex number |
| +TYPE-LIMB-ARRAY+ | 18 | Bignum limb array |
| +TYPE-STREAM+ | 19 | I/O stream |
| +TYPE-MV-ARRAY+ | 20 | Multiple values buffer |
| +TYPE-SLOT-VECTOR+ | 21 | CLOS slot storage |
| +TYPE-KEYWORD-ARRAY+ | 22 | CLOS initarg keywords |
| +TYPE-CLOSURE-ARRAY+ | 23 | CLOS initform closures |
| +TYPE-MACRO-ENVIRONMENT+ | 24 | Macro expansion env |
| +TYPE-HASH-ENTRY+ | 25 | Hash table entry |
| +TYPE-HASH-TABLE+ | 26 | Hash table |
| +TYPE-BUCKET-ARRAY+ | 27 | Hash bucket array |
| +TYPE-MDARRAY+ | 28 | Multidimensional array |

**Source Files**:
- `src/clysm/compiler/codegen/gc-types.lisp`: Definitions
- `src/clysm/stage0/types.lisp`: Stage 0 copy (identical values)

---

### 3. Runtime Function Entry

Represents a function dispatched to the runtime library.

| Field | Type | Description |
|-------|------|-------------|
| symbol | symbol | Lisp function symbol |
| runtime-name | keyword | Wasm function name (e.g., :$packagep*-rt) |
| arity | integer or nil | Expected arg count (nil = variadic) |

**Package Primitive Entries**:

| Symbol | Runtime Name | Arity | HyperSpec Reference |
|--------|--------------|-------|---------------------|
| PACKAGEP* | :$packagep*-rt | 1 | [f_pkgp.htm](resources/HyperSpec/Body/f_pkgp.htm) |
| FIND-PACKAGE* | :$find-package*-rt | 1 | [f_find_p.htm](resources/HyperSpec/Body/f_find_p.htm) |
| INTERN* | :$intern*-rt | nil | [f_intern.htm](resources/HyperSpec/Body/f_intern.htm) |
| SYMBOL-PACKAGE* | :$symbol-package*-rt | 1 | [f_symb_2.htm](resources/HyperSpec/Body/f_symb_2.htm) |

**Storage**: In-memory hash-table `*runtime-function-table*` in func-section.lisp

---

### 4. Compilation Environment (Extended)

Extension to existing compilation environment to support constant bindings.

| Field | Type | Description |
|-------|------|-------------|
| constants | hash-table | Symbol → value mapping |
| ...existing fields... | ... | (preserved from env.lisp) |

**Relationship**: Compilation environment references `*constant-bindings*` for constant lookup during symbol resolution.

---

## State Transitions

### Constant Binding Lifecycle

```
┌─────────────────┐
│ DEFCONSTANT     │
│ form parsed     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Evaluate        │
│ value-form      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Store in        │
│ *constant-      │
│ bindings*       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Available for   │
│ constant        │
│ folding         │
└─────────────────┘
```

### Symbol Reference Resolution (Extended)

```
┌─────────────────┐
│ Symbol          │
│ reference       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐     Yes    ┌─────────────────┐
│ In *constant-   │───────────▶│ Emit i32.const  │
│ bindings*?      │            │ with value      │
└────────┬────────┘            └─────────────────┘
         │ No
         ▼
┌─────────────────┐     Yes    ┌─────────────────┐
│ Local variable? │───────────▶│ Emit local.get  │
└────────┬────────┘            └─────────────────┘
         │ No
         ▼
┌─────────────────┐     Yes    ┌─────────────────┐
│ Global/special? │───────────▶│ Emit global.get │
└────────┬────────┘            └─────────────────┘
         │ No
         ▼
┌─────────────────┐
│ Compile error:  │
│ unbound symbol  │
└─────────────────┘
```

---

## Validation Rules

1. **Constant uniqueness**: Each constant symbol may only be bound once per compilation session (warn on redefinition per ANSI CL)
2. **Value type**: Type constants MUST be non-negative integers (fixnums)
3. **Package primitive arity**: PACKAGEP* and SYMBOL-PACKAGE* MUST receive exactly 1 argument
4. **INTERN* arity**: MUST receive 1 or 2 arguments (symbol-name, optional package)

---

## Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                    Compilation Session                       │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────────┐        ┌──────────────────────────┐   │
│  │ *constant-       │        │ *runtime-function-table* │   │
│  │ bindings*        │        │                          │   │
│  │                  │        │ PACKAGEP* → :$packagep*  │   │
│  │ +TYPE-CONS+ → 2  │        │ FIND-PACKAGE* → ...      │   │
│  │ +TYPE-SYMBOL+→3  │        │ INTERN* → ...            │   │
│  │ ...              │        │ SYMBOL-PACKAGE* → ...    │   │
│  └────────┬─────────┘        └────────────┬─────────────┘   │
│           │                               │                  │
│           │ lookup                        │ dispatch         │
│           ▼                               ▼                  │
│  ┌──────────────────────────────────────────────────────┐   │
│  │              compile-expression                       │   │
│  │                                                       │   │
│  │  symbol-ref → check constants → i32.const            │   │
│  │  func-call  → check runtime-table → call $func       │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```
