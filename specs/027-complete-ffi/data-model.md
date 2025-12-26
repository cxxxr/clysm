# Data Model: Complete FFI Foundation

**Feature**: 027-complete-ffi
**Date**: 2025-12-27

## Entities

### ForeignFunctionDecl (Existing - types.lisp)

Declaration for an imported host function.

| Field | Type | Description |
|-------|------|-------------|
| lisp-name | symbol | Lisp-side function name |
| module-name | string | Wasm import module name (e.g., "host") |
| field-name | string | Wasm import field name (e.g., "log") |
| param-types | list | List of marshal types (`:fixnum`, `:float`, etc.) |
| return-type | keyword | Marshal type for return value or `:void` |
| type-index | fixnum | Assigned Wasm type index (set during compilation) |

**Lifecycle**:
1. Created by `define-foreign-function` macro
2. Registered in `*ffi-environment*` at load time
3. Type index assigned during `assign-import-indices`
4. Used during codegen to emit import calls

### ExportDecl (Existing - types.lisp)

Declaration for an exported Lisp function.

| Field | Type | Description |
|-------|------|-------------|
| lisp-name | symbol | Lisp function to export |
| export-name | string | Wasm export name |
| param-types | list | List of marshal types for parameters |
| return-type | keyword | Marshal type for return value |
| wrapper-func-index | fixnum | Assigned wrapper function index |

**Lifecycle**:
1. Created by `export-function` macro
2. Registered in `*ffi-environment*` at load time
3. Wrapper index assigned during `assign-export-indices`
4. Wrapper function generated during codegen

### FFI Environment (Existing - types.lisp)

Compile-time registry for all FFI declarations.

| Field | Type | Description |
|-------|------|-------------|
| imports | hash-table | lisp-name → ForeignFunctionDecl |
| exports | list | List of ExportDecl |
| next-import-func-index | fixnum | Counter for import function indices |
| type-cache | hash-table | Signature → type index (deduplication) |

**Global Instance**: `*ffi-environment*`

### Marshal Type (Existing - types.lisp)

Type specifier for FFI marshalling.

| Value | Lisp Type | Wasm Type | Host Type |
|-------|-----------|-----------|-----------|
| `:fixnum` | integer (-2^30 to 2^30-1) | i31ref → i32 | number |
| `:float` | double-float | (ref $float) → f64 | number |
| `:string` | string | (ref $string) → externref | string |
| `:boolean` | t/nil | anyref → i32 (1/0) | boolean |
| `:anyref` | any | anyref → anyref | any |
| `:void` | N/A | N/A | undefined |

### AST-FFI-Call (New - ast.lisp)

AST node for FFI function calls.

| Field | Type | Description |
|-------|------|-------------|
| declaration | ForeignFunctionDecl | Reference to FFI declaration |
| arguments | list | List of argument AST nodes |
| source-location | source-location | For error reporting |

**Relationship**: Created during parsing when call target is FFI-declared function.

### AST-Call-Host (New - ast.lisp)

AST node for dynamic host function calls.

| Field | Type | Description |
|-------|------|-------------|
| function-name | ast-node | AST for function name expression (string) |
| arguments | list | List of argument AST nodes |
| source-location | source-location | For error reporting |

**Relationship**: Created when parsing `(ffi:call-host ...)` forms.

## Relationships

```
┌─────────────────────────────────────────────────────┐
│                  *ffi-environment*                  │
│                  (FFI Environment)                  │
├─────────────────────────────────────────────────────┤
│  imports: hash-table ─────┐                         │
│  exports: list ───────────┼──┐                      │
│  type-cache: hash-table   │  │                      │
└───────────────────────────┼──┼──────────────────────┘
                            │  │
          ┌─────────────────┘  │
          ▼                    ▼
┌──────────────────┐  ┌──────────────────┐
│ ForeignFunction  │  │    ExportDecl    │
│      Decl        │  │                  │
├──────────────────┤  ├──────────────────┤
│ lisp-name ───────┼──│ lisp-name        │
│ module-name      │  │ export-name      │
│ field-name       │  │ param-types ─────┼──┐
│ param-types ─────┼──│ return-type ─────┼──┤
│ return-type ─────┼──│ wrapper-func-idx │  │
│ type-index       │  └──────────────────┘  │
└──────────────────┘                        │
          │                                 │
          ▼                                 ▼
┌──────────────────────────────────────────────┐
│               Marshal Type                    │
│  (:fixnum :float :string :boolean :anyref)   │
└──────────────────────────────────────────────┘
```

## State Transitions

### FFI Declaration Lifecycle

```
┌─────────────┐  define-foreign-function  ┌──────────────┐
│  Undefined  │ ────────────────────────► │  Registered  │
└─────────────┘                           └──────────────┘
                                                  │
                                                  │ compile-to-wasm
                                                  ▼
                                          ┌──────────────┐
                                          │Index Assigned│
                                          └──────────────┘
                                                  │
                                                  │ emit-import-section
                                                  ▼
                                          ┌──────────────┐
                                          │   Emitted    │
                                          └──────────────┘
```

### FFI Call Compilation Flow

```
┌───────────────┐  parse-expr    ┌──────────────┐
│  S-expression │ ─────────────► │  AST Node    │
│ (my-ffi-fn x) │                │ ast-ffi-call │
└───────────────┘                └──────────────┘
                                        │
                                        │ compile-ffi-call
                                        ▼
                                 ┌──────────────┐
                                 │ Wasm Instrs  │
                                 │ marshal-args │
                                 │ call $import │
                                 │ unmarshal    │
                                 └──────────────┘
```

## Validation Rules

### ForeignFunctionDecl Validation

- `lisp-name`: Must be a symbol
- `module-name`: Must be non-empty string
- `field-name`: Must be non-empty string
- `param-types`: Each must be valid marshal-type
- `return-type`: Must be valid marshal-type

### ExportDecl Validation

- `lisp-name`: Must be a symbol (and function must exist)
- `export-name`: Must be non-empty string, unique across exports
- `param-types`: Each must be valid marshal-type
- `return-type`: Must be valid marshal-type

### Runtime Marshalling Validation

- Fixnum: Value must be in i31ref range (-2^30 to 2^30-1)
- String: Must be non-nil or explicitly handled
- Boolean: Any Lisp value (nil=false, non-nil=true)
