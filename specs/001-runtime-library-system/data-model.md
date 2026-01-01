# Data Model: Runtime Library System

**Feature**: 001-runtime-library-system
**Date**: 2026-01-01
**Status**: Draft

## Entities

### Primitive

Core operation that compiles directly to Wasm instructions.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| name | symbol | Primitive function name | Unique in registry |
| wasm-emitter | function | `(env args) -> wasm-instructions` | Required |
| signature | plist | `(:params (:type*) :result :type)` | Required |
| inline-p | boolean | Always inline at call sites | Default: nil |
| category | keyword | `:memory`, `:arithmetic`, `:predicate`, `:ffi` | Required |

**Lifecycle**:
1. Registered at compiler load time via `register-primitive`
2. Looked up during compilation via `get-primitive`
3. Immutable after registration (no runtime modification)

### Runtime-Function

Lisp function compiled using Clysm, stored in runtime library.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| name | symbol | Function name | Unique in runtime module |
| lambda-list | list | Parameter specification | Valid CL lambda list |
| body | list | Function body forms | References only primitives or other runtime functions |
| source-file | pathname | Origin file | One of: io.lisp, list-ops.lisp, sequences.lisp, strings.lisp |
| compiled-index | fixnum | Wasm function index | Assigned during compilation |
| dependencies | list | Functions called by this function | Auto-computed |

**Lifecycle**:
1. Defined in runtime library source file
2. Loaded and parsed by runtime loader
3. Dependency analysis performed
4. Compiled to Wasm during runtime compilation phase
5. Index assigned in function section

### Primitive-Registry

Mapping from primitive names to their implementations.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| table | hash-table | name → primitive | :test #'eq |
| categories | hash-table | category → (name*) | For documentation/introspection |

**Operations**:
- `register-primitive (name emitter &key signature inline-p category)`
- `get-primitive (name) -> primitive or nil`
- `primitive-p (name) -> boolean`
- `list-primitives (&optional category) -> (name*)`

### Runtime-Module

Collection of runtime functions organized by category.

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| name | keyword | Module name | `:io`, `:list-ops`, `:sequences`, `:strings` |
| source-file | pathname | Source file path | Must exist |
| functions | list | Runtime-function objects | Ordered by dependency |
| exports | list | Exported function names | Subset of functions |

**Lifecycle**:
1. Source file loaded at compile time
2. Forms extracted and classified
3. Functions compiled with dependency ordering
4. Module merged into final Wasm output

## Relationships

```text
Primitive-Registry 1--* Primitive
                         |
                         | (referenced by)
                         v
Runtime-Module 1--* Runtime-Function
                         |
                         | (dependencies)
                         v
                   Runtime-Function (or Primitive)
```

## State Transitions

### Compilation Pipeline

```text
[Source Files] --load--> [Parsed Forms]
                              |
                              v
                    [Dependency Analysis]
                              |
                              v
                    [Topological Sort]
                              |
                              v
                    [Wasm Compilation]
                              |
                              v
                    [Index Assignment]
                              |
                              v
                    [Module Merge]
```

### Function Resolution

```text
compile-call(name, args)
       |
       v
   primitive-p(name)?
       |
   +---+---+
   | YES   | NO
   v       v
emit-primitive  runtime-function-p(name)?
               |
           +---+---+
           | YES   | NO
           v       v
      emit-call    undefined-function-error
      (by index)
```

## Validation Rules

### Primitive Registration
- Name must be a symbol
- Emitter must be a function of (env args)
- Signature must have :params and :result
- Category must be one of: `:memory`, `:arithmetic`, `:predicate`, `:ffi`

### Runtime Function Validation
- Body must not reference unregistered primitives
- Body must not reference undefined runtime functions
- No circular dependency chains without forward declarations
- Lambda list must be compilable (no unsupported keywords)

### Module Validation
- All source files must exist
- All exported functions must be defined
- Dependency order must be valid (topological sort possible)
