# Data Model: Cross-Compile Stage 0

**Phase 1 Output** | **Date**: 2025-12-27

## Entities

### E1: Module Order

The 41 source modules in compilation dependency order.

**Source**: `src/clysm/validation/compiler-order.lisp:*compilation-order*`

| Group | Modules | Count |
|-------|---------|-------|
| Backend | leb128, sections, wasm-emit, wat-print | 4 |
| Reader | tokenizer, parser, package, reader | 4 |
| Compiler Core | ast, env, analyzer/*, transform/*, codegen/*, compiler | 10 |
| Runtime | objects, special-vars, multi-value, printer, condition-runtime | 5 |
| CLOS | mop, defclass, instance, slot-access, generic, defmethod, combination, dispatch, method-combination | 9 |
| Conditions | package, types, handlers, restarts, signaling, standard | 6 |
| **Total** | | **38** |

**Note**: The spec mentions 41 modules but `*compilation-order*` lists 38. Additional modules may be discovered during implementation.

### E2: Bootstrap Build Context

State maintained during the bootstrap compilation process.

```text
bootstrap-context
├── source-files: list of pathnames         # 41 files in order
├── all-forms: list of S-expressions        # Collected compilable forms
├── compilation-start: timestamp            # For timing
├── current-module: pathname                # Progress tracking
├── error-info: (module, message, feature)  # If compilation fails
└── output-path: pathname                   # dist/clysm-stage0.wasm
```

### E3: Stage 0 Binary Structure

The compiled Wasm module structure.

```text
clysm-stage0.wasm (WasmGC binary)
├── Type Section (ID 1)
│   ├── Types 0-22: GC types (nil, unbound, cons, symbol, string, closure, etc.)
│   └── Types 23+: Function types for all defuns
├── Import Section (ID 2) - if I/O used
│   └── host.* imports for FFI
├── Function Section (ID 3)
│   ├── Index 0: $main (entry point)
│   ├── Indices 1-N: Compiler functions (parse, compile, emit, etc.)
│   └── Indices N+: Lambda closures
├── Tag Section (ID 13)
│   └── $lisp-throw for exceptions
├── Global Section (ID 6)
│   ├── 0: $nil (singleton)
│   ├── 1: $unbound (sentinel)
│   ├── 2: $mv-count (i32)
│   ├── 3: $mv-buffer (array ref)
│   └── 4+: Special variable symbols
├── Export Section (ID 7)
│   ├── "_start" -> func 0
│   └── "compile" -> func N (TBD)
├── Element Section (ID 9)
│   └── Declarative segment for lambda refs
└── Code Section (ID 10)
    └── Function bodies
```

### E4: Verification Test Cases

Test expressions and expected results.

| Test ID | Input Expression | Expected Result | Validates |
|---------|------------------|-----------------|-----------|
| V001 | `(+ 1 2)` | 3 (i32) | Basic arithmetic |
| V002 | `(defun f (x) (* x 2)) (f 21)` | 42 (i32) | Function definition |
| V003 | `(if (> 10 5) 'greater 'less)` | GREATER (symbol) | Control flow |

### E5: Host Shim Interface

JavaScript interface for wasmtime verification.

```typescript
interface HostShim {
  // Read UTF-8 string from externref
  read_string(ref: externref): string;

  // Write Wasm bytes from externref
  write_bytes(ref: externref): void;

  // Get last written bytes (for verification)
  get_output(): Uint8Array;
}
```

## Relationships

```text
Module Order ─────> Bootstrap Context
    │                    │
    │ reads              │ produces
    ▼                    ▼
Source Files        Stage 0 Binary
                         │
                         │ exports
                         ▼
                    Host Shim
                         │
                         │ verifies
                         ▼
                    Test Cases
```

## State Transitions

### Bootstrap Process

```text
[Init] ──load clysm──> [Ready] ──read modules──> [Collected]
                                                      │
[Done] <──write file── [Compiled] <──compile-to-wasm──┘
   │
   └──> [Validated] (wasm-tools) ──> [Verified] (wasmtime tests)
```

### Error States

```text
[Collected] ──syntax error──> [Parse Error: module, line]
[Collected] ──unsupported feature──> [Compile Error: module, feature]
[Compiled] ──invalid wasm──> [Validation Error: wasm-tools message]
[Validated] ──wrong result──> [Verification Error: test, expected, actual]
```

## Validation Rules

1. **Module Order**: All 41 modules must exist and be readable
2. **Compilable Forms**: Only defun, defmacro, defvar, defparameter, defconstant, defstruct, defclass, progn, let, etc.
3. **No Unsupported Features**: All CL symbols must be in blessed subset (Feature 036)
4. **Valid Wasm**: `wasm-tools validate` must exit with code 0
5. **Correct Results**: Verification tests must produce expected values
