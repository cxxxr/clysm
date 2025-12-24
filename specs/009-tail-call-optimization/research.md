# Research: Tail Call Optimization

**Date**: 2025-12-24
**Feature**: 009-tail-call-optimization

## WebAssembly Tail Call Instructions

### Decision: Use native Wasm tail call instructions
**Rationale**: WebAssembly 3.0 includes native tail call support through the tail-call proposal. Using native instructions provides better performance than trampoline-based approaches and is required by the project constitution (Principle IV).

**Alternatives considered**:
- Trampoline pattern: Rejected - adds overhead, more complex implementation
- CPS transformation: Rejected - changes semantics, complex to implement correctly

### Instruction Opcodes

| Instruction | Opcode | Description |
|------------|--------|-------------|
| `return_call` | 0x12 | Tail call to known function |
| `return_call_indirect` | 0x13 | Tail call through table (not needed for clysm) |
| `return_call_ref` | 0x15 | Tail call through typed function reference |

### Instruction Semantics

Tail calls behave like `return` followed by `call`:
1. Pop arguments from operand stack
2. Discard current call frame (unwind stack)
3. Jump to target function with arguments
4. Callee's return becomes caller's return

### Typing Rules

For `return_call x` where function x has type `[t1*] -> [t2*]`:
- Instruction type: `[t3* t1*] -> [t4*]` (stack-polymorphic)
- Constraint: Current function must return `[t2*]`
- Note: Caller and callee parameter types do NOT need to match

For `return_call_ref $t`:
- Takes type index `$t` as u32 immediate
- Reference on stack must be of type `(ref $t)`
- Same polymorphic typing as `return_call`

### Runtime Support

**wasmtime**: Supports tail calls with `--wasm-features tail-call` or by default in recent versions.

**V8**: Shipped in V8 v11.2 (Chrome 111+).

## Tail Position Analysis

### Decision: Add tail-position flag to compilation environment
**Rationale**: Propagating a boolean flag through compilation is simpler than AST rewriting and matches how other Lisp compilers (SBCL, CCL) implement TCO.

**Alternatives considered**:
- AST transformation pass: Rejected - more complex, requires AST mutation
- Post-processing of instruction list: Rejected - loses context needed to determine tail position

### Tail Position Rules for Common Lisp

A call is in tail position if its result is immediately returned without further computation.

| Form | Tail Position |
|------|---------------|
| `(defun f (...) body*)` | Last form of body |
| `(lambda (...) body*)` | Last form of body |
| `(progn e1 ... en)` | `en` if progn is in tail position |
| `(if test then else)` | `then` and `else` if if is in tail position |
| `(let ((v e)*) body*)` | Last form of body if let is in tail position |
| `(let* ((v e)*) body*)` | Last form of body if let* is in tail position |
| `(block name body*)` | Last form of body |
| `(return-from name value)` | `value` expression |
| `(flet ((f (p*) body)*) body*)` | Last form of outer body |
| `(labels ((f (p*) body)*) body*)` | Last form of outer body AND last forms of each local function body |

### Non-Tail Positions

Calls are NOT in tail position in:
- Arguments to other calls: `(f (g x))` - `g` is not tail
- Bindings: `(let ((x (f y))) ...)` - `f` is not tail
- Non-final forms: `(progn (f x) y)` - `f` is not tail
- `catch` protected forms: `(catch 'tag (f x))` - `f` is not tail (stack needed for unwinding)
- `unwind-protect` cleanup: `(unwind-protect body cleanup*)` - cleanup forms are not tail

## Implementation Strategy

### Decision: Pass tail-context through compilation environment
**Rationale**: The compilation environment (`cenv`) already carries context through compilation. Adding a `tail-position-p` slot is minimal and non-invasive.

### Changes Required

1. **func-section.lisp**: Add `tail-position-p` slot to cenv, modify compile functions
2. **compiler.lisp**: Add `return_call` and `return_call_ref` to emit-instruction

### Propagation Pattern

```lisp
;; When entering function body: tail-position = true
;; When compiling progn: tail-position passes to last form only
;; When compiling if: tail-position passes to both branches
;; When compiling let body: tail-position passes to last form only
;; When compiling call arguments: tail-position = false
```

## Existing Code Analysis

### Current State

- `tail-call.lisp` exists with stub `analyze-tail-positions` function (not needed for inline approach)
- `func-section.lisp` defines opcodes: `:return_call` (0x12), `:return_call_ref` (0x15)
- `tco-test.lisp` has 10 integration tests ready
- Tests currently pass (without TCO) because they don't exceed stack limits

### Integration Points

The existing `compile-to-instructions` dispatch already handles all relevant forms:
- `ast-call` -> `compile-call` -> `compile-regular-call` / `compile-funcall` / `compile-local-function-call`
- `ast-if` -> `compile-if`
- `ast-progn` -> `compile-progn`
- `ast-let` -> `compile-let`

## References

- [WebAssembly tail-call proposal](https://github.com/WebAssembly/tail-call/blob/main/proposals/tail-call/Overview.md)
- [Typed Function References proposal](https://github.com/WebAssembly/function-references/blob/master/proposals/function-references/Overview.md)
- [V8 WebAssembly tail calls blog post](https://v8.dev/blog/wasm-tail-call)
- [WebAssembly 3.0 Specification](https://webassembly.github.io/spec/core/binary/instructions.html)
