# Research: Tagbody/Go Control Flow

**Feature Branch**: `004-tagbody-go`
**Date**: 2025-12-23

## Overview

This document consolidates research findings for implementing `tagbody`/`go` control flow in the Clysm Common Lisp to WebAssembly compiler.

---

## Decision 1: Wasm Control Flow Strategy for Arbitrary Jumps

**Question**: How to implement arbitrary forward and backward jumps within `tagbody` given Wasm's structured control flow constraints?

**Decision**: Use a three-strategy approach based on jump pattern analysis:
1. **Sequential**: No `go` statements → simple sequential execution
2. **Simple Loop**: Single tag with backward jumps only → Wasm `loop` instruction
3. **Dispatch Loop**: Complex patterns → `block`/`loop`/`br_table` dispatch mechanism

**Rationale**:
- Wasm `block` only supports forward jumps (to block end)
- Wasm `loop` only supports backward jumps (to loop start)
- `tagbody`/`go` requires arbitrary jumps between tags
- The three-strategy approach optimizes for common patterns while handling all cases

**Alternatives Considered**:

| Alternative | Why Rejected |
|-------------|--------------|
| Always use dispatch loop | Adds overhead for simple cases; `loop`/`do` macros would be slower |
| Trampolining via function calls | Stack overhead; violates tail-call optimization requirements |
| CPS transformation | Excessive code transformation complexity; harder to debug |
| Single `block` nesting | Cannot express backward jumps |

---

## Decision 2: br_table Index Mapping

**Question**: How to map tag indices to `br_table` branch targets?

**Decision**: Use nested `block` structure where `br_table` branch depth directly maps to segment index via formula: `depth = num_segments - segment_index`

**Rationale**:
- `br_table` branches to nested blocks from innermost to outermost
- Branch depth 0 exits innermost block, depth N exits N-th enclosing block
- By nesting blocks in reverse segment order, segment index maps naturally to branch depth

**Example**:
```
segments = [seg_0, seg_1, seg_2]  (3 segments)

Nesting:
  (block $seg_2    ;; br 0 exits here → runs seg_2
    (block $seg_1  ;; br 1 exits here → runs seg_1
      (block $seg_0  ;; br 2 exits here → runs seg_0
        (br_table 2 1 0 $exit ...)  ;; index maps to depth
      )
      ;; seg_0 code runs after br 2
    )
    ;; seg_1 code runs after br 1
  )
  ;; seg_2 code runs after br 0
```

---

## Decision 3: Program Counter Local Variable

**Question**: How to track current segment in dispatch loop?

**Decision**: Use an `i32` local variable `$pc` to store the current segment index.

**Rationale**:
- `br_table` requires a runtime value on the stack
- Local variable provides O(1) read/write access
- Each `go` sets `$pc` to target segment index before jumping to dispatch loop

**Implementation**:
```wat
(local $pc i32)
(local.set $pc (i32.const 0))  ;; start at segment 0

(loop $dispatch
  ...
  (br_table ... (local.get $pc))
  ...
)
```

---

## Decision 4: Dead Code Handling After `go`

**Question**: How to handle code after `go` that can never execute?

**Decision**: Stop code generation after encountering `go` within a segment.

**Rationale**:
- Code after `go` is semantically unreachable
- Generating it wastes code space
- Wasm validators may warn about unreachable code
- Not generating provides cleaner output

**Alternative Considered**:
- Generate code with `unreachable` marker → Rejected: unnecessary complexity, wastes space

---

## Decision 5: Nested Tagbody Scope Isolation

**Question**: How to handle nested `tagbody` forms with potentially overlapping tag names?

**Decision**: Each `tagbody` creates an independent scope. Inner `go` can only target inner tags; outer tags are invisible.

**Rationale**:
- Common Lisp specification requires this behavior
- Implemented via `tagbody-context` in compilation environment
- Each `tagbody` pushes a new context; `go` only searches current context
- Attempting to `go` to an outer tag results in compile-time error (undefined tag)

---

## Decision 6: Strategy Detection Algorithm

**Question**: How to efficiently determine which compilation strategy to use?

**Decision**: Analyze segments in a single pass to collect go targets and determine jump directions.

**Algorithm**:
```lisp
(defun analyze-tagbody-strategy (segments)
  (let* ((tags (remove nil (mapcar #'car segments)))
         (go-targets (collect-go-targets segments)))
    (cond
      ((null go-targets) :sequential)
      ((and (= (length tags) 1)
            (= (length go-targets) 1)
            (eq (first tags) (first go-targets))
            (all-goes-are-backward-p segments (first tags)))
       :simple-loop)
      (t :dispatch))))
```

**Rationale**:
- O(n) analysis where n is number of forms
- Covers all cases with minimal checks
- Simple loop detection optimizes the most common iteration pattern

---

## Decision 7: tagbody Return Value

**Question**: What value should `tagbody` return?

**Decision**: Always return NIL (as `ref.null none` or reference to NIL singleton).

**Rationale**:
- Common Lisp specification: "tagbody always returns nil"
- Consistent with current NIL representation strategy (singleton struct, not Wasm null)
- Generated code appends `(ref.null none)` after control flow completes

---

## Best Practices Applied

### From Wasm GC Specification
- Use structured control flow (block, loop, if)
- `br_table` for computed jumps
- Local variables for state tracking

### From Constitution
- TDD: Write tests before implementing each strategy
- WasmGC-First: Use standard Wasm instructions, no linear memory hacks
- Nix-First: Tests run via `nix flake check`

---

## Open Questions (None)

All technical questions have been resolved. Implementation can proceed.
