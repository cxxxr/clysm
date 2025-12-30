# Implementation Plan: Phase 15A - ANSI List Operations Extension

**Branch**: `001-ansi-list-ops` | **Date**: 2025-12-30 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-ansi-list-ops/spec.md`

## Summary

Implement 21 ANSI Common Lisp list operation functions for the Clysm compiler. Functions are organized into 5 categories: list tail operations ([last](resources/HyperSpec/Body/f_last.htm), [butlast/nbutlast](resources/HyperSpec/Body/f_butlas.htm), [nth](resources/HyperSpec/Body/f_nth.htm), [nthcdr](resources/HyperSpec/Body/f_nthcdr.htm)), membership search ([member/member-if/member-if-not](resources/HyperSpec/Body/f_mem_m.htm)), association list operations ([assoc/assoc-if](resources/HyperSpec/Body/f_assocc.htm), [rassoc/rassoc-if](resources/HyperSpec/Body/f_rassoc.htm), [pairlis](resources/HyperSpec/Body/f_pairli.htm), [acons](resources/HyperSpec/Body/f_acons.htm), [copy-alist](resources/HyperSpec/Body/f_cp_ali.htm)), set operations ([intersection](resources/HyperSpec/Body/f_intera.htm), [union](resources/HyperSpec/Body/f_unionc.htm), [set-difference](resources/HyperSpec/Body/f_set_di.htm), [subsetp](resources/HyperSpec/Body/f_subset.htm), [adjoin](resources/HyperSpec/Body/f_adjoin.htm)), and the [pushnew](resources/HyperSpec/Body/m_push.htm) macro.

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) for host compiler
**Target Platform**: WasmGC (WebAssembly with GC proposal)
**Primary Dependencies**: alexandria, babel (UTF-8), trivial-gray-streams, existing clysm compiler infrastructure
**Storage**: N/A (in-memory compilation)
**Testing**: rove framework, existing test infrastructure in tests/unit/
**Project Type**: Single (compiler library)
**Performance Goals**: Functions compile to efficient Wasm using cons cell traversal via struct.get
**Constraints**: ANSI CL compliance, WasmGC type system constraints, worklist-based iteration for recursion-heavy operations
**Scale/Scope**: 21 functions, ~500-800 lines of implementation code

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | All list ops use $cons struct (type 2) with struct.get/struct.set |
| II. Lispオブジェクト表現規約 | ✅ PASS | NIL handling via ref.eq, proper sentinel checks |
| III. 関数・クロージャ実装戦略 | ✅ PASS | :test/:key args passed as closure objects |
| IV. Wasm制御フロー活用 | ✅ PASS | Loop-based iteration instead of recursion where needed |
| V. シャローバインディング | N/A | No special variables introduced |
| VI. 段階的動的コンパイル | N/A | Standard compiled functions |
| VII. TDD（非交渉） | ✅ REQUIRED | Test-first for each function group |
| VIII. Nix-Firstワークフロー | ✅ PASS | No new dependencies required |
| IX. ANSI CL仕様参照規約 | ✅ REQUIRED | HyperSpec links included in this plan |

**Gate Status**: PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/001-ansi-list-ops/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── lib/
│   └── list-ops.lisp    # NEW: List operation implementations
├── compiler/
│   ├── codegen/
│   │   └── builtins.lisp  # MODIFY: Add builtin dispatch for new functions
│   └── transform/
│       └── macro.lisp     # MODIFY: Add pushnew macro expansion

tests/
├── unit/
│   └── list-ops/
│       ├── assoc-test.lisp      # EXISTS: Extend with rassoc tests
│       ├── member-test.lisp     # NEW: member/member-if/member-if-not
│       ├── set-ops-test.lisp    # NEW: intersection/union/set-difference
│       ├── list-tail-test.lisp  # NEW: last/butlast/nth/nthcdr
│       └── alist-construct-test.lisp  # NEW: pairlis/acons/copy-alist
└── contract/
    └── list-ops-contract-test.lisp  # NEW: Wasm output structure verification
```

**Structure Decision**: Single project layout. New list operations go in `src/clysm/lib/list-ops.lisp` following existing pattern in `lib/macros.lisp`. Tests organized under `tests/unit/list-ops/` subdirectory.

## Complexity Tracking

No constitution violations requiring justification.

## Implementation Phases

### Phase 1: List Tail Operations (P1)

Functions: [last](resources/HyperSpec/Body/f_last.htm), [butlast](resources/HyperSpec/Body/f_butlas.htm), [nbutlast](resources/HyperSpec/Body/f_butlas.htm), [nth](resources/HyperSpec/Body/f_nth.htm), [nthcdr](resources/HyperSpec/Body/f_nthcdr.htm)

**Implementation Notes**:
- `nth` and `nthcdr`: Already have basic tests in `cons-test.lisp` (T079, T080) - extend implementation
- `last`: Traverse to find last n cons cells, use loop/worklist pattern
- `butlast`: Copy all but last n elements (fresh list)
- `nbutlast`: Destructive - modify CDR of (length - n)th cell to NIL

**Wasm Pattern**:
```wat
;; nthcdr pattern - loop n times applying cdr
(loop $traverse
  (br_if $done (i32.eqz (local.get $n)))
  (local.set $list (struct.get 2 1 (ref.cast (ref $cons) (local.get $list))))
  (local.set $n (i32.sub (local.get $n) (i32.const 1)))
  (br $traverse))
```

### Phase 2: Membership Operations (P1)

Functions: [member](resources/HyperSpec/Body/f_mem_m.htm), [member-if](resources/HyperSpec/Body/f_mem_m.htm), [member-if-not](resources/HyperSpec/Body/f_mem_m.htm)

**Implementation Notes**:
- Support `:test` keyword (default `#'eql`)
- Support `:key` keyword (default `#'identity`)
- Return tail of list starting from match, or NIL
- `member-if`/`member-if-not`: Take predicate instead of item

**Keyword Handling**:
```lisp
;; Compiler transforms keyword args to closure application
(member item list :test #'equal :key #'car)
;; → internal: (member-internal item list <equal-closure> <car-closure>)
```

### Phase 3: Association List Lookup (P1)

Functions: [assoc](resources/HyperSpec/Body/f_assocc.htm), [assoc-if](resources/HyperSpec/Body/f_assocc.htm), [rassoc](resources/HyperSpec/Body/f_rassoc.htm), [rassoc-if](resources/HyperSpec/Body/f_rassoc.htm)

**Implementation Notes**:
- Existing tests in `tests/unit/list-ops/assoc-test.lisp` - extend
- `assoc`: Search by key (CAR of each alist entry)
- `rassoc`: Search by value (CDR of each alist entry)
- Skip non-cons elements per ANSI CL
- Support `:test` and `:key` keywords

**Wasm Pattern**:
```wat
;; assoc pattern - iterate alist, test CAR of each cons
(loop $search
  (br_if $not-found (ref.is_null (local.get $alist)))
  (local.set $entry (struct.get 2 0 (ref.cast (ref $cons) (local.get $alist))))
  ;; Skip if entry is not cons
  (if (ref.test (ref $cons) (local.get $entry))
    (then
      ;; Apply :key to CAR, then :test with item
      ;; Return entry if match
    ))
  (local.set $alist (struct.get 2 1 (ref.cast (ref $cons) (local.get $alist))))
  (br $search))
```

### Phase 4: Association List Construction (P2)

Functions: [pairlis](resources/HyperSpec/Body/f_pairli.htm), [acons](resources/HyperSpec/Body/f_acons.htm), [copy-alist](resources/HyperSpec/Body/f_cp_ali.htm)

**Implementation Notes**:
- `acons`: Simple `(cons (cons key value) alist)`
- `pairlis`: Iterate parallel lists, cons pairs, append to result
- `copy-alist`: Copy top-level spine and each entry cons cell

### Phase 5: Set Operations (P2)

Functions: [intersection](resources/HyperSpec/Body/f_intera.htm), [union](resources/HyperSpec/Body/f_unionc.htm), [set-difference](resources/HyperSpec/Body/f_set_di.htm), [subsetp](resources/HyperSpec/Body/f_subset.htm), [adjoin](resources/HyperSpec/Body/f_adjoin.htm)

**Implementation Notes**:
- All use `member` internally for set membership tests
- Support `:test` and `:key` keywords
- Result order unspecified per ANSI CL
- `adjoin`: `(if (member item list :test test :key key) list (cons item list))`

### Phase 6: PUSHNEW Macro (P3)

Function: [pushnew](resources/HyperSpec/Body/m_push.htm)

**Implementation Notes**:
- Macro expansion using existing SETF infrastructure (feature 028)
- Support `:test` and `:key` keywords
- Expansion pattern:
```lisp
(pushnew item place :test test :key key)
;; → (setf place (adjoin item place :test test :key key))
```

## Dependencies

| Dependency | Status | Notes |
|------------|--------|-------|
| cons/car/cdr primitives | ✅ Exists | Basic cons operations working |
| struct.get/struct.set codegen | ✅ Exists | Wasm struct access |
| Keyword argument handling | ✅ Exists | Used by existing functions |
| SETF infrastructure | ✅ Exists | Feature 028 |
| EQL/EQUAL/EQUALP | ✅ Exists | Feature 024 |
| Closure compilation | ✅ Exists | For :test/:key args |

## Test Strategy

Following TDD (Constitution VII):

1. **Unit Tests** (`tests/unit/list-ops/`):
   - Each function has dedicated test file
   - Test compilation produces valid Wasm
   - Test keyword argument handling
   - Test edge cases (empty list, n > length, etc.)

2. **Contract Tests** (`tests/contract/`):
   - Verify Wasm output structure
   - Verify type indices and field accesses

3. **Integration Tests**:
   - Interpreter execution of each function
   - End-to-end compile → run → verify

## Success Metrics

Per spec SC-001 through SC-007:
- [ ] All 21 functions implemented and operational
- [ ] Empty list handling correct for all functions
- [ ] :test/:key parameters work with custom functions
- [ ] Verification examples pass
- [ ] cons test category 50%+ pass rate
- [ ] Integration with compilation pipeline
- [ ] Functions usable in interpreter
