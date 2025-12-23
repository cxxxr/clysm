# Implementation Plan: Cons Cell and List Operations

**Branch**: `006-cons-list-ops` | **Date**: 2025-12-23 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/006-cons-list-ops/spec.md`

## Summary

Implement Common Lisp cons cells (the fundamental pair data structure) and basic list operations using the existing WasmGC `$cons` type (Type 2). This feature enables list construction (`cons`, `list`), access (`car`, `cdr`), type predicates (`consp`, `null`, `atom`, `listp`), and destructive modification (`rplaca`, `rplacd`).

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) + alexandria, rove (testing)
**Primary Dependencies**: Existing clysm compiler infrastructure, WasmGC `$cons` type
**Storage**: N/A (WasmGC heap-allocated structs)
**Testing**: rove + integration tests via wasmtime
**Target Platform**: WebAssembly with GC proposal
**Project Type**: Single project
**Performance Goals**: O(1) cons/car/cdr operations, O(n) list construction
**Constraints**: All cons cells allocated via WasmGC (no linear memory)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- [x] **I. WasmGC-First**: Uses existing `$cons` struct type (Type 2) with anyref fields
- [x] **II. Lisp Object Representation**: NIL handling per constitution (singleton struct, not Wasm null)
- [x] **III. Function/Closure**: List operations implemented as primitive calls in compiler
- [x] **IV. Wasm Control Flow**: N/A (no control flow changes)
- [x] **V. Shallow Binding**: N/A (no dynamic scope changes)
- [x] **VI. Tiered Eval**: N/A (compile-time feature)
- [x] **VII. TDD**: Tests written first, must fail before implementation
- [x] **VIII. Nix-First**: All tests runnable via `nix flake check`

## Project Structure

### Documentation (this feature)

```text
specs/006-cons-list-ops/
├── plan.md              # This file
├── research.md          # WasmGC cons type analysis
├── data-model.md        # Entity definitions
├── spec.md              # Feature specification
├── checklists/
│   └── requirements.md  # Validation checklist
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
src/
└── clysm/
    └── compiler/
        ├── codegen/
        │   ├── func-section.lisp  # Main: compile-cons, compile-car, compile-cdr, etc.
        │   └── gc-types.lisp      # Existing: $cons type definition (Type 2)
        └── ast.lisp               # AST definitions (already present)

tests/
├── unit/
│   └── cons-test.lisp             # New: unit tests for cons operations
└── integration/
    └── list-test.lisp             # New: integration tests for list operations
```

**Structure Decision**: Single project extending existing compiler structure with new primitive operations

## Key Implementation Details

### Existing $cons Type (Type 2)

```wat
;; Already defined in gc-types.lisp
(type $cons (struct
  (field car (mut anyref))   ;; field index 0
  (field cdr (mut anyref)))) ;; field index 1
```

### Cons Creation Pattern

```lisp
;; (cons x y) compiles to:
;; 1. Evaluate x -> anyref on stack
;; 2. Evaluate y -> anyref on stack
;; 3. struct.new 2 (creates $cons with car=x, cdr=y)
```

```wat
;; Generated Wasm:
<compile x>       ;; anyref (car value)
<compile y>       ;; anyref (cdr value)
struct.new 2      ;; create $cons (Type 2)
```

### Car/Cdr Access Pattern

```lisp
;; (car x) compiles to:
;; 1. Evaluate x -> anyref on stack
;; 2. Check if null (NIL) -> return NIL
;; 3. Check if cons -> struct.get 2 0
;; 4. Else -> signal error
```

```wat
;; Generated Wasm for (car x):
<compile x>               ;; anyref
local.tee $tmp
ref.eq $nil_singleton     ;; is it NIL?
if (result anyref)
  global.get $nil         ;; return NIL
else
  local.get $tmp
  ref.cast (ref 2)        ;; cast to $cons
  struct.get 2 0          ;; get car field
end
```

### Type Predicate Patterns

```wat
;; consp: ref.test against $cons type
<compile x>
ref.test (ref 2)          ;; is it $cons?
if (result anyref)
  global.get $t           ;; return T
else
  global.get $nil         ;; return NIL
end

;; null: ref.eq against NIL singleton
<compile x>
global.get $nil
ref.eq
if (result anyref)
  global.get $t
else
  global.get $nil
end

;; atom: NOT consp
<compile x>
ref.test (ref 2)
if (result anyref)
  global.get $nil         ;; cons -> not atom
else
  global.get $t           ;; not cons -> atom
end

;; listp: consp OR null
<compile x>
local.tee $tmp
ref.test (ref 2)          ;; consp?
if (result anyref)
  global.get $t
else
  local.get $tmp
  global.get $nil
  ref.eq                  ;; null?
  if (result anyref)
    global.get $t
  else
    global.get $nil
  end
end
```

### Destructive Modification Pattern

```wat
;; (rplaca cons new-value)
<compile cons>
ref.cast (ref 2)          ;; cast to $cons
<compile new-value>
struct.set 2 0            ;; set car field
<compile cons>            ;; return the cons (per CL spec)

;; (rplacd cons new-value)
<compile cons>
ref.cast (ref 2)
<compile new-value>
struct.set 2 1            ;; set cdr field
<compile cons>
```

### List Function Pattern

```lisp
;; (list a b c) compiles to:
;; (cons a (cons b (cons c nil)))
;; Built right-to-left for efficiency
```

```wat
;; Generated for (list 1 2 3):
global.get $nil           ;; start with NIL
i32.const 3
ref.i31                   ;; push 3
<swap>
struct.new 2              ;; (cons 3 nil)
i32.const 2
ref.i31
<swap>
struct.new 2              ;; (cons 2 (cons 3 nil))
i32.const 1
ref.i31
<swap>
struct.new 2              ;; (cons 1 (cons 2 (cons 3 nil)))
```

## Implementation Phases

### Phase 1: Core Cons Operations (User Story 1) - MVP

**Goal**: Basic cons cell creation and access

1. **T001**: Write unit test `test-cons-creation` verifying cons compiles to `struct.new 2`
2. **T002**: Write unit test `test-car-access` verifying car compiles to `struct.get 2 0`
3. **T003**: Write unit test `test-cdr-access` verifying cdr compiles to `struct.get 2 1`
4. **T004**: Implement `compile-cons` generating `struct.new 2` with evaluated car/cdr
5. **T005**: Add `cons` to `*primitives*` list in compiler
6. **T006**: Implement `compile-car` with NIL check and `struct.get 2 0`
7. **T007**: Implement `compile-cdr` with NIL check and `struct.get 2 1`
8. **T008**: Add `car` and `cdr` to `*primitives*` list
9. **T009**: Write integration test `test-cons-car-cdr-roundtrip`

**Checkpoint**: `(car (cons 1 2))` returns 1

### Phase 2: NIL Handling (User Story 2)

**Goal**: car/cdr of NIL returns NIL per Common Lisp semantics

10. **T010**: Write integration test `test-car-nil-returns-nil`
11. **T011**: Write integration test `test-cdr-nil-returns-nil`
12. **T012**: Verify NIL singleton comparison works in car/cdr implementations
13. **T013**: Write test for nested NIL access `(cdr (cdr (cons 1 nil)))`

**Checkpoint**: `(car nil)` returns NIL (not error)

### Phase 3: List Function (User Story 3)

**Goal**: Variadic list construction

14. **T014**: Write unit test `test-list-empty` verifying `(list)` returns NIL
15. **T015**: Write unit test `test-list-single` verifying `(list 1)` structure
16. **T016**: Write unit test `test-list-multiple` verifying `(list 1 2 3)` structure
17. **T017**: Implement `compile-list` building cons chain from right to left
18. **T018**: Add `list` to `*primitives*` list
19. **T019**: Write integration test `test-list-traversal`

**Checkpoint**: `(car (cdr (list 1 2 3)))` returns 2

### Phase 4: Type Predicates (User Story 4)

**Goal**: Type testing functions

20. **T020**: Write unit test `test-consp-true` for cons cell input
21. **T021**: Write unit test `test-consp-false` for NIL and fixnum input
22. **T022**: Implement `compile-consp` using `ref.test (ref 2)`
23. **T023**: Implement `compile-null` using `ref.eq` against NIL
24. **T024**: Implement `compile-atom` as NOT consp
25. **T025**: Implement `compile-listp` as consp OR null
26. **T026**: Add predicates to `*primitives*` list
27. **T027**: Write integration tests for all predicates

**Checkpoint**: `(consp (cons 1 2))` returns T

### Phase 5: Quoted List Literals (User Story 5)

**Goal**: Quote syntax creates proper list structures

28. **T028**: Write test `test-quote-empty-list` for `'()`
29. **T029**: Write test `test-quote-list` for `'(1 2 3)`
30. **T030**: Write test `test-quote-nested-list` for `'((1 2) 3)`
31. **T031**: Verify/update `compile-literal` to handle list literals
32. **T032**: Ensure nested quoted lists work correctly

**Checkpoint**: `(car '(a b c))` returns symbol A

### Phase 6: Destructive Modification (User Story 6)

**Goal**: In-place cons cell modification

33. **T033**: Write unit test `test-rplaca` for car modification
34. **T034**: Write unit test `test-rplacd` for cdr modification
35. **T035**: Implement `compile-rplaca` using `struct.set 2 0`
36. **T036**: Implement `compile-rplacd` using `struct.set 2 1`
37. **T037**: Add `rplaca` and `rplacd` to `*primitives*` list
38. **T038**: Write integration test verifying modification persists
39. **T039**: Verify return value is modified cons (not new value)

**Checkpoint**: `(let ((x (cons 1 2))) (rplaca x 10) (car x))` returns 10

### Phase 7: List Accessors (User Story 7)

**Goal**: Convenience functions

40. **T040**: Implement `first` through `tenth` as car/cdr compositions
41. **T041**: Implement `rest` as alias for `cdr`
42. **T042**: Implement `nth` with index and list traversal
43. **T043**: Implement `nthcdr` returning tail after n cdrs
44. **T044**: Write tests for all accessors
45. **T045**: Add all accessors to `*primitives*` list

**Checkpoint**: `(third '(a b c d))` returns C

### Phase 8: Edge Cases & Error Handling

**Goal**: Handle invalid inputs correctly

46. **T046**: Write test for car/cdr on non-cons, non-nil value (should error)
47. **T047**: Implement type error signaling for invalid car/cdr
48. **T048**: Write test for `(nth -1 list)` behavior
49. **T049**: Write test for rplaca/rplacd on non-cons (should error)

**Checkpoint**: All error conditions handled correctly

### Phase 9: Performance & Polish

**Goal**: Meet success criteria

50. **T050**: Write stress test creating 10,000 cons cells
51. **T051**: Verify O(1) cons/car/cdr via timing tests
52. **T052**: Run full test suite via `nix flake check`
53. **T053**: Code cleanup and documentation

**Checkpoint**: All success criteria met

## Dependencies

| From | To | Type |
|------|-----|------|
| Phase 2 | Phase 1 | Blocking (NIL needs car/cdr) |
| Phase 3 | Phase 1 | Blocking (list needs cons) |
| Phase 4 | Phase 1 | Blocking (predicates need cons type) |
| Phase 5 | Phase 3 | Extension (quoted lists need list) |
| Phase 6 | Phase 1 | Extension (rplaca/rplacd need cons) |
| Phase 7 | Phase 1 | Extension (accessors need car/cdr) |
| Phase 8 | Phase 6 | Extension (errors need all ops) |
| Phase 9 | Phase 8 | Validation |

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| NIL representation mismatch | Verify NIL singleton global exists and is accessible |
| Type cast failures | Use `ref.test` before `ref.cast` to avoid runtime errors |
| Stack ordering issues | Careful attention to evaluation order in struct.new |
| Quote handling complexity | Leverage existing literal compilation infrastructure |

## Success Criteria Verification

| Criterion | Test | Phase |
|-----------|------|-------|
| SC-001: All 15 FRs pass tests | Full test suite | 9 |
| SC-002: O(1) cons/car/cdr | Stress test timing | 9 |
| SC-003: O(n) list construction | List stress test | 9 |
| SC-004: Existing tests pass | nix flake check | 9 |
| SC-005: Nested quotes 10 levels | test-quote-deeply-nested | 5 |
| SC-006: WasmGC allocation | No linear memory usage | All |
