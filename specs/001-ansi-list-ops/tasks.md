# Tasks: Phase 15A - ANSI List Operations Extension

**Input**: Design documents from `/specs/001-ansi-list-ops/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: REQUIRED per Constitution VII (TDD). Tests are written FIRST and must FAIL before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Test structure: `tests/unit/list-ops/`, `tests/contract/`

---

## Phase 1: Setup

**Purpose**: Project structure and test infrastructure

- [ ] T001 Create `src/clysm/lib/list-ops.lisp` with package definition
- [ ] T002 [P] Create `tests/unit/list-ops/` directory structure
- [ ] T003 [P] Create test package in `tests/unit/list-ops/package.lisp`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T004 Register list-ops functions in `src/clysm/compiler/codegen/builtins.lisp` (add dispatch entries for: last, butlast, nbutlast, nth, nthcdr, member, member-if, member-if-not, assoc, assoc-if, rassoc, rassoc-if, pairlis, acons, copy-alist, intersection, union, set-difference, subsetp, adjoin)
- [ ] T005 Add list-ops.lisp to ASDF system definition in `clysm.asd`
- [ ] T006 Add test files to ASDF test system definition

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - List Tail Operations (Priority: P1) 🎯 MVP

**Goal**: Implement [last](resources/HyperSpec/Body/f_last.htm), [butlast/nbutlast](resources/HyperSpec/Body/f_butlas.htm), [nth](resources/HyperSpec/Body/f_nth.htm), [nthcdr](resources/HyperSpec/Body/f_nthcdr.htm) for basic list access

**Independent Test**: `(nth 2 '(a b c d))` → `c`, `(last '(a b c))` → `(c)`, `(nthcdr 2 '(a b c d))` → `(c d)`

### Tests for User Story 1 (TDD - Write FIRST) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T007 [P] [US1] Create unit test file `tests/unit/list-ops/list-tail-test.lisp` with test package
- [ ] T008 [P] [US1] Write test for `last` basic case in `tests/unit/list-ops/list-tail-test.lisp`
- [ ] T009 [P] [US1] Write test for `last` with n argument in `tests/unit/list-ops/list-tail-test.lisp`
- [ ] T010 [P] [US1] Write test for `butlast` basic case in `tests/unit/list-ops/list-tail-test.lisp`
- [ ] T011 [P] [US1] Write test for `nbutlast` destructive behavior in `tests/unit/list-ops/list-tail-test.lisp`
- [ ] T012 [P] [US1] Write test for `nth` zero-indexed access in `tests/unit/list-ops/list-tail-test.lisp`
- [ ] T013 [P] [US1] Write test for `nthcdr` cdr-n-times in `tests/unit/list-ops/list-tail-test.lisp`
- [ ] T014 [P] [US1] Write edge case tests (empty list, n > length) in `tests/unit/list-ops/list-tail-test.lisp`

### Implementation for User Story 1

- [ ] T015 [US1] Implement `nthcdr` function in `src/clysm/lib/list-ops.lisp` (loop-based traversal)
- [ ] T016 [US1] Implement `nth` function using `nthcdr` in `src/clysm/lib/list-ops.lisp`
- [ ] T017 [US1] Implement `last` function in `src/clysm/lib/list-ops.lisp` (two-pointer technique)
- [ ] T018 [US1] Implement `butlast` function in `src/clysm/lib/list-ops.lisp` (fresh list copy)
- [ ] T019 [US1] Implement `nbutlast` function in `src/clysm/lib/list-ops.lisp` (destructive rplacd)
- [ ] T020 [US1] Verify all US1 tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: User Story 1 complete - nth, nthcdr, last, butlast, nbutlast working independently

---

## Phase 4: User Story 2 - Membership Operations (Priority: P1)

**Goal**: Implement [member/member-if/member-if-not](resources/HyperSpec/Body/f_mem_m.htm) with :test/:key support

**Independent Test**: `(member 3 '(1 2 3 4))` → `(3 4)`, `(member-if #'evenp '(1 2 3))` → `(2 3)`

### Tests for User Story 2 (TDD - Write FIRST) ⚠️

- [ ] T021 [P] [US2] Create unit test file `tests/unit/list-ops/member-test.lisp` with test package
- [ ] T022 [P] [US2] Write test for `member` basic EQL comparison in `tests/unit/list-ops/member-test.lisp`
- [ ] T023 [P] [US2] Write test for `member` with :test #'equal in `tests/unit/list-ops/member-test.lisp`
- [ ] T024 [P] [US2] Write test for `member` with :key #'car in `tests/unit/list-ops/member-test.lisp`
- [ ] T025 [P] [US2] Write test for `member-if` predicate in `tests/unit/list-ops/member-test.lisp`
- [ ] T026 [P] [US2] Write test for `member-if-not` predicate in `tests/unit/list-ops/member-test.lisp`
- [ ] T027 [P] [US2] Write edge case tests (not found returns NIL) in `tests/unit/list-ops/member-test.lisp`

### Implementation for User Story 2

- [ ] T028 [US2] Implement `member` with :test/:key support in `src/clysm/lib/list-ops.lisp`
- [ ] T029 [US2] Implement `member-if` with :key support in `src/clysm/lib/list-ops.lisp`
- [ ] T030 [US2] Implement `member-if-not` with :key support in `src/clysm/lib/list-ops.lisp`
- [ ] T031 [US2] Verify all US2 tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: User Story 2 complete - member family working independently

---

## Phase 5: User Story 3 - Association List Lookup (Priority: P1)

**Goal**: Implement [assoc/assoc-if](resources/HyperSpec/Body/f_assocc.htm), [rassoc/rassoc-if](resources/HyperSpec/Body/f_rassoc.htm) for alist operations

**Independent Test**: `(assoc 'b '((a . 1) (b . 2)))` → `(B . 2)`, `(rassoc 2 '((a . 1) (b . 2)))` → `(B . 2)`

### Tests for User Story 3 (TDD - Write FIRST) ⚠️

- [ ] T032 [P] [US3] Extend existing `tests/unit/list-ops/assoc-test.lisp` with rassoc tests
- [ ] T033 [P] [US3] Write test for `assoc` not found returns NIL in `tests/unit/list-ops/assoc-test.lisp`
- [ ] T034 [P] [US3] Write test for `assoc-if` predicate in `tests/unit/list-ops/assoc-test.lisp`
- [ ] T035 [P] [US3] Write test for `rassoc` basic lookup in `tests/unit/list-ops/assoc-test.lisp`
- [ ] T036 [P] [US3] Write test for `rassoc` with :test/:key in `tests/unit/list-ops/assoc-test.lisp`
- [ ] T037 [P] [US3] Write test for `rassoc-if` predicate in `tests/unit/list-ops/assoc-test.lisp`
- [ ] T038 [P] [US3] Write test for skipping non-cons elements in `tests/unit/list-ops/assoc-test.lisp`

### Implementation for User Story 3

- [ ] T039 [US3] Implement `assoc` with :test/:key support in `src/clysm/lib/list-ops.lisp`
- [ ] T040 [US3] Implement `assoc-if` with :key support in `src/clysm/lib/list-ops.lisp`
- [ ] T041 [US3] Implement `rassoc` with :test/:key support in `src/clysm/lib/list-ops.lisp`
- [ ] T042 [US3] Implement `rassoc-if` with :key support in `src/clysm/lib/list-ops.lisp`
- [ ] T043 [US3] Verify all US3 tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: User Story 3 complete - assoc family working independently

---

## Phase 6: User Story 4 - Association List Construction (Priority: P2)

**Goal**: Implement [pairlis](resources/HyperSpec/Body/f_pairli.htm), [acons](resources/HyperSpec/Body/f_acons.htm), [copy-alist](resources/HyperSpec/Body/f_cp_ali.htm) for alist building

**Independent Test**: `(acons 'a 1 '((b . 2)))` → `((A . 1) (B . 2))`, `(pairlis '(a b) '(1 2))` → `((A . 1) (B . 2))`

### Tests for User Story 4 (TDD - Write FIRST) ⚠️

- [ ] T044 [P] [US4] Create unit test file `tests/unit/list-ops/alist-construct-test.lisp` with test package
- [ ] T045 [P] [US4] Write test for `acons` basic construction in `tests/unit/list-ops/alist-construct-test.lisp`
- [ ] T046 [P] [US4] Write test for `pairlis` with two lists in `tests/unit/list-ops/alist-construct-test.lisp`
- [ ] T047 [P] [US4] Write test for `pairlis` with existing alist in `tests/unit/list-ops/alist-construct-test.lisp`
- [ ] T048 [P] [US4] Write test for `copy-alist` creates fresh conses in `tests/unit/list-ops/alist-construct-test.lisp`
- [ ] T049 [P] [US4] Write test for `copy-alist` preserves car/cdr values in `tests/unit/list-ops/alist-construct-test.lisp`

### Implementation for User Story 4

- [ ] T050 [US4] Implement `acons` function in `src/clysm/lib/list-ops.lisp`
- [ ] T051 [US4] Implement `pairlis` function in `src/clysm/lib/list-ops.lisp`
- [ ] T052 [US4] Implement `copy-alist` function in `src/clysm/lib/list-ops.lisp`
- [ ] T053 [US4] Verify all US4 tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: User Story 4 complete - alist construction working independently

---

## Phase 7: User Story 5 - Set Operations (Priority: P2)

**Goal**: Implement [intersection](resources/HyperSpec/Body/f_intera.htm), [union](resources/HyperSpec/Body/f_unionc.htm), [set-difference](resources/HyperSpec/Body/f_set_di.htm), [subsetp](resources/HyperSpec/Body/f_subset.htm), [adjoin](resources/HyperSpec/Body/f_adjoin.htm)

**Independent Test**: `(intersection '(1 2 3) '(2 3 4))` → `(2 3)` or `(3 2)`, `(subsetp '(1 2) '(1 2 3))` → `T`

### Tests for User Story 5 (TDD - Write FIRST) ⚠️

- [ ] T054 [P] [US5] Create unit test file `tests/unit/list-ops/set-ops-test.lisp` with test package
- [ ] T055 [P] [US5] Write test for `intersection` basic in `tests/unit/list-ops/set-ops-test.lisp`
- [ ] T056 [P] [US5] Write test for `intersection` with :test/:key in `tests/unit/list-ops/set-ops-test.lisp`
- [ ] T057 [P] [US5] Write test for `union` basic in `tests/unit/list-ops/set-ops-test.lisp`
- [ ] T058 [P] [US5] Write test for `set-difference` basic in `tests/unit/list-ops/set-ops-test.lisp`
- [ ] T059 [P] [US5] Write test for `subsetp` true case in `tests/unit/list-ops/set-ops-test.lisp`
- [ ] T060 [P] [US5] Write test for `subsetp` false case in `tests/unit/list-ops/set-ops-test.lisp`
- [ ] T061 [P] [US5] Write test for `adjoin` adds if not member in `tests/unit/list-ops/set-ops-test.lisp`
- [ ] T062 [P] [US5] Write test for `adjoin` no-op if member in `tests/unit/list-ops/set-ops-test.lisp`

### Implementation for User Story 5

- [ ] T063 [US5] Implement `adjoin` function (uses member) in `src/clysm/lib/list-ops.lisp`
- [ ] T064 [US5] Implement `intersection` function in `src/clysm/lib/list-ops.lisp`
- [ ] T065 [US5] Implement `union` function in `src/clysm/lib/list-ops.lisp`
- [ ] T066 [US5] Implement `set-difference` function in `src/clysm/lib/list-ops.lisp`
- [ ] T067 [US5] Implement `subsetp` function in `src/clysm/lib/list-ops.lisp`
- [ ] T068 [US5] Verify all US5 tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: User Story 5 complete - set operations working independently

---

## Phase 8: User Story 6 - PUSHNEW Macro (Priority: P3)

**Goal**: Implement [pushnew](resources/HyperSpec/Body/m_push.htm) macro using SETF + ADJOIN

**Independent Test**: `(let ((x '(1 2))) (pushnew 3 x) x)` → `(3 1 2)`

### Tests for User Story 6 (TDD - Write FIRST) ⚠️

- [ ] T069 [P] [US6] Create unit test file `tests/unit/list-ops/pushnew-test.lisp` with test package
- [ ] T070 [P] [US6] Write test for `pushnew` adds to list in `tests/unit/list-ops/pushnew-test.lisp`
- [ ] T071 [P] [US6] Write test for `pushnew` no-op if member in `tests/unit/list-ops/pushnew-test.lisp`
- [ ] T072 [P] [US6] Write test for `pushnew` with :test/:key in `tests/unit/list-ops/pushnew-test.lisp`

### Implementation for User Story 6

- [ ] T073 [US6] Add `pushnew` macro definition in `src/clysm/compiler/transform/macro.lisp`
- [ ] T074 [US6] Implement macro expansion: `(setf place (adjoin item place :test test :key key))`
- [ ] T075 [US6] Verify all US6 tests pass with `sbcl --eval "(asdf:test-system :clysm)"`

**Checkpoint**: User Story 6 complete - pushnew macro working

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Contract tests, validation, and documentation

- [ ] T076 [P] Create contract test file `tests/contract/list-ops-contract-test.lisp`
- [ ] T077 [P] Write contract tests verifying Wasm output structure for list ops
- [ ] T078 Run verification examples from spec SC-004:
  - `(intersection '(1 2 3) '(2 3 4))` → contains 2 and 3
  - `(member 2 '(1 2 3))` → `(2 3)`
  - `(assoc 'b '((a . 1) (b . 2)))` → `(B . 2)`
- [ ] T079 Run full test suite and verify cons category 50%+ pass rate
- [ ] T080 Validate Wasm output with `wasm-tools validate`
- [ ] T081 [P] Run quickstart.md validation scenarios

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-8)**: All depend on Foundational phase completion
  - US1, US2, US3 are all P1 and can proceed in parallel
  - US4, US5 are P2 and can proceed in parallel after US2 (member used by set ops)
  - US6 is P3 and depends on US5 (adjoin)
- **Polish (Phase 9)**: Depends on all user stories being complete

### User Story Dependencies

| Story | Depends On | Notes |
|-------|------------|-------|
| US1 (List Tail) | Foundational | Independent |
| US2 (Membership) | Foundational | Independent |
| US3 (Alist Lookup) | Foundational | Independent |
| US4 (Alist Construct) | Foundational | Independent |
| US5 (Set Ops) | US2 | Uses `member` internally |
| US6 (PUSHNEW) | US5 | Uses `adjoin` |

### Within Each User Story

1. Write tests FIRST (TDD) - must FAIL before implementation
2. Implement functions in dependency order
3. Verify all story tests pass
4. Story complete before moving to next

### Parallel Opportunities

**Phase 1 (Setup)**:
- T002, T003 can run in parallel

**Phase 2 (Foundational)**:
- Sequential (single file modifications)

**User Stories**:
- US1, US2, US3, US4 can run in parallel (no inter-story dependencies)
- All test tasks within a story can run in parallel
- Implementation tasks mostly sequential within a story

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Write test for last basic case in tests/unit/list-ops/list-tail-test.lisp"
Task: "Write test for butlast basic case in tests/unit/list-ops/list-tail-test.lisp"
Task: "Write test for nth zero-indexed access in tests/unit/list-ops/list-tail-test.lisp"
Task: "Write test for nthcdr cdr-n-times in tests/unit/list-ops/list-tail-test.lisp"
```

## Parallel Example: Multiple User Stories

```bash
# After Foundational phase, launch independent stories in parallel:
# Developer A: User Story 1 (List Tail)
# Developer B: User Story 2 (Membership)
# Developer C: User Story 3 (Alist Lookup)
```

---

## Implementation Strategy

### MVP First (User Stories 1-3 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (List Tail - basic accessors)
4. Complete Phase 4: User Story 2 (Membership - enables set ops later)
5. Complete Phase 5: User Story 3 (Alist Lookup)
6. **STOP and VALIDATE**: Test all P1 stories independently
7. Run spec verification examples

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently (nth, last work)
3. Add User Story 2 → Test independently (member works)
4. Add User Story 3 → Test independently (assoc works)
5. Add User Story 4 → Test independently (alist construction works)
6. Add User Story 5 → Test independently (set ops work)
7. Add User Story 6 → Test independently (pushnew works)
8. Polish phase → All verification criteria met

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests FAIL before implementing (TDD per Constitution VII)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- HyperSpec references included for ANSI CL compliance (Constitution IX)
