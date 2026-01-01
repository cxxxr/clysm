# Tasks: I/O and List Operations Runtime Migration

**Input**: Design documents from `/specs/001-io-list-runtime/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Per Constitution VII (TDD), tests are REQUIRED for this feature.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Paths based on plan.md structure

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and test directory structure

- [ ] T001 Verify current func-section.lisp baseline with `wc -l src/clysm/compiler/codegen/func-section.lisp` (expect 18233 lines)
- [ ] T002 [P] Create test directory `tests/unit/list-runtime/`
- [ ] T003 [P] Create test directory `tests/unit/io-runtime/`
- [ ] T004 [P] Create test directory `tests/contract/runtime-migration/`
- [ ] T005 Run baseline tests with `sbcl --eval "(asdf:test-system :clysm)"` to verify green state

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T006 Add `*runtime-function-table*` parameter in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T007 Implement `compile-runtime-call` function in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T008 Update function dispatch logic to check `*runtime-function-table*` in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T009 Verify FFI primitives `%host-write-char` and `%host-write-string` work in `src/clysm/streams/ffi-io.lisp`

**Checkpoint**: Foundation ready - runtime function dispatch mechanism in place

---

## Phase 3: User Story 1 - Runtime Library I/O Functions (Priority: P1) 🎯 MVP

**Goal**: Implement I/O functions ([princ](resources/HyperSpec/Body/f_wr_pr.htm), [print](resources/HyperSpec/Body/f_wr_pr.htm), [format](resources/HyperSpec/Body/f_format.htm), [write](resources/HyperSpec/Body/f_wr_pr.htm)) in runtime library using FFI primitives

**Independent Test**: Compile a program using princ, print, format, write and verify output matches current implementation

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation (Constitution VII)**

- [ ] T010 [P] [US1] Unit test for princ in `tests/unit/io-runtime/princ-test.lisp`
- [ ] T011 [P] [US1] Unit test for prin1 in `tests/unit/io-runtime/prin1-test.lisp`
- [ ] T012 [P] [US1] Unit test for print in `tests/unit/io-runtime/print-test.lisp`
- [ ] T013 [P] [US1] Unit test for write in `tests/unit/io-runtime/write-test.lisp`
- [ ] T014 [P] [US1] Unit test for format basic directives (~A, ~S, ~D, ~%) in `tests/unit/io-runtime/format-test.lisp`
- [ ] T015 [P] [US1] Unit test for terpri in `tests/unit/io-runtime/terpri-test.lisp`
- [ ] T016 [US1] Contract test verifying I/O output matches current codegen in `tests/contract/runtime-migration/io-behavior-test.lisp`

### Implementation for User Story 1

- [ ] T017 [US1] Create `src/clysm/lib/io-runtime.lisp` with package declaration and file header
- [ ] T018 [US1] Implement `princ-rt` using %host-write-string in `src/clysm/lib/io-runtime.lisp` (FR-001)
- [ ] T019 [US1] Implement `prin1-rt` with escape syntax in `src/clysm/lib/io-runtime.lisp`
- [ ] T020 [US1] Implement `print-rt` (newline, prin1, space) in `src/clysm/lib/io-runtime.lisp` (FR-002)
- [ ] T021 [US1] Implement `write-rt` with keyword args (:stream, :escape, :radix, :base) in `src/clysm/lib/io-runtime.lisp` (FR-003)
- [ ] T022 [US1] Implement `terpri-rt` in `src/clysm/lib/io-runtime.lisp`
- [ ] T023 [US1] Implement `format-rt` with basic directives (~A, ~S, ~D, ~%) in `src/clysm/lib/io-runtime.lisp` (FR-004)
- [ ] T024 [US1] Implement format directives ~B, ~O, ~X, ~&, ~~ in `src/clysm/lib/io-runtime.lisp`
- [ ] T025 [US1] Implement format directives ~R, ~C, ~F, ~E, ~G, ~$ in `src/clysm/lib/io-runtime.lisp`
- [ ] T026 [US1] Add io-runtime.lisp to ASDF system definition in `clysm.asd`
- [ ] T027 [US1] Add I/O functions (princ, prin1, print, write, format, terpri) to `*runtime-function-table*`
- [ ] T028 [US1] Run US1 tests and verify all pass

**Checkpoint**: I/O functions work via runtime library, output identical to codegen

---

## Phase 4: User Story 2 - Runtime Library List Search Operations (Priority: P1)

**Goal**: Implement list search functions ([member](resources/HyperSpec/Body/f_mem_m.htm), [assoc](resources/HyperSpec/Body/f_assocc.htm), [find](resources/HyperSpec/Body/f_find_.htm), [position](resources/HyperSpec/Body/f_pos_p.htm)) using car/cdr/consp primitives

**Independent Test**: Compile programs using member, assoc, find, position with various arguments and verify ANSI CL behavior

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation (Constitution VII)**

- [ ] T029 [P] [US2] Unit test for member in `tests/unit/list-runtime/member-test.lisp`
- [ ] T030 [P] [US2] Unit test for assoc in `tests/unit/list-runtime/assoc-test.lisp`
- [ ] T031 [P] [US2] Unit test for rassoc in `tests/unit/list-runtime/rassoc-test.lisp`
- [ ] T032 [P] [US2] Unit test for find (list and vector) in `tests/unit/list-runtime/find-test.lisp`
- [ ] T033 [P] [US2] Unit test for position (list and vector) in `tests/unit/list-runtime/position-test.lisp`
- [ ] T034 [US2] Contract test verifying list functions match current codegen in `tests/contract/runtime-migration/list-behavior-test.lisp`

### Implementation for User Story 2

- [ ] T035 [US2] Create `src/clysm/lib/list-runtime.lisp` with package declaration and file header
- [ ] T036 [US2] Implement `member-rt` with :test, :test-not, :key in `src/clysm/lib/list-runtime.lisp` (FR-005)
- [ ] T037 [US2] Implement `assoc-rt` with :test, :test-not, :key in `src/clysm/lib/list-runtime.lisp` (FR-007)
- [ ] T038 [US2] Implement `rassoc-rt` with :test, :test-not, :key in `src/clysm/lib/list-runtime.lisp`
- [ ] T039 [US2] Implement `find-rt` with :test, :key, :start, :end, :from-end in `src/clysm/lib/list-runtime.lisp` (FR-009)
- [ ] T040 [US2] Implement `position-rt` with :test, :key, :start, :end, :from-end in `src/clysm/lib/list-runtime.lisp` (FR-011)
- [ ] T041 [US2] Add list-runtime.lisp to ASDF system definition in `clysm.asd`
- [ ] T042 [US2] Add list functions (member, assoc, rassoc, find, position) to `*runtime-function-table*`
- [ ] T043 [US2] Run US2 tests and verify all pass

**Checkpoint**: Core list search functions work via runtime library

---

## Phase 5: User Story 3 - Compile-* Function Removal (Priority: P2)

**Goal**: Remove inline codegen functions from func-section.lisp after runtime migration is verified

**Dependencies**: Requires US1 and US2 to be complete and passing

**Independent Test**: Verify func-section.lisp < 11,000 lines and all existing tests still pass

### Tests for User Story 3

- [ ] T044 [US3] Regression test suite covering all migrated functions in `tests/contract/runtime-migration/regression-test.lisp`

### Implementation for User Story 3

- [ ] T045 [US3] Remove `compile-princ` function (lines 17818-17855) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T046 [US3] Remove `compile-prin1` function (lines 17857-17885) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T047 [US3] Remove `compile-print` function (lines 17887-17934) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T048 [US3] Remove `compile-write` function (lines 17936-17991) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T049 [US3] Remove `compile-format*` functions (lines 17993-18233) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T050 [US3] Remove `compile-terpri` function from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T051 [US3] Remove `compile-member` function (lines 11985-12064) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T052 [US3] Remove `compile-assoc` function (lines 12066-12151) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T053 [US3] Remove `compile-rassoc` function (lines 12153-12240) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T054 [US3] Remove `compile-find` and `compile-find-if` functions (lines 11092-11233) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T055 [US3] Remove `compile-position` and `compile-position-if` functions (lines 11235-11415) from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T056 [US3] Verify func-section.lisp line count with `wc -l` (target: < 11,000 lines, SC-001)
- [ ] T057 [US3] Run full test suite to verify no regressions (SC-002)

**Checkpoint**: Core compile-* functions removed, line count target achieved

---

## Phase 6: User Story 4 - Variant Functions Migration (Priority: P2)

**Goal**: Migrate -if and -if-not variants of list functions to runtime library

**Independent Test**: Compile programs using member-if, assoc-if, find-if, position-if variants and verify correct behavior

### Tests for User Story 4

- [ ] T058 [P] [US4] Unit test for member-if and member-if-not in `tests/unit/list-runtime/member-if-test.lisp`
- [ ] T059 [P] [US4] Unit test for assoc-if in `tests/unit/list-runtime/assoc-if-test.lisp`
- [ ] T060 [P] [US4] Unit test for find-if and find-if-not in `tests/unit/list-runtime/find-if-test.lisp`
- [ ] T061 [P] [US4] Unit test for position-if and position-if-not in `tests/unit/list-runtime/position-if-test.lisp`

### Implementation for User Story 4

- [ ] T062 [US4] Implement `member-if-rt` with :key in `src/clysm/lib/list-runtime.lisp` (FR-006)
- [ ] T063 [US4] Implement `member-if-not-rt` with :key in `src/clysm/lib/list-runtime.lisp` (FR-006)
- [ ] T064 [US4] Implement `assoc-if-rt` with :key in `src/clysm/lib/list-runtime.lisp` (FR-008)
- [ ] T065 [US4] Implement `rassoc-if-rt` with :key in `src/clysm/lib/list-runtime.lisp`
- [ ] T066 [US4] Implement `find-if-rt` and `find-if-not-rt` in `src/clysm/lib/list-runtime.lisp` (FR-010)
- [ ] T067 [US4] Implement `position-if-rt` and `position-if-not-rt` in `src/clysm/lib/list-runtime.lisp` (FR-012)
- [ ] T068 [US4] Add -if/-if-not variants to `*runtime-function-table*`
- [ ] T069 [US4] Remove `compile-member-if*` functions from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T070 [US4] Remove `compile-assoc-if` function from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T071 [US4] Remove `compile-find-if-not` and `compile-position-if-not` from `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T072 [US4] Run US4 tests and verify all pass

**Checkpoint**: All -if/-if-not variants work via runtime library

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, performance verification, documentation updates

- [ ] T073 Integration test with sample program in `tests/integration/io-list/migration-test.lisp`
- [ ] T074 Performance benchmark comparing runtime vs old codegen (SC-003: < 10% overhead)
- [ ] T075 Verify Stage 1 generation with `sbcl --load build/stage1-complete.lisp`
- [ ] T076 Validate generated Wasm with `wasm-tools validate dist/clysm-stage1.wasm`
- [ ] T077 Verify runtime library total lines < 2,000 (SC-006)
- [ ] T078 Final func-section.lisp line count verification (SC-001)
- [ ] T079 [P] Update CLAUDE.md with feature completion under "Recent Changes"
- [ ] T080 Run quickstart.md validation steps

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational (Phase 2)
- **User Story 2 (Phase 4)**: Depends on Foundational (Phase 2), can run parallel with US1
- **User Story 3 (Phase 5)**: **Depends on BOTH US1 and US2** (cannot remove codegen until runtime works)
- **User Story 4 (Phase 6)**: Depends on Foundational (Phase 2), can run parallel with US1/US2
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

```
                    ┌─────────┐
                    │  Setup  │
                    └────┬────┘
                         │
                    ┌────▼────┐
                    │ Found.  │
                    └────┬────┘
           ┌─────────────┼─────────────┐
           │             │             │
      ┌────▼────┐   ┌────▼────┐   ┌────▼────┐
      │  US1    │   │  US2    │   │  US4    │
      │ (P1 I/O)│   │(P1 List)│   │(P2 -if) │
      └────┬────┘   └────┬────┘   └────┬────┘
           │             │             │
           └──────┬──────┘             │
                  │                    │
             ┌────▼────┐               │
             │   US3   │◄──────────────┘
             │(P2 Remove)
             └────┬────┘
                  │
             ┌────▼────┐
             │ Polish  │
             └─────────┘
```

### Within Each User Story

- Tests MUST be written and FAIL before implementation (Constitution VII)
- Implementation follows TDD cycle: Red → Green → Refactor
- Run story-specific tests after each implementation task
- Story complete before moving to dependent stories

### Parallel Opportunities

**After Foundational phase completes:**
- US1, US2, and US4 can all start in parallel (different files, no dependencies)
- All tests marked [P] within a story can run in parallel
- US3 must wait for US1 and US2 to complete

**Within phases:**
- T002, T003, T004 (Setup test directories) can run in parallel
- T010-T016 (US1 tests) can run in parallel
- T029-T033 (US2 tests) can run in parallel
- T058-T061 (US4 tests) can run in parallel

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all US1 tests together (all [P] marked):
Task: "Unit test for princ in tests/unit/io-runtime/princ-test.lisp"
Task: "Unit test for prin1 in tests/unit/io-runtime/prin1-test.lisp"
Task: "Unit test for print in tests/unit/io-runtime/print-test.lisp"
Task: "Unit test for write in tests/unit/io-runtime/write-test.lisp"
Task: "Unit test for format in tests/unit/io-runtime/format-test.lisp"
Task: "Unit test for terpri in tests/unit/io-runtime/terpri-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 + User Story 2)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (I/O Functions)
4. Complete Phase 4: User Story 2 (List Functions)
5. **STOP and VALIDATE**: Test both stories independently
6. Proceed to US3 (removal) only after validation

### Incremental Delivery

1. Setup + Foundational → Runtime dispatch mechanism ready
2. Add US1 → Test I/O functions independently → Validate output identical
3. Add US2 → Test list functions independently → Validate ANSI CL behavior
4. Add US3 → Remove codegen, verify line count target
5. Add US4 → Complete -if variants → Full migration complete
6. Polish → Performance validation, documentation

### Parallel Team Strategy

With 2-3 developers after Foundational phase:
- Developer A: User Story 1 (I/O)
- Developer B: User Story 2 (List)
- Developer C: User Story 4 (-if variants)
- All: User Story 3 (removal) after US1+US2 complete

---

## Summary

| Metric | Value |
|--------|-------|
| Total Tasks | 80 |
| Setup Tasks | 5 |
| Foundational Tasks | 4 |
| US1 Tasks (I/O) | 19 |
| US2 Tasks (List) | 15 |
| US3 Tasks (Removal) | 14 |
| US4 Tasks (Variants) | 15 |
| Polish Tasks | 8 |
| Parallel Opportunities | 32 tasks marked [P] |

**MVP Scope**: User Story 1 + User Story 2 (I/O and List functions working via runtime)

**Target Outcome**: func-section.lisp reduced from 18,233 to under 11,000 lines (40%+ reduction)

---

## Notes

- [P] tasks = different files, no dependencies on incomplete tasks
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- Verify tests fail before implementing (Constitution VII - TDD)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
