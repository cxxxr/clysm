# Tasks: ANSI Sequence Generic Functions (Phase 15B)

**Input**: Design documents from `/specs/001-ansi-sequence-functions/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Included per Constitution VII (TDD mandated)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story (US1-US6)
- Exact file paths included

## Path Conventions

- **Source**: `src/clysm/lib/`
- **Tests**: `tests/unit/sequences/`
- **Contracts**: `tests/contract/sequences/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Create file structure and shared utilities

- [ ] T001 Create test directory structure `tests/unit/sequences/` and `tests/contract/sequences/`
- [ ] T002 [P] Create `src/clysm/lib/sequences.lisp` with package header and HyperSpec references
- [ ] T003 [P] Create `src/clysm/lib/sequences-util.lisp` with package header

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core utilities that ALL sequence functions depend on

**CRITICAL**: No user story work can begin until this phase is complete

- [ ] T004 Implement `validate-bounding-indices` in `src/clysm/lib/sequences-util.lisp`
- [ ] T005 [P] Implement `%sequence-length` type-dispatching helper in `src/clysm/lib/sequences-util.lisp`
- [ ] T006 [P] Implement `%sequence-ref` type-dispatching element accessor in `src/clysm/lib/sequences-util.lisp`
- [ ] T007 [P] Implement `%make-sequence-result` for constructing result sequences in `src/clysm/lib/sequences-util.lisp`
- [ ] T008 Add unit tests for utility functions in `tests/unit/sequences/util-test.lisp`
- [ ] T009 Register sequence utility exports in `src/clysm/lib/sequences-util.lisp`

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Element Counting Functions (Priority: P1)

**Goal**: Implement count, count-if, count-if-not for lists, vectors, and strings

**Independent Test**: Call count functions on all sequence types, verify counts match expected values

**HyperSpec**: [f_countc.htm](resources/HyperSpec/Body/f_countc.htm)

### Tests for User Story 1

> **TDD: Write tests FIRST, ensure they FAIL before implementation**

- [ ] T010 [P] [US1] Create test file `tests/unit/sequences/count-test.lisp` with rove test package
- [ ] T011 [P] [US1] Write tests for `count*` basic functionality (list, vector, string) in `tests/unit/sequences/count-test.lisp`
- [ ] T012 [P] [US1] Write tests for `count*` with :key keyword in `tests/unit/sequences/count-test.lisp`
- [ ] T013 [P] [US1] Write tests for `count*` with :test keyword in `tests/unit/sequences/count-test.lisp`
- [ ] T014 [P] [US1] Write tests for `count*` with :start/:end bounds in `tests/unit/sequences/count-test.lisp`
- [ ] T015 [P] [US1] Write tests for `count*` with :from-end in `tests/unit/sequences/count-test.lisp`
- [ ] T016 [P] [US1] Write tests for `count-if*` and `count-if-not*` in `tests/unit/sequences/count-test.lisp`
- [ ] T017 [P] [US1] Write edge case tests (empty seq, start=end, invalid bounds) in `tests/unit/sequences/count-test.lisp`

### Implementation for User Story 1

- [ ] T018 [US1] Implement `count*` for lists in `src/clysm/lib/sequences.lisp`
- [ ] T019 [US1] Extend `count*` for vectors in `src/clysm/lib/sequences.lisp`
- [ ] T020 [US1] Extend `count*` for strings in `src/clysm/lib/sequences.lisp`
- [ ] T021 [P] [US1] Implement `count-if*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T022 [P] [US1] Implement `count-if-not*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T023 [US1] Run `tests/unit/sequences/count-test.lisp` and verify all tests pass
- [ ] T024 [US1] Export count functions in `src/clysm/lib/sequences.lisp`

**Checkpoint**: count family fully functional and tested independently

---

## Phase 4: User Story 2 - Element Search Functions (Priority: P1)

**Goal**: Implement find, find-if, find-if-not, position, position-if, position-if-not

**Independent Test**: Call find/position functions, verify correct element/index or NIL returned

**HyperSpec**: [f_find_.htm](resources/HyperSpec/Body/f_find_.htm), [f_pos_p.htm](resources/HyperSpec/Body/f_pos_p.htm)

### Tests for User Story 2

- [ ] T025 [P] [US2] Create test file `tests/unit/sequences/find-test.lisp` with rove test package
- [ ] T026 [P] [US2] Create test file `tests/unit/sequences/position-test.lisp` with rove test package
- [ ] T027 [P] [US2] Write tests for `find*` basic functionality in `tests/unit/sequences/find-test.lisp`
- [ ] T028 [P] [US2] Write tests for `find*` with :test, :key, :from-end in `tests/unit/sequences/find-test.lisp`
- [ ] T029 [P] [US2] Write tests for `find-if*` and `find-if-not*` in `tests/unit/sequences/find-test.lisp`
- [ ] T030 [P] [US2] Write tests for `position*` basic functionality in `tests/unit/sequences/position-test.lisp`
- [ ] T031 [P] [US2] Write tests for `position*` with :test, :key, :from-end in `tests/unit/sequences/position-test.lisp`
- [ ] T032 [P] [US2] Write tests for `position-if*` and `position-if-not*` in `tests/unit/sequences/position-test.lisp`
- [ ] T033 [P] [US2] Write edge case tests for find/position in respective test files

### Implementation for User Story 2

- [ ] T034 [US2] Implement `find*` with etypecase dispatch in `src/clysm/lib/sequences.lisp`
- [ ] T035 [P] [US2] Implement `find-if*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T036 [P] [US2] Implement `find-if-not*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T037 [US2] Implement `position*` with etypecase dispatch in `src/clysm/lib/sequences.lisp`
- [ ] T038 [P] [US2] Implement `position-if*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T039 [P] [US2] Implement `position-if-not*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T040 [US2] Run find and position tests, verify all pass
- [ ] T041 [US2] Export find and position functions in `src/clysm/lib/sequences.lisp`

**Checkpoint**: find and position families fully functional and tested

---

## Phase 5: User Story 3 - Sequence Comparison Functions (Priority: P2)

**Goal**: Implement mismatch and search for comparing/finding subsequences

**Independent Test**: Compare sequences, verify mismatch positions and search indices

**HyperSpec**: [f_mismat.htm](resources/HyperSpec/Body/f_mismat.htm), [f_search.htm](resources/HyperSpec/Body/f_search.htm)

### Tests for User Story 3

- [ ] T042 [P] [US3] Create test file `tests/unit/sequences/mismatch-test.lisp` with rove test package
- [ ] T043 [P] [US3] Create test file `tests/unit/sequences/search-test.lisp` with rove test package
- [ ] T044 [P] [US3] Write tests for `mismatch*` basic functionality in `tests/unit/sequences/mismatch-test.lisp`
- [ ] T045 [P] [US3] Write tests for `mismatch*` with :start1/:end1/:start2/:end2 in `tests/unit/sequences/mismatch-test.lisp`
- [ ] T046 [P] [US3] Write tests for `mismatch*` with :from-end in `tests/unit/sequences/mismatch-test.lisp`
- [ ] T047 [P] [US3] Write tests for `search*` basic functionality in `tests/unit/sequences/search-test.lisp`
- [ ] T048 [P] [US3] Write tests for `search*` with :from-end in `tests/unit/sequences/search-test.lisp`
- [ ] T049 [P] [US3] Write edge case tests for mismatch/search (empty seq, no match)

### Implementation for User Story 3

- [ ] T050 [US3] Implement `mismatch*` with two-sequence bounds validation in `src/clysm/lib/sequences.lisp`
- [ ] T051 [US3] Implement `search*` with subsequence matching in `src/clysm/lib/sequences.lisp`
- [ ] T052 [US3] Run mismatch and search tests, verify all pass
- [ ] T053 [US3] Export mismatch and search functions in `src/clysm/lib/sequences.lisp`

**Checkpoint**: mismatch and search fully functional and tested

---

## Phase 6: User Story 4 - Element Substitution Functions (Priority: P2)

**Goal**: Implement substitute, substitute-if, substitute-if-not, nsubstitute, nsubstitute-if, nsubstitute-if-not

**Independent Test**: Substitute elements, verify correct replacements and original/new sequence behavior

**HyperSpec**: [f_substc.htm](resources/HyperSpec/Body/f_substc.htm)

### Tests for User Story 4

- [ ] T054 [P] [US4] Create test file `tests/unit/sequences/substitute-test.lisp` with rove test package
- [ ] T055 [P] [US4] Write tests for `substitute*` non-destructive behavior in `tests/unit/sequences/substitute-test.lisp`
- [ ] T056 [P] [US4] Write tests for `substitute*` with :count keyword in `tests/unit/sequences/substitute-test.lisp`
- [ ] T057 [P] [US4] Write tests for `substitute*` with :from-end + :count in `tests/unit/sequences/substitute-test.lisp`
- [ ] T058 [P] [US4] Write tests for `substitute-if*` and `substitute-if-not*` in `tests/unit/sequences/substitute-test.lisp`
- [ ] T059 [P] [US4] Write tests for `nsubstitute*` destructive behavior in `tests/unit/sequences/substitute-test.lisp`
- [ ] T060 [P] [US4] Write tests for `nsubstitute-if*` and `nsubstitute-if-not*` in `tests/unit/sequences/substitute-test.lisp`

### Implementation for User Story 4

- [ ] T061 [US4] Implement `%substitute-impl` internal helper with copy-p flag in `src/clysm/lib/sequences.lisp`
- [ ] T062 [US4] Implement `substitute*` as wrapper around `%substitute-impl` in `src/clysm/lib/sequences.lisp`
- [ ] T063 [P] [US4] Implement `substitute-if*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T064 [P] [US4] Implement `substitute-if-not*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T065 [US4] Implement `nsubstitute*` as wrapper with copy-p=nil in `src/clysm/lib/sequences.lisp`
- [ ] T066 [P] [US4] Implement `nsubstitute-if*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T067 [P] [US4] Implement `nsubstitute-if-not*` using shared pattern in `src/clysm/lib/sequences.lisp`
- [ ] T068 [US4] Run substitute tests, verify all pass
- [ ] T069 [US4] Export substitute functions in `src/clysm/lib/sequences.lisp`

**Checkpoint**: substitute family fully functional with destructive/non-destructive variants

---

## Phase 7: User Story 5 - Duplicate Removal Functions (Priority: P2)

**Goal**: Implement remove-duplicates and delete-duplicates

**Independent Test**: Remove duplicates, verify unique elements in result

**HyperSpec**: [f_rm_dup.htm](resources/HyperSpec/Body/f_rm_dup.htm)

### Tests for User Story 5

- [ ] T070 [P] [US5] Create test file `tests/unit/sequences/remove-duplicates-test.lisp` with rove test package
- [ ] T071 [P] [US5] Write tests for `remove-duplicates*` keeping last occurrences in `tests/unit/sequences/remove-duplicates-test.lisp`
- [ ] T072 [P] [US5] Write tests for `remove-duplicates*` with :from-end t (keep first) in `tests/unit/sequences/remove-duplicates-test.lisp`
- [ ] T073 [P] [US5] Write tests for `remove-duplicates*` with :key in `tests/unit/sequences/remove-duplicates-test.lisp`
- [ ] T074 [P] [US5] Write tests for `remove-duplicates*` with :test in `tests/unit/sequences/remove-duplicates-test.lisp`
- [ ] T075 [P] [US5] Write tests for `delete-duplicates*` destructive behavior in `tests/unit/sequences/remove-duplicates-test.lisp`

### Implementation for User Story 5

- [ ] T076 [US5] Implement `%remove-duplicates-impl` internal helper in `src/clysm/lib/sequences.lisp`
- [ ] T077 [US5] Implement `remove-duplicates*` as wrapper in `src/clysm/lib/sequences.lisp`
- [ ] T078 [US5] Implement `delete-duplicates*` as destructive wrapper in `src/clysm/lib/sequences.lisp`
- [ ] T079 [US5] Run remove-duplicates tests, verify all pass
- [ ] T080 [US5] Export remove-duplicates functions in `src/clysm/lib/sequences.lisp`

**Checkpoint**: remove-duplicates family fully functional

---

## Phase 8: User Story 6 - Sequence Modification Functions (Priority: P3)

**Goal**: Implement fill and replace for bulk sequence modifications

**Independent Test**: Fill/replace sequences, verify modifications in place

**HyperSpec**: [f_fill.htm](resources/HyperSpec/Body/f_fill.htm), [f_replac.htm](resources/HyperSpec/Body/f_replac.htm)

### Tests for User Story 6

- [ ] T081 [P] [US6] Create test file `tests/unit/sequences/fill-test.lisp` with rove test package
- [ ] T082 [P] [US6] Create test file `tests/unit/sequences/replace-test.lisp` with rove test package
- [ ] T083 [P] [US6] Write tests for `fill*` basic functionality in `tests/unit/sequences/fill-test.lisp`
- [ ] T084 [P] [US6] Write tests for `fill*` with :start/:end in `tests/unit/sequences/fill-test.lisp`
- [ ] T085 [P] [US6] Write tests for `replace*` basic functionality in `tests/unit/sequences/replace-test.lisp`
- [ ] T086 [P] [US6] Write tests for `replace*` with :start1/:end1/:start2/:end2 in `tests/unit/sequences/replace-test.lisp`

### Implementation for User Story 6

- [ ] T087 [US6] Implement `fill*` with type dispatch in `src/clysm/lib/sequences.lisp`
- [ ] T088 [US6] Implement `replace*` with type dispatch in `src/clysm/lib/sequences.lisp`
- [ ] T089 [US6] Run fill and replace tests, verify all pass
- [ ] T090 [US6] Export fill and replace functions in `src/clysm/lib/sequences.lisp`

**Checkpoint**: All 21 sequence functions implemented

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, ANSI compliance, integration

- [ ] T091 [P] Update `src/clysm/clysm.asd` to include sequences.lisp and sequences-util.lisp
- [ ] T092 [P] Register sequence functions as builtins in `src/clysm/compiler/codegen/builtins.lisp`
- [ ] T093 Run full test suite `sbcl --eval "(asdf:test-system :clysm)"`
- [ ] T094 [P] Create ANSI compliance test in `tests/contract/sequences/ansi-compliance-test.lisp`
- [ ] T095 Run ANSI sequences test suite and measure compliance rate
- [ ] T096 Verify 60%+ ANSI sequences compliance (SC-001)
- [ ] T097 [P] Add HyperSpec links to all function docstrings per Constitution IX
- [ ] T098 Run `wasm-tools validate dist/clysm-stage1.wasm` to verify no regressions
- [ ] T099 Update CLAUDE.md with completed feature entry

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phases 3-8)**: Depend on Foundational completion
  - US1 and US2 are both P1, can run in parallel
  - US3, US4, US5 are all P2, can run in parallel after US1/US2
  - US6 is P3, can run after P2 stories
- **Polish (Phase 9)**: Depends on all user stories

### User Story Dependencies

| Story | Priority | Depends On | Can Parallelize With |
|-------|----------|------------|----------------------|
| US1 (count) | P1 | Foundational | US2 |
| US2 (find/position) | P1 | Foundational | US1 |
| US3 (mismatch/search) | P2 | Foundational | US4, US5 |
| US4 (substitute) | P2 | Foundational | US3, US5 |
| US5 (remove-duplicates) | P2 | Foundational | US3, US4 |
| US6 (fill/replace) | P3 | Foundational | None (lowest priority) |

### Within Each User Story

1. Tests FIRST (TDD per Constitution VII)
2. Ensure tests FAIL before implementation
3. Implement functions
4. Verify all tests PASS
5. Export functions

---

## Parallel Execution Examples

### Phase 2: Foundational (All [P] tasks together)

```bash
# Run in parallel:
Task T005: "%sequence-length helper"
Task T006: "%sequence-ref helper"
Task T007: "%make-sequence-result helper"
```

### Phase 3: User Story 1 Tests (All [P] tasks together)

```bash
# Run all test tasks in parallel:
Task T011-T017: All count test tasks
```

### Multiple User Stories in Parallel

```bash
# With 2 developers after Foundational:
Developer A: US1 (count) → US3 (mismatch/search) → US6 (fill/replace)
Developer B: US2 (find/position) → US4 (substitute) → US5 (remove-duplicates)
```

---

## Implementation Strategy

### MVP First (US1 + US2 Only)

1. Complete Setup (Phase 1)
2. Complete Foundational (Phase 2)
3. Complete US1: count family
4. Complete US2: find/position family
5. **STOP**: Basic sequence operations working, ~9 functions
6. Validate with ANSI tests

### Full Implementation

1. Setup → Foundational → US1 + US2 (P1 functions)
2. US3 + US4 + US5 (P2 functions) - can parallelize
3. US6 (P3 functions)
4. Polish and compliance validation

### Target Metrics

- **Total Functions**: 21
- **Total Tasks**: 99
- **Tasks per Story**: US1(15), US2(17), US3(12), US4(16), US5(11), US6(10)
- **Parallel Opportunities**: 48 tasks marked [P]
- **MVP Scope**: US1 + US2 = 32 tasks, 9 functions

---

## Notes

- [P] tasks = different files, safe to parallelize
- [USn] labels map tasks to user stories for traceability
- TDD mandated by Constitution VII - write tests first
- HyperSpec links required by Constitution IX
- Each user story is independently testable
- Commit after each task or logical group
