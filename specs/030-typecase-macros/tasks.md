# Tasks: Type Dispatch Macros

**Input**: Design documents from `/specs/030-typecase-macros/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Required per Constitution Principle VII (TDD Non-negotiable)

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, etc.)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/lib/macros.lisp` for implementation
- **Unit tests**: `tests/unit/typecase/`
- **Contract tests**: `tests/contract/`
- **Integration tests**: `tests/integration/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and test directory structure

- [x] T001 Create test directory structure at tests/unit/typecase/
- [x] T002 [P] Create tests/contract/ directory if not exists
- [x] T003 [P] Create tests/integration/ directory if not exists

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Helper functions used by ALL macro expanders

**CRITICAL**: All user stories depend on these helpers being complete first.

- [x] T004 Implement `type-specifier-to-predicate` helper function in src/clysm/lib/macros.lisp
- [x] T005 [P] Implement `construct-expected-type` helper function in src/clysm/lib/macros.lisp
- [x] T006 [P] Implement `validate-exhaustive-clauses` helper function (rejects otherwise/t) in src/clysm/lib/macros.lisp
- [x] T007 Add `install-typecase-macros` function stub in src/clysm/lib/macros.lisp
- [x] T008 Call `install-typecase-macros` from `install-standard-macros` in src/clysm/lib/macros.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Basic Type Dispatch with typecase (Priority: P1)

**Goal**: Implement typecase macro for type-based conditional dispatch

**Independent Test**: Evaluate typecase forms with different value types, verify correct branch executes

### Tests for User Story 1

> **TDD: Write tests FIRST, ensure they FAIL before implementation**

- [x] T009 [P] [US1] Write unit test for typecase basic dispatch in tests/unit/typecase/typecase-test.lisp
- [x] T010 [P] [US1] Write unit test for typecase with multiple type specifiers per clause in tests/unit/typecase/typecase-test.lisp
- [x] T011 [P] [US1] Write unit test for typecase with otherwise/t clauses in tests/unit/typecase/typecase-test.lisp
- [x] T012 [P] [US1] Write unit test for typecase returning NIL when no match in tests/unit/typecase/typecase-test.lisp
- [x] T013 [P] [US1] Write unit test for typecase single keyform evaluation in tests/unit/typecase/typecase-test.lisp
- [x] T014 [US1] Run tests and verify they FAIL (make-typecase-expander not found)

### Implementation for User Story 1

- [x] T015 [US1] Implement `make-typecase-expander` function in src/clysm/lib/macros.lisp
- [x] T016 [US1] Handle otherwise/t clause detection in make-typecase-expander
- [x] T017 [US1] Handle multiple type specifiers ((type1 type2) body) in make-typecase-expander
- [x] T018 [US1] Register typecase macro in `install-typecase-macros` in src/clysm/lib/macros.lisp
- [x] T019 [US1] Run tests and verify they PASS
- [x] T020 [P] [US1] Write contract test for typecase Wasm output validation in tests/contract/typecase-wasm-test.lisp
- [x] T021 [US1] Run `nix flake check` and verify typecase tests pass

**Checkpoint**: typecase macro fully functional and tested independently

---

## Phase 4: User Story 2 - Exhaustive Type Dispatch with etypecase (Priority: P1)

**Goal**: Implement etypecase macro with type-error signaling when no match

**Independent Test**: Evaluate etypecase with matching/non-matching types, verify type-error signaled correctly

### Tests for User Story 2

- [x] T022 [P] [US2] Write unit test for etypecase matching clause in tests/unit/typecase/etypecase-test.lisp
- [x] T023 [P] [US2] Write unit test for etypecase type-error when no match in tests/unit/typecase/etypecase-test.lisp
- [x] T024 [P] [US2] Write unit test for etypecase expected-type construction (or type1 type2) in tests/unit/typecase/etypecase-test.lisp
- [x] T025 [P] [US2] Write unit test for etypecase rejection of otherwise/t clauses in tests/unit/typecase/etypecase-test.lisp
- [x] T026 [US2] Run tests and verify they FAIL (make-etypecase-expander not found)

### Implementation for User Story 2

- [x] T027 [US2] Implement `make-etypecase-expander` function in src/clysm/lib/macros.lisp
- [x] T028 [US2] Add type-error signaling with datum and expected-type in make-etypecase-expander
- [x] T029 [US2] Use `construct-expected-type` helper for (or ...) construction
- [x] T030 [US2] Use `validate-exhaustive-clauses` to reject otherwise/t
- [x] T031 [US2] Register etypecase macro in `install-typecase-macros` in src/clysm/lib/macros.lisp
- [x] T032 [US2] Run tests and verify they PASS
- [x] T033 [P] [US2] Write contract test for etypecase Wasm output validation in tests/contract/typecase-wasm-test.lisp
- [x] T034 [US2] Run `nix flake check` and verify etypecase tests pass

**Checkpoint**: etypecase macro fully functional and tested independently

---

## Phase 5: User Story 4 - Type Assertion with check-type (Priority: P1)

**Goal**: Implement check-type macro with store-value restart

**Independent Test**: Call check-type with correct/incorrect types, verify store-value restart works

**Note**: Implementing US4 before US3 because check-type patterns are reused in ctypecase

### Tests for User Story 4

- [x] T035 [P] [US4] Write unit test for check-type success (returns NIL) in tests/unit/typecase/check-type-test.lisp
- [x] T036 [P] [US4] Write unit test for check-type type-error signaling in tests/unit/typecase/check-type-test.lisp
- [x] T037 [P] [US4] Write unit test for check-type store-value restart in tests/unit/typecase/check-type-test.lisp
- [x] T038 [P] [US4] Write unit test for check-type optional type-string in tests/unit/typecase/check-type-test.lisp
- [x] T039 [US4] Run tests and verify they FAIL (make-check-type-expander not found)

### Implementation for User Story 4

- [x] T040 [US4] Implement `make-check-type-expander` function in src/clysm/lib/macros.lisp
- [x] T041 [US4] Add loop structure for re-validation after store-value
- [x] T042 [US4] Add restart-case with store-value restart
- [x] T043 [US4] Handle optional type-string in error message
- [x] T044 [US4] Register check-type macro in `install-typecase-macros` in src/clysm/lib/macros.lisp
- [x] T045 [US4] Run tests and verify they PASS
- [x] T046 [P] [US4] Write contract test for check-type Wasm output validation in tests/contract/typecase-wasm-test.lisp
- [x] T047 [US4] Run `nix flake check` and verify check-type tests pass

**Checkpoint**: check-type macro fully functional and tested independently

---

## Phase 6: User Story 3 - Correctable Type Dispatch with ctypecase (Priority: P2)

**Goal**: Implement ctypecase macro combining typecase dispatch with store-value restart

**Independent Test**: Evaluate ctypecase with store-value restart, verify place update and re-dispatch

### Tests for User Story 3

- [x] T048 [P] [US3] Write unit test for ctypecase matching clause in tests/unit/typecase/ctypecase-test.lisp
- [x] T049 [P] [US3] Write unit test for ctypecase type-error with store-value restart in tests/unit/typecase/ctypecase-test.lisp
- [x] T050 [P] [US3] Write unit test for ctypecase place update and re-dispatch in tests/unit/typecase/ctypecase-test.lisp
- [x] T051 [P] [US3] Write unit test for ctypecase rejection of otherwise/t clauses in tests/unit/typecase/ctypecase-test.lisp
- [x] T052 [US3] Run tests and verify they FAIL (make-ctypecase-expander not found)

### Implementation for User Story 3

- [x] T053 [US3] Implement `make-ctypecase-expander` function in src/clysm/lib/macros.lisp
- [x] T054 [US3] Combine typecase clause expansion with check-type restart pattern
- [x] T055 [US3] Add loop structure for re-evaluation after store-value
- [x] T056 [US3] Use `validate-exhaustive-clauses` to reject otherwise/t
- [x] T057 [US3] Register ctypecase macro in `install-typecase-macros` in src/clysm/lib/macros.lisp
- [x] T058 [US3] Run tests and verify they PASS
- [x] T059 [P] [US3] Write contract test for ctypecase Wasm output validation in tests/contract/typecase-wasm-test.lisp
- [x] T060 [US3] Run `nix flake check` and verify ctypecase tests pass

**Checkpoint**: ctypecase macro fully functional and tested independently

---

## Phase 7: User Story 5 - Compound Type Specifiers (Priority: P2)

**Goal**: Support or/and/not/member/satisfies type specifiers

**Independent Test**: Evaluate typecase with compound specifiers, verify correct dispatch

### Tests for User Story 5

- [x] T061 [P] [US5] Write unit test for (or type1 type2) specifier in tests/unit/typecase/compound-types-test.lisp
- [x] T062 [P] [US5] Write unit test for (and type1 type2) specifier in tests/unit/typecase/compound-types-test.lisp
- [x] T063 [P] [US5] Write unit test for (not type) specifier in tests/unit/typecase/compound-types-test.lisp
- [x] T064 [P] [US5] Write unit test for (member item1 item2) specifier in tests/unit/typecase/compound-types-test.lisp
- [x] T065 [P] [US5] Write unit test for (satisfies predicate) specifier in tests/unit/typecase/compound-types-test.lisp
- [x] T066 [US5] Run tests and verify they FAIL or show gaps in type-specifier-to-predicate

### Implementation for User Story 5

- [x] T067 [US5] Extend `type-specifier-to-predicate` for (or ...) compound type in src/clysm/lib/macros.lisp
- [x] T068 [US5] Extend `type-specifier-to-predicate` for (and ...) compound type in src/clysm/lib/macros.lisp
- [x] T069 [US5] Extend `type-specifier-to-predicate` for (not type) compound type in src/clysm/lib/macros.lisp
- [x] T070 [US5] Extend `type-specifier-to-predicate` for (member ...) compound type in src/clysm/lib/macros.lisp
- [x] T071 [US5] Extend `type-specifier-to-predicate` for (satisfies pred) compound type in src/clysm/lib/macros.lisp
- [x] T072 [US5] Run tests and verify they PASS
- [x] T073 [P] [US5] Write contract test for compound types Wasm output validation in tests/contract/typecase-wasm-test.lisp
- [x] T074 [US5] Run `nix flake check` and verify compound type tests pass

**Checkpoint**: All compound type specifiers working across all macros

---

## Phase 8: Integration & Validation

**Purpose**: Verify all 892 compiler typecase usages work

- [x] T075 [P] Write integration test for ANSI typecase compliance in tests/integration/typecase-ansi-test.lisp
- [x] T076 [P] Write integration test for ANSI etypecase compliance in tests/integration/typecase-ansi-test.lisp
- [x] T077 [P] Write integration test for ANSI ctypecase compliance in tests/integration/typecase-ansi-test.lisp
- [x] T078 [P] Write integration test for ANSI check-type compliance in tests/integration/typecase-ansi-test.lisp
- [x] T079 Run compiler self-test to verify existing typecase usages work
- [x] T080 Run `nix flake check` for full validation
- [x] T081 Document any edge cases found in specs/030-typecase-macros/notes.md

**Checkpoint**: All 892 compiler typecase usages pass without modification

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Final cleanup and documentation

- [x] T082 [P] Export typecase macro symbols from clysm/lib/macros package
- [x] T083 [P] Add docstrings to all expander functions in src/clysm/lib/macros.lisp
- [x] T084 Run quickstart.md validation steps
- [x] T085 Final `nix flake check` verification
- [x] T086 Update CLAUDE.md with feature completion notes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1 (typecase): Can start after Foundational
  - US2 (etypecase): Can start after Foundational
  - US4 (check-type): Can start after Foundational
  - US3 (ctypecase): Best after US1 + US4 (reuses patterns)
  - US5 (compound types): Best after US1 (extends type-specifier-to-predicate)
- **Integration (Phase 8)**: Depends on all user stories complete
- **Polish (Phase 9)**: Depends on Integration

### User Story Dependencies

```
Foundation
    │
    ├── US1 (typecase) ──────────┬──────────────────────┐
    │                            │                      │
    ├── US2 (etypecase) ─────────┼───────────┐          │
    │                            │           │          │
    ├── US4 (check-type) ────────┤           │          │
    │                            │           │          │
    └────────┬───────────────────┘           │          │
             │                               │          │
             v                               v          v
         US3 (ctypecase)              US5 (compound)
             │                               │
             └───────────────────────────────┘
                            │
                            v
                     Integration
```

### Within Each User Story

1. Tests written FIRST and FAIL
2. Implementation
3. Tests PASS
4. Contract test (Wasm validation)
5. `nix flake check` verification

### Parallel Opportunities

**Foundational phase (after Setup)**:
- T004, T005, T006 can run in parallel (different helper functions)

**User Story tests (within each story)**:
- All tests marked [P] within a story can run in parallel

**Contract tests**:
- T020, T033, T046, T059, T073 can run in parallel after their stories complete

**Integration tests**:
- T075, T076, T077, T078 can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "[US1] Write unit test for typecase basic dispatch"
Task: "[US1] Write unit test for typecase with multiple type specifiers"
Task: "[US1] Write unit test for typecase with otherwise/t clauses"
Task: "[US1] Write unit test for typecase returning NIL when no match"
Task: "[US1] Write unit test for typecase single keyform evaluation"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1 (typecase)
4. **STOP and VALIDATE**: Run `nix flake check`
5. typecase is now usable

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add US1 (typecase) → Test → Validate (MVP!)
3. Add US2 (etypecase) → Test → Validate
4. Add US4 (check-type) → Test → Validate
5. Add US3 (ctypecase) → Test → Validate
6. Add US5 (compound) → Test → Validate
7. Integration + Polish → Complete

### Self-Hosting Milestone

After Phase 8 (Integration), run compiler self-compilation test.
If all 892 typecase usages pass → Self-hosting blocker removed!

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story is independently completable and testable
- TDD required: Write tests FIRST, ensure FAIL, then implement
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Run `nix flake check` frequently

---

## Summary

| Story | Priority | Tasks | Description |
|-------|----------|-------|-------------|
| US1 | P1 | 13 | typecase - Basic type dispatch |
| US2 | P1 | 13 | etypecase - Exhaustive dispatch with error |
| US4 | P1 | 13 | check-type - Type assertion with restart |
| US3 | P2 | 13 | ctypecase - Correctable dispatch |
| US5 | P2 | 14 | Compound type specifiers |
| Setup | - | 3 | Directory structure |
| Foundational | - | 5 | Helper functions |
| Integration | - | 7 | ANSI compliance & validation |
| Polish | - | 5 | Documentation & cleanup |

**Total Tasks**: 86

**Parallel Opportunities**: 42 tasks marked [P]

**MVP Scope**: Phases 1-3 (Setup + Foundational + US1 typecase) = 21 tasks
