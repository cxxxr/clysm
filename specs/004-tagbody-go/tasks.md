# Tasks: Tagbody/Go Control Flow

**Input**: Design documents from `/specs/004-tagbody-go/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md

**Tests**: REQUIRED per Constitution VII (TDD non-negotiable). Tests must be written first and fail before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Main implementation: `src/clysm/compiler/codegen/func-section.lisp`
- Unit tests: `tests/unit/tagbody-test.lisp`
- Integration tests: `tests/integration/control-flow-test.lisp`

---

## Phase 1: Setup

**Purpose**: No additional setup required - using existing project structure

- [x] T001 Verify existing test infrastructure works by running `rove clysm.asd`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data structures and analysis functions that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Data Structures

- [x] T002 Define `tagbody-context` struct in src/clysm/compiler/codegen/func-section.lisp with fields: strategy, tags, pc-local, dispatch-depth, loop-label, num-segments
- [x] T003 Add `tagbody-context` slot to `compilation-env` struct in src/clysm/compiler/codegen/func-section.lisp
- [x] T004 Update `copy-compilation-env` function to properly handle tagbody-context for nested tagbody support in src/clysm/compiler/codegen/func-section.lisp

### Strategy Analysis Functions

- [x] T005 [P] Write unit test for `collect-go-targets` function in tests/unit/tagbody-test.lisp
- [x] T006 [P] Write unit test for `all-goes-are-backward-p` function in tests/unit/tagbody-test.lisp
- [x] T007 [P] Write unit test for `analyze-tagbody-strategy` function covering all three strategies in tests/unit/tagbody-test.lisp
- [x] T008 Implement `collect-go-targets` function to extract all go target tags from segments in src/clysm/compiler/codegen/func-section.lisp
- [x] T009 Implement `all-goes-are-backward-p` function to check if all goes to a tag are backward jumps in src/clysm/compiler/codegen/func-section.lisp
- [x] T010 Implement `analyze-tagbody-strategy` function returning :sequential, :simple-loop, or :dispatch in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Foundation ready - run tests to verify strategy analysis works correctly

---

## Phase 3: User Story 4 - Sequential Execution (Priority: P4)

**Goal**: Handle tagbody without go statements - simplest strategy, good for initial validation

**Independent Test**: Compile tagbody with tags but no go, verify sequential execution and NIL return

**Note**: Implementing P4 first because :sequential strategy is simplest and validates basic infrastructure

### Tests for User Story 4 (TDD - Write First)

- [x] T011 [P] [US4] Write integration test `tagbody-no-tags` in tests/integration/control-flow-test.lisp - tagbody with just forms, no tags
- [x] T012 [P] [US4] Write integration test `tagbody-sequential` in tests/integration/control-flow-test.lisp - tagbody with tags but no go

### Implementation for User Story 4

- [x] T013 [US4] Implement `compile-tagbody-sequential` function in src/clysm/compiler/codegen/func-section.lisp - compile forms sequentially, return NIL
- [x] T014 [US4] Run tests to verify :sequential strategy works

**Checkpoint**: User Story 4 should be fully functional - sequential tagbody works

---

## Phase 4: User Story 1 - Simple Loop Iteration (Priority: P1) 🎯 MVP

**Goal**: Efficient loop compilation using Wasm `loop` instruction without br_table overhead

**Independent Test**: Compile counting loop, verify correct iteration count and no stack overflow

### Tests for User Story 1 (TDD - Write First)

- [x] T015 [P] [US1] Write integration test `tagbody-simple-loop` in tests/integration/control-flow-test.lisp - count to 5 with single backward go
- [x] T016 [P] [US1] Write integration test `tagbody-deep-loop` in tests/integration/control-flow-test.lisp - 10,000 iterations without stack overflow

### Implementation for User Story 1

- [x] T017 [US1] Implement `compile-tagbody-simple-loop` function in src/clysm/compiler/codegen/func-section.lisp - generate Wasm loop instruction
- [x] T018 [US1] Implement `compile-go-simple` function in src/clysm/compiler/codegen/func-section.lisp - generate br to loop start
- [x] T019 [US1] Run tests to verify :simple-loop strategy works with 10,000 iterations

**Checkpoint**: User Story 1 (MVP) complete - simple loops work efficiently

---

## Phase 5: User Story 2 - Forward Jump (Priority: P2)

**Goal**: Support go statements that skip over code (forward jumps)

**Independent Test**: Compile code with forward go, verify skipped code does not execute

**Note**: Forward jumps require :dispatch strategy

### Tests for User Story 2 (TDD - Write First)

- [x] T020 [P] [US2] Write integration test `tagbody-forward-jump` in tests/integration/control-flow-test.lisp - skip over assignment with go
- [x] T021 [P] [US2] Write integration test `tagbody-multi-forward` in tests/integration/control-flow-test.lisp - multiple consecutive forward jumps

### Implementation for User Story 2

- [x] T022 [US2] Implement `compile-tagbody-dispatch` function skeleton in src/clysm/compiler/codegen/func-section.lisp - generate block/loop/br_table structure
- [x] T023 [US2] Implement nested block generation for dispatch strategy in src/clysm/compiler/codegen/func-section.lisp
- [x] T024 [US2] Implement br_table index mapping using formula: depth = num_segments - segment_index in src/clysm/compiler/codegen/func-section.lisp
- [x] T025 [US2] Implement `compile-tagbody-segment` function in src/clysm/compiler/codegen/func-section.lisp - compile forms within segment, stop after go
- [x] T026 [US2] Implement `compile-go-dispatch` function in src/clysm/compiler/codegen/func-section.lisp - set $pc and br to dispatch loop
- [x] T027 [US2] Add $pc local variable allocation for dispatch strategy in src/clysm/compiler/codegen/func-section.lisp
- [x] T028 [US2] Run tests to verify forward jumps work correctly

**Checkpoint**: User Story 2 complete - forward jump patterns work

---

## Phase 6: User Story 3 - Complex Multi-directional Jumps (Priority: P3)

**Goal**: Support complex patterns with both forward and backward jumps between multiple tags

**Independent Test**: Compile state-machine-like code, verify correct tag transitions

### Tests for User Story 3 (TDD - Write First)

- [x] T029 [P] [US3] Write integration test `tagbody-complex-jump` in tests/integration/control-flow-test.lisp - alternating forward/backward jumps
- [x] T030 [P] [US3] Write integration test `tagbody-fallthrough` in tests/integration/control-flow-test.lisp - verify fallthrough between tags without go
- [x] T031 [P] [US3] Write integration test `tagbody-three-tags` in tests/integration/control-flow-test.lisp - three or more tags with complex jumps

### Implementation for User Story 3

- [x] T032 [US3] Extend `compile-tagbody-dispatch` to handle fallthrough between segments in src/clysm/compiler/codegen/func-section.lisp
- [x] T033 [US3] Add br $exit at end of final segment for normal completion in src/clysm/compiler/codegen/func-section.lisp
- [x] T034 [US3] Run tests to verify complex jump patterns work correctly

**Checkpoint**: User Story 3 complete - complex state machine patterns work

---

## Phase 7: Integration & Entry Points

**Purpose**: Wire up main compile-tagbody and compile-go to use strategy-based dispatch

### Tests for Integration

- [x] T035 [P] Write integration test for compile-time error on undefined tag in tests/integration/control-flow-test.lisp
- [x] T036 [P] Write integration test for compile-time error on go outside tagbody in tests/integration/control-flow-test.lisp
- [x] T037 [P] Write integration test for nested tagbody with independent scopes in tests/integration/control-flow-test.lisp

### Implementation

- [x] T038 Refactor `compile-tagbody` to dispatch based on analyze-tagbody-strategy result in src/clysm/compiler/codegen/func-section.lisp
- [x] T039 Refactor `compile-go` to check tagbody-context and dispatch to appropriate strategy in src/clysm/compiler/codegen/func-section.lisp
- [x] T040 Add compile-time error for undefined tag in `compile-go` in src/clysm/compiler/codegen/func-section.lisp
- [x] T041 Add compile-time error for go outside tagbody in `compile-go` in src/clysm/compiler/codegen/func-section.lisp
- [x] T042 Handle nested tagbody by properly managing tagbody-context in compilation-env in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: All strategies integrated - full tagbody/go support working

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Verification, optimization, and documentation

- [x] T043 [P] Verify generated WAT for simple-loop has no br_table (SC-005) by manual inspection
- [x] T044 [P] Verify all error messages are clear and actionable (SC-006)
- [x] T045 Run full test suite via `nix flake check` to ensure Nix-First compliance
- [x] T046 Code cleanup: Remove any debug code, ensure consistent formatting in src/clysm/compiler/codegen/func-section.lisp

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 4 (Phase 3)**: Depends on Foundational - simplest strategy, validates infrastructure
- **User Story 1 (Phase 4)**: Depends on Foundational - MVP, most common pattern
- **User Story 2 (Phase 5)**: Depends on Foundational - introduces dispatch strategy
- **User Story 3 (Phase 6)**: Depends on User Story 2 (reuses dispatch implementation)
- **Integration (Phase 7)**: Depends on all user stories
- **Polish (Phase 8)**: Depends on Integration

### User Story Dependencies

| Story | Can Start After | Dependencies |
|-------|-----------------|--------------|
| US4 (Sequential) | Phase 2 | None - independent |
| US1 (Simple Loop) | Phase 2 | None - independent |
| US2 (Forward Jump) | Phase 2 | None - introduces dispatch |
| US3 (Complex) | US2 | Extends dispatch implementation |

### Within Each User Story (TDD Order)

1. Write tests FIRST - must FAIL
2. Implement minimum code to pass
3. Refactor if needed
4. Verify tests pass
5. Move to next task

### Parallel Opportunities

**Phase 2 (Foundational)**:
```bash
# These can run in parallel:
T005, T006, T007  # Unit tests for analysis functions
```

**Phase 3-6 (User Stories)**:
```bash
# Tests within each story can run in parallel:
# US4: T011, T012
# US1: T015, T016
# US2: T020, T021
# US3: T029, T030, T031
```

**Phase 7 (Integration)**:
```bash
# Integration tests can run in parallel:
T035, T036, T037
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001)
2. Complete Phase 2: Foundational (T002-T010)
3. Complete Phase 3: User Story 4 (T011-T014) - validates basic infrastructure
4. Complete Phase 4: User Story 1 (T015-T019) - **MVP DONE**
5. **STOP and VALIDATE**: Test simple loops work correctly
6. Demo/deploy if ready

### Incremental Delivery

| Milestone | Phases | Capability |
|-----------|--------|------------|
| Foundation | 1-2 | Strategy analysis working |
| MVP | 1-4 | Simple loops (most common pattern) |
| Forward Jumps | 1-5 | Skip patterns working |
| Full Feature | 1-7 | All tagbody/go patterns |
| Production | 1-8 | Verified and polished |

---

## Notes

- Constitution requires TDD - all tests must be written and fail before implementation
- rove is the testing framework - use `(deftest ...)` syntax
- Main implementation file: `src/clysm/compiler/codegen/func-section.lisp`
- Commit after each logical group of tasks
- Verify `nix flake check` passes before marking phase complete
