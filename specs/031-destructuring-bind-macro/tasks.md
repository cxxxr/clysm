# Tasks: Destructuring-Bind Macro

**Input**: Design documents from `/specs/031-destructuring-bind-macro/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: REQUIRED per Constitution Principle VII (TDD Non-Negotiable)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4, US5)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Source: `src/clysm/lib/`
- Tests: `tests/unit/`, `tests/contract/`, `tests/integration/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and package configuration

- [x] T001 Export destructuring-bind symbols in src/clysm/package.lisp (add to clysm/lib/macros exports)
- [x] T002 [P] Create src/clysm/lib/destructuring.lisp with package header and initial defstruct stubs
- [x] T003 [P] Create tests/unit/destructuring-bind-test.lisp with package definition and rove imports

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data structures that ALL user stories depend on

**CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 Implement parsed-lambda-list struct in src/clysm/lib/destructuring.lisp (whole-var, required-params, optional-params, rest-var, key-params, allow-other-keys-p)
- [x] T005 [P] Implement param-spec struct in src/clysm/lib/destructuring.lisp (type: :variable | :nested, var, nested-list)
- [x] T006 [P] Implement optional-param-spec struct in src/clysm/lib/destructuring.lisp (param, default-form, supplied-p)
- [x] T007 [P] Implement key-param-spec struct in src/clysm/lib/destructuring.lisp (keyword, param, default-form, supplied-p)
- [x] T008 Implement parse-destructuring-lambda-list skeleton function in src/clysm/lib/destructuring.lisp (state machine per data-model.md)
- [x] T009 Implement generate-destructuring-code skeleton function in src/clysm/lib/destructuring.lisp
- [x] T010 Implement make-destructuring-bind-expander skeleton in src/clysm/lib/macros.lisp (wire parse + generate)
- [x] T011 Register destructuring-bind in install-standard-macros in src/clysm/lib/macros.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Destructuring with Required Parameters (Priority: P1)

**Goal**: Extract values from a list into variables with required parameters and nested patterns

**Independent Test**: `(destructuring-bind (a b c) '(1 2 3) (list a b c))` → `(1 2 3)`

### Tests for User Story 1 (TDD - Write FIRST, must FAIL)

- [x] T012 [P] [US1] Unit test: parse required params `(a b c)` in tests/unit/destructuring-bind-test.lisp
- [x] T013 [P] [US1] Unit test: parse nested params `((a b) c)` in tests/unit/destructuring-bind-test.lisp
- [x] T014 [P] [US1] Unit test: generate code for required params in tests/unit/destructuring-bind-test.lisp
- [x] T015 [P] [US1] Unit test: program-error on insufficient elements in tests/unit/destructuring-bind-test.lisp
- [x] T016 [P] [US1] Contract test: expansion validates with wasm-tools in tests/contract/destructuring-wasm-test.lisp
- [x] T017 [P] [US1] Integration test: basic destructuring ANSI compliance in tests/integration/destructuring-ansi-test.lisp

### Implementation for User Story 1

- [x] T018 [US1] Implement parse-required-params in src/clysm/lib/destructuring.lisp (handles symbols and nested lists)
- [x] T019 [US1] Implement recursive nested pattern detection in parse-destructuring-lambda-list in src/clysm/lib/destructuring.lisp
- [x] T020 [US1] Implement generate-required-bindings in src/clysm/lib/destructuring.lisp (car/cdr traversal)
- [x] T021 [US1] Implement nested code generation with gensym temporaries in src/clysm/lib/destructuring.lisp
- [x] T022 [US1] Implement program-error signaling for insufficient elements in src/clysm/lib/destructuring.lisp
- [x] T023 [US1] Implement excess element check (error when no &rest) in src/clysm/lib/destructuring.lisp
- [x] T024 [US1] Wire US1 logic into make-destructuring-bind-expander in src/clysm/lib/macros.lisp
- [x] T025 [US1] Run nix flake check and verify US1 tests pass

**Checkpoint**: User Story 1 complete - basic destructuring works independently

---

## Phase 4: User Story 2 - Optional and Rest Parameters (Priority: P2)

**Goal**: Handle `&optional` with defaults/supplied-p and `&rest` for variable-length lists

**Independent Test**: `(destructuring-bind (a &optional b &rest c) '(1) (list a b c))` → `(1 NIL NIL)`

### Tests for User Story 2 (TDD - Write FIRST, must FAIL)

- [x] T026 [P] [US2] Unit test: parse &optional params in tests/unit/destructuring-bind-test.lisp
- [x] T027 [P] [US2] Unit test: parse &optional with default in tests/unit/destructuring-bind-test.lisp
- [x] T028 [P] [US2] Unit test: parse &optional with supplied-p in tests/unit/destructuring-bind-test.lisp
- [x] T029 [P] [US2] Unit test: parse &rest param in tests/unit/destructuring-bind-test.lisp
- [x] T030 [P] [US2] Unit test: generate code for &optional with defaults in tests/unit/destructuring-bind-test.lisp
- [x] T031 [P] [US2] Integration test: &optional and &rest ANSI compliance in tests/integration/destructuring-ansi-test.lisp

### Implementation for User Story 2

- [x] T032 [US2] Implement parse-optional-params in src/clysm/lib/destructuring.lisp (var, (var default), (var default supplied-p))
- [x] T033 [US2] Implement parse-rest-param in src/clysm/lib/destructuring.lisp (symbol after &rest)
- [x] T034 [US2] Update state machine to handle &optional→&rest transitions in src/clysm/lib/destructuring.lisp
- [x] T035 [US2] Implement generate-optional-bindings in src/clysm/lib/destructuring.lisp (consp test, default form evaluation)
- [x] T036 [US2] Implement generate-rest-binding in src/clysm/lib/destructuring.lisp (direct cdr assignment)
- [x] T037 [US2] Implement supplied-p variable binding in src/clysm/lib/destructuring.lisp
- [x] T038 [US2] Ensure default forms only evaluated when needed in src/clysm/lib/destructuring.lisp
- [x] T039 [US2] Run nix flake check and verify US2 tests pass

**Checkpoint**: User Story 2 complete - &optional and &rest work independently

---

## Phase 5: User Story 3 - Keyword Parameters (Priority: P3)

**Goal**: Handle `&key` with defaults/supplied-p and `&allow-other-keys`

**Independent Test**: `(destructuring-bind (&key x y) '(:x 1 :y 2) (list x y))` → `(1 2)`

### Tests for User Story 3 (TDD - Write FIRST, must FAIL)

- [x] T040 [P] [US3] Unit test: parse &key params in tests/unit/destructuring-bind-test.lisp
- [x] T041 [P] [US3] Unit test: parse &key with alternate keyword name in tests/unit/destructuring-bind-test.lisp
- [x] T042 [P] [US3] Unit test: parse &allow-other-keys in tests/unit/destructuring-bind-test.lisp
- [x] T043 [P] [US3] Unit test: generate code for &key extraction in tests/unit/destructuring-bind-test.lisp
- [x] T044 [P] [US3] Unit test: error on unknown keyword without &allow-other-keys in tests/unit/destructuring-bind-test.lisp
- [x] T045 [P] [US3] Integration test: &key ANSI compliance in tests/integration/destructuring-ansi-test.lisp

### Implementation for User Story 3

- [x] T046 [US3] Implement parse-key-params in src/clysm/lib/destructuring.lisp (var, (var default), ((keyword var) default supplied-p))
- [x] T047 [US3] Implement parse-allow-other-keys in src/clysm/lib/destructuring.lisp
- [x] T048 [US3] Update state machine to handle &rest→&key and &key→&allow-other-keys transitions in src/clysm/lib/destructuring.lisp
- [x] T049 [US3] Implement generate-key-bindings in src/clysm/lib/destructuring.lisp (getf-like plist traversal)
- [x] T050 [US3] Implement unknown key validation in src/clysm/lib/destructuring.lisp
- [x] T051 [US3] Implement &allow-other-keys handling (skip validation) in src/clysm/lib/destructuring.lisp
- [x] T052 [US3] Handle duplicate keys (first occurrence wins per ANSI) in src/clysm/lib/destructuring.lisp
- [x] T053 [US3] Run nix flake check and verify US3 tests pass

**Checkpoint**: User Story 3 complete - &key and &allow-other-keys work independently

---

## Phase 6: User Story 4 - Whole and Body Parameters (Priority: P4)

**Goal**: Handle `&whole` and `&body` for macro-style destructuring

**Independent Test**: `(destructuring-bind (&whole w a b) '(1 2) (list w a b))` → `((1 2) 1 2)`

### Tests for User Story 4 (TDD - Write FIRST, must FAIL)

- [x] T054 [P] [US4] Unit test: parse &whole param in tests/unit/destructuring-bind-test.lisp
- [x] T055 [P] [US4] Unit test: parse &body as &rest synonym in tests/unit/destructuring-bind-test.lisp
- [x] T056 [P] [US4] Unit test: generate code for &whole binding in tests/unit/destructuring-bind-test.lisp
- [x] T057 [P] [US4] Integration test: &whole and &body ANSI compliance in tests/integration/destructuring-ansi-test.lisp

### Implementation for User Story 4

- [x] T058 [US4] Implement parse-whole-param in src/clysm/lib/destructuring.lisp (must be first in lambda-list)
- [x] T059 [US4] Implement &body as synonym for &rest in src/clysm/lib/destructuring.lisp
- [x] T060 [US4] Update state machine to handle START→&whole→required transitions in src/clysm/lib/destructuring.lisp
- [x] T061 [US4] Implement generate-whole-binding in src/clysm/lib/destructuring.lisp (bind before destructuring)
- [x] T062 [US4] Ensure nested &whole works correctly in src/clysm/lib/destructuring.lisp
- [x] T063 [US4] Run nix flake check and verify US4 tests pass

**Checkpoint**: User Story 4 complete - &whole and &body work independently

---

## Phase 7: User Story 5 - Compiler Self-Hosting Support (Priority: P5)

**Goal**: Validate all 9 compiler locations using destructuring-bind compile and execute correctly

**Independent Test**: Compile and run each of the 9 compiler locations

### Tests for User Story 5 (TDD - Write FIRST, must FAIL)

- [x] T064 [P] [US5] Integration test: ast.lisp defclass pattern `(defclass-sym name supers slots &rest options)` in tests/integration/destructuring-ansi-test.lisp
- [x] T065 [P] [US5] Integration test: compiler.lisp pattern `(name kind index)` in tests/integration/destructuring-ansi-test.lisp
- [x] T066 [P] [US5] Integration test: restarts.lisp &body pattern in tests/integration/destructuring-ansi-test.lisp
- [x] T067 [P] [US5] Integration test: restarts.lisp &key pattern in tests/integration/destructuring-ansi-test.lisp
- [x] T068 [P] [US5] Integration test: handlers.lisp patterns in tests/integration/destructuring-ansi-test.lisp

### Implementation for User Story 5

- [x] T069 [US5] Verify ast.lisp line 361 pattern compiles in self-hosting test
- [x] T070 [US5] Verify compiler.lisp line 543 pattern compiles in self-hosting test
- [x] T071 [US5] Verify compiler.lisp line 1235 pattern compiles in self-hosting test
- [x] T072 [US5] Verify macro.lisp line 401 pattern compiles in self-hosting test
- [x] T073 [US5] Verify restarts.lisp line 125 pattern compiles in self-hosting test
- [x] T074 [US5] Verify restarts.lisp line 217 pattern compiles in self-hosting test
- [x] T075 [US5] Verify handlers.lisp line 52 pattern compiles in self-hosting test
- [x] T076 [US5] Verify handlers.lisp line 99 pattern compiles in self-hosting test
- [x] T077 [US5] Verify sections.lisp line 338 pattern compiles in self-hosting test
- [x] T078 [US5] Run full self-hosting compilation test
- [x] T079 [US5] Run nix flake check and verify all US5 tests pass

**Checkpoint**: User Story 5 complete - all 9 compiler locations work, self-hosting validated

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [x] T080 [P] Add docstrings to all public functions in src/clysm/lib/destructuring.lisp
- [x] T081 [P] Test 5+ levels of nesting depth per SC-002 in tests/integration/destructuring-ansi-test.lisp
- [x] T082 [P] Test dotted-list patterns per FR-013 in tests/unit/destructuring-bind-test.lisp
- [x] T083 Code review: verify all error messages include parameter names
- [x] T084 Run complete test suite with nix flake check
- [x] T085 Validate against spec.md success criteria (SC-001 through SC-005)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1-US4 can proceed in parallel (independent features)
  - US5 depends on US1-US4 (validates all features together)
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **US1 (P1)**: Can start after Phase 2 - No dependencies on other stories
- **US2 (P2)**: Can start after Phase 2 - No dependencies on US1 (different lambda-list sections)
- **US3 (P3)**: Can start after Phase 2 - No dependencies on US1/US2 (different lambda-list section)
- **US4 (P4)**: Can start after Phase 2 - No dependencies on US1-US3 (different lambda-list section)
- **US5 (P5)**: MUST wait for US1-US4 - Integration validation of all features

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Parse functions before generate functions
- Core logic before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational struct tasks marked [P] can run in parallel (T005-T007)
- All tests for a user story marked [P] can run in parallel
- User Stories 1-4 can run in parallel (independent lambda-list sections)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together (TDD - must fail first):
Task: "Unit test: parse required params in tests/unit/destructuring-bind-test.lisp"
Task: "Unit test: parse nested params in tests/unit/destructuring-bind-test.lisp"
Task: "Unit test: generate code for required params in tests/unit/destructuring-bind-test.lisp"
Task: "Unit test: program-error on insufficient elements in tests/unit/destructuring-bind-test.lisp"
```

---

## Parallel Example: Foundational Structs

```bash
# Launch all struct definitions together:
Task: "Implement param-spec struct in src/clysm/lib/destructuring.lisp"
Task: "Implement optional-param-spec struct in src/clysm/lib/destructuring.lisp"
Task: "Implement key-param-spec struct in src/clysm/lib/destructuring.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Basic destructuring)
4. **STOP and VALIDATE**: Test US1 independently
5. This covers 5/9 compiler locations (required-only patterns)

### Self-Hosting Minimum (US1 + partial US2)

1. Complete US1 (required parameters)
2. Add &rest and &body from US2
3. This covers 8/9 compiler locations

### Full ANSI Compliance

1. Complete all user stories US1-US4
2. Validate with US5 (self-hosting)
3. Polish phase for edge cases

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- TDD required: Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Run `nix flake check` frequently to catch regressions
