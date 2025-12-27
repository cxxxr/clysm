# Tasks: FORMAT Function Foundation

**Input**: Design documents from `/specs/032-format-function/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: Required per Constitution (VII. TDD) - Red-Green-Refactor cycle mandatory.

**Organization**: Tasks grouped by user story priority. US1 and US2 are already implemented in existing `format.lisp`.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US3, US4, US5, US6)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/streams/`, `src/clysm/conditions/`
- **Tests**: `tests/unit/format/`, `tests/contract/`, `tests/integration/`

---

## Phase 1: Setup (Test Infrastructure) ✅

**Purpose**: Create test directory structure for FORMAT tests

- [x] T001 Create test directory structure `tests/unit/format/`
- [x] T002 [P] Create basic test file `tests/unit/format/basic-test.lisp` with package definition
- [x] T003 [P] Create iteration test file `tests/unit/format/iteration-test.lisp` with package definition
- [x] T004 [P] Create conditional test file `tests/unit/format/conditional-test.lisp` with package definition
- [x] T005 [P] Create recursive test file `tests/unit/format/recursive-test.lisp` with package definition
- [x] T006 Register test packages in `clysm.asd` test system

---

## Phase 2: Foundational (Blocking Prerequisites) ✅

**Purpose**: Core infrastructure required by ALL subsequent user stories

### Tests for Foundational

- [x] T007 [P] Write failing test for format-error condition in `tests/unit/format/basic-test.lisp`
- [x] T008 [P] Write failing test for column tracking in `tests/unit/format/basic-test.lisp`

### Implementation for Foundational

- [x] T009 Define `format-error` condition (subtype of simple-error) in `src/clysm/conditions/types.lisp`
- [x] T010 Export `format-error` and accessors from `src/clysm/conditions/package.lisp`
- [x] T011 Add column tracking slot to stream-related state in `src/clysm/streams/format.lisp`
- [x] T012 Update `write-char` wrapper to track column (0 after newline, else increment) in `src/clysm/streams/format.lisp`
- [x] T013 Replace `cl:error` calls with `format-error` signaling in `src/clysm/streams/format.lisp`
- [x] T014 Refactor `parse-format-string` to use `parse-directive` dispatcher in `src/clysm/streams/format.lisp`

**Checkpoint**: Foundation ready - format-error works, column tracking active, parser extensible

---

## Phase 3: User Story 3 - Newline and Whitespace Control (Priority: P2) ✅

**Goal**: Implement ~& (fresh-line) directive that outputs newline only if not at column 0

**Independent Test**: `(format nil "x~&y")` returns `"x\ny"` but `(format nil "~&y")` returns `"y"` (no leading newline if at column 0)

**Note**: ~%, ~~  already implemented; only ~& is new

### Tests for User Story 3

- [x] T015 [P] [US3] Write failing test for ~& fresh-line at column 0 in `tests/unit/format/basic-test.lisp`
- [x] T016 [P] [US3] Write failing test for ~& fresh-line at column > 0 in `tests/unit/format/basic-test.lisp`
- [x] T017 [P] [US3] Write failing test for ~& after ~% in `tests/unit/format/basic-test.lisp`

### Implementation for User Story 3

- [x] T018 [US3] Add `:fresh-line` case to `parse-directive` in `src/clysm/streams/format.lisp`
- [x] T019 [US3] Implement `format-fresh-line` handler that checks column before outputting newline in `src/clysm/streams/format.lisp`
- [x] T020 [US3] Add `:fresh-line` execution case in `format` function in `src/clysm/streams/format.lisp`
- [x] T021 [US3] Export `format-fresh-line` if needed from `src/clysm/streams/package.lisp`

**Checkpoint**: US3 complete - ~& works correctly with column tracking

---

## Phase 4: User Story 4 - List Iteration (Priority: P2) ✅

**Goal**: Implement ~{body~} for iterating over lists and ~^ for early exit

**Independent Test**: `(format nil "~{~A~^, ~}" '(a b c))` returns `"A, B, C"` (symbols print uppercase)

### Tests for User Story 4

- [x] T022 [P] [US4] Write failing test for ~{~A~} basic iteration in `tests/unit/format/iteration-test.lisp`
- [x] T023 [P] [US4] Write failing test for ~{~A~^, ~} with separator in `tests/unit/format/iteration-test.lisp`
- [x] T024 [P] [US4] Write failing test for empty list producing no output in `tests/unit/format/iteration-test.lisp`
- [x] T025 [P] [US4] Write failing test for nested iteration in `tests/unit/format/iteration-test.lisp`
- [x] T026 [P] [US4] Write failing test for malformed ~{ without ~} signaling format-error in `tests/unit/format/iteration-test.lisp`

### Implementation for User Story 4

- [x] T027 [US4] Define `iteration-directive` struct with body-directives and body-string fields in `src/clysm/streams/format.lisp`
- [x] T028 [US4] Define `escape-directive` struct for ~^ in `src/clysm/streams/format.lisp`
- [x] T029 [US4] Implement `parse-iteration-directive` that scans for matching ~} and recursively parses body in `src/clysm/streams/format.lisp`
- [x] T030 [US4] Add ~{ dispatch case in `parse-directive` in `src/clysm/streams/format.lisp`
- [x] T031 [US4] Add ~^ parsing as escape-directive in `parse-directive` in `src/clysm/streams/format.lisp`
- [x] T032 [US4] Implement `execute-iteration` that loops over list, binds elements, executes body in `src/clysm/streams/format.lisp`
- [x] T033 [US4] Implement ~^ handling that exits loop when no more elements remain in `src/clysm/streams/format.lisp`
- [x] T034 [US4] Add `:iteration` and `:escape` execution cases in `format` function in `src/clysm/streams/format.lisp`

**Checkpoint**: US4 complete - ~{~} and ~^ work for list iteration

---

## Phase 5: User Story 5 - Conditional Formatting (Priority: P3) ✅

**Goal**: Implement ~[clause~;clause~] for index-based selection and ~:[~;~] for boolean selection

**Independent Test**: `(format nil "~[zero~;one~;two~]" 1)` returns `"one"`

### Tests for User Story 5

- [x] T035 [P] [US5] Write failing test for ~[~;~] index selection in `tests/unit/format/conditional-test.lisp`
- [x] T036 [P] [US5] Write failing test for ~:; default clause in `tests/unit/format/conditional-test.lisp`
- [x] T037 [P] [US5] Write failing test for ~:[false~;true~] boolean form in `tests/unit/format/conditional-test.lisp`
- [x] T038 [P] [US5] Write failing test for out-of-range index with no default producing no output in `tests/unit/format/conditional-test.lisp`
- [x] T039 [P] [US5] Write failing test for malformed ~[ without ~] signaling format-error in `tests/unit/format/conditional-test.lisp`

### Implementation for User Story 5

- [x] T040 [US5] Define `conditional-directive` struct with clauses, boolean-p, default-index fields in `src/clysm/streams/format.lisp`
- [x] T041 [US5] Define `clause-info` struct with index, directives, default-p fields in `src/clysm/streams/format.lisp`
- [x] T042 [US5] Implement `parse-conditional-directive` that scans for ~], splits on ~;, detects ~:[ and ~:; in `src/clysm/streams/format.lisp`
- [x] T043 [US5] Add ~[ dispatch case in `parse-directive` in `src/clysm/streams/format.lisp`
- [x] T044 [US5] Implement `execute-conditional` that selects clause by index or boolean in `src/clysm/streams/format.lisp`
- [x] T045 [US5] Add `:conditional` execution case in `format` function in `src/clysm/streams/format.lisp`

**Checkpoint**: US5 complete - ~[~] and ~:[~] work for conditional selection

---

## Phase 6: User Story 6 - Recursive Processing (Priority: P3) ✅

**Goal**: Implement ~? for recursive format string processing

**Independent Test**: `(format nil "~?" "Value: ~A" '(42))` returns `"Value: 42"`

### Tests for User Story 6

- [x] T046 [P] [US6] Write failing test for ~? basic recursion in `tests/unit/format/recursive-test.lisp`
- [x] T047 [P] [US6] Write failing test for ~? with multiple arguments in `tests/unit/format/recursive-test.lisp`
- [x] T048 [P] [US6] Write failing test for ~? with non-string control signaling format-error in `tests/unit/format/recursive-test.lisp`
- [x] T049 [P] [US6] Write failing test for ~? with non-list args signaling format-error in `tests/unit/format/recursive-test.lisp`

### Implementation for User Story 6

- [x] T050 [US6] Define `recursive-directive` struct in `src/clysm/streams/format.lisp`
- [x] T051 [US6] Add ~? parsing case in `parse-directive` in `src/clysm/streams/format.lisp`
- [x] T052 [US6] Implement `execute-recursive` that consumes 2 args and calls format recursively in `src/clysm/streams/format.lisp`
- [x] T053 [US6] Add `:recursive` execution case in `format` function in `src/clysm/streams/format.lisp`

**Checkpoint**: US6 complete - ~? works for composable format strings

---

## Phase 7: Polish & Cross-Cutting Concerns ✅

**Purpose**: Integration testing, ANSI compliance, edge cases

### Integration Tests

- [x] T054 [P] Create contract test for Wasm validation in `tests/contract/format-wasm-test.lisp`
- [x] T055 [P] Create ANSI compliance test suite in `tests/integration/format-ansi-test.lisp`
- [x] T056 Write test for nested directives (iteration containing conditional) in `tests/integration/format-ansi-test.lisp`
- [x] T057 Write test for edge cases (insufficient args, malformed strings) in `tests/integration/format-ansi-test.lisp`

### Self-Hosting Validation

- [x] T058 Audit all 114 FORMAT call sites in compiler for directive usage in `src/clysm/`
- [x] T059 Create smoke test that compiles a sample file using self-hosted FORMAT in `tests/integration/format-self-host-test.lisp`

### Documentation & Cleanup

- [x] T060 [P] Update exports in `src/clysm/streams/package.lisp` for all new symbols
- [x] T061 [P] Add docstrings to all new functions in `src/clysm/streams/format.lisp`
- [x] T062 Run `nix flake check` to validate full test suite passes
- [x] T063 Run quickstart.md validation examples

---

## Summary

**Completed Phases**: 1, 2, 3, 4, 5, 6, 7 (63 tasks)
**Remaining Phase**: None - ALL COMPLETE

**Implementation Status**:
- ~A, ~S, ~D, ~%, ~~ : ✅ Implemented (US1, US2)
- ~& (fresh-line) : ✅ Implemented (US3)
- ~{~} (iteration) with ~^ (escape) : ✅ Implemented (US4)
- ~[~] (conditional) with ~:[ (boolean) and ~:; (default) : ✅ Implemented (US5)
- ~? (recursive) : ✅ Implemented (US6)

**Test Coverage**:
- Unit tests: 22 tests (basic, iteration, conditional, recursive)
- Contract tests: 10 tests (Wasm parsing validation)
- Integration tests: 20+ tests (ANSI compliance, self-hosting)

**Directive Usage Audit** (114 call sites):
- ~A: 52 uses, ~%: 43 uses, ~D: 22 uses, ~S: 8 uses, ~&: 4 uses
- ~{~^~}: 1 use (ansi-test/conditions.lisp)

All format tests pass. Feature complete for self-hosting.
