# Tasks: I/O Print Primitives

**Input**: Design documents from `/specs/001-io-print-primitives/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: TDD required per Constitution VII. Tests are written before implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

Single project: `src/clysm/`, `tests/` at repository root

---

## Phase 1: Setup

**Purpose**: Project initialization and test infrastructure

**Prerequisites**: Run `nix develop` before starting (Constitution VIII)

- [ ] T001 Create test directories for I/O print primitives: `tests/unit/io/`, `tests/contract/io/`, `tests/integration/io/`
- [ ] T002 [P] Verify existing FFI declarations available in `src/clysm/streams/ffi-io.lisp` (%host-write-char, %host-write-string)
- [ ] T003 [P] Verify existing format implementation in `src/clysm/streams/format.lisp` (parse-format-string, princ-to-string, prin1-to-string)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core compiler infrastructure that MUST be complete before ANY user story

**CRITICAL**: No user story work can begin until this phase is complete

- [ ] T004 Add print/prin1/princ/terpri/write to primitive member list in `src/clysm/compiler/codegen/func-section.lisp` (around line 846)
- [ ] T005 Create shared helper `compile-stream-fd-for-output` in `src/clysm/compiler/codegen/func-section.lisp` to handle stream destination dispatch (nil/t/stream)
- [ ] T006 Create shared helper `compile-object-to-string` in `src/clysm/compiler/codegen/func-section.lisp` to convert object to string with escape flag

**Checkpoint**: Foundation ready - print primitives recognized by compiler

---

## Phase 3: User Story 1 - Basic Print Output Compilation (Priority: P1) MVP

**Goal**: Compile [print](resources/HyperSpec/Body/f_wr_pr.htm), [prin1](resources/HyperSpec/Body/f_wr_pr.htm), [princ](resources/HyperSpec/Body/f_wr_pr.htm), [terpri](resources/HyperSpec/Body/f_terpri.htm) to valid Wasm

**Independent Test**: Compile `(defun greet (x) (print x) x)` and verify Wasm validates

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T007 [P] [US1] Unit test for terpri compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-terpri)
- [ ] T008 [P] [US1] Unit test for princ compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-princ)
- [ ] T009 [P] [US1] Unit test for prin1 compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-prin1)
- [ ] T010 [P] [US1] Unit test for print compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-print)
- [ ] T011 [P] [US1] Contract test for print Wasm imports in `tests/contract/io/print-wasm-test.lisp` (test-print-imports-clysm-io)

### Implementation for User Story 1

- [ ] T012 [US1] Implement `compile-terpri` handler in `src/clysm/compiler/codegen/func-section.lisp` - write newline via FFI, return NIL
- [ ] T013 [US1] Implement `compile-princ` handler in `src/clysm/compiler/codegen/func-section.lisp` - call princ-to-string, write via FFI, return object
- [ ] T014 [US1] Implement `compile-prin1` handler in `src/clysm/compiler/codegen/func-section.lisp` - call prin1-to-string, write via FFI, return object
- [ ] T015 [US1] Implement `compile-print` handler in `src/clysm/compiler/codegen/func-section.lisp` - newline, prin1, space, return object
- [ ] T016 [US1] Add dispatch cases for print/prin1/princ/terpri in `compile-primitive-call` (around line 915) in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T017 [US1] Verify all US1 unit tests pass
- [ ] T018 [US1] Run `wasm-tools validate` on compiled `(defun greet (x) (print x) x)`

**Checkpoint**: Print primitives compile and validate. DEFUN with print no longer fails.

---

## Phase 4: User Story 2 - Format String Compilation (Priority: P1)

**Goal**: Compile [format](resources/HyperSpec/Body/f_format.htm) with directives ~A, ~S, ~D, ~%, ~&, ~~ to valid Wasm

**Independent Test**: Compile `(format nil "~A" 42)` and verify execution returns "42"

### Tests for User Story 2

- [ ] T019 [P] [US2] Unit test for format ~A compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-format-aesthetic)
- [ ] T020 [P] [US2] Unit test for format ~S compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-format-standard)
- [ ] T021 [P] [US2] Unit test for format ~D compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-format-decimal)
- [ ] T022 [P] [US2] Unit test for format ~% ~& ~~ compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-format-misc-directives)
- [ ] T023 [P] [US2] Contract test for format nil Wasm structure in `tests/contract/io/print-wasm-test.lisp` (test-format-nil-no-ffi-io)
- [ ] T024 [P] [US2] Integration test `(format nil "~A" 42) => "42"` in `tests/integration/io/print-self-host-test.lisp`

### Implementation for User Story 2

- [ ] T025 [US2] Create `compile-format-directive` dispatcher in `src/clysm/compiler/codegen/func-section.lisp` - dispatch on directive type
- [ ] T026 [US2] Implement `compile-format-aesthetic` for ~A directive in `src/clysm/compiler/codegen/func-section.lisp` - call princ-to-string
- [ ] T027 [US2] Implement `compile-format-standard` for ~S directive in `src/clysm/compiler/codegen/func-section.lisp` - call prin1-to-string
- [ ] T028 [US2] Implement `compile-format-decimal` for ~D directive in `src/clysm/compiler/codegen/func-section.lisp` - call write-to-string with base 10
- [ ] T029 [US2] Implement `compile-format-newline` for ~% directive in `src/clysm/compiler/codegen/func-section.lisp` - insert char code 10
- [ ] T030 [US2] Implement `compile-format-fresh-line` for ~& directive in `src/clysm/compiler/codegen/func-section.lisp` - conditional newline
- [ ] T031 [US2] Implement `compile-format-tilde` for ~~ directive in `src/clysm/compiler/codegen/func-section.lisp` - insert literal tilde
- [ ] T032 [US2] Implement `compile-format` main handler in `src/clysm/compiler/codegen/func-section.lisp` - parse string, dispatch directives, build result
- [ ] T033 [US2] Add dispatch case for format in `compile-primitive-call` in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T034 [US2] Verify all US2 unit tests pass
- [ ] T035 [US2] Verify integration test `(format nil "~A" 42) => "42"` passes

**Checkpoint**: Format with nil destination compiles. SC-002 satisfied.

---

## Phase 5: User Story 3 - Write Function Compilation (Priority: P2)

**Goal**: Compile [write](resources/HyperSpec/Body/f_wr_pr.htm) with minimal keyword support (:stream, :escape)

**Independent Test**: Compile `(write obj)` and verify Wasm validates

### Tests for User Story 3

- [ ] T036 [P] [US3] Unit test for write compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-write)
- [ ] T037 [P] [US3] Unit test for write with :escape keyword in `tests/unit/io/print-primitives-test.lisp` (test-compile-write-escape)
- [ ] T038 [P] [US3] Contract test for write Wasm structure in `tests/contract/io/print-wasm-test.lisp` (test-write-returns-object)

### Implementation for User Story 3

- [ ] T039 [US3] Implement `compile-write` handler in `src/clysm/compiler/codegen/func-section.lisp` - parse :stream/:escape kwargs, generate appropriate output
- [ ] T040 [US3] Add dispatch case for write in `compile-primitive-call` in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T041 [US3] Verify all US3 unit tests pass

**Checkpoint**: Write function compiles. FR-012 satisfied.

---

## Phase 6: User Story 4 - Format to Stream Output (Priority: P3)

**Goal**: Compile `(format t ...)` to output via FFI to stdout

**Independent Test**: Compile `(format t "Hello~%")` and verify stdout output when executed

### Tests for User Story 4

- [ ] T042 [P] [US4] Unit test for format t compilation in `tests/unit/io/print-primitives-test.lisp` (test-compile-format-t)
- [ ] T043 [P] [US4] Contract test for format t Wasm imports in `tests/contract/io/print-wasm-test.lisp` (test-format-t-uses-ffi-io)
- [ ] T044 [P] [US4] Integration test for format t output in `tests/integration/io/print-self-host-test.lisp` (test-format-t-stdout)

### Implementation for User Story 4

- [ ] T045 [US4] Extend `compile-format` to handle t destination in `src/clysm/compiler/codegen/func-section.lisp` - use FFI write functions
- [ ] T046 [US4] Verify format t returns NIL (per ANSI spec) in `src/clysm/compiler/codegen/func-section.lisp`
- [ ] T047 [US4] Verify all US4 tests pass

**Checkpoint**: Format to stdout works. FR-014 satisfied.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Validation, documentation, and cleanup

- [ ] T048 [P] Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"`
- [ ] T049 [P] Run Stage 1 generation to measure compilation rate: `sbcl --load build/stage1-complete.lisp`
- [ ] T050 Verify no regression in existing tests (SC-006)
- [ ] T051 [P] Run wasm-tools validate on Stage 1 output: `wasm-tools validate dist/clysm-stage1.wasm`
- [ ] T052 Check `dist/stage1-report.json` for compilation rate toward 55% target (SC-004)
- [ ] T053 Update CLAUDE.md Recent Changes section with feature completion

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 and US2 are both P1 priority - implement sequentially or parallel
  - US3 (P2) can start after Foundation, independent of US1/US2
  - US4 (P3) depends on US2 (format compilation infrastructure)
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **US1 (P1)**: Foundation only - no story dependencies
- **US2 (P1)**: Foundation only - no story dependencies
- **US3 (P2)**: Foundation only - no story dependencies
- **US4 (P3)**: Depends on US2 (reuses format compilation infrastructure)

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD per Constitution VII)
- Helper functions before main handlers
- Main handlers before dispatch integration
- All tests must pass before checkpoint

### Parallel Opportunities

**Phase 1**: T002, T003 can run in parallel
**Phase 3 Tests**: T007, T008, T009, T010, T011 can all run in parallel
**Phase 4 Tests**: T019-T024 can all run in parallel
**Phase 5 Tests**: T036, T037, T038 can all run in parallel
**Phase 6 Tests**: T042, T043, T044 can all run in parallel
**Phase 7**: T048, T049, T051 can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for US1 together (TDD - write first):
Task: "Unit test for terpri compilation in tests/unit/io/print-primitives-test.lisp"
Task: "Unit test for princ compilation in tests/unit/io/print-primitives-test.lisp"
Task: "Unit test for prin1 compilation in tests/unit/io/print-primitives-test.lisp"
Task: "Unit test for print compilation in tests/unit/io/print-primitives-test.lisp"
Task: "Contract test for print Wasm imports in tests/contract/io/print-wasm-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL)
3. Complete Phase 3: User Story 1 (print/prin1/princ/terpri)
4. **STOP and VALIDATE**: DEFUN with print compiles, Wasm validates
5. Measure impact on compilation rate

### Incremental Delivery

1. Setup + Foundational → Primitives recognized
2. Add US1 (print functions) → Test → SC-001 satisfied (print compiles)
3. Add US2 (format nil) → Test → SC-002 satisfied (format nil works)
4. Add US3 (write) → Test → FR-012 satisfied
5. Add US4 (format t) → Test → FR-014 satisfied
6. Polish → SC-004 measured (compilation rate)

### Parallel Team Strategy

With multiple developers:
1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: US1 (print primitives)
   - Developer B: US2 (format nil)
3. After US2: Developer B continues with US4 (format t)
4. US3 (write) can be done by either developer anytime after Foundation

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story independently testable
- Verify tests fail before implementing (TDD)
- Commit after each task or logical group
- Main implementation file: `src/clysm/compiler/codegen/func-section.lisp`
- ANSI CL compliance: HyperSpec links in comments required (Constitution IX)
