# Tasks: FFI-based Stream I/O

**Input**: Design documents from `/specs/015-ffi-stream-io/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: TDD is REQUIRED per constitution (Principle VII). Tests must be written first and fail before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/` at repository root
- **Tests**: `tests/` at repository root
- **Host Shim**: `host-shim/` at repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization, module structure, and host shim setup

- [x] T001 Create streams package definition in src/clysm/streams/package.lisp
- [x] T002 [P] Create host-shim directory structure with host-shim/README.md
- [x] T003 [P] Create tests/streams/ directory structure
- [x] T004 Update ASDF system definition to include streams module in clysm.asd

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### WasmGC Stream Type

- [x] T005 Add +type-stream+ constant (index 19) in src/clysm/compiler/codegen/gc-types.lisp
- [x] T006 Implement make-stream-type constructor in src/clysm/compiler/codegen/gc-types.lisp
- [x] T007 Register $stream type in generate-type-definitions in src/clysm/compiler/codegen/gc-types.lisp
- [x] T008 Add :stream-ref to emit-value-type-extended in src/clysm/compiler/codegen/gc-types.lisp

### Stream Conditions

- [x] T009 [P] Define stream-error condition class in src/clysm/conditions/types.lisp
- [x] T010 [P] Define end-of-file condition class in src/clysm/conditions/types.lisp

### FFI Host Imports

- [x] T011 Define %host-write-char FFI import in src/clysm/streams/ffi-io.lisp
- [x] T012 Define %host-write-string FFI import in src/clysm/streams/ffi-io.lisp
- [x] T013 [P] Define %host-read-char FFI import in src/clysm/streams/ffi-io.lisp
- [x] T014 [P] Define %host-read-line FFI import in src/clysm/streams/ffi-io.lisp

### Stream Type Infrastructure

- [x] T015 Define stream-direction type and constants in src/clysm/streams/types.lisp
- [x] T016 Implement make-stream constructor in src/clysm/streams/types.lisp
- [x] T017 Implement stream-fd accessor in src/clysm/streams/types.lisp
- [x] T018 Implement stream-direction accessor in src/clysm/streams/types.lisp

### Host Shim Foundation

- [x] T019 Implement write-char host function in host-shim/io-shim.js
- [x] T020 [P] Implement write-string host function in host-shim/io-shim.js
- [x] T021 [P] Implement read-char host function in host-shim/io-shim.js
- [x] T022 [P] Implement read-line host function in host-shim/io-shim.js
- [x] T023 Create module loader/runner script in host-shim/run-wasm.js

### Standard Stream Initialization

- [x] T024 Define *standard-input* special variable in src/clysm/streams/types.lisp
- [x] T025 [P] Define *standard-output* special variable in src/clysm/streams/types.lisp
- [x] T026 [P] Define *error-output* special variable in src/clysm/streams/types.lisp
- [x] T027 Implement initialize-standard-streams function in src/clysm/streams/types.lisp
- [x] T028 Call initialize-standard-streams from module start in src/clysm/streams/types.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic Character Output (Priority: P1) 🎯 MVP

**Goal**: Implement write-char and write-string that output to standard streams via FFI

**Independent Test**: Compile and run a program that writes text to stdout/stderr and verify output appears in host environment

### Tests for User Story 1 (TDD Required)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T029 [P] [US1] Unit test for write-char basic output in tests/unit/stream-write-test.lisp
- [x] T030 [P] [US1] Unit test for write-char with stream argument in tests/unit/stream-write-test.lisp
- [x] T031 [P] [US1] Unit test for write-string basic output in tests/unit/stream-write-test.lisp
- [x] T032 [P] [US1] Unit test for write-string :start/:end in tests/unit/stream-write-test.lisp
- [x] T033 [P] [US1] Unit test for write-char type error (non-character) in tests/unit/stream-write-test.lisp
- [x] T034 [P] [US1] Unit test for write-string type error (non-string) in tests/unit/stream-write-test.lisp
- [x] T035 [US1] Integration test with wasmtime for stdout in tests/streams/integration-test.lisp
- [x] T036 [US1] Integration test with wasmtime for stderr in tests/streams/integration-test.lisp

### Implementation for User Story 1

- [x] T037 [US1] Implement write-char function in src/clysm/streams/write.lisp
- [x] T038 [US1] Implement write-string function in src/clysm/streams/write.lisp
- [x] T039 [US1] Add type checking with type-error signaling in src/clysm/streams/write.lisp
- [x] T040 [US1] Add output-stream-p validation in src/clysm/streams/write.lisp
- [x] T041 [US1] Export write-char, write-string from streams package in src/clysm/streams/package.lisp

**Checkpoint**: write-char and write-string work with stdout/stderr. MVP complete.

---

## Phase 4: User Story 2 - Basic Character Input (Priority: P2)

**Goal**: Implement read-char and read-line that read from standard input via FFI with proper EOF handling

**Independent Test**: Compile and run a program that reads input and verify it receives text from host environment

### Tests for User Story 2 (TDD Required)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T042 [P] [US2] Unit test for read-char basic input in tests/unit/stream-read-test.lisp
- [x] T043 [P] [US2] Unit test for read-char EOF with eof-error-p=t in tests/unit/stream-read-test.lisp
- [x] T044 [P] [US2] Unit test for read-char EOF with eof-error-p=nil in tests/unit/stream-read-test.lisp
- [x] T045 [P] [US2] Unit test for read-line basic input in tests/unit/stream-read-test.lisp
- [x] T046 [P] [US2] Unit test for read-line EOF handling in tests/unit/stream-read-test.lisp
- [x] T047 [P] [US2] Unit test for read-line missing-newline-p return value in tests/unit/stream-read-test.lisp
- [x] T048 [US2] Integration test with wasmtime stdin in tests/streams/integration-test.lisp

### Implementation for User Story 2

- [x] T049 [US2] Implement read-char function with eof-error-p/eof-value in src/clysm/streams/read.lisp
- [x] T050 [US2] Implement read-line function with multiple values in src/clysm/streams/read.lisp
- [x] T051 [US2] Handle EOF from host (-1 for char, null for line) in src/clysm/streams/read.lisp
- [x] T052 [US2] Signal end-of-file condition when appropriate in src/clysm/streams/read.lisp
- [x] T053 [US2] Add input-stream-p validation in src/clysm/streams/read.lisp
- [x] T054 [US2] Export read-char, read-line from streams package in src/clysm/streams/package.lisp

**Checkpoint**: read-char and read-line work with stdin including EOF handling

---

## Phase 5: User Story 3 - Format Function Support (Priority: P3)

**Goal**: Implement format function with ~A, ~S, ~D, ~%, ~~ directives

**Independent Test**: Compile and run programs using format with each directive and verify correct output

### Tests for User Story 3 (TDD Required)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T055 [P] [US3] Unit test for format ~A directive in tests/unit/stream-format-test.lisp
- [x] T056 [P] [US3] Unit test for format ~S directive in tests/unit/stream-format-test.lisp
- [x] T057 [P] [US3] Unit test for format ~D directive in tests/unit/stream-format-test.lisp
- [x] T058 [P] [US3] Unit test for format ~% directive in tests/unit/stream-format-test.lisp
- [x] T059 [P] [US3] Unit test for format ~~ directive in tests/unit/stream-format-test.lisp
- [x] T060 [P] [US3] Unit test for format destination=nil (return string) in tests/unit/stream-format-test.lisp
- [x] T061 [P] [US3] Unit test for format destination=t (stdout) in tests/unit/stream-format-test.lisp
- [x] T062 [P] [US3] Unit test for format destination=stream in tests/unit/stream-format-test.lisp
- [x] T063 [P] [US3] Unit test for format with multiple directives in tests/unit/stream-format-test.lisp
- [x] T064 [US3] Integration test format output with wasmtime in tests/streams/integration-test.lisp

### Implementation for User Story 3

- [x] T065 [US3] Define FormatDirective struct in src/clysm/streams/format.lisp
- [x] T066 [US3] Define FormatStringInfo struct in src/clysm/streams/format.lisp
- [x] T067 [US3] Implement parse-format-string function in src/clysm/streams/format.lisp
- [x] T068 [US3] Implement format-aesthetic (~A) handler in src/clysm/streams/format.lisp
- [x] T069 [P] [US3] Implement format-standard (~S) handler in src/clysm/streams/format.lisp
- [x] T070 [P] [US3] Implement format-decimal (~D) handler in src/clysm/streams/format.lisp
- [x] T071 [P] [US3] Implement format-newline (~%) handler in src/clysm/streams/format.lisp
- [x] T072 [P] [US3] Implement format-tilde (~~) handler in src/clysm/streams/format.lisp
- [x] T073 [US3] Implement format function with destination dispatch in src/clysm/streams/format.lisp
- [x] T074 [US3] Handle format to nil (string-output-stream) in src/clysm/streams/format.lisp
- [x] T075 [US3] Export format from streams package in src/clysm/streams/package.lisp

**Checkpoint**: format function works with all five directives and all destination types

---

## Phase 6: User Story 4 - Stream Object Management (Priority: P4)

**Goal**: Streams as first-class values with predicates and dynamic rebinding support

**Independent Test**: Compile and run programs that pass streams to functions, use streamp, and dynamically rebind stream variables

### Tests for User Story 4 (TDD Required)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T076 [P] [US4] Unit test for streamp predicate in tests/streams/stream-test.lisp
- [x] T077 [P] [US4] Unit test for input-stream-p predicate in tests/streams/stream-test.lisp
- [x] T078 [P] [US4] Unit test for output-stream-p predicate in tests/streams/stream-test.lisp
- [x] T079 [US4] Unit test for stream passed to write-string in tests/streams/stream-test.lisp
- [x] T080 [US4] Unit test for dynamic rebinding of *standard-output* in tests/streams/stream-test.lisp
- [x] T081 [US4] Integration test for stream variable rebinding with wasmtime in tests/streams/integration-test.lisp

### Implementation for User Story 4

- [x] T082 [P] [US4] Implement streamp predicate in src/clysm/streams/types.lisp
- [x] T083 [P] [US4] Implement input-stream-p predicate in src/clysm/streams/types.lisp
- [x] T084 [P] [US4] Implement output-stream-p predicate in src/clysm/streams/types.lisp
- [x] T085 [US4] Verify streams work as first-class values (pass to functions) in src/clysm/streams/types.lisp
- [x] T086 [US4] Verify dynamic rebinding of stream specials via existing shallow binding in src/clysm/streams/types.lisp
- [x] T087 [US4] Export streamp, input-stream-p, output-stream-p from streams package in src/clysm/streams/package.lisp

**Checkpoint**: All stream predicates work, streams are first-class values, dynamic rebinding works

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, documentation, and cleanup

- [x] T088 Validate no linear memory usage in stream I/O code
- [x] T089 [P] Update host-shim/README.md with usage documentation
- [x] T090 [P] Add Unicode test cases to all test files
- [x] T091 Run all tests with wasmtime and verify pass
- [x] T092 Verify wasm-tools validate passes on generated Wasm
- [x] T093 Run quickstart.md verification checklist
- [x] T094 Code review for ANSI Common Lisp compliance

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 (P1): Independent - MVP target
  - US2 (P2): Independent of US1
  - US3 (P3): Depends on US1 (uses write-char/write-string for output)
  - US4 (P4): Independent of other stories (predicates and rebinding)
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

```
                    ┌─────────────────────┐
                    │  Foundational (P2)  │
                    └─────────┬───────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        │                     │                     │
        ▼                     ▼                     ▼
   ┌─────────┐           ┌─────────┐           ┌─────────┐
   │ US1 (P1)│           │ US2 (P2)│           │ US4 (P4)│
   │ Output  │           │ Input   │           │ Stream  │
   └────┬────┘           └─────────┘           │ Objects │
        │                                      └─────────┘
        │
        ▼
   ┌─────────┐
   │ US3 (P3)│
   │ Format  │
   └─────────┘
```

- **US1** can start immediately after Foundation
- **US2** can start immediately after Foundation (parallel with US1)
- **US3** should start after US1 (format uses write functions)
- **US4** can start immediately after Foundation (parallel with US1/US2)

### Parallel Opportunities

Within Phase 2 (Foundational):
- T009, T010: Condition classes in parallel
- T013, T014: Read FFI imports in parallel
- T019-T022: Host shim functions in parallel
- T024-T026: Special variables in parallel

Within each User Story:
- All tests marked [P] can run in parallel
- Format directive handlers (T069-T072) can run in parallel

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all US1 tests in parallel:
Task: "Unit test for write-char basic output in tests/streams/write-test.lisp"
Task: "Unit test for write-char with stream argument in tests/streams/write-test.lisp"
Task: "Unit test for write-string basic output in tests/streams/write-test.lisp"
Task: "Unit test for write-string :start/:end in tests/streams/write-test.lisp"
Task: "Unit test for write-char type error in tests/streams/write-test.lisp"
Task: "Unit test for write-string type error in tests/streams/write-test.lisp"
```

## Parallel Example: Format Directive Handlers

```bash
# Launch format handlers in parallel:
Task: "Implement format-standard (~S) handler in src/clysm/streams/format.lisp"
Task: "Implement format-decimal (~D) handler in src/clysm/streams/format.lisp"
Task: "Implement format-newline (~%) handler in src/clysm/streams/format.lisp"
Task: "Implement format-tilde (~~) handler in src/clysm/streams/format.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (write-char, write-string)
4. **STOP and VALIDATE**: Test with wasmtime - can output text
5. Deploy/demo if ready - basic output works

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add User Story 1 → Test → Deploy (MVP: Output works)
3. Add User Story 2 → Test → Deploy (Input works)
4. Add User Story 3 → Test → Deploy (Format works)
5. Add User Story 4 → Test → Deploy (Full stream support)

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (Output)
   - Developer B: User Story 2 (Input)
   - Developer C: User Story 4 (Stream predicates)
3. After US1 complete:
   - Developer A: User Story 3 (Format) - depends on US1
4. All stories integrate cleanly

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD required per constitution: Write tests first, verify they fail
- Constitution Principle I: NO linear memory - verify in T088
- All Wasm output must pass wasm-tools validate
- Commit after each task or logical group
