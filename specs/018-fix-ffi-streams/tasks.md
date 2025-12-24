# Tasks: Fix FFI Streams Module

**Input**: Design documents from `/specs/018-fix-ffi-streams/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md

**Note**: This is a **fix task** for an existing implementation. Tests already exist but are disabled. The goal is to enable the module, fix package configuration issues, and verify functionality.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US5)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/streams/` (existing, no changes needed)
- **Tests**: `tests/unit/stream-*-test.lisp`, `tests/streams/`
- **Config**: `clysm.asd`, `tests/package.lisp`

---

## Phase 1: Setup (Baseline Verification)

**Purpose**: Establish baseline by verifying current system state before changes

- [ ] T001 Run existing test suite to establish baseline: `(asdf:test-system :clysm)`
- [ ] T002 Verify gc-types exports +type-stream+ in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T003 Document baseline test count and pass rate

**Checkpoint**: Baseline established - all existing tests pass before fix begins

---

## Phase 2: Foundational (Package Configuration Fixes)

**Purpose**: Fix the package configuration issues identified in research.md - BLOCKS all user story work

**⚠️ CRITICAL**: These fixes must complete before stream module can be enabled

### Package Definition Tasks

- [ ] T004 [P] Add clysm/tests/unit/stream-types package definition to tests/package.lisp
- [ ] T005 [P] Add clysm/tests/unit/stream-write package definition to tests/package.lisp
- [ ] T006 [P] Add clysm/tests/unit/stream-read package definition to tests/package.lisp
- [ ] T007 [P] Add clysm/tests/unit/stream-format package definition to tests/package.lisp

### Test File Package Updates

- [ ] T008 [P] Update package declaration in tests/unit/stream-types-test.lisp to use clysm/tests/unit/stream-types
- [ ] T009 [P] Update package declaration in tests/unit/stream-write-test.lisp to use clysm/tests/unit/stream-write
- [ ] T010 [P] Update package declaration in tests/unit/stream-read-test.lisp to use clysm/tests/unit/stream-read
- [ ] T011 [P] Update package declaration in tests/unit/stream-format-test.lisp to use clysm/tests/unit/stream-format

**Checkpoint**: Package configuration fixed - module enablement can proceed

---

## Phase 3: User Stories 1 & 2 - Enable Streams Module (Priority: P1) 🎯 MVP

**Goal**: Re-enable the streams module and fix any remaining issues so the system compiles without errors

**Independent Test**: `(asdf:load-system :clysm)` succeeds without errors

**Why combined**: US1 (Re-enable) and US2 (Fix Issues) are the same work - enabling requires fixing, and fixing enables.

### Enable Module Tasks

- [ ] T012 [US1] Uncomment streams module definition in clysm.asd (lines 115-125)
- [ ] T013 [US1] Attempt system load: `(asdf:load-system :clysm)` and document any errors
- [ ] T014 [US2] Fix any compilation errors found during load attempt (if any)
- [ ] T015 [US1] Verify streams package exports are accessible after load
- [ ] T016 [US1] Verify *standard-input*, *standard-output*, *error-output* are initialized

### Enable Tests Tasks

- [ ] T017 [US2] Uncomment stream unit tests in clysm.asd (lines 193-197)
- [ ] T018 [US2] Uncomment streams test module in clysm.asd (lines 205-210)
- [ ] T019 [US2] Attempt test system load: `(asdf:load-system :clysm/tests)`
- [ ] T020 [US2] Fix any test loading errors found (if any)

**Checkpoint**: Streams module enabled and loads successfully

---

## Phase 4: User Story 3 - Pass All Test Suites (Priority: P2)

**Goal**: Verify all tests pass after streams module is enabled

**Independent Test**: `(asdf:test-system :clysm)` shows 100% pass rate

### Test Verification Tasks

- [ ] T021 [US3] Run full test suite: `(asdf:test-system :clysm)`
- [ ] T022 [US3] Document test results and any failures
- [ ] T023 [US3] Fix stream-types-test failures (if any) in tests/unit/stream-types-test.lisp
- [ ] T024 [US3] Fix stream-write-test failures (if any) in tests/unit/stream-write-test.lisp
- [ ] T025 [US3] Fix stream-read-test failures (if any) in tests/unit/stream-read-test.lisp
- [ ] T026 [US3] Fix stream-format-test failures (if any) in tests/unit/stream-format-test.lisp
- [ ] T027 [US3] Fix integration test failures (if any) in tests/streams/stream-test.lisp
- [ ] T028 [US3] Verify no regressions in existing (non-stream) tests
- [ ] T029 [US3] Run full test suite again and confirm 100% pass rate

**Checkpoint**: All tests pass including stream tests

---

## Phase 5: User Story 4 - ANSI CL Compliance Verification (Priority: P2)

**Goal**: Verify format, write, and read functions behave according to ANSI CL specification

**Independent Test**: All format directives produce correct output per ANSI spec

### Compliance Verification Tasks

- [ ] T030 [US4] Test ~A directive: `(format nil "~A" "test")` returns "test"
- [ ] T031 [US4] Test ~S directive: `(format nil "~S" "test")` returns "\"test\""
- [ ] T032 [US4] Test ~D directive: `(format nil "~D" 42)` returns "42"
- [ ] T033 [US4] Test ~% directive: `(format nil "~%")` returns newline string
- [ ] T034 [US4] Test ~~ directive: `(format nil "~~")` returns "~"
- [ ] T035 [US4] Test write-char: verify character output to stream
- [ ] T036 [US4] Test write-string: verify string output to stream
- [ ] T037 [US4] Test streamp predicate: `(streamp *standard-output*)` returns T
- [ ] T038 [US4] Test input-stream-p: returns T for *standard-input*
- [ ] T039 [US4] Test output-stream-p: returns T for *standard-output*
- [ ] T040 [US4] Document ANSI compliance status in commit message

**Checkpoint**: All ANSI CL format directives verified compliant

---

## Phase 6: User Story 5 - FFI Integration Verification (Priority: P3)

**Goal**: Verify stream operations correctly use FFI foundation for host I/O

**Independent Test**: FFI imports in ffi-io.lisp generate correct Wasm imports

### FFI Verification Tasks

- [ ] T041 [US5] Verify define-foreign-function macro expands correctly in src/clysm/streams/ffi-io.lisp
- [ ] T042 [US5] Verify %host-write-char FFI import is properly defined
- [ ] T043 [US5] Verify %host-write-string FFI import is properly defined
- [ ] T044 [US5] Verify %host-read-char FFI import is properly defined
- [ ] T045 [US5] Verify %host-read-line FFI import is properly defined
- [ ] T046 [US5] Test type-error signaling for invalid write-char argument
- [ ] T047 [US5] Test type-error signaling for invalid write-string argument
- [ ] T048 [US5] Test type-error signaling for read from output-only stream
- [ ] T049 [US5] Document FFI integration status in commit message

**Checkpoint**: FFI integration verified working

---

## Phase 7: Polish & Documentation

**Purpose**: Final cleanup and documentation

- [ ] T050 Run nix flake check to verify CI passes
- [ ] T051 Update CLAUDE.md with feature completion status
- [ ] T052 Create commit with conventional message documenting root cause
- [ ] T053 Run quickstart.md verification commands
- [ ] T054 Final test suite run: `(asdf:test-system :clysm)` with 100% pass

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup
    ↓
Phase 2: Foundational (Package Fixes)
    ↓
Phase 3: US1+US2 (Enable Module) ← MVP
    ↓
Phase 4: US3 (Verify Tests)
    ↓
Phase 5: US4 (ANSI Compliance) ←→ Phase 6: US5 (FFI Verification)
    ↓
Phase 7: Polish
```

### User Story Dependencies

| Story | Depends On | Can Run In Parallel With |
|-------|------------|--------------------------|
| US1+US2 | Phase 2 (Foundational) | None (must complete first) |
| US3 | US1+US2 | None (verification of US1+US2) |
| US4 | US3 | US5 (independent verifications) |
| US5 | US3 | US4 (independent verifications) |

### Within-Phase Parallelism

**Phase 2** (T004-T011): All tasks marked [P] can run in parallel
- Package definitions (T004-T007): 4 tasks, same file but different sections
- Test file updates (T008-T011): 4 tasks, different files

**Phase 5 & 6**: Can run in parallel once Phase 4 completes

---

## Parallel Execution Examples

### Phase 2: Package Configuration (4 parallel streams)

```bash
# All in tests/package.lisp but independent additions:
Task: "Add clysm/tests/unit/stream-types package definition"
Task: "Add clysm/tests/unit/stream-write package definition"
Task: "Add clysm/tests/unit/stream-read package definition"
Task: "Add clysm/tests/unit/stream-format package definition"

# All different files:
Task: "Update package in tests/unit/stream-types-test.lisp"
Task: "Update package in tests/unit/stream-write-test.lisp"
Task: "Update package in tests/unit/stream-read-test.lisp"
Task: "Update package in tests/unit/stream-format-test.lisp"
```

### Phase 5 & 6: Compliance & FFI (parallel)

```bash
# Can run simultaneously after Phase 4:
Task Agent 1: US4 ANSI Compliance (T030-T040)
Task Agent 2: US5 FFI Verification (T041-T049)
```

---

## Implementation Strategy

### MVP First (Phase 1-3)

1. ✅ Complete Phase 1: Setup (baseline)
2. ✅ Complete Phase 2: Package fixes
3. ✅ Complete Phase 3: Enable module
4. **STOP and VALIDATE**: `(asdf:load-system :clysm)` succeeds
5. If load works, MVP is complete - streams module is enabled

### Full Verification (Phase 4-7)

1. Complete Phase 4: Run all tests
2. Complete Phase 5 & 6 in parallel: ANSI + FFI verification
3. Complete Phase 7: Polish and commit
4. Final validation: `nix flake check` passes

### Rollback Strategy

If issues are found that cannot be easily fixed:
1. Re-comment streams module in clysm.asd
2. Document specific blocking issues in research.md
3. Create follow-up tasks for deeper investigation

---

## Notes

- **TDD Applied**: Tests already exist but are disabled. This fix enables them.
- **Constitution Compliance**: TDD principle satisfied by enabling and running existing tests.
- **Low Risk**: Research indicates issues are package configuration, not implementation bugs.
- **Commit After**: Each phase completion should be a logical commit point.
- **Documentation**: Root cause analysis should be included in commit messages per FR-004.
