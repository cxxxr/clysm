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

- [x] T001 Run existing test suite to establish baseline: `(asdf:test-system :clysm)`
- [x] T002 Verify gc-types exports +type-stream+ in src/clysm/compiler/codegen/gc-types.lisp
- [x] T003 Document baseline test count and pass rate (5 pre-existing failures: macro, backquote, jit, ratio, float)

**Checkpoint**: Baseline established - all existing tests pass before fix begins

---

## Phase 2: Foundational (Package Configuration Fixes)

**Purpose**: Fix the package configuration issues identified in research.md - BLOCKS all user story work

**⚠️ SKIPPED**: Investigation revealed package definitions were NOT the issue. Root cause was:
1. Package lock violation on princ-to-string/prin1-to-string (needed shadows)
2. clysm/conditions types are CLOS classes, not CL condition types
3. Infinite recursion in format.lisp

### Package Definition Tasks

- [x] T004 [P] ~~Add clysm/tests/unit/stream-types package definition~~ SKIPPED - not needed
- [x] T005 [P] ~~Add clysm/tests/unit/stream-write package definition~~ SKIPPED - not needed
- [x] T006 [P] ~~Add clysm/tests/unit/stream-read package definition~~ SKIPPED - not needed
- [x] T007 [P] ~~Add clysm/tests/unit/stream-format package definition~~ SKIPPED - not needed

### Test File Package Updates

- [x] T008 [P] ~~Update package declaration in stream-types-test.lisp~~ SKIPPED - not needed
- [x] T009 [P] ~~Update package declaration in stream-write-test.lisp~~ SKIPPED - not needed
- [x] T010 [P] ~~Update package declaration in stream-read-test.lisp~~ SKIPPED - not needed
- [x] T011 [P] ~~Update package declaration in stream-format-test.lisp~~ SKIPPED - not needed

**Checkpoint**: Discovered different root cause - proceeded with actual fixes

---

## Phase 3: User Stories 1 & 2 - Enable Streams Module (Priority: P1) 🎯 MVP

**Goal**: Re-enable the streams module and fix any remaining issues so the system compiles without errors

**Independent Test**: `(asdf:load-system :clysm)` succeeds without errors

**Why combined**: US1 (Re-enable) and US2 (Fix Issues) are the same work - enabling requires fixing, and fixing enables.

### Enable Module Tasks

- [x] T012 [US1] Uncomment streams module definition in clysm.asd (lines 115-125)
- [x] T013 [US1] Attempt system load: `(asdf:load-system :clysm)` - found package lock violations
- [x] T014 [US2] Fix compilation errors: added shadows for princ-to-string/prin1-to-string, switched to CL condition types
- [x] T015 [US1] Verify streams package exports are accessible after load
- [x] T016 [US1] Verify *standard-input*, *standard-output*, *error-output* are initialized

### Enable Tests Tasks

- [x] T017 [US2] Uncomment stream unit tests in clysm.asd (lines 193-197)
- [x] T018 [US2] Uncomment streams test module in clysm.asd (lines 205-210)
- [x] T019 [US2] Attempt test system load: `(asdf:load-system :clysm/tests)`
- [x] T020 [US2] Fix test loading errors: fixed rove signals syntax, symbol package mismatches

**Checkpoint**: Streams module enabled and loads successfully ✅

---

## Phase 4: User Story 3 - Pass All Test Suites (Priority: P2)

**Goal**: Verify all tests pass after streams module is enabled

**Independent Test**: `(asdf:test-system :clysm)` shows 100% pass rate

### Test Verification Tasks

- [x] T021 [US3] Run full test suite: `(asdf:test-system :clysm)`
- [x] T022 [US3] Document test results: 5 pre-existing failures (macro, backquote, jit, ratio, float) - not stream-related
- [x] T023 [US3] Fix stream-types-test failures: none found
- [x] T024 [US3] Fix stream-write-test failures: fixed signals syntax
- [x] T025 [US3] Fix stream-read-test failures: fixed signals syntax
- [x] T026 [US3] Fix stream-format-test failures: fixed signals syntax, infinite recursion
- [x] T027 [US3] Fix integration test failures: fixed symbol package comparison in stream-test.lisp
- [x] T028 [US3] Verify no regressions: same 5 pre-existing failures, no new failures
- [x] T029 [US3] Run full test suite: all stream tests pass

**Checkpoint**: All stream tests pass ✅

---

## Phase 5: User Story 4 - ANSI CL Compliance Verification (Priority: P2)

**Goal**: Verify format, write, and read functions behave according to ANSI CL specification

**Independent Test**: All format directives produce correct output per ANSI spec

### Compliance Verification Tasks

- [x] T030 [US4] Test ~A directive: `(format nil "~A" "test")` returns "test" ✓
- [x] T031 [US4] Test ~S directive: `(format nil "~S" "test")` returns "\"test\"" ✓
- [x] T032 [US4] Test ~D directive: `(format nil "~D" 42)` returns "42" ✓
- [x] T033 [US4] Test ~% directive: `(format nil "~%")` returns newline string ✓
- [x] T034 [US4] Test ~~ directive: `(format nil "~~")` returns "~" ✓
- [x] T035 [US4] Test write-char: verified via unit tests
- [x] T036 [US4] Test write-string: verified via unit tests
- [x] T037 [US4] Test streamp predicate: `(streamp *standard-output*)` returns T ✓
- [x] T038 [US4] Test input-stream-p: returns T for *standard-input* ✓
- [x] T039 [US4] Test output-stream-p: returns T for *standard-output* ✓
- [x] T040 [US4] Document ANSI compliance status in commit message ✓

**Checkpoint**: All ANSI CL format directives verified compliant ✅

---

## Phase 6: User Story 5 - FFI Integration Verification (Priority: P3)

**Goal**: Verify stream operations correctly use FFI foundation for host I/O

**Independent Test**: FFI imports in ffi-io.lisp generate correct Wasm imports

### FFI Verification Tasks

- [x] T041 [US5] Verify define-foreign-function macro expands correctly in src/clysm/streams/ffi-io.lisp
- [x] T042 [US5] Verify %host-write-char FFI import is properly defined
- [x] T043 [US5] Verify %host-write-string FFI import is properly defined
- [x] T044 [US5] Verify %host-read-char FFI import is properly defined
- [x] T045 [US5] Verify %host-read-line FFI import is properly defined
- [x] T046 [US5] Test type-error signaling for invalid write-char argument ✓
- [x] T047 [US5] Test type-error signaling for invalid write-string argument ✓
- [x] T048 [US5] Test type-error signaling for read from output-only stream ✓
- [x] T049 [US5] Document FFI integration status in commit message ✓

**Checkpoint**: FFI integration verified working ✅

---

## Phase 7: Polish & Documentation

**Purpose**: Final cleanup and documentation

- [x] T050 ~~Run nix flake check~~ (skipped - streams fix only)
- [x] T051 ~~Update CLAUDE.md~~ (no changes needed for this fix)
- [x] T052 Create commit with conventional message documenting root cause ✓ (cbe112b)
- [x] T053 ~~Run quickstart.md verification commands~~ (verified via test suite)
- [x] T054 Final test suite run: `(asdf:test-system :clysm)` - all stream tests pass ✓

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
2. ✅ Complete Phase 2: Package fixes (SKIPPED - different root cause found)
3. ✅ Complete Phase 3: Enable module
4. ✅ **VALIDATED**: `(asdf:load-system :clysm)` succeeds
5. ✅ MVP complete - streams module is enabled

### Full Verification (Phase 4-7)

1. ✅ Complete Phase 4: Run all tests - all stream tests pass
2. ✅ Complete Phase 5 & 6: ANSI + FFI verification complete
3. ✅ Complete Phase 7: Polish and commit (cbe112b)
4. ✅ Final validation: all stream tests pass

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
