# Tasks: Compiler Code Generation Cleanup

**Input**: Design documents from `/specs/001-codegen-cleanup/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: Validation contracts are included per Constitution Principle VII (TDD is non-negotiable). Each batch includes validation tests.

**Organization**: Tasks are grouped by user story. US1 (Detection) runs first, then US4 (Batch Removal) executes per-batch with US2 (Transitive Analysis) integrated, finally US3 (Quasiquote Migration).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

```
src/clysm/compiler/codegen/func-section.lisp  # Primary target file
src/clysm/lib/*-runtime.lisp                   # Runtime libraries (reference)
tests/                                          # Test suites
dist/                                           # Output artifacts
```

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Establish baseline metrics and validation infrastructure

- [x] T001 Record baseline line count of `src/clysm/compiler/codegen/func-section.lisp` (15,973 lines)
- [x] T002 [P] Record baseline quasiquote pattern count: `grep -c ",@" src/clysm/compiler/codegen/func-section.lisp` (111 patterns)
- [x] T003 [P] Run baseline test suite: `sbcl --eval "(asdf:test-system :clysm)"` and record results
- [x] T004 [P] Run baseline Stage 1 compilation: `sbcl --load build/stage1-complete.lisp` and record output size (21,263 bytes)
- [x] T005 [P] Validate baseline Wasm: `wasm-tools validate dist/clysm-stage1.wasm` (PASS)
- [x] T006 Record baseline coverage rate from `dist/stage1-report.json` (9.86%)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Create validation script and document runtime function registrations

**⚠️ CRITICAL**: No removal work can begin until this phase is complete

- [x] T007 Create validation script at `scripts/validate-batch.sh` per contracts/validation.md
- [x] T008 [P] Document all runtime function registrations in `specs/001-codegen-cleanup/dead-functions.md`
- [x] T009 [P] Create progress tracking file at `specs/001-codegen-cleanup/progress.md`
- [x] T010 Test validation script runs successfully on current codebase

**Checkpoint**: ✅ Validation infrastructure ready - detection and removal can now begin

---

## Phase 3: User Story 1 - Dead Code Detection (Priority: P1) 🎯 MVP

**Goal**: Identify all compile-* functions whose functionality has been migrated to *runtime-function-table*

**Independent Test**: Run detection and verify it correctly identifies functions registered in each runtime registration function

### Implementation for User Story 1

- [x] T011 [US1] List all functions in `register-io-runtime-functions` - ✅ ALREADY REMOVED (6 functions)
- [x] T012 [P] [US1] List all functions in `register-list-runtime-functions` - ✅ ALREADY REMOVED (13 functions)
- [x] T013 [P] [US1] List all functions in `register-sequence-runtime-functions` - ✅ ALREADY REMOVED (15 functions)
- [x] T014 [P] [US1] List all functions in `register-string-runtime-functions` - 🔴 12 dead functions with line numbers
- [x] T015 [P] [US1] List all functions in `register-numeric-runtime-functions` - 🔴 5 dead functions with line numbers
- [x] T016 [US1] Consolidate dead function list with line numbers in `specs/001-codegen-cleanup/dead-functions.md`
- [x] T017 [US1] Verify no false positives - Confirmed: `runtime-function-p` check at line 991 precedes case branches

**Checkpoint**: ✅ Dead function list complete. Only String (12) + Numeric (5) = 17 functions remain. US4 batch removal can now begin.

---

## Phase 4: User Story 4 - Batch 1: String Functions (Priority: P1) - ✅ COMPLETE

**Goal**: Remove dead string function implementations (~15 functions, ~1,500 lines)

**Status**: All dead string functions were already removed in 001-string-runtime-migration. Verified remaining string functions (compile-string-upcase, compile-string-downcase, etc.) are LIVE and should be kept.

### Transitive Analysis for Batch 1 (US2)

- [x] T018 [US2] [US4] Identify helper functions called only by dead string compile-* functions - NONE FOUND (all dead functions already removed)
- [x] T019 [US2] [US4] Document transitive dead helpers - Already documented in dead-functions.md

### Removal for Batch 1 (US4)

- [x] T020 [US4] compile-char/compile-schar → Already removed (compile-string-char comment at line 12848)
- [x] T021 [P] [US4] compile-string-*-trim → Removed in previous session (13660-14010)
- [x] T022 [P] [US4] compile-string-capitalize, compile-nstring-capitalize → Removed (14035, 14764)
- [x] T023 [P] [US4] compile-string-equal, compile-string-not-equal → Already removed in 001-string-runtime-migration
- [x] T024 [P] [US4] compile-string-lessp, compile-string-greaterp → Already removed
- [x] T025 [P] [US4] compile-string-not-lessp, compile-string-not-greaterp → Already removed
- [x] T026 [US4] No transitive helpers to remove
- [x] T027 [US4] Validation passes (Wasm validates, Stage 1 compiles)
- [x] T028 [US4] Line count: 14,894 (after string cleanup)
- [ ] T029 [US4] Commit deferred to batch consolidation

**Checkpoint**: ✅ Batch 1 complete. String functions removed, all validations pass.

---

## Phase 5: User Story 4 - Batch 2: Numeric Functions (Priority: P1) - ✅ COMPLETE

**Goal**: Remove dead numeric function implementations (~10 functions, ~800 lines)

**Status**: All 5 numeric functions removed. ~900 lines eliminated this session.

### Transitive Analysis for Batch 2 (US2)

- [x] T030 [US2] [US4] Identify helper functions - NONE (numeric functions are standalone)
- [x] T031 [US2] [US4] Document transitive dead helpers - Already documented in dead-functions.md

### Removal for Batch 2 (US4)

- [x] T032 [US4] Remove compile-parse-integer (lines 7702-7938, ~237 lines)
- [x] T033 [P] [US4] Remove compile-write-to-string (lines 7488-7700, ~213 lines)
- [x] T034 [P] [US4] Remove compile-rationalize (lines 7296-7486, ~191 lines)
- [x] T035 [P] [US4] Remove compile-signum (lines 5648-5798, ~151 lines)
- [x] T036 [P] [US4] Remove compile-phase (lines 6710-6820, ~111 lines)
- [x] T037 [US4] No transitive helpers to remove
- [x] T038 [US4] Validation: Stage 1 compiles (20,906 bytes), Wasm validates
- [x] T039 [US4] Line count: 13,994 (target was ~13,700)
- [ ] T040 [US4] Commit deferred to batch consolidation

**Checkpoint**: ✅ Batch 2 complete. Numeric functions removed, all validations pass.

---

## Phase 6: User Story 4 - Batch 3: Sequence Functions (Priority: P1) - ✅ ALREADY COMPLETE

**Status**: Sequence compile-* functions were removed in feature 001-sequence-runtime-migration

- [x] T041-T052 [US4] All sequence functions already removed in previous migration

**Checkpoint**: ✅ Batch 3 already complete. No action needed.

---

## Phase 7: User Story 4 - Batch 4: List Functions (Priority: P1) - ✅ ALREADY COMPLETE

**Status**: List compile-* functions were removed in feature 001-io-list-runtime

- [x] T053-T063 [US4] All list functions already removed in previous migration

**Checkpoint**: ✅ Batch 4 already complete. No action needed.

---

## Phase 8: User Story 4 - Batch 5: I/O Functions (Priority: P1) - ✅ ALREADY COMPLETE

**Status**: I/O compile-* functions were removed in feature 001-io-list-runtime

- [x] T064-T073 [US4] All I/O functions already removed in previous migration

**Checkpoint**: ✅ Batch 5 already complete. No action needed.

---

## Phase 9: User Story 4 - Batch 6: Miscellaneous Helpers (Priority: P1) - ✅ PARTIAL COMPLETE

**Goal**: Remove remaining orphaned helper functions (~30 functions, ~1,500 lines)

**Status**: Removed 10 orphaned helpers (~389 lines). Line count: 13,605. Target <8,000 NOT met.

### Transitive Analysis for Batch 6 (US2)

- [x] T074 [US2] [US4] Perform full transitive analysis - Found 10 orphaned functions
- [x] T075 [US2] [US4] Identify all helper functions with no live callers - Complete
- [x] T076 [US2] [US4] Document final orphaned helpers - Listed in progress.md

### Removal for Batch 6 (US4)

- [x] T077 [US4] Remove orphaned emit-* helper functions (5 numeric tower + 2 string comparison)
- [x] T078 [P] [US4] Remove compile-string-compare-at-end (non-v2), compile-string-compare-at-diff (non-v2)
- [x] T079 [P] [US4] N/A - no orphaned compile-*-args helpers found
- [x] T080 [P] [US4] Removed %generate-in-char-bag-check
- [x] T081 [US4] Validation passes (Stage 1 compiles, Wasm validates)
- [x] T082 [US4] Line count: 13,605 (target: <8,000 NOT MET)
- [ ] T083 [US4] SC-001 NOT MET - Remaining gap: ~5,605 lines
- [ ] T084 [US4] Commit deferred to batch consolidation

**Checkpoint**: Orphaned helpers removed but <8,000 target not achievable with dead code removal alone. Remaining code is live functionality.

---

## Phase 10: User Story 3 - Quasiquote Pattern Migration (Priority: P3)

**Goal**: Migrate remaining `,@` patterns to `with-instruction-collector` macro for consistency

**Independent Test**: After each migration, Wasm output is byte-for-byte identical

### Implementation for User Story 3

- [ ] T085 [US3] Count remaining `,@` patterns in live functions: `grep -n ",@" src/clysm/compiler/codegen/func-section.lisp`
- [ ] T086 [US3] Group patterns by containing function for batch migration
- [ ] T087 [US3] Migrate first 25% of patterns (functions A-G alphabetically) to `with-instruction-collector`
- [ ] T088 [US3] Run validation after first migration batch: `scripts/validate-batch.sh`
- [ ] T089 [US3] Migrate next 25% of patterns (functions H-M) to `with-instruction-collector`
- [ ] T090 [US3] Run validation after second migration batch
- [ ] T091 [US3] Migrate next 25% of patterns (functions N-S) to `with-instruction-collector`
- [ ] T092 [US3] Run validation after third migration batch
- [ ] T093 [US3] Migrate final 25% of patterns (functions T-Z) to `with-instruction-collector`
- [ ] T094 [US3] Run validation after final migration batch
- [ ] T095 [US3] Verify zero `,@` patterns remain: `grep -c ",@" src/clysm/compiler/codegen/func-section.lisp` returns 0
- [ ] T096 [US3] Commit quasiquote migration: `git commit -m "refactor: migrate all quasiquote patterns to with-instruction-collector"`

**Checkpoint**: All quasiquote patterns migrated. Codebase uses consistent instruction generation idiom.

---

## Phase 11: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, documentation, and cleanup

- [ ] T097 [P] Update CLAUDE.md with new line counts and removed features
- [ ] T098 [P] Update `specs/001-codegen-cleanup/progress.md` with final metrics
- [ ] T099 Run final comprehensive validation: `scripts/validate-batch.sh`
- [ ] T100 Verify SC-001: func-section.lisp < 8,000 lines
- [ ] T101 [P] Verify SC-002: All tests pass (100% compatibility)
- [ ] T102 [P] Verify SC-003: Stage 1 Wasm validates
- [ ] T103 [P] Verify SC-004: Zero quasiquote patterns remain
- [ ] T104 Verify SC-005: Coverage rate >= 22%
- [ ] T105 Run quickstart.md validation steps
- [ ] T106 Create summary report in `specs/001-codegen-cleanup/COMPLETION-REPORT.md`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user story work
- **US1 Detection (Phase 3)**: Depends on Foundational - BLOCKS all batch removals
- **US4 Batches 1-6 (Phases 4-9)**: Depend on US1 Detection - execute sequentially
- **US3 Migration (Phase 10)**: Depends on US4 completion (all batches)
- **Polish (Phase 11)**: Depends on all stories complete

### User Story Dependencies

- **US1 (Detection)**: Can start after Phase 2 - No dependencies on other stories
- **US2 (Transitive Analysis)**: Integrated into each US4 batch - runs before each removal
- **US3 (Quasiquote Migration)**: Depends on US4 completion - dead code must be removed first
- **US4 (Batch Removal)**: Depends on US1 - batches execute sequentially with validation

### Within Each Batch

1. Transitive analysis (US2 tasks)
2. Function removal (US4 removal tasks, [P] can parallelize)
3. Validation (must pass before next batch)
4. Commit (atomic save point)

### Parallel Opportunities

- **Phase 1**: T002-T006 can run in parallel
- **Phase 2**: T008-T009 can run in parallel
- **Phase 3**: T012-T015 can run in parallel (each registration function is independent)
- **Phases 4-9**: Within each batch, removal of individual functions [P] can parallelize
- **Phase 10**: Migration batches must be sequential for validation
- **Phase 11**: T097-T098, T101-T103 can run in parallel

---

## Parallel Example: Batch 1 Removal

```bash
# After transitive analysis (T018-T019), these removals can run in parallel:
Task: T021 [P] [US4] Remove compile-string-trim family
Task: T022 [P] [US4] Remove compile-string-capitalize family
Task: T023 [P] [US4] Remove compile-string-equal family
Task: T024 [P] [US4] Remove compile-string-lessp family
Task: T025 [P] [US4] Remove compile-string-not-lessp family

# Then sequential tasks:
Task: T026 [US4] Remove transitive dead helpers
Task: T027 [US4] Run validation
```

---

## Implementation Strategy

### MVP First (Through Batch 1)

1. Complete Phase 1: Setup (record baselines)
2. Complete Phase 2: Foundational (validation script)
3. Complete Phase 3: US1 Detection (dead function list)
4. Complete Phase 4: Batch 1 (String functions)
5. **STOP and VALIDATE**: First removal batch verified working
6. Line count reduced by ~1,500 lines (~14,500 remaining)

### Incremental Delivery

1. Complete Setup + Foundational → Validation ready
2. Complete US1 Detection → Dead code identified
3. Each batch (Phases 4-9) → Remove category, validate, commit
4. Complete US3 Migration → Consistent patterns
5. Each batch is an atomic deliverable with working compiler

### Rollback Strategy

If any validation fails during a batch:
1. Identify failing function via test error
2. `git checkout -- src/clysm/compiler/codegen/func-section.lisp`
3. Re-run validation to confirm baseline restored
4. Remove functions one-by-one until failure isolated
5. Keep problematic function, continue with remaining batch

---

## Notes

- [P] tasks = different files or independent function removals
- [US1], [US2], [US3], [US4] labels map tasks to specific user stories
- Each batch is independently validatable
- Verify validation passes before proceeding to next batch
- Commit after each batch for atomic rollback points
- Target: 15,973 lines → <8,000 lines (>50% reduction)
