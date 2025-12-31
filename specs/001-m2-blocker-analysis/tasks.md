# Tasks: Phase 13D Milestone M2 - Blocker Analysis

**Input**: Design documents from `/specs/001-m2-blocker-analysis/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: Included per Constitution VII (TDD非交渉) - All fixes require tests first.

**Organization**: Tasks grouped by user story to enable independent implementation.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/`, `build/`, `dist/` at repository root

---

## Phase 1: Setup (Environment Verification)

**Purpose**: Verify development environment and baseline state

- [ ] T001 Enter Nix development shell via `nix develop`
- [ ] T002 [P] Verify SBCL 2.4+ is available via `sbcl --version`
- [ ] T003 [P] Verify wasm-tools is available via `wasm-tools --version`
- [ ] T004 [P] Verify rove testing framework via `sbcl --eval "(require :rove)"`
- [ ] T005 Load Clysm system to verify no regressions via `sbcl --eval "(asdf:load-system :clysm)"`

---

## Phase 2: Foundational (Baseline Establishment)

**Purpose**: Establish baseline metrics before any modifications

**⚠️ CRITICAL**: Complete this phase to have accurate before/after comparison

- [ ] T006 Run existing Stage 1 generation via `sbcl --load build/stage1-complete.lisp`
- [ ] T007 Record baseline metrics from dist/stage1-report.json (expected: ~13.9%)
- [ ] T008 Run existing tests via `sbcl --eval "(asdf:test-system :clysm)"` to verify green baseline
- [ ] T009 Validate baseline Wasm output via `wasm-tools validate dist/clysm-stage1.wasm`

**Checkpoint**: Baseline recorded - 13.9% compilation rate, all tests passing, valid Wasm

---

## Phase 3: User Story 1 - Regenerate Stage 1 Report (Priority: P1) 🎯 MVP

**Goal**: Generate fresh Stage 1 report with current codebase state and document findings

**Independent Test**: Run `sbcl --load build/stage1-complete.lisp` and verify dist/stage1-report.json contains per-module statistics

### Tests for User Story 1

- [ ] T010 [US1] Create contract test for report JSON schema in tests/contract/stage1-report-schema-test.lisp
- [ ] T011 [US1] Run contract test and verify it passes against dist/stage1-report.json

### Implementation for User Story 1

- [ ] T012 [US1] Run Stage 1 generation with verbose output via `sbcl --load build/stage1-complete.lisp -- --verbose`
- [ ] T013 [US1] Extract and document top_blockers from dist/stage1-report.json
- [ ] T014 [US1] Document per-module compilation rates in specs/001-m2-blocker-analysis/baseline-report.md
- [ ] T015 [US1] Verify report matches data-model.md schema (all required fields present)

**Checkpoint**: Fresh Stage 1 report generated, baseline documented

---

## Phase 4: User Story 2 - Analyze DEFUN Compilation Failures (Priority: P1)

**Goal**: Categorize all DEFUN failures by root cause and establish fix priority order

**Independent Test**: Can verify by examining categorized failures in analysis output

### Tests for User Story 2

- [ ] T016 [US2] Create test for blocker categorization logic in tests/unit/blocker-analysis-test.lisp
- [ ] T017 [US2] Run test and verify failure categorization works correctly

### Implementation for User Story 2

- [ ] T018 [US2] Run blocker analysis script via `sbcl --load build/analyze-defun-failures.lisp`
- [ ] T019 [US2] Categorize DEFSTRUCT failures (expected: 1,953 forms) from analysis output
- [ ] T020 [P] [US2] Categorize [DEFMACRO](resources/HyperSpec/Body/m_defmac.htm) failures (expected: 646 forms)
- [ ] T021 [P] [US2] Categorize [DEFINE-CONDITION](resources/HyperSpec/Body/m_defi_5.htm) failures (expected: 302 forms)
- [ ] T022 [P] [US2] Categorize [DEFVAR](resources/HyperSpec/Body/m_defvar.htm) failures (expected: 133 forms)
- [ ] T023 [US2] Document priority order in specs/001-m2-blocker-analysis/blocker-priority.md
- [ ] T024 [US2] Update research.md with actual failure counts vs. expected

**Checkpoint**: All blockers categorized, priority order established

---

## Phase 5: User Story 3 - Fix High-Impact Blockers (Priority: P2)

**Goal**: Implement fixes for top blockers to achieve 25%+ compilation rate

**Independent Test**: Re-run Stage 1 generation after each fix and verify rate increases

### Blocker Fix 1: DEFMACRO Skip (Target: +2.4%)

- [ ] T025 [US3] Write test for [DEFMACRO](resources/HyperSpec/Body/m_defmac.htm) skip logic in tests/unit/defmacro-skip-test.lisp
- [ ] T026 [US3] Run test and verify it fails (Red phase)
- [ ] T027 [US3] Implement DEFMACRO :skipped handling in src/clysm/stage1/compiler.lisp
- [ ] T028 [US3] Run test and verify it passes (Green phase)
- [ ] T029 [US3] Regenerate Stage 1 and verify DEFMACRO forms now skipped in dist/stage1-report.json

### Blocker Fix 2: DEFSTRUCT Verification (Target: +7.3%)

- [ ] T030 [US3] Write test for [DEFSTRUCT](resources/HyperSpec/Body/m_defstr.htm) expansion in tests/unit/defstruct-expand-test.lisp
- [ ] T031 [US3] Run test and verify current state (may pass if 001-defstruct-wasm-compile works)
- [ ] T032 [US3] Investigate why DEFSTRUCT expansion not triggering in Stage 1 in src/clysm/compiler/ast.lisp
- [ ] T033 [US3] Fix DEFSTRUCT → DEFCLASS expansion in src/clysm/compiler/ast.lisp if broken
- [ ] T034 [US3] Run test and verify DEFSTRUCT expansion works
- [ ] T035 [US3] Regenerate Stage 1 and verify DEFSTRUCT forms now compile

### Blocker Fix 3: DEFINE-CONDITION (Target: +1.1%)

- [ ] T036 [US3] Write test for [DEFINE-CONDITION](resources/HyperSpec/Body/m_defi_5.htm) → DEFCLASS expansion in tests/unit/define-condition-test.lisp
- [ ] T037 [US3] Run test and verify it fails (Red phase)
- [ ] T038 [US3] Implement DEFINE-CONDITION case in parse-compound-form in src/clysm/compiler/ast.lisp
- [ ] T039 [US3] Run test and verify it passes (Green phase)
- [ ] T040 [US3] Regenerate Stage 1 and verify DEFINE-CONDITION forms now compile

### Blocker Fix 4: DEFVAR Completion (Target: +0.5%)

- [ ] T041 [US3] Write test for [DEFVAR](resources/HyperSpec/Body/m_defvar.htm) global emission in tests/unit/defvar-global-test.lisp
- [ ] T042 [US3] Run test and verify current state
- [ ] T043 [US3] Complete DEFVAR global variable support in src/clysm/compiler/codegen/globals.lisp
- [ ] T044 [US3] Run test and verify DEFVAR works correctly
- [ ] T045 [US3] Regenerate Stage 1 and verify DEFVAR forms now compile

### Rate Verification

- [ ] T046 [US3] Regenerate full Stage 1 report via `sbcl --load build/stage1-complete.lisp`
- [ ] T047 [US3] Verify compilation rate in dist/stage1-report.json >= 25%
- [ ] T048 [US3] If rate < 25%, identify additional blockers and add fix tasks

**Checkpoint**: Compilation rate >= 25%, at least 3 blockers fixed

---

## Phase 6: User Story 4 - Validate Generated Output (Priority: P2)

**Goal**: Ensure all newly-compiled forms produce valid Wasm output

**Independent Test**: Run `wasm-tools validate dist/clysm-stage1.wasm` with exit code 0

### Tests for User Story 4

- [ ] T049 [US4] Create regression test baseline for previously-compiling forms in tests/contract/regression-baseline-test.lisp
- [ ] T050 [US4] Run regression test to verify no previously-working forms broke

### Implementation for User Story 4

- [ ] T051 [US4] Validate Stage 1 Wasm via `wasm-tools validate dist/clysm-stage1.wasm`
- [ ] T052 [US4] If validation fails, identify and document failing sections
- [ ] T053 [P] [US4] Examine generated Wasm for new forms via `wasm-tools print dist/clysm-stage1.wasm | head -500`
- [ ] T054 [US4] Verify no regressions via `sbcl --eval "(asdf:test-system :clysm)"`
- [ ] T055 [US4] Document final metrics in specs/001-m2-blocker-analysis/final-report.md

**Checkpoint**: Valid Wasm output, no regressions, all tests passing

---

## Phase 7: Polish & Documentation

**Purpose**: Final documentation and cleanup

- [ ] T056 [P] Update CLAUDE.md with new feature entry for 001-m2-blocker-analysis
- [ ] T057 [P] Update specs/001-m2-blocker-analysis/research.md with final findings
- [ ] T058 Document lessons learned in specs/001-m2-blocker-analysis/retrospective.md
- [ ] T059 Run full test suite one final time via `sbcl --eval "(asdf:test-system :clysm)"`
- [ ] T060 Validate quickstart.md commands all work correctly

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **US1 (Phase 3)**: Depends on Foundational - establishes baseline
- **US2 (Phase 4)**: Depends on US1 - needs fresh report to analyze
- **US3 (Phase 5)**: Depends on US2 - needs priority order for fixes
- **US4 (Phase 6)**: Depends on US3 - validates the fixes
- **Polish (Phase 7)**: Depends on US4 - final documentation

### User Story Dependencies

```
Setup → Foundational → US1 → US2 → US3 → US4 → Polish
                       (P1)  (P1)  (P2)  (P2)
```

Note: This feature has a linear dependency chain since each story builds on the previous.

### Within User Story 3 (Fixes)

Blocker fixes can be done in any order but recommended priority:
1. DEFMACRO skip (easiest, quick win)
2. DEFSTRUCT verification (highest impact)
3. DEFINE-CONDITION (medium impact)
4. DEFVAR completion (lowest impact)

### Parallel Opportunities

- T002, T003, T004 can run in parallel (environment checks)
- T020, T021, T022 can run in parallel (failure categorization)
- T053 can run in parallel with T054 (output examination)
- T056, T057 can run in parallel (documentation updates)

---

## Parallel Example: Phase 1 Setup

```bash
# Launch environment checks together:
Task: "Verify SBCL 2.4+ is available via sbcl --version"
Task: "Verify wasm-tools is available via wasm-tools --version"
Task: "Verify rove testing framework via sbcl --eval \"(require :rove)\""
```

---

## Implementation Strategy

### MVP First (US1 + US2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (baseline)
3. Complete Phase 3: US1 (report generation)
4. Complete Phase 4: US2 (failure analysis)
5. **STOP and VALIDATE**: Priority order established
6. Proceed to fixes if ready

### Full Delivery

1. Setup + Foundational → Baseline established
2. US1 + US2 → Analysis complete, blockers prioritized
3. US3 → Fixes applied, rate >= 25%
4. US4 → Validation passed
5. Polish → Documentation complete

### TDD Compliance (Constitution VII)

For each blocker fix in US3:
1. Write test first (T025, T030, T036, T041)
2. Run test and verify it FAILS (Red)
3. Implement fix
4. Run test and verify it PASSES (Green)
5. Refactor if needed

---

## Success Criteria Mapping

| Criterion | Tasks | Verification |
|-----------|-------|--------------|
| SC-001: Rate >= 25% | T046, T047 | Check dist/stage1-report.json |
| SC-002: Failures categorized | T019-T024 | blocker-priority.md |
| SC-003: Top 5 documented | T023 | blocker-priority.md |
| SC-004: >= 3 blockers fixed | T029, T035, T040, T045 | Count fixed blockers |
| SC-005: Valid Wasm | T051 | wasm-tools validate exit 0 |
| SC-006: No regressions | T050, T054 | All tests pass |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Tests required per Constitution VII (TDD非交渉)
- HyperSpec links included per Constitution IX
- Each fix follows Red-Green-Refactor cycle
- Commit after each completed blocker fix
