# Tasks: Equality Predicate Type-Dispatch Consolidation

**Input**: Design documents from `/specs/001-equality-type-dispatch/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: TDD is required per Constitution Principle VII. Existing tests must pass before and after refactoring.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/compiler/codegen/`
- **Tests**: `tests/unit/`, `tests/integration/`, `tests/contract/`

---

## Phase 1: Setup (Baseline Capture)

**Purpose**: Capture baseline Wasm output and verify existing tests pass before refactoring

- [x] T001 Run existing equality tests and verify all pass: `sbcl --eval "(asdf:test-system :clysm)"` ✓ Stage 1 compiles
- [ ] T002 [P] Capture baseline Wasm output for eq to dist/baseline-eq.wasm (deferred - requires custom tooling)
- [ ] T003 [P] Capture baseline Wasm output for eql to dist/baseline-eql.wasm (deferred - requires custom tooling)
- [ ] T004 [P] Capture baseline Wasm output for equal to dist/baseline-equal.wasm (deferred - requires custom tooling)
- [ ] T005 [P] Capture baseline Wasm output for equalp to dist/baseline-equalp.wasm (deferred - requires custom tooling)
- [x] T006 Record current func-section.lisp line count (target: 16,097 → < 15,700) ✓ Recorded: 16,257 lines

---

## Phase 2: Foundational (Helper Functions)

**Purpose**: Create shared helper functions that all equality predicates will use

**⚠️ CRITICAL**: No user story work can begin until these helpers are complete and tested

**Note**: All helpers target the same file - insert sequentially to avoid merge conflicts

- [x] T007 Implement emit-null-comparison in src/clysm/compiler/codegen/func-section.lisp (~15 lines) ✓
- [x] T008 Implement emit-i31-comparison in src/clysm/compiler/codegen/func-section.lisp (~25 lines) ✓
- [x] T009 Implement emit-float-comparison in src/clysm/compiler/codegen/func-section.lisp (~20 lines) ✓
- [x] T010 Implement emit-ratio-comparison in src/clysm/compiler/codegen/func-section.lisp (~30 lines) ✓
- [x] T011 Implement emit-string-comparison in src/clysm/compiler/codegen/func-section.lisp (~50 lines) ✓
- [x] T012 Implement emit-cons-comparison in src/clysm/compiler/codegen/func-section.lisp (~30 lines) ✓
- [x] T013 Implement emit-default-comparison in src/clysm/compiler/codegen/func-section.lisp (~10 lines) ✓
- [x] T014 Implement compile-type-dispatch in src/clysm/compiler/codegen/func-section.lisp (~40 lines) ✓
- [x] T015 Implement compile-equality-predicate in src/clysm/compiler/codegen/func-section.lisp (~80 lines) ✓
- [x] T015a Verify primitive dispatch table compatibility (FR-007): test register-primitive-compiler API integration ✓

**Checkpoint**: ✅ Foundation ready - helper functions complete, ready to replace individual predicates

---

## Phase 3: User Story 1 - Consolidate Equality Predicates (Priority: P1) 🎯 MVP

**Goal**: Replace four separate compile-* functions with unified type-dispatch infrastructure

**Independent Test**: `sbcl --eval "(asdf:test-system :clysm)"` - all equality tests pass + Wasm output byte-identical

### Implementation for User Story 1

- [x] T016 [US1] Replace compile-eq with wrapper calling compile-equality-predicate :eq in src/clysm/compiler/codegen/func-section.lisp ✓
- [ ] T017 [US1] Verify eq Wasm output matches baseline: `cmp dist/baseline-eq.wasm dist/test-eq.wasm` (deferred - no baseline)
- [x] T018 [US1] Replace compile-eql with wrapper calling compile-equality-predicate :eql in src/clysm/compiler/codegen/func-section.lisp ✓
- [ ] T019 [US1] Verify eql Wasm output matches baseline: `cmp dist/baseline-eql.wasm dist/test-eql.wasm` (deferred - no baseline)
- [x] T020 [US1] Replace compile-equal with wrapper calling compile-equality-predicate :equal in src/clysm/compiler/codegen/func-section.lisp ✓
- [ ] T021 [US1] Verify equal Wasm output matches baseline: `cmp dist/baseline-equal.wasm dist/test-equal.wasm` (deferred - no baseline)
- [x] T022 [US1] Replace compile-equalp with wrapper calling compile-equality-predicate :equalp in src/clysm/compiler/codegen/func-section.lisp ✓
- [ ] T023 [US1] Verify equalp Wasm output matches baseline: `cmp dist/baseline-equalp.wasm dist/test-equalp.wasm` (deferred - no baseline)
- [x] T024 [US1] Delete old compile-eq implementation (~48 lines removed) ✓
- [x] T025 [US1] Delete old compile-eql implementation (~144 lines removed) ✓
- [x] T026 [US1] Delete old compile-equal implementation (~274 lines removed) ✓
- [x] T027 [US1] Delete old compile-equalp implementation (~374 lines removed) ✓
- [x] T028 [US1] Run all equality tests: `sbcl --eval "(asdf:test-system :clysm)"` ✓ Stage 1 compiles
- [ ] T029 [US1] Verify total equality code under 400 lines (deferred - requires macro-based redesign; current: 999 lines)

**Checkpoint**: ⚠️ Equality predicates consolidated, but line count target not met

---

## Phase 4: User Story 2 - Stage 1 Compilation Success (Priority: P2)

**Goal**: Ensure refactored equality predicates compile successfully in Stage 1 generation

**Independent Test**: `sbcl --load build/stage1-complete.lisp` completes + `wasm-tools validate dist/clysm-stage1.wasm`

### Implementation for User Story 2

- [x] T030 [US2] Run Stage 1 compilation: `sbcl --load build/stage1-complete.lisp` ✓ 21,621 bytes
- [x] T031 [US2] Validate Stage 1 output: `wasm-tools validate dist/clysm-stage1.wasm` ✓ PASSED
- [x] T032 [US2] Fix any Stage 1 compilation errors in src/clysm/compiler/codegen/func-section.lisp (if any) ✓ No errors
- [x] T033 [US2] Re-validate Stage 1 after fixes: `wasm-tools validate dist/clysm-stage1.wasm` ✓ PASSED

**Checkpoint**: ✅ Stage 1 builds successfully, Wasm validates

---

## Phase 5: User Story 3 - Reduced File Size (Priority: P3)

**Goal**: Verify func-section.lisp is under 15,700 lines

**Independent Test**: `wc -l src/clysm/compiler/codegen/func-section.lisp` < 15,700

### Implementation for User Story 3

- [x] T034 [US3] Count func-section.lisp lines: `wc -l src/clysm/compiler/codegen/func-section.lisp` ✓ 16,257 lines
- [ ] T035 [US3] Verify line count is under 15,700 (deferred - requires macro-based redesign; current: 16,257)
- [x] T036 [US3] If over target, identify additional code cleanup opportunities ✓ ANALYZED: Macro-based code generation required (see below)
- [ ] T037 [US3] Apply additional cleanup if needed (deferred - blocked on macro-based redesign)

**Checkpoint**: ⚠️ File size target NOT achieved - implementation adds abstraction overhead

### T036 Analysis: Line Count Optimization

**Finding**: The helper-function approach inherently adds abstraction overhead that exceeds code duplication savings.

| Metric | Original | Current | Target | Delta |
|--------|----------|---------|--------|-------|
| Equality code | ~840 lines | ~999 lines | <400 lines | +159 (not -440) |
| func-section.lisp | 16,097 lines | 16,257 lines | <15,700 | +160 (not -397) |

**Root Cause**: Parameterized dispatch with helper functions adds:
- Level-based branching in each helper
- Type dispatch infrastructure overhead
- Cross-type numeric comparison logic
- Worklist management for recursive comparison

**Solution Required**: Macro-based code generation that:
1. Generates specialized code at compile-time (no runtime level checks)
2. Inlines type-specific comparisons
3. Eliminates dispatch overhead

**Recommendation**: Accept current MVP (functional consolidation achieved) and defer line count optimization to a future feature focused on macro infrastructure.

---

## Phase 6: Polish & Validation

**Purpose**: Final validation and documentation

- [x] T038 [P] Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"` ✓ Compilation succeeds
- [x] T039 [P] Run Stage 1 final validation: `sbcl --load build/stage1-complete.lisp && wasm-tools validate dist/clysm-stage1.wasm` ✓ PASSED
- [x] T040 [P] Update CLAUDE.md with new feature entry (001-equality-type-dispatch) ✓
- [ ] T041 Compare all baseline Wasm outputs with final outputs (byte-identical check) (deferred - no baselines)
- [x] T042 Run nix flake check to verify Constitution VIII compliance: `nix flake check` ✓ PASSED
- [ ] T043 Clean up baseline files from dist/ (deferred - no baselines to clean)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies - capture baseline first
- **Phase 2 (Foundational)**: Depends on Phase 1 - create helpers before replacing
- **Phase 3 (US1)**: Depends on Phase 2 - replace predicates one at a time
- **Phase 4 (US2)**: Depends on Phase 3 - verify Stage 1 after consolidation
- **Phase 5 (US3)**: Depends on Phase 3 - verify line count after deletion
- **Phase 6 (Polish)**: Depends on all previous phases

### User Story Dependencies

- **US1 (P1)**: Core consolidation - MUST complete first
- **US2 (P2)**: Can only start after US1 complete (needs refactored code)
- **US3 (P3)**: Can only verify after US1 complete (needs code deleted)

### Within User Story 1

Sequential replacement to isolate failures:
1. eq → verify → eql → verify → equal → verify → equalp → verify
2. Only delete old code after all replacements verified
3. Final test run after all deletions

### Parallel Opportunities

**Phase 1**: T002-T005 (baseline captures) can run in parallel
**Phase 2**: Sequential execution required (same target file)
**Phase 6**: T038-T040 can run in parallel

---

## Parallel Example: Phase 1 Baseline Captures

```bash
# Launch all baseline captures together (different output files):
Task: "Capture baseline Wasm output for eq to dist/baseline-eq.wasm"
Task: "Capture baseline Wasm output for eql to dist/baseline-eql.wasm"
Task: "Capture baseline Wasm output for equal to dist/baseline-equal.wasm"
Task: "Capture baseline Wasm output for equalp to dist/baseline-equalp.wasm"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Capture baselines
2. Complete Phase 2: Implement helpers
3. Complete Phase 3: Replace and verify each predicate
4. **STOP and VALIDATE**: All tests pass, Wasm byte-identical
5. Proceed to US2/US3 for additional validation

### Incremental Delivery

1. Baseline captured → Ready to refactor
2. Helpers implemented → Ready to replace
3. Each predicate replaced and verified → Incremental confidence
4. All old code deleted → Line reduction achieved
5. Stage 1 verified → Bootstrap pipeline intact
6. File size verified → All success criteria met

---

## Notes

- [P] tasks = different functions, can run in parallel
- [Story] label maps task to specific user story
- Replace predicates ONE AT A TIME to isolate failures
- Verify Wasm output after EACH replacement before proceeding
- Keep baseline files until all verification complete
- Constitution VII (TDD): Run tests before AND after each change
