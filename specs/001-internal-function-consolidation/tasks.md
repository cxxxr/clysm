# Tasks: Compiler Internal Function Consolidation

**Input**: Design documents from `/specs/001-internal-function-consolidation/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md

**Tests**: Existing test suite (asdf:test-system :clysm) is used for regression testing. No new test tasks needed.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Compiler source in `src/clysm/compiler/`
- Library source in `src/clysm/lib/`

---

## Phase 1: Setup (Baseline Verification)

**Purpose**: Establish baseline metrics and verify starting state

- [x] T001 Run baseline tests via sbcl --eval "(asdf:test-system :clysm)" and verify all pass
- [x] T002 Record baseline func-section.lisp line count via wc -l src/clysm/compiler/codegen/func-section.lisp (expect: 18,351)
- [x] T003 Run baseline Stage 1 generation via sbcl --load build/stage1-complete.lisp and record compilation rate (expect: 22.15%)
- [x] T004 Verify wasm-tools validate dist/clysm-stage1.wasm passes

---

## Phase 2: Foundational (Package Export Infrastructure)

**Purpose**: Ensure package export mechanisms are in place before modifying exports

**⚠️ CRITICAL**: Export verification must complete before dead code removal

- [x] T005 Verify func-section package defpackage structure in src/clysm/compiler/codegen/func-section.lisp (line 1-50)
- [x] T006 [P] Verify clysm package re-export mechanism in src/clysm/clysm.lisp
- [x] T007 [P] Identify dispatch table references to compile-* functions in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: Package export infrastructure understood - User Story 1 can now begin

---

## Phase 3: User Story 1 - Export Internal Functions (Priority: P1) 🎯 MVP

**Goal**: Export 7 internal compiler functions so Stage 1 compilation succeeds for 242+ forms

**Independent Test**: Run Stage 1 generation and verify zero "undefined function" errors for the 7 targeted functions

### Verify Already-Exported Functions (5 functions)

- [x] T008 [P] [US1] Verify ENV-ADD-LOCAL export chain from func-section.lisp:196 → clysm package
- [x] T009 [P] [US1] Verify COMPILE-TO-INSTRUCTIONS export chain from func-section.lisp:351 → clysm package
- [x] T010 [P] [US1] Verify MAKE-WASM-STRUCT-TYPE* export from type-construction.lisp:101 → clysm package
- [x] T011 [P] [US1] Verify AST-LITERAL-VALUE export chain from ast.lisp:28 → clysm package
- [x] T012 [P] [US1] Verify LOOP-KEYWORD-EQ export chain from macros.lisp:818 → clysm package

### Add Missing Exports (2 functions)

- [x] T013 [US1] Add COMPILE-UNARY-MATH-FFI to :export list in defpackage clysm/compiler/codegen/func-section in src/clysm/compiler/codegen/func-section.lisp
- [x] T014 [US1] Add COMPILE-CXR-CHAIN to :export list in defpackage clysm/compiler/codegen/func-section in src/clysm/compiler/codegen/func-section.lisp
- [x] T015 [US1] Add re-export of COMPILE-UNARY-MATH-FFI and COMPILE-CXR-CHAIN from clysm package in src/clysm/clysm.lisp

### Verification

- [x] T016 [US1] Run tests via sbcl --eval "(asdf:test-system :clysm)" after exports added
- [x] T017 [US1] Run Stage 1 generation and verify zero undefined function errors for 7 targeted functions

**Checkpoint**: User Story 1 complete - all 7 internal functions now exported and accessible

---

## Phase 4: User Story 2 - Remove Dead Code (Priority: P2)

**Goal**: Remove dead code from func-section.lisp to reduce from 18,351 to <12,000 lines

**Independent Test**: Verify func-section.lisp line count <12,000 and all existing tests pass

### Remove I/O Dead Code (~1,150 lines)

- [x] T018 [P] [US2] Remove compile-princ function from src/clysm/compiler/codegen/func-section.lisp
- [x] T019 [P] [US2] Remove compile-prin1 function from src/clysm/compiler/codegen/func-section.lisp
- [x] T020 [P] [US2] Remove compile-print function from src/clysm/compiler/codegen/func-section.lisp
- [x] T021 [P] [US2] Remove compile-write function from src/clysm/compiler/codegen/func-section.lisp
- [x] T022 [P] [US2] Remove compile-format function from src/clysm/compiler/codegen/func-section.lisp
- [x] T023 [P] [US2] Remove compile-terpri function from src/clysm/compiler/codegen/func-section.lisp
- [x] T024 [US2] Run tests after I/O dead code removal via sbcl --eval "(asdf:test-system :clysm)"

### Remove List Dead Code (~2,550 lines)

- [x] T025 [P] [US2] Remove compile-member function from src/clysm/compiler/codegen/func-section.lisp
- [x] T026 [P] [US2] Remove compile-member-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T027 [P] [US2] Remove compile-member-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T028 [P] [US2] Remove compile-assoc function from src/clysm/compiler/codegen/func-section.lisp
- [x] T029 [P] [US2] Remove compile-assoc-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T030 [P] [US2] Remove compile-assoc-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T031 [P] [US2] Remove compile-rassoc function from src/clysm/compiler/codegen/func-section.lisp
- [x] T032 [P] [US2] Remove compile-rassoc-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T033 [P] [US2] Remove compile-rassoc-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T034 [P] [US2] Remove compile-find function from src/clysm/compiler/codegen/func-section.lisp
- [x] T035 [P] [US2] Remove compile-find-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T036 [P] [US2] Remove compile-find-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T037 [P] [US2] Remove compile-position function from src/clysm/compiler/codegen/func-section.lisp
- [x] T038 [P] [US2] Remove compile-position-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T039 [P] [US2] Remove compile-position-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T040 [US2] Run tests after list dead code removal via sbcl --eval "(asdf:test-system :clysm)"

### Remove Sequence Dead Code (~2,650 lines)

- [x] T041 [P] [US2] Remove compile-remove function from src/clysm/compiler/codegen/func-section.lisp
- [x] T042 [P] [US2] Remove compile-remove-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T043 [P] [US2] Remove compile-remove-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T044 [P] [US2] Remove compile-delete function from src/clysm/compiler/codegen/func-section.lisp
- [x] T045 [P] [US2] Remove compile-delete-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T046 [P] [US2] Remove compile-delete-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T047 [P] [US2] Remove compile-count function from src/clysm/compiler/codegen/func-section.lisp
- [x] T048 [P] [US2] Remove compile-count-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T049 [P] [US2] Remove compile-count-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T050 [P] [US2] Remove compile-substitute function from src/clysm/compiler/codegen/func-section.lisp
- [x] T051 [P] [US2] Remove compile-substitute-if function from src/clysm/compiler/codegen/func-section.lisp
- [x] T052 [P] [US2] Remove compile-substitute-if-not function from src/clysm/compiler/codegen/func-section.lisp
- [x] T053 [US2] Run tests after sequence dead code removal via sbcl --eval "(asdf:test-system :clysm)"

### Update Dispatch Tables

- [x] T054 [US2] Remove compile-* references from dispatch tables in src/clysm/compiler/codegen/func-section.lisp
- [x] T055 [US2] Run full test suite after dispatch table updates

### Verification

- [x] T056 [US2] Verify func-section.lisp line count via wc -l (target: <12,000 lines)

**Checkpoint**: User Story 2 complete - func-section.lisp reduced to <12,000 lines

---

## Phase 5: User Story 3 - Verify Compilation Rate (Priority: P1)

**Goal**: Verify Stage 1 compilation rate increased from 22.15% to 25%+

**Independent Test**: Check dist/stage1-report.json for compilation_rate >= 0.25

### Final Verification

- [x] T057 [US3] Run Stage 1 generation via sbcl --load build/stage1-complete.lisp
- [x] T058 [US3] Validate Wasm output via wasm-tools validate dist/clysm-stage1.wasm
- [x] T059 [US3] Extract compilation rate from dist/stage1-report.json and verify >= 25%
- [x] T060 [US3] Verify zero undefined function errors for 7 targeted functions in Stage 1 log

**Checkpoint**: User Story 3 complete - Stage 1 compilation rate >= 25%

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final cleanup and documentation

- [x] T061 Update CLAUDE.md with new feature entry in Recent Changes section
- [x] T062 Update COMPLETED-FEATURES.md in docs/features/ with 001-internal-function-consolidation
- [x] T063 Run final validation using quickstart.md commands
- [x] T064 Create summary of changes: exports added, lines removed, rate improvement

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion
- **User Story 1 (Phase 3)**: Depends on Foundational - Export functions
- **User Story 2 (Phase 4)**: Depends on User Story 1 - Cannot remove dead code until exports verified
- **User Story 3 (Phase 5)**: Depends on User Stories 1 & 2 - Measures combined result
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P2)**: Should start after US1 to ensure exports don't break during dead code removal
- **User Story 3 (P1)**: Measures combined result of US1 + US2

### Within Each User Story

- Parallel tasks [P] can run simultaneously (all target different functions in same file)
- Verification tasks must run sequentially after modifications
- Test runs after each category of changes

### Parallel Opportunities

**Phase 3 (US1)**: T008-T012 can run in parallel (5 verification tasks)
**Phase 4 (US2)**:
- T018-T023 can run in parallel (6 I/O removal tasks)
- T025-T039 can run in parallel (15 list removal tasks)
- T041-T052 can run in parallel (12 sequence removal tasks)

---

## Parallel Example: User Story 2 - I/O Dead Code Removal

```bash
# Launch all I/O dead code removal tasks in parallel:
Task: "Remove compile-princ function from src/clysm/compiler/codegen/func-section.lisp"
Task: "Remove compile-prin1 function from src/clysm/compiler/codegen/func-section.lisp"
Task: "Remove compile-print function from src/clysm/compiler/codegen/func-section.lisp"
Task: "Remove compile-write function from src/clysm/compiler/codegen/func-section.lisp"
Task: "Remove compile-format function from src/clysm/compiler/codegen/func-section.lisp"
Task: "Remove compile-terpri function from src/clysm/compiler/codegen/func-section.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (baseline verification)
2. Complete Phase 2: Foundational (package infrastructure)
3. Complete Phase 3: User Story 1 (export functions)
4. **STOP and VALIDATE**: Run Stage 1 and verify zero undefined function errors
5. If rate improved sufficiently, can deploy without dead code removal

### Incremental Delivery

1. Complete Setup + Foundational → Baseline established
2. Add User Story 1 → Test independently → Exports working (MVP!)
3. Add User Story 2 → Test independently → Dead code removed
4. Add User Story 3 → Verify final metrics → Success criteria met
5. Each story adds value without breaking previous stories

### Recommended Execution Order

1. T001-T004 (Setup) - 4 tasks
2. T005-T007 (Foundational) - 3 tasks
3. T008-T017 (US1: Exports) - 10 tasks
4. T018-T056 (US2: Dead Code) - 39 tasks
5. T057-T060 (US3: Verification) - 4 tasks
6. T061-T064 (Polish) - 4 tasks

---

## Notes

- [P] tasks = different functions in same file, can be done in parallel
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Run tests after each category of changes to catch regressions early
- Commit after each logical group of changes
- Stop at any checkpoint to validate story independently
- Total estimated line reduction: ~6,350 lines (18,351 → ~12,000)
