# Tasks: Common Lisp Sequence Functions

**Input**: Design documents from `/specs/007-sequence-functions/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: TDDが憲法で必須のため、各関数にテストを先行して作成

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- コンパイラ組み込み関数: `src/clysm/compiler/codegen/func-section.lisp`
- 統合テスト: `tests/integration/sequence-test.lisp`

---

## Phase 1: Setup

**Purpose**: テストインフラとファイル構造の準備

- [X] T001 Create test file scaffold in tests/integration/sequence-test.lisp
- [X] T002 [P] Add sequence function symbols to compiler's known-functions in src/clysm/compiler/codegen/func-section.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: すべてのシーケンス関数が依存するヘルパー関数

**⚠️ CRITICAL**: User Story実装前に完了必須

- [X] T003 Implement helper: generate-nil-check instructions in src/clysm/compiler/codegen/func-section.lisp
- [X] T004 [P] Implement helper: generate-list-loop-structure instructions in src/clysm/compiler/codegen/func-section.lisp
- [X] T005 Verify existing compile-and-run test infrastructure works in tests/integration/sequence-test.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Basic List Operations (Priority: P1) 🎯 MVP

**Goal**: length, append, reverse, nreverse, last, butlast, copy-list の7関数を実装

**Independent Test**: `(length '(1 2 3))` => 3、`(reverse '(1 2 3))` => (3 2 1) で検証

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T006 [P] [US1] Test: length - basic, empty, single in tests/integration/sequence-test.lisp
- [X] T007 [P] [US1] Test: append - two lists, empty, multiple in tests/integration/sequence-test.lisp
- [X] T008 [P] [US1] Test: reverse - basic, empty, single in tests/integration/sequence-test.lisp
- [X] T009 [P] [US1] Test: nreverse - basic, verify destructive in tests/integration/sequence-test.lisp
- [X] T010 [P] [US1] Test: last - basic, with n, empty in tests/integration/sequence-test.lisp
- [X] T011 [P] [US1] Test: butlast - basic, with n, empty in tests/integration/sequence-test.lisp
- [X] T012 [P] [US1] Test: copy-list - basic, verify independence in tests/integration/sequence-test.lisp

### Implementation for User Story 1

- [X] T013 [US1] Implement compile-length in src/clysm/compiler/codegen/func-section.lisp
- [X] T014 [US1] Implement compile-append in src/clysm/compiler/codegen/func-section.lisp
- [X] T015 [US1] Implement compile-reverse in src/clysm/compiler/codegen/func-section.lisp
- [X] T016 [US1] Implement compile-nreverse in src/clysm/compiler/codegen/func-section.lisp
- [X] T017 [US1] Implement compile-last in src/clysm/compiler/codegen/func-section.lisp
- [X] T018 [US1] Implement compile-butlast in src/clysm/compiler/codegen/func-section.lisp
- [X] T019 [US1] Implement compile-copy-list in src/clysm/compiler/codegen/func-section.lisp
- [X] T020 [US1] Register length/append/reverse/nreverse/last/butlast/copy-list in compile-primitive-call dispatcher

**Checkpoint**: User Story 1 complete - 7 basic list functions working

---

## Phase 4: User Story 2 - Higher-Order Functions (Priority: P1)

**Goal**: mapcar, mapc, maplist, reduce の4関数を実装

**Independent Test**: `(mapcar #'1+ '(1 2 3))` => (2 3 4)、`(reduce #'+ '(1 2 3 4))` => 10 で検証

### Tests for User Story 2

- [X] T021 [P] [US2] Test: mapcar - basic, empty, with lambda in tests/integration/sequence-test.lisp
- [X] T022 [P] [US2] Test: mapc - side effects, return value in tests/integration/sequence-test.lisp
- [X] T023 [P] [US2] Test: maplist - basic, cdrs application in tests/integration/sequence-test.lisp
- [X] T024 [P] [US2] Test: reduce - basic, with initial-value, empty in tests/integration/sequence-test.lisp

### Implementation for User Story 2

- [X] T025 [US2] Implement compile-mapcar in src/clysm/compiler/codegen/func-section.lisp
- [X] T026 [US2] Implement compile-mapc in src/clysm/compiler/codegen/func-section.lisp
- [X] T027 [US2] Implement compile-maplist in src/clysm/compiler/codegen/func-section.lisp
- [X] T028 [US2] Implement compile-reduce with :initial-value support in src/clysm/compiler/codegen/func-section.lisp
- [X] T029 [US2] Register mapcar/mapc/maplist/reduce in compile-primitive-call dispatcher

**Checkpoint**: User Story 2 complete - 4 higher-order functions working

---

## Phase 5: User Story 3 - Search and Filter Functions (Priority: P2)

**Goal**: find, find-if, position, position-if, remove, remove-if, remove-if-not, count, count-if の9関数を実装

**Independent Test**: `(find 2 '(1 2 3))` => 2、`(remove 2 '(1 2 3 2))` => (1 3) で検証

### Tests for User Story 3

- [X] T030 [P] [US3] Test: find - found, not found, nil element in tests/integration/sequence-test.lisp
- [X] T031 [P] [US3] Test: find-if - basic, not found in tests/integration/sequence-test.lisp
- [X] T032 [P] [US3] Test: position - found, not found, 0-indexed in tests/integration/sequence-test.lisp
- [X] T033 [P] [US3] Test: position-if - basic, not found in tests/integration/sequence-test.lisp
- [X] T034 [P] [US3] Test: remove - single, multiple, non-destructive in tests/integration/sequence-test.lisp
- [X] T035 [P] [US3] Test: remove-if - basic, empty result in tests/integration/sequence-test.lisp
- [X] T036 [P] [US3] Test: remove-if-not - basic, filter behavior in tests/integration/sequence-test.lisp
- [X] T037 [P] [US3] Test: count - none, single, multiple in tests/integration/sequence-test.lisp
- [X] T038 [P] [US3] Test: count-if - basic, none matching in tests/integration/sequence-test.lisp

### Implementation for User Story 3

- [X] T039 [US3] Implement compile-find in src/clysm/compiler/codegen/func-section.lisp
- [X] T040 [US3] Implement compile-find-if in src/clysm/compiler/codegen/func-section.lisp
- [X] T041 [US3] Implement compile-position in src/clysm/compiler/codegen/func-section.lisp
- [X] T042 [US3] Implement compile-position-if in src/clysm/compiler/codegen/func-section.lisp
- [X] T043 [US3] Implement compile-remove in src/clysm/compiler/codegen/func-section.lisp
- [X] T044 [US3] Implement compile-remove-if in src/clysm/compiler/codegen/func-section.lisp
- [X] T045 [US3] Implement compile-remove-if-not in src/clysm/compiler/codegen/func-section.lisp
- [X] T046 [US3] Implement compile-count in src/clysm/compiler/codegen/func-section.lisp
- [X] T047 [US3] Implement compile-count-if in src/clysm/compiler/codegen/func-section.lisp
- [X] T048 [US3] Register find/find-if/position/position-if/remove/remove-if/remove-if-not/count/count-if in dispatcher

**Checkpoint**: User Story 3 complete - 9 search/filter functions working

---

## Phase 6: User Story 4 - List Membership and Association (Priority: P3)

**Goal**: member, assoc, rassoc の3関数を実装 (Tier 4オプションだが基本的なもの)

**Independent Test**: `(member 2 '(1 2 3))` => (2 3)、`(assoc 'a '((a . 1)))` => (a . 1) で検証

### Tests for User Story 4

- [X] T049 [P] [US4] Test: member - found, not found, returns tail in tests/integration/sequence-test.lisp
- [X] T050 [P] [US4] Test: assoc - found, not found, nil key in tests/integration/sequence-test.lisp
- [X] T051 [P] [US4] Test: rassoc - found, not found in tests/integration/sequence-test.lisp

### Implementation for User Story 4

- [X] T052 [US4] Implement compile-member in src/clysm/compiler/codegen/func-section.lisp
- [X] T053 [US4] Implement compile-assoc in src/clysm/compiler/codegen/func-section.lisp
- [X] T054 [US4] Implement compile-rassoc in src/clysm/compiler/codegen/func-section.lisp
- [X] T055 [US4] Register member/assoc/rassoc in compile-primitive-call dispatcher

**Checkpoint**: User Story 4 complete - 3 membership/association functions working

---

## Phase 7: User Story 5 - Quantifier Predicates (Priority: P3)

**Goal**: every, some, notany, notevery の4関数を実装 (Tier 4オプション)

**Independent Test**: `(every #'numberp '(1 2 3))` => t、`(some #'evenp '(1 2 3))` => 2 で検証

### Tests for User Story 5

- [X] T056 [P] [US5] Test: every - all true, one false, empty in tests/integration/sequence-test.lisp
- [X] T057 [P] [US5] Test: some - found, not found, returns value in tests/integration/sequence-test.lisp
- [X] T058 [P] [US5] Test: notany - none match, one matches in tests/integration/sequence-test.lisp
- [X] T059 [P] [US5] Test: notevery - one fails, all pass in tests/integration/sequence-test.lisp

### Implementation for User Story 5

- [X] T060 [US5] Implement compile-every in src/clysm/compiler/codegen/func-section.lisp
- [X] T061 [US5] Implement compile-some in src/clysm/compiler/codegen/func-section.lisp
- [X] T062 [US5] Implement compile-notany in src/clysm/compiler/codegen/func-section.lisp
- [X] T063 [US5] Implement compile-notevery in src/clysm/compiler/codegen/func-section.lisp
- [X] T064 [US5] Register every/some/notany/notevery in compile-primitive-call dispatcher

**Checkpoint**: User Story 5 complete - 4 quantifier predicates working

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: 品質向上と最終検証

- [X] T065 [P] Add edge case tests: empty list, single element, 100+ elements in tests/integration/sequence-test.lisp
- [X] T066 [P] Add nested list tests in tests/integration/sequence-test.lisp
- [X] T067 [P] Add nil-element-in-list tests in tests/integration/sequence-test.lisp
- [X] T068 Verify all existing tests still pass (list-test.lisp, closure-test.lisp)
- [X] T069 Run wasm-tools validate on generated output
- [X] T070 Run nix flake check for final validation
- [X] T071 Update quickstart.md progress checklist in specs/007-sequence-functions/quickstart.md

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1 (P1) and US2 (P1) are both priority 1, can be worked in parallel
  - US3 (P2) can start after Foundational
  - US4 (P3) and US5 (P3) can start after Foundational
- **Polish (Phase 8)**: Depends on at least US1-US3 being complete (Tier 1-3 必須)

### User Story Dependencies

- **User Story 1 (P1)**: Foundation → 基本リスト操作 (他に依存なし)
- **User Story 2 (P1)**: Foundation → 高階関数 (US1と独立、ただしreverseをmapcar内部で使う場合はUS1先行)
- **User Story 3 (P2)**: Foundation → 探索/フィルタ (US1/US2と独立)
- **User Story 4 (P3)**: Foundation → membership/assoc (US3のfindと類似だが独立)
- **User Story 5 (P3)**: Foundation → quantifiers (US2のmapcarパターンを流用可能)

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Implementation tasks are sequential within each function
- Register in dispatcher after all functions in that story are implemented

### Parallel Opportunities

- All tests within a story marked [P] can run in parallel
- US1 and US2 are both P1 and can be worked in parallel
- US4 and US5 are both P3 and can be worked in parallel

---

## Implementation Summary

**Status**: ✅ COMPLETE (71/71 tasks)

### Completed:
- **Phase 1**: Setup complete
- **Phase 2**: Foundational complete
- **Phase 3**: User Story 1 - 7 basic list functions implemented
- **Phase 4**: User Story 2 - 4 higher-order functions implemented
- **Phase 5**: User Story 3 - 9 search/filter functions implemented
- **Phase 6**: User Story 4 - 3 membership/association functions implemented
- **Phase 7**: User Story 5 - 4 quantifier predicates implemented
- **Phase 8**: Polish complete - all validation passed

### Test Results:
- **54/54 sequence tests passing**
- **All existing tests still pass**
- **nix flake check passed**

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD required by constitution - tests MUST fail before implementation
- Commit after each function implementation (test + impl)
- Stop at any checkpoint to validate story independently
- Tier 4 (US4, US5) is optional but recommended
