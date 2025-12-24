# Tasks: ANSI Common Lispパッケージシステム

**Input**: Design documents from `/specs/013-package-system/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/package-api.md

**Tests**: TDD必須（Constitution VII）- 全機能にテストファースト

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/reader/`, `src/clysm/lib/`, `src/clysm/runtime/`
- **Tests**: `tests/unit/`, `tests/integration/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project structure verification and test infrastructure

- [X] T001 Verify existing package system implementation in src/clysm/reader/package.lisp
- [X] T002 [P] Create tests/unit/tokenizer-package-test.lisp test file scaffold
- [X] T003 [P] Create tests/integration/package-integration-test.lisp test file scaffold

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Extend Package structure - MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Tests for Foundational

- [X] T004 [P] Write test for extended package structure (internal/external symbols) in tests/unit/package-test.lisp
- [X] T005 [P] Write test for package-designator conversion (string/keyword/package) in tests/unit/package-test.lisp

### Implementation for Foundational

- [X] T006 Extend package structure to separate internal-symbols and external-symbols hash tables in src/clysm/reader/package.lisp
- [X] T007 Add use-list, used-by-list, shadowing-symbols fields to package structure in src/clysm/reader/package.lisp
- [X] T008 Implement package-designator-to-package helper function in src/clysm/reader/package.lisp
- [X] T009 Implement package-error condition in src/clysm/reader/package.lisp
- [X] T010 Update existing intern-symbol to use internal-symbols table in src/clysm/reader/package.lisp

**Checkpoint**: Foundation ready - Package structure extended, user story implementation can begin

---

## Phase 3: User Story 1 - パッケージの定義と切り替え (Priority: P1) 🎯 MVP

**Goal**: defpackageでパッケージを定義し、in-packageでカレントパッケージを切り替えられる

**Independent Test**: `(defpackage :my-pkg) (in-package :my-pkg) foo` → `MY-PKG::FOO`

### Tests for User Story 1 (TDD Required)

- [X] T011 [P] [US1] Write test for make-package with :nicknames :use options in tests/unit/package-test.lisp
- [X] T012 [P] [US1] Write test for find-package by name and nickname in tests/unit/package-test.lisp
- [X] T013 [P] [US1] Write test for delete-package in tests/unit/package-test.lisp
- [X] T014 [P] [US1] Write test for rename-package in tests/unit/package-test.lisp
- [X] T015 [P] [US1] Write test for list-all-packages in tests/unit/package-test.lisp
- [X] T016 [P] [US1] Write test for defpackage macro in tests/integration/package-integration-test.lisp
- [X] T017 [P] [US1] Write test for in-package macro in tests/integration/package-integration-test.lisp

### Implementation for User Story 1

- [X] T018 [US1] Implement make-package function with :nicknames :use in src/clysm/reader/package.lisp
- [X] T019 [US1] Update find-package* to accept package-designators in src/clysm/reader/package.lisp
- [X] T020 [US1] Implement delete-package function in src/clysm/reader/package.lisp
- [X] T021 [US1] Implement rename-package function in src/clysm/reader/package.lisp
- [X] T022 [US1] Implement list-all-packages function in src/clysm/reader/package.lisp
- [X] T023 [US1] Create src/clysm/lib/package-macros.lisp with defpackage macro
- [X] T024 [US1] Implement in-package macro in src/clysm/lib/package-macros.lisp
- [X] T025 [US1] Register package-macros.lisp in ASDF system definition

**Checkpoint**: User Story 1 complete - defpackage/in-package functional

---

## Phase 4: User Story 2 - シンボルのエクスポートとインポート (Priority: P1)

**Goal**: export/import/unexportでシンボルをパッケージ間で共有できる

**Independent Test**: `(export 'foo :pkg-a) (import 'pkg-a:foo :pkg-b)` → `PKG-B`から`FOO`にアクセス可能

### Tests for User Story 2 (TDD Required)

- [X] T026 [P] [US2] Write test for export moving symbol to external table in tests/unit/package-test.lisp
- [X] T027 [P] [US2] Write test for unexport moving symbol back to internal in tests/unit/package-test.lisp
- [X] T028 [P] [US2] Write test for import adding foreign symbol in tests/unit/package-test.lisp
- [X] T029 [P] [US2] Write test for import name conflict detection in tests/unit/package-test.lisp
- [X] T030 [P] [US2] Write test for shadow creating shadowing symbol in tests/unit/package-test.lisp
- [X] T031 [P] [US2] Write test for shadowing-import overwriting in tests/unit/package-test.lisp
- [X] T032 [P] [US2] Write test for defpackage :export option in tests/integration/package-integration-test.lisp
- [X] T033 [P] [US2] Write test for defpackage :import-from option in tests/integration/package-integration-test.lisp

### Implementation for User Story 2

- [X] T034 [US2] Implement export function in src/clysm/reader/package.lisp
- [X] T035 [US2] Implement unexport function in src/clysm/reader/package.lisp
- [X] T036 [US2] Implement import function with name conflict check in src/clysm/reader/package.lisp
- [X] T037 [US2] Implement shadow function in src/clysm/reader/package.lisp
- [X] T038 [US2] Implement shadowing-import function in src/clysm/reader/package.lisp
- [X] T039 [US2] Update defpackage to support :export option in src/clysm/lib/package-macros.lisp
- [X] T040 [US2] Update defpackage to support :import-from option in src/clysm/lib/package-macros.lisp
- [X] T041 [US2] Update defpackage to support :shadow option in src/clysm/lib/package-macros.lisp

**Checkpoint**: User Story 2 complete - export/import functional

---

## Phase 5: User Story 3 - パッケージ修飾子によるシンボル参照 (Priority: P1)

**Goal**: `cl:car`, `pkg::internal`, `:keyword` 形式でシンボルを参照できる

**Independent Test**: `cl:car` → `CL`パッケージの`CAR`シンボル

### Tests for User Story 3 (TDD Required)

- [X] T042 [P] [US3] Write test for tokenizing pkg:symbol (single colon) in tests/unit/tokenizer-package-test.lisp
- [X] T043 [P] [US3] Write test for tokenizing pkg::symbol (double colon) in tests/unit/tokenizer-package-test.lisp
- [X] T044 [P] [US3] Write test for tokenizing :keyword (leading colon) in tests/unit/tokenizer-package-test.lisp
- [X] T045 [P] [US3] Write test for error on ::symbol (invalid) in tests/unit/tokenizer-package-test.lisp
- [X] T046 [P] [US3] Write test for error on pkg: (no symbol name) in tests/unit/tokenizer-package-test.lisp
- [X] T047 [P] [US3] Write test for parsing qualified-symbol token in tests/unit/parser-test.lisp
- [X] T048 [P] [US3] Write test for external symbol export check on single colon in tests/integration/package-integration-test.lisp

### Implementation for User Story 3

- [X] T049 [US3] Modify read-number-or-symbol to detect colons in src/clysm/reader/tokenizer.lisp
- [X] T050 [US3] Implement read-qualified-symbol function in src/clysm/reader/tokenizer.lisp
- [X] T051 [US3] Generate :qualified-symbol token with :package-name :symbol-name :external in src/clysm/reader/tokenizer.lisp
- [X] T052 [US3] Handle edge cases (::foo, pkg:, empty symbol) in src/clysm/reader/tokenizer.lisp
- [X] T053 [US3] Add parse-qualified-symbol to parser in src/clysm/reader/parser.lisp
- [X] T054 [US3] Implement external symbol export check (single colon) in src/clysm/reader/parser.lisp
- [X] T055 [US3] Update parse-atom to handle :qualified-symbol token type in src/clysm/reader/parser.lisp

**Checkpoint**: User Story 3 complete - package qualifiers work in reader

---

## Phase 6: User Story 4 - 標準パッケージの利用 (Priority: P2)

**Goal**: CL, CL-USER, KEYWORDパッケージがシステム起動時に存在する

**Independent Test**: `(find-package :cl)` → `#<PACKAGE "COMMON-LISP">`

### Tests for User Story 4 (TDD Required)

- [X] T056 [P] [US4] Write test for COMMON-LISP package exists with nickname CL in tests/unit/package-test.lisp
- [X] T057 [P] [US4] Write test for COMMON-LISP-USER package exists using CL in tests/unit/package-test.lisp
- [X] T058 [P] [US4] Write test for KEYWORD package exists in tests/unit/package-test.lisp
- [X] T059 [P] [US4] Write test for *package* initialized to CL-USER in tests/unit/package-test.lisp
- [X] T060 [P] [US4] Write test for keyword auto-export and self-evaluation in tests/unit/package-test.lisp
- [X] T061 [P] [US4] Write test for CL symbols accessible from CL-USER in tests/integration/package-integration-test.lisp

### Implementation for User Story 4

- [X] T062 [US4] Create src/clysm/runtime/package-init.lisp for standard package setup (implemented in package.lisp)
- [X] T063 [US4] Implement initialize-common-lisp-package with CL symbols export in src/clysm/runtime/package-init.lisp
- [X] T064 [US4] Implement initialize-keyword-package with auto-export in src/clysm/runtime/package-init.lisp
- [X] T065 [US4] Implement initialize-cl-user-package with :use CL in src/clysm/runtime/package-init.lisp
- [X] T066 [US4] Update initialize-packages to use new functions in src/clysm/reader/package.lisp
- [X] T067 [US4] Export implemented CL functions/macros (car, cdr, cons, etc.) in src/clysm/runtime/package-init.lisp
- [X] T068 [US4] Register package-init.lisp in ASDF system definition (functionality in package.lisp)

**Checkpoint**: User Story 4 complete - standard packages available

---

## Phase 7: User Story 5 - シンボルインターンとルックアップ (Priority: P2)

**Goal**: intern/find-symbol/uninternで多値返却対応のシンボル操作ができる

**Independent Test**: `(intern "FOO" :pkg)` → `PKG::FOO, NIL` (多値)

### Tests for User Story 5 (TDD Required)

- [X] T069 [P] [US5] Write test for intern returning multiple values (symbol, status) in tests/unit/package-test.lisp
- [X] T070 [P] [US5] Write test for intern returning :internal for new symbol in tests/unit/package-test.lisp
- [X] T071 [P] [US5] Write test for intern returning :external for exported symbol in tests/unit/package-test.lisp
- [X] T072 [P] [US5] Write test for intern returning :inherited for used symbol in tests/unit/package-test.lisp
- [X] T073 [P] [US5] Write test for find-symbol not creating new symbol in tests/unit/package-test.lisp
- [X] T074 [P] [US5] Write test for find-symbol returning nil, nil for missing in tests/unit/package-test.lisp
- [X] T075 [P] [US5] Write test for unintern removing symbol in tests/unit/package-test.lisp

### Implementation for User Story 5

- [X] T076 [US5] Implement intern function with multiple value return in src/clysm/reader/package.lisp
- [X] T077 [US5] Implement inherited symbol search in use-list for intern in src/clysm/reader/package.lisp
- [X] T078 [US5] Implement find-symbol function (search only, no create) in src/clysm/reader/package.lisp
- [X] T079 [US5] Implement unintern function in src/clysm/reader/package.lisp
- [X] T080 [US5] Update intern-symbol to use new intern implementation in src/clysm/reader/package.lisp

**Checkpoint**: User Story 5 complete - full intern/find-symbol/unintern API

---

## Phase 8: User Story 6 - use-packageによるパッケージ継承 (Priority: P3)

**Goal**: use-package/unuse-packageでパッケージのエクスポートシンボルを継承できる

**Independent Test**: `(use-package :pkg-a :pkg-b)` → `PKG-B`から`PKG-A`のエクスポートシンボルにアクセス

### Tests for User Story 6 (TDD Required)

- [X] T081 [P] [US6] Write test for use-package adding to use-list in tests/unit/package-test.lisp
- [X] T082 [P] [US6] Write test for use-package updating used-by-list in tests/unit/package-test.lisp
- [X] T083 [P] [US6] Write test for use-package name conflict detection in tests/unit/package-test.lisp
- [X] T084 [P] [US6] Write test for unuse-package removing from use-list in tests/unit/package-test.lisp
- [X] T085 [P] [US6] Write test for defpackage :use option in tests/integration/package-integration-test.lisp
- [X] T086 [P] [US6] Write test for inherited symbol access after use-package in tests/integration/package-integration-test.lisp

### Implementation for User Story 6

- [X] T087 [US6] Implement use-package function with conflict detection in src/clysm/reader/package.lisp
- [X] T088 [US6] Implement use-package updating used-by-list in src/clysm/reader/package.lisp
- [X] T089 [US6] Implement unuse-package function in src/clysm/reader/package.lisp
- [X] T090 [US6] Update defpackage to support :use option with use-package in src/clysm/lib/package-macros.lisp

**Checkpoint**: User Story 6 complete - use-package inheritance works

---

## Phase 9: Package Information & Polish

**Purpose**: Package information accessors and cross-cutting concerns

### Tests for Package Information

- [X] T091 [P] Write test for package-name function in tests/unit/package-test.lisp
- [X] T092 [P] Write test for package-nicknames function in tests/unit/package-test.lisp
- [X] T093 [P] Write test for package-use-list function in tests/unit/package-test.lisp
- [X] T094 [P] Write test for package-used-by-list function in tests/unit/package-test.lisp
- [X] T095 [P] Write test for package-shadowing-symbols function in tests/unit/package-test.lisp
- [X] T096 [P] Write test for packagep predicate in tests/unit/package-test.lisp
- [X] T097 [P] Write test for symbol-package function in tests/unit/package-test.lisp

### Implementation for Package Information

- [X] T098 [P] Implement package-name function in src/clysm/reader/package.lisp
- [X] T099 [P] Implement package-nicknames function in src/clysm/reader/package.lisp
- [X] T100 [P] Implement package-use-list function in src/clysm/reader/package.lisp
- [X] T101 [P] Implement package-used-by-list function in src/clysm/reader/package.lisp
- [X] T102 [P] Implement package-shadowing-symbols function in src/clysm/reader/package.lisp
- [X] T103 [P] Implement packagep predicate in src/clysm/reader/package.lisp
- [X] T104 [P] Implement symbol-package function in src/clysm/reader/package.lisp

### Polish & Cross-Cutting

- [X] T105 Run all tests and verify pass in tests/
- [X] T106 Run quickstart.md validation scenarios
- [X] T107 Update existing code to use new package API where appropriate
- [X] T108 Verify nix flake check passes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-8)**: All depend on Foundational completion
  - US1-US3 are all P1 (can work in parallel after Foundational)
  - US4-US5 are P2 (can start after US1-US3, or in parallel)
  - US6 is P3 (can start after US4-US5, or in parallel)
- **Polish (Phase 9)**: Depends on all user stories being complete

### User Story Dependencies

| Story | Priority | Depends On | Can Parallel With |
|-------|----------|------------|-------------------|
| US1 | P1 | Foundational | US2, US3 |
| US2 | P1 | Foundational, (uses US1's make-package) | US1, US3 |
| US3 | P1 | Foundational | US1, US2 |
| US4 | P2 | Foundational, US1 (package creation) | US5 |
| US5 | P2 | Foundational, US2 (export for :external status) | US4, US6 |
| US6 | P3 | Foundational, US2 (export), US4 (CL package) | - |

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Package structure extensions before functions
- Functions before macros
- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

**Setup Phase:**
```
T002 [P] tokenizer-package-test.lisp scaffold
T003 [P] package-integration-test.lisp scaffold
```

**Foundational Phase:**
```
T004 [P] package structure test
T005 [P] package-designator test
```

**US3 Tests (all parallel):**
```
T042-T048 [P] All tokenizer and parser tests
```

**Phase 9 Package Info (all parallel):**
```
T091-T097 [P] All package info tests
T098-T104 [P] All package info implementations
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 4)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL)
3. Complete Phase 3: User Story 1 (defpackage/in-package)
4. Complete Phase 6: User Story 4 (standard packages)
5. **STOP and VALIDATE**: Test basic package creation and standard packages
6. Deploy/demo if ready - this gives working namespace management

### Full Implementation

1. Complete MVP (US1 + US4)
2. Add US2 (export/import) - enables module APIs
3. Add US3 (package qualifiers) - enables `cl:car` syntax
4. Add US5 (intern/find-symbol) - enables metaprogramming
5. Add US6 (use-package) - enables package inheritance
6. Add Phase 9 (polish) - complete package info API

### Incremental Delivery

Each user story adds value:
- **After US1**: Can create and switch packages
- **After US1+US4**: Standard CL packages available
- **After US2**: Can export/import symbols between packages
- **After US3**: Full ANSI CL reader syntax for packages
- **After US5**: Dynamic symbol manipulation
- **After US6**: Package inheritance hierarchy

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- TDD Required: All tests must fail before implementation (Constitution VII)
- Each user story should be independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
