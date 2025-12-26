# Tasks: ANSI Common Lisp Equality Predicates and Logical Operators

**Input**: Design documents from `/specs/024-equality-predicates/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/, quickstart.md

**Tests**: REQUIRED - TDD methodology mandated by Constitution Principle VII (non-negotiable)

**Organization**: Tasks grouped by user story priority (P1 → P2 → P3). Within P1, ordered by dependency: not → eq → eql → and/or.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US5, US6)
- Include exact file paths in descriptions

## Path Conventions

- **Implementation**: `src/clysm/compiler/codegen/func-section.lisp`
- **Unit tests**: `tests/unit/`
- **Contract tests**: `tests/contract/`
- **Integration tests**: `tests/integration/`

---

## Phase 1: Setup

**Purpose**: Create test file stubs and verify dependencies

- [x] T001 Create unit test file stub at tests/unit/equality-predicates-test.lisp
- [x] T002 [P] Create unit test file stub at tests/unit/logical-operators-test.lisp
- [x] T003 [P] Create contract test file stub at tests/contract/equality-wasm-test.lisp
- [x] T004 [P] Create integration test file stub at tests/integration/equality-ansi-test.lisp
- [x] T005 Verify dependencies exist: char=, char-equal, string=, string-equal from feature 008 in src/clysm/compiler/codegen/func-section.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Shared utilities for boolean result wrapping

**⚠️ CRITICAL**: These patterns are used by ALL equality predicates

- [x] T006 Add wrap-boolean-result helper function in src/clysm/compiler/codegen/func-section.lisp (converts i32 0/1 to T/NIL)
- [x] T007 Verify NIL singleton global is accessible as $nil in src/clysm/compiler/codegen/func-section.lisp
- [x] T008 Verify T representation (ref.i31 (i32.const 1)) pattern works in existing type predicates

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 5 - Logical Negation with not (Priority: P1) 🎯 MVP-1

**Goal**: Implement `not` predicate that returns T if argument is NIL, NIL otherwise

**Independent Test**: `(not nil)` → T, `(not t)` → NIL, `(not 42)` → NIL

**Why First**: Simplest predicate, establishes boolean result pattern for all others

### Tests for US5 (Write FIRST, verify FAIL)

- [x] T009 [US5] Write test not-nil: (not nil) returns T in tests/unit/equality-predicates-test.lisp
- [x] T010 [P] [US5] Write test not-t: (not t) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T011 [P] [US5] Write test not-non-nil: (not 42) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T012 [P] [US5] Write test not-symbol: (not 'foo) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T013 [US5] Run tests and verify all FAIL (Red phase)

### Implementation for US5

- [x] T014 [US5] Implement compile-not function in src/clysm/compiler/codegen/func-section.lisp
- [x] T015 [US5] Add NOT case to compile-primitive-call dispatcher in src/clysm/compiler/codegen/func-section.lisp
- [x] T016 [US5] Run tests and verify all PASS (Green phase)
- [x] T017 [P] [US5] Write contract test: not generates ref.eq with NIL in tests/contract/equality-wasm-test.lisp
- [x] T018 [US5] Run wasm-tools validate on compiled (not x) output

**Checkpoint**: `not` is fully functional and tested

---

## Phase 4: User Story 1 - Pointer Identity with eq (Priority: P1) 🎯 MVP-2

**Goal**: Implement `eq` predicate using Wasm `ref.eq` for identity comparison

**Independent Test**: `(eq 'a 'a)` → T, `(eq 'a 'b)` → NIL, `(eq (cons 1 2) (cons 1 2))` → NIL

### Tests for US1 (Write FIRST, verify FAIL)

- [x] T019 [US1] Write test eq-same-symbol: (eq 'a 'a) returns T in tests/unit/equality-predicates-test.lisp
- [x] T020 [P] [US1] Write test eq-different-symbols: (eq 'a 'b) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T021 [P] [US1] Write test eq-same-fixnum: (eq 42 42) returns T in tests/unit/equality-predicates-test.lisp
- [x] T022 [P] [US1] Write test eq-nil-nil: (eq nil nil) returns T in tests/unit/equality-predicates-test.lisp
- [x] T023 [P] [US1] Write test eq-different-cons: (eq (cons 1 2) (cons 1 2)) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T024 [US1] Run tests and verify all FAIL (Red phase)

### Implementation for US1

- [x] T025 [US1] Implement compile-eq function in src/clysm/compiler/codegen/func-section.lisp
- [x] T026 [US1] Add EQ case to compile-primitive-call dispatcher in src/clysm/compiler/codegen/func-section.lisp
- [x] T027 [US1] Run tests and verify all PASS (Green phase)
- [x] T028 [P] [US1] Write contract test: eq generates single ref.eq instruction in tests/contract/equality-wasm-test.lisp
- [x] T029 [US1] Run wasm-tools validate on compiled (eq x y) output

**Checkpoint**: `eq` is fully functional and tested

---

## Phase 5: User Story 2 - Type-Aware Value Comparison with eql (Priority: P1)

**Goal**: Implement `eql` with type dispatch - same type + same value for numbers, char= for characters

**Independent Test**: `(eql 1 1)` → T, `(eql 1 1.0)` → NIL, `(eql 3.14 3.14)` → T

**Depends On**: eq (for fallback and fixnum comparison)

### Tests for US2 (Write FIRST, verify FAIL)

- [x] T030 [US2] Write test eql-same-fixnum: (eql 42 42) returns T in tests/unit/equality-predicates-test.lisp
- [x] T031 [P] [US2] Write test eql-different-fixnum: (eql 42 43) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T032 [P] [US2] Write test eql-cross-type-fixnum-float: (eql 1 1.0) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T033 [P] [US2] Write test eql-same-float: (eql 3.14 3.14) returns T in tests/unit/equality-predicates-test.lisp
- [x] T034 [P] [US2] Write test eql-same-ratio: (eql 1/2 1/2) returns T in tests/unit/equality-predicates-test.lisp
- [x] T035 [P] [US2] Write test eql-same-char: (eql #\a #\a) returns T in tests/unit/equality-predicates-test.lisp
- [x] T036 [P] [US2] Write test eql-different-char: (eql #\a #\A) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T037 [P] [US2] Write test eql-fallback-eq: (eql 'a 'a) returns T in tests/unit/equality-predicates-test.lisp
- [x] T038 [US2] Run tests and verify all FAIL (Red phase)

### Implementation for US2

- [x] T039 [US2] Implement compile-eql function with type dispatch in src/clysm/compiler/codegen/func-section.lisp
- [x] T040 [US2] Add fixnum branch (i31ref ref.eq) in compile-eql
- [x] T041 [US2] Add float branch (f64.eq after struct.get) in compile-eql
- [x] T042 [US2] Add ratio branch (compare numerator and denominator) in compile-eql
- [x] T043 [US2] Add character branch (i31ref ref.eq) in compile-eql
- [x] T044 [US2] Add fallback to ref.eq for other types in compile-eql
- [x] T045 [US2] Add EQL case to compile-primitive-call dispatcher in src/clysm/compiler/codegen/func-section.lisp
- [x] T046 [US2] Run tests and verify all PASS (Green phase)
- [x] T047 [P] [US2] Write contract test: eql generates ref.test for type dispatch in tests/contract/equality-wasm-test.lisp
- [x] T048 [US2] Run wasm-tools validate on compiled (eql x y) output

**Checkpoint**: `eql` is fully functional with type-aware comparison

---

## Phase 6: User Story 6 - Short-Circuit Boolean with and/or (Priority: P1)

**Goal**: Verify and/or special forms compile correctly (already parsed to nested if)

**Independent Test**: `(and t nil t)` → NIL (short-circuits), `(or nil 5 99)` → 5

**Note**: Parser already transforms and/or to nested if; this phase verifies correct compilation

### Tests for US6 (Write FIRST, verify behavior)

- [x] T049 [US6] Write test and-all-true: (and t t t) returns T in tests/unit/logical-operators-test.lisp
- [x] T050 [P] [US6] Write test and-short-circuit: (and t nil t) returns NIL in tests/unit/logical-operators-test.lisp
- [x] T051 [P] [US6] Write test and-returns-last: (and 1 2 3) returns 3 in tests/unit/logical-operators-test.lisp
- [x] T052 [P] [US6] Write test and-empty: (and) returns T in tests/unit/logical-operators-test.lisp
- [x] T053 [P] [US6] Write test or-first-non-nil: (or nil nil t) returns T in tests/unit/logical-operators-test.lisp
- [x] T054 [P] [US6] Write test or-short-circuit: (or nil 5 99) returns 5 in tests/unit/logical-operators-test.lisp
- [x] T055 [P] [US6] Write test or-empty: (or) returns NIL in tests/unit/logical-operators-test.lisp
- [x] T056 [US6] Run tests and verify behavior

### Verification for US6

- [x] T057 [US6] Verify parse-and-form exists and transforms correctly in src/clysm/compiler/ast.lisp
- [x] T058 [US6] Verify parse-or-form exists and transforms correctly in src/clysm/compiler/ast.lisp
- [x] T059 [US6] Verify (and) edge case produces T in AST transformation
- [x] T060 [US6] Verify (or) edge case produces NIL in AST transformation
- [x] T061 [P] [US6] Write contract test: and compiles to nested if structure in tests/contract/equality-wasm-test.lisp
- [x] T062 [US6] Run wasm-tools validate on compiled and/or expressions

**Checkpoint**: `and`/`or` compile correctly with short-circuit semantics

---

## Phase 7: User Story 3 - Structural Equality with equal (Priority: P2)

**Goal**: Implement `equal` with recursive cons/string comparison, fallback to eql

**Independent Test**: `(equal '(1 2 3) '(1 2 3))` → T, `(equal "hello" "hello")` → T

**Depends On**: eql (for fallback), string= (from feature 008)

### Tests for US3 (Write FIRST, verify FAIL)

- [x] T063 [US3] Write test equal-same-list: (equal '(1 2 3) '(1 2 3)) returns T in tests/unit/equality-predicates-test.lisp
- [x] T064 [P] [US3] Write test equal-different-list: (equal '(1 2 3) '(1 2 4)) returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T065 [P] [US3] Write test equal-nested-list: (equal '((a b) c) '((a b) c)) returns T in tests/unit/equality-predicates-test.lisp
- [x] T066 [P] [US3] Write test equal-strings: (equal "hello" "hello") returns T in tests/unit/equality-predicates-test.lisp
- [x] T067 [P] [US3] Write test equal-different-strings: (equal "hello" "world") returns NIL in tests/unit/equality-predicates-test.lisp
- [x] T068 [P] [US3] Write test equal-fallback-eql: (equal 42 42) returns T in tests/unit/equality-predicates-test.lisp
- [x] T069 [P] [US3] Write test equal-empty-lists: (equal nil nil) returns T in tests/unit/equality-predicates-test.lisp
- [x] T070 [US3] Run tests and verify all FAIL (Red phase)

### Implementation for US3

- [x] T071 [US3] Implement compile-equal function with cons/string dispatch in src/clysm/compiler/codegen/func-section.lisp
- [x] T072 [US3] Add cons branch with recursive car/cdr comparison in compile-equal
- [x] T073 [US3] Add string branch delegating to string= in compile-equal
- [x] T074 [US3] Add fallback to eql for other types in compile-equal
- [x] T075 [US3] Add EQUAL case to compile-primitive-call dispatcher in src/clysm/compiler/codegen/func-section.lisp
- [x] T076 [US3] Run tests and verify all PASS (Green phase)
- [x] T077 [P] [US3] Write contract test: equal generates call $equal for recursion in tests/contract/equality-wasm-test.lisp
- [x] T078 [US3] Run wasm-tools validate on compiled (equal x y) output

**Checkpoint**: `equal` is fully functional with structural comparison

---

## Phase 8: User Story 4 - Case-Insensitive Equality with equalp (Priority: P3)

**Goal**: Implement `equalp` with case-insensitive char/string and type-coercing numeric comparison

**Independent Test**: `(equalp "HELLO" "hello")` → T, `(equalp 1 1.0)` → T

**Depends On**: equal (for structure), char-equal, string-equal (from feature 008), numeric = (from feature 010)

### Tests for US4 (Write FIRST, verify FAIL)

- [x] T079 [US4] Write test equalp-case-insensitive-string: (equalp "HELLO" "hello") returns T in tests/unit/equality-predicates-test.lisp
- [x] T080 [P] [US4] Write test equalp-case-insensitive-char: (equalp #\A #\a) returns T in tests/unit/equality-predicates-test.lisp
- [x] T081 [P] [US4] Write test equalp-numeric-coercion: (equalp 1 1.0) returns T in tests/unit/equality-predicates-test.lisp
- [x] T082 [P] [US4] Write test equalp-nested-case-insensitive: (equalp '("A" "B") '("a" "b")) returns T in tests/unit/equality-predicates-test.lisp
- [x] T083 [P] [US4] Write test equalp-fallback-equal: (equalp '(1 2) '(1 2)) returns T in tests/unit/equality-predicates-test.lisp
- [x] T084 [US4] Run tests and verify all FAIL (Red phase)

### Implementation for US4

- [x] T085 [US4] Implement compile-equalp function in src/clysm/compiler/codegen/func-section.lisp
- [x] T086 [US4] Add character branch using char-equal in compile-equalp
- [x] T087 [US4] Add string branch using string-equal in compile-equalp
- [x] T088 [US4] Add number branch using numeric = for type coercion in compile-equalp
- [x] T089 [US4] Add cons branch with recursive equalp comparison in compile-equalp
- [x] T090 [US4] Add fallback to equal for other types in compile-equalp
- [x] T091 [US4] Add EQUALP case to compile-primitive-call dispatcher in src/clysm/compiler/codegen/func-section.lisp
- [x] T092 [US4] Run tests and verify all PASS (Green phase)
- [x] T093 [P] [US4] Write contract test: equalp uses char-equal and string-equal in tests/contract/equality-wasm-test.lisp
- [x] T094 [US4] Run wasm-tools validate on compiled (equalp x y) output

**Checkpoint**: `equalp` is fully functional with case-insensitive comparison

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: ANSI CL compliance validation and final quality checks

- [x] T095 [P] Add ANSI CL eq tests (eq.1-5) to tests/integration/equality-ansi-test.lisp
- [x] T096 [P] Add ANSI CL eql tests (eql.1-8) to tests/integration/equality-ansi-test.lisp
- [x] T097 [P] Add ANSI CL equal tests (equal.1-10) to tests/integration/equality-ansi-test.lisp
- [x] T098 [P] Add ANSI CL equalp tests (equalp.1-12) to tests/integration/equality-ansi-test.lisp
- [x] T099 [P] Add ANSI CL not tests to tests/integration/equality-ansi-test.lisp
- [x] T100 Run full test suite with nix flake check
- [x] T101 Verify all Wasm output passes wasm-tools validate
- [x] T102 Run quickstart.md checklist validation
- [x] T103 Update CLAUDE.md with feature 024 completion notes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **US5 not (Phase 3)**: Depends on Foundational - establishes boolean pattern
- **US1 eq (Phase 4)**: Depends on Foundational - foundation for other predicates
- **US2 eql (Phase 5)**: Depends on eq - uses eq for fallback
- **US6 and/or (Phase 6)**: Depends on Foundational - verifies AST parsing
- **US3 equal (Phase 7)**: Depends on eql - uses eql for fallback
- **US4 equalp (Phase 8)**: Depends on equal - extends equal with case-insensitivity
- **Polish (Phase 9)**: Depends on all user stories being complete

### User Story Dependencies

```
Foundational
    │
    ├── US5 (not) ─────────────────────────────┐
    │                                          │
    ├── US1 (eq) ──► US2 (eql) ──► US3 (equal) ──► US4 (equalp)
    │                                          │
    └── US6 (and/or) ──────────────────────────┘
                                               │
                                               ▼
                                            Polish
```

### Within Each User Story

1. Write tests FIRST (Red phase)
2. Verify tests FAIL
3. Implement functionality
4. Verify tests PASS (Green phase)
5. Write contract tests
6. Run wasm-tools validate

### Parallel Opportunities

- T002, T003, T004: Test file stubs can be created in parallel
- T010, T011, T012: `not` tests on different scenarios
- T020-T023: `eq` tests on different scenarios
- T031-T037: `eql` tests on different type combinations
- T050-T055: `and`/`or` tests on different scenarios
- T064-T069: `equal` tests on different structures
- T080-T083: `equalp` tests on different scenarios
- T095-T099: ANSI CL integration tests can run in parallel

---

## Parallel Example: Phase 5 (eql)

```bash
# Launch all eql tests in parallel (different test cases):
Task: "Write test eql-different-fixnum in tests/unit/equality-predicates-test.lisp"
Task: "Write test eql-cross-type-fixnum-float in tests/unit/equality-predicates-test.lisp"
Task: "Write test eql-same-float in tests/unit/equality-predicates-test.lisp"
Task: "Write test eql-same-ratio in tests/unit/equality-predicates-test.lisp"
Task: "Write test eql-same-char in tests/unit/equality-predicates-test.lisp"
```

---

## Implementation Strategy

### MVP First (P1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: US5 (not) → Test independently
4. Complete Phase 4: US1 (eq) → Test independently
5. Complete Phase 5: US2 (eql) → Test independently
6. Complete Phase 6: US6 (and/or) → Test independently
7. **STOP and VALIDATE**: All P1 stories complete
8. Run nix flake check

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. US5 (not) → Boolean logic available (MVP-1)
3. US1 (eq) → Identity comparison available (MVP-2)
4. US2 (eql) → Type-aware comparison available
5. US6 (and/or) → Boolean operators available
6. US3 (equal) → Structural comparison available
7. US4 (equalp) → Case-insensitive comparison available
8. Polish → ANSI CL compliance verified

---

## Notes

- [P] tasks = different files or independent test cases
- [USx] label maps task to specific user story
- TDD is non-negotiable per Constitution Principle VII
- Each story should be independently testable after completion
- Verify tests fail before implementing (Red phase)
- Commit after each Green phase
- Stop at any checkpoint to validate story independently
