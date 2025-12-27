# Tasks: Portable UTF-8 Encoding

**Input**: Design documents from `/specs/034-portable-utf8/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md

**Tests**: Included per Constitution requirement (VII. TDD - 非交渉)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and file structure

- [ ] T001 Create src/clysm/lib/utf8.lisp with package header and exports
- [ ] T002 Add utf8.lisp to clysm.asd :components list (after lib/macros.lisp)
- [ ] T003 [P] Create tests/unit/utf8-test.lisp with test package definition

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**This phase is minimal** - the UTF-8 functions have no foundational dependencies beyond the file setup in Phase 1.

- [ ] T004 Define decoding-error condition type in src/clysm/lib/utf8.lisp with position and invalid-bytes slots
- [ ] T005 Export decoding-error, decoding-error-position, decoding-error-invalid-bytes from package

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Encode Lisp Strings to UTF-8 Bytes (Priority: P1) MVP

**Goal**: Implement `string-to-utf8-octets` function that converts Common Lisp strings to UTF-8 byte vectors

**Independent Test**: Encode various strings (ASCII, Japanese, emoji) and verify byte sequences match UTF-8 specification

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T006 [P] [US1] Test ASCII encoding "hello" -> #(104 101 108 108 111) in tests/unit/utf8-test.lisp
- [ ] T007 [P] [US1] Test 2-byte encoding (Latin-1 extended, e.g., "ñ" -> #(195 177)) in tests/unit/utf8-test.lisp
- [ ] T008 [P] [US1] Test 3-byte encoding (CJK, "日本語" -> 9 bytes) in tests/unit/utf8-test.lisp
- [ ] T009 [P] [US1] Test 4-byte encoding (emoji, "🎉" -> #(240 159 142 137)) in tests/unit/utf8-test.lisp
- [ ] T010 [P] [US1] Test empty string "" -> #() in tests/unit/utf8-test.lisp
- [ ] T011 [P] [US1] Test maximum code point U+10FFFF (4 bytes) in tests/unit/utf8-test.lisp
- [ ] T012 [P] [US1] Test NUL character U+0000 -> #(0) in tests/unit/utf8-test.lisp

### Implementation for User Story 1

- [ ] T013 [US1] Implement calculate-utf8-length helper (count bytes needed) in src/clysm/lib/utf8.lisp
- [ ] T014 [US1] Implement encode-char-to-utf8 helper (encode single char) in src/clysm/lib/utf8.lisp
- [ ] T015 [US1] Implement string-to-utf8-octets main function in src/clysm/lib/utf8.lisp
- [ ] T016 [US1] Export string-to-utf8-octets from clysm package in src/clysm/package.lisp
- [ ] T017 [US1] Verify all US1 tests pass

**Checkpoint**: User Story 1 complete - string-to-utf8-octets is fully functional

---

## Phase 4: User Story 2 - Decode UTF-8 Bytes to Lisp Strings (Priority: P2)

**Goal**: Implement `utf8-octets-to-string` function that converts UTF-8 byte vectors to Common Lisp strings

**Independent Test**: Decode known UTF-8 byte sequences and verify resulting strings match expected characters

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T018 [P] [US2] Test ASCII decoding #(104 101 108 108 111) -> "hello" in tests/unit/utf8-test.lisp
- [ ] T019 [P] [US2] Test 2-byte decoding (Latin-1 extended) in tests/unit/utf8-test.lisp
- [ ] T020 [P] [US2] Test 3-byte decoding (CJK, 9 bytes -> "日本語") in tests/unit/utf8-test.lisp
- [ ] T021 [P] [US2] Test 4-byte decoding (#(240 159 142 137) -> "🎉") in tests/unit/utf8-test.lisp
- [ ] T022 [P] [US2] Test empty vector #() -> "" in tests/unit/utf8-test.lisp
- [ ] T023 [P] [US2] Test round-trip: decode(encode(s)) = s for various strings in tests/unit/utf8-test.lisp

### Implementation for User Story 2

- [ ] T024 [US2] Implement decode-utf8-lead-byte helper (determine sequence length) in src/clysm/lib/utf8.lisp
- [ ] T025 [US2] Implement decode-utf8-sequence helper (decode 1-4 bytes to code point) in src/clysm/lib/utf8.lisp
- [ ] T026 [US2] Implement utf8-octets-to-string main function in src/clysm/lib/utf8.lisp
- [ ] T027 [US2] Export utf8-octets-to-string from clysm package in src/clysm/package.lisp
- [ ] T028 [US2] Verify all US2 tests pass

**Checkpoint**: User Story 2 complete - utf8-octets-to-string works for valid input

---

## Phase 5: User Story 3 - Handle Invalid UTF-8 Sequences Gracefully (Priority: P2)

**Goal**: Make `utf8-octets-to-string` signal `decoding-error` for all categories of invalid UTF-8

**Independent Test**: Provide malformed UTF-8 byte sequences and verify correct condition is signaled with position info

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T029 [P] [US3] Test invalid lead byte 0xFF signals decoding-error in tests/unit/utf8-test.lisp
- [ ] T030 [P] [US3] Test overlong lead bytes 0xC0, 0xC1 signal decoding-error in tests/unit/utf8-test.lisp
- [ ] T031 [P] [US3] Test unexpected continuation byte 0x80 at start signals error in tests/unit/utf8-test.lisp
- [ ] T032 [P] [US3] Test missing continuation (truncated sequence) signals error in tests/unit/utf8-test.lisp
- [ ] T033 [P] [US3] Test overlong 3-byte encoding for ASCII signals error in tests/unit/utf8-test.lisp
- [ ] T034 [P] [US3] Test surrogate code point U+D800 (encoded) signals error in tests/unit/utf8-test.lisp
- [ ] T035 [P] [US3] Test decoding-error includes correct position in tests/unit/utf8-test.lisp
- [ ] T036 [P] [US3] Test decoding-error includes invalid-bytes vector in tests/unit/utf8-test.lisp

### Implementation for User Story 3

- [ ] T037 [US3] Add validation for invalid lead bytes (0xC0-0xC1, 0xF5-0xFF) in decode-utf8-lead-byte
- [ ] T038 [US3] Add validation for unexpected continuation bytes in decode-utf8-sequence
- [ ] T039 [US3] Add validation for missing continuation bytes in decode-utf8-sequence
- [ ] T040 [US3] Add validation for overlong encodings in decode-utf8-sequence
- [ ] T041 [US3] Add validation for surrogate code points (U+D800-U+DFFF) in decode-utf8-sequence
- [ ] T042 [US3] Ensure decoding-error includes correct position and invalid-bytes
- [ ] T043 [US3] Verify all US3 tests pass

**Checkpoint**: User Story 3 complete - all invalid UTF-8 categories properly detected

---

## Phase 6: User Story 4 - Replace All Babel Usages in Compiler (Priority: P1)

**Goal**: Replace all 6 `babel:string-to-octets` calls with `string-to-utf8-octets` and remove Babel dependency

**Independent Test**: Run `grep -r 'babel:' src/` (0 matches), run existing test suite (all pass), validate Wasm output

**Dependencies**: Requires US1 complete (encoding function available)

### Tests for User Story 4

- [ ] T044 [P] [US4] Contract test: compile string literal, verify Wasm validates in tests/contract/utf8-wasm-test.lisp
- [ ] T045 [P] [US4] Contract test: compile with Japanese string, verify Wasm validates in tests/contract/utf8-wasm-test.lisp
- [ ] T046 [P] [US4] Integration test: full compile with strings, compare output to baseline in tests/integration/utf8-migration-test.lisp

### Implementation for User Story 4

- [ ] T047 [P] [US4] Replace babel usage in src/clysm/backend/sections.lisp:74 (encode-name function)
- [ ] T048 [P] [US4] Replace babel usage in src/clysm/compiler/compiler.lisp:545
- [ ] T049 [P] [US4] Replace babel usage in src/clysm/compiler/codegen/func-section.lisp:418 (compile-string-literal)
- [ ] T050 [P] [US4] Replace babel usage in src/clysm/ffi/export-gen.lisp:51
- [ ] T051 [P] [US4] Replace babel usages in src/clysm/ffi/import-gen.lisp:89,93 (2 sites)
- [ ] T052 [US4] Run grep -r 'babel:' src/ and verify 0 matches
- [ ] T053 [US4] Run full test suite and verify all tests pass
- [ ] T054 [US4] Verify Wasm output with wasm-tools validate

**Checkpoint**: User Story 4 complete - Babel dependency fully removed from compiler source

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Cleanup and package finalization

- [ ] T055 Remove babel from clysm.asd :depends-on list
- [ ] T056 Run nix flake check to verify clean build
- [ ] T057 Verify tests pass on SBCL (primary target)
- [ ] T058 [P] Add docstrings to exported functions in src/clysm/lib/utf8.lisp

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion
- **User Story 1 (Phase 3)**: Depends on Foundational - REQUIRED for MVP
- **User Story 2 (Phase 4)**: Depends on Foundational - Independent of US1
- **User Story 3 (Phase 5)**: Depends on User Story 2 (extends decode function)
- **User Story 4 (Phase 6)**: Depends on User Story 1 (uses encoding function)
- **Polish (Phase 7)**: Depends on User Story 4 complete

### User Story Dependencies

```text
       ┌────────────────┐
       │ Foundational   │
       └───────┬────────┘
               │
       ┌───────┴───────┐
       │               │
       v               v
   ┌──────┐       ┌──────┐
   │ US1  │       │ US2  │
   │(P1)  │       │(P2)  │
   └──┬───┘       └──┬───┘
      │              │
      │              v
      │          ┌──────┐
      │          │ US3  │
      │          │(P2)  │
      │          └──────┘
      │
      v
   ┌──────┐
   │ US4  │
   │(P1)  │
   └──────┘
```

### Within Each User Story

1. Tests MUST be written and FAIL before implementation (TDD per Constitution)
2. Helper functions before main function
3. Export after implementation
4. Verification test run at end

### Parallel Opportunities

**Phase 1 (Setup)**: T001, T002, T003 can run in parallel

**Phase 3 (US1 Tests)**: T006-T012 can all run in parallel

**Phase 4 (US2 Tests)**: T018-T023 can all run in parallel

**Phase 5 (US3 Tests)**: T029-T036 can all run in parallel

**Phase 6 (US4 Replacements)**: T047-T051 can all run in parallel (different files)

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all US1 tests together (different test cases, same file but independent):
Task: "Test ASCII encoding in tests/unit/utf8-test.lisp"
Task: "Test 2-byte encoding in tests/unit/utf8-test.lisp"
Task: "Test 3-byte encoding in tests/unit/utf8-test.lisp"
Task: "Test 4-byte encoding in tests/unit/utf8-test.lisp"
Task: "Test empty string in tests/unit/utf8-test.lisp"
Task: "Test maximum code point in tests/unit/utf8-test.lisp"
Task: "Test NUL character in tests/unit/utf8-test.lisp"
```

## Parallel Example: User Story 4 Replacements

```bash
# Launch all babel replacements together (different files):
Task: "Replace babel in sections.lisp"
Task: "Replace babel in compiler.lisp"
Task: "Replace babel in func-section.lisp"
Task: "Replace babel in export-gen.lisp"
Task: "Replace babel in import-gen.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 + User Story 4)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1 (encoding)
4. Complete Phase 6: User Story 4 (migration)
5. **STOP and VALIDATE**: Babel dependency removed, compiler works
6. Deploy/demo: Portable compiler achieved

### Full Feature (Add Decoding)

7. Complete Phase 4: User Story 2 (decoding)
8. Complete Phase 5: User Story 3 (error handling)
9. Complete Phase 7: Polish

### Parallel Team Strategy

With 2 developers after Foundational:
- Developer A: US1 → US4 (encoding and migration)
- Developer B: US2 → US3 (decoding and error handling)

---

## Notes

- [P] tasks = different files or independent test cases
- [Story] label maps task to specific user story for traceability
- TDD is REQUIRED per Constitution (VII. TDD - 非交渉)
- All replacements use simple pattern: `(babel:string-to-octets X :encoding :utf-8)` → `(string-to-utf8-octets X)`
- Commit after each task or logical group
- Stop at any checkpoint to validate independently
