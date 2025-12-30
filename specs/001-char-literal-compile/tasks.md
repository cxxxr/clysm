# Tasks: Character Literal Compilation Support

**Input**: Design documents from `/specs/001-char-literal-compile/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md
**Constitution**: TDD REQUIRED (Principle VII) - All tests must be written and FAIL before implementation

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/clysm/compiler/codegen/func-section.lisp`
- **Unit Tests**: `tests/unit/char-literal-test.lisp`
- **Contract Tests**: `tests/contract/char-wasm-test.lisp`

---

## Phase 1: Setup (Verification)

**Purpose**: Verify development environment and understand existing code

- [x] T001 Verify nix develop environment with SBCL and wasm-tools available
- [x] T002 Read and understand `compile-quoted-element` function in src/clysm/compiler/codegen/func-section.lisp lines 509-525
- [x] T003 Verify existing tests pass with `sbcl --eval "(asdf:test-system :clysm)" --quit`

---

## Phase 2: Foundational (None Required)

**Purpose**: No foundational tasks needed - existing infrastructure is sufficient

**Checkpoint**: Existing compiler infrastructure ready for feature addition

---

## Phase 3: User Story 1 - Compile Quoted Character Literals (Priority: P1)

**Goal**: Enable compilation of basic character literals (#\Space, #\Tab) in quoted expressions

**Independent Test**: Compile `(member char '(#\Space #\Tab))` and verify Wasm is valid

### Tests for User Story 1 (TDD REQUIRED)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T004 [P] [US1] Create unit test file at tests/unit/char-literal-test.lisp with rove test package setup
- [x] T005 [P] [US1] Add unit test: `compile-quoted-element` with `#\Space` returns `((:i32.const 32) :ref.i31)` in tests/unit/char-literal-test.lisp
- [x] T006 [P] [US1] Add unit test: `compile-quoted-element` with `#\Tab` returns `((:i32.const 9) :ref.i31)` in tests/unit/char-literal-test.lisp
- [x] T007 [P] [US1] Add unit test: `compile-quoted-element` with `#\Newline` returns `((:i32.const 10) :ref.i31)` in tests/unit/char-literal-test.lisp
- [x] T008 [US1] Run unit tests to confirm they FAIL (character branch not implemented yet)

### Implementation for User Story 1

- [x] T009 [US1] Add `characterp` branch to `compile-quoted-element` in src/clysm/compiler/codegen/func-section.lisp after line 520 (after complexp branch)
- [x] T010 [US1] Run unit tests to confirm they PASS
- [x] T011 [US1] Create contract test file at tests/contract/char-wasm-test.lisp with wasm-tools validation
- [x] T012 [US1] Add contract test: compile `'(#\Space #\Tab)` and validate with wasm-tools in tests/contract/char-wasm-test.lisp
- [x] T013 [US1] Run contract tests to verify Wasm output is valid

**Checkpoint**: User Story 1 complete - basic character literals (#\Space, #\Tab, #\Newline) compile to valid Wasm ✓

---

## Phase 4: User Story 2 - Support All Standard Character Literals (Priority: P2)

**Goal**: Extend support to all standard Common Lisp character literals

**Independent Test**: Compile forms with printable chars (#\a, #\Z), all named chars, and Unicode chars

### Tests for User Story 2 (TDD REQUIRED)

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T014 [P] [US2] Add unit test: `compile-quoted-element` with `#\a` returns `((:i32.const 97) :ref.i31)` in tests/unit/char-literal-test.lisp
- [x] T015 [P] [US2] Add unit test: `compile-quoted-element` with `#\A` returns `((:i32.const 65) :ref.i31)` in tests/unit/char-literal-test.lisp
- [x] T016 [P] [US2] Add unit test: `compile-quoted-element` with `#\Return` returns `((:i32.const 13) :ref.i31)` in tests/unit/char-literal-test.lisp
- [x] T017 [US2] Run unit tests to confirm new tests PASS (implementation already covers all characters)

### Implementation for User Story 2

- [x] T018 [US2] Verify implementation handles all printable ASCII characters (no code change expected - char-code works for all)
- [x] T019 [US2] Add contract test: compile `'(#\a #\Z #\! #\Return)` and validate in tests/contract/char-wasm-test.lisp
- [x] T020 [US2] Add contract test: compile `(member x '(#\a #\b #\c))` defun form and validate in tests/contract/char-wasm-test.lisp
- [x] T021 [US2] Run all contract tests to verify extended character support

**Checkpoint**: User Story 2 complete - all standard character literals supported ✓

---

## Phase 5: Polish & Integration Verification

**Purpose**: Verify end-to-end functionality and measure compilation rate improvement

- [x] T022 Run full test suite with `sbcl --eval "(asdf:test-system :clysm)" --quit`
- [x] T023 Generate Stage 1 with `sbcl --load build/stage1-complete.lisp`
- [x] T024 Check dist/stage1-report.json for compilation rate improvement (target: 14% → 20%+)
  - **Note**: Compilation rate at 13.63%. Character literals working correctly; rate depends on DEFUN/DEFSTRUCT blockers.
- [x] T025 Validate dist/clysm-stage1.wasm with `wasm-tools validate dist/clysm-stage1.wasm`
- [x] T026 Run quickstart.md REPL verification steps (verified via contract tests)
- [x] T027 [P] Update CLAUDE.md if compilation rate improvement is confirmed
  - **Note**: N/A - character literal feature complete but compilation rate improvement not achieved

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - verify environment first
- **Foundational (Phase 2)**: N/A - existing infrastructure sufficient
- **User Story 1 (Phase 3)**: Depends on Setup - TDD cycle
- **User Story 2 (Phase 4)**: Depends on User Story 1 completion (extends same implementation)
- **Polish (Phase 5)**: Depends on all user stories complete

### User Story Dependencies

- **User Story 1 (P1)**: Independent - core feature implementation
- **User Story 2 (P2)**: Logically extends US1 but tests can be written in parallel; implementation verification only

### Within Each User Story

1. Create test file structure (if new)
2. Write failing tests FIRST
3. Run tests to confirm FAILURE
4. Implement feature
5. Run tests to confirm PASS
6. Add contract tests
7. Verify Wasm validation

### Parallel Opportunities

```text
# Phase 3 - Unit tests can run in parallel:
T004, T005, T006, T007 can all run in parallel (different test cases, same file)

# Phase 4 - Unit tests can run in parallel:
T014, T015, T016 can all run in parallel (different test cases, same file)
```

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all unit tests for User Story 1 together:
Task: "Add unit test: compile-quoted-element with #\Space in tests/unit/char-literal-test.lisp"
Task: "Add unit test: compile-quoted-element with #\Tab in tests/unit/char-literal-test.lisp"
Task: "Add unit test: compile-quoted-element with #\Newline in tests/unit/char-literal-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup verification
2. Complete Phase 3: User Story 1 (TDD cycle)
3. **STOP and VALIDATE**: Run `(member char '(#\Space #\Tab))` compilation
4. Verify Wasm passes validation
5. MVP delivered - basic character literals work

### Full Feature Delivery

1. Complete Setup (Phase 1)
2. Complete User Story 1 (Phase 3) → Test independently → MVP!
3. Complete User Story 2 (Phase 4) → Test independently → Full coverage
4. Complete Polish (Phase 5) → Measure compilation rate improvement
5. Each story adds value without breaking previous stories

### Single Developer Strategy

Given this is a focused ~5 line code change:

1. T001-T003: Environment verification (5 min)
2. T004-T013: User Story 1 full TDD cycle (30 min)
3. T014-T021: User Story 2 test extension (20 min)
4. T022-T027: Integration verification (15 min)

**Estimated total**: ~70 minutes

---

## Notes

- TDD is REQUIRED per Constitution Principle VII
- Implementation is ~3 lines of code (characterp branch)
- All tests marked [P] can run in parallel within their phase
- User Story 2 tests may PASS immediately since char-code handles all characters
- Key success metric: compilation rate 14% → 20%+
- Commit after each task or logical group
- Reference: [characterp](resources/HyperSpec/Body/f_chp.htm), [char-code](resources/HyperSpec/Body/f_char_c.htm)
