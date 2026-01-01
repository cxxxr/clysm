# Tasks: Runtime Library System

**Input**: Design documents from `/specs/001-runtime-library-system/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: Included per Constitution principle VII (TDD requirement).

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story (US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure) ✅ COMPLETE

**Purpose**: Project initialization and new module structure

- [x] T001 Create src/clysm/runtime/ directory structure
- [x] T002 Create src/clysm/runtime/package.lisp with runtime library package definition
- [x] T003 Create src/clysm/compiler/codegen/primitives.lisp with empty registry structure
- [x] T004 [P] Create tests/contract/runtime/ directory structure
- [x] T005 [P] Create tests/unit/runtime/ directory structure
- [x] T006 [P] Create tests/integration/runtime/ directory structure
- [x] T007 Update clysm.asd to include new runtime module with component order

---

## Phase 2: Foundational (Blocking Prerequisites) ✅ COMPLETE

**Purpose**: Core primitives registry that MUST be complete before user stories

**✅ CHECKPOINT PASSED**: 24 primitives registered. `(list-primitives)` returns 24 entries.

### Tests for Foundational Phase

- [x] T008 [P] Contract test for primitive struct in tests/contract/runtime/primitives-struct-test.lisp
- [x] T009 [P] Contract test for registry hash-table in tests/contract/runtime/registry-test.lisp
- [x] T010 [P] Unit test for register-primitive in tests/unit/runtime/register-test.lisp
- [x] T011 [P] Unit test for get-primitive and primitive-p in tests/unit/runtime/lookup-test.lisp

### Implementation for Foundational Phase

- [x] T012 Define primitive struct (name, wasm-emitter, signature, inline-p, category) in src/clysm/compiler/codegen/primitives.lisp
- [x] T013 Implement *primitives-registry* hash-table in src/clysm/compiler/codegen/primitives.lisp
- [x] T014 Implement register-primitive function in src/clysm/compiler/codegen/primitives.lisp
- [x] T015 Implement get-primitive function in src/clysm/compiler/codegen/primitives.lisp
- [x] T016 Implement primitive-p predicate in src/clysm/compiler/codegen/primitives.lisp
- [x] T017 [P] Implement list-primitives introspection function in src/clysm/compiler/codegen/primitives.lisp
- [x] T018 Register memory primitives (cons, car, cdr, rplaca, rplacd) in src/clysm/compiler/codegen/primitives.lisp
- [x] T019 [P] Register type predicates (consp, null, numberp, stringp, symbolp) in src/clysm/compiler/codegen/primitives.lisp
- [x] T020 [P] Register arithmetic primitives (+, -, *, /, mod, <, >, =, <=, >=) in src/clysm/compiler/codegen/primitives.lisp
- [x] T021 [P] Register FFI primitives (%host-write-char, %host-write-string, %host-read-char, %host-read-line) in src/clysm/compiler/codegen/primitives.lisp

**Checkpoint**: All primitives registered. Verify with `(list-primitives)` returning 20+ entries.

---

## Phase 3: User Story 1 - Add New Standard Function (Priority: P1) ✅ COMPLETE

**Goal**: Enable developers to add runtime library functions using Lisp source that compile to Wasm

**Independent Test**: Add `assoc` to runtime library, verify it compiles to valid Wasm and executes correctly

### Tests for User Story 1

- [x] T022 [P] [US1] Contract test for runtime module loading in tests/contract/runtime/module-load-test.lisp
- [x] T023 [P] [US1] Contract test for dependency analysis in tests/contract/runtime/dependency-test.lisp
- [x] T024 [P] [US1] Contract test for Wasm validation in tests/contract/runtime/wasm-valid-test.lisp
- [x] T025 [P] [US1] Unit test for runtime-function struct in tests/unit/runtime/function-struct-test.lisp
- [x] T026 [P] [US1] Unit test for compile-runtime-function in tests/unit/runtime/compile-fn-test.lisp
- [x] T027 [P] [US1] Unit test for undefined primitive detection in tests/unit/runtime/undefined-prim-test.lisp
- [x] T028 [US1] Integration test for assoc compilation and execution in tests/integration/runtime/assoc-test.lisp

### Implementation for User Story 1

- [x] T029 [US1] Define runtime-function struct (name, lambda-list, body, source-file, compiled-index, dependencies) in src/clysm/runtime/rt-package.lisp
- [x] T030 [US1] Define runtime-module struct (name, source-file, functions, exports) in src/clysm/runtime/rt-package.lisp
- [x] T031 [US1] Implement load-runtime-source to read and parse runtime .lisp files in src/clysm/runtime/loader.lisp
- [x] T032 [US1] Implement analyze-dependencies to compute function dependency graph in src/clysm/runtime/loader.lisp
- [x] T033 [US1] Implement topological-sort for compilation order in src/clysm/runtime/loader.lisp
- [x] T034 [US1] Integrate primitive-p check into compile-call for function resolution in src/clysm/compiler/codegen/func-section.lisp
- [x] T035 [US1] Implement undefined-primitive-error and circular-dependency-error conditions in src/clysm/runtime/loader.lisp
- [x] T036 [US1] Implement compile-runtime-function using existing compile-form in src/clysm/runtime/compiler.lisp
- [x] T037 [US1] Implement compile-runtime-module to compile all functions in dependency order in src/clysm/runtime/compiler.lisp
- [x] T038 [US1] Implement merge-runtime-module to combine runtime Wasm with user Wasm in src/clysm/runtime/compiler.lisp
- [x] T039 [P] [US1] Create src/clysm/runtime/list-ops.lisp with [assoc](resources/HyperSpec/Body/f_assocc.htm) implementation
- [x] T040 [P] [US1] Add [member](resources/HyperSpec/Body/f_mem_m.htm) to src/clysm/runtime/list-ops.lisp
- [x] T041 [P] [US1] Add [find](resources/HyperSpec/Body/f_find_.htm) to src/clysm/runtime/list-ops.lisp
- [x] T042 [P] [US1] Add [position](resources/HyperSpec/Body/f_pos_p.htm) to src/clysm/runtime/list-ops.lisp
- [x] T043 [US1] Add [nth](resources/HyperSpec/Body/f_nth.htm) and [nthcdr](resources/HyperSpec/Body/f_nthcdr.htm) to src/clysm/runtime/list-ops.lisp
- [x] T044 [US1] Validate list-ops.lisp compiles with wasm-tools validate

**Checkpoint**: Developer can add new function to runtime library and it appears in Wasm output. ✅ PASSED
- 14 functions loaded from list-ops.lisp
- Dependency analysis correct (nth → nthcdr)
- Topological sort produces valid compilation order

---

## Phase 4: User Story 2 - Migrate I/O Functions (Priority: P2)

**Goal**: Migrate I/O functions from func-section.lisp to runtime library, achieving 40% line reduction

**Independent Test**: Compile program using `princ`, verify output matches original behavior, measure line reduction

### Tests for User Story 2

- [ ] T045 [P] [US2] Contract test for I/O primitives validation in tests/contract/runtime/io-primitives-test.lisp
- [ ] T046 [P] [US2] Migration parity test for terpri in tests/integration/runtime/terpri-parity-test.lisp
- [ ] T047 [P] [US2] Migration parity test for write-char in tests/integration/runtime/write-char-parity-test.lisp
- [ ] T048 [P] [US2] Migration parity test for write-string in tests/integration/runtime/write-string-parity-test.lisp
- [ ] T049 [P] [US2] Migration parity test for princ in tests/integration/runtime/princ-parity-test.lisp
- [ ] T050 [P] [US2] Migration parity test for prin1 in tests/integration/runtime/prin1-parity-test.lisp
- [ ] T051 [P] [US2] Migration parity test for print in tests/integration/runtime/print-parity-test.lisp
- [ ] T052 [P] [US2] Migration parity test for format in tests/integration/runtime/format-parity-test.lisp
- [ ] T053 [US2] Line count baseline test to verify 40% reduction in tests/integration/runtime/linecount-test.lisp

### Implementation for User Story 2

- [ ] T054 [US2] Create src/clysm/runtime/io.lisp with package header and stream utilities
- [ ] T055 [US2] Implement [terpri](resources/HyperSpec/Body/f_terpri.htm) in src/clysm/runtime/io.lisp using %host-write-char
- [ ] T056 [US2] Implement [write-char](resources/HyperSpec/Body/f_wr_cha.htm) in src/clysm/runtime/io.lisp using %host-write-char
- [ ] T057 [US2] Implement [write-string](resources/HyperSpec/Body/f_wr_stg.htm) in src/clysm/runtime/io.lisp using %host-write-string
- [ ] T058 [US2] Implement [princ](resources/HyperSpec/Body/f_wr_pr.htm) in src/clysm/runtime/io.lisp with type dispatch
- [ ] T059 [US2] Implement [prin1](resources/HyperSpec/Body/f_wr_pr.htm) in src/clysm/runtime/io.lisp with escape sequences
- [ ] T060 [US2] Implement [print](resources/HyperSpec/Body/f_wr_pr.htm) in src/clysm/runtime/io.lisp using prin1 + terpri
- [ ] T061 [US2] Implement [write](resources/HyperSpec/Body/f_wr_pr.htm) in src/clysm/runtime/io.lisp with keyword support
- [ ] T062 [US2] Implement [format](resources/HyperSpec/Body/f_format.htm) in src/clysm/runtime/io.lisp with directive parsing
- [ ] T063 [US2] Remove compile-terpri codegen from src/clysm/compiler/codegen/func-section.lisp
- [ ] T064 [US2] Remove compile-write-char codegen from src/clysm/compiler/codegen/func-section.lisp
- [ ] T065 [US2] Remove compile-write-string codegen from src/clysm/compiler/codegen/func-section.lisp
- [ ] T066 [US2] Remove compile-princ codegen from src/clysm/compiler/codegen/func-section.lisp
- [ ] T067 [US2] Remove compile-prin1 codegen from src/clysm/compiler/codegen/func-section.lisp
- [ ] T068 [US2] Remove compile-print codegen from src/clysm/compiler/codegen/func-section.lisp
- [ ] T069 [US2] Remove compile-write codegen from src/clysm/compiler/codegen/func-section.lisp
- [ ] T070 [US2] Remove compile-format codegen from src/clysm/compiler/codegen/func-section.lisp
- [ ] T071 [US2] Update function dispatch in compile-call to use runtime I/O functions
- [ ] T072 [US2] Verify func-section.lisp line count < 11,000 (40% reduction from 18,229)
- [ ] T073 [US2] Validate io.lisp compiles with wasm-tools validate

**Checkpoint**: All I/O functions migrated. `wc -l func-section.lisp` shows <11,000 lines.

---

## Phase 5: User Story 3 - Bootstrap Self-Hosting (Priority: P3)

**Goal**: Complete runtime library with all functions needed for compiler self-hosting

**Independent Test**: Run `sbcl --load build/stage1-complete.lisp` and verify compilation rate increase

### Tests for User Story 3

- [ ] T074 [P] [US3] Contract test for sequence operations Wasm validity in tests/contract/runtime/sequence-wasm-test.lisp
- [ ] T075 [P] [US3] Contract test for string operations Wasm validity in tests/contract/runtime/strings-wasm-test.lisp
- [ ] T076 [P] [US3] Unit test for remove/remove-if in tests/unit/runtime/remove-test.lisp
- [ ] T077 [P] [US3] Unit test for count/count-if in tests/unit/runtime/count-test.lisp
- [ ] T078 [P] [US3] Unit test for substitute in tests/unit/runtime/substitute-test.lisp
- [ ] T079 [P] [US3] Unit test for princ-to-string/prin1-to-string in tests/unit/runtime/to-string-test.lisp
- [ ] T080 [US3] Integration test for Stage 1 compilation rate in tests/integration/runtime/stage1-rate-test.lisp

### Implementation for User Story 3

- [ ] T081 [US3] Create src/clysm/runtime/sequences.lisp with package header
- [ ] T082 [US3] Implement [remove](resources/HyperSpec/Body/f_rm_rm.htm) in src/clysm/runtime/sequences.lisp
- [ ] T083 [US3] Implement [remove-if](resources/HyperSpec/Body/f_rm_rm.htm) in src/clysm/runtime/sequences.lisp
- [ ] T084 [US3] Implement [count](resources/HyperSpec/Body/f_countc.htm) in src/clysm/runtime/sequences.lisp
- [ ] T085 [US3] Implement [count-if](resources/HyperSpec/Body/f_countc.htm) in src/clysm/runtime/sequences.lisp
- [ ] T086 [US3] Implement [substitute](resources/HyperSpec/Body/f_sbs_s.htm) in src/clysm/runtime/sequences.lisp
- [ ] T087 [US3] Create src/clysm/runtime/strings.lisp with package header
- [ ] T088 [US3] Implement [princ-to-string](resources/HyperSpec/Body/f_wr_to_.htm) in src/clysm/runtime/strings.lisp
- [ ] T089 [US3] Implement [prin1-to-string](resources/HyperSpec/Body/f_wr_to_.htm) in src/clysm/runtime/strings.lisp
- [ ] T090 [US3] Implement [write-to-string](resources/HyperSpec/Body/f_wr_to_.htm) in src/clysm/runtime/strings.lisp
- [ ] T091 [US3] Validate sequences.lisp compiles with wasm-tools validate
- [ ] T092 [US3] Validate strings.lisp compiles with wasm-tools validate
- [ ] T093 [US3] Run `sbcl --load build/stage1-complete.lisp` and measure compilation rate
- [ ] T094 [US3] Verify Stage 1 compilation rate increases by at least 10 percentage points

**Checkpoint**: Runtime library complete. Stage 1 compilation rate improved toward self-hosting.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Performance validation, documentation, cleanup

- [ ] T095 [P] Run benchmark comparing old codegen vs runtime library performance
- [ ] T096 [P] Verify build time increase <20% per SC-006
- [ ] T097 [P] Add HyperSpec links to all runtime function docstrings per Constitution IX
- [ ] T098 [P] Update CLAUDE.md with runtime library module documentation
- [ ] T099 Run full test suite: `sbcl --eval "(asdf:test-system :clysm)"`
- [ ] T100 Run quickstart.md validation steps

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies - can start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 - BLOCKS all user stories
- **Phase 3 (US1)**: Depends on Phase 2 completion
- **Phase 4 (US2)**: Depends on Phase 2 (can run parallel to US1 if staffed)
- **Phase 5 (US3)**: Depends on Phase 2 (US1 and US2 provide foundation but not strict blocker)
- **Phase 6 (Polish)**: Depends on all user stories

### User Story Dependencies

- **US1 (P1)**: Only depends on Foundational phase - can be MVP
- **US2 (P2)**: Independent of US1, both use primitives registry
- **US3 (P3)**: Independent of US1/US2, but benefits from their patterns

### Within Each User Story

- Tests written FIRST, must FAIL before implementation
- Structs/models before loaders
- Loaders before compilers
- Individual functions can be parallel within a module
- Validation last

### Parallel Opportunities

**Phase 1**:
- T004, T005, T006 (directory creation) in parallel

**Phase 2**:
- T008-T011 (all tests) in parallel
- T019, T020, T021 (primitive registration by category) in parallel

**Phase 3 (US1)**:
- T022-T027 (all tests) in parallel
- T039-T042 (list function implementations) in parallel

**Phase 4 (US2)**:
- T045-T052 (parity tests) in parallel
- T063-T070 (codegen removal) sequentially (same file)

**Phase 5 (US3)**:
- T074-T079 (all tests) in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all US1 tests together:
Task: "T022 [P] [US1] Contract test for runtime module loading"
Task: "T023 [P] [US1] Contract test for dependency analysis"
Task: "T024 [P] [US1] Contract test for Wasm validation"
Task: "T025 [P] [US1] Unit test for runtime-function struct"
Task: "T026 [P] [US1] Unit test for compile-runtime-function"
Task: "T027 [P] [US1] Unit test for undefined primitive detection"

# After tests pass, launch list function implementations together:
Task: "T039 [P] [US1] Create list-ops.lisp with assoc"
Task: "T040 [P] [US1] Add member to list-ops.lisp"
Task: "T041 [P] [US1] Add find to list-ops.lisp"
Task: "T042 [P] [US1] Add position to list-ops.lisp"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T007)
2. Complete Phase 2: Foundational (T008-T021)
3. Complete Phase 3: User Story 1 (T022-T044)
4. **STOP and VALIDATE**: Add `assoc` to runtime, verify Wasm compiles
5. Demonstrate: New functions can be added without touching func-section.lisp

### Incremental Delivery

1. Setup + Foundational → Primitives registry ready
2. User Story 1 → Can add runtime functions → **MVP!**
3. User Story 2 → I/O migrated, 40% line reduction → Major milestone
4. User Story 3 → Self-hosting enabled → Bootstrap goal achieved

### Success Metrics

| Metric | Target | Verification |
|--------|--------|--------------|
| SC-001 | func-section.lisp < 11,000 lines | `wc -l func-section.lisp` |
| SC-002 | Add function without codegen changes | Add new runtime function |
| SC-003 | Migrated functions pass tests | `rove tests/integration/runtime/` |
| SC-004 | Stage 1 rate +10 points | `sbcl --load build/stage1-complete.lisp` |
| SC-006 | Build time increase <20% | Stage 1 timing comparison |

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Constitution VII requires TDD: tests MUST fail before implementation
- HyperSpec links required per Constitution IX
- Commit after each task or logical group
- Stop at any checkpoint to validate independently
