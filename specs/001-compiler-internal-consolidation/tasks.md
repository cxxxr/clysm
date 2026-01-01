# Tasks: Compiler Internal Function Consolidation

**Input**: Design documents from `/specs/001-compiler-internal-consolidation/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md

**Tests**: Required per Constitution VII (TDD非交渉). Tests MUST be written first and verified to FAIL before implementation.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4, US5)
- Exact file paths included in all task descriptions

## Path Conventions

```text
src/clysm/
├── compiler/codegen/func-section.lisp   # MODIFY: Remove inline compile-*
├── lib/env-runtime.lisp                 # NEW: Wasm-compilable env functions
├── lib/package-stubs.lisp               # NEW: FFI-backed package operations
├── lib/type-construction.lisp           # NEW: Type definition helpers
tests/
├── unit/env-runtime/                    # NEW: Environment function tests
├── contract/runtime-dispatch/           # NEW: Dispatch verification
└── integration/stage1/                  # MODIFY: Regression tests
```

---

## Phase 1: Setup

**Purpose**: Capture baseline and prepare development environment

- [x] T001 Run Stage 1 compilation and save baseline report to dist/baseline-report.json
- [x] T002 Record baseline compilation rate and compiled form count from dist/stage1-report.json
  - Baseline: 22.17% coverage, 5632 compiled forms, 27731 bytes
- [x] T003 [P] Verify wasm-tools validate passes on current dist/clysm-stage1.wasm
- [x] T004 [P] Create test directory structure tests/unit/env-runtime/ and tests/contract/runtime-dispatch/

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before user story implementation

**⚠️ CRITICAL**: All user stories depend on this phase completing successfully

- [x] T005 Verify *runtime-function-table* pattern works in src/clysm/compiler/codegen/func-section.lisp:66-148
  - **FINDING**: Pattern is DEFINED but registration functions are NEVER CALLED. Table is EMPTY at runtime.
- [x] T006 [P] Document existing list-runtime.lisp function signatures in src/clysm/lib/list-runtime.lisp
  - 12 functions: member-rt, assoc-rt, rassoc-rt, find-rt, position-rt + -if/-if-not variants
- [x] T007 [P] Document existing io-runtime.lisp FFI pattern in src/clysm/lib/io-runtime.lisp
  - FFI pattern via clysm/streams::%host-write-char, %host-write-string
- [x] T008 Verify FFI infrastructure in src/clysm/ffi/types.lisp supports new function imports
- [ ] T009 Create shared test utilities for Stage 1 validation in tests/helpers/stage1-validation.lisp
  - DEFERRED: Not blocking for current work

**Checkpoint**: Foundation ready - user story implementation can begin

---

## Phase 3: User Story 5 - Runtime Library Function Migration (Priority: P3) 🎯 MVP

**Goal**: Remove inline compile-member, compile-assoc, etc. (~600 lines) from func-section.lisp. Functions already registered in *runtime-function-table*.

**Independent Test**: Compile `(defun test-list () (member 'a '(a b c)))` and verify :call :$member-rt in output.

**Risk Level**: ~~LOW~~ **BLOCKED** - See research.md CORRECTION (2026-01-01)

### ⚠️ PHASE BLOCKED - Runtime Library Not in Stage 1

**Discovery (2026-01-01)**:
1. Registration functions (`register-io-runtime-functions`, `register-list-runtime-functions`) are DEFINED but NEVER CALLED
2. `*runtime-function-table*` is EMPTY at runtime - inline compile-* functions are the ONLY working path
3. Runtime library files (list-runtime.lisp, io-runtime.lisp) are NOT included in Stage 1 output
4. **Enabling runtime dispatch without the runtime library causes compilation regression** (tested: 5632→5342 compiled)

**Required Prerequisites** (before unblocking):
1. Include runtime library files in Stage 1 compilation
2. Add registration calls at load time
3. Export runtime functions in the Wasm module

### Tests for User Story 5 (TDD Required) - DEFERRED

- [ ] T010 [P] [US5] DEFERRED: Write contract test verifying member compiles to runtime call
- [ ] T011 [P] [US5] DEFERRED: Write contract test verifying assoc compiles to runtime call
- [ ] T012 [P] [US5] DEFERRED: Write contract test verifying find compiles to runtime call
- [ ] T013 [P] [US5] DEFERRED: Write contract test verifying position compiles to runtime call
- [ ] T014 [US5] DEFERRED: Verify all tests FAIL with current inline implementation

### Implementation for User Story 5 - DEFERRED

- [ ] T015 [US5] DEFERRED: Locate and comment out compile-member function
- [ ] T016 [US5] DEFERRED: Locate and comment out compile-assoc function
- [ ] T017 [US5] DEFERRED: Locate and comment out compile-rassoc function
- [ ] T018 [US5] DEFERRED: Locate and comment out compile-find function
- [ ] T019 [US5] DEFERRED: Locate and comment out compile-position function
- [ ] T020 [US5] DEFERRED: Locate and comment out compile-member-if and compile-assoc-if functions
- [ ] T021 [US5] DEFERRED: Run Stage 1 compilation and verify no regressions
- [ ] T022 [US5] DEFERRED: Validate Wasm output with wasm-tools validate
- [ ] T023 [US5] DEFERRED: Verify all tests from T010-T013 now PASS
- [ ] T024 [US5] DEFERRED: Delete commented code and run final validation

**Checkpoint**: BLOCKED - Requires runtime library integration first.

---

## Phase 4: User Story 3 - Package System FFI Integration (Priority: P2) ✅

**Goal**: Create Wasm-compilable package stubs (PACKAGEP*, FIND-PACKAGE*, INTERN*) that delegate to host via FFI.

**Independent Test**: Compile `(defun test-pkg () (find-package* "CL"))` and verify FFI import in output.

**Risk Level**: LOW - Isolated FFI calls following io-runtime.lisp pattern

**Status (2026-01-01)**: Implementation complete. Functions work for host evaluation.
Stage 1 PACKAGEP* blocker count unchanged (25) due to compiler forward reference limitation.

### Tests for User Story 3 (TDD Required)

- [x] T025 [P] [US3] Write unit test for find-package* FFI call in tests/unit/package-stubs/test-find-package.lisp
- [x] T026 [P] [US3] Write unit test for intern* FFI call in tests/unit/package-stubs/test-intern.lisp
- [x] T027 [P] [US3] Write unit test for packagep* FFI call in tests/unit/package-stubs/test-packagep.lisp
- [x] T028 [US3] Verify all tests FAIL (functions not yet defined) - VERIFIED: all NIL

### Implementation for User Story 3

- [x] T029 [US3] Create src/clysm/lib/package-stubs.lisp with package declaration
- [x] T030 [US3] Implement %host-find-package FFI declaration in src/clysm/lib/package-stubs.lisp
- [x] T031 [US3] Implement find-package* function wrapping %host-find-package in src/clysm/lib/package-stubs.lisp
- [x] T032 [US3] Implement %host-intern FFI declaration in src/clysm/lib/package-stubs.lisp
- [x] T033 [US3] Implement intern* function wrapping %host-intern in src/clysm/lib/package-stubs.lisp
- [x] T034 [US3] Implement %host-packagep FFI declaration in src/clysm/lib/package-stubs.lisp
- [x] T035 [US3] Implement packagep* function wrapping %host-packagep in src/clysm/lib/package-stubs.lisp
- [x] T036 [US3] Add host shim implementations in host-shim/package-shim.js
- [x] T037 [US3] Register package functions in clysm.asd system definition
- [x] T038 [US3] Run Stage 1 compilation and verify no regressions - PASSED: 27731 bytes
- [x] T039 [US3] Validate Wasm output with wasm-tools validate - PASSED
- [ ] T040 [US3] Verify all tests from T025-T027 now PASS - DEFERRED: Tests need clysm/tests package setup

**Checkpoint**: Package stubs complete. FFI calls work, Stage 1 still validates.

### Discovery: Compiler Forward Reference Limitation

The Stage 1 PACKAGEP* blocker (25 occurrences) persists because:
1. `compile-regular-call` in func-section.lisp:8128 calls `env-lookup-function`
2. Functions must be compiled BEFORE they can be called (no forward references)
3. When compiling `src/clysm/reader/package.lisp`, functions that call `packagep*` fail
   because `packagep*` hasn't been added to the function environment yet

**Potential Solutions** (future work):
- Pre-register all function names before compiling bodies
- Use late-binding/runtime dispatch for internal calls
- Compile files in dependency order (packagep* first)

---

## Phase 5: User Story 4 - Wasm Type Definition Generation (Priority: P2) ✅

**Goal**: Implement MAKE-WASM-STRUCT-TYPE as placeholder using existing type indices (0-28). Full dynamic registration deferred.

**Independent Test**: Call `(make-wasm-struct-type 'closure)` and verify returns +type-closure+ (index 5).

**Risk Level**: LOW - Deferred scope, using existing indices only

**Status (2026-01-01)**: Implementation complete. Functions work for host evaluation.
Stage 1 blocker count unchanged (17) - same forward reference limitation as Phase 4.
Original code calls defstruct constructor `make-wasm-struct-type`, not new `make-wasm-struct-type*`.

### Tests for User Story 4 (TDD Required)

- [x] T041 [P] [US4] Write unit test for known type lookup (closure → 3) in tests/unit/type-construction/test-known-types.lisp
- [x] T042 [P] [US4] Write unit test for unknown type error in tests/unit/type-construction/test-unknown-types.lisp
- [x] T043 [US4] Verify all tests FAIL (functions not yet defined) - VERIFIED: all NIL

### Implementation for User Story 4

- [x] T044 [US4] Create src/clysm/lib/type-construction.lisp with package declaration
- [x] T045 [US4] Define *known-type-indices* alist mapping type names to indices (29 types)
- [x] T046 [US4] Implement make-wasm-struct-type* returning placeholder with known index
- [x] T047 [US4] Implement type-index-for-name helper function
- [x] T048 [US4] Register type-construction.lisp in clysm.asd system definition
- [x] T049 [US4] Run Stage 1 compilation and verify no regressions - PASSED: 27731 bytes
- [ ] T050 [US4] Verify all tests from T041-T042 now PASS - DEFERRED: Tests need clysm/tests package setup

**Checkpoint**: Type construction placeholder complete. Known types work, unknown types error clearly.

### Note: Defstruct Constructor Issue

The blocker calls `make-wasm-struct-type` (defstruct constructor from gc-types.lisp:126).
The new function is `make-wasm-struct-type*` (with asterisk suffix per project conventions).
To fix the blocker, code in gc-types.lisp would need to use `make-wasm-struct-type*` instead.

---

## Phase 6: User Story 1 - Environment Management Compilation (Priority: P1) ⚠️ BLOCKED

**Goal**: Create Wasm-compilable versions of ENV-ADD-LOCAL, ENV-LOOKUP, ENV-ADD-CLOSURE-VAR using only primitives compilable to Wasm.

**Independent Test**: Compile `(defun test-let (x) (let ((y 1)) (+ x y)))` and verify env functions compile without error.

**Risk Level**: MEDIUM - Affects closure compilation, requires careful testing

### ⚠️ BLOCKED: Forward Reference Limitation

**Status (2026-01-01)**: Phase 6 blocked by the same forward reference limitation discovered in Phases 4-5.

**Issue**: Even if we implement `env-add-local-wasm`, functions that call it (e.g., compile-let, compile-lambda)
will fail with "Undefined function: ENV-ADD-LOCAL-WASM" because the function must be registered in the
compilation environment BEFORE it can be called.

**Root Cause**: `compile-regular-call` at func-section.lisp:8128 requires `env-lookup-function` to find
the function index at compile time, not at link time.

**Required Before Unblocking**:
1. Implement two-pass compilation (collect all function names first, compile bodies second)
2. OR implement lazy function resolution (defer lookup to call-time)
3. OR compile files in strict dependency order with function pre-registration

### Tests for User Story 1 (TDD Required)

- [ ] T051 [P] [US1] Write unit test for env-add-local-wasm in tests/unit/env-runtime/test-add-local.lisp
- [ ] T052 [P] [US1] Write unit test for env-lookup-wasm in tests/unit/env-runtime/test-lookup.lisp
- [ ] T053 [P] [US1] Write unit test for env-add-closure-var-wasm in tests/unit/env-runtime/test-closure-var.lisp
- [ ] T054 [P] [US1] Write integration test for nested let compilation in tests/integration/stage1/test-nested-let.lisp
- [ ] T055 [P] [US1] Write integration test for closure variable capture in tests/integration/stage1/test-closure-capture.lisp
- [ ] T056 [US1] Verify all tests FAIL (functions not yet defined)

### Implementation for User Story 1

- [ ] T057 [US1] Create src/clysm/lib/env-runtime.lisp with package declaration
- [ ] T058 [US1] Implement make-env-wasm using cons cells in src/clysm/lib/env-runtime.lisp
- [ ] T059 [US1] Implement env-locals accessor in src/clysm/lib/env-runtime.lisp
- [ ] T060 [US1] Implement env-counter-box accessor in src/clysm/lib/env-runtime.lisp
- [ ] T061 [US1] Implement env-add-local-wasm function in src/clysm/lib/env-runtime.lisp
- [ ] T062 [US1] Implement env-lookup-wasm function in src/clysm/lib/env-runtime.lisp
- [ ] T063 [US1] Implement env-add-closure-var-wasm function in src/clysm/lib/env-runtime.lisp
- [ ] T064 [US1] Implement extend-env-wasm for nested scopes in src/clysm/lib/env-runtime.lisp
- [ ] T065 [US1] Register env-runtime.lisp in clysm.asd system definition
- [ ] T066 [US1] Hook env-*-wasm functions into Stage 1 compilation pipeline in build/stage1-complete.lisp
- [ ] T067 [US1] Run Stage 1 compilation and verify ENV-ADD-LOCAL removed from blockers
- [ ] T068 [US1] Validate Wasm output with wasm-tools validate
- [ ] T069 [US1] Verify all tests from T051-T055 now PASS
- [ ] T070 [US1] Verify nested lambda/closure forms compile correctly

**Checkpoint**: Environment management complete. ENV-ADD-LOCAL blocker eliminated.

---

## Phase 7: User Story 2 - Instruction Compilation Dispatch (Priority: P1) ⚠️ BLOCKED

**Goal**: Create Wasm-compilable COMPILE-TO-INSTRUCTIONS dispatcher that handles all special forms.

**Independent Test**: Compile `(defun test-if (x) (if x 1 2))` and verify dispatch generates valid Wasm.

**Risk Level**: MEDIUM - Central dispatch, but builds on US1 environment work

**Dependency**: Requires US1 (environment management) complete

### ⚠️ BLOCKED: Forward Reference Limitation

**Status (2026-01-01)**: Phase 7 blocked by same issue as Phase 6. See research.md for details.

### Tests for User Story 2 (TDD Required)

- [ ] T071 [P] [US2] Write unit test for if-expression dispatch in tests/unit/compile-dispatch/test-if.lisp
- [ ] T072 [P] [US2] Write unit test for let-binding dispatch in tests/unit/compile-dispatch/test-let.lisp
- [ ] T073 [P] [US2] Write unit test for lambda dispatch in tests/unit/compile-dispatch/test-lambda.lisp
- [ ] T074 [P] [US2] Write unit test for progn dispatch in tests/unit/compile-dispatch/test-progn.lisp
- [ ] T075 [P] [US2] Write integration test for complex function compilation in tests/integration/stage1/test-complex-fn.lisp
- [ ] T076 [US2] Verify all tests FAIL (Wasm version not yet defined)

### Implementation for User Story 2

- [ ] T077 [US2] Create compile-to-instructions-wasm stub in src/clysm/lib/env-runtime.lisp
- [ ] T078 [US2] Implement literal dispatch case in compile-to-instructions-wasm
- [ ] T079 [US2] Implement var-ref dispatch case using env-lookup-wasm
- [ ] T080 [US2] Implement if dispatch case in compile-to-instructions-wasm
- [ ] T081 [US2] Implement let/let* dispatch case using env-add-local-wasm
- [ ] T082 [US2] Implement lambda dispatch case using env-add-closure-var-wasm
- [ ] T083 [US2] Implement progn dispatch case in compile-to-instructions-wasm
- [ ] T084 [US2] Implement block/return-from dispatch case
- [ ] T085 [US2] Implement fallback for unknown AST types (signal clear error)
- [ ] T086 [US2] Hook compile-to-instructions-wasm into Stage 1 pipeline
- [ ] T087 [US2] Run Stage 1 compilation and verify COMPILE-TO-INSTRUCTIONS removed from blockers
- [ ] T088 [US2] Validate Wasm output with wasm-tools validate
- [ ] T089 [US2] Verify all tests from T071-T075 now PASS
- [ ] T090 [US2] Verify compilation rate increased (target: 30%+)

**Checkpoint**: Instruction dispatch complete. COMPILE-TO-INSTRUCTIONS blocker eliminated.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, cleanup, and documentation

- [ ] T091 Run full Stage 1 compilation and compare to baseline from T001
- [ ] T092 Verify compilation rate meets SC-001 (22% → 30%+)
- [ ] T093 Verify all success criteria SC-002 through SC-008 from spec.md
- [ ] T094 [P] Update CLAUDE.md with new runtime library modules
- [ ] T095 [P] Update clysm.asd with all new source files
- [ ] T096 Delete baseline-report.json after successful validation
- [ ] T097 Run wasm-tools validate final time on dist/clysm-stage1.wasm
- [ ] T098 Verify file size < 50KB (2x baseline of 24.5KB)
- [ ] T099 Run full test suite via sbcl --eval "(asdf:test-system :clysm)"
- [ ] T100 Document lessons learned in specs/001-compiler-internal-consolidation/NOTES.md

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup ─────────────────┐
                                │
Phase 2: Foundational ──────────┼── BLOCKS all user stories
                                │
       ┌────────────────────────┘
       │
       ├── Phase 3: US5 (P3) Runtime Migration [LOW RISK - Start here]
       │
       ├── Phase 4: US3 (P2) Package Stubs [LOW RISK]
       │
       ├── Phase 5: US4 (P2) Type Construction [LOW RISK - Deferred]
       │
       └── Phase 6: US1 (P1) Environment Mgmt [MEDIUM RISK]
                     │
                     └── Phase 7: US2 (P1) Instruction Dispatch [MEDIUM RISK]
                                   │
                                   └── Phase 8: Polish
```

### User Story Dependencies

| Story | Depends On | Can Start After |
|-------|------------|-----------------|
| US5 (Runtime Migration) | Foundational only | Phase 2 complete |
| US3 (Package Stubs) | Foundational only | Phase 2 complete |
| US4 (Type Construction) | Foundational only | Phase 2 complete |
| US1 (Environment Mgmt) | Foundational only | Phase 2 complete |
| US2 (Instruction Dispatch) | US1 complete | Phase 6 complete |

### Within Each User Story

1. Write tests FIRST (TDD per Constitution VII)
2. Verify tests FAIL before implementation
3. Implement minimal code to pass tests
4. Validate with wasm-tools after each change
5. Check for regressions before marking complete

### Parallel Opportunities

**Phase 2 (Foundational)**: T006, T007, T008 can run in parallel

**Phase 3 (US5)**: T010-T013 tests can run in parallel

**Phase 4 (US3)**: T025-T027 tests can run in parallel

**Phase 5 (US4)**: T041-T042 tests can run in parallel

**Phase 6 (US1)**: T051-T055 tests can run in parallel

**Phase 7 (US2)**: T071-T075 tests can run in parallel

**Cross-Story**: US5, US3, US4, US1 can theoretically run in parallel after Phase 2 (different files)

---

## Parallel Example: User Story 5 (Runtime Migration)

```bash
# Launch all tests in parallel:
Task: "T010 [P] [US5] Write contract test for member"
Task: "T011 [P] [US5] Write contract test for assoc"
Task: "T012 [P] [US5] Write contract test for find"
Task: "T013 [P] [US5] Write contract test for position"

# Then sequential implementation (same file):
Task: "T015 [US5] Comment out compile-member"
Task: "T016 [US5] Comment out compile-assoc"
# ... etc
```

---

## Implementation Strategy

### Risk-Based Approach (Recommended)

Per research.md, implement in order of increasing risk:

1. **US5 (P3)**: Runtime migration - lowest risk, immediate code reduction
2. **US3 (P2)**: Package stubs - isolated FFI, low risk
3. **US4 (P2)**: Type construction - deferred scope, low risk
4. **US1 (P1)**: Environment management - medium risk, core infrastructure
5. **US2 (P1)**: Instruction dispatch - medium risk, depends on US1

### MVP First (US5 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: US5 (Runtime Migration)
4. **STOP and VALIDATE**: ~600 lines removed, zero regressions
5. Continue with US3, US4, US1, US2 incrementally

### Success Validation

After each user story:
- Run `sbcl --load build/stage1-complete.lisp`
- Run `wasm-tools validate dist/clysm-stage1.wasm`
- Compare `dist/stage1-report.json` to baseline
- Verify no regression in compiled form count

---

## Notes

- [P] tasks = different files, no dependencies on incomplete tasks
- [Story] label maps task to specific user story for traceability
- TDD required per Constitution VII - tests MUST fail before implementation
- Validate Wasm after EVERY change to catch issues early
- Keep baseline-report.json until all phases complete for regression checking
- US2 depends on US1 - cannot parallelize these two
