# Tasks: CLOS Foundation

**Input**: Design documents from `/specs/026-clos-foundation/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: REQUIRED per Constitution Principle VII (TDD)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/clysm/`, `tests/` at repository root
- Paths follow existing clysm compiler structure

---

## Phase 1: Setup (Test Infrastructure)

**Purpose**: Create test infrastructure for CLOS components

- [X] T001 Create unit test directory at tests/unit/clos/
- [X] T002 [P] Create contract test file at tests/contract/clos-wasm-test.lisp with test skeleton
- [X] T003 [P] Create integration test file at tests/integration/clos-ansi-test.lisp with test skeleton
- [X] T004 Update tests/package.lisp to include new CLOS test packages

---

## Phase 2: Foundational (WasmGC Types)

**Purpose**: Core WasmGC type infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Tests for Foundational Phase

- [X] T005 [P] Write contract test for $instance type (index 6) structure in tests/contract/clos-wasm-test.lisp
- [X] T006 [P] Write contract test for $standard-class type (index 7) structure in tests/contract/clos-wasm-test.lisp
- [X] T007 [P] Write contract test for $slot-vector array type (index 21) in tests/contract/clos-wasm-test.lisp
- [X] T008 [P] Write contract test for $keyword-array type (index 22) in tests/contract/clos-wasm-test.lisp
- [X] T009 [P] Write contract test for $closure-array type (index 23) in tests/contract/clos-wasm-test.lisp

### Implementation for Foundational Phase

- [X] T010 Define type index constants +type-instance+ (6), +type-standard-class+ (7) in src/clysm/compiler/codegen/gc-types.lisp
- [X] T011 Define type index constants +type-slot-vector+ (21), +type-keyword-array+ (22), +type-closure-array+ (23) in src/clysm/compiler/codegen/gc-types.lisp
- [X] T012 Implement make-instance-type function returning $instance struct definition in src/clysm/compiler/codegen/gc-types.lisp
- [X] T013 Implement make-standard-class-type function returning $standard-class struct definition in src/clysm/compiler/codegen/gc-types.lisp
- [X] T014 Implement make-slot-vector-type function returning array (mut anyref) definition in src/clysm/compiler/codegen/gc-types.lisp
- [X] T015 [P] Implement make-keyword-array-type function returning array (ref $symbol) definition in src/clysm/compiler/codegen/gc-types.lisp
- [X] T016 [P] Implement make-closure-array-type function returning array (ref null $closure) definition in src/clysm/compiler/codegen/gc-types.lisp
- [X] T017 Update generate-type-definitions to emit types 6, 7, 21, 22, 23 (replacing nil placeholders) in src/clysm/compiler/codegen/gc-types.lisp
- [X] T018 Update emit-type-section to encode CLOS types in correct binary format in src/clysm/compiler/compiler.lisp
- [X] T019 Export new type constants from clysm/compiler/codegen/gc-types package in src/clysm/package.lisp
- [X] T020 Run contract tests T005-T009 and verify all pass with wasm-tools validate

**Checkpoint**: Foundation ready - CLOS type infrastructure complete, user story implementation can now begin

---

## Phase 3: User Story 1 - Define a Class with Slots (Priority: P1) 🎯 MVP

**Goal**: Developers can define classes using `defclass` with slots having `:initarg`, `:accessor`, `:initform`

**Independent Test**: Define `(defclass point () ((x :initarg :x) (y :initarg :y)))` and verify class metadata is created

### Tests for User Story 1

- [X] T021 [P] [US1] Write unit test for defclass AST parsing in tests/unit/clos/defclass-test.lisp
- [X] T022 [P] [US1] Write unit test for slot-definition creation with :initarg/:accessor/:initform in tests/unit/clos/defclass-test.lisp
- [X] T023 [P] [US1] Write unit test for class registration in class registry in tests/unit/clos/defclass-test.lisp
- [X] T024 [P] [US1] Write unit test for class precedence list computation in tests/unit/clos/defclass-test.lisp
- [X] T025 [P] [US1] Write contract test for class global emission ($class-point) in tests/contract/clos-wasm-test.lisp
- [X] T026 [US1] Write integration test: compile defclass and verify Wasm validates in tests/integration/clos-ansi-test.lisp

### Implementation for User Story 1

- [X] T027 [US1] Add ast-defclass struct (name, superclass, slots, options) in src/clysm/compiler/ast.lisp
- [X] T028 [US1] Add ast-slot-definition struct (name, initarg, accessor, initform, index) in src/clysm/compiler/ast.lisp
- [X] T029 [US1] Implement parse-defclass-to-ast function converting defclass form to ast-defclass in src/clysm/compiler/ast.lisp
- [X] T030 [US1] Implement compile-defclass in src/clysm/compiler/codegen/func-section.lisp (class global emission)
- [X] T031 [US1] Implement emit-class-global generating $class-{name} global with struct.new in src/clysm/compiler/codegen/func-section.lisp
- [X] T032 [US1] Implement class-id allocation (sequential i32 counter) in src/clysm/compiler/codegen/func-section.lisp
- [X] T033 [US1] Integrate defclass compilation into main compile-form dispatch in src/clysm/compiler/compiler.lisp
- [X] T034 [US1] Export ast-defclass, parse-defclass-to-ast from clysm/compiler/ast package in src/clysm/package.lisp
- [X] T035 [US1] Run unit tests T021-T024 and verify all pass
- [X] T036 [US1] Run contract test T025 and integration test T026, verify Wasm validates

**Checkpoint**: User Story 1 complete - `defclass` works, class metadata generated

---

## Phase 4: User Story 2 - Create Instances with make-instance (Priority: P1)

**Goal**: Developers can create instances with `(make-instance 'class-name :slot1 val1 ...)`

**Independent Test**: `(make-instance 'point :x 3 :y 4)` creates instance with correct slot values

### Tests for User Story 2

- [X] T037 [P] [US2] Write unit test for make-instance AST parsing in tests/unit/clos/make-instance-test.lisp
- [X] T038 [P] [US2] Write unit test for initarg matching to slot indices in tests/unit/clos/make-instance-test.lisp
- [X] T039 [P] [US2] Write unit test for initform evaluation when initarg not provided in tests/unit/clos/make-instance-test.lisp
- [X] T040 [P] [US2] Write unit test for UNBOUND sentinel when no initarg/initform in tests/unit/clos/make-instance-test.lisp
- [X] T041 [P] [US2] Write contract test for instance creation codegen (struct.new $instance) in tests/contract/clos-wasm-test.lisp
- [X] T042 [US2] Write integration test: full point class instantiation in tests/integration/clos-ansi-test.lisp

### Implementation for User Story 2

- [X] T043 [US2] Add ast-make-instance struct (class-name, initargs) in src/clysm/compiler/ast.lisp
- [X] T044 [US2] Implement parse-make-instance-to-ast function in src/clysm/compiler/ast.lisp
- [X] T045 [US2] Implement compile-make-instance generating slot allocation + struct.new in src/clysm/compiler/codegen/func-section.lisp
- [X] T046 [US2] Implement emit-slot-initialization loop (initargs lookup, initform eval, UNBOUND fallback) in src/clysm/compiler/codegen/func-section.lisp
- [X] T047 [US2] Implement class lookup validation (signal error for undefined class) in src/clysm/compiler/codegen/func-section.lisp
- [X] T048 [US2] Integrate make-instance compilation into main compile-form dispatch in src/clysm/compiler/compiler.lisp
- [X] T049 [US2] Export ast-make-instance, parse-make-instance-to-ast in src/clysm/package.lisp
- [X] T050 [US2] Run unit tests T037-T040 and verify all pass
- [X] T051 [US2] Run contract test T041 and integration test T042, verify Wasm validates

**Checkpoint**: User Stories 1+2 complete - can define classes AND create instances

---

## Phase 5: User Story 3 - Access Slots via Accessors (Priority: P2)

**Goal**: Developers can read/write slots using generated accessor functions

**Independent Test**: `(point-x instance)` returns slot value; `(setf (point-x instance) 10)` updates it

### Tests for User Story 3

- [X] T052 [P] [US3] Write unit test for accessor reader generation in tests/unit/clos/accessor-test.lisp
- [X] T053 [P] [US3] Write unit test for accessor writer (setf) generation in tests/unit/clos/accessor-test.lisp
- [X] T054 [P] [US3] Write unit test for type checking in accessor (error on non-instance) in tests/unit/clos/accessor-test.lisp
- [X] T055 [P] [US3] Write contract test for slot read codegen (array.get $slot-vector) in tests/contract/clos-wasm-test.lisp
- [X] T056 [P] [US3] Write contract test for slot write codegen (array.set $slot-vector) in tests/contract/clos-wasm-test.lisp
- [X] T057 [US3] Write integration test: accessor read/write roundtrip in tests/integration/clos-ansi-test.lisp

### Implementation for User Story 3

- [X] T058 [US3] Implement compile-slot-reader generating struct.get + array.get in src/clysm/compiler/codegen/func-section.lisp
- [X] T059 [US3] Implement compile-slot-writer generating struct.get + array.set in src/clysm/compiler/codegen/func-section.lisp
- [X] T060 [US3] Implement generate-accessor-functions creating reader/writer for each :accessor slot in src/clysm/compiler/codegen/func-section.lisp
- [X] T061 [US3] Add ref.cast $instance type check before slot access in accessor codegen in src/clysm/compiler/codegen/func-section.lisp
- [X] T062 [US3] Integrate accessor generation into compile-defclass in src/clysm/compiler/codegen/func-section.lisp
- [X] T063 [US3] Run unit tests T052-T054 and verify all pass
- [X] T064 [US3] Run contract tests T055-T056 and integration test T057, verify Wasm validates

**Checkpoint**: User Story 3 complete - accessors work for slot read/write

---

## Phase 6: User Stories 4+5 - Methods and Dispatch (Priority: P2)

**Goal**: Developers can define methods with `defmethod` and generic functions dispatch to correct method

**Independent Test**: Define `(defmethod area ((p point)) ...)` and `(area some-point)` calls correct implementation

### Tests for User Stories 4+5

- [X] T065 [P] [US4] Write unit test for defmethod AST parsing in tests/unit/clos/defmethod-test.lisp
- [X] T066 [P] [US4] Write unit test for method specializer extraction in tests/unit/clos/defmethod-test.lisp
- [X] T067 [P] [US4] Write unit test for generic function implicit creation in tests/unit/clos/defmethod-test.lisp
- [X] T068 [P] [US5] Write unit test for compute-applicable-methods in tests/unit/clos/defmethod-test.lisp
- [X] T069 [P] [US5] Write unit test for method specificity sorting in tests/unit/clos/defmethod-test.lisp
- [X] T070 [P] [US5] Write contract test for dispatch codegen (class-id lookup + call) in tests/contract/clos-wasm-test.lisp
- [X] T071 [US4] Write integration test: single method definition and call in tests/integration/clos-ansi-test.lisp
- [X] T072 [US5] Write integration test: multi-method dispatch in tests/integration/clos-ansi-test.lisp

### Implementation for User Stories 4+5

- [X] T073 [US4] Add ast-defmethod struct (name, specializers, lambda-list, body) in src/clysm/compiler/ast.lisp
- [X] T074 [US4] Implement parse-defmethod-to-ast function in src/clysm/compiler/ast.lisp
- [X] T075 [US4] Implement compile-defmethod generating method closure + GF registration in src/clysm/compiler/codegen/func-section.lisp
- [X] T076 [US4] Implement generic function registry (create GF on first defmethod) in src/clysm/compiler/codegen/func-section.lisp
- [X] T077 [US5] Implement compile-gf-call generating dispatch code with class-id lookup in src/clysm/compiler/codegen/func-section.lisp
- [X] T078 [US5] Implement $instance-of-class helper function for superclass chain walk in src/clysm/compiler/codegen/func-section.lisp
- [X] T079 [US5] Implement no-applicable-method error signaling in dispatch fallback in src/clysm/compiler/codegen/func-section.lisp
- [X] T080 [US4] Integrate defmethod compilation into main compile-form dispatch in src/clysm/compiler/compiler.lisp
- [X] T081 [US5] Integrate generic function call compilation for known GF names in src/clysm/compiler/compiler.lisp
- [X] T082 [US4] Export ast-defmethod, parse-defmethod-to-ast in src/clysm/package.lisp
- [X] T083 [US4] Run unit tests T065-T069 and verify all pass
- [X] T084 [US5] Run contract test T070 and integration tests T071-T072, verify Wasm validates

**Checkpoint**: User Stories 4+5 complete - defmethod and dispatch work

---

## Phase 7: User Story 6 - Single Inheritance (Priority: P3)

**Goal**: Developers can define subclasses that inherit slots from parent classes

**Independent Test**: `(defclass colored-point (point) ((color ...)))` inherits x, y slots from point

### Tests for User Story 6

- [X] T085 [P] [US6] Write unit test for superclass resolution in defclass in tests/unit/clos/defclass-test.lisp
- [X] T086 [P] [US6] Write unit test for slot inheritance (parent slots prepended) in tests/unit/clos/defclass-test.lisp
- [X] T087 [P] [US6] Write unit test for duplicate slot detection across hierarchy in tests/unit/clos/defclass-test.lisp
- [X] T088 [P] [US6] Write unit test for multiple inheritance rejection (error signal) in tests/unit/clos/defclass-test.lisp
- [X] T089 [P] [US6] Write contract test for $superclass field linking in class global in tests/contract/clos-wasm-test.lisp
- [X] T090 [US6] Write integration test: subclass instance with inherited+own slots in tests/integration/clos-ansi-test.lisp
- [X] T091 [US6] Write integration test: method on parent applies to child instance in tests/integration/clos-ansi-test.lisp

### Implementation for User Story 6

- [X] T092 [US6] Implement superclass resolution in compile-defclass (lookup class, verify finalized) in src/clysm/compiler/codegen/func-section.lisp
- [X] T093 [US6] Implement slot inheritance (prepend parent slots, reindex) in compile-defclass in src/clysm/compiler/codegen/func-section.lisp
- [X] T094 [US6] Implement duplicate slot detection including inherited slots in src/clysm/compiler/codegen/func-section.lisp
- [X] T095 [US6] Implement multiple inheritance check (error if >1 superclass) in src/clysm/compiler/codegen/func-section.lisp
- [X] T096 [US6] Implement circular inheritance detection in superclass resolution in src/clysm/compiler/codegen/func-section.lisp
- [X] T097 [US6] Update emit-class-global to set $superclass field reference in src/clysm/compiler/codegen/func-section.lisp
- [X] T098 [US6] Update $instance-of-class to walk superclass chain for dispatch in src/clysm/compiler/codegen/func-section.lisp
- [X] T099 [US6] Run unit tests T085-T088 and verify all pass
- [X] T100 [US6] Run contract test T089 and integration tests T090-T091, verify Wasm validates

**Checkpoint**: User Story 6 complete - single inheritance works

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup

- [X] T101 [P] Update CLAUDE.md with Feature 026 documentation summary
- [X] T102 [P] Run full test suite (nix flake check) and fix any failures
- [X] T103 [P] Validate verification example: `(make-instance 'point :x 3 :y 4)` with accessor calls
- [X] T104 Validate verification example: defmethod with dispatch works correctly
- [X] T105 Run wasm-tools validate on generated Wasm for all test cases
- [X] T106 Update src/clysm/package.lisp exports documentation comments
- [X] T107 Code review: verify Constitution compliance (TDD, WasmGC-first, no linear memory)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - class definition
- **User Story 2 (Phase 4)**: Depends on US1 - instance creation needs classes
- **User Story 3 (Phase 5)**: Depends on US1 - accessors need class slots
- **User Stories 4+5 (Phase 6)**: Depends on US1 - methods need classes
- **User Story 6 (Phase 7)**: Depends on US1, enhances dispatch from US5
- **Polish (Phase 8)**: Depends on all user stories complete

### User Story Dependencies

```
Phase 2 (Foundational)
       │
       ▼
Phase 3 (US1: defclass) ──────────────────────────┐
       │                                          │
       ├──────────────────┬───────────────────────┤
       ▼                  ▼                       ▼
Phase 4 (US2)      Phase 5 (US3)         Phase 6 (US4+5)
(make-instance)    (accessors)           (methods/dispatch)
       │                                          │
       └──────────────────────────────────────────┤
                                                  ▼
                                          Phase 7 (US6)
                                          (inheritance)
                                                  │
                                                  ▼
                                          Phase 8 (Polish)
```

### Parallel Opportunities

**Within Phase 2 (Foundational)**:
- T005-T009 (contract tests) can run in parallel
- T015-T016 (array type definitions) can run in parallel

**Within Phase 3 (US1)**:
- T021-T025 (tests) can run in parallel
- After tests pass, T027-T028 (AST structs) can run in parallel

**Within Phase 4 (US2)**:
- T037-T041 (tests) can run in parallel

**Within Phase 5 (US3)**:
- T052-T056 (tests) can run in parallel

**Within Phase 6 (US4+5)**:
- T065-T070 (tests) can run in parallel
- T073-T074 (AST structs) can run in parallel

**Within Phase 7 (US6)**:
- T085-T089 (tests) can run in parallel

---

## Parallel Example: Phase 2 Tests

```bash
# Launch all foundational contract tests together:
Task: "Write contract test for $instance type structure in tests/contract/clos-wasm-test.lisp"
Task: "Write contract test for $standard-class type structure in tests/contract/clos-wasm-test.lisp"
Task: "Write contract test for $slot-vector array type in tests/contract/clos-wasm-test.lisp"
Task: "Write contract test for $keyword-array type in tests/contract/clos-wasm-test.lisp"
Task: "Write contract test for $closure-array type in tests/contract/clos-wasm-test.lisp"
```

---

## Implementation Strategy

### MVP First (User Stories 1+2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (WasmGC types)
3. Complete Phase 3: User Story 1 (defclass)
4. Complete Phase 4: User Story 2 (make-instance)
5. **STOP and VALIDATE**: Test `(make-instance 'point :x 3 :y 4)` end-to-end
6. MVP achieved - can define classes and create instances

### Incremental Delivery

1. Complete Setup + Foundational → Type infrastructure ready
2. Add US1 (defclass) → Can define classes
3. Add US2 (make-instance) → Can create instances → **MVP!**
4. Add US3 (accessors) → Can read/write slots
5. Add US4+5 (methods/dispatch) → Full polymorphism
6. Add US6 (inheritance) → Class hierarchies

### TDD Cycle per User Story

For each user story phase:
1. Write all unit tests first (T0XX [P] [USN] tests) - must FAIL
2. Write contract tests - must FAIL
3. Implement models/AST structures
4. Implement services/codegen
5. Re-run tests - all must PASS
6. Run wasm-tools validate on output
7. Checkpoint: story independently testable

---

## Summary Statistics

| Metric | Count |
|--------|-------|
| Total Tasks | 107 |
| Setup Phase | 4 |
| Foundational Phase | 16 |
| US1 (defclass) | 16 |
| US2 (make-instance) | 15 |
| US3 (accessors) | 13 |
| US4+5 (methods/dispatch) | 20 |
| US6 (inheritance) | 16 |
| Polish Phase | 7 |
| Parallelizable Tasks | 47 |

---

## Notes

- All tests MUST be written and FAIL before implementation (Constitution Principle VII)
- [P] tasks = different files, no dependencies
- [USN] label maps task to specific user story for traceability
- Commit after each task or logical group
- Run `wasm-tools validate` after each phase completion
- Stop at any checkpoint to validate story independently
