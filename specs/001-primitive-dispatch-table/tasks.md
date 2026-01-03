# Tasks: Primitive Dispatch Table

**Input**: Design documents from `/specs/001-primitive-dispatch-table/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: REQUIRED per Constitution VII (TDD). Tests written before implementation.

**Organization**: Tasks grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3, US4)
- Exact file paths included in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [ ] T001 Add primitive-dispatch.lisp and primitive-registry.lisp to clysm.asd in :components section
- [ ] T002 [P] Create package definition and file skeleton in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T003 [P] Create package definition and file skeleton in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T004 [P] Create test file skeleton in tests/unit/primitive-dispatch-test.lisp
- [ ] T005 [P] Create test file skeleton in tests/contract/primitive-dispatch-wasm-test.lisp

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data structures that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T006 Define primitive-entry struct (compiler-fn, arity, flags) in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T007 Create *primitive-symbol-table* hash-table with :test 'eq :size 300 in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T008 Create *primitive-string-table* hash-table with :test 'equal :size 50 in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T009 Define dispatch-primitive function skeleton (op args env) -> list in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T010 Define clear-primitive-tables function for testing in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T011 Export core symbols from primitive-dispatch package (primitive-entry, *primitive-symbol-table*, *primitive-string-table*, dispatch-primitive)

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Register New Primitive Compiler (Priority: P1) 🎯 MVP

**Goal**: Enable adding new primitives via registration API without modifying core dispatch

**Independent Test**: Register a custom test-primitive, verify it's callable and produces correct output

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T012 [P] [US1] Unit test: register-primitive-compiler creates entry in symbol table in tests/unit/primitive-dispatch-test.lisp
- [ ] T013 [P] [US1] Unit test: register-primitive-compiler with :string-name creates entry in both tables in tests/unit/primitive-dispatch-test.lisp
- [ ] T014 [P] [US1] Unit test: re-registration replaces existing entry in tests/unit/primitive-dispatch-test.lisp
- [ ] T015 [P] [US1] Unit test: primitive-registered-p returns T for registered, NIL for unregistered in tests/unit/primitive-dispatch-test.lisp
- [ ] T016 [P] [US1] Unit test: primitive-compiler-entry returns entry struct or NIL in tests/unit/primitive-dispatch-test.lisp
- [ ] T017 [P] [US1] Unit test: list-registered-primitives returns all registered symbols in tests/unit/primitive-dispatch-test.lisp

### Implementation for User Story 1

- [ ] T018 [US1] Implement register-primitive-compiler (symbol compiler-fn &key arity flags string-name) in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T019 [US1] Implement unregister-primitive-compiler (symbol &key string-name) in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T020 [US1] Implement primitive-registered-p (op) in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T021 [US1] Implement primitive-compiler-entry (op) with symbol-first, string-fallback lookup in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T022 [US1] Implement list-registered-primitives (&key table) in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T023 [US1] Implement describe-primitive (op &optional stream) for REPL debugging in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T024 [US1] Export registration API functions from package in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T025 [US1] Run tests and verify all US1 unit tests pass

**Checkpoint**: Registration API complete - can register/query primitives but not yet dispatch

---

## Phase 4: User Story 2 - Compile Standard Primitives via Symbol Lookup (Priority: P1)

**Goal**: Standard primitives (+, -, cons, car) compile correctly using symbol-based dispatch

**Independent Test**: Compile (+ 1 2) and (cons 1 2) via new dispatch, verify Wasm output matches old behavior

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T026 [P] [US2] Unit test: dispatch-primitive returns instructions for registered symbol in tests/unit/primitive-dispatch-test.lisp
- [ ] T027 [P] [US2] Unit test: dispatch-primitive returns NIL for unregistered symbol in tests/unit/primitive-dispatch-test.lisp
- [ ] T028 [P] [US2] Contract test: (+ 1 2) generates valid Wasm via table dispatch in tests/contract/primitive-dispatch-wasm-test.lisp
- [ ] T029 [P] [US2] Contract test: (cons 1 2) generates valid Wasm via table dispatch in tests/contract/primitive-dispatch-wasm-test.lisp
- [ ] T030 [P] [US2] Contract test: nested primitives (car (cons 1 2)) dispatch correctly in tests/contract/primitive-dispatch-wasm-test.lisp

### Implementation for User Story 2

- [ ] T031 [US2] Complete dispatch-primitive implementation: lookup entry, funcall compiler-fn in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T032 [US2] Add dispatch-primitive call at start of compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [ ] T033 [US2] Register arithmetic operators (+, -, *, /, truncate, mod, rem) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T034 [US2] Register rounding functions (floor, ceiling, round, ffloor, fceiling, fround) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T035 [US2] Register increment/decrement (1+, 1-) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T036 [US2] Register comparison operators (<, >, <=, >=, =, /=) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T037 [US2] Register list operations (cons, car, cdr, list, first, rest, nth, nthcdr) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T038 [US2] Register cXXr accessors (caar, cadr, cdar, cddr, caaar...cdddr) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T039 [US2] Register type predicates (consp, null, not, atom, listp, integerp, floatp, symbolp) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T040 [US2] Run existing compiler test suite to verify no regressions from registered primitives

**Checkpoint**: Symbol-based dispatch working for core primitives

---

## Phase 5: User Story 3 - Compile Cross-Package Symbols via String Lookup (Priority: P2)

**Goal**: Cross-package symbols (%SETF-AREF, MAKE-INSTANCE*) dispatch correctly by name

**Independent Test**: Register %SETF-AREF with string key, compile setf form from different package, verify dispatch

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T041 [P] [US3] Unit test: dispatch-primitive finds string-table entry when symbol-table lookup fails in tests/unit/primitive-dispatch-test.lisp
- [ ] T042 [P] [US3] Unit test: symbol-table lookup takes precedence over string-table in tests/unit/primitive-dispatch-test.lisp
- [ ] T043 [P] [US3] Contract test: %SETF-AREF dispatches correctly from any package in tests/contract/primitive-dispatch-wasm-test.lisp
- [ ] T044 [P] [US3] Contract test: MAKE-INSTANCE* dispatches correctly via string lookup in tests/contract/primitive-dispatch-wasm-test.lisp

### Implementation for User Story 3

- [ ] T045 [US3] Verify string-fallback logic in dispatch-primitive in src/clysm/compiler/codegen/primitive-dispatch.lisp
- [ ] T046 [US3] Register %SETF-* primitives with :string-name (%SETF-AREF, %SETF-SVREF, %SETF-SCHAR, %SETF-ELT, %SETF-GETF, %SETF-ROW-MAJOR-AREF) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T047 [US3] Register CLOS primitives with :string-name (MAKE-INSTANCE*, SLOT-VALUE*, SET-SLOT-VALUE*, STANDARD-INSTANCE-P) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T048 [US3] Register MOP primitives with :string-name (INSTANCE-CLASS, CLASS-NAME) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T049 [US3] Register runtime setup stubs with :string-name (REGISTER-STRUCTURE-CLASS, DEFINE-CLASS*, REGISTER-SETF-EXPANDER) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T050 [US3] Remove cond string= block from compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [ ] T051 [US3] Run tests to verify string-based dispatch works correctly

**Checkpoint**: String-based lookup complete - all 18 cross-package primitives migrated

---

## Phase 6: User Story 4 - Maintain Backward Compatibility (Priority: P1)

**Goal**: All 248 case-statement primitives migrated, exact behavior preserved

**Independent Test**: Full compiler test suite passes with zero modifications

### Tests for User Story 4

- [ ] T052 [US4] Run full compiler test suite (sbcl --eval "(asdf:test-system :clysm)") to establish baseline
- [ ] T053 [US4] Verify Stage 1 generation produces valid Wasm (sbcl --load build/stage1-complete.lisp)

### Implementation for User Story 4

- [ ] T054 [P] [US4] Register equality predicates (eq, eql, equal, equalp) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T055 [P] [US4] Register numeric predicates (zerop, plusp, minusp, oddp, evenp, signum) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T056 [P] [US4] Register bitwise operations (logand, logior, logxor, lognot, ash, logcount, integer-length, logbitp, logtest) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T057 [P] [US4] Register byte operations (byte, byte-size, byte-position, ldb, dpb, mask-field, deposit-field) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T058 [P] [US4] Register sequence functions (length, append, reverse, nreverse, last, butlast, copy-list) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T059 [P] [US4] Register higher-order functions (mapcar, mapc, maplist, reduce, every, some, notany, notevery) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T060 [P] [US4] Register alist/set operations (acons, pairlis, copy-alist, adjoin, union, intersection, set-difference, subsetp) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T061 [P] [US4] Register character functions (char-code, code-char, char=, char<, char>, upper-case-p, lower-case-p) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T062 [P] [US4] Register extended character functions (graphic-char-p, standard-char-p, both-case-p, char-name, name-char, digit-char, char-int) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T063 [P] [US4] Register string predicates and comparison (stringp, string=, string<, string-equal, etc.) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T064 [P] [US4] Register string manipulation (make-string, string, string-upcase, string-downcase, string-capitalize) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T065 [P] [US4] Register string trim functions (string-trim, string-left-trim, string-right-trim, nstring-upcase, nstring-downcase) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T066 [P] [US4] Register numeric accessors and conversion (numerator, denominator, complex, realpart, imagpart, float, rational) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T067 [P] [US4] Register trigonometric functions (sin, cos, tan, asin, acos, atan, sinh, cosh, tanh) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T068 [P] [US4] Register math functions (exp, log, sqrt, expt, abs, max, min, gcd, lcm) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T069 [P] [US4] Register hash table operations (make-hash-table, gethash, puthash, remhash, maphash, hash-table-count) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T070 [P] [US4] Register array operations (make-array, aref, svref, elt, coerce, array-rank, array-dimension) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T071 [P] [US4] Register I/O and misc (error, gensym, typep, apply, funcall, vector-push-extend, write-byte) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T072 [P] [US4] Register property list and symbol operations (getf, symbol-name, keywordp, rplaca, rplacd, nconc) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T073 [P] [US4] Register remaining list accessors (second through tenth) in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T074 [P] [US4] Register subseq, concatenate, copy-seq, find-package, intern in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T075 [P] [US4] Register write-to-string, parse-integer, rationalize in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T076 [US4] Remove empty case statement from compile-primitive-call in src/clysm/compiler/codegen/func-section.lisp
- [ ] T077 [US4] Run full compiler test suite to verify all tests pass
- [ ] T078 [US4] Validate Stage 1 Wasm output with wasm-tools validate dist/clysm-stage1.wasm

**Checkpoint**: All 266 primitives migrated, case statement removed, full backward compatibility verified

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, cleanup, and validation

- [ ] T079 [P] Add HyperSpec links for all registered primitives as comments in src/clysm/compiler/codegen/primitive-registry.lisp
- [ ] T080 [P] Update CLAUDE.md Recent Changes section with feature summary
- [ ] T081 [P] Add primitive dispatch to clysm.asd :depends-on if needed
- [ ] T082 Verify all primitive-dispatch-test.lisp tests pass
- [ ] T083 Verify all primitive-dispatch-wasm-test.lisp tests pass
- [ ] T084 Remove obsolete comments referencing old case statement from func-section.lisp
- [ ] T085 Run Stage 1 and Stage 2 generation to verify fixpoint not broken

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational - provides registration API
- **User Story 2 (Phase 4)**: Depends on US1 - uses registration to migrate symbol primitives
- **User Story 3 (Phase 5)**: Depends on US1 - uses registration for string primitives
- **User Story 4 (Phase 6)**: Depends on US1, US2, US3 - completes full migration
- **Polish (Phase 7)**: Depends on US4 completion

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P1)**: Can start after US1 - Requires registration API
- **User Story 3 (P2)**: Can start after US1 - Requires registration API, parallel with US2
- **User Story 4 (P1)**: Requires US1, US2, US3 complete for full migration

### Parallel Opportunities

**Phase 1 Setup**: T002, T003, T004, T005 can run in parallel

**Phase 3 US1 Tests**: T012-T017 can run in parallel (different test cases)

**Phase 4 US2 Tests**: T026-T030 can run in parallel

**Phase 5 US3 Tests**: T041-T044 can run in parallel

**Phase 6 US4 Registrations**: T054-T075 can run in parallel (different primitive categories)

---

## Parallel Example: User Story 4 Mass Registration

```bash
# Launch all primitive category registrations together:
Task: "Register equality predicates (eq, eql, equal, equalp)"
Task: "Register numeric predicates (zerop, plusp, minusp, oddp, evenp)"
Task: "Register bitwise operations (logand, logior, logxor, lognot, ash)"
Task: "Register sequence functions (length, append, reverse, nreverse)"
# ... all T054-T075 can run in parallel
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test registration API independently
5. Demo: Register a new primitive without modifying dispatch function

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add US1 → Test registration API → MVP achieved!
3. Add US2 → Test symbol dispatch → Core primitives working
4. Add US3 → Test string dispatch → Cross-package support
5. Add US4 → Complete migration → Full backward compatibility
6. Each story adds value without breaking previous stories

---

## Notes

- Constitution VII requires TDD: Write tests FIRST, ensure they FAIL before implementing
- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Commit after each task or logical group
- Run `sbcl --eval "(asdf:test-system :clysm)"` frequently to catch regressions
- Run `wasm-tools validate` on Wasm output to verify correctness
