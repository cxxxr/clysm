# Feature Specification: Self-Hosting Blockers Resolution

**Feature Branch**: `043-self-hosting-blockers`
**Created**: 2025-12-28
**Status**: Draft
**Input**: User description: "Feature 043: Self-Hosting Blockers Resolution を実装する。目標はStage 0コンパイル率を50%以上に引き上げること。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Stage 0 Compilation Rate Improvement (Priority: P1)

As the Clysm bootstrap process, the Stage 0 compiler needs to successfully compile at least 50% of the compiler's own source forms to demonstrate progress toward self-hosting capability.

**Why this priority**: This is the primary goal of the entire feature. Without achieving the 50% compilation rate threshold, the self-hosting roadmap cannot progress. All other stories contribute to this goal.

**Independent Test**: Run the bootstrap compilation process and measure the percentage of forms that compile successfully. Current baseline is ~20%, target is 50%+.

**Acceptance Scenarios**:

1. **Given** the Clysm compiler source code (45 modules), **When** the Stage 0 bootstrap compilation runs, **Then** at least 50% of compilable forms are successfully compiled without error.
2. **Given** the compiler/codegen/ directory containing core compilation logic, **When** all files in this directory are processed, **Then** every file has at least one successfully compiled form.
3. **Given** the compilation progress report, **When** the bootstrap process completes, **Then** a machine-readable report shows form counts, success rates, and categorized failures.

---

### User Story 2 - LOOP Macro Iteration Support (Priority: P1)

As compiler source code using LOOP constructs, iteration patterns need to be understood and processed so that forms containing common LOOP clauses can be compiled.

**Why this priority**: LOOP is one of the most frequently used macros in Common Lisp code. The compiler source heavily relies on LOOP for iteration. Without LOOP support, many critical forms cannot compile.

**Independent Test**: Compile forms containing LOOP with for/as, collect, append, count, sum, maximize, and minimize clauses. Verify each clause type produces correct results.

**Acceptance Scenarios**:

1. **Given** a form with `(loop for x in list collect x)`, **When** compiled and executed, **Then** it returns a list of the same elements.
2. **Given** a form with `(loop for i from 1 to 10 sum i)`, **When** compiled and executed, **Then** it returns 55.
3. **Given** a form with `(loop for x in list count (evenp x))`, **When** compiled and executed, **Then** it returns the count of even elements.
4. **Given** a form with `(loop for x in list append (list x x))`, **When** compiled and executed, **Then** it returns a flattened list with each element duplicated.
5. **Given** a form with `(loop for x in list maximize x)`, **When** compiled and executed, **Then** it returns the maximum value.
6. **Given** a form with `(loop for x in list minimize x)`, **When** compiled and executed, **Then** it returns the minimum value.

---

### User Story 3 - Function Default Values (Priority: P1)

As compiler source code using &optional and &key parameters with default values, function definitions need to properly handle default value initialization.

**Why this priority**: Many compiler functions use default values for optional parameters. Without this support, those function definitions cannot be compiled.

**Independent Test**: Compile and call functions with &optional and &key parameters that have default values. Verify defaults are used when arguments are omitted.

**Acceptance Scenarios**:

1. **Given** `(defun foo (&optional (x 10)) x)`, **When** called as `(foo)`, **Then** it returns 10.
2. **Given** `(defun bar (&optional (x 1) (y 2)) (+ x y))`, **When** called as `(bar 5)`, **Then** it returns 7 (5 + 2).
3. **Given** `(defun baz (&key (name "default")) name)`, **When** called as `(baz)`, **Then** it returns "default".
4. **Given** `(defun qux (&key (a 1) (b 2)) (list a b))`, **When** called as `(qux :b 20)`, **Then** it returns (1 20).

---

### User Story 4 - Hash Table Operations (Priority: P2)

As compiler source code using hash tables for registries and caches, hash table creation and manipulation operations need to be available.

**Why this priority**: The compiler uses hash tables for symbol tables, macro registries, and various caches. These are core data structures but are less frequently used than LOOP or default parameters in the bootstrap subset.

**Independent Test**: Create hash tables, store and retrieve values, remove entries, and iterate over contents. Verify all operations work correctly.

**Acceptance Scenarios**:

1. **Given** `(make-hash-table)`, **When** executed, **Then** a new empty hash table is created.
2. **Given** a hash table and `(setf (gethash 'key ht) 'value)`, **When** executed, **Then** the key-value pair is stored.
3. **Given** a hash table with entries, **When** `(gethash 'key ht)` is called, **Then** the stored value is returned (with second value indicating presence).
4. **Given** a hash table with entries, **When** `(remhash 'key ht)` is called, **Then** the entry is removed and T is returned if it existed.
5. **Given** a hash table with entries, **When** `(maphash fn ht)` is called, **Then** the function is called for each key-value pair.

---

### User Story 5 - Association List Functions (Priority: P2)

As compiler source code using association lists for lightweight mappings, lookup and manipulation functions need to be available.

**Why this priority**: Association lists are used throughout the compiler for environment bindings, property lists, and other lightweight mappings. They complement hash tables for smaller datasets.

**Independent Test**: Test assoc, rassoc, member, adjoin, union, and intersection with various inputs and :test/:key options.

**Acceptance Scenarios**:

1. **Given** `(assoc 'b '((a . 1) (b . 2) (c . 3)))`, **When** executed, **Then** returns (b . 2).
2. **Given** `(rassoc 2 '((a . 1) (b . 2) (c . 3)))`, **When** executed, **Then** returns (b . 2).
3. **Given** `(member 'b '(a b c d))`, **When** executed, **Then** returns (b c d).
4. **Given** `(adjoin 'x '(a b c))`, **When** executed, **Then** returns (x a b c) if x not present.
5. **Given** `(union '(a b) '(b c))`, **When** executed, **Then** returns a list containing a, b, and c (order unspecified).
6. **Given** `(intersection '(a b c) '(b c d))`, **When** executed, **Then** returns a list containing b and c.

---

### User Story 6 - Sequence Search and Modification (Priority: P2)

As compiler source code using sequence functions for searching and transforming, position, find, remove, and substitute need to support :key and :test options.

**Why this priority**: Sequence operations are common in compiler code for transforming AST nodes and searching through lists. The :key and :test options enable flexible matching.

**Independent Test**: Test position, find, remove, and substitute with various :key and :test options. Verify correct behavior for both lists and vectors.

**Acceptance Scenarios**:

1. **Given** `(position 'b '(a b c))`, **When** executed, **Then** returns 1.
2. **Given** `(find 'b '(a b c))`, **When** executed, **Then** returns b.
3. **Given** `(remove 'b '(a b c b d))`, **When** executed, **Then** returns (a c d).
4. **Given** `(substitute 'x 'b '(a b c b d))`, **When** executed, **Then** returns (a x c x d).
5. **Given** `(find "B" '("a" "b" "c") :test #'string-equal)`, **When** executed, **Then** returns "b" (case-insensitive match).
6. **Given** `(position 2 '((a 1) (b 2) (c 3)) :key #'second)`, **When** executed, **Then** returns 1.

---

### Edge Cases

- What happens when LOOP body is empty or contains only accumulation clauses?
- How does system handle &optional parameters with side-effecting default forms (evaluated only when needed)?
- What happens when gethash is called on a non-existent key (should return NIL and NIL as second value)?
- How does member handle circular lists (should not infinite loop in standard usage)?
- What happens when remove/substitute are called with :count 0 (should return unchanged sequence)?
- How does intersection handle duplicate elements in either input list?

## Requirements *(mandatory)*

### Functional Requirements

**LOOP Macro Extension**:
- **FR-001**: System MUST support `for` clause with `in` iterator for list iteration
- **FR-002**: System MUST support `for` clause with `from/to/by` for numeric iteration
- **FR-003**: System MUST support `for` clause with `as` alias syntax
- **FR-004**: System MUST support `collect` clause for list accumulation
- **FR-005**: System MUST support `append` clause for flattening list accumulation
- **FR-006**: System MUST support `count` clause for conditional counting
- **FR-007**: System MUST support `sum` clause for numeric accumulation
- **FR-008**: System MUST support `maximize` clause for maximum tracking
- **FR-009**: System MUST support `minimize` clause for minimum tracking
- **FR-010**: System MUST support combining multiple clauses in a single LOOP form

**Default Parameter Values**:
- **FR-011**: System MUST compile &optional parameters with default value forms: `(&optional (x default))`
- **FR-012**: System MUST evaluate default forms only when argument is not supplied
- **FR-013**: System MUST compile &key parameters with default value forms: `(&key (name default))`
- **FR-014**: System MUST support supplied-p variables: `(&optional (x default x-p))`

**Hash Table Operations**:
- **FR-015**: System MUST provide `make-hash-table` function with :test option support
- **FR-016**: System MUST provide `gethash` function returning two values (value and present-p)
- **FR-017**: System MUST provide `(setf gethash)` for storing key-value pairs
- **FR-018**: System MUST provide `remhash` function for entry removal
- **FR-019**: System MUST provide `maphash` function for iteration

**List Functions**:
- **FR-020**: System MUST provide `assoc` function with :test and :key options
- **FR-021**: System MUST provide `rassoc` function with :test and :key options
- **FR-022**: System MUST provide `member` function with :test and :key options
- **FR-023**: System MUST provide `adjoin` function with :test and :key options
- **FR-024**: System MUST provide `union` function with :test and :key options
- **FR-025**: System MUST provide `intersection` function with :test and :key options

**Sequence Functions**:
- **FR-026**: System MUST provide `position` function with :test, :key, :start, :end, :from-end options
- **FR-027**: System MUST provide `find` function with :test, :key, :start, :end, :from-end options
- **FR-028**: System MUST provide `remove` function with :test, :key, :count, :start, :end, :from-end options
- **FR-029**: System MUST provide `substitute` function with :test, :key, :count, :start, :end, :from-end options

**Verification**:
- **FR-030**: System MUST produce Stage 0 binary that compiles at least 50% of source forms
- **FR-031**: System MUST successfully compile at least one form from each file in compiler/codegen/
- **FR-032**: System MUST generate machine-readable compilation progress report

### Key Entities

- **LOOP Clause**: A directive within LOOP that controls iteration, accumulation, or termination behavior
- **Default Value Form**: An expression evaluated to provide a parameter's value when no argument is supplied
- **Hash Table**: A mutable mapping from keys to values with O(1) average lookup time
- **Association List**: A list of cons cells where each car is a key and cdr is a value
- **Compilation Form**: A single Lisp expression extracted from source code for compilation
- **Compilation Rate**: Percentage of forms successfully compiled (successful / total * 100)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Stage 0 compilation rate reaches or exceeds 50% (current baseline: ~20%)
- **SC-002**: Every file in compiler/codegen/ directory has at least one successfully compiled form
- **SC-003**: All six LOOP clause types (collect, append, count, sum, maximize, minimize) pass their acceptance tests
- **SC-004**: Functions with &optional and &key default values compile and execute correctly
- **SC-005**: All five hash table operations (make, get, setf, rem, maphash) pass their acceptance tests
- **SC-006**: All six list functions (assoc, rassoc, member, adjoin, union, intersection) pass their acceptance tests
- **SC-007**: All four sequence functions (position, find, remove, substitute) with :key/:test options pass their acceptance tests
- **SC-008**: Bootstrap process completes without uncaught errors and produces valid Wasm binary
- **SC-009**: Compilation progress is measurable and trackable between runs (for fixed-point verification readiness)

## Assumptions

- LOOP clauses `for`, `as`, `collect`, `append`, `count`, `sum`, `maximize`, `minimize` cover the patterns used in Clysm source code
- The :test option for hash tables and list/sequence functions defaults to #'eql (ANSI CL standard)
- Hash table implementation will use WasmGC struct types for key-value storage
- Default parameter forms are evaluated in the scope of the function body
- The 50% compilation rate target is based on compilable forms (excluding in-package, declare, etc.)
- Fixed-point verification readiness means the infrastructure exists to compare Stage 1 and Stage 2 binaries

## Dependencies

- Feature 029: LOOP macro basic infrastructure (existing partial support)
- Feature 007: Sequence functions (existing basic implementation)
- Feature 028: Setf macros (required for `(setf gethash)`)
- Feature 038: Stage 0 capability extension (baseline 20% compilation rate)
