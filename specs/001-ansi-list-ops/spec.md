# Feature Specification: Phase 15A - ANSI List Operations Extension

**Feature Branch**: `001-ansi-list-ops`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 15A: リスト操作拡張 (ANSI-Sequences) を実装する。目標はANSI Common Lisp準拠のリスト操作関数の完全実装。butlast, nbutlast, last, nth, nthcdrのリスト末尾操作、intersection, union, set-difference, subsetp, adjoin, pushnewの集合演算、member, member-if, member-if-notのメンバーシップ検索（:test, :keyキーワード対応）、assoc, assoc-if, rassoc, rassoc-ifの連想リスト操作（:test, :keyキーワード対応）、pairlis, acons, copy-alistの連想リスト構築を実装。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - List Tail Operations (Priority: P1)

Lisp programmers need to access and manipulate list elements by position and retrieve list suffixes. These are fundamental operations used throughout nearly all Lisp code.

**Why this priority**: These are the most basic list accessor functions. Many other list operations depend on `nth`, `nthcdr`, and `last`. Without these, users cannot implement higher-level list algorithms.

**Independent Test**: Can be fully tested by calling each function with sample lists and verifying correct element/sublist extraction. Delivers immediate value for basic list manipulation.

**Acceptance Scenarios**:

1. **Given** a list `(a b c d e)`, **When** calling `(last list)`, **Then** returns `(e)` (last cons cell)
2. **Given** a list `(a b c d e)`, **When** calling `(last list 2)`, **Then** returns `(d e)` (last 2 cons cells)
3. **Given** a list `(1 2 3 4 5)`, **When** calling `(butlast list)`, **Then** returns `(1 2 3 4)` (all but last element)
4. **Given** a list `(1 2 3 4 5)`, **When** calling `(butlast list 2)`, **Then** returns `(1 2 3)` (all but last 2 elements)
5. **Given** a list `(1 2 3 4 5)`, **When** calling `(nbutlast list)`, **Then** destructively modifies and returns `(1 2 3 4)`
6. **Given** a list `(a b c d)`, **When** calling `(nth 2 list)`, **Then** returns `c` (zero-indexed third element)
7. **Given** a list `(a b c d)`, **When** calling `(nthcdr 2 list)`, **Then** returns `(c d)` (cdr applied n times)

---

### User Story 2 - Membership and Search Operations (Priority: P1)

Lisp programmers need to search for elements in lists and retrieve the tail starting from a match. This is essential for conditional logic and list filtering.

**Why this priority**: `member` and its variants are used extensively for list searching and conditional checks. They enable pattern matching and element existence tests.

**Independent Test**: Can be tested by searching for elements in lists with various :test and :key functions. Delivers value for list-based lookups and filtering.

**Acceptance Scenarios**:

1. **Given** a list `(1 2 3 4 5)`, **When** calling `(member 3 list)`, **Then** returns `(3 4 5)`
2. **Given** a list `(1 2 3)`, **When** calling `(member 5 list)`, **Then** returns `NIL`
3. **Given** a list `((a 1) (b 2) (c 3))`, **When** calling `(member 'b list :key #'car)`, **Then** returns `((b 2) (c 3))`
4. **Given** a list `("A" "B" "C")`, **When** calling `(member "b" list :test #'equalp)`, **Then** returns `("B" "C")`
5. **Given** a list `(1 2 3 4 5)`, **When** calling `(member-if #'evenp list)`, **Then** returns `(2 3 4 5)`
6. **Given** a list `(1 2 3 4 5)`, **When** calling `(member-if-not #'evenp list)`, **Then** returns `(1 2 3 4 5)` (first odd)

---

### User Story 3 - Association List Operations (Priority: P1)

Lisp programmers need to create and query association lists (alists), which are a fundamental key-value data structure in Lisp for simple mappings.

**Why this priority**: Alists are one of the primary data structures in Lisp for associative data. ASSOC and related functions are used pervasively for configuration, symbol tables, and metadata.

**Independent Test**: Can be tested by creating alists and performing lookups with various :test and :key options. Delivers value for key-value storage and retrieval.

**Acceptance Scenarios**:

1. **Given** an alist `((a . 1) (b . 2) (c . 3))`, **When** calling `(assoc 'b alist)`, **Then** returns `(b . 2)`
2. **Given** an alist `((a . 1) (b . 2))`, **When** calling `(assoc 'd alist)`, **Then** returns `NIL`
3. **Given** an alist `(("a" . 1) ("b" . 2))`, **When** calling `(assoc "B" alist :test #'equalp)`, **Then** returns `("b" . 2)`
4. **Given** an alist `((a . 1) (b . 2) (c . 3))`, **When** calling `(assoc-if #'(lambda (k) (eq k 'b)) alist)`, **Then** returns `(b . 2)`
5. **Given** an alist `((1 . a) (2 . b) (3 . c))`, **When** calling `(rassoc 'b alist)`, **Then** returns `(2 . b)`
6. **Given** an alist `((1 . a) (2 . b))`, **When** calling `(rassoc-if #'(lambda (v) (eq v 'a)) alist)`, **Then** returns `(1 . a)`

---

### User Story 4 - Association List Construction (Priority: P2)

Lisp programmers need to construct and copy association lists efficiently using standard utility functions.

**Why this priority**: While alists can be constructed manually with `cons`, having standard construction functions improves code clarity and reduces errors. These are builder utilities rather than core accessors.

**Independent Test**: Can be tested by constructing alists from keys and values lists, and verifying the resulting structure. Delivers value for alist initialization.

**Acceptance Scenarios**:

1. **Given** keys `(a b c)` and values `(1 2 3)`, **When** calling `(pairlis keys values)`, **Then** returns `((a . 1) (b . 2) (c . 3))` or equivalent
2. **Given** keys `(a b)` and values `(1 2)` with existing alist `((c . 3))`, **When** calling `(pairlis keys values alist)`, **Then** returns `((a . 1) (b . 2) (c . 3))` or equivalent
3. **Given** key `'a` and value `1` and alist `((b . 2))`, **When** calling `(acons 'a 1 alist)`, **Then** returns `((a . 1) (b . 2))`
4. **Given** an alist `((a . 1) (b . 2))`, **When** calling `(copy-alist alist)`, **Then** returns a fresh copy with new cons cells (not eq to original pairs)

---

### User Story 5 - Set Operations (Priority: P2)

Lisp programmers need to perform set operations (intersection, union, difference) on lists treated as sets, supporting custom equality tests.

**Why this priority**: Set operations are commonly used but less frequent than basic list access and alist operations. They depend on membership functions being implemented first.

**Independent Test**: Can be tested by performing set operations on sample lists and verifying results contain correct elements. Order may vary.

**Acceptance Scenarios**:

1. **Given** lists `(1 2 3)` and `(2 3 4)`, **When** calling `(intersection list1 list2)`, **Then** returns a list containing exactly `2` and `3` (order unspecified)
2. **Given** lists `(1 2 3)` and `(2 3 4)`, **When** calling `(union list1 list2)`, **Then** returns a list containing `1 2 3 4` (order unspecified, no duplicates)
3. **Given** lists `(1 2 3)` and `(2 3 4)`, **When** calling `(set-difference list1 list2)`, **Then** returns a list containing only `1`
4. **Given** lists `(1 2)` and `(1 2 3)`, **When** calling `(subsetp list1 list2)`, **Then** returns `T`
5. **Given** lists `(1 2 3)` and `(1 2)`, **When** calling `(subsetp list1 list2)`, **Then** returns `NIL`
6. **Given** list `(1 2 3)` and item `4`, **When** calling `(adjoin 4 list)`, **Then** returns `(4 1 2 3)` or equivalent containing 4
7. **Given** list `(1 2 3)` and item `2`, **When** calling `(adjoin 2 list)`, **Then** returns the original list (no duplicate)

---

### User Story 6 - PUSHNEW Macro (Priority: P3)

Lisp programmers need the PUSHNEW macro to conditionally add elements to a list only if not already present.

**Why this priority**: PUSHNEW is a convenience macro that combines ADJOIN with SETF. While useful, it's less fundamental than the core functions and can be implemented after ADJOIN works.

**Independent Test**: Can be tested by using PUSHNEW on a place and verifying the place is modified only when the item is not already present.

**Acceptance Scenarios**:

1. **Given** a variable `lst` bound to `(1 2 3)`, **When** evaluating `(pushnew 4 lst)`, **Then** `lst` becomes `(4 1 2 3)` and the form returns the new list
2. **Given** a variable `lst` bound to `(1 2 3)`, **When** evaluating `(pushnew 2 lst)`, **Then** `lst` remains `(1 2 3)` (no change)
3. **Given** a variable `lst` bound to `((a 1) (b 2))`, **When** evaluating `(pushnew '(c 3) lst :key #'car)`, **Then** `lst` becomes `((c 3) (a 1) (b 2))`

---

### Edge Cases

- What happens when passing an empty list to list operations? (Should return NIL or empty results as appropriate)
- What happens when `n` exceeds list length in `nth`, `nthcdr`, `butlast`? (Returns NIL)
- What happens when `n` is negative in position-based functions? (Behavior is undefined in ANSI CL, implementation should signal error or treat as 0)
- How do circular lists behave? (Functions may loop infinitely; this is acceptable per ANSI CL)
- What happens when :test and :key are both specified? (:key is applied first to extract comparison value, then :test compares)
- What happens when alist contains non-cons elements? (ASSOC skips non-cons elements per ANSI CL)

## Requirements *(mandatory)*

### Functional Requirements

#### List Tail Operations

- **FR-001**: System MUST implement `last` returning the last n cons cells of a list (default n=1)
- **FR-002**: System MUST implement `butlast` returning a fresh list of all but the last n elements (default n=1)
- **FR-003**: System MUST implement `nbutlast` destructively modifying a list to remove the last n elements
- **FR-004**: System MUST implement `nth` returning the nth element (zero-indexed) of a list
- **FR-005**: System MUST implement `nthcdr` returning the result of applying cdr n times to a list

#### Membership Operations

- **FR-006**: System MUST implement `member` searching for an item in a list, supporting :test and :key keyword arguments
- **FR-007**: System MUST implement `member-if` searching for an element satisfying a predicate, supporting :key
- **FR-008**: System MUST implement `member-if-not` searching for an element not satisfying a predicate, supporting :key

#### Association List Operations

- **FR-009**: System MUST implement `assoc` searching an alist by key, supporting :test and :key
- **FR-010**: System MUST implement `assoc-if` searching an alist where key satisfies predicate
- **FR-011**: System MUST implement `rassoc` searching an alist by value (reverse association), supporting :test and :key
- **FR-012**: System MUST implement `rassoc-if` searching an alist where value satisfies predicate
- **FR-013**: System MUST implement `pairlis` constructing an alist from parallel key and value lists
- **FR-014**: System MUST implement `acons` prepending a new key-value pair to an alist
- **FR-015**: System MUST implement `copy-alist` creating a fresh copy of an alist (top-level cons cells copied)

#### Set Operations

- **FR-016**: System MUST implement `intersection` returning elements common to two lists
- **FR-017**: System MUST implement `union` returning elements in either list without duplicates
- **FR-018**: System MUST implement `set-difference` returning elements in first list but not second
- **FR-019**: System MUST implement `subsetp` testing if first list is a subset of second
- **FR-020**: System MUST implement `adjoin` adding an item to a list if not already present

#### Macro

- **FR-021**: System MUST implement `pushnew` macro that conditionally pushes onto a place

#### Keyword Arguments

- **FR-022**: All search functions MUST support :test keyword (default #'eql) for equality comparison
- **FR-023**: All search functions MUST support :key keyword (default #'identity) for element extraction
- **FR-024**: Set operations MUST support :test and :key keywords

### Key Entities

- **List**: A chain of cons cells, where each cell has a CAR (element) and CDR (rest of list)
- **Association List (Alist)**: A list of cons cells where CAR is key and CDR is value
- **Set (as list)**: A list treated as an unordered collection with no duplicate elements
- **Predicate Function**: A function returning boolean (generalized boolean in CL) for conditional operations
- **Test Function**: A two-argument function used for equality comparison (e.g., EQ, EQL, EQUAL, EQUALP)
- **Key Function**: A one-argument function extracting the comparison value from an element

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 21 ANSI CL list functions specified are implemented and operational
- **SC-002**: Each function correctly handles empty list inputs returning appropriate NIL/empty results
- **SC-003**: Each function with :test/:key parameters works correctly with custom functions
- **SC-004**: Verification examples pass:
  - `(intersection '(1 2 3) '(2 3 4))` returns list containing exactly 2 and 3
  - `(member 2 '(1 2 3))` returns `(2 3)`
  - `(assoc 'b '((a . 1) (b . 2)))` returns `(B . 2)`
- **SC-005**: Cons-related test category achieves 50% or greater pass rate
- **SC-006**: All functions integrate with existing compilation pipeline without errors
- **SC-007**: Functions are usable in the interpreter and produce valid output

## Assumptions

- The :test-not keyword argument (deprecated in ANSI CL) is NOT implemented
- Default test function is EQL for all membership/set operations per ANSI CL
- Default key function is IDENTITY for all operations per ANSI CL
- Circular list handling follows ANSI CL behavior (may loop infinitely)
- PUSHNEW uses existing SETF infrastructure from feature 028
- N-versions of set operations (NINTERSECTION, NUNION, NSET-DIFFERENCE) are out of scope for this phase
