# Feature Specification: ANSI Sequence Generic Functions (Phase 15B)

**Feature Branch**: `001-ansi-sequence-functions`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 15B: シーケンス汎用関数を実装する。目標はsequencesカテゴリANSIテスト準拠率60%+達成。count/count-if/count-if-not、position/position-if/position-if-not、find/find-if/find-if-not、remove-duplicates/delete-duplicates、mismatch/search、substitute/substitute-if/nsubstitute/nsubstitute-if、replace/fillを実装。:test, :key, :start, :end, :from-endキーワード引数を完全サポート。リスト・ベクタ・文字列の全シーケンス型に対応。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Element Counting Functions (Priority: P1)

A Common Lisp developer using the clysm compiler needs to count elements in sequences. They write code using `count`, `count-if`, and `count-if-not` to count occurrences in lists, vectors, and strings.

**Why this priority**: Counting is a fundamental sequence operation used extensively in data processing. These functions form the basis for many higher-level operations.

**Independent Test**: Can be fully tested by calling count functions on various sequence types and verifying the returned count matches expected values.

**Acceptance Scenarios**:

1. **Given** a list `'(1 2 1 3 1 4)`, **When** calling `(count 1 list)`, **Then** returns `3`
2. **Given** a vector `#(a b c b d b)`, **When** calling `(count 'b vector)`, **Then** returns `3`
3. **Given** a string `"hello"`, **When** calling `(count #\l string)`, **Then** returns `2`
4. **Given** a list `'(1 2 3 4 5)`, **When** calling `(count-if #'evenp list)`, **Then** returns `2`
5. **Given** a list `'(1 2 3 4 5)`, **When** calling `(count-if-not #'evenp list)`, **Then** returns `3`
6. **Given** a list `'((a 1) (b 2) (a 3))`, **When** calling `(count 'a list :key #'car)`, **Then** returns `2`
7. **Given** a list `'(1 2 3 4 5)`, **When** calling `(count-if #'evenp list :start 1 :end 4)`, **Then** returns `2` (counting 2, 4)

---

### User Story 2 - Element Search Functions (Priority: P1)

A Common Lisp developer needs to find elements or their positions in sequences. They use `find`, `find-if`, `find-if-not`, `position`, `position-if`, and `position-if-not` to locate elements.

**Why this priority**: Finding and positioning are essential for data lookup and manipulation. These functions enable conditional element access.

**Independent Test**: Can be fully tested by calling find/position functions and verifying correct element/index is returned or NIL when not found.

**Acceptance Scenarios**:

1. **Given** a list `'(a b c d)`, **When** calling `(find 'c list)`, **Then** returns `c`
2. **Given** a list `'(a b c d)`, **When** calling `(find 'z list)`, **Then** returns `NIL`
3. **Given** a list `'(1 2 3 4)`, **When** calling `(position 3 list)`, **Then** returns `2`
4. **Given** a list `'(1 2 3 4 5)`, **When** calling `(find-if #'evenp list)`, **Then** returns `2`
5. **Given** a list `'(1 2 3 4)`, **When** calling `(position-if-not #'oddp list)`, **Then** returns `1`
6. **Given** a string `"hello"`, **When** calling `(position #\l string)`, **Then** returns `2`
7. **Given** a list `'(1 2 3 4 3 2 1)`, **When** calling `(position 3 list :from-end t)`, **Then** returns `4`
8. **Given** a list `'("apple" "banana" "cherry")`, **When** calling `(find "banana" list :test #'string=)`, **Then** returns `"banana"`

---

### User Story 3 - Sequence Comparison Functions (Priority: P2)

A Common Lisp developer needs to compare sequences to find differences or search for subsequences. They use `mismatch` and `search` for these operations.

**Why this priority**: Comparison functions enable pattern matching and difference detection, which are important for text processing and data validation.

**Independent Test**: Can be fully tested by comparing sequences and verifying mismatch positions or search results.

**Acceptance Scenarios**:

1. **Given** sequences `"abcd"` and `"abXd"`, **When** calling `(mismatch seq1 seq2)`, **Then** returns `2`
2. **Given** sequences `"abcd"` and `"abcd"`, **When** calling `(mismatch seq1 seq2)`, **Then** returns `NIL`
3. **Given** sequences `'(a b c)` and `'(a b c d e)`, **When** calling `(search seq1 seq2)`, **Then** returns `0`
4. **Given** sequences `'(b c)` and `'(a b c d)`, **When** calling `(search seq1 seq2)`, **Then** returns `1`
5. **Given** sequences `'(x y)` and `'(a b c)`, **When** calling `(search seq1 seq2)`, **Then** returns `NIL`
6. **Given** sequences `"bc"` and `"abcbc"`, **When** calling `(search seq1 seq2 :from-end t)`, **Then** returns `3`

---

### User Story 4 - Element Substitution Functions (Priority: P2)

A Common Lisp developer needs to replace elements in sequences. They use `substitute`, `substitute-if`, `nsubstitute`, and `nsubstitute-if` for non-destructive and destructive replacements.

**Why this priority**: Substitution enables data transformation, which is core to functional programming patterns.

**Independent Test**: Can be fully tested by substituting elements and verifying the resulting sequence contains correct replacements.

**Acceptance Scenarios**:

1. **Given** a list `'(1 2 1 3 1)`, **When** calling `(substitute 9 1 list)`, **Then** returns `'(9 2 9 3 9)` and original list is unchanged
2. **Given** a string `"hello"`, **When** calling `(substitute #\x #\l string)`, **Then** returns `"hexxo"`
3. **Given** a list `'(1 2 3 4)`, **When** calling `(substitute-if 0 #'oddp list)`, **Then** returns `'(0 2 0 4)`
4. **Given** a vector `#(1 2 1 3)`, **When** calling `(nsubstitute 9 1 vector)`, **Then** the original vector is modified to `#(9 2 9 3)`
5. **Given** a list `'(1 2 1 3 1)`, **When** calling `(substitute 9 1 list :count 2)`, **Then** returns `'(9 2 9 3 1)` (only first 2 replaced)
6. **Given** a list `'(1 2 1 3 1)`, **When** calling `(substitute 9 1 list :from-end t :count 2)`, **Then** returns `'(1 2 9 3 9)` (last 2 replaced)

---

### User Story 5 - Duplicate Removal Functions (Priority: P2)

A Common Lisp developer needs to remove duplicate elements from sequences. They use `remove-duplicates` and `delete-duplicates` for non-destructive and destructive operations.

**Why this priority**: Duplicate removal is essential for data deduplication and set-like operations.

**Independent Test**: Can be fully tested by removing duplicates and verifying the resulting sequence has unique elements.

**Acceptance Scenarios**:

1. **Given** a list `'(a b a c b d)`, **When** calling `(remove-duplicates list)`, **Then** returns `'(a c b d)` (keeping last occurrences by default)
2. **Given** a list `'(a b a c b d)`, **When** calling `(remove-duplicates list :from-end t)`, **Then** returns `'(a b c d)` (keeping first occurrences)
3. **Given** a string `"abracadabra"`, **When** calling `(remove-duplicates string)`, **Then** returns `"cdbra"`
4. **Given** a list `'((a 1) (b 2) (a 3))`, **When** calling `(remove-duplicates list :key #'car)`, **Then** returns `'((b 2) (a 3))`
5. **Given** a list `'("A" "a" "B" "b")`, **When** calling `(remove-duplicates list :test #'string-equal)`, **Then** returns `'("B" "b")` or `'("A" "B")` depending on :from-end

---

### User Story 6 - Sequence Modification Functions (Priority: P3)

A Common Lisp developer needs to replace portions of sequences or fill them with values. They use `replace` and `fill` for these bulk operations.

**Why this priority**: These functions enable efficient bulk modifications but are less frequently used than search and substitution.

**Independent Test**: Can be fully tested by replacing/filling sequences and verifying the modifications.

**Acceptance Scenarios**:

1. **Given** a list `'(a b c d e)`, **When** calling `(fill list 'x)`, **Then** the list is modified to `'(x x x x x)`
2. **Given** a vector `#(1 2 3 4 5)`, **When** calling `(fill vector 0 :start 1 :end 4)`, **Then** the vector is modified to `#(1 0 0 0 5)`
3. **Given** sequences `'(a b c d)` and `'(1 2 3)`, **When** calling `(replace seq1 seq2)`, **Then** seq1 is modified to `'(1 2 3 d)`
4. **Given** sequences `'(a b c d e)` and `'(x y z)`, **When** calling `(replace seq1 seq2 :start1 1 :end1 3)`, **Then** seq1 is modified to `'(a x y d e)`

---

### Edge Cases

- What happens when start index equals end index? Functions should return unchanged sequence or 0 count.
- What happens when start index exceeds sequence length? Should signal a type-error or bounding-indices-bad-error.
- What happens when :count is 0? No elements should be substituted.
- What happens with empty sequences? Functions should handle gracefully (return NIL, 0, or empty sequence as appropriate).
- What happens when :test and :test-not are both supplied? Should signal an error per ANSI spec.
- What happens when :key returns NIL for some elements? NIL should be used for comparison.

## Requirements *(mandatory)*

### Functional Requirements

**Counting Functions**:
- **FR-001**: System MUST implement `count` that counts occurrences of an item in a sequence
- **FR-002**: System MUST implement `count-if` that counts elements satisfying a predicate
- **FR-003**: System MUST implement `count-if-not` that counts elements not satisfying a predicate

**Search Functions**:
- **FR-004**: System MUST implement `find` that returns the first element matching an item
- **FR-005**: System MUST implement `find-if` that returns the first element satisfying a predicate
- **FR-006**: System MUST implement `find-if-not` that returns the first element not satisfying a predicate
- **FR-007**: System MUST implement `position` that returns the index of the first matching element
- **FR-008**: System MUST implement `position-if` that returns the index of the first element satisfying a predicate
- **FR-009**: System MUST implement `position-if-not` that returns the index of the first element not satisfying a predicate

**Comparison Functions**:
- **FR-010**: System MUST implement `mismatch` that returns the index of the first position where two sequences differ
- **FR-011**: System MUST implement `search` that finds a subsequence within a sequence

**Substitution Functions**:
- **FR-012**: System MUST implement `substitute` that returns a copy with matching elements replaced
- **FR-013**: System MUST implement `substitute-if` that returns a copy with predicate-matching elements replaced
- **FR-014**: System MUST implement `substitute-if-not` that returns a copy with non-matching elements replaced
- **FR-015**: System MUST implement `nsubstitute` that destructively replaces matching elements
- **FR-016**: System MUST implement `nsubstitute-if` that destructively replaces predicate-matching elements
- **FR-017**: System MUST implement `nsubstitute-if-not` that destructively replaces non-matching elements

**Duplicate Functions**:
- **FR-018**: System MUST implement `remove-duplicates` that returns a copy with duplicate elements removed
- **FR-019**: System MUST implement `delete-duplicates` that destructively removes duplicate elements

**Modification Functions**:
- **FR-020**: System MUST implement `replace` that copies elements from one sequence to another
- **FR-021**: System MUST implement `fill` that destructively fills a sequence with a value

**Keyword Argument Support**:
- **FR-022**: All applicable functions MUST support `:test` keyword for custom equality test (default `#'eql`)
- **FR-023**: All applicable functions MUST support `:key` keyword for element transformation before comparison
- **FR-024**: All applicable functions MUST support `:start` and `:end` keywords for bounding the operation
- **FR-025**: All applicable functions MUST support `:from-end` keyword for reverse direction processing
- **FR-026**: Substitution functions MUST support `:count` keyword to limit number of substitutions
- **FR-027**: Comparison functions MUST support `:start1`, `:end1`, `:start2`, `:end2` for both sequences

**Sequence Type Support**:
- **FR-028**: All functions MUST work correctly with lists (proper lists)
- **FR-029**: All functions MUST work correctly with vectors (including simple-vectors)
- **FR-030**: All functions MUST work correctly with strings (as character vectors)

**Error Handling**:
- **FR-031**: Functions MUST signal appropriate errors for invalid bounding indices
- **FR-032**: Functions MUST signal an error when both `:test` and `:test-not` are supplied

### Key Entities

- **Sequence**: Abstract type encompassing lists, vectors, and strings as ordered collections of elements
- **Bounding Index Designator**: Integer specifying start or end position within a sequence (0-indexed)
- **Predicate**: Function returning generalized boolean used by `-if` and `-if-not` variants
- **Test Function**: Two-argument function for element comparison (`:test` keyword)
- **Key Function**: One-argument function for element transformation before comparison

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: ANSI CL sequences category test compliance rate reaches 60% or higher
- **SC-002**: All 21 sequence functions pass their respective unit tests for lists, vectors, and strings
- **SC-003**: All 6 keyword arguments (:test, :key, :start, :end, :from-end, :count) work correctly across applicable functions
- **SC-004**: No regressions in existing sequence functionality (Phase 15A functions remain working)
- **SC-005**: Performance is acceptable for sequences up to 10,000 elements (operations complete without noticeable delay)

## Assumptions

- The clysm compiler already has working implementations of basic sequence operations from Phase 15A (length, subseq, reverse, etc.)
- The `:test-not` keyword is deprecated but may still be supported for ANSI compliance
- Default test function is `#'eql` per ANSI specification
- Default key function is `#'identity` (no transformation)
- When `:from-end` is true, processing order changes but returned positions are still 0-indexed from the start
- Destructive variants (nsubstitute, delete-duplicates, fill, replace) are permitted to modify the original sequence
- Non-destructive variants (substitute, remove-duplicates) must return a fresh sequence, leaving the original unchanged
