# Feature Specification: Phase 15C - ANSI Array Operations Enhancement

**Feature Branch**: `001-ansi-array-ops`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 15C: 配列操作強化 (ANSI-Arrays) を実装する。目標はANSI Common Lisp配列関数の完全サポートとarraysカテゴリテスト50%+達成。実装対象: array-rank, array-dimension, array-dimensions, array-total-size, array-row-major-index, row-major-aref, (setf row-major-aref), adjustable-array-p, adjust-array。WasmGCのarray型を活用し、多次元配列は1次元配列+次元情報で表現する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Query Array Dimensions and Structure (Priority: P1)

Compiler developers need to query array metadata (rank, dimensions, total size) to implement array operations correctly and generate efficient Wasm code.

**Why this priority**: Array metadata queries are fundamental operations that all other array functions depend on. Without knowing dimensions, subscript validation and element access cannot work correctly.

**Independent Test**: Can be fully tested by creating arrays of various dimensions and verifying metadata queries return correct values.

**Acceptance Scenarios**:

1. **Given** a 2x3x4 multidimensional array, **When** calling array-rank, **Then** the system returns 3 (number of dimensions)
2. **Given** a 2x3x4 multidimensional array, **When** calling array-dimension with axis 1, **Then** the system returns 3 (size of second dimension)
3. **Given** a 2x3x4 multidimensional array, **When** calling array-dimensions, **Then** the system returns the list (2 3 4)
4. **Given** a 2x3x4 multidimensional array, **When** calling array-total-size, **Then** the system returns 24 (2*3*4)
5. **Given** a simple vector of 10 elements, **When** calling array-rank, **Then** the system returns 1

---

### User Story 2 - Access Elements by Row-Major Index (Priority: P2)

Compiler developers need to access array elements using a linear row-major index, enabling efficient iteration and bulk operations without subscript decomposition overhead.

**Why this priority**: Row-major access is essential for implementing efficient array traversal in compiled code, and is the natural mapping to underlying 1D storage.

**Independent Test**: Can be fully tested by creating arrays, computing row-major indices from subscripts, and verifying element access/modification works correctly.

**Acceptance Scenarios**:

1. **Given** a 3x4 array with element "X" at position (1,2), **When** calling array-row-major-index with subscripts (1 2), **Then** the system returns 6 (1*4 + 2)
2. **Given** a 3x4 array with element "X" at position (1,2), **When** calling row-major-aref with index 6, **Then** the system returns "X"
3. **Given** a 3x4 array, **When** setting element at row-major index 6 via (setf row-major-aref), **Then** the element at position (1,2) is updated
4. **Given** a 2x3x4 array, **When** calling array-row-major-index with subscripts (1 2 3), **Then** the system returns 23 (1*12 + 2*4 + 3)

---

### User Story 3 - Check and Modify Array Adjustability (Priority: P3)

Compiler developers need to determine if arrays can be resized and to adjust array dimensions dynamically, supporting Common Lisp's adjustable array semantics.

**Why this priority**: Adjustable arrays are less commonly used than fixed arrays but are required for ANSI compliance. Some algorithms depend on dynamic array resizing.

**Independent Test**: Can be fully tested by creating adjustable and non-adjustable arrays, checking their properties, and attempting resize operations.

**Acceptance Scenarios**:

1. **Given** an array created with :adjustable t, **When** calling adjustable-array-p, **Then** the system returns T
2. **Given** an array created without :adjustable, **When** calling adjustable-array-p, **Then** the system returns NIL
3. **Given** an adjustable 2x3 array, **When** calling adjust-array with new dimensions (4 5), **Then** the array is resized to 4x5 with existing elements preserved
4. **Given** an adjustable array of 10 elements, **When** calling adjust-array with dimension 5, **Then** the array is shrunk to 5 elements
5. **Given** an adjustable array, **When** calling adjust-array with :initial-element, **Then** new positions are filled with the specified value

---

### Edge Cases

- What happens when subscript indices are out of bounds? System signals a type-error condition.
- What happens when row-major-index exceeds array-total-size? System signals a type-error condition.
- How does adjust-array handle non-adjustable arrays? System signals an error.
- What happens with zero-dimensional arrays (scalars)? array-rank returns 0, array-dimensions returns NIL, array-total-size returns 1.
- How does system handle negative subscripts? System signals a type-error condition.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement array-rank returning the number of dimensions of any array
- **FR-002**: System MUST implement array-dimension returning the size of a specific dimension (0-indexed axis)
- **FR-003**: System MUST implement array-dimensions returning a list of all dimension sizes
- **FR-004**: System MUST implement array-total-size returning the product of all dimensions
- **FR-005**: System MUST implement array-row-major-index computing linear index from subscripts using row-major order
- **FR-006**: System MUST implement row-major-aref accessing elements by row-major index
- **FR-007**: System MUST implement (setf row-major-aref) for setting elements by row-major index
- **FR-008**: System MUST implement adjustable-array-p returning T for adjustable arrays, NIL otherwise
- **FR-009**: System MUST implement adjust-array for resizing adjustable arrays
- **FR-010**: System MUST signal appropriate errors for out-of-bounds access
- **FR-011**: System MUST support arrays of any valid element type (T, fixnum, character, etc.)
- **FR-012**: System MUST represent multidimensional arrays internally as 1D storage with dimension metadata
- **FR-013**: System MUST preserve existing elements during adjust-array when dimensions increase
- **FR-014**: System MUST support :initial-element parameter in adjust-array for new positions

### Key Entities

- **Array**: A multidimensional collection with fixed or adjustable dimensions, containing elements accessible by subscripts or row-major index
- **Dimension Metadata**: Information stored with each array tracking rank, dimension sizes, total size, and adjustability flag
- **Row-Major Index**: A linear index mapping multidimensional subscripts to 1D storage position

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 9 specified array functions compile to valid Wasm bytecode without errors
- **SC-002**: Arrays category tests achieve 50%+ pass rate (improvement from current baseline)
- **SC-003**: All array metadata queries (rank, dimension, dimensions, total-size) return correct values for arrays up to 8 dimensions
- **SC-004**: Row-major index computation and access work correctly for arrays of any dimensionality
- **SC-005**: adjust-array preserves existing element values when resizing adjustable arrays
- **SC-006**: Error conditions signal appropriate type-error for invalid access

## Assumptions

- Multidimensional arrays are represented as 1D arrays with separate dimension metadata structures
- The existing array infrastructure (make-array, aref, simple vector operations) from 001-ansi-array-primitives is available
- Array dimension information is stored in a separate structure associated with each array
- Adjustable arrays incur a small overhead for tracking adjustability status
