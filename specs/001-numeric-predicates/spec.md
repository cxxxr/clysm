# Feature Specification: Phase 14B - Numeric Type Predicates Enhancement

**Feature Branch**: `001-numeric-predicates`
**Created**: 2025-12-30
**Status**: Draft
**Input**: Phase 14B: 数値型述語強化を実装する。目標はANSI CL準拠の数値述語とバイト操作関数の追加。

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Sign Testing Functions (Priority: P1)

Compiler users need to test numeric values for sign (positive, negative, zero) using standard Common Lisp predicates. This enables conditional logic based on numeric value characteristics without manual comparison operations.

**Why this priority**: Sign predicates are the most fundamental numeric tests, used extensively in mathematical and control flow code. They form the foundation for numeric type checking.

**Independent Test**: Can be fully tested by compiling Lisp code using `plusp`, `minusp`, and `zerop` predicates and verifying correct boolean results for various numeric inputs.

**Acceptance Scenarios**:

1. **Given** a positive integer value, **When** `plusp` is called, **Then** it returns true (non-nil)
2. **Given** zero, **When** `plusp` is called, **Then** it returns false (nil)
3. **Given** a negative floating-point value, **When** `minusp` is called, **Then** it returns true
4. **Given** integer zero, **When** `zerop` is called, **Then** it returns true
5. **Given** floating-point 0.0, **When** `zerop` is called, **Then** it returns true
6. **Given** a non-zero value, **When** `zerop` is called, **Then** it returns false

---

### User Story 2 - Parity Testing Functions (Priority: P1)

Compiler users need to determine whether integer values are odd or even using standard Common Lisp predicates. This supports algorithms that depend on integer parity for iteration, indexing, or mathematical operations.

**Why this priority**: Parity predicates are equally fundamental as sign predicates and are heavily used in loop constructs, array indexing, and mathematical algorithms.

**Independent Test**: Can be fully tested by compiling Lisp code using `oddp` and `evenp` predicates and verifying correct boolean results for various integer inputs.

**Acceptance Scenarios**:

1. **Given** an odd integer (e.g., 7), **When** `oddp` is called, **Then** it returns true
2. **Given** an even integer (e.g., 8), **When** `evenp` is called, **Then** it returns true
3. **Given** zero, **When** `evenp` is called, **Then** it returns true (zero is even)
4. **Given** negative odd integer (-5), **When** `oddp` is called, **Then** it returns true
5. **Given** negative even integer (-4), **When** `evenp` is called, **Then** it returns true
6. **Given** an odd integer, **When** `evenp` is called, **Then** it returns false

---

### User Story 3 - Bit Testing Functions (Priority: P2)

Compiler users need to test individual bits within integers and check if any bits are common between two integers. This enables low-level bit manipulation algorithms, flag checking, and mask operations.

**Why this priority**: Bit testing is essential for systems programming, flag management, and binary protocol handling. (Note: US1/US2 sign/parity predicates are already implemented in func-section.lisp:4414-4603)

**Independent Test**: Can be fully tested by compiling Lisp code using `logbitp` and `logtest` and verifying correct results for various bit positions and integer pairs.

**Acceptance Scenarios**:

1. **Given** index 0 and integer 5 (binary 101), **When** `logbitp` is called, **Then** it returns true (bit 0 is set)
2. **Given** index 1 and integer 5 (binary 101), **When** `logbitp` is called, **Then** it returns false (bit 1 is not set)
3. **Given** integers 5 (101) and 3 (011), **When** `logtest` is called, **Then** it returns true (bit 0 is common)
4. **Given** integers 4 (100) and 3 (011), **When** `logtest` is called, **Then** it returns false (no common bits)
5. **Given** negative index to `logbitp`, **When** called, **Then** an error condition is signaled

---

### User Story 4 - Byte Specifier Functions (Priority: P2)

Compiler users need to create and inspect byte specifiers that define contiguous bit fields within integers. Byte specifiers enable portable bit-field manipulation without implementation-specific knowledge.

**Why this priority**: Byte specifiers are prerequisite for byte operations. They provide the abstraction layer that makes byte operations portable and clean.

**Independent Test**: Can be fully tested by creating byte specifiers with `byte` and extracting size/position with `byte-size` and `byte-position`.

**Acceptance Scenarios**:

1. **Given** size 8 and position 0, **When** `byte` is called, **Then** it returns a byte specifier representing the low 8 bits
2. **Given** a byte specifier created with size 8, position 4, **When** `byte-size` is called, **Then** it returns 8
3. **Given** a byte specifier created with size 8, position 4, **When** `byte-position` is called, **Then** it returns 4
4. **Given** size 0 and any position, **When** `byte` is called, **Then** it returns a valid (empty) byte specifier

---

### User Story 5 - Byte Extraction and Manipulation (Priority: P3)

Compiler users need to extract, deposit, and mask bit fields within integers using byte specifiers. This enables clean binary data manipulation for protocol parsing, hardware interfaces, and packed data structures.

**Why this priority**: Byte operations are the highest-level bit manipulation functions. They depend on all previous stories and provide the most sophisticated bit manipulation capabilities.

**Independent Test**: Can be fully tested by using `ldb`, `dpb`, `mask-field`, and `deposit-field` to manipulate bit fields in integers and verifying correct results.

**Acceptance Scenarios**:

1. **Given** byte specifier (size 4, position 4) and integer 0xAB (10101011), **When** `ldb` is called, **Then** it returns 0xA (the high nibble shifted to position 0)
2. **Given** byte specifier (size 4, position 4), value 0xC, and integer 0xAB, **When** `dpb` is called, **Then** it returns 0xCB (replacing high nibble)
3. **Given** byte specifier (size 4, position 4) and integer 0xAB, **When** `mask-field` is called, **Then** it returns 0xA0 (bits kept in original position)
4. **Given** byte specifier (size 4, position 4), value 0xC0 (already positioned), and integer 0xAB, **When** `deposit-field` is called, **Then** it returns 0xCB
5. **Given** byte specifier with size 0, **When** `ldb` is called on any integer, **Then** it returns 0

---

### Edge Cases

- Zero is correctly identified as even (not odd)
- Floating-point positive/negative zero handling for `zerop` (both should return true)
- Very large integers for bit operations (beyond fixnum range)
- Bit index larger than integer bit width returns false for `logbitp`
- Empty byte specifier (size 0) operations behave correctly
- Byte specifier position + size exceeding typical integer width

## Requirements *(mandatory)*

### Functional Requirements

**Already Implemented** (func-section.lisp:4414-4603):

- **FR-001**: System provides [`plusp`](resources/HyperSpec/Body/f_minusp.htm) predicate that returns true for positive real numbers *(implemented)*
- **FR-002**: System provides [`minusp`](resources/HyperSpec/Body/f_minusp.htm) predicate that returns true for negative real numbers *(implemented)*
- **FR-003**: System provides [`zerop`](resources/HyperSpec/Body/f_zerop.htm) predicate that returns true for numeric zero (integer or float) *(implemented)*
- **FR-004**: System provides [`oddp`](resources/HyperSpec/Body/f_evenpc.htm) predicate that returns true for odd integers *(implemented)*
- **FR-005**: System provides [`evenp`](resources/HyperSpec/Body/f_evenpc.htm) predicate that returns true for even integers (including zero) *(implemented)*

**To Be Implemented** (10 new functions):

- **FR-006**: System MUST implement [`logbitp`](resources/HyperSpec/Body/f_logbtp.htm) that tests if a specific bit index is set in an integer
- **FR-007**: System MUST implement [`logtest`](resources/HyperSpec/Body/f_logtes.htm) that returns true if two integers share any set bits
- **FR-008**: System MUST implement [`byte`](resources/HyperSpec/Body/f_by_by.htm) constructor that creates a byte specifier from size and position
- **FR-009**: System MUST implement [`byte-size`](resources/HyperSpec/Body/f_by_by.htm) accessor that extracts the size from a byte specifier
- **FR-010**: System MUST implement [`byte-position`](resources/HyperSpec/Body/f_by_by.htm) accessor that extracts the position from a byte specifier
- **FR-011**: System MUST implement [`ldb`](resources/HyperSpec/Body/f_ldb.htm) (load byte) that extracts a byte field as a right-shifted value
- **FR-012**: System MUST implement [`dpb`](resources/HyperSpec/Body/f_dpb.htm) (deposit byte) that replaces a byte field in an integer
- **FR-013**: System MUST implement [`mask-field`](resources/HyperSpec/Body/f_mask_f.htm) that extracts a byte field keeping original bit positions
- **FR-014**: System MUST implement [`deposit-field`](resources/HyperSpec/Body/f_deposi.htm) that deposits a pre-positioned byte field into an integer

**Cross-Cutting Requirements**:

- **FR-015**: All predicates MUST return standard Common Lisp boolean values (T or NIL)
- **FR-016**: All functions MUST conform to ANSI Common Lisp semantics and type signatures

### Key Entities

- **Byte Specifier**: An opaque value that encodes the size (number of bits) and position (starting bit index) of a contiguous bit field within an integer. Used as the first argument to byte manipulation functions.
- **Real Number**: Numeric values that can be tested with sign predicates (integers, floats, ratios)
- **Integer**: Whole number values that can be tested with parity predicates and bit operations

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 10 new functions (plus 5 existing) compile successfully to valid Wasm
- **SC-002**: All functions produce ANSI-compliant results for standard test cases
- **SC-003**: ANSI CL test suite "numbers" category coverage increases after implementation
- **SC-004**: Sign predicates execute correctly for integer, float, and ratio inputs
- **SC-005**: Byte operations correctly manipulate fields up to the full integer width
- **SC-006**: All functions integrate seamlessly with existing bit operations (ash, logand, logior, logxor)

## Assumptions

- Integer precision: i31ref (31-bit signed fixnum) for primary operations; bignum support deferred
- Byte specifiers encoded as fixnum using `(size << 6) | position` format
- Existing bit operations from 002-numeric-functions are available and working
- Type dispatch for sign predicates follows existing numeric type handling patterns
- Error handling follows existing clysm condition system patterns
