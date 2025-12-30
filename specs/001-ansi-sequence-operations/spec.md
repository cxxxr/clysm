# Feature Specification: ANSI CL Sequence Operations

**Feature Branch**: `001-ansi-sequence-operations`
**Created**: 2025-12-29
**Status**: Draft
**Input**: Phase 13D-2: ANSI CL シーケンス操作関数を実装する。目標はreader/compilerの文字列処理に必要なシーケンス操作のプリミティブ実装。

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Subsequence Extraction (Priority: P1)

As a compiler developer, I want to compile code containing `subseq` expressions so that string slicing and byte array extraction operations work correctly, enabling compilation of tokenizer.lisp, utf8.lisp, and leb128.lisp which heavily rely on subsequence extraction.

**Why this priority**: `subseq` is the most frequently used sequence operation in the compiler codebase (found in 10+ critical files including tokenizer, interpreter, and UTF-8 processing). This is the highest-impact primitive to unblock self-hosting.

**Independent Test**: Can be tested by compiling `(subseq "hello" 1 3)` and verifying it returns `"el"`. Also test byte vector slicing and list subsequence extraction.

**Acceptance Scenarios**:

1. **Given** a string `"hello world"`, **When** `subseq` is called with start 0 and end 5, **Then** the compiled Wasm returns the string `"hello"`
2. **Given** a byte vector `#(1 2 3 4 5)`, **When** `subseq` is called with start 2, **Then** the result is `#(3 4 5)` (rest of sequence)
3. **Given** a list `'(a b c d e)`, **When** `subseq` is called with start 1 and end 3, **Then** the result is `'(b c)`
4. **Given** a `setf` form on `subseq`, **When** compiled, **Then** the target range is replaced with the new value

---

### User Story 2 - Sequence Concatenation (Priority: P2)

As a compiler developer, I want to compile code containing `concatenate` expressions so that byte array combining and string building operations work correctly, enabling compilation of stage2/generator.lisp and compiler.lisp which assemble Wasm byte chunks.

**Why this priority**: `concatenate` is essential for Wasm bytecode assembly where multiple byte vectors are joined together. This is critical for the Stage 1/2 generation pipeline.

**Independent Test**: Can be tested by compiling `(concatenate 'string "foo" "bar")` returning `"foobar"`, and `(concatenate 'vector #(1 2) #(3 4))` returning `#(1 2 3 4)`.

**Acceptance Scenarios**:

1. **Given** two strings `"foo"` and `"bar"`, **When** `concatenate` is called with type `'string`, **Then** the result is `"foobar"`
2. **Given** three byte vectors, **When** `concatenate` is called with type `'(vector (unsigned-byte 8))`, **Then** all bytes are combined in order
3. **Given** multiple lists, **When** `concatenate` is called with type `'list`, **Then** a new list containing all elements in order is returned
4. **Given** an empty sequence among arguments, **When** `concatenate` is called, **Then** it is handled correctly (skipped in output)

---

### User Story 3 - String Creation (Priority: P3)

As a compiler developer, I want to compile code containing `make-string` expressions so that string buffer allocation with initial characters works correctly, enabling formatted output and string building operations.

**Why this priority**: `make-string` is used for creating string buffers (e.g., indentation strings in compiler.lisp). While less frequent than `subseq`/`concatenate`, it is still a blocking function for full compiler compilation.

**Independent Test**: Can be tested by compiling `(make-string 5 :initial-element #\x)` returning `"xxxxx"`.

**Acceptance Scenarios**:

1. **Given** a length 5 and initial-element `#\Space`, **When** `make-string` is called, **Then** a string of 5 spaces is returned
2. **Given** a length 0, **When** `make-string` is called, **Then** an empty string `""` is returned
3. **Given** no initial-element specified, **When** `make-string` is called, **Then** a string of null characters (or implementation-defined default) is returned

---

### User Story 4 - Array Creation Extensions (Priority: P4)

As a compiler developer, I want to compile `make-array` expressions with `:initial-element` and `:initial-contents` keyword arguments so that arrays can be created with pre-filled values, enabling CLOS slot-vector initialization and byte buffer setup.

**Why this priority**: Many places in the compiler use `make-array` with `:initial-element` for slot vectors, buffers, and lookup tables. This extends the existing `make-array` support.

**Independent Test**: Can be tested by compiling `(make-array 3 :initial-element 0)` returning `#(0 0 0)`, and `(make-array 3 :initial-contents '(a b c))` returning `#(a b c)`.

**Acceptance Scenarios**:

1. **Given** size 5 and `:initial-element nil`, **When** `make-array` is called, **Then** a vector `#(nil nil nil nil nil)` is returned
2. **Given** size 3 and `:initial-contents '(1 2 3)`, **When** `make-array` is called, **Then** a vector `#(1 2 3)` is returned
3. **Given** `:initial-contents` with mismatched length, **When** `make-array` is called, **Then** an error is signaled
4. **Given** `:element-type '(unsigned-byte 8)` with `:initial-element 0`, **When** `make-array` is called, **Then** a byte vector is returned

---

### User Story 5 - Sequence Copy (Priority: P5)

As a compiler developer, I want to compile code containing `copy-seq` expressions so that sequences can be safely copied without sharing structure, enabling defensive copying in the interpreter and compiler.

**Why this priority**: `copy-seq` is used less frequently than other sequence operations but is important for maintaining correctness when sequences might be modified.

**Independent Test**: Can be tested by compiling `(copy-seq #(1 2 3))` returning a new vector equal to but not identical to the original.

**Acceptance Scenarios**:

1. **Given** a vector `#(1 2 3)`, **When** `copy-seq` is called, **Then** a new vector with the same elements is returned
2. **Given** a string `"hello"`, **When** `copy-seq` is called, **Then** a new string with the same characters is returned
3. **Given** a list `'(a b c)`, **When** `copy-seq` is called, **Then** a new list (fresh cons cells) with the same elements is returned
4. **Given** an empty sequence, **When** `copy-seq` is called, **Then** an empty sequence of the same type is returned

---

### Edge Cases

- What happens when `subseq` start index exceeds end index? - Signal a bounding-indices-bad-error
- What happens when `subseq` indices are out of bounds? - Signal a bounding-indices-bad-error
- How does `concatenate` handle mixed sequence types? - Each argument is converted to the result type
- What happens when `make-string` receives a negative length? - Signal a type-error for invalid array dimension
- How does `copy-seq` handle circular lists? - Undefined behavior (circular lists are out of scope)
- What happens when `(setf subseq)` replacement has different length? - Only min(replacement-length, range-length) elements are copied

## HyperSpec References

| Function | HyperSpec | Description |
|----------|-----------|-------------|
| [subseq](../../resources/HyperSpec/Body/f_subseq.htm) | f_subseq.htm | Subsequence extraction |
| [concatenate](../../resources/HyperSpec/Body/f_concat.htm) | f_concat.htm | Sequence concatenation |
| [make-string](../../resources/HyperSpec/Body/f_mk_stg.htm) | f_mk_stg.htm | String creation |
| [make-array](../../resources/HyperSpec/Body/f_mk_ar.htm) | f_mk_ar.htm | Array creation |
| [copy-seq](../../resources/HyperSpec/Body/f_cp_seq.htm) | f_cp_seq.htm | Sequence copying |

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST compile `subseq` expressions to Wasm for strings, extracting byte ranges and creating new strings
- **FR-002**: Compiler MUST compile `subseq` expressions to Wasm for vectors, using `array.copy` to create new arrays
- **FR-003**: Compiler MUST compile `subseq` expressions to Wasm for lists, iterating and building new cons cells
- **FR-004**: Compiler MUST compile `(setf subseq)` expressions to Wasm for in-place modification
- **FR-005**: Compiler MUST compile `concatenate` expressions to Wasm for result-type `'string`
- **FR-006**: Compiler MUST compile `concatenate` expressions to Wasm for result-type `'vector` and specialized vector types
- **FR-007**: Compiler MUST compile `concatenate` expressions to Wasm for result-type `'list`
- **FR-008**: Compiler MUST compile `make-string` expressions to Wasm with `:initial-element` support
- **FR-009**: Compiler MUST compile `make-array` expressions with `:initial-element` keyword
- **FR-010**: Compiler MUST compile `make-array` expressions with `:initial-contents` keyword
- **FR-011**: Compiler MUST compile `copy-seq` expressions to Wasm for strings, vectors, and lists
- **FR-012**: Compiled code MUST signal appropriate error conditions for out-of-bounds indices
- **FR-013**: Compiled code MUST signal type-error for invalid argument types
- **FR-014**: All sequence operations MUST preserve element types in their results

### Key Entities

- **Sequence**: Abstract concept encompassing lists, vectors, and strings; all operations work polymorphically
- **Bounding Indices**: Start and optional end parameters for `subseq`; end defaults to sequence length
- **Result Type**: First argument to `concatenate` specifying the output sequence type

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Compilation rate increases from current level to at least 40% of compiler forms
- **SC-002**: tokenizer.lisp compiles completely without sequence-related errors
- **SC-003**: utf8.lisp compiles completely without sequence-related errors
- **SC-004**: The test expression `(subseq "hello" 1 3)` compiles and executes correctly, returning `"el"`
- **SC-005**: The test expression `(concatenate 'string "foo" "bar")` compiles and executes correctly, returning `"foobar"`
- **SC-006**: The test expression `(make-string 3 :initial-element #\x)` compiles and executes correctly, returning `"xxx"`
- **SC-007**: Generated Wasm modules pass `wasm-tools validate` without errors

## Assumptions

- Phase 13D-1 (array primitives: aref, svref, schar, elt, coerce) is completed before this phase
- The existing WasmGC type system (type index 2 for `$string`, arrays) is sufficient for sequence operations
- UTF-8 encoding for strings is maintained; `subseq` on strings works at byte level for Wasm, with character-level semantics handled at Lisp level
- Error signaling uses the existing condition system infrastructure:
  - `type-error` (existing in `src/clysm/conditions/types.lisp:85`) for invalid argument types
  - `bounding-indices-bad-error` will be defined as a new condition type (subtype of `type-error`) for out-of-bounds indices, per ANSI CL [HyperSpec definition](../../resources/HyperSpec/Body/e_boundi.htm)
- List sequence operations build new cons cells (no structure sharing)
- Negative indices are not supported (ANSI CL does not specify negative indexing)

## Dependencies

- Phase 13D-1: ANSI CL Array Primitives (001-ansi-array-primitives) - provides foundation for array access
- Existing WasmGC `array.new`, `array.copy` instruction support in codegen
- Existing cons cell infrastructure for list operations
- setf expander infrastructure (feature 028) for `(setf subseq)`

## Out of Scope

- Sequence functions like `map`, `reduce`, `find`, `remove`, `sort` (to be addressed in later phases)
- `replace` function (similar to setf subseq but standalone)
- `fill` function (array/sequence filling)
- Specialized array types (bit-vector, adjustable arrays, fill-pointers)
- Multidimensional array operations
- Displaced arrays and shared structure
- Performance optimizations (bounds-check elimination, SIMD operations)
- Character-level string operations for multibyte UTF-8 (handled by existing UTF-8 layer)
