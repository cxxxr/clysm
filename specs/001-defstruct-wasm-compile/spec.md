# Feature Specification: DEFSTRUCT Wasm Compilation

**Feature Branch**: `001-defstruct-wasm-compile`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 13D-10: DEFSTRUCTのWasmコンパイルを実装する。目標はCommon Lisp標準のDEFSTRUCTをWasmGCのstruct型にコンパイルすること。アプローチとしてDEFSTRUCTをDEFCLASSに展開するマクロを実装する。:conc-name, :include, :copier, :predicateオプションをサポートし、make-X, X-slot, (setf X-slot)アクセサ関数を自動生成する。検証基準: (1) defstruct定義がコンパイル成功、(2) コンストラクタ/アクセサが動作、(3) コンパイル率25%+達成、(4) Stage 1サイズ50KB+。"

## User Scenarios & Testing

### User Story 1 - Basic Structure Definition (Priority: P1)

A Lisp developer defines a simple structure using `defstruct` to create a data type with named slots. The structure compiles to WasmGC format and can be instantiated and accessed at runtime.

**Why this priority**: This is the fundamental use case - without basic defstruct support, no structure-based code can compile. This blocks the self-hosting bootstrap since the Clysm compiler uses structures internally.

**Independent Test**: Can be fully tested by compiling a simple defstruct definition and verifying the resulting Wasm module contains the expected struct type definition.

**Acceptance Scenarios**:

1. **Given** a defstruct definition `(defstruct point x y)`, **When** compiled to Wasm, **Then** the output contains a valid WasmGC struct type with two fields
2. **Given** a compiled defstruct, **When** the constructor `(make-point :x 10 :y 20)` is called, **Then** a new instance is created with the specified slot values
3. **Given** a point instance, **When** accessor `(point-x p)` is called, **Then** the x slot value is returned

---

### User Story 2 - Structure Options Support (Priority: P2)

A Lisp developer uses defstruct options to customize the generated structure, including custom conc-name, predicate functions, and copier functions.

**Why this priority**: Many real-world structures use options for cleaner APIs. The Clysm compiler itself uses `:conc-name` for brevity in accessor names.

**Independent Test**: Can be tested by compiling defstruct with each option and verifying the correctly-named functions are generated.

**Acceptance Scenarios**:

1. **Given** `(defstruct (node (:conc-name n-)) left right)`, **When** compiled, **Then** accessors are named `n-left` and `n-right` instead of `node-left` and `node-right`
2. **Given** `(defstruct (record (:predicate recordp)))`, **When** compiled, **Then** `recordp` function is generated that returns true for record instances
3. **Given** `(defstruct (item (:copier copy-item)))`, **When** compiled, **Then** `copy-item` function creates a shallow copy of an item instance
4. **Given** `(defstruct (thing (:conc-name nil)))`, **When** compiled, **Then** accessors have no prefix (just slot names)

---

### User Story 3 - Structure Inheritance (Priority: P2)

A Lisp developer uses the `:include` option to create a structure that inherits slots from a parent structure.

**Why this priority**: Inheritance enables code reuse and is used in the compiler's AST node hierarchy. Required for comprehensive defstruct support.

**Independent Test**: Can be tested by defining a parent and child structure, then verifying the child has all parent slots plus its own.

**Acceptance Scenarios**:

1. **Given** `(defstruct parent a b)` and `(defstruct (child (:include parent)) c)`, **When** compiled, **Then** child instances have slots a, b, and c
2. **Given** a child instance, **When** parent accessors are called, **Then** they correctly access inherited slots
3. **Given** a child instance, **When** parent predicate is checked, **Then** it returns true (child is-a parent)

---

### User Story 4 - Setf Accessors (Priority: P1)

A Lisp developer modifies structure slot values using setf with the generated accessor functions.

**Why this priority**: Mutability is essential for many algorithms. Without setf support, structures would be read-only.

**Independent Test**: Can be tested by creating an instance, modifying a slot with setf, and reading back the new value.

**Acceptance Scenarios**:

1. **Given** a point instance with x=10, **When** `(setf (point-x p) 20)` is executed, **Then** subsequent `(point-x p)` returns 20
2. **Given** setf on an accessor, **When** executed, **Then** the setf form returns the new value as per CL semantics

---

### Edge Cases

- What happens when defstruct slot has a default value? The default should be used when constructor keyword is omitted.
- What happens when defstruct slot has a :type declaration? Type information should be recorded for potential optimization.
- What happens with :read-only slots? Setf should signal an error or be blocked at compile time.
- How does system handle circular structure references? Should work via eqref semantics in WasmGC.
- What happens when :include references an undefined structure? Compilation should signal an error with clear message.

## Requirements

### Functional Requirements

- **FR-001**: System MUST expand `defstruct` forms into equivalent `defclass` definitions with appropriate slot specifications
- **FR-002**: System MUST generate a constructor function `make-NAME` that accepts keyword arguments for each slot
- **FR-003**: System MUST generate accessor functions `NAME-SLOT` for each slot that return the slot value
- **FR-004**: System MUST generate setf expanders for each accessor to enable `(setf (NAME-SLOT x) value)`
- **FR-005**: System MUST support `:conc-name` option to customize accessor name prefix (including nil for no prefix)
- **FR-006**: System MUST support `:include` option to inherit slots from a parent structure
- **FR-007**: System MUST support `:predicate` option to generate a type predicate function
- **FR-008**: System MUST support `:copier` option to generate a structure copying function
- **FR-009**: System MUST handle slot default values specified in the slot definition
- **FR-010**: System MUST compile the resulting defclass expansion to valid WasmGC struct types
- **FR-011**: System MUST register structure type information for use by `type-of` and `typep`

### Key Entities

- **Structure Type**: A user-defined data type with named slots, represented as a WasmGC struct
- **Slot Definition**: A slot within a structure, with name, optional default value, optional type, and optional read-only flag
- **Constructor Function**: Generated `make-NAME` function for creating instances
- **Accessor Function**: Generated `NAME-SLOT` function for reading slot values
- **Setf Expander**: Registered transformation for `(setf (accessor x) value)` to slot mutation

## Success Criteria

### Measurable Outcomes

- **SC-001**: All defstruct definitions in the Clysm compiler source compile successfully without errors
- **SC-002**: Generated constructors create instances that pass structure predicate tests
- **SC-003**: Generated accessors correctly read slot values that were set during construction
- **SC-004**: Setf forms on accessors correctly modify slot values
- **SC-005**: Compiler compilation rate reaches 25% or higher (up from current 14.2%)
- **SC-006**: Generated Stage 1 Wasm binary size reaches 50KB or larger (up from current 24.5KB)
- **SC-007**: All generated Wasm passes validation without errors

## Assumptions

- The existing CLOS implementation (defclass/defmethod) provides sufficient foundation for structure representation
- WasmGC struct types can adequately represent Common Lisp structures with proper boxing
- The `:type` and `:named` options are deferred to a future phase (standard CL allows omitting them)
- The `:print-function` and `:print-object` options are deferred (require format/write support)
- Slot :type declarations are informational only; runtime type checking is not enforced
- The :read-only slot option prevents setf at compile time but does not require runtime enforcement

## Out of Scope

- BOA constructors (By-Order-of-Arguments) - use keyword constructors only for simplicity
- :print-function and :print-object options - require print infrastructure
- :type option (for vector/list representation) - WasmGC structs are always used
- :named option - structures are always named in this implementation
- Structure redefinition semantics - redefining a structure during compilation is undefined
