# Feature Specification: make-instance* Primitive Implementation

**Feature Branch**: `001-make-instance-primitive`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 13D-1c: CLOS内部関数 make-instance* を実装する"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - DEFSTRUCT Compilation Success (Priority: P1)

A Clysm compiler developer defines a structure using DEFSTRUCT and expects the generated constructor code to compile successfully to WebAssembly.

**Why this priority**: DEFSTRUCT is fundamental for compiler self-hosting. Currently 1,953 DEFSTRUCT usages fail compilation because make-instance* is not recognized as a primitive.

**Independent Test**: Can be fully tested by compiling `(defstruct point x y)` and verifying the generated Wasm is valid.

**Acceptance Scenarios**:

1. **Given** a DEFSTRUCT definition `(defstruct point x y)`, **When** compiled with Clysm, **Then** the compilation succeeds without "unknown function" errors for make-instance*
2. **Given** a constructor call `(make-point :x 1 :y 2)`, **When** compiled with Clysm, **Then** Wasm code is generated that creates an instance with the specified slot values

---

### User Story 2 - Stage 1 Compilation Rate Improvement (Priority: P1)

A Clysm maintainer runs the Stage 1 bootstrap build and expects significantly fewer DEFSTRUCT-related compilation failures, improving the overall compilation rate toward self-hosting.

**Why this priority**: Self-hosting requires compiling the Clysm compiler itself. DEFSTRUCT failures account for a large portion of current compilation blockers.

**Independent Test**: Run `sbcl --load build/stage1-complete.lisp` and check that stage1-report.json shows 0 DEFSTRUCT-related failures.

**Acceptance Scenarios**:

1. **Given** the current codebase with DEFSTRUCT definitions throughout, **When** Stage 1 build is executed, **Then** all DEFSTRUCT forms compile successfully
2. **Given** Stage 1 compilation, **When** the report is generated, **Then** the compilation rate improves from ~14% to 30% or higher

---

### User Story 3 - Structure Accessor Compilation (Priority: P2)

A developer using structures expects that slot accessor functions (e.g., point-x, point-y) compile correctly and work with the instances created by make-instance*.

**Why this priority**: Accessors are essential for working with structures, but depend on make-instance* working first.

**Independent Test**: Compile and run accessor calls on structure instances.

**Acceptance Scenarios**:

1. **Given** a structure definition and constructor, **When** an accessor like `(point-x p)` is compiled, **Then** it produces valid Wasm that retrieves the correct slot value
2. **Given** a setf accessor form `(setf (point-x p) 10)`, **When** compiled, **Then** it produces valid Wasm that updates the slot value

---

### Edge Cases

- What happens when make-instance* is called with an unknown class name?
  - Behavior: Runtime error signaling "class not found"
- What happens when required initargs are missing?
  - Behavior: Slots are initialized to nil (Common Lisp semantics)
- How does make-instance* handle inherited slots from :include?
  - Behavior: Creates instance with all slots including inherited ones

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST recognize `make-instance*` as a primitive function in the compilation dispatch
- **FR-002**: Compiler MUST generate Wasm code that creates a CLOS instance when `make-instance*` is compiled
- **FR-003**: Generated code MUST support keyword initargs for slot initialization
- **FR-004**: Compiler MUST handle `make-instance*` calls with class name as first argument
- **FR-005**: Generated Wasm MUST pass validation via wasm-tools
- **FR-006**: DEFSTRUCT-generated constructors MUST compile successfully after this implementation
- **FR-007**: Compiler MUST handle make-instance* with any number of initarg pairs

### Key Entities

- **make-instance***: Internal CLOS function that creates instances; distinct from the standard `make-instance` generic function
- **Primitive**: A built-in function recognized by the compiler for special code generation handling
- **Structure Instance**: A CLOS instance representing a structure with class reference and slot storage

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All DEFSTRUCT definitions in the codebase (1,953 usages) compile without make-instance*-related errors
- **SC-002**: Stage 1 build compilation rate improves from 14% to at least 30%
- **SC-003**: Generated Wasm for DEFSTRUCT forms passes validation (wasm-tools validate exits with code 0)
- **SC-004**: stage1-report.json shows 0 failures with "make-instance*" in the error message

## Assumptions

- The existing `make-instance*` function in `src/clysm/clos/instance.lisp` provides the runtime semantics
- DEFSTRUCT macro correctly expands to DEFCLASS + make-instance* constructor calls
- The $instance WasmGC type (index 6) is suitable for structure instances
- Slot initialization follows Common Lisp semantics (nil for unspecified slots)
