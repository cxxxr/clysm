# Feature Specification: Stage 0 Capability Extension

**Feature Branch**: `038-stage0-extend`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 12拡張: Stage 0能力拡張を実装する。目標はセルフホスティング（Phase 13）への準備として、Stage 0のコンパイル率を1.6%から50%以上に引き上げること。具体的には(1) defconstant/defparameterをトップレベル定数としてコンパイル、(2) define-conditionをdefclass展開でサポート、(3) declareフォームの適切なスキップまたは処理、(4) ブートストラップエラー報告の詳細化。検証基準はコンパイル成功フォーム数の大幅増加とwasm-tools validate通過。"

## Background

Feature 037 (Cross-Compile Stage 0) established the bootstrap infrastructure for compiling Clysm's 41 compiler modules into a single WebAssembly binary. The current implementation achieves only 1.6% compilation rate (14/849 forms) due to unsupported Common Lisp constructs.

This feature extends Stage 0 capabilities to achieve 50%+ compilation rate, preparing the foundation for self-hosting (Phase 13).

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compile Constant Definitions (Priority: P1)

As a Clysm compiler developer, I want defconstant and defparameter forms to compile to Wasm globals, so that constants defined throughout the compiler source are available in the Stage 0 binary.

**Why this priority**: defconstant/defparameter are fundamental to CL code structure. The compiler source uses many such definitions for configuration, type indices, and opcodes. Without this support, large portions of the codebase cannot be compiled.

**Independent Test**: Can be tested by compiling a single defconstant form and verifying the Wasm output contains a global definition with the correct initial value.

**Acceptance Scenarios**:

1. **Given** a source file containing `(defconstant +max-stack+ 1000)`, **When** bootstrap compiles this form, **Then** the output Wasm contains a global for the constant with value 1000
2. **Given** a source file containing `(defparameter *debug-level* 0)`, **When** bootstrap compiles this form, **Then** the output Wasm contains a mutable global initialized to 0
3. **Given** defconstant with a computed value `(defconstant +bytes+ (* 8 1024))`, **When** bootstrap compiles this form, **Then** the value is computed at compile-time and stored as 8192
4. **Given** defconstant referencing another constant `(defconstant +doubled+ (* 2 +max-stack+))`, **When** bootstrap compiles this form, **Then** constant folding resolves to 2000 (given +max-stack+ = 1000)

---

### User Story 2 - Compile Condition Definitions (Priority: P2)

As a Clysm compiler developer, I want define-condition forms to compile via defclass expansion, so that the condition types used throughout the compiler are available in Stage 0.

**Why this priority**: The condition system is essential for error handling. The compiler defines many condition classes that must exist for proper error signaling and handling.

**Independent Test**: Can be tested by compiling a single define-condition form and verifying it produces equivalent structure to a defclass with condition parent.

**Acceptance Scenarios**:

1. **Given** `(define-condition my-error (error) ((msg :initarg :msg)))`, **When** bootstrap expands this form, **Then** it becomes `(defclass my-error (error) ((msg :initarg :msg)))` for compilation
2. **Given** a define-condition with :report option, **When** bootstrap expands this form, **Then** the :report option is handled appropriately (registered or ignored for basic support)
3. **Given** nested condition inheritance `(define-condition parse-error (my-error) ...)`, **When** bootstrap compiles both forms in order, **Then** both condition classes exist with correct inheritance

---

### User Story 3 - Handle Declaration Forms (Priority: P2)

As a Clysm compiler developer, I want declare forms to be properly handled (skipped or processed), so that functions containing type hints and optimization hints can compile successfully.

**Why this priority**: Most non-trivial CL functions contain declare forms for type declarations and optimization hints. Currently these cause compilation failures.

**Independent Test**: Can be tested by compiling a defun with declare forms and verifying the function body compiles correctly while declarations are appropriately handled.

**Acceptance Scenarios**:

1. **Given** `(defun foo (x) (declare (type fixnum x)) (+ x 1))`, **When** bootstrap compiles this form, **Then** the declare form is skipped and the function body compiles correctly
2. **Given** multiple declare forms `(defun bar (x y) (declare (type integer x)) (declare (optimize speed)) ...)`, **When** bootstrap compiles this form, **Then** all declare forms are skipped and the function compiles
3. **Given** a let form with declare `(let ((x 1)) (declare (type fixnum x)) x)`, **When** bootstrap compiles this form, **Then** the declaration is skipped and the let body compiles

---

### User Story 4 - Enhanced Error Reporting (Priority: P3)

As a Clysm compiler developer, I want detailed error reports showing which forms failed and why, so that I can identify and prioritize remaining unsupported features.

**Why this priority**: Visibility into compilation failures is essential for iterative improvement of the compiler subset.

**Independent Test**: Can be tested by running bootstrap on source containing known-unsupported forms and verifying the error report shows form type, location, and error message.

**Acceptance Scenarios**:

1. **Given** a form that fails to compile, **When** bootstrap runs, **Then** the error report shows the operator name (e.g., "defstruct"), the form preview (first 100 chars), and the error message
2. **Given** multiple failures of the same type, **When** bootstrap completes, **Then** the summary groups failures by operator and shows counts
3. **Given** a successful bootstrap run, **When** compilation completes, **Then** the report shows percentage progress (e.g., "427/849 forms compiled (50.3%)")

---

### User Story 5 - Handle defstruct via Expansion (Priority: P3)

As a Clysm compiler developer, I want defstruct forms to expand to constructor and accessor defuns, so that structure definitions used throughout the compiler can compile.

**Why this priority**: The compiler source heavily uses defstruct for AST nodes, compilation contexts, and intermediate representations.

**Independent Test**: Can be tested by compiling a simple defstruct and verifying constructor and accessor functions are generated.

**Acceptance Scenarios**:

1. **Given** `(defstruct point x y)`, **When** bootstrap expands this form, **Then** it generates make-point, point-x, point-y functions (using lists or arrays as backend)
2. **Given** defstruct with :constructor option, **When** bootstrap expands this form, **Then** the custom constructor name is used
3. **Given** defstruct with slot defaults `(defstruct point (x 0) (y 0))`, **When** bootstrap expands this form, **Then** the constructor uses the specified defaults

---

### Edge Cases

- What happens when defconstant references an undefined constant at compile time? Error with clear message identifying the undefined constant
- How does define-condition handle non-standard options like :default-initargs? Pass through to defclass (supported by CLOS foundation)
- What happens when declare contains unsupported declaration specifiers? Log warning at debug level, skip declaration entirely
- How are proclaim forms handled differently from declare? Skip at top level, similar behavior to declare within forms
- What happens if defstruct slot type conflicts with initialization value? Ignore type declarations, proceed with value

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST compile defconstant forms to immutable Wasm globals with compile-time evaluated values
- **FR-002**: System MUST compile defparameter forms to mutable Wasm globals
- **FR-003**: System MUST expand define-condition forms to defclass forms before compilation
- **FR-004**: System MUST skip declare forms within function and let bodies without causing compilation failure
- **FR-005**: System MUST skip proclaim forms at top level without causing compilation failure
- **FR-006**: System MUST generate per-operator failure statistics in the bootstrap output
- **FR-007**: System MUST show compilation rate percentage (successful/total forms)
- **FR-008**: System MUST preserve existing wasm-tools validation requirement
- **FR-009**: System SHOULD expand simple defstruct forms to constructor and accessor functions
- **FR-010**: System MUST handle constant folding for defconstant initialization expressions

### Key Entities

- **Bootstrap Context**: State maintained during compilation, extended with failure tracking by operator type
- **Form Statistics**: Per-operator counts of success/failure for reporting
- **Constant Registry**: Compile-time mapping of constant names to values for constant folding during bootstrap

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Bootstrap compilation rate increases from 1.6% to at least 50% (427+ forms of 849)
- **SC-002**: Output Wasm binary passes wasm-tools validate without errors
- **SC-003**: All defconstant/defparameter forms in the compiler source compile successfully
- **SC-004**: All define-condition forms in the compiler source compile after expansion
- **SC-005**: Functions containing declare forms compile successfully with declarations skipped
- **SC-006**: Bootstrap error report provides clear operator-grouped failure summary
- **SC-007**: Bootstrap completion provides percentage-based progress indication

## Assumptions

- Clysm's existing CLOS foundation (Feature 026) provides adequate defclass compilation for condition expansion
- Constant folding can handle basic arithmetic and known-constant references
- defstruct expansion to simple constructor/accessor pattern is acceptable (full CLOS-based struct support is not required for Stage 0)
- Wasm globals can represent all constant value types used in the compiler (fixnums, strings, symbols)
- Declaration forms are informational only and can be safely ignored for basic compilation
- The 50% target is achievable with the specified improvements based on analysis of current failure patterns

## Scope Boundaries

### In Scope
- defconstant/defparameter compilation as Wasm globals
- define-condition to defclass expansion
- declare/proclaim form skipping
- Enhanced error reporting with operator grouping
- Basic defstruct expansion (constructor + accessors)
- Constant folding for simple expressions

### Out of Scope
- Full ANSI CL defstruct semantics (copier, predicate, print-function, BOA constructors)
- Type declaration enforcement from declare forms
- Optimization hint processing from declare forms
- Multiple inheritance for conditions
- Full proclaim semantics (only top-level skipping)
- Runtime struct type checking
