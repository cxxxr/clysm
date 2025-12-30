# Feature Specification: Compile-Time Directive Processing

**Feature Branch**: `001-compile-time-directives`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Phase 13D-3: コンパイル時ディレクティブ処理を実装する。目標はin-package、defpackage、declaim、proclaimをコンパイル時に評価し、AST生成をスキップすること。これにより49件のin-package、9件のdefpackage、3件のdeclaimによるコンパイルエラー（計61件）が解決され、コンパイル率が向上する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - IN-PACKAGE Directive Handling (Priority: P1)

As a developer compiling Clysm source files, I want the compiler to correctly process `in-package` forms at compile-time so that subsequent forms are compiled in the correct package context without generating unnecessary Wasm code.

**Why this priority**: `in-package` accounts for 49 out of 61 affected forms (80%). Resolving this has the highest impact on compilation success rate.

**Independent Test**: Can be fully tested by compiling a source file containing `(in-package :clysm)` followed by symbol definitions and verifying that (1) no AST/Wasm is generated for in-package, (2) subsequent symbols are interned in the correct package.

**Acceptance Scenarios**:

1. **Given** a source file with `(in-package :clysm)` at the top, **When** the compiler processes this form, **Then** the compiler's current package is set to CLYSM and no AST nodes are generated for this form.
2. **Given** multiple `in-package` forms in a file (switching between packages), **When** the compiler processes each form, **Then** the package context switches correctly for subsequent forms.
3. **Given** an `in-package` referencing a non-existent package, **When** the compiler processes this form, **Then** an appropriate compile-time error is signaled.

---

### User Story 2 - DEFPACKAGE Directive Handling (Priority: P2)

As a developer compiling Clysm source files, I want the compiler to correctly process `defpackage` forms at compile-time so that new packages are created and available for use by subsequent `in-package` forms.

**Why this priority**: `defpackage` must work correctly for `in-package` to function properly. This resolves 9 compilation errors.

**Independent Test**: Can be tested by compiling a file with `(defpackage :my-package (:use :cl))` and verifying the package is created at compile-time without Wasm generation.

**Acceptance Scenarios**:

1. **Given** a source file with `(defpackage :my-package (:use :cl) (:export #:my-function))`, **When** the compiler processes this form, **Then** the package MY-PACKAGE is created with appropriate use-list and exports, and no AST is generated.
2. **Given** a `defpackage` followed by `in-package` for the same package, **When** compilation proceeds, **Then** the in-package correctly switches to the newly created package.
3. **Given** a `defpackage` with complex options (nicknames, shadow, import-from), **When** the compiler processes it, **Then** all options take effect at compile-time.

---

### User Story 3 - DECLAIM/PROCLAIM Directive Handling (Priority: P3)

As a developer compiling Clysm source files, I want the compiler to correctly process `declaim` and `proclaim` forms at compile-time so that declarations (optimize, type, special) affect subsequent compilation.

**Why this priority**: This resolves 3 remaining compilation errors and enables optimization declarations.

**Independent Test**: Can be tested by compiling `(declaim (optimize (speed 3) (safety 0)))` and verifying no Wasm is generated and the declaration affects the compilation environment.

**Acceptance Scenarios**:

1. **Given** a source file with `(declaim (optimize (speed 3)))`, **When** the compiler processes this form, **Then** the optimization settings are recorded in the compilation environment and no AST is generated.
2. **Given** a `(declaim (special *my-var*))` form, **When** the compiler processes it, **Then** `*my-var*` is marked as special for subsequent compilation.
3. **Given** a `(proclaim '(type fixnum x))` form, **When** the compiler evaluates it, **Then** the type declaration is recorded at compile-time.

---

### Edge Cases

- What happens when `in-package` references a package that will be defined later in the file? The compiler signals an error since packages must be defined before use.
- How does the system handle malformed directive forms (e.g., `(in-package)` with no argument)? Standard Common Lisp error handling applies; appropriate compile-time errors are signaled.
- What happens with nested eval-when containing these directives? The directives are evaluated according to eval-when semantics.
- How are these directives handled in non-toplevel positions? They behave as per ANSI CL specification (in-package is only meaningful at toplevel).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Compiler MUST recognize `in-package` forms at toplevel and evaluate them at compile-time.
- **FR-002**: Compiler MUST recognize `defpackage` forms at toplevel and evaluate them at compile-time.
- **FR-003**: Compiler MUST recognize `declaim` forms at toplevel and evaluate them at compile-time.
- **FR-004**: Compiler MUST recognize `proclaim` forms at toplevel and evaluate them at compile-time.
- **FR-005**: For all recognized directive forms, the compiler MUST NOT generate AST nodes or Wasm bytecode.
- **FR-006**: The `compile-toplevel-form` function MUST include dispatch logic to detect these directive forms before standard AST generation.
- **FR-007**: Directive evaluation MUST modify the compile-time environment (current package, declarations) appropriately.
- **FR-008**: Errors in directive evaluation (e.g., undefined package) MUST be signaled as compile-time errors with clear messages.
- **FR-009**: The return value for processed directives MUST be nil or an empty progn to indicate no Wasm output needed.

### Key Entities

- **Compile-Time Environment**: The state maintained during compilation including current package, optimization settings, and type/special declarations.
- **Toplevel Form**: A form appearing at the top level of a source file, subject to compile-time evaluation rules per ANSI CL.
- **Directive Form**: One of in-package, defpackage, declaim, or proclaim - forms that affect the compilation environment rather than producing runtime code.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 49 `in-package` forms in the Clysm codebase compile without error.
- **SC-002**: All 9 `defpackage` forms in the Clysm codebase compile without error.
- **SC-003**: All 3 `declaim` forms in the Clysm codebase compile without error.
- **SC-004**: Total compilation error count decreases by at least 61 (from the current count attributable to these forms).
- **SC-005**: No Wasm bytecode is generated for any of these directive forms.
- **SC-006**: Compilation of files with these directives completes without manual intervention.

## Assumptions

- The SBCL host environment correctly implements all ANSI CL package and declaration semantics.
- The compile-time environment is accessible and modifiable from within `compile-toplevel-form`.
- These directives appear at toplevel; non-toplevel occurrences follow standard CL semantics.
- The existing compilation pipeline can be extended with a dispatch check before AST generation.
