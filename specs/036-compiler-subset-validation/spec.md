# Feature Specification: Compiler Subset Validation (Phase 11)

**Feature Branch**: `036-compiler-subset-validation`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 11: コンパイラサブセット検証 (Lisp-10) - Validate that Clysm compiler modules can be compiled by Clysm itself"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - CL Feature Usage Analysis (Priority: P1)

A compiler maintainer wants to understand which Common Lisp features are used by each compiler module and whether Clysm supports them, enabling identification of self-compilation gaps.

**Why this priority**: This is the foundation for self-hosting validation. Without knowing which CL features are used, we cannot plan which features need to be implemented or work around.

**Independent Test**: Can be fully tested by running the static analyzer on each module directory and producing a feature usage report.

**Acceptance Scenarios**:

1. **Given** a compiler module directory (e.g., `src/clysm/backend/`), **When** the static analyzer runs, **Then** it produces a list of all CL symbols/forms used in that module
2. **Given** a CL feature usage list for a module, **When** compared against Clysm's supported feature list, **Then** the analyzer categorizes each feature as "supported", "partial", or "unsupported"
3. **Given** all modules have been analyzed, **When** generating the summary report, **Then** the report shows per-module feature coverage percentages

---

### User Story 2 - Dependency-Order Compilation (Priority: P1)

A compiler maintainer wants to compile compiler modules in proper dependency order, ensuring each module compiles successfully before dependent modules are attempted.

**Why this priority**: Compilation order is critical - a module cannot be compiled if its dependencies fail. This is a prerequisite for the final self-hosting goal.

**Independent Test**: Can be fully tested by attempting to compile each module in the specified order and verifying each produces valid Wasm output.

**Acceptance Scenarios**:

1. **Given** a module with no dependencies (e.g., `leb128.lisp`), **When** compiled by Clysm, **Then** valid Wasm binary is produced
2. **Given** a module that depends on previously compiled modules (e.g., `sections.lisp` depends on `leb128.lisp`), **When** compiled by Clysm with dependencies available, **Then** valid Wasm binary is produced
3. **Given** a compilation fails for a module, **When** the error is logged, **Then** the specific unsupported CL feature or error is identified

---

### User Story 3 - Wasm Validation Test Suite (Priority: P2)

A compiler maintainer wants automated tests that verify each compiled module produces valid Wasm, ensuring ongoing self-compilation capability.

**Why this priority**: Automated testing prevents regression and provides continuous validation of self-compilation capability.

**Independent Test**: Can be fully tested by running the test suite which compiles each module and validates the output.

**Acceptance Scenarios**:

1. **Given** a module compilation test, **When** the test runs, **Then** it compiles the module and validates the Wasm output
2. **Given** a module produces invalid Wasm, **When** the validation test fails, **Then** the specific validation error is reported
3. **Given** all module compilation tests pass, **When** the test suite completes, **Then** a summary shows 100% validation success

---

### User Story 4 - Blessed Subset Documentation (Priority: P2)

A compiler maintainer wants a documented "blessed subset" of Common Lisp that is guaranteed to be self-compilable, enabling other Clysm developers to know which features they can safely use.

**Why this priority**: Documentation enables the community to write self-compilable code and understand Clysm's capabilities.

**Independent Test**: Can be fully tested by verifying the blessed-subset.lisp file lists all supported features and can be loaded without errors.

**Acceptance Scenarios**:

1. **Given** self-compilation validation is complete, **When** generating blessed-subset.lisp, **Then** all verified CL features are listed with their support status
2. **Given** a CL feature in blessed-subset.lisp is marked "supported", **When** used in compiler modules, **Then** it compiles successfully
3. **Given** blessed-subset.lisp exists, **When** a developer reads it, **Then** they understand which CL features can be used for self-compilable code

---

### Edge Cases

- What happens when a module uses a CL feature that Clysm partially supports (e.g., subset of keyword arguments)?
- How does the system handle circular dependencies between modules?
- What happens when a module uses reader macros or complex macro expansions?
- How does the system handle SBCL-specific extensions vs. standard CL?

## Requirements *(mandatory)*

### Functional Requirements

**Phase 11A: Static Analysis**

- **FR-001**: System MUST scan source files in `backend/`, `reader/`, `compiler/`, `runtime/`, `clos/`, `conditions/` directories
- **FR-002**: System MUST extract all CL symbols referenced in each file (defun, defmacro, let, loop, etc.)
- **FR-003**: System MUST compare extracted symbols against Clysm's supported feature registry
- **FR-004**: System MUST categorize each symbol as "supported", "partial", or "unsupported"
- **FR-005**: System MUST generate a per-module feature coverage report

**Phase 11B: Dependency-Order Compilation**

- **FR-006**: System MUST compile modules in the specified order: `leb128.lisp` → `sections.lisp` → `tokenizer.lisp` → `parser.lisp` → `ast.lisp` → `codegen/*` → `compiler.lisp`
- **FR-007**: System MUST halt compilation sequence if a module fails to compile
- **FR-008**: System MUST log specific errors including unsupported CL features when compilation fails
- **FR-009**: System MUST produce valid Wasm binary output for each successfully compiled module

**Phase 11C: Wasm Validation Tests**

- **FR-010**: System MUST provide a test for each compiler module that compiles it and validates the output
- **FR-011**: System MUST use `wasm-tools validate` for Wasm validation
- **FR-012**: System MUST report detailed validation errors when a module's Wasm is invalid
- **FR-013**: System MUST provide a test summary showing pass/fail status for all modules

**Phase 11D: Blessed Subset Documentation**

- **FR-014**: System MUST generate `blessed-subset.lisp` documenting all self-compilable CL features
- **FR-015**: `blessed-subset.lisp` MUST categorize features by type (special forms, macros, functions, data types)
- **FR-016**: `blessed-subset.lisp` MUST include usage notes for partially supported features

### Key Entities

- **CL Feature**: A Common Lisp symbol or form (e.g., `defun`, `loop`, `cond`) with support status
- **Module**: A Lisp source file in the compiler codebase with dependencies on other modules
- **Feature Coverage Report**: A document showing which CL features a module uses and their support status
- **Blessed Subset**: The documented collection of CL features guaranteed to compile under Clysm

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of compiler modules produce Wasm output that passes `wasm-tools validate`
- **SC-002**: Static analysis covers all source files in the 6 target directories (backend/, reader/, compiler/, runtime/, clos/, conditions/)
- **SC-003**: Feature coverage report identifies all CL symbols used across the compiler codebase
- **SC-004**: `blessed-subset.lisp` documents all verified self-compilable CL features
- **SC-005**: Compilation test suite achieves 100% pass rate for all modules in dependency order
- **SC-006**: Any unsupported CL feature blocking self-compilation is identified with specific file:line reference

## Assumptions

- The compiler modules are written in standard Common Lisp without SBCL-specific extensions that cannot be polyfilled
- The dependency order specified (`leb128.lisp` → `sections.lisp` → ...) reflects the actual dependency graph
- `wasm-tools validate` is available in the build environment
- Clysm's feature support registry is accurate and up-to-date
- Partial feature support (e.g., subset of keyword arguments) can be documented and worked around

## Out of Scope

- Implementing missing CL features identified by static analysis (that's a separate feature)
- Runtime behavior testing of compiled modules (this feature focuses on compilation validation only)
- Performance benchmarking of compiled modules
- Comparison with other Wasm-targeting Lisp compilers
