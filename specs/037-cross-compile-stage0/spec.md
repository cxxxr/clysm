# Feature Specification: Cross-Compile Stage 0 (Lisp-11)

**Feature Branch**: `037-cross-compile-stage0`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 12: クロスコンパイル (Lisp-11) を実装する。目標はSBCLでClysm Compiler全体をコンパイルし、wasmtime実行可能なclysm-stage0.wasmを生成すること。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Build Stage 0 Compiler Binary (Priority: P1)

As a Clysm developer, I want to run a single build command that compiles the entire Clysm compiler to a WebAssembly binary, so that I can have a self-contained Stage 0 compiler that runs on wasmtime.

**Why this priority**: This is the core deliverable - without a working Stage 0 binary, the cross-compilation feature provides no value. This enables the self-hosting bootstrap chain.

**Independent Test**: Can be tested by running `sbcl --load build/bootstrap.lisp` and verifying that `dist/clysm-stage0.wasm` is produced and validates with `wasm-tools validate`.

**Acceptance Scenarios**:

1. **Given** SBCL 2.4+ is installed and all Clysm source files exist, **When** I run the bootstrap build script, **Then** a file `dist/clysm-stage0.wasm` is created.
2. **Given** `dist/clysm-stage0.wasm` exists, **When** I run `wasm-tools validate dist/clysm-stage0.wasm`, **Then** validation passes with exit code 0.
3. **Given** a build failure occurs in any module, **When** the build script runs, **Then** an error message identifies the failing module path and the specific error.

---

### User Story 2 - Verify Basic Arithmetic Compilation (Priority: P2)

As a Clysm developer, I want to verify that Stage 0 can compile basic arithmetic expressions, so that I can confirm the compiler core functionality works.

**Why this priority**: Arithmetic is the simplest possible verification - if this fails, no other features can work. This validates the core expression compiler and Wasm instruction generation.

**Independent Test**: Can be tested by invoking Stage 0 with `(+ 1 2)` as input and verifying the output produces a valid Wasm module that computes 3.

**Acceptance Scenarios**:

1. **Given** `dist/clysm-stage0.wasm` exists and wasmtime is installed, **When** I invoke Stage 0 to compile `(+ 1 2)`, **Then** it produces a valid Wasm module.
2. **Given** Stage 0 compiled `(+ 1 2)` to Wasm, **When** the generated Wasm is executed, **Then** the result is the integer 3.

---

### User Story 3 - Verify Function Definition and Invocation (Priority: P3)

As a Clysm developer, I want to verify that Stage 0 can compile function definitions and invocations, so that I can confirm structured code compilation works.

**Why this priority**: Function definition is essential for any non-trivial program. This validates defun, lambda, and function call compilation.

**Independent Test**: Can be tested by compiling `(defun add (a b) (+ a b))` followed by `(add 3 4)` and verifying the result is 7.

**Acceptance Scenarios**:

1. **Given** Stage 0 is running, **When** I compile `(defun square (x) (* x x))`, **Then** it produces a valid Wasm module with a function named "square".
2. **Given** the square function is defined, **When** I compile and execute `(square 5)`, **Then** the result is 25.

---

### User Story 4 - Verify Control Flow Compilation (Priority: P4)

As a Clysm developer, I want to verify that Stage 0 can compile control flow constructs (if, when, cond), so that I can confirm conditional logic compilation works.

**Why this priority**: Control flow is required for any conditional logic. This validates if/when/cond special forms and their Wasm block/br generation.

**Independent Test**: Can be tested by compiling `(if (> 5 3) 'yes 'no)` and verifying the result is the symbol YES.

**Acceptance Scenarios**:

1. **Given** Stage 0 is running, **When** I compile `(if t 1 2)`, **Then** executing the result yields 1.
2. **Given** Stage 0 is running, **When** I compile `(if nil 1 2)`, **Then** executing the result yields 2.
3. **Given** Stage 0 is running, **When** I compile `(when t 42)`, **Then** executing the result yields 42.

---

### Edge Cases

- What happens when a source file contains syntax errors? Build fails with parser error including file path and line number.
- How does the build handle circular dependencies? The fixed 41-module order from `*compilation-order*` prevents cycles.
- What happens if a module uses an unsupported CL feature? Build fails with error identifying the unsupported feature and module.
- What happens if wasmtime is not installed? Verification tests skip with a warning, but Stage 0 binary is still produced.
- How is compile-time state isolated from SBCL's environment? A fresh package namespace is used to prevent symbol conflicts.

## Requirements *(mandatory)*

### Functional Requirements

**Phase 12A: Module Integration**

- **FR-001**: Build system MUST compile all 41 modules defined in `*compilation-order*` in dependency order.
- **FR-002**: Build system MUST merge all compiled modules into a single Wasm binary (`clysm-stage0.wasm`).
- **FR-003**: Build system MUST export a main entry point function for wasmtime invocation.
- **FR-004**: Build system MUST generate valid WasmGC-compatible output (using GC types, not linear memory).

**Phase 12B: Bootstrap Script**

- **FR-005**: Bootstrap script (`build/bootstrap.lisp`) MUST be runnable with `sbcl --load build/bootstrap.lisp`.
- **FR-006**: Bootstrap script MUST isolate compile-time state in a dedicated package to prevent SBCL symbol conflicts.
- **FR-007**: Bootstrap script MUST output the compiled binary to `dist/clysm-stage0.wasm`.
- **FR-008**: Bootstrap script MUST report progress as each module is compiled.
- **FR-009**: Bootstrap script MUST halt and report errors with file path when compilation fails.

**Phase 12C: Verification**

- **FR-010**: Stage 0 binary MUST pass `wasm-tools validate` without errors.
- **FR-011**: Stage 0 binary MUST be executable with wasmtime.
- **FR-012**: Stage 0 MUST correctly compile and execute `(+ 1 2)` yielding 3.
- **FR-013**: Stage 0 MUST correctly compile `defun` forms and execute the defined functions.
- **FR-014**: Stage 0 MUST correctly compile `if`/`when`/`cond` control flow forms.

### Key Entities

- **Stage 0 Binary**: The cross-compiled Clysm compiler as a single Wasm binary (`clysm-stage0.wasm`). Contains all 41 modules merged together with exported entry points.
- **Module Order**: The dependency-ordered list of 41 source files from `*compilation-order*`. Backend -> Reader -> Compiler -> Runtime -> CLOS -> Conditions.
- **Bootstrap Script**: The SBCL-runnable build script that orchestrates compilation and binary generation.
- **Verification Suite**: A set of simple test expressions that validate Stage 0 functionality.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Running `sbcl --load build/bootstrap.lisp` produces `dist/clysm-stage0.wasm` within 5 minutes on a standard development machine.
- **SC-002**: `wasm-tools validate dist/clysm-stage0.wasm` exits with code 0 (valid).
- **SC-003**: Stage 0 compiles and correctly executes `(+ 1 2)` yielding 3.
- **SC-004**: Stage 0 compiles and correctly executes `(defun f (x) (* x 2)) (f 21)` yielding 42.
- **SC-005**: Stage 0 compiles and correctly executes `(if (> 10 5) 'greater 'less)` yielding GREATER.
- **SC-006**: All 41 modules compile without unsupported feature errors.
- **SC-007**: Build failure messages include the failing module's file path.

## Assumptions

1. **SBCL 2.4+ available**: The build environment has a compatible SBCL installation.
2. **wasm-tools installed**: For validation, though Stage 0 is produced regardless.
3. **wasmtime installed**: For execution verification, though Stage 0 is produced regardless.
4. **No new CL features needed**: All required CL features are in the blessed subset (Feature 036 confirmed 100% coverage).
5. **Module order is correct**: The 41-module order in `*compilation-order*` is verified as correct dependency order.
6. **Single-file compilation**: Each module can be compiled independently before merging.

## Out of Scope

- Stage 1 compilation (Stage 0 compiling itself) - that's a future milestone.
- REPL functionality in Stage 0 - initial focus is compile-only.
- JIT tier promotion in Stage 0 - interpreter tier only for initial verification.
- Full ANSI CL test suite in Stage 0 - basic verification only.
- Multi-platform host shim (Node.js, browser) - wasmtime CLI only for now.
