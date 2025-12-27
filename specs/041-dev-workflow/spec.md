# Feature Specification: Development Workflow Establishment

**Feature Branch**: `041-dev-workflow`
**Created**: 2025-12-27
**Status**: Draft
**Input**: User description: "Phase 13C: 開発ワークフロー確立を実装する。目標はセルフホスティング開発サイクルの実現。(1) CLIインターフェース: ./clysm compile src/**/*.lisp -o output.wasm コマンドを実装。(2) インクリメンタルコンパイル: ファイルタイムスタンプと依存関係を追跡し、変更されたモジュールのみ再コンパイル。(3) エラーリカバリ: 1ファイルのエラーで全体が停止せず、エラーを報告しつつ他のファイルを処理継続。(4) REPLからのコンパイル: (compile-file path) 関数でWasm生成可能に。最終的にSBCLなしでClysm自身を開発できるようにする。"

## User Scenarios & Testing

### User Story 1 - CLI Compilation (Priority: P1)

A developer compiles Clysm source files to WebAssembly using a command-line interface, enabling batch compilation without manual REPL interaction.

**Why this priority**: The CLI is the primary interface for building projects. Without it, developers cannot create distributable binaries or automate builds in CI/CD pipelines. This is the foundational capability for self-hosting.

**Independent Test**: Can be fully tested by running the CLI with source files and verifying a valid Wasm binary is produced.

**Acceptance Scenarios**:

1. **Given** a set of Lisp source files in a directory, **When** the developer runs `./clysm compile src/**/*.lisp -o output.wasm`, **Then** a valid WebAssembly binary is produced at the specified output path.
2. **Given** no input files match the glob pattern, **When** the developer runs the compile command, **Then** an appropriate error message is displayed and the command exits with a non-zero status.
3. **Given** the output directory does not exist, **When** the developer runs the compile command with `-o path/to/output.wasm`, **Then** the necessary directories are created automatically.
4. **Given** multiple source files with dependencies, **When** the developer runs the compile command, **Then** files are compiled in dependency order and all exports are available in the final binary.

---

### User Story 2 - Incremental Compilation (Priority: P2)

A developer modifies source files and recompiles, with only changed modules being reprocessed to minimize compilation time.

**Why this priority**: Incremental compilation significantly improves developer productivity during iterative development. However, correct full compilation (P1) must work first before optimization.

**Independent Test**: Can be fully tested by modifying a single file and verifying only that file (and its dependents) are recompiled.

**Acceptance Scenarios**:

1. **Given** a previous successful compilation, **When** the developer modifies one source file and recompiles, **Then** only the modified file and files depending on it are recompiled.
2. **Given** a previous successful compilation, **When** the developer runs compile without any file changes, **Then** compilation completes immediately with a message indicating no changes detected.
3. **Given** a source file that was deleted since last compilation, **When** the developer recompiles, **Then** the system detects the deletion and performs appropriate cleanup.
4. **Given** a new source file added to the project, **When** the developer recompiles, **Then** the new file is detected and compiled along with any existing files that may depend on it.

---

### User Story 3 - Error Recovery (Priority: P2)

A developer can compile a project even when some source files contain errors, receiving error reports while other files are processed successfully.

**Why this priority**: Error recovery enables partial builds and better error diagnosis. Developers can see all errors across the project rather than fixing one file at a time. Ranked P2 alongside incremental compilation as both improve developer experience.

**Independent Test**: Can be fully tested by introducing errors in some files and verifying other files still compile and errors are reported.

**Acceptance Scenarios**:

1. **Given** a project with one file containing syntax errors, **When** the developer runs compile, **Then** the error is reported with file name and location, and other files are compiled successfully.
2. **Given** a project with multiple files containing errors, **When** the developer runs compile, **Then** all errors are collected and reported in a summary.
3. **Given** a file with an error that other files depend on, **When** the developer runs compile, **Then** dependent files are skipped with a clear message explaining why.
4. **Given** all files have errors, **When** the developer runs compile, **Then** all errors are reported and the command exits with non-zero status without producing partial output.

---

### User Story 4 - REPL Compilation (Priority: P3)

A developer compiles individual files from within a running REPL session, enabling interactive development and testing.

**Why this priority**: REPL compilation supports interactive development workflows but requires the core compilation infrastructure (P1) to be working first. It is a convenience feature for iterative development.

**Independent Test**: Can be fully tested by loading the REPL and calling compile-file on a source file.

**Acceptance Scenarios**:

1. **Given** a running Clysm REPL, **When** the developer calls `(compile-file "path/to/source.lisp")`, **Then** the file is compiled and a Wasm module is generated.
2. **Given** a running REPL with previously compiled modules loaded, **When** the developer recompiles a file, **Then** the new compilation replaces the previous version.
3. **Given** a file path that does not exist, **When** the developer calls compile-file, **Then** an appropriate condition is signaled with a restart to provide an alternative path.
4. **Given** a file with compilation errors, **When** the developer calls compile-file, **Then** errors are reported interactively and the developer can inspect and potentially fix issues.

---

### User Story 5 - Self-Hosting Development (Priority: P1)

A developer can use a Clysm-compiled binary to compile Clysm source code, achieving true self-hosting where SBCL is no longer required for development.

**Why this priority**: This is the ultimate goal of the feature. It validates that all components work together and that Clysm can sustain its own development cycle.

**Independent Test**: Can be fully tested by using the Stage 1 binary to compile Clysm source and verifying the output matches expected behavior.

**Acceptance Scenarios**:

1. **Given** a working Stage 1 Clysm binary running on wasmtime, **When** the developer uses it to compile Clysm source files, **Then** a new Wasm binary is produced.
2. **Given** the Stage 1 binary and source files, **When** the developer makes a code change and recompiles using Stage 1, **Then** the change is reflected in the new binary.
3. **Given** only wasmtime and Node.js installed (no SBCL), **When** the developer runs the full build process, **Then** a working Clysm binary is produced.

---

### Edge Cases

- What happens when circular dependencies exist between modules?
- How does the system handle very large source files (>100KB)?
- What happens when the output file is locked by another process?
- How does the system behave when disk space is exhausted during compilation?
- What happens when a file is modified during compilation?
- How does the system handle symbolic links in source paths?
- What happens when glob patterns match non-Lisp files?

## Requirements

### Functional Requirements

- **FR-001**: System MUST provide a CLI command `./clysm compile` that accepts glob patterns for input files.
- **FR-002**: System MUST support an `-o` or `--output` flag to specify the output Wasm binary path.
- **FR-003**: System MUST compile source files in dependency order based on package definitions and load directives.
- **FR-004**: System MUST track file modification timestamps for incremental compilation.
- **FR-005**: System MUST maintain a dependency graph of modules to determine which files need recompilation.
- **FR-006**: System MUST continue processing remaining files when one file fails to compile.
- **FR-007**: System MUST report all compilation errors with source file path, line number, and descriptive message.
- **FR-008**: System MUST provide a `compile-file` function callable from the REPL.
- **FR-009**: System MUST support compilation without SBCL when using a Stage 1+ binary.
- **FR-010**: System MUST create output directories if they do not exist.
- **FR-011**: System MUST validate the final Wasm binary structure before writing.
- **FR-012**: System MUST provide progress feedback during compilation showing current file and percentage complete.
- **FR-013**: System MUST exit with status code 0 on success, non-zero on failure.
- **FR-014**: System MUST support a `--verbose` flag for detailed compilation output.
- **FR-015**: System MUST support a `--force` flag to bypass incremental compilation and recompile all files.
- **FR-016**: System MUST persist incremental compilation state between invocations.
- **FR-017**: System MUST detect and report circular dependencies as errors.

### Key Entities

- **SourceModule**: Represents a single Lisp source file with its path, modification time, parsed forms, and dependency relationships.
- **DependencyGraph**: Tracks relationships between modules, enabling topological sorting for compilation order and change propagation for incremental builds.
- **CompilationCache**: Stores intermediate compilation results and file timestamps to enable incremental compilation across sessions.
- **CompilationResult**: Contains the outcome of compiling a module, including success/failure status, generated bytes, errors, and warnings.
- **CompilationSession**: Manages the state of a single compilation run, including progress tracking, error collection, and output generation.

## Success Criteria

### Measurable Outcomes

- **SC-001**: Developers can compile the full Clysm codebase (45+ modules) to a valid Wasm binary in a single command.
- **SC-002**: Incremental compilation of a single changed file completes in under 5 seconds for typical module sizes.
- **SC-003**: Full compilation provides clear progress indication, showing at minimum current file and overall percentage.
- **SC-004**: All compilation errors include file path and line number, enabling direct navigation to the error source.
- **SC-005**: A Stage 1 Clysm binary can successfully compile Clysm source code without SBCL installed.
- **SC-006**: Compilation with errors in some files still produces valid output for error-free files when possible.
- **SC-007**: The REPL compile-file function produces identical output to the CLI for the same source file.

## Assumptions

- wasmtime and Node.js are available in the runtime environment for executing Wasm binaries.
- Source files use UTF-8 encoding.
- File modification timestamps are reliable on the target filesystem.
- The host filesystem provides standard directory and file operations through the FFI shim.
- Module dependencies are determined by in-package declarations and explicit require/load directives.
- The existing Stage 1 infrastructure (Feature 039) provides the foundation for Wasm-based compilation.

## Dependencies

- Feature 037: Cross-Compile Stage 0 - Provides initial bootstrap capability
- Feature 038: Stage 0 Capability Extension - Extends compiled feature set
- Feature 039: Stage 1 Compiler Generation - Provides wasmtime integration and source reading
- Feature 040: Fixed-Point Verification - Validates self-hosting correctness
- Feature 035: FFI Filesystem Access - Provides file I/O capabilities
