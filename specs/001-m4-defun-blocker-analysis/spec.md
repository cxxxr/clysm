# Feature Specification: Phase 13D M4 - DEFUN Blocker Analysis and Resolution

**Feature Branch**: `001-m4-defun-blocker-analysis`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 13D M4: DEFUNブロッカー詳細分析と解消を実装する。現在DEFUNは18,997回失敗し全失敗の90%を占める。目標：(1) DEFUN失敗の詳細エラーログ収集機構を追加、(2) 上位エラーパターンを分類、(3) &aux等の複雑なlambda-list対応、(4) backend/およびreader/モジュールの完全コンパイル達成。検証基準：コンパイル率35%以上、DEFUNエラー15,000以下。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Detailed DEFUN Error Logging (Priority: P1)

As a compiler developer, I need to understand exactly why each DEFUN form fails to compile so that I can prioritize and fix the most impactful issues.

**Why this priority**: Without detailed error information, developers cannot systematically address compilation failures. This is the foundation for all subsequent improvements.

**Independent Test**: Can be fully tested by running Stage 1 compilation and verifying that each DEFUN failure includes detailed error context (error type, lambda-list details, failing subform).

**Acceptance Scenarios**:

1. **Given** a DEFUN form that fails to compile, **When** compilation is attempted, **Then** the error log includes: function name, error type, failing subform, and stack trace context
2. **Given** multiple DEFUN failures during Stage 1 generation, **When** compilation completes, **Then** a summary report groups failures by error pattern with occurrence counts
3. **Given** an error log from compilation, **When** a developer reviews it, **Then** they can identify the root cause without additional debugging

---

### User Story 2 - Error Pattern Classification (Priority: P1)

As a compiler developer, I need failures automatically categorized by error pattern so that I can fix entire categories of issues rather than individual failures.

**Why this priority**: With 18,997 failures, individual analysis is impractical. Pattern classification enables batch resolution and strategic prioritization.

**Independent Test**: Can be fully tested by running compilation and verifying that the output report categorizes errors into distinct patterns with counts and examples.

**Acceptance Scenarios**:

1. **Given** compilation with multiple DEFUN failures, **When** the analysis runs, **Then** failures are grouped into distinct error pattern categories
2. **Given** a pattern category (e.g., "unsupported lambda-list keyword"), **When** reviewing the report, **Then** it shows the count, percentage of total failures, and representative examples
3. **Given** the top 10 error patterns, **When** ranked by occurrence count, **Then** they collectively account for at least 80% of all failures

---

### User Story 3 - Complex Lambda-List Support (Priority: P2)

As a compiler developer, I need the compiler to handle complex lambda-list features (&aux, &allow-other-keys, nested destructuring) so that more DEFUN forms can compile successfully.

**Why this priority**: Lambda-list issues are a major blocker category. Supporting these features directly reduces DEFUN failures.

**Independent Test**: Can be tested by compiling DEFUN forms containing each lambda-list feature and verifying successful compilation without runtime errors.

**Acceptance Scenarios**:

1. **Given** a DEFUN with &aux parameters, **When** compiled, **Then** the generated Wasm correctly initializes auxiliary variables
2. **Given** a DEFUN with &allow-other-keys, **When** compiled, **Then** keyword argument handling ignores unrecognized keywords at runtime
3. **Given** a DEFUN with &whole or destructuring in parameters, **When** compiled, **Then** pattern matching executes correctly at runtime

---

### User Story 4 - Module Compilation Achievement (Priority: P2)

As a compiler developer, I need the backend/ and reader/ modules to compile completely so that progress toward self-hosting is measurable and demonstrable.

**Why this priority**: These modules are critical components of the compiler. Complete compilation validates that fixes work on real code.

**Independent Test**: Can be tested by running Stage 1 compilation and verifying that all forms in backend/ and reader/ directories compile without errors.

**Acceptance Scenarios**:

1. **Given** all source files in src/clysm/backend/, **When** Stage 1 compilation runs, **Then** every form compiles successfully or is intentionally skipped
2. **Given** all source files in src/clysm/reader/, **When** Stage 1 compilation runs, **Then** every form compiles successfully or is intentionally skipped
3. **Given** a module that previously had failures, **When** recompiled after fixes, **Then** the compilation rate for that module reaches 100%

---

### Edge Cases

- What happens when a DEFUN has malformed lambda-list syntax?
- How does the system handle deeply nested &aux initialization expressions?
- What occurs when &key and &allow-other-keys interact with default values?
- How are recursive function definitions with complex lambda-lists handled?
- What happens when a DEFUN references undefined macros or special forms?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST log detailed error information for each DEFUN compilation failure, including function name, error type, and failing subform
- **FR-002**: System MUST generate a summary report grouping DEFUN failures by error pattern with occurrence counts
- **FR-003**: System MUST support &aux parameters in lambda-lists, correctly initializing auxiliary bindings in generated Wasm
- **FR-004**: System MUST support &allow-other-keys in lambda-lists for flexible keyword argument handling
- **FR-005**: System MUST compile all DEFUN forms in src/clysm/backend/ module without errors
- **FR-006**: System MUST compile all DEFUN forms in src/clysm/reader/ module without errors
- **FR-007**: System MUST reduce total DEFUN compilation failures to 15,000 or fewer
- **FR-008**: System MUST achieve an overall compilation rate of 35% or higher

### Key Entities

- **Error Log Entry**: Individual compilation failure with function name, error type, error message, source location, and failing subform context
- **Error Pattern Category**: Classification of similar errors with pattern identifier, description, occurrence count, and representative examples
- **Compilation Report**: Aggregate statistics including total forms, compiled count, skipped count, failed count, and per-pattern breakdown
- **Lambda-List Descriptor**: Parsed representation of DEFUN parameter list including required, optional, rest, key, aux, and allow-other-keys components

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Compilation rate increases from current baseline to at least 35%
- **SC-002**: DEFUN-specific compilation failures decrease from 18,997 to 15,000 or fewer
- **SC-003**: 100% of forms in backend/ module compile successfully or are intentionally marked as skipped
- **SC-004**: 100% of forms in reader/ module compile successfully or are intentionally marked as skipped
- **SC-005**: Error analysis report identifies the top 10 error patterns that account for at least 80% of failures
- **SC-006**: Each error log entry contains sufficient information to identify root cause without additional debugging

## Assumptions

- The current compilation baseline is approximately 14.26% (based on M2 milestone)
- DEFUN failures constitute approximately 90% of all compilation failures
- Error patterns are sufficiently regular that categorization is meaningful
- The backend/ and reader/ modules are representative test targets that exercise common DEFUN patterns
- Existing lambda-list support covers &optional, &rest, and &key but lacks &aux, &allow-other-keys, and &whole
- The Stage 1 report infrastructure (dist/stage1-report.json) can be extended to include detailed error information

## Dependencies

- Phase 13D M2 (Blocker Analysis) must be complete, providing baseline compilation statistics
- Phase 13D M3 (CLOS Primitives) should be complete for slot-value* and make-instance* support
- Existing Stage 1 generation infrastructure (build/stage1-complete.lisp) must be operational

## Scope Boundaries

**In Scope**:
- DEFUN compilation error logging and analysis
- Lambda-list feature extensions (&aux, &allow-other-keys)
- backend/ and reader/ module compilation
- Error pattern classification and reporting

**Out of Scope**:
- DEFCLASS, DEFMETHOD, or other definition form improvements (handled in separate milestones)
- Runtime behavior validation beyond basic compilation success
- Performance optimization of generated Wasm code
- Self-hosting execution (Stage 1 running and producing Stage 2)
