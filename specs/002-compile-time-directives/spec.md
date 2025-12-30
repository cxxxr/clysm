# Feature Specification: Compile-Time Directive Skip Integration

**Feature Branch**: `002-compile-time-directives`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 13D-3: コンパイル時ディレクティブスキップを実装する。目標はin-package, defpackage, declaimをNIL返却（コード生成なし）として処理し、コンパイル時のみ意味を持つフォームでの不要なコンパイルエラーを解消する。これにより284+αの失敗フォームが解消され、コンパイル率が13.6%→18-20%に改善される見込み。Stage 1レポートでDEFPACKAGE失敗が0になることを検証基準とする。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Stage 1 Generator Handles Directive Forms (Priority: P1)

As a developer running Stage 1 generation, I want the compiler to properly skip compile-time directives (in-package, defpackage, declaim) so that they don't appear as compilation failures in the Stage 1 report.

**Why this priority**: The current Stage 1 generator counts 284 DEFPACKAGE forms as failures because it doesn't properly handle nil returns from the compiler. Fixing this has the highest impact on reported compilation rate.

**Independent Test**: Can be fully tested by running `sbcl --load build/stage1-complete.lisp` and verifying that the Stage 1 report shows 0 failures for DEFPACKAGE, IN-PACKAGE, and DECLAIM operators in the top_blockers list.

**Acceptance Scenarios**:

1. **Given** a source file with `(defpackage :foo (:use :cl))` at the top, **When** Stage 1 generation processes this form, **Then** the form is marked as "skipped" (not "failed") and DEFPACKAGE does not appear in top_blockers.
2. **Given** a source file with `(in-package :clysm)` at the top, **When** Stage 1 generation processes this form, **Then** the form is marked as "skipped" and does not increment the failed count.
3. **Given** a source file with `(declaim (optimize (speed 3)))`, **When** Stage 1 generation processes this form, **Then** the form is marked as "skipped" and does not appear as a blocker.

---

### User Story 2 - Stage 1 Report Distinguishes Skipped Forms (Priority: P2)

As a developer reviewing the Stage 1 report, I want to see compile-time directives counted separately as "skipped" so that I can accurately assess the true compilation success rate for runtime code.

**Why this priority**: Accurate reporting is essential for tracking bootstrap progress. Mixing directives with actual compilation failures distorts the metrics.

**Independent Test**: Can be tested by examining `dist/stage1-report.json` after generation and verifying it contains a "skipped" count separate from "compiled" and "failed".

**Acceptance Scenarios**:

1. **Given** the Stage 1 generation completes, **When** I examine the report JSON, **Then** I see a "skipped" field in the summary showing the count of directive forms.
2. **Given** a codebase with 284 DEFPACKAGE forms, **When** Stage 1 generation completes, **Then** the "skipped" count includes these 284 forms and "failed" count is reduced accordingly.
3. **Given** the Stage 1 report, **When** I check top_blockers, **Then** DEFPACKAGE, IN-PACKAGE, and DECLAIM do not appear in the list.

---

### User Story 3 - Compilation Rate Improvement Verification (Priority: P3)

As a project maintainer, I want to verify that directive skip integration improves the compilation rate from approximately 13.6% to 18-20% as expected.

**Why this priority**: This validates the expected impact of the feature and ensures the bootstrap pipeline is progressing.

**Independent Test**: Can be tested by comparing compilation rates before and after the change, calculating (compiled / (total - skipped)) * 100.

**Acceptance Scenarios**:

1. **Given** Stage 1 generation with directive skip enabled, **When** I calculate the adjusted compilation rate, **Then** the rate is at least 18% (compiled forms / total non-directive forms).
2. **Given** the previous Stage 1 report showed 13.6% coverage, **When** I run the updated Stage 1 generation, **Then** the reported coverage increases by at least 4 percentage points.

---

### Edge Cases

- What happens when a malformed directive form is encountered (e.g., `(in-package)` with no argument)? The directive evaluation signals a compile-time error, which is recorded as a failure (not skipped).
- How are directives in non-toplevel positions handled? They follow standard CL semantics and may compile differently.
- What happens if a directive evaluation fails (e.g., defpackage for a package that already exists)? The error is captured and the form is marked as failed with an error message.
- How are proclaim forms handled vs declaim? Both are recognized as directives and skipped.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Stage 1 generator MUST distinguish between forms that return nil (directives) and forms that fail compilation with errors.
- **FR-002**: When `compile-to-wasm` returns nil, the form MUST be classified as "skipped" rather than "failed".
- **FR-003**: The `classify-forms` function MUST track a separate "skipped" count for directive forms.
- **FR-004**: The Stage 1 report JSON MUST include a "skipped" field in the summary section.
- **FR-005**: Forms classified as "skipped" MUST NOT appear in the top_blockers analysis.
- **FR-006**: The coverage percentage calculation MUST use the formula: compiled / (total - skipped) * 100.
- **FR-007**: The following operators MUST be recognized as directives: in-package, defpackage, declaim, proclaim.
- **FR-008**: Directive evaluation errors MUST still be captured and reported as failures (not skipped).

### Key Entities

- **Compilation Result**: A record of a form's compilation outcome, now including three states: compiled, failed, or skipped.
- **Stage 1 Report**: The JSON report generated after Stage 1 compilation, containing summary statistics and per-module breakdowns.
- **Directive Form**: One of in-package, defpackage, declaim, or proclaim - forms that return nil from compile-to-wasm.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: DEFPACKAGE operator has 0 entries in the top_blockers list after Stage 1 generation.
- **SC-002**: IN-PACKAGE and DECLAIM operators have 0 entries in the top_blockers list.
- **SC-003**: The Stage 1 report summary includes a "skipped" count of at least 284 (the known DEFPACKAGE count).
- **SC-004**: The compilation coverage percentage increases from 13.6% to at least 18%.
- **SC-005**: All existing tests pass without regression.
- **SC-006**: Stage 1 Wasm binary continues to pass `wasm-tools validate`.

## Assumptions

- The existing directive handling infrastructure in `directive.lisp` correctly identifies and evaluates compile-time directives.
- The `compile-to-wasm` function returns nil for properly handled directives.
- The current 284 DEFPACKAGE failures are due to the nil return handling issue in `test-form-compilation`.
- The Stage 1 report format can be extended to include a "skipped" field without breaking consumers.
