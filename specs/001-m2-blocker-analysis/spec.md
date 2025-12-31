# Feature Specification: Phase 13D Milestone M2 - Blocker Analysis and Compilation Rate Improvement

**Feature Branch**: `001-m2-blocker-analysis`
**Created**: 2025-12-31
**Status**: Draft
**Input**: User description: "Phase 13D 検証マイルストーンM2: Stage 1レポート再生成と残存ブロッカー分析を実行する。目標はコンパイル率14%→25%+。まずsbcl --load build/stage1-complete.lispでレポートを再生成し、DEFUNコンパイル失敗の具体的な原因を特定。TAGBODY/GO、複雑なlambda-list、未登録プリミティブ等の問題を洗い出し、優先度順に修正する。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Regenerate Stage 1 Report (Priority: P1)

As a compiler developer, I need to regenerate the Stage 1 compilation report to get current statistics and identify remaining blockers after recent feature additions.

**Why this priority**: The report provides the baseline data needed for all subsequent analysis and fixes. Without an updated report, we cannot accurately identify what issues remain.

**Independent Test**: Can be fully tested by running the Stage 1 generation script and verifying the report output contains per-module statistics, blocker counts, and compilation rates.

**Acceptance Scenarios**:

1. **Given** the current codebase with recent feature additions, **When** the Stage 1 generation script is executed, **Then** a new report is generated with updated compilation statistics
2. **Given** Stage 1 generation completes, **When** the report is examined, **Then** it contains module-by-module breakdown of compiled/failed/skipped forms
3. **Given** the generated report, **When** the current compilation rate is calculated, **Then** the baseline rate is documented for comparison

---

### User Story 2 - Analyze DEFUN Compilation Failures (Priority: P1)

As a compiler developer, I need to identify the specific causes of DEFUN compilation failures so I can prioritize which blockers to fix first for maximum compilation rate improvement.

**Why this priority**: DEFUN forms represent the bulk of compilable code. Understanding why they fail is essential to improving the compilation rate.

**Independent Test**: Can be fully tested by extracting all DEFUN compilation errors from the report and categorizing them by failure type ([DEFSTRUCT](resources/HyperSpec/Body/m_defstr.htm), [DEFMACRO](resources/HyperSpec/Body/m_defmac.htm), [DEFINE-CONDITION](resources/HyperSpec/Body/m_defi_5.htm), [DEFVAR](resources/HyperSpec/Body/m_defvar.htm), etc.).

**Acceptance Scenarios**:

1. **Given** the Stage 1 report, **When** DEFUN failures are analyzed, **Then** each failure is categorized by root cause
2. **Given** categorized failures, **When** sorted by frequency, **Then** the top blockers by impact are identified
3. **Given** failure analysis, **When** dependencies between blockers are examined, **Then** a priority order for fixes is established

---

### User Story 3 - Fix High-Impact Blockers (Priority: P2)

As a compiler developer, I need to implement fixes for the highest-impact blockers so that the compilation rate increases from 14% toward the 25%+ target.

**Why this priority**: This is the core work that delivers the goal, but depends on completing the analysis first to ensure fixes are prioritized correctly.

**Independent Test**: Each blocker fix can be tested independently by re-running Stage 1 generation and verifying the compilation rate increases and specific previously-failing forms now compile successfully.

**Acceptance Scenarios**:

1. **Given** a prioritized blocker (e.g., [DEFSTRUCT](resources/HyperSpec/Body/m_defstr.htm) or [DEFINE-CONDITION](resources/HyperSpec/Body/m_defi_5.htm)), **When** the fix is implemented, **Then** forms using that construct compile successfully
2. **Given** a fix is applied, **When** Stage 1 is regenerated, **Then** the compilation rate increases measurably
3. **Given** multiple fixes are applied, **When** the final compilation rate is measured, **Then** it reaches at least 25%

---

### User Story 4 - Validate Generated Output (Priority: P2)

As a compiler developer, I need to ensure that newly-compiled forms produce valid output that passes validation.

**Why this priority**: Compilation rate improvements are meaningless if the generated output is invalid. This ensures quality alongside quantity.

**Independent Test**: Can be tested by running the validation tool against the generated Stage 1 output after each fix.

**Acceptance Scenarios**:

1. **Given** Stage 1 output is generated after fixes, **When** validation is run, **Then** the output passes validation with no errors
2. **Given** newly-compiled forms, **When** their generated code is examined, **Then** it follows correct instruction patterns

---

### Edge Cases

- What happens when a blocker fix causes regressions in previously-working forms?
- How does the system handle circular dependencies between blockers (e.g., a fix requires another unfixed feature)?
- What happens when a form uses multiple unsupported features simultaneously?
- How are partial fixes handled when a blocker cannot be fully resolved in this milestone?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST regenerate the Stage 1 compilation report with current statistics
- **FR-002**: Report MUST categorize all compilation failures by root cause type
- **FR-003**: Report MUST identify the top blockers ranked by impact (number of affected forms)
- **FR-004**: System MUST implement [DEFSTRUCT](resources/HyperSpec/Body/m_defstr.htm) → [DEFCLASS](resources/HyperSpec/Body/m_defcla.htm) expansion for Stage 1 compilation (1,953 forms)
- **FR-005**: System MUST implement [DEFINE-CONDITION](resources/HyperSpec/Body/m_defi_5.htm) → DEFCLASS expansion (302 forms)
- **FR-006**: System MUST skip [DEFMACRO](resources/HyperSpec/Body/m_defmac.htm) forms during Stage 1 compilation (host-expanded, 646 forms)
- **FR-007**: System MUST complete [DEFVAR](resources/HyperSpec/Body/m_defvar.htm) global variable support (133 forms)
- **FR-008**: Each fix MUST be validated by successful re-compilation of affected forms
- **FR-009**: Generated output MUST pass validation after applying fixes
- **FR-010**: Compilation rate MUST increase from baseline 14% to at least 25%

### Key Entities

- **Stage 1 Report**: Compilation statistics document containing per-module breakdown, blocker analysis, and compilation rates
- **Blocker**: A missing feature or bug that prevents one or more forms from compiling successfully
- **Compilation Form**: An individual Lisp form (e.g., DEFUN, DEFMACRO) that the compiler attempts to process
- **Primitive**: A built-in operation that the compiler must recognize and translate

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Compilation rate increases from 14% to at least 25% (11+ percentage point improvement)
- **SC-002**: All DEFUN compilation failures are categorized with identified root causes
- **SC-003**: Top 5 blockers by impact are identified and documented
- **SC-004**: At least 3 high-impact blockers are fixed in this milestone
- **SC-005**: Generated output continues to pass validation after all fixes
- **SC-006**: No regressions in previously-compiling forms after fixes are applied

## Assumptions

- The current Stage 1 generation infrastructure is functional and produces accurate reports
- [TAGBODY](resources/HyperSpec/Body/s_tagbod.htm)/[GO](resources/HyperSpec/Body/s_go.htm) control flow is already implemented (verified in research phase - not a blocker)
- Complex lambda-list handling is partially implemented and not a top-5 blocker (deferred to future milestone)
- Blocker categories (DEFSTRUCT, DEFMACRO, DEFINE-CONDITION, DEFVAR) cover the majority of fixable failures
- Fixing high-impact blockers will yield proportional compilation rate improvements
- Validation tools are available and provide reliable feedback
- The 25% target is achievable within the scope of addressing the identified blocker categories
