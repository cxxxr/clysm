# Tasks: Closure Type Index Fix

**Input**: Design documents from `/specs/003-fix-closure-types/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md

**Tests**: 既存の closure-test.lisp をそのまま使用。新規テスト作成は不要（TDD: Red → Green サイクル）。

**Organization**: この修正はバグ修正であり、単一の変更で全ユーザーストーリーが解決される。

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

```text
src/clysm/compiler/codegen/gc-types.lisp    # 修正対象
tests/integration/closure-test.lisp          # 検証対象
tests/integration/special-var-test.lisp      # リグレッション確認
```

---

## Phase 1: Setup

**Purpose**: 現状確認と修正準備

- [ ] T001 現在の closure テスト失敗を確認: `sbcl --eval '(asdf:test-system :clysm)'` を実行し、closure テスト 16件失敗を記録
- [ ] T002 手動検証で現在の動作を確認: `(funcall (lambda () 42))` が 1 を返すことを確認

---

## Phase 2: Foundational (Core Fix)

**Purpose**: 型インデックス定数の修正（全ユーザーストーリーに影響）

**⚠️ CRITICAL**: この単一の修正で US1/US2/US3 が全て解決される

- [ ] T003 +type-binding-frame+ 定数を削除またはコメントアウトする in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T004 +type-func-0+ を 9 から 8 に変更する in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T005 +type-func-1+ を 10 から 9 に変更する in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T006 +type-func-2+ を 11 から 10 に変更する in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T007 +type-func-3+ を 12 から 11 に変更する in src/clysm/compiler/codegen/gc-types.lisp
- [ ] T008 +type-func-n+ を 13 から 12 に変更する in src/clysm/compiler/codegen/gc-types.lisp

**Checkpoint**: 型インデックス定数が compiler.lisp の実際の型配置と一致

---

## Phase 3: User Story 1 - Basic Lambda Execution (Priority: P1) 🎯 MVP

**Goal**: 引数なし/あり lambda の funcall が正しく動作する

**Independent Test**: `(funcall (lambda () 42))` が 42 を返す

### Verification for User Story 1

- [ ] T009 [US1] 手動検証: `(funcall (lambda () 42))` が 42 を返すことを確認
- [ ] T010 [US1] 手動検証: `(funcall (lambda (x) x) 42)` が 42 を返すことを確認
- [ ] T011 [US1] 手動検証: `(funcall (lambda (a b) (+ a b)) 10 20)` が 30 を返すことを確認
- [ ] T012 [US1] テスト実行: closure-test.lisp の test-lambda-* テストが全て成功することを確認

**Checkpoint**: Basic Lambda Execution が動作

---

## Phase 4: User Story 2 - Closure Variable Capture (Priority: P1)

**Goal**: let で束縛した変数を lambda 内からキャプチャできる

**Independent Test**: `(let ((x 10)) (funcall (lambda () x)))` が 10 を返す

### Verification for User Story 2

- [ ] T013 [US2] 手動検証: `(let ((x 10)) (funcall (lambda () x)))` が 10 を返すことを確認
- [ ] T014 [US2] 手動検証: `(let ((x 10) (y 20)) (funcall (lambda () (+ x y))))` が 30 を返すことを確認
- [ ] T015 [US2] テスト実行: closure-test.lisp の test-*-capture テストが全て成功することを確認

**Checkpoint**: Closure Variable Capture が動作

---

## Phase 5: User Story 3 - Local Function Definitions (Priority: P2)

**Goal**: flet/labels でローカル関数を定義・呼び出しできる

**Independent Test**: `(flet ((f (x) (+ x 1))) (f 10))` が 11 を返す

### Verification for User Story 3

- [ ] T016 [US3] 手動検証: `(flet ((f (x) (+ x 1))) (f 10))` が 11 を返すことを確認
- [ ] T017 [US3] 手動検証: `(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5))` が 120 を返すことを確認
- [ ] T018 [US3] テスト実行: closure-test.lisp の test-flet-* と test-labels-* テストが全て成功することを確認

**Checkpoint**: Local Function Definitions が動作

---

## Phase 6: Polish & Regression Check

**Purpose**: リグレッション確認と最終検証

- [ ] T019 テスト実行: special-var-test.lisp の全テストが引き続き成功することを確認
- [ ] T020 テスト実行: 全テストスイート `sbcl --eval '(asdf:test-system :clysm)'` を実行
- [ ] T021 成功基準確認: closure テスト失敗数が 0 になっていることを確認
- [ ] T022 成功基準確認: closure/tco/control-flow 以外のテストでリグレッションがないことを確認
- [ ] T023 仕様書更新: specs/003-fix-closure-types/spec.md の Status を Complete に更新

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - 現状確認
- **Foundational (Phase 2)**: Depends on Setup - 型インデックス修正
- **US1 (Phase 3)**: Depends on Foundational - 検証のみ
- **US2 (Phase 4)**: Depends on Foundational - 検証のみ
- **US3 (Phase 5)**: Depends on Foundational - 検証のみ
- **Polish (Phase 6)**: Depends on all US verification

### User Story Dependencies

- **User Story 1 (P1)**: Foundational 完了後に検証可能
- **User Story 2 (P1)**: Foundational 完了後に検証可能（US1 と並行可）
- **User Story 3 (P2)**: Foundational 完了後に検証可能（US1/US2 と並行可）

### Task Dependencies within Foundational

```
T003 → T004 → T005 → T006 → T007 → T008
```

実質的には T003-T008 は単一ファイルの連続編集のため、順次実行。

### Parallel Opportunities

- T009-T012 (US1 検証) は Foundational 完了後に並行実行可能
- T013-T015 (US2 検証) は Foundational 完了後に並行実行可能
- T016-T018 (US3 検証) は Foundational 完了後に並行実行可能
- 全 US 検証は互いに独立しており並行可能

---

## Parallel Example: Verification Phase

```bash
# Launch all US verifications together after Foundational:
Task: "T009-T012 [US1] Lambda execution verification"
Task: "T013-T015 [US2] Closure capture verification"
Task: "T016-T018 [US3] Local function verification"
```

---

## Implementation Strategy

### Quick Fix Approach

この修正は非常に局所的であり、以下の順序で完了可能:

1. **T001-T002**: 現状確認（Red 状態の記録）
2. **T003-T008**: gc-types.lisp の定数修正（単一ファイル編集）
3. **T009-T018**: 全 US の検証（Green 状態の確認）
4. **T019-T023**: リグレッション確認と完了

### Estimated Effort

- **Phase 1 (Setup)**: 5 分
- **Phase 2 (Foundational)**: 5 分（6行の定数値変更）
- **Phase 3-5 (Verification)**: 15 分
- **Phase 6 (Polish)**: 10 分
- **Total**: 約 35 分

---

## Notes

- この修正は TDD の "Red → Green" サイクルに従う
- 新規テスト作成は不要（既存の closure-test.lisp で検証）
- 単一ファイル (gc-types.lisp) の定数値変更のみ
- リグレッションリスクは低い（型インデックスは compile-funcall でのみ参照）
- Commit: Phase 2 完了後に 1 コミットで完了
