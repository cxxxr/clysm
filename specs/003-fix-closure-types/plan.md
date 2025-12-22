# Implementation Plan: Closure Type Index Fix

**Branch**: `003-fix-closure-types` | **Date**: 2025-12-23 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/003-fix-closure-types/spec.md`

## Summary

gc-types.lisp で定義された型インデックス定数（+type-func-0+ 等）と compiler.lisp の emit-type-section で実際に生成される型の配置が不整合のため、closure テストが全て失敗している。gc-types.lisp の定数値を compiler.lisp の実際の型配置に合わせることで修正する。

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compiler generates Wasm binaries)
**Testing**: rove (Common Lisp test framework)
**Target Platform**: WasmGC (WebAssembly with GC extension)
**Project Type**: single (compiler project)
**Performance Goals**: N/A (bug fix, no new performance requirements)
**Constraints**: 既存テストのリグレッション防止必須
**Scale/Scope**: 2ファイルの定数値変更

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | 型インデックスの整合性修正はWasmGC仕様準拠を維持 |
| II. Lispオブジェクト表現規約 | ✅ PASS | NIL/UNBOUND表現に影響なし |
| III. 関数・クロージャ実装戦略 | ✅ PASS | クロージャ構造体の設計自体は変更なし、型インデックスのみ修正 |
| IV. Wasm制御フロー活用 | ✅ PASS | 影響なし |
| V. シャローバインディング | ✅ PASS | 特別変数テストは引き続き成功すること |
| VI. 段階的動的コンパイル | ✅ PASS | 影響なし |
| VII. TDD（非交渉） | ✅ PASS | 既存テストで検証、Red-Greenサイクル適用 |
| VIII. Nix-Firstワークフロー | ✅ PASS | 影響なし |

**Gate Result**: PASS - 全原則に準拠

## Project Structure

### Documentation (this feature)

```text
specs/003-fix-closure-types/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/
├── clysm/
│   ├── compiler/
│   │   ├── codegen/
│   │   │   ├── gc-types.lisp    # ← 修正対象: 型インデックス定数
│   │   │   └── func-section.lisp
│   │   └── compiler.lisp        # ← 参照: emit-type-section
│   └── ...
└── ...

tests/
├── integration/
│   ├── closure-test.lisp        # ← 検証対象: 現在16件失敗
│   └── special-var-test.lisp    # ← リグレッション確認
└── ...
```

**Structure Decision**: 既存の単一プロジェクト構造を維持。修正対象は `src/clysm/compiler/codegen/gc-types.lisp` の定数値のみ。

## Complexity Tracking

> Constitution Check に違反なし。複雑性追加の正当化は不要。

## Phase 0: Research

### 型インデックス配置の現状分析

**問題の詳細**:

gc-types.lisp の定義:
```lisp
(defconstant +type-binding-frame+ 8)  ; 未使用（将来のUS5用）
(defconstant +type-func-0+ 9)
(defconstant +type-func-1+ 10)
(defconstant +type-func-2+ 11)
(defconstant +type-func-3+ 12)
(defconstant +type-func-n+ 13)
```

compiler.lisp emit-type-section の実際の生成:
```
Type 0-5: GC struct types ($nil, $unbound, $cons, $symbol, $string, $closure)
Type 6-7: Reserved (empty struct placeholders)
Type 8: func_0 (closure + 0 args)
Type 9: func_1 (closure + 1 arg)
Type 10: func_2 (closure + 2 args)
Type 11: func_3 (closure + 3 args)
Type 12: func_N (variadic)
```

**影響分析**:
- `compile-funcall` が `+type-func-0+` (=9) で call_ref を生成
- 実際の lambda 関数は type 8 で定義される
- 結果: 型不整合により wasmtime が予期しない動作

### 修正方針の決定

**Decision**: gc-types.lisp の定数を compiler.lisp の実際の配置に合わせる

**Rationale**:
1. compiler.lisp の型セクション生成ロジックは複雑で変更リスクが高い
2. gc-types.lisp の定数変更は局所的で影響範囲が明確
3. 既存のテスト（特別変数等）への影響を最小化

**Alternatives considered**:
1. compiler.lisp を変更して binding_frame 型を type 8 に追加 → 複雑性増加、リスク高
2. 両ファイルを同時に大幅改修 → 不必要な複雑性

### 修正内容

gc-types.lisp の変更:
```lisp
;; Before
(defconstant +type-binding-frame+ 8)
(defconstant +type-func-0+ 9)
(defconstant +type-func-1+ 10)
(defconstant +type-func-2+ 11)
(defconstant +type-func-3+ 12)
(defconstant +type-func-n+ 13)

;; After
;; +type-binding-frame+ は削除または将来のために保持（コメントアウト）
(defconstant +type-func-0+ 8)
(defconstant +type-func-1+ 9)
(defconstant +type-func-2+ 10)
(defconstant +type-func-3+ 11)
(defconstant +type-func-n+ 12)
```

## Phase 1: Design

### Data Model

この修正は既存のデータモデルに変更を加えない。型インデックス定数の値のみを修正する。

詳細は [data-model.md](./data-model.md) を参照。

### Contracts

API/インターフェースの変更なし。内部定数値の修正のみ。

### Quickstart

[quickstart.md](./quickstart.md) を参照。

## Verification Plan

### Pre-fix Verification
1. 現在の closure テスト失敗を確認: `sbcl --eval '(asdf:test-system :clysm)'`
2. 失敗テスト数を記録（期待: closure テスト 16件失敗）

### Post-fix Verification
1. gc-types.lisp の定数を修正
2. テスト実行: `sbcl --eval '(asdf:test-system :clysm)'`
3. SC-001: closure-test.lisp の全テスト成功を確認
4. SC-002: special-var-test.lisp のテスト維持を確認
5. SC-005: その他のテストでリグレッションなしを確認

### Manual Verification
```bash
sbcl --eval '(ql:quickload :clysm)' \
     --eval '(print (clysm/tests/helpers:compile-and-run (quote (funcall (lambda () 42)))))' \
     --quit
# 期待: 42 (現在は 1)
```
