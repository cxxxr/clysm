# Implementation Plan: Common Lisp Sequence Functions

**Branch**: `007-sequence-functions` | **Date**: 2025-12-23 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/007-sequence-functions/spec.md`

## Summary

Phase 8aとして、Common Lisp標準シーケンス関数をClysmコンパイラに実装する。
既存のcons/car/cdr/list基盤を活用し、Tier 1-3の20関数を追加する。

実装アプローチ：
- 基本関数（length, append, reverse等）はコンパイラ組み込みとして実装
- 高階関数（mapcar, reduce等）は既存のfuncall/クロージャ基盤を活用
- TDDサイクルに従い、テスト先行で各関数を実装

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - コンパイラ本体
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (コンパイラはWasmバイナリを生成)
**Testing**: rove (integration tests via compile-and-run pattern)
**Target Platform**: WasmGC (wasmtime for validation)
**Project Type**: single - compiler project
**Performance Goals**: 1000要素リストに対するmapcarが1秒以内
**Constraints**: ANSI Common Lisp仕様準拠、wasm-tools validate通過
**Scale/Scope**: 20関数 (Tier 1-3必須) + 9関数 (Tier 4オプション)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム | PASS | anyref/i31refを使用、cons structを活用 |
| II. Lispオブジェクト表現 | PASS | NILはシングルトン、既存list基盤を継続使用 |
| III. 関数・クロージャ実装 | PASS | mapcar等は既存closure構造体を使用 |
| IV. Wasm制御フロー | PASS | 末尾再帰可能な関数はreturn_call対応 |
| V. シャローバインディング | N/A | 動的スコープ不使用 |
| VI. 段階的動的コンパイル | N/A | コンパイル時のみ |
| VII. TDD（非交渉） | PASS | tests/integration/sequence-test.lisp作成 |
| VIII. Nix-Firstワークフロー | PASS | nix flake check必須 |

**GATE RESULT**: PASS - 全原則に準拠、Phase 0に進行可能

## Project Structure

### Documentation (this feature)

```text
specs/007-sequence-functions/
├── spec.md              # Feature specification (完了)
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (API定義)
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── compiler/
│   └── codegen/
│       └── func-section.lisp    # 既存: compile-* 関数を追加
├── lib/
│   └── sequences.lisp           # 新規: 複雑な関数のLisp実装
└── runtime/
    └── objects.lisp             # 既存: 必要に応じて型追加

tests/
├── integration/
│   ├── list-test.lisp           # 既存: cons/car/cdr テスト
│   └── sequence-test.lisp       # 新規: シーケンス関数テスト
└── unit/
    └── sequence-unit-test.lisp  # 新規: 個別関数の単体テスト
```

**Structure Decision**: 既存のsingle projectレイアウトを維持。
シーケンス関数はfunc-section.lispへの追加と、必要に応じてlib/sequences.lispでの
Lisp実装を併用する。

## Complexity Tracking

> **No violations - complexity tracking not required**

本機能は既存アーキテクチャを拡張するものであり、新たな複雑性の導入はない。
