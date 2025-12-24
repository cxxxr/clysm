# Implementation Plan: ANSI Common Lispパッケージシステム

**Branch**: `013-package-system` | **Date**: 2025-12-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/013-package-system/spec.md`

## Summary

ANSI Common Lisp準拠のパッケージシステムを実装する。既存の基本的なパッケージ実装（`reader/package.lisp`）を拡張し、完全なパッケージAPI（defpackage, export, import, use-package等）、標準パッケージ（CL, CL-USER, KEYWORD）、およびリーダーでのパッケージ修飾子（`cl:car`, `pkg::internal`, `:keyword`）サポートを追加する。

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - コンパイラ実装言語
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A（インメモリハッシュテーブル）
**Testing**: rove (TDD必須 - Constitution VII)
**Target Platform**: WebAssembly GC (WasmGC)
**Project Type**: single（Common Lispコンパイラ）
**Performance Goals**: シンボル検索 O(1)（ハッシュテーブル）
**Constraints**: 既存リーダー（tokenizer.lisp, parser.lisp）との統合が必要
**Scale/Scope**: 32関数/マクロ実装（FR-001〜FR-032）

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム設計 | ✅ PASS | パッケージ・シンボルは既存のstructベース表現を活用 |
| II. Lispオブジェクト表現規約 | ✅ PASS | NIL/T はCL:NIL/CL:Tとして扱う（Edge Casesに明記） |
| III. 関数・クロージャ実装戦略 | ✅ N/A | パッケージ関数は通常の関数として実装 |
| IV. Wasm制御フロー活用 | ✅ N/A | パッケージシステムは制御フローに依存しない |
| V. シャローバインディング | ✅ PASS | `*package*` はスペシャル変数として既存機構を使用 |
| VI. 段階的動的コンパイル | ✅ N/A | パッケージシステムはインタプリタ/JIT両方で動作 |
| VII. TDD（非交渉） | ✅ REQUIRED | 全機能にテストファースト必須 |
| VIII. Nix-Firstワークフロー | ✅ PASS | 既存のflake.nixを使用 |

**Gate Result**: ✅ PASS - 全原則に準拠

## Project Structure

### Documentation (this feature)

```text
specs/013-package-system/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (internal API contracts)
│   └── package-api.md
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
src/clysm/
├── reader/
│   ├── package.lisp      # [EXTEND] 既存パッケージ実装を拡張
│   ├── tokenizer.lisp    # [MODIFY] パッケージ修飾子のトークン化
│   └── parser.lisp       # [MODIFY] パッケージ修飾付きシンボルのパース
├── runtime/
│   ├── package-init.lisp # [NEW] 標準パッケージ初期化
│   └── objects.lisp      # [EXTEND] シンボル構造体にパッケージ参照追加
└── lib/
    └── package-macros.lisp # [NEW] defpackage, in-package マクロ

tests/
├── unit/
│   ├── package-test.lisp         # [EXTEND] 既存テストを拡張
│   └── tokenizer-package-test.lisp # [NEW] パッケージ修飾子トークン化テスト
└── integration/
    └── package-integration-test.lisp # [NEW] パッケージ統合テスト
```

**Structure Decision**: 既存の`reader/package.lisp`を拡張し、新規ファイル（package-init.lisp, package-macros.lisp）を追加する単一プロジェクト構成を維持。

## Complexity Tracking

> No constitution violations requiring justification.

| Item | Justification | Alternative Rejected |
|------|---------------|---------------------|
| N/A | - | - |
