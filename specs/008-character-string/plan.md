# Implementation Plan: Common Lisp文字型と文字列操作

**Branch**: `008-character-string` | **Date**: 2025-12-23 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/008-character-string/spec.md`

## Summary

Common Lisp文字型（character）と文字列型（string）の実装。文字型はWasmGC i31refとして
Unicodeコードポイント（21ビット）を表現し、文字列型は既存の`$string`型（immutable i8 array）
を活用してUTF-8エンコーディングで実装する。リーダー拡張、39の組み込み関数、型述語を含む。

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - コンパイラ本体
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A（コンパイラはWasmバイナリを生成）
**Testing**: rove (BDD-style Lisp testing framework)
**Target Platform**: WebAssembly GC (wasmtime for validation)
**Project Type**: Single project - コンパイラ実装
**Performance Goals**: 文字アクセスO(n)許容（UTF-8）、比較O(n)
**Constraints**: i31ref（31ビット）に収まるUnicodeコードポイント、UTF-8エンコーディング
**Scale/Scope**: 39関数（文字10種 + 文字列29種）

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. WasmGC-First型システム | ✅ Pass | 文字=i31ref、文字列=既存$string (array i8) |
| II. Lispオブジェクト表現規約 | ✅ Pass | NILシングルトン維持、文字列は新型 |
| III. 関数・クロージャ実装戦略 | ✅ Pass | 高階関数なし（比較関数は直接実装） |
| IV. Wasm制御フロー活用 | ✅ Pass | ループにblock/loop使用 |
| V. シャローバインディング | N/A | 動的スコープ不使用 |
| VI. 段階的動的コンパイル | N/A | リテラルはコンパイル時処理 |
| VII. TDD（非交渉） | ✅ Required | テストファースト必須 |
| VIII. Nix-Firstワークフロー | ✅ Pass | 既存flake.nix使用 |

**Gate Result**: PASS - 全原則に準拠

## Project Structure

### Documentation (this feature)

```text
specs/008-character-string/
├── spec.md              # 機能仕様（完了）
├── plan.md              # 本ファイル
├── research.md          # Phase 0 調査結果
├── data-model.md        # Phase 1 型定義
├── quickstart.md        # Phase 1 クイックリファレンス
├── checklists/
│   └── requirements.md  # 品質チェックリスト（完了）
└── tasks.md             # Phase 2 タスク分解（別コマンド）
```

### Source Code (repository root)

```text
src/clysm/
├── reader/
│   ├── tokenizer.lisp   # 変更: #\x 文字リテラル追加
│   └── parser.lisp      # 変更: :character トークン処理
├── compiler/
│   └── codegen/
│       ├── gc-types.lisp     # 確認: $string型（既存）
│       └── func-section.lisp # 変更: 39関数追加
└── backend/
    └── wat-print.lisp   # 確認: 文字列リテラル出力

tests/
├── unit/
│   ├── tokenizer-test.lisp  # 追加: 文字リテラルテスト
│   └── character-test.lisp  # 新規: 文字関数単体テスト
└── integration/
    ├── character-test.lisp  # 新規: 文字E2Eテスト
    └── string-test.lisp     # 新規: 文字列E2Eテスト
```

**Structure Decision**: 既存のsrc/clysm構造を維持し、reader/tokenizerとcompiler/codegen/func-section.lispを拡張

## Complexity Tracking

> 憲法違反なし - このセクションは空

## Implementation Tiers

### Tier 1: 文字型基盤 (P1)

**スコープ**: FR-001〜FR-005（型、リテラル、char-code/code-char）

1. リーダー拡張: `#\x`, `#\Space`, `#\Newline`, `#\Tab`, `#\Return`
2. AST: `:character` リテラル型追加
3. コード生成: 文字→i31ref変換
4. `char-code`, `code-char` 実装

### Tier 2: 文字比較・変換 (P1)

**スコープ**: FR-006〜FR-010

1. 比較: `char=`, `char/=`, `char<`, `char>`, `char<=`, `char>=`
2. Case-insensitive: `char-equal`, `char-lessp`, `char-greaterp`, `char-not-lessp`, `char-not-greaterp`
3. ケース変換: `char-upcase`, `char-downcase`
4. 述語: `alpha-char-p`, `digit-char-p`, `alphanumericp`, `upper-case-p`, `lower-case-p`
5. 型述語: `characterp`

### Tier 3: 文字列基盤 (P1)

**スコープ**: FR-011〜FR-015

1. 確認: 既存`$string`型（array i8）
2. リーダー確認: `"..."` リテラル（既存）
3. エスケープ: `\n`, `\t`, `\\`, `\"` 確認
4. `length` 文字列対応（UTF-8文字数カウント）
5. `char`, `schar` 実装（UTF-8デコード）

### Tier 4: 文字列生成・比較 (P2)

**スコープ**: FR-016〜FR-019

1. `make-string` 実装
2. `string` 変換関数
3. 比較: `string=`, `string/=`, `string<`, `string>`, `string<=`, `string>=`
4. Case-insensitive: `string-equal`, `string-lessp`, `string-greaterp`, `string-not-lessp`, `string-not-greaterp`, `string-not-equal`

### Tier 5: 文字列変換・操作 (P2-P3)

**スコープ**: FR-020〜FR-023

1. ケース変換: `string-upcase`, `string-downcase`, `string-capitalize`
2. `subseq` 文字列対応
3. `concatenate` 'string対応
4. 型述語: `stringp`

## Key Technical Decisions

### 文字表現

```wat
;; 文字 = Unicodeコードポイントをi31refとしてエンコード
;; 範囲: 0x0〜0x10FFFF（21ビット、i31に収まる）
(ref.i31 codepoint)  ;; 文字→i31ref
(i31.get_s char_ref) ;; i31ref→コードポイント
```

### 文字列表現

```wat
;; 既存の$string型を使用
(type $string (array i8))  ;; UTF-8バイト配列

;; 文字アクセス: O(n)でUTF-8をデコード
;; length: UTF-8文字数（バイト数ではない）
```

### UTF-8デコードパターン

```
バイト1: 0xxxxxxx        → 1バイト文字（ASCII）
バイト1: 110xxxxx        → 2バイト文字
バイト1: 1110xxxx        → 3バイト文字
バイト1: 11110xxx        → 4バイト文字
```

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| UTF-8デコード複雑性 | 中 | Tier 3で段階的実装、ヘルパー関数抽出 |
| 文字列length性能 | 低 | O(n)許容済み、キャッシュ検討は将来 |
| サロゲートペア処理 | 低 | Unicodeスカラー値のみ（サロゲート除外）|

## References

- [ANSI Common Lisp - Characters](http://www.lispworks.com/documentation/HyperSpec/Body/t_ch.htm)
- [ANSI Common Lisp - Strings](http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm)
- [UTF-8 Encoding](https://en.wikipedia.org/wiki/UTF-8)
- 007-sequence-functions実装パターン
