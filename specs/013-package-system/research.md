# Research: ANSI Common Lispパッケージシステム

**Date**: 2025-12-24
**Feature**: 013-package-system

## 1. 既存実装の分析

### 現状

`src/clysm/reader/package.lisp` に基本的なパッケージ実装が存在：

- **完了済み機能**:
  - `make-package*`: パッケージ作成（名前、ニックネーム）
  - `find-package*`: パッケージ検索
  - `intern-symbol`: シンボルインターン
  - `find-symbol*`: シンボル検索
  - 標準パッケージ（COMMON-LISP, KEYWORD, CL-USER）の初期化

- **不足機能**（本実装で追加）:
  - export/import/use-package 機構
  - defpackage/in-package マクロ
  - パッケージ修飾子のリーダー対応
  - 外部/内部シンボルの区別
  - 名前衝突検出・解決

### 決定事項

**Decision**: 既存の `reader/package.lisp` を拡張する
**Rationale**: ゼロから作り直すより、既存のハッシュテーブルベース実装を活用する方が効率的
**Alternatives Rejected**:
- 新規パッケージシステム作成 → コード重複、既存テスト破壊のリスク

## 2. パッケージ修飾子のトークン化戦略

### ANSI CL仕様

```
pkg:symbol    - 外部シンボル参照（エクスポート必須）
pkg::symbol   - 内部シンボル参照（エクスポート不要）
:keyword      - キーワードシンボル
```

### 実装アプローチ

**Decision**: トークナイザーで `:` を特別扱いし、パッケージ修飾付きシンボルトークンを生成
**Rationale**:
- 現在 `:` はキーワード開始としてのみ処理されている
- `read-number-or-symbol` を拡張してコロン検出を追加

**トークン形式**:
```lisp
;; 現状
(:symbol "FOO")
(:keyword "BAR")

;; 拡張後
(:qualified-symbol :package "PKG" :name "FOO" :external t)  ; pkg:foo
(:qualified-symbol :package "PKG" :name "FOO" :external nil) ; pkg::foo
(:keyword "BAR")  ; :bar（既存形式を維持）
```

**Alternatives Rejected**:
- パーサーでコロン検出 → トークン境界が不明確になる

## 3. パッケージ構造の拡張

### 現状構造

```lisp
(:name <name> :symbols <hash-table> :nicknames <list>)
```

### 拡張構造

```lisp
(:name <name>
 :nicknames <list>
 :internal-symbols <hash-table>   ; 内部シンボル
 :external-symbols <hash-table>   ; エクスポートシンボル
 :use-list <package-list>         ; useしているパッケージ
 :used-by-list <package-list>     ; useされているパッケージ
 :shadowing-symbols <list>)       ; シャドウイングシンボル
```

**Decision**: 既存の `:symbols` を `:internal-symbols` にリネームし、`:external-symbols` を追加
**Rationale**: ANSI CLの外部/内部シンボル区別に必須
**Alternatives Rejected**:
- 単一テーブルにフラグ追加 → シンボル検索時のオーバーヘッド

## 4. シンボル検索アルゴリズム

### ANSI CL仕様に基づく検索順序

1. パッケージの外部シンボルテーブル
2. パッケージの内部シンボルテーブル
3. use-listの各パッケージの外部シンボルテーブル

### find-symbol の返り値

```lisp
(values symbol status)
;; status: :internal, :external, :inherited, nil
```

**Decision**: 多値返却でステータスを返す
**Rationale**: ANSI CL仕様準拠
**Alternatives Rejected**:
- 単一値（シンボルのみ） → ステータス情報が失われる

## 5. 名前衝突検出

### 衝突条件

- `use-package` 時: use対象パッケージの外部シンボルと同名シンボルが現パッケージにある場合
- `import` 時: 同名の異なるシンボルが既に存在する場合
- `export` 時: useされているパッケージに同名シンボルがある場合

### 解決手段

1. `shadow`: 現パッケージにシャドウイングシンボルを作成
2. `shadowing-import`: 他パッケージのシンボルをシャドウイングとしてインポート

**Decision**: 衝突時は `package-error` コンディションをシグナル
**Rationale**: ANSI CL仕様準拠、リスタート（`resolve-conflict`, `shadowing-import` など）は将来対応
**Alternatives Rejected**:
- 暗黙的解決 → 予期しない動作の原因

## 6. 標準パッケージの初期化

### COMMON-LISP パッケージ

**Decision**: 実装済み関数・マクロのシンボルのみをエクスポート
**Rationale**: 未実装関数への参照は未束縛シンボルとなる（エラーではない）

### KEYWORD パッケージ

**Decision**: 全シンボルを自動エクスポート、自己評価
**Rationale**: ANSI CL仕様

### COMMON-LISP-USER パッケージ

**Decision**: `(:use :cl)` 付きで初期化、`*package*` の初期値
**Rationale**: ANSI CL仕様

## 7. リーダー統合

### Tokenizer変更

- `read-number-or-symbol` を拡張
- シンボル文字読み取り中にコロンを検出
- コロン位置でパッケージ名とシンボル名を分割

### Parser変更

- `:qualified-symbol` トークンを処理
- パッケージ検索・シンボル参照解決を実行
- 外部シンボル参照時のエクスポートチェック

**Decision**: パース時にパッケージ解決を行う（遅延しない）
**Rationale**: エラー報告の明確化、デバッグ容易性

## 8. テスト戦略

### 単体テスト

1. パッケージ作成・検索
2. シンボルインターン・検索
3. export/import/use-package
4. 名前衝突検出
5. トークナイザー（パッケージ修飾子）

### 統合テスト

1. defpackage → in-package → シンボル読み込み
2. 複数パッケージ間のシンボル共有
3. エラーケース（未エクスポートシンボルへのアクセス等）

**Decision**: TDD必須（Constitution VII）
**Rationale**: Red-Green-Refactor サイクルで品質保証
