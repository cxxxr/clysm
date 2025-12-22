# Research: Closure Type Index Fix

**Date**: 2025-12-23
**Branch**: 003-fix-closure-types

## 問題の根本原因

### 発見経緯

closure テストが全て失敗し、結果が常に `1` を返す現象を調査。
`(funcall (lambda () 42))` の期待値は `42` だが、実際は `1` が返る。

### Wasm 逆アセンブル結果

```wat
;; 生成された Wasm モジュールの型セクション
(type (;8;) (func (param anyref) (result anyref)))          ;; func_0
(type (;9;) (func (param anyref anyref) (result anyref)))   ;; func_1
...

;; main 関数内の funcall コンパイル結果
call_ref 9  ;; ← 問題: type 9 (func_1) を参照しているが、lambda は type 8

;; lambda 関数の定義
(func (;1;) (type 8) (param anyref) (result anyref)  ;; type 8 で定義
  i32.const 42
  ref.i31
)
```

### 型インデックス不整合の詳細

| 型名 | gc-types.lisp | compiler.lisp 実際 | 差分 |
|------|--------------|-------------------|------|
| binding_frame | 8 | (未生成) | - |
| func_0 | 9 | 8 | -1 |
| func_1 | 10 | 9 | -1 |
| func_2 | 11 | 10 | -1 |
| func_3 | 12 | 11 | -1 |
| func_n | 13 | 12 | -1 |

## 修正方針の比較

### Option A: gc-types.lisp を修正（採用）

**変更内容**:
- `+type-func-0+` を 9 → 8 に変更
- 他の func 型インデックスも同様に -1

**メリット**:
- 変更箇所が 1 ファイル、5 行のみ
- compiler.lisp のロジック変更なし
- リグレッションリスク最小

**デメリット**:
- `+type-binding-frame+` が未使用になる（将来の US5 で必要）

### Option B: compiler.lisp を修正

**変更内容**:
- emit-type-section に binding_frame 型生成を追加

**メリット**:
- gc-types.lisp の設計意図を維持
- 将来の US5 対応がスムーズ

**デメリット**:
- 型セクション生成ロジックの変更が必要
- 型インデックスのずれが他の箇所に影響する可能性
- テスト範囲が広がる

### 決定

**Option A を採用**

理由:
1. 現時点で binding_frame は使用されていない
2. US5 実装時に再検討可能
3. バグ修正の最小限介入原則

## 影響範囲の確認

### gc-types.lisp の +type-func-*+ 使用箇所

```bash
grep -n "type-func" src/clysm/compiler/codegen/*.lisp
```

結果:
- `func-section.lisp`: compile-funcall 内で call_ref の型指定に使用
- `gc-types.lisp`: 定数定義

### 他の型インデックス定数の使用確認

- `+type-closure+` (5): closure 構造体の struct.new/struct.get で使用 → 影響なし
- `+type-cons+` (2): cons セルの生成で使用 → 影響なし
- `+type-symbol+` (3): シンボル操作で使用 → 影響なし

## 検証計画

### テストマトリクス

| テストカテゴリ | 現状 | 修正後期待 |
|--------------|------|-----------|
| closure-test | 16件失敗 | 全件成功 |
| special-var-test | 全件成功 | 全件成功（維持） |
| その他統合テスト | 成功 | 成功（維持） |

### 手動検証ケース

1. `(funcall (lambda () 42))` → 42
2. `(funcall (lambda (x) x) 42)` → 42
3. `(funcall (lambda (a b) (+ a b)) 10 20)` → 30
4. `(let ((x 10)) (funcall (lambda () x)))` → 10
