# Research: Common Lisp Sequence Functions

**Feature**: 007-sequence-functions
**Date**: 2025-12-23

## Overview

本ドキュメントはシーケンス関数実装に必要な技術調査結果をまとめる。
Technical Contextに不明点（NEEDS CLARIFICATION）がないため、
ベストプラクティスと実装パターンに焦点を当てる。

---

## 1. 実装戦略: コンパイラ組み込み vs Lisp実装

### Decision: ハイブリッドアプローチ

基本関数はコンパイラ組み込み（compile-*関数）として実装し、
複雑な関数はLispで実装してブートストラップ時にロードする。

### Rationale

| 関数タイプ | 実装方式 | 理由 |
|-----------|---------|------|
| length, reverse, nreverse | コンパイラ組み込み | 単純なループ、性能重要 |
| append, copy-list | コンパイラ組み込み | 再帰的cons構築、最適化可能 |
| last, butlast | コンパイラ組み込み | 単純なトラバーサル |
| mapcar, mapc, maplist | コンパイラ組み込み | funcall統合、性能重要 |
| reduce | コンパイラ組み込み | :initial-value処理、性能重要 |
| find, find-if, position, position-if | コンパイラ組み込み | 早期終了最適化 |
| remove, remove-if, remove-if-not | コンパイラ組み込み | 新リスト構築 |
| count, count-if | コンパイラ組み込み | 単純なカウンタ |
| member, assoc, rassoc | コンパイラ組み込み | 早期終了最適化 |
| every, some, notany, notevery | コンパイラ組み込み | 早期終了最適化 |
| subst | Lisp実装 | 木構造再帰、複雑性高 |

### Alternatives Considered

1. **全てLisp実装**: コンパイラ変更不要だが、性能が劣る
2. **全てコンパイラ組み込み**: 性能最適だが、複雑な関数のデバッグが困難
3. **ハイブリッド（採用）**: 性能とメンテナンス性のバランス

---

## 2. Wasm命令生成パターン

### 既存パターン: compile-nth の分析

```lisp
;; 既存の compile-nth パターン
(defun compile-nth-accessor (n args env)
  "Generate direct accessor for (nth N list) where N is constant"
  ;; n回のcdr + 最後にcar
  (let ((instructions (compile-to-instructions (first args) env)))
    (dotimes (i n)
      (setf instructions (append instructions (generate-cdr-instructions))))
    (append instructions (generate-car-instructions))))
```

### 新規パターン: length のループ構造

```wat
;; length の生成パターン
(func $length (param $list anyref) (result i32)
  (local $count i32)
  (local $current anyref)
  (local.set $current (local.get $list))
  (block $done
    (loop $loop
      ;; nil check
      (br_if $done (ref.eq (local.get $current) (global.get $nil)))
      ;; count++
      (local.set $count (i32.add (local.get $count) (i32.const 1)))
      ;; current = cdr(current)
      (local.set $current
        (struct.get $cons $cdr
          (ref.cast (ref $cons) (local.get $current))))
      (br $loop)))
  (local.get $count))
```

### 新規パターン: mapcar の高階関数呼び出し

```wat
;; mapcar の生成パターン (単一リスト版)
(func $mapcar (param $fn anyref) (param $list anyref) (result anyref)
  ;; 結果リストを逆順で構築し、最後にnreverse
  ;; または末尾consポインタを保持して順方向構築
  ...)
```

### Decision: 順方向構築（last-cons追跡）

- 逆順構築+nreverseは2パス必要
- 末尾consポインタを追跡すれば1パスで完了

---

## 3. 高階関数のfuncall統合

### 既存インフラ

既存の`compile-funcall`は以下の構造でクロージャを呼び出す：

```lisp
;; 既存パターン
(compile-funcall '(funcall fn arg1 arg2) env)
;; → closure構造体の適切なcode_N スロットを参照してcall_ref
```

### mapcar での活用

```lisp
(defun compile-mapcar (args env)
  "Generate code for (mapcar fn list)"
  (let ((fn-code (compile-to-instructions (first args) env))
        (list-code (compile-to-instructions (second args) env)))
    ;; 1. fnを評価してlocal.setで保存
    ;; 2. listをトラバース
    ;; 3. 各要素に対して保存したfnをfuncall
    ;; 4. 結果をconsで連結
    ...))
```

### Decision: ループ内でのクロージャ呼び出しパターン

1. 関数引数を一度だけ評価しローカル変数に保存
2. ループ内では保存した参照を使用
3. アリティに応じたcode_Nスロットをcall_ref

---

## 4. 破壊的操作 (nreverse, rplaca, rplacd)

### 既存インフラ

`rplaca`/`rplacd`は既に実装済み（006-cons-list-ops）:

```lisp
(defun compile-rplaca (args env)
  ;; struct.set $cons $car
  ...)
```

### nreverse の実装

```wat
;; nreverse: インプレースで逆順化
(func $nreverse (param $list anyref) (result anyref)
  (local $prev anyref)
  (local $current anyref)
  (local $next anyref)
  (local.set $prev (global.get $nil))
  (local.set $current (local.get $list))
  (block $done
    (loop $loop
      (br_if $done (ref.eq (local.get $current) (global.get $nil)))
      ;; next = cdr(current)
      (local.set $next
        (struct.get $cons $cdr (ref.cast (ref $cons) (local.get $current))))
      ;; cdr(current) = prev
      (struct.set $cons $cdr
        (ref.cast (ref $cons) (local.get $current))
        (local.get $prev))
      ;; prev = current
      (local.set $prev (local.get $current))
      ;; current = next
      (local.set $current (local.get $next))
      (br $loop)))
  (local.get $prev))
```

---

## 5. キーワード引数の処理

### Phase 1 スコープ

初期実装では以下のデフォルトを使用：
- `:test` → `#'eql` (固定)
- `:key` → `nil` (identity関数、固定)
- `:initial-value` → reduceでのみサポート

### 将来拡張

キーワード引数の完全サポートには以下が必要：
1. `&key`パラメータのコンパイル対応
2. キーワードシンボルの認識
3. デフォルト値処理

これはPhase 8bまたはそれ以降で対応。

---

## 6. エラー処理

### ANSI CL準拠

| 状況 | ANSI CL動作 | Phase 8a実装 |
|------|-------------|--------------|
| (length non-list) | type-error | 未定義動作（後続対応） |
| (reduce #'+ nil) | error (no initial-value) | 未定義動作（後続対応） |
| (nth -1 list) | undefined | 未定義動作 |

### Decision: エラー処理は後続フェーズ

本フェーズでは正常パスの実装に集中し、
条件システム（Phase 8c）実装後にエラー処理を追加する。

---

## 7. テスト戦略

### 既存パターンの活用

```lisp
;; list-test.lisp のパターンを継承
(deftest test-length-basic
  "Verify (length '(1 2 3)) returns 3"
  (ok (= 3 (clysm/tests:compile-and-run '(length '(1 2 3))))
      "(length '(1 2 3)) should return 3"))
```

### テストカテゴリ

1. **基本動作**: 各関数の正常パス
2. **空リスト**: `nil`に対する動作
3. **単一要素**: `(x)`に対する動作
4. **大規模リスト**: 100要素以上
5. **ネスト**: `((1 2) (3 4))`形式
6. **クロージャ統合**: mapcar + lambda

---

## Summary

| 項目 | 決定 | 根拠 |
|------|------|------|
| 実装方式 | ハイブリッド（組み込み主体） | 性能とメンテナンス性のバランス |
| リスト構築 | 順方向（last-cons追跡） | 1パス完了、性能向上 |
| 高階関数 | 既存funcall活用 | コード再利用、一貫性 |
| キーワード引数 | デフォルト固定 | スコープ限定、将来拡張 |
| エラー処理 | 後続フェーズ | 条件システム待ち |
