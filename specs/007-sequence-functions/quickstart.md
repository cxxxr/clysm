# Quickstart: Sequence Functions Implementation

**Feature**: 007-sequence-functions
**Date**: 2025-12-23

## Prerequisites

1. Nix環境がインストールされていること
2. `007-sequence-functions` ブランチにチェックアウト済み
3. 既存のcons/list機能（006-cons-list-ops）が動作すること

## 開発環境セットアップ

```bash
# 1. 開発シェルに入る
nix develop

# 2. 依存関係の確認
sbcl --version  # SBCL 2.4+
wasmtime --version
wasm-tools --version

# 3. 既存テストがパスすることを確認
./scripts/run-tests.sh
```

## TDDワークフロー

### Step 1: テスト作成

```bash
# tests/integration/sequence-test.lisp を作成
```

```lisp
;;;; sequence-test.lisp
(in-package #:clysm/tests/integration/sequence)

;; length のテスト
(deftest test-length-empty-list
  "Verify (length nil) returns 0"
  (ok (= 0 (clysm/tests:compile-and-run '(length nil)))
      "(length nil) should return 0"))

(deftest test-length-basic
  "Verify (length '(1 2 3)) returns 3"
  (ok (= 3 (clysm/tests:compile-and-run '(length (list 1 2 3))))
      "(length '(1 2 3)) should return 3"))
```

### Step 2: テスト失敗確認 (Red)

```bash
# テストを実行し、失敗を確認
./scripts/run-tests.sh sequence
# => FAIL: length is not defined
```

### Step 3: 実装 (Green)

```lisp
;; src/clysm/compiler/codegen/func-section.lisp に追加

(defun compile-length (args env)
  "Generate code for (length list)"
  (let ((list-code (compile-to-instructions (first args) env)))
    ;; ループでcons cellをカウント
    `(,@list-code
      (local.set $list)
      (i32.const 0)
      (local.set $count)
      (block $done
        (loop $loop
          (local.get $list)
          (global.get $nil)
          (ref.eq)
          (br_if $done)
          (local.get $count)
          (i32.const 1)
          (i32.add)
          (local.set $count)
          (local.get $list)
          (ref.cast (ref $cons))
          (struct.get $cons $cdr)
          (local.set $list)
          (br $loop)))
      (local.get $count)
      (ref.i31))))
```

### Step 4: リファクタリング (Refactor)

```bash
# テストがパスしたら、コードを整理
# 重複を排除、命名を改善
```

### Step 5: 次の関数へ

上記サイクルを以下の順序で繰り返す：

1. **Tier 1**: length → append → reverse → nreverse → last → butlast → copy-list
2. **Tier 2**: mapcar → mapc → maplist → reduce
3. **Tier 3**: find → find-if → position → position-if → remove → remove-if → remove-if-not → count → count-if

## ファイル編集ガイド

### コンパイラ組み込み関数の追加

```lisp
;; 1. src/clysm/compiler/codegen/func-section.lisp
;;    compile-primitive-call に新しい関数を追加

(defun compile-primitive-call (op args env)
  (case op
    ;; 既存
    (cons (compile-cons args env))
    (car (compile-car args env))
    ;; 新規追加
    (length (compile-length args env))
    (append (compile-append args env))
    ...))

;; 2. 各関数の実装を追加
(defun compile-length (args env)
  ...)
```

### テストの追加

```lisp
;; tests/integration/sequence-test.lisp

;; 各関数につき最低5つのテストケース:
;; 1. 基本動作
;; 2. 空リスト
;; 3. 単一要素
;; 4. 大規模リスト (100要素)
;; 5. ネストしたリスト or 特殊ケース
```

## 検証コマンド

```bash
# 個別テスト実行
./scripts/run-tests.sh sequence

# 全テスト実行
./scripts/run-tests.sh

# Wasm検証
wasm-tools validate output.wasm

# Nix品質ゲート
nix flake check
```

## トラブルシューティング

### よくあるエラー

| エラー | 原因 | 解決策 |
|--------|------|--------|
| `ref.cast failed` | 型不一致 | cons cellの型確認を追加 |
| `unreachable` | nilチェック漏れ | block/br_ifでnilガード |
| `stack underflow` | 命令順序ミス | WATで検証 |

### デバッグ方法

```bash
# WAT形式で出力して確認
wasm-tools print output.wasm > output.wat

# 特定関数の命令列を確認
grep -A 50 '$length' output.wat
```

## 進捗チェックリスト

### Tier 1 (7関数) ✅ COMPLETE
- [X] length
- [X] append
- [X] reverse
- [X] nreverse
- [X] last
- [X] butlast
- [X] copy-list

### Tier 2 (4関数) ✅ COMPLETE
- [X] mapcar
- [X] mapc
- [X] maplist
- [X] reduce

### Tier 3 (9関数) ✅ COMPLETE
- [X] find
- [X] find-if
- [X] position
- [X] position-if
- [X] remove
- [X] remove-if
- [X] remove-if-not
- [X] count
- [X] count-if

### Tier 4 (7関数) ✅ COMPLETE
- [X] member
- [X] assoc
- [X] rassoc
- [X] every
- [X] some
- [X] notany
- [X] notevery

### 完了条件
- [X] 全Tier 1-3関数が実装済み (20/20)
- [X] Tier 4関数も実装済み (7/7) - 合計27関数
- [X] 54のテストケースが存在
- [X] 既存テストがすべてパス
- [X] `nix flake check` がパス
