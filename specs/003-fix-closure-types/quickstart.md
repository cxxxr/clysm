# Quickstart: Closure Type Index Fix

**Date**: 2025-12-23
**Branch**: 003-fix-closure-types

## 修正手順

### 1. 現状確認（テスト失敗）

```bash
# テスト実行（closure テストが失敗することを確認）
sbcl --non-interactive --eval '(asdf:test-system :clysm)' 2>&1 | tail -20
```

期待される出力:
```
Summary:
  3 tests failed.
    - clysm/tests/integration/closure
    - clysm/tests/integration/tco
    - clysm/tests/integration/control-flow
```

### 2. gc-types.lisp を修正

ファイル: `src/clysm/compiler/codegen/gc-types.lisp`

変更内容:
```lisp
;; 削除（または将来のためにコメントアウト）
;; (defconstant +type-binding-frame+ 8 "Type index for binding frame")

;; 値を修正
(defconstant +type-func-0+ 8 "Type index for 0-arg function type")  ;; 9 → 8
(defconstant +type-func-1+ 9 "Type index for 1-arg function type")  ;; 10 → 9
(defconstant +type-func-2+ 10 "Type index for 2-arg function type") ;; 11 → 10
(defconstant +type-func-3+ 11 "Type index for 3-arg function type") ;; 12 → 11
(defconstant +type-func-n+ 12 "Type index for N-arg function type") ;; 13 → 12
```

### 3. テスト実行（修正確認）

```bash
# フルテスト
sbcl --non-interactive --eval '(asdf:test-system :clysm)'

# 手動確認
sbcl --non-interactive \
  --eval '(ql:quickload :clysm :silent t)' \
  --eval '(print (clysm/tests/helpers:compile-and-run (quote (funcall (lambda () 42)))))'
# 期待: 42
```

### 4. 成功基準チェック

| 基準 | 確認方法 |
|------|----------|
| SC-001 | closure-test.lisp 全件成功 |
| SC-002 | special-var-test.lisp 全件成功 |
| SC-003 | `(funcall (lambda () 42))` → 42 |
| SC-004 | `(funcall (lambda (x) x) 42)` → 42 |
| SC-005 | 他のテストでリグレッションなし |

## トラブルシューティング

### SBCL が古い定数を使用する場合

```bash
# FASL キャッシュをクリア
rm -rf ~/.cache/common-lisp/

# 再コンパイル
sbcl --eval '(asdf:load-system :clysm :force t)' --quit
```

### 変更が反映されない場合

```bash
# パッケージをアンロードして再ロード
sbcl --eval '(asdf:clear-system :clysm)' \
     --eval '(asdf:load-system :clysm)' \
     --quit
```
