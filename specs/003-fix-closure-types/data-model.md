# Data Model: Closure Type Index Fix

**Date**: 2025-12-23
**Branch**: 003-fix-closure-types

## 概要

この修正はデータモデルに構造的な変更を加えない。型インデックス定数の値のみを修正する。

## 型インデックス定数（修正対象）

### Before（現状）

```lisp
;; src/clysm/compiler/codegen/gc-types.lisp
(defconstant +type-nil+ 0)
(defconstant +type-unbound+ 1)
(defconstant +type-cons+ 2)
(defconstant +type-symbol+ 3)
(defconstant +type-string+ 4)
(defconstant +type-closure+ 5)
(defconstant +type-instance+ 6)
(defconstant +type-standard-class+ 7)
(defconstant +type-binding-frame+ 8)  ;; ← 問題: 未生成
(defconstant +type-func-0+ 9)         ;; ← 問題: 実際は type 8
(defconstant +type-func-1+ 10)
(defconstant +type-func-2+ 11)
(defconstant +type-func-3+ 12)
(defconstant +type-func-n+ 13)
```

### After（修正後）

```lisp
;; src/clysm/compiler/codegen/gc-types.lisp
(defconstant +type-nil+ 0)
(defconstant +type-unbound+ 1)
(defconstant +type-cons+ 2)
(defconstant +type-symbol+ 3)
(defconstant +type-string+ 4)
(defconstant +type-closure+ 5)
(defconstant +type-instance+ 6)
(defconstant +type-standard-class+ 7)
;; +type-binding-frame+ は削除（将来のUS5で再追加予定）
(defconstant +type-func-0+ 8)         ;; 修正: 9 → 8
(defconstant +type-func-1+ 9)         ;; 修正: 10 → 9
(defconstant +type-func-2+ 10)        ;; 修正: 11 → 10
(defconstant +type-func-3+ 11)        ;; 修正: 12 → 11
(defconstant +type-func-n+ 12)        ;; 修正: 13 → 12
```

## Wasm 型セクション配置（参考）

compiler.lisp emit-type-section が生成する実際の型配置:

| Index | Type Name | Description |
|-------|-----------|-------------|
| 0 | $nil | NIL シングルトン構造体 |
| 1 | $unbound | UNBOUND センチネル構造体 |
| 2 | $cons | Cons セル (car, cdr) |
| 3 | $symbol | シンボル (name, value, function, plist) |
| 4 | $string | 文字列 (i8 array) |
| 5 | $closure | クロージャ (code_0, code_1, code_2, code_N, env) |
| 6 | (reserved) | 空構造体（将来の +type-instance+ 用） |
| 7 | (reserved) | 空構造体（将来の +type-standard-class+ 用） |
| 8 | $func_0 | 0引数関数型 (closure) -> anyref |
| 9 | $func_1 | 1引数関数型 (closure, arg) -> anyref |
| 10 | $func_2 | 2引数関数型 (closure, arg1, arg2) -> anyref |
| 11 | $func_3 | 3引数関数型 (closure, arg1, arg2, arg3) -> anyref |
| 12 | $func_N | 可変引数関数型 (closure, args-list) -> anyref |
| 13+ | (user funcs) | ユーザー定義関数の型 |

## 関連エンティティ（変更なし）

### Closure 構造体

```wat
(type $closure (struct
  (field $code_0 (ref null $func_0))  ;; 0引数用エントリポイント
  (field $code_1 (ref null $func_1))  ;; 1引数用エントリポイント
  (field $code_2 (ref null $func_2))  ;; 2引数用エントリポイント
  (field $code_N (ref null $func_N))  ;; 汎用エントリポイント
  (field $env (mut anyref))           ;; キャプチャ環境
))
```

### 関数型定義

```wat
(type $func_0 (func (param anyref) (result anyref)))
(type $func_1 (func (param anyref anyref) (result anyref)))
(type $func_2 (func (param anyref anyref anyref) (result anyref)))
(type $func_3 (func (param anyref anyref anyref anyref) (result anyref)))
(type $func_N (func (param anyref anyref) (result anyref)))
```

## 影響を受けるコード

### compile-funcall (func-section.lisp)

```lisp
;; 修正不要: +type-func-*+ の値が正しくなれば自動的に動作
(let ((func-type (case arity
                   (0 +type-func-0+)   ;; 8 になる
                   (1 +type-func-1+)   ;; 9 になる
                   (2 +type-func-2+)   ;; 10 になる
                   (3 +type-func-3+)   ;; 11 になる
                   (t +type-func-n+)))) ;; 12 になる
  (setf result (append result (list (list :call_ref func-type)))))
```
