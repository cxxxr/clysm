# Contracts: 008-character-string

本機能はCommon Lispコンパイラの内部拡張であり、外部APIコントラクトは定義されない。

## 内部インターフェース

文字/文字列関数はコンパイラ組み込み関数として実装され、以下のパターンに従う：

### コンパイラ関数シグネチャ

```lisp
;; func-section.lisp に追加
(defun compile-FUNCTION-NAME (args env)
  "Compile (FUNCTION-NAME ...) to Wasm instructions"
  ;; 引数検証
  ;; ローカル変数割り当て
  ;; Wasm命令生成
  ;; 結果返却
  )
```

### 型定義（既存）

```wat
;; gc-types.lisp, index 4
(type $string (array i8))
```

### テストコントラクト

各関数は `compile-and-run` ヘルパーで検証：

```lisp
(deftest test-char-code
  (ok (= 65 (compile-and-run '(char-code #\A)))))
```
