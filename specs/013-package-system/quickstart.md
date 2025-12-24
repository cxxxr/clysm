# Quickstart: ANSI Common Lispパッケージシステム

**Date**: 2025-12-24
**Feature**: 013-package-system

## 概要

Clysm3コンパイラにANSI Common Lisp準拠のパッケージシステムを実装する。この機能により、名前空間の分離、シンボルのエクスポート/インポート、およびパッケージ修飾子によるシンボル参照が可能になる。

## 開発環境セットアップ

```bash
# 開発シェルに入る（Nix必須）
cd /home/user/src/clysm-workbench/clysm3
nix develop

# テスト実行
./run-tests.sh  # または rove clysm-tests.asd
```

## 主要ファイル

### 変更対象

| File | Action | Description |
|------|--------|-------------|
| `src/clysm/reader/package.lisp` | EXTEND | パッケージ構造拡張、export/import等追加 |
| `src/clysm/reader/tokenizer.lisp` | MODIFY | パッケージ修飾子トークン化 |
| `src/clysm/reader/parser.lisp` | MODIFY | 修飾付きシンボルのパース |
| `tests/unit/package-test.lisp` | EXTEND | 単体テスト追加 |

### 新規作成

| File | Description |
|------|-------------|
| `src/clysm/lib/package-macros.lisp` | defpackage, in-package マクロ |
| `tests/integration/package-integration-test.lisp` | 統合テスト |
| `tests/unit/tokenizer-package-test.lisp` | トークナイザーテスト |

## 実装フェーズ

### Phase 1: パッケージ構造拡張

```lisp
;; 現在の構造
(:name <name> :symbols <hash-table> :nicknames <list>)

;; 拡張後
(:name <name>
 :nicknames <list>
 :internal-symbols <hash-table>
 :external-symbols <hash-table>
 :use-list <list>
 :used-by-list <list>
 :shadowing-symbols <list>)
```

**テストファースト**:
```lisp
(deftest package-structure
  (testing "package has separate internal/external tables"
    (let ((pkg (make-package* "TEST")))
      (ok (hash-table-p (package-internal-symbols* pkg)))
      (ok (hash-table-p (package-external-symbols* pkg))))))
```

### Phase 2: シンボル操作関数

**実装順序**:
1. `intern` / `find-symbol` (多値返却対応)
2. `export` / `unexport`
3. `import` / `shadowing-import`
4. `use-package` / `unuse-package`

**テスト例**:
```lisp
(deftest export-import
  (testing "export makes symbol external"
    (let ((pkg (make-package* "EXP-TEST")))
      (intern-symbol "FOO" pkg)
      (export-symbol 'foo pkg)
      (multiple-value-bind (sym status)
          (find-symbol* "FOO" pkg)
        (ok (eq status :external))))))
```

### Phase 3: リーダー拡張

**トークナイザー変更**:
```lisp
;; read-number-or-symbol を拡張
;; コロン検出時に qualified-symbol トークンを生成

;; 入力: "cl:car"
;; トークン: (:qualified-symbol :package-name "CL"
;;                              :symbol-name "CAR"
;;                              :external t)
```

**パーサー変更**:
```lisp
(defun parse-qualified-symbol (token)
  "Parse a package-qualified symbol token."
  (let* ((pkg-name (getf token :package-name))
         (sym-name (getf token :symbol-name))
         (external-p (getf token :external))
         (pkg (find-package* pkg-name)))
    (unless pkg
      (parser-error (format nil "Package not found: ~A" pkg-name)))
    (if external-p
        ;; 外部シンボル参照 - エクスポートチェック必須
        (let ((sym (find-external-symbol sym-name pkg)))
          (unless sym
            (parser-error (format nil "Symbol ~A not exported from ~A"
                                  sym-name pkg-name)))
          sym)
        ;; 内部シンボル参照 - インターンのみ
        (intern-symbol sym-name pkg))))
```

### Phase 4: マクロ実装

**defpackage**:
```lisp
(defmacro defpackage (name &rest options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((pkg (or (find-package* ,(string name))
                    (make-package* ,(string name)))))
       ,@(process-defpackage-options options)
       pkg)))
```

**in-package**:
```lisp
(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((pkg (find-package* ,(string name))))
       (unless pkg
         (error "Package not found: ~A" ',name))
       (setf *package* pkg))))
```

## テスト実行

```bash
# 全テスト
./run-tests.sh

# パッケージ関連のみ
sbcl --eval '(ql:quickload :clysm-tests)' \
     --eval '(rove:run :clysm/tests/unit/package)' \
     --quit

# 統合テスト
sbcl --eval '(ql:quickload :clysm-tests)' \
     --eval '(rove:run :clysm/tests/integration/package-integration)' \
     --quit
```

## よくあるエラーと対処

### "Package not found"

原因: パッケージが未定義または名前のタイポ
対処: `(find-package "NAME")` で存在確認

### "Symbol not exported"

原因: シングルコロン（`:`）でエクスポートされていないシンボルにアクセス
対処: ダブルコロン（`::`）を使用するか、元パッケージでexport

### "Name conflict"

原因: use-package時に同名シンボルが衝突
対処: `shadow` または `shadowing-import` で解決

## 参考リソース

- [ANSI CL仕様 11章 Packages](http://www.lispworks.com/documentation/HyperSpec/Body/11_.htm)
- [CLtL2 Chapter 11](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node111.html)
- 既存実装: `src/clysm/reader/package.lisp`
- 既存テスト: `tests/unit/package-test.lisp`
