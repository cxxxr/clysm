# Research: Common Lisp文字型と文字列操作

**Feature**: 008-character-string
**Date**: 2025-12-23
**Status**: Complete

## 1. 既存実装の調査

### 1.1 型定義システム（gc-types.lisp）

**発見事項**:
- `$string`型は既に定義済み: `(array i8)` として index 4
- 文字型は i31ref として表現可能（21ビット Unicodeコードポイント）
- 型インデックス体系: 0=nil, 1=unbound, 2=cons, 3=symbol, 4=string, 5=closure

**決定**: 文字は独自の構造体型を必要としない。i31ref として直接エンコード。

**根拠**: Unicodeコードポイント（最大0x10FFFF = 21ビット）はi31ref（31ビット符号付き）に収まる。
ヒープ割り当て不要で高速。

### 1.2 リーダー/トークナイザー（tokenizer.lisp）

**発見事項**:
- 文字列リテラル `"..."` は `read-string-token` で処理済み
- エスケープシーケンス `\n`, `\t`, `\\`, `\"` は実装済み
- `#\` ディスパッチマクロは未実装

**決定**: `read-character-token` 関数を追加し、`#` ディスパッチ処理を拡張。

**実装パターン**:
```lisp
(defun read-character-token (stream)
  "Read #\x or #\Name character literal"
  (let ((char (read-char stream)))
    (cond
      ((alpha-char-p char)
       ;; Could be named character like #\Space
       (let ((name (read-character-name stream char)))
         (or (named-char name) char)))
      (t char))))
```

### 1.3 組み込み関数パターン（func-section.lisp）

**発見事項**:
- 関数ディスパッチは `compile-primitive-call` の case 文
- 各関数は `compile-FUNCTION-NAME` パターンで実装
- ローカル変数は `env-add-local` で管理
- 戻り値は命令リストとして返却

**決定**: 既存パターンに従い、文字/文字列関数を追加。

**参照実装**:
- `length` (line 2722): ループパターン
- `append` (line 2762): リスト構築パターン
- `mapcar` (line 3100): 高階関数パターン

## 2. 技術的決定事項

### 2.1 文字型の内部表現

**決定**: i31ref（WasmGC即値参照）

**根拠**:
1. Unicodeコードポイント（0〜0x10FFFF）は21ビット
2. i31refは31ビット符号付き整数を格納可能
3. ヒープ割り当て不要で高速アクセス
4. Fixnumと同じパターンで一貫性あり

**代替案の却下**:
- 構造体: オーバーヘッド大、メリットなし
- 配列: 単一値に不適切

### 2.2 文字列型の内部表現

**決定**: 既存の`$string`型（immutable array i8）をUTF-8エンコーディングで使用

**根拠**:
1. 既に定義済み（gc-types.lisp）
2. UTF-8は可変長だがメモリ効率良好
3. Web/WASI互換性

**トレードオフ**:
- 文字アクセス: O(n)（UTF-8デコード必要）
- 長さ取得: O(n)（文字数カウント必要）
- 仕様で許容済み（Assumptions参照）

### 2.3 名前付き文字のマッピング

**決定**: ANSI Common Lisp標準に準拠

| 名前 | コードポイント | 備考 |
|------|---------------|------|
| Space | 0x20 | ASCII空白 |
| Newline | 0x0A | LF |
| Tab | 0x09 | 水平タブ |
| Return | 0x0D | CR |
| Linefeed | 0x0A | Newlineの別名 |
| Page | 0x0C | フォームフィード |
| Backspace | 0x08 | バックスペース |
| Rubout | 0x7F | DEL |

### 2.4 UTF-8デコードアルゴリズム

**決定**: インライン実装（ヘルパー関数なし）

```wat
;; UTF-8バイト列からコードポイントを抽出
;; バイト1のパターンで長さを判定
(if (i32.lt_u (i32.and byte1 0x80) 0x80)
    ;; 1バイト: 0xxxxxxx
    byte1
    (if (i32.eq (i32.and byte1 0xE0) 0xC0)
        ;; 2バイト: 110xxxxx 10xxxxxx
        (i32.or (i32.shl (i32.and byte1 0x1F) 6)
                (i32.and byte2 0x3F))
        ;; 3バイト, 4バイト...
    ))
```

## 3. 実装優先順位

### 優先度1（基盤）
1. `#\x` リテラルのリーダー対応
2. `char-code`, `code-char`
3. `characterp`

### 優先度2（文字操作）
1. 比較関数（`char=`, `char<` など6種）
2. Case-insensitive比較（5種）
3. ケース変換（`char-upcase`, `char-downcase`）
4. 述語（`alpha-char-p` など5種）

### 優先度3（文字列基盤）
1. `length` のUTF-8対応
2. `char`, `schar`
3. `stringp`

### 優先度4（文字列操作）
1. 比較関数（12種）
2. `make-string`
3. `string` 変換
4. ケース変換（3種）
5. `subseq`, `concatenate`

## 4. テスト戦略

### 単体テスト
- トークナイザー: 文字リテラルパース
- AST: 文字ノード生成
- コード生成: 各関数のWasm命令

### 統合テスト
- compile-and-run パターン使用
- 各関数の正常系・異常系
- Unicode文字（日本語など）

### 契約テスト
- 生成Wasmの型整合性
- wasm-tools validate パス確認

## 5. 参考資料

- [HyperSpec: Characters](http://www.lispworks.com/documentation/HyperSpec/Body/13_a.htm)
- [HyperSpec: Strings](http://www.lispworks.com/documentation/HyperSpec/Body/16_.htm)
- [UTF-8 RFC 3629](https://tools.ietf.org/html/rfc3629)
- 007-sequence-functions 実装（本プロジェクト）
