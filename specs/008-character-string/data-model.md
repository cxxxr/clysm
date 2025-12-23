# Data Model: Common Lisp文字型と文字列操作

**Feature**: 008-character-string
**Date**: 2025-12-23

## 1. 型定義

### 1.1 Character（文字型）

**WasmGC表現**: `i31ref`（即値参照）

```wat
;; 文字はi31refとして直接表現
;; エンコード: (ref.i31 codepoint)
;; デコード: (i31.get_s char_ref)

;; 例: 文字 'A' (U+0041)
(ref.i31 (i32.const 65))
```

**値域**:
- 最小: 0x0（NUL文字）
- 最大: 0x10FFFF（Unicode最大コードポイント）
- 除外: 0xD800〜0xDFFF（サロゲート範囲）

**属性**:
| 属性 | 値 |
|------|-----|
| 型分類 | 即値（immediate） |
| ヒープ割り当て | 不要 |
| 可変性 | 不変（immutable） |
| 等価性 | コードポイント比較 |

### 1.2 String（文字列型）

**WasmGC表現**: `(ref $string)` where `$string = (array i8)`

```wat
;; 既存の型定義（gc-types.lisp, index 4）
(type $string (array i8))

;; 例: 文字列 "hello"
(array.new_fixed $string 5
  (i32.const 104)  ;; 'h'
  (i32.const 101)  ;; 'e'
  (i32.const 108)  ;; 'l'
  (i32.const 108)  ;; 'l'
  (i32.const 111)) ;; 'o'
```

**エンコーディング**: UTF-8

**属性**:
| 属性 | 値 |
|------|-----|
| 型分類 | ヒープ割り当て（heap-allocated） |
| 内部表現 | UTF-8バイト配列 |
| 可変性 | 不変（immutable） |
| 長さ | バイト数（`array.len`）≠ 文字数 |
| 等価性 | バイト列比較 |

## 2. 型判定ロジック

### 2.1 characterp

```wat
;; 文字かどうかを判定
;; 現在の実装では、i31refの値がUnicode範囲内かで判定
(func $characterp (param $obj anyref) (result i32)
  (if (ref.test (ref i31) (local.get $obj))
    (then
      (local.set $code (i31.get_s (ref.cast (ref i31) (local.get $obj))))
      ;; 0 <= code <= 0x10FFFF かつサロゲート範囲外
      (i32.and
        (i32.and
          (i32.ge_s (local.get $code) (i32.const 0))
          (i32.le_s (local.get $code) (i32.const 0x10FFFF)))
        (i32.or
          (i32.lt_u (local.get $code) (i32.const 0xD800))
          (i32.gt_u (local.get $code) (i32.const 0xDFFF)))))
    (else (i32.const 0))))
```

**注**: Fixnumとの区別は現在の設計では行わない。文字とFixnumは同じi31ref表現を共有し、
型情報はコンテキストで決定される。将来的にタグ付けを検討。

### 2.2 stringp

```wat
;; 文字列かどうかを判定
(func $stringp (param $obj anyref) (result i32)
  (ref.test (ref $string) (local.get $obj)))
```

## 3. UTF-8エンコーディング

### 3.1 バイトパターン

| 範囲 | バイト1 | バイト2 | バイト3 | バイト4 |
|------|---------|---------|---------|---------|
| U+0000〜U+007F | 0xxxxxxx | - | - | - |
| U+0080〜U+07FF | 110xxxxx | 10xxxxxx | - | - |
| U+0800〜U+FFFF | 1110xxxx | 10xxxxxx | 10xxxxxx | - |
| U+10000〜U+10FFFF | 11110xxx | 10xxxxxx | 10xxxxxx | 10xxxxxx |

### 3.2 デコード関数（擬似コード）

```lisp
(defun utf8-decode-char (bytes index)
  "Decode UTF-8 character at INDEX, return (values codepoint next-index)"
  (let ((b1 (aref bytes index)))
    (cond
      ((< b1 #x80)                    ; 1バイト
       (values b1 (1+ index)))
      ((= (logand b1 #xE0) #xC0)      ; 2バイト
       (let ((b2 (aref bytes (1+ index))))
         (values (logior (ash (logand b1 #x1F) 6)
                         (logand b2 #x3F))
                 (+ index 2))))
      ((= (logand b1 #xF0) #xE0)      ; 3バイト
       (let ((b2 (aref bytes (1+ index)))
             (b3 (aref bytes (+ index 2))))
         (values (logior (ash (logand b1 #x0F) 12)
                         (ash (logand b2 #x3F) 6)
                         (logand b3 #x3F))
                 (+ index 3))))
      ((= (logand b1 #xF8) #xF0)      ; 4バイト
       (let ((b2 (aref bytes (1+ index)))
             (b3 (aref bytes (+ index 2)))
             (b4 (aref bytes (+ index 3))))
         (values (logior (ash (logand b1 #x07) 18)
                         (ash (logand b2 #x3F) 12)
                         (ash (logand b3 #x3F) 6)
                         (logand b4 #x3F))
                 (+ index 4)))))))
```

### 3.3 エンコード関数（擬似コード）

```lisp
(defun utf8-encode-char (codepoint)
  "Encode codepoint to UTF-8 bytes, return list of bytes"
  (cond
    ((< codepoint #x80)
     (list codepoint))
    ((< codepoint #x800)
     (list (logior #xC0 (ash codepoint -6))
           (logior #x80 (logand codepoint #x3F))))
    ((< codepoint #x10000)
     (list (logior #xE0 (ash codepoint -12))
           (logior #x80 (logand (ash codepoint -6) #x3F))
           (logior #x80 (logand codepoint #x3F))))
    (t
     (list (logior #xF0 (ash codepoint -18))
           (logior #x80 (logand (ash codepoint -12) #x3F))
           (logior #x80 (logand (ash codepoint -6) #x3F))
           (logior #x80 (logand codepoint #x3F))))))
```

## 4. 名前付き文字テーブル

| 名前 | コードポイント | UTF-8バイト |
|------|---------------|-------------|
| Space | U+0020 | 0x20 |
| Newline | U+000A | 0x0A |
| Tab | U+0009 | 0x09 |
| Return | U+000D | 0x0D |
| Linefeed | U+000A | 0x0A |
| Page | U+000C | 0x0C |
| Backspace | U+0008 | 0x08 |
| Rubout | U+007F | 0x7F |

## 5. 関数シグネチャ

### 5.1 文字関数

| 関数 | 入力 | 出力 | 備考 |
|------|------|------|------|
| char-code | character | fixnum | コードポイント抽出 |
| code-char | fixnum | character/nil | 無効値はNIL |
| char-upcase | character | character | 大文字変換 |
| char-downcase | character | character | 小文字変換 |
| char= | char, char | boolean | 等価比較 |
| char< | char, char | boolean | 順序比較 |
| alpha-char-p | character | boolean | 英字判定 |
| digit-char-p | character, [radix] | fixnum/nil | 数字判定 |
| characterp | any | boolean | 型判定 |

### 5.2 文字列関数

| 関数 | 入力 | 出力 | 備考 |
|------|------|------|------|
| length | string | fixnum | 文字数（O(n)） |
| char | string, index | character | 文字取得（O(n)） |
| schar | simple-string, index | character | 同上 |
| make-string | size, [:initial-element] | string | 新規作成 |
| string | designator | string | 変換 |
| string= | str, str | boolean | 等価比較 |
| string< | str, str | boolean/nil | 辞書順比較 |
| string-upcase | string | string | 大文字変換 |
| subseq | string, start, [end] | string | 部分文字列 |
| concatenate | 'string, strings... | string | 連結 |
| stringp | any | boolean | 型判定 |

## 6. エラー条件

| 条件 | 関数 | 動作 |
|------|------|------|
| インデックス範囲外 | char, schar, subseq | エラー |
| 無効なコードポイント | code-char | NIL返却 |
| 型エラー | 全関数 | エラー |
| 負のインデックス | char, schar, subseq | エラー |
| end < start | subseq | エラー |
