# Quickstart: Common Lisp文字型と文字列操作

**Feature**: 008-character-string

## 文字リテラル

```lisp
;; 単一文字
#\a          ; 小文字 a
#\A          ; 大文字 A
#\0          ; 数字 0

;; 名前付き文字
#\Space      ; 空白
#\Newline    ; 改行 (LF)
#\Tab        ; タブ
#\Return     ; キャリッジリターン (CR)
```

## 文字列リテラル

```lisp
;; 基本
"hello"
"Hello, World!"

;; エスケープシーケンス
"line1\nline2"    ; 改行
"col1\tcol2"      ; タブ
"say \"hi\""      ; ダブルクォート
"back\\slash"     ; バックスラッシュ

;; Unicode
"こんにちは"      ; 日本語
"émoji 🎉"        ; 絵文字
```

## 文字関数

### 変換

```lisp
(char-code #\A)       ; => 65
(code-char 97)        ; => #\a
(code-char -1)        ; => NIL (無効)

(char-upcase #\a)     ; => #\A
(char-downcase #\A)   ; => #\a
```

### 比較

```lisp
;; 大文字小文字区別
(char= #\a #\a)       ; => T
(char= #\a #\A)       ; => NIL
(char< #\a #\b)       ; => T
(char> #\z #\a)       ; => T

;; 大文字小文字無視
(char-equal #\a #\A)  ; => T
(char-lessp #\A #\b)  ; => T
```

### 述語

```lisp
(characterp #\a)      ; => T
(characterp 65)       ; => NIL

(alpha-char-p #\a)    ; => T
(alpha-char-p #\1)    ; => NIL

(digit-char-p #\5)    ; => 5
(digit-char-p #\a)    ; => NIL
(digit-char-p #\F 16) ; => 15 (16進)

(alphanumericp #\a)   ; => T
(alphanumericp #\5)   ; => T
(alphanumericp #\!)   ; => NIL

(upper-case-p #\A)    ; => T
(lower-case-p #\a)    ; => T
```

## 文字列関数

### アクセス

```lisp
(length "hello")      ; => 5
(length "こんにちは")   ; => 5 (文字数)

(char "hello" 0)      ; => #\h
(char "hello" 4)      ; => #\o
(schar "hello" 1)     ; => #\e
```

### 生成

```lisp
(make-string 5)                        ; => "     " (空白5文字)
(make-string 3 :initial-element #\x)   ; => "xxx"

(string #\a)          ; => "a"
(string 'hello)       ; => "HELLO" (シンボルから)
```

### 比較

```lisp
;; 大文字小文字区別
(string= "abc" "abc")     ; => T
(string= "abc" "ABC")     ; => NIL
(string< "apple" "banana"); => T (辞書順)

;; 大文字小文字無視
(string-equal "abc" "ABC"); => T
(string-lessp "ABC" "def"); => T
```

### 変換

```lisp
(string-upcase "Hello")       ; => "HELLO"
(string-downcase "HELLO")     ; => "hello"
(string-capitalize "hello world") ; => "Hello World"
```

### 操作

```lisp
(subseq "hello" 1 4)          ; => "ell"
(subseq "hello" 2)            ; => "llo"

(concatenate 'string "hello" " " "world")
; => "hello world"
```

### 型判定

```lisp
(stringp "hello")     ; => T
(stringp #\a)         ; => NIL
(stringp 'symbol)     ; => NIL
```

## エラーケース

```lisp
;; インデックス範囲外
(char "hello" 10)     ; => ERROR
(char "" 0)           ; => ERROR

;; 負のインデックス
(char "hello" -1)     ; => ERROR

;; subseq範囲エラー
(subseq "hello" 3 1)  ; => ERROR (end < start)
```

## 実装メモ

### 文字表現
- 内部: i31ref（Unicodeコードポイント）
- 範囲: 0x0〜0x10FFFF（サロゲート除く）

### 文字列表現
- 内部: (array i8)（UTF-8バイト配列）
- `length`: 文字数（バイト数ではない）、O(n)
- `char`: 指定位置の文字、O(n)

### Unicode対応
```lisp
(char-code #\あ)      ; => 12354 (U+3042)
(length "あいう")      ; => 3
(char "日本語" 1)     ; => #\本
```
