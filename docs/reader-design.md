# Clysm Reader Design

## Overview

WASM上で動作するS式リーダーの設計。

## Input Stream Abstraction

文字列とポジションで入力ストリームを表現:

```
Input Stream Structure:
[string-ptr:i32] [position:i32] [length:i32]
```

グローバル変数で管理:
- `$input-string` - 入力文字列へのポインタ
- `$input-pos` - 現在の読み取り位置
- `$input-len` - 入力文字列の長さ

## Core Primitives

### 低レベル入力

| 関数 | 説明 |
|------|------|
| `stream-init` | 文字列から入力ストリームを初期化 |
| `stream-peek-char` | 次の文字を先読み（位置を進めない） |
| `stream-read-char` | 次の文字を読み取り（位置を進める） |
| `stream-unread-char` | 位置を1つ戻す |
| `stream-eof-p` | EOF判定 |

## Tokenizer

### トークン型

| 型 | 値 |
|----|-----|
| EOF | 0 |
| LPAREN | 1 |
| RPAREN | 2 |
| DOT | 3 |
| QUOTE | 4 |
| BACKQUOTE | 5 |
| COMMA | 6 |
| COMMA-AT | 7 |
| NUMBER | 8 |
| SYMBOL | 9 |
| STRING | 10 |

### トークン構造

```
Token Structure:
[type:i32] [value:i32]
```

- NUMBER: valueは整数値
- SYMBOL: valueは文字列ポインタ
- STRING: valueは文字列ポインタ
- その他: valueは未使用

## Read Algorithm

```
read():
  token = next-token()
  case token.type:
    LPAREN -> read-list()
    QUOTE -> (cons 'quote (cons (read) nil))
    BACKQUOTE -> (cons 'backquote (cons (read) nil))
    COMMA -> (cons 'unquote (cons (read) nil))
    COMMA-AT -> (cons 'splice-unquote (cons (read) nil))
    NUMBER -> token.value
    SYMBOL -> intern(token.value)
    STRING -> token.value
    EOF -> error or return :eof

read-list():
  items = nil
  loop:
    token = peek-token()
    case token.type:
      RPAREN -> return reverse(items)
      DOT ->
        next-token()  ; consume DOT
        last = read()
        expect RPAREN
        return nreverse-with-last(items, last)
      else ->
        item = read()
        items = cons(item, items)
```

## Number Parsing

整数のみ（最初のフェーズ）:
- 符号: `+` または `-`
- 数字: `0-9`

```
parse-integer(str):
  sign = 1
  if str[0] == '-': sign = -1, start = 1
  elif str[0] == '+': start = 1
  else: start = 0

  result = 0
  for i = start to len(str):
    digit = str[i] - '0'
    result = result * 10 + digit

  return result * sign
```

## Symbol Parsing

シンボル構成文字:
- アルファベット: `A-Z`, `a-z`
- 数字（先頭以外）: `0-9`
- 特殊: `+`, `-`, `*`, `/`, `=`, `<`, `>`, `!`, `?`, `_`, `&`, `%`, `$`, `@`, `~`, `^`

シンボル終端文字:
- 空白: space, tab, newline
- 括弧: `(`, `)`
- 引用: `'`, `` ` ``, `,`
- セミコロン: `;`
- ダブルクォート: `"`

## String Parsing

```
parse-string():
  skip opening "
  result = empty-string
  loop:
    ch = read-char()
    if ch == '"': return result
    if ch == '\\':
      ch = read-char()
      case ch:
        'n' -> append newline
        't' -> append tab
        '"' -> append "
        '\\' -> append \
    else:
      append ch
```

## Comment Handling

```
skip-whitespace-and-comments():
  loop:
    ch = peek-char()
    case ch:
      space, tab, newline, carriage-return:
        read-char()
        continue
      ';':
        skip-line()
        continue
      else:
        return
```

## Implementation Phases

### Phase 1: 基本実装
1. 入力ストリームプリミティブ
2. 数値パーサー
3. シンボルパーサー（internなし、文字列として返す）
4. リストパーサー
5. `read-from-string` 関数

### Phase 2: 完全実装
1. シンボルのintern
2. 文字列リテラル
3. クォート処理
4. コメント処理

### Phase 3: 拡張
1. バッククォート/アンクォート
2. `#'` (function)
3. `#\` (character)
4. `#|...|#` (block comment)

## Testing

```lisp
;; Phase 1 tests
(read-from-string "123") => 123
(read-from-string "-42") => -42
(read-from-string "hello") => "HELLO" (or symbol)
(read-from-string "(+ 1 2)") => (+ 1 2)
(read-from-string "(a b c)") => (a b c)
(read-from-string "(a . b)") => (a . b)
(read-from-string "((a b) (c d))") => ((a b) (c d))
```
