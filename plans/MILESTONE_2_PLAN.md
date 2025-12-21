# Milestone 2: 言語学的層（インタプリタ）実装プラン

## 概要

Milestone 1で構築したカーネル・ランタイム（データ構造とプリミティブ）の上に、
Common Lispの「言語学的層」を実装する。この層は制御構造と動的機能を提供し、
最小限のLispインタプリタとして動作する。

## 目標

- シンボルテーブル（パッケージシステムの基盤）と`intern`関数
- S式Reader/Printer
- Wasm内インタプリタ（eval関数）
- 動的スコープ（スペシャル変数）
- 非局所脱出（block/return-from, catch/throw）

## 成果物

```
src/
├── kernel/
│   ├── kernel.wat      # (既存) 基本データ構造
│   └── eval.wat        # (新規) インタプリタ・制御構造
js/
├── bridge.js           # (既存) カーネルブリッジ
├── reader.js           # (新規) S式パーサー
└── printer.js          # (新規) オブジェクト表示
test/
├── kernel.test.js      # (既存) カーネルテスト
├── reader.test.js      # (新規) Readerテスト
├── printer.test.js     # (新規) Printerテスト
└── eval.test.js        # (新規) インタプリタテスト
```

---

## Phase 2.1: シンボルテーブルとintern

### 2.1.1 パッケージ構造体の定義 (kernel.wat)

```wat
;; パッケージ（シンボルテーブル）
(type $package (struct
  (field $name (ref $string))           ;; パッケージ名
  (field $symbols (mut (ref $vector)))  ;; シンボルハッシュテーブル
  (field $nicknames (mut anyref))       ;; ニックネームリスト
  (field $use_list (mut anyref))        ;; 使用パッケージリスト
))
```

### 2.1.2 ハッシュテーブル実装

シンボルのインターニングには文字列をキーとするハッシュテーブルが必要。

```wat
;; ハッシュテーブルエントリ
(type $hash_entry (struct
  (field $key (ref $string))
  (field $value anyref)
  (field $next (mut anyref))  ;; チェイン法
))

;; ハッシュテーブル
(type $hash_table (struct
  (field $buckets (ref $vector))
  (field $count (mut i32))
))
```

### 2.1.3 エクスポート関数

| 関数 | 説明 |
|------|------|
| `make_package(name)` | パッケージ生成 |
| `intern(name, pkg)` | シンボルをインターン |
| `find_symbol(name, pkg)` | シンボル検索 |
| `string_hash(s)` | 文字列ハッシュ値計算 |
| `string_equal(a, b)` | 文字列比較 |

### 2.1.4 グローバルパッケージ

```wat
;; COMMON-LISP-USERパッケージ（デフォルト）
(global $CL_USER (mut (ref null $package)) (ref.null $package))

;; KEYWORDパッケージ
(global $KEYWORD (mut (ref null $package)) (ref.null $package))
```

### 2.1.5 検証コード

```javascript
const kernel = await loadKernel();
const sym1 = kernel.intern("FOO");
const sym2 = kernel.intern("FOO");
assert(kernel.eq(sym1, sym2)); // 同一オブジェクト

const bar = kernel.intern("BAR");
assert(!kernel.eq(sym1, bar)); // 異なるオブジェクト
```

---

## Phase 2.2: Reader（S式パーサー）

### 2.2.1 トークナイザ (reader.js)

```javascript
class Tokenizer {
  constructor(input) {
    this.input = input;
    this.pos = 0;
  }

  // トークン種別
  // - LPAREN: (
  // - RPAREN: )
  // - DOT: .
  // - QUOTE: '
  // - NUMBER: 整数・浮動小数点
  // - STRING: "..."
  // - SYMBOL: シンボル名
  // - EOF: 終端
}
```

### 2.2.2 パーサー (reader.js)

```javascript
class Reader {
  constructor(kernel, tokenizer) {
    this.kernel = kernel;
    this.tokenizer = tokenizer;
  }

  read() {
    // S式をパースしてWasmオブジェクトを返す
  }

  readList() {
    // リスト構造をパース
  }

  readAtom(token) {
    // アトム（数値、シンボル、文字列）をパース
  }
}
```

### 2.2.3 サポートする構文

| 構文 | 例 | 変換結果 |
|------|-----|---------|
| 整数 | `42`, `-17` | Fixnum |
| シンボル | `foo`, `+` | Symbol（intern経由） |
| リスト | `(a b c)` | Cons chain |
| ドット対 | `(a . b)` | Single cons |
| クォート | `'x` | `(quote x)` |
| 文字列 | `"hello"` | String |
| NIL | `nil`, `()` | NIL |
| T | `t` | T |

### 2.2.4 検証コード

```javascript
const reader = new Reader(kernel);
const expr = reader.readFromString("(+ 1 2)");
// => (CONS #<SYMBOL +> (CONS 1 (CONS 2 NIL)))
```

---

## Phase 2.3: Printer（オブジェクト表示）

### 2.3.1 実装 (printer.js)

```javascript
class Printer {
  constructor(kernel) {
    this.kernel = kernel;
  }

  print(obj) {
    if (this.kernel.isFixnum(obj)) {
      return String(this.kernel.fixnumValue(obj));
    }
    if (this.kernel.isNull(obj)) {
      return "NIL";
    }
    if (this.kernel.isCons(obj)) {
      return this.printList(obj);
    }
    if (this.kernel.isSymbol(obj)) {
      return this.printSymbol(obj);
    }
    if (this.kernel.isString(obj)) {
      return `"${this.kernel.stringToJS(obj)}"`;
    }
    // ...
  }

  printList(cons) {
    // 適切なリスト・ドット対表記
  }
}
```

### 2.3.2 出力形式

| オブジェクト | 出力 |
|-------------|------|
| Fixnum 42 | `42` |
| NIL | `NIL` |
| T | `T` |
| Symbol FOO | `FOO` |
| Cons (1 2 3) | `(1 2 3)` |
| Dotted (1 . 2) | `(1 . 2)` |
| String "hi" | `"hi"` |

---

## Phase 2.4: クロージャと環境

### 2.4.1 クロージャ構造体 (kernel.wat)

```wat
;; 関数型シグネチャ（統一形式）
;; すべての関数: (env, args_list) -> anyref
(type $func_sig (func (param anyref anyref) (result anyref)))

;; クロージャ構造体
(type $closure (struct
  (field $code (ref $func_sig))    ;; Wasm関数への参照
  (field $env (mut anyref))        ;; レキシカル環境
  (field $name (mut anyref))       ;; 関数名（デバッグ用）
  (field $lambda_list (mut anyref)) ;; 引数リスト（デバッグ用）
))
```

### 2.4.2 レキシカル環境

```wat
;; 環境フレーム
(type $env_frame (struct
  (field $bindings (ref $vector))  ;; 変数バインディング
  (field $parent (mut anyref))     ;; 親環境
))
```

### 2.4.3 プリミティブ関数

| 関数 | 説明 |
|------|------|
| `make_closure(code, env)` | クロージャ生成 |
| `closurep(x)` | クロージャ判定 |
| `closure_env(c)` | 環境取得 |
| `make_env_frame(parent, size)` | 環境フレーム生成 |
| `env_ref(env, depth, index)` | 環境から変数取得 |
| `env_set(env, depth, index, val)` | 環境に変数設定 |

---

## Phase 2.5: インタプリタ (eval)

### 2.5.1 eval関数の構造 (eval.wat)

```wat
;; メインのeval関数
(func $eval (param $expr anyref) (param $env anyref) (result anyref)
  ;; 1. 自己評価オブジェクト（数値、文字列）
  ;; 2. シンボル → 変数参照
  ;; 3. リスト → 特殊形式 or 関数呼び出し
)
```

### 2.5.2 特殊形式（Phase 2.5で実装）

| 形式 | 実装方法 |
|------|---------|
| `quote` | 引数をそのまま返却 |
| `if` | 条件分岐 |
| `progn` | 逐次評価、最後の値を返却 |
| `setq` | 変数への代入 |
| `lambda` | クロージャ構造体を生成 |
| `let` | 新しい環境フレームを作成 |
| `let*` | 逐次的な環境拡張 |

### 2.5.3 関数呼び出し

```wat
;; apply関数
(func $apply (param $func anyref) (param $args anyref) (result anyref)
  ;; 1. funcがクロージャか確認
  ;; 2. クロージャからcode, envを取得
  ;; 3. call_refでcode実行
)
```

### 2.5.4 検証コード

```lisp
;; 基本的な評価
(quote foo)        ; => FOO
(if t 1 2)         ; => 1
(if nil 1 2)       ; => 2
(progn 1 2 3)      ; => 3

;; lambda と関数呼び出し
((lambda (x) x) 42)                    ; => 42
((lambda (x y) (+ x y)) 10 20)         ; => 30

;; let
(let ((x 10)) x)                       ; => 10
(let ((x 1) (y 2)) (+ x y))            ; => 3

;; ネストしたlet
(let ((x 10))
  (let ((y 20))
    (+ x y)))                          ; => 30
```

---

## Phase 2.6: 動的スコープ（スペシャル変数）

### 2.6.1 バインディングスタック

```wat
;; バインディングスタックエントリ
(type $binding_entry (struct
  (field $symbol (ref $symbol))
  (field $old_value anyref)
))

;; グローバルなバインディングスタック
(global $binding_stack (mut (ref $vector)) ...)
(global $binding_sp (mut i32) (i32.const 0))
```

### 2.6.2 動的バインディング操作

```wat
;; スペシャル変数をバインド
(func $bind_special (param $sym (ref $symbol)) (param $val anyref)
  ;; 1. 現在の symbol.$value をスタックにプッシュ
  ;; 2. symbol.$value を新しい値で上書き
)

;; スペシャル変数をアンバインド
(func $unbind_special (param $sym (ref $symbol))
  ;; 1. スタックから古い値をポップ
  ;; 2. symbol.$value を復元
)
```

### 2.6.3 スペシャル変数の宣言

```lisp
;; *で囲まれた名前をスペシャル変数として扱う慣習
(let ((*print-base* 16))
  ...)
```

### 2.6.4 検証コード

```lisp
(defvar *x* 10)
(defun get-x () *x*)
(get-x)                    ; => 10
(let ((*x* 20))
  (get-x))                 ; => 20 (動的スコープ)
(get-x)                    ; => 10 (復元)
```

---

## Phase 2.7: 非局所脱出

### 2.7.1 Wasm Exception Handling

```wat
;; 例外タグの定義
(tag $block_exit (param anyref anyref))  ;; (tag-id, value)
(tag $catch_throw (param anyref anyref)) ;; (tag, value)
```

### 2.7.2 block / return-from

```wat
;; block の実装
(func $eval_block (param $name anyref) (param $body anyref) (param $env anyref) (result anyref)
  (local $tag_id anyref)
  ;; 一意なタグIDを生成
  (local.set $tag_id (call $make_block_tag (local.get $name)))

  (try_table (result anyref)
    (catch $block_exit ...)
    ;; bodyを評価
    (call $eval_progn (local.get $body) (local.get $env))
  )
)

;; return-from の実装
(func $eval_return_from (param $name anyref) (param $value anyref) (param $env anyref) (result anyref)
  ;; タグIDを検索して throw
  (throw $block_exit
    (call $find_block_tag (local.get $name) (local.get $env))
    (local.get $value))
)
```

### 2.7.3 catch / throw

```wat
;; catch の実装
(func $eval_catch (param $tag anyref) (param $body anyref) (param $env anyref) (result anyref)
  (try_table (result anyref)
    (catch $catch_throw ...)
    (call $eval_progn (local.get $body) (local.get $env))
  )
)

;; throw の実装
(func $eval_throw (param $tag anyref) (param $value anyref) (result anyref)
  (throw $catch_throw (local.get $tag) (local.get $value))
)
```

### 2.7.4 unwind-protect

```wat
(func $eval_unwind_protect (param $protected anyref) (param $cleanup anyref) (param $env anyref) (result anyref)
  (local $result anyref)
  (local $exception_caught i32)

  (try_table (result anyref)
    (catch_all ...)
    ;; protected formを評価
    (local.set $result (call $eval (local.get $protected) (local.get $env)))
    (local.get $result)
  )
  ;; catch_all で例外捕捉時:
  ;; 1. cleanupを実行
  ;; 2. rethrowで例外を再送出
)
```

### 2.7.5 検証コード

```lisp
;; block / return-from
(block foo
  (+ 1 (return-from foo 42) 3))  ; => 42

;; catch / throw
(catch 'done
  (+ 1 (throw 'done 100) 2))     ; => 100

;; unwind-protect
(let ((x 0))
  (catch 'exit
    (unwind-protect
        (throw 'exit 'escaped)
      (setq x 1)))
  x)                              ; => 1 (cleanupが実行された)
```

---

## Phase 2.8: 組み込み関数

### 2.8.1 基本関数

| 関数 | 説明 |
|------|------|
| `car`, `cdr` | リスト操作 |
| `cons` | コンス生成 |
| `eq`, `eql`, `equal` | 等価性判定 |
| `atom`, `consp`, `symbolp`, `numberp` | 型述語 |
| `+`, `-`, `*`, `/` | 算術演算 |
| `<`, `>`, `<=`, `>=`, `=` | 比較演算 |
| `not`, `null` | 論理演算 |

### 2.8.2 リスト関数

| 関数 | 説明 |
|------|------|
| `list` | リスト生成 |
| `append` | リスト連結 |
| `reverse` | リスト反転 |
| `length` | リスト長 |
| `nth` | N番目の要素 |
| `nthcdr` | N番目のcdr |

### 2.8.3 高階関数

| 関数 | 説明 |
|------|------|
| `funcall` | 関数呼び出し |
| `apply` | 引数リストで関数呼び出し |
| `mapcar` | マップ関数 |

---

## 実装順序

```
Phase 2.1: シンボルテーブル・intern
├── 文字列ハッシュ関数
├── ハッシュテーブル実装
├── パッケージ構造体
├── intern/find_symbol
└── テスト

Phase 2.2: Reader
├── Tokenizer
├── Parser (数値、シンボル、リスト)
├── クォート展開
└── テスト

Phase 2.3: Printer
├── 型別表示関数
├── リスト表示（循環検出なし）
└── テスト

Phase 2.4: クロージャ・環境
├── クロージャ構造体
├── 環境フレーム
├── 環境操作関数
└── テスト

Phase 2.5: eval（基本）
├── 自己評価オブジェクト
├── シンボル評価
├── quote, if, progn
├── lambda, let, let*
├── 関数呼び出し (apply)
└── テスト

Phase 2.6: 動的スコープ
├── バインディングスタック
├── bind_special / unbind_special
├── defvar サポート
└── テスト

Phase 2.7: 非局所脱出
├── Wasm EH タグ定義
├── block / return-from
├── catch / throw
├── unwind-protect
└── テスト

Phase 2.8: 組み込み関数
├── 基本関数
├── リスト関数
├── 高階関数
└── 統合テスト
```

---

## 技術的考慮事項

### Wasm Exception Handling

- `try_table` / `throw` / `catch` を使用
- Node.js 22では `--experimental-wasm-exnref` フラグが必要な場合あり
- wasm-tools は最新のEH仕様をサポート

### パフォーマンス

- インタプリタは即応性優先（Tier 1）
- ホットパスの最適化はMilestone 3（JITコンパイラ）で対応
- シンボルルックアップはO(1)を目指す（ハッシュテーブル）

### 制限事項（Milestone 2では未実装）

- マクロ（defmacro）
- 多値返却（values, multiple-value-bind）
- 末尾呼び出し最適化
- tagbody / go
- CLOSの特殊形式

---

## 検証基準

### 機能テスト

```lisp
;; 全フェーズ完了後の統合テスト

;; 階乗
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 5)  ; => 120

;; フィボナッチ
(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(fib 10)  ; => 55

;; 高階関数
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
; => (1 4 9 16 25)

;; 動的スコープ
(defvar *multiplier* 2)
(defun multiply (x) (* x *multiplier*))
(multiply 10)                    ; => 20
(let ((*multiplier* 10))
  (multiply 10))                 ; => 100

;; 非局所脱出
(defun find-first (pred list)
  (catch 'found
    (mapcar (lambda (x)
              (if (funcall pred x)
                  (throw 'found x)))
            list)
    nil))
(find-first (lambda (x) (> x 5)) '(1 3 7 2 9))
; => 7
```

---

## 次のステップ（Milestone 3への接続）

Milestone 2完了後：

1. **JITコンパイラ**: eval済みS式からWasmバイナリ生成
2. **compile関数**: 関数をネイティブWasmにコンパイル
3. **最適化**: 末尾呼び出し、型推論、インライン化
4. **defmacro**: コンパイル時マクロ展開

---

*このプランは実装を進める中で適宜更新する。*
