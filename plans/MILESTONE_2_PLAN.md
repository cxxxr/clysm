# Milestone 2: 言語学的層（インタプリタ）実装プラン

## 概要

Milestone 1で構築したカーネル・ランタイム（データ構造とプリミティブ）の上に、
Common Lispの「言語学的層」を実装する。この層は制御構造と動的機能を提供し、
最小限のLispインタプリタとして動作する。

## 実装状況

| Phase | 内容 | 状態 | テスト数 |
|-------|------|------|----------|
| 2.1 | シンボルテーブル・intern | ✅ 完了 | 93 |
| 2.2 | Reader（S式パーサー） | ✅ 完了 | 58 |
| 2.3 | Printer（オブジェクト表示） | ✅ 完了 | 41 |
| 2.4 | クロージャと環境 | ✅ 完了 | 11 |
| 2.5 | eval関数（基本） | ✅ 完了 | 72 |
| 2.6 | 動的スコープ | ✅ 完了 | 7 |
| 2.7 | 非局所脱出 | ✅ 完了 | 4 |
| 2.8 | 組み込み関数 | ✅ 完了 | - |

**合計: 289テスト パス**

## 目標

- シンボルテーブル（パッケージシステムの基盤）と`intern`関数
- S式Reader/Printer
- Wasm評価器（eval/apply）をカーネルへ統合
- 動的スコープ（スペシャル変数）
- 非局所脱出（block/return-from, catch/throw）

## 成果物

```
src/
├── kernel/
│   └── kernel.wat      # (拡張) eval/apply/動的束縛/非局所脱出
js/
├── bridge.js           # (拡張) 環境・クロージャ操作追加
├── reader.js           # (新規) S式パーサー
├── printer.js          # (新規) オブジェクト表示
test/
├── kernel.test.js      # (拡張) 環境・クロージャテスト追加
├── reader.test.js      # (新規) Readerテスト
├── printer.test.js     # (新規) Printerテスト
└── eval.test.js        # (新規) 評価器テスト
```

### 実装上の重要な決定

1. **Wasm評価器へ回帰**: `eval/apply` と解釈クロージャ（GC struct）を `src/kernel/kernel.wat` に統合し、
   JS側は Reader/Printer/instantiate（＋エラーブリッジ）に限定する。

2. **NILシンボル検索バグの修正**: `find_symbol_in_package`が「見つからない」場合にNILを返すと、
   NILシンボル自体の検索で誤判定。UNBOUNDマーカーを返すよう修正。

3. **浅いバインディング（Shallow Binding）**: 動的スコープはWasm側トレイルで実装。
   シンボルの値セルを直接書き換え、評価器が確実に復元する。

---

## Phase 2.1: シンボルテーブルとintern ✅

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

## Phase 2.2: Reader（S式パーサー） ✅

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

## Phase 2.3: Printer（オブジェクト表示） ✅

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

## Phase 2.4: クロージャと環境 ✅

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

## Phase 2.5: インタプリタ (eval) ✅

> **実装注記**: eval/apply は Wasm 側（`src/kernel/kernel.wat`）に統合。
> 解釈クロージャは GC struct（`$interpreted_closure`）で表現。

### 2.5.1 eval関数の構造 (kernel.wat)

```wat
;; exported entry point (top-level env is NIL)
(func (export "eval") (param anyref) (result anyref))

;; interpreted closure object
(type $interpreted_closure (struct
  (field $params anyref)
  (field $body anyref)
  (field $env anyref)))
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

## Phase 2.6: 動的スコープ（スペシャル変数） ✅

> **実装注記**: Wasm 側トレイル（`$TRAIL`）で shallow binding を実装。
> 非局所脱出でも評価器が必ず復元する。

### 2.6.1 バインディングトレイル (kernel.wat)

```wat
;; global trail stack (symbol, oldValue) pairs
(global $TRAIL (mut (ref null $vector)) (ref.null $vector))
(global $TRAIL_SP (mut i32) (i32.const 0))

;; push old binding, then write new value
(func $bind_special (param (ref $symbol)) (param anyref))
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

## Phase 2.7: 非局所脱出 ✅

> **実装注記**: Wasm評価器内で「制御オブジェクト（RETURN-FROM/THROW/ERROR）を返して伝播させる」方式で実装。
> `unwind-protect` は cleanup を必ず評価し、cleanup の非局所脱出があればそれを優先する。

### 2.7.1 検証コード

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

## Phase 2.8: 組み込み関数 ✅

> **注記**: 基本的なプリミティブはPhase 2.5で実装済み。
> 追加の組み込み関数は必要に応じて実装。

### 2.8.1 実装済みプリミティブ

| 関数 | 説明 | 状態 |
|------|------|------|
| `car`, `cdr` | リスト操作 | ✅ |
| `cons` | コンス生成 | ✅ |
| `rplaca`, `rplacd` | 破壊的更新 | ✅ |
| `eq`, `eql`, `equal` | 等価性判定 | ✅ |
| `atom`, `consp`, `symbolp`, `numberp`, `stringp`, `functionp` | 型述語 | ✅ |
| `null`, `not` | 論理演算 | ✅ |
| `+`, `-`, `*`, `/` | 算術演算 | ✅ |
| `<`, `>`, `<=`, `>=`, `=` | 比較演算 | ✅ |
| `list` | リスト生成 | ✅ |
| `length` | リスト長 | ✅ |
| `funcall` | 関数呼び出し | ✅ |
| `apply` | 引数リストで関数呼び出し | ✅ |

### 2.8.2 未実装リスト関数

| 関数 | 説明 | 状態 |
|------|------|------|
| `append` | リスト連結 | ⏳ |
| `reverse` | リスト反転 | ⏳ |
| `nth` | N番目の要素 | ⏳ |
| `nthcdr` | N番目のcdr | ⏳ |
| `last` | 最後のコンス | ⏳ |
| `butlast` | 最後以外 | ⏳ |

### 2.8.3 未実装高階関数

| 関数 | 説明 | 状態 |
|------|------|------|
| `mapcar` | マップ関数 | ⏳ |
| `mapc` | 副作用マップ | ⏳ |
| `reduce` | 畳み込み | ⏳ |
| `remove-if` | フィルター | ⏳ |
| `find-if` | 検索 | ⏳ |

---

## 実装順序

```
Phase 2.1: シンボルテーブル・intern ✅
├── djb2ハッシュ関数 (kernel.wat)
├── ハッシュテーブル・チェイン法 (kernel.wat)
├── パッケージ構造体 (CL-USER, KEYWORD)
├── intern/find_symbol (NIL検索バグ修正済み)
└── テスト (93)

Phase 2.2: Reader ✅
├── Tokenizer (reader.js)
├── Parser (数値、シンボル、文字列、リスト、ドット対)
├── クォート展開 ('x → (quote x))
├── コメント処理 (;)
└── テスト (58)

Phase 2.3: Printer ✅
├── 型別表示関数 (printer.js)
├── リスト表示（正規リスト、ドット対）
├── エスケープシーケンス
└── テスト (41)

Phase 2.4: クロージャ・環境 ✅
├── 環境フレーム構造体 (kernel.wat)
├── env_ref, env_set, env_lookup, env_set_at
├── クロージャ・プリミティブ型定義
└── テスト (11)

Phase 2.5: eval（基本） ✅
├── $interpreted_closure (kernel.wat)
├── 自己評価オブジェクト、シンボル評価
├── quote, if, progn, setq
├── lambda, let, let*, defun, defvar, function
├── プリミティブ関数 (22種)
└── テスト (72)

Phase 2.6: 動的スコープ ✅
├── special 判定（*foo* 規約 + defvarでのマーク）
├── トレイル（shallow binding）
├── bind_special / trail_restore_to
└── テスト (7)

Phase 2.7: 非局所脱出 ✅
├── block / return-from
├── catch / throw
├── unwind-protect
└── テスト (4)

Phase 2.8: 組み込み関数 ✅
├── リスト関数 (append, reverse, nth, etc.)
├── 高階関数 (mapcar, reduce, etc.)
└── 統合テスト
```

---

## 技術的考慮事項

### Wasm GC と JavaScript Interop

- **クロージャ問題**: Wasm GCの`ref $func_sig`型はJavaScriptに渡せない
- **解決策**: `InterpretedClosure`クラスをJavaScript側で実装
- **将来**: Wasm内完結の評価器はMilestone 3以降で検討

### 非局所脱出の実装方針

- **Phase 2.7**: JavaScript例外機構 (`throw`/`catch`) を使用
- **将来の最適化**: Wasm Exception Handling (`try_table`/`throw`/`catch`)
- Node.js 22では Wasm EH がネイティブサポート

### パフォーマンス

- インタプリタは即応性優先（Tier 1）
- ホットパスの最適化はMilestone 3（JITコンパイラ）で対応
- シンボルルックアップはO(1)（ハッシュテーブル + djb2）

### 制限事項（Milestone 2では未実装）

- マクロ（defmacro）
- 多値返却（values, multiple-value-bind）
- 末尾呼び出し最適化
- tagbody / go
- CLOSの特殊形式

---

## 検証基準

### 実装済みテスト ✅

```lisp
;; 階乗 (test/eval.test.js)
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 5)  ; => 120 ✅

;; フィボナッチ (test/eval.test.js)
(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(fib 10)  ; => 55 ✅

;; 高階関数 (test/eval.test.js)
(defun apply-twice (f x)
  (funcall f (funcall f x)))
(defun addone (x) (+ x 1))
(apply-twice (function addone) 0)  ; => 2 ✅

;; クロージャ (test/eval.test.js)
(defun make-adder (n)
  (lambda (x) (+ x n)))
(defvar *add5* (make-adder 5))
(funcall *add5* 10)  ; => 15 ✅

;; 動的スコープ (test/eval.test.js)
(defvar *multiplier* 2)
(defun multiply (x) (* x *multiplier*))
(multiply 10)                    ; => 20 ✅
(let ((*multiplier* 10))
  (multiply 10))                 ; => 100 ✅
```

### 未実装テスト ⏳

```lisp
;; mapcar (Phase 2.8で実装予定)
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
; => (1 4 9 16 25)

;; 非局所脱出 (Phase 2.7で実装予定)
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

## 次のステップ

### Milestone 2 残り作業

1. **Phase 2.7: 非局所脱出**
   - `block` / `return-from`
   - `catch` / `throw`
   - `unwind-protect`

2. **Phase 2.8: 組み込み関数**
   - `mapcar`, `reduce`, `remove-if`, `find-if`
   - `append`, `reverse`, `nth`, `nthcdr`

### Milestone 3への接続

1. **JITコンパイラ**: eval済みS式からWasmバイナリ生成
2. **compile関数**: 関数をネイティブWasmにコンパイル
3. **最適化**: 末尾呼び出し、型推論、インライン化
4. **defmacro**: コンパイル時マクロ展開

---

*最終更新: 2025-12-21 (Phase 2.1-2.6 完了)*
