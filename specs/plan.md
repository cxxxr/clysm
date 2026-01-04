# Clysm v1.0 実装プラン
## WebAssembly GCターゲット Common Lispカーネル

### 概要

| 項目 | 内容 |
|------|------|
| ターゲット | WebAssembly GC (Wasm 3.0) |
| 実装言語 | Common Lisp (SBCL) |
| 設計方針 | 単純性・単値性・静的安定性 |
| スコープ | 第1レイヤー（カーネル言語） |
| 外部依存 | なし（セルフホスティング準備） |

---

## 依存関係ポリシー

### 原則

- **外部ライブラリ依存ゼロ**
- ANSI Common Lisp標準機能のみ使用
- セルフホスティングを見据えた設計

### 自前実装ユーティリティ

```lisp
;;; src/clysm/util.lisp

;; hash-table操作
(defun hash-table-keys (ht)
  (loop for k being the hash-keys of ht collect k))

;; リスト操作
(defun flatten (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (flatten (car tree))
                   (flatten (cdr tree))))))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

;; シーケンス操作
(defun lastcar (list)
  (car (last list)))

(defun ensure-list (x)
  (if (listp x) x (list x)))

;; 条件
(defmacro if-let ((var form) then &optional else)
  `(let ((,var ,form))
     (if ,var ,then ,else)))

(defmacro when-let ((var form) &body body)
  `(let ((,var ,form))
     (when ,var ,@body)))
```

---

## Phase 0: プロジェクト基盤

**目的**: 開発環境とビルドシステムの確立

### 0.1 Nix環境
```
- flake.nix: SBCL + wasm-tools + wasmtime
- devShell: 開発依存関係
- direnv連携
```

### 0.2 ASDFシステム定義
```lisp
;;; clysm.asd
(asdf:defsystem #:clysm
  :description "Common Lisp to WebAssembly GC Compiler"
  :version "0.1.0"
  :license "MIT"
  :depends-on ()  ;; 外部依存なし
  :serial t
  :components
  ((:module "src"
    :pathname "src/clysm"
    :components
    ((:file "package")
     (:file "util")
     (:file "conditions")
     ...))))
```

### 0.3 基本ディレクトリ構造
```
src/clysm/
├── package.lisp
├── util.lisp         # 自前ユーティリティ
├── conditions.lisp   # エラー定義
├── backend/          # Wasmエミッタ
├── compiler/         # コンパイラ
├── runtime/          # ランタイム表現
└── reader/           # S式リーダー

tests/
├── unit/
├── integration/
└── contract/         # Wasm検証
```

---

## Phase 1: Wasmバイナリエミッタ

**目的**: Wasmバイナリを直接生成する基盤

### 1.1 LEB128エンコーディング
```lisp
;; 符号なし/符号付きLEB128
(encode-uleb128 value) → bytes
(encode-sleb128 value) → bytes
```

### 1.2 モジュール構造
```
Magic: 0x00 0x61 0x73 0x6D
Version: 0x01 0x00 0x00 0x00

Sections:
  1: Type     - 関数シグネチャ + GC型定義
  2: Import   - 外部インポート
  3: Function - 関数インデックス
  7: Export   - エクスポート
 10: Code     - 関数ボディ
```

### 1.3 WAT出力（デバッグ用）
```lisp
(print-wat module stream)
;; 人間可読なWATフォーマット出力
```

---

## Phase 2: WasmGC型システム

**目的**: Lispオブジェクトのメモリ表現を定義

### 2.1 型階層設計
```wat
;; ルート型
anyref   ;; すべてのLispオブジェクト

;; プリミティブ
i31ref   ;; Fixnum (31ビット整数)

;; 構造体型
$nil     ;; NILシングルトン
$unbound ;; UNBOUND センチネル
$cons    ;; コンスセル
$symbol  ;; シンボル
$closure ;; クロージャ
$string  ;; 文字列
$vector  ;; ベクタ
```

### 2.2 コンスセル
```wat
(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))))
```

### 2.3 シンボル
```wat
(type $symbol (struct
  (field $name (ref $string))      ;; 印字名
  (field $value (mut anyref))      ;; グローバル値
  (field $function (mut anyref))   ;; 関数定義
  (field $plist (mut anyref))))    ;; プロパティリスト
```

### 2.4 クロージャ
```wat
;; アリティディスパッチ（0-2引数 + 汎用）
(type $closure (struct
  (field $code_0 (ref null $func_0))
  (field $code_1 (ref null $func_1))
  (field $code_2 (ref null $func_2))
  (field $code_N (ref null $func_N))
  (field $env (mut anyref))))
```

### 2.5 特殊オブジェクト
```
NIL      → 専用struct（car/cdr両方が自身を指す）
UNBOUND  → センチネル（未束縛検出用）
T        → シンボル（真値）
```

---

## Phase 3: 基本データ型とプリミティブ

**目的**: コアデータ操作の実装

### 3.1 Fixnum演算
```lisp
;; i31ref使用（ヒープ割り当てなし）
(+ a b)  → i32.add + ref.i31
(- a b)  → i32.sub + ref.i31
(* a b)  → i32.mul + ref.i31
(< a b)  → i32.lt_s → boolean変換
```

### 3.2 コンスセル操作
```lisp
(cons x y)   → struct.new $cons
(car cell)   → struct.get $cons $car
(cdr cell)   → struct.get $cons $cdr
(rplaca c v) → struct.set $cons $car
(rplacd c v) → struct.set $cons $cdr
```

### 3.3 型述語
```lisp
(null x)     → ref.eq NIL
(atom x)     → NOT (consp x)
(consp x)    → ref.test $cons
(symbolp x)  → ref.test $symbol
(numberp x)  → ref.test (ref i31)
(functionp x)→ ref.test $closure
```

### 3.4 等価性
```lisp
(eq x y)     → ref.eq
(eql x y)    → eq OR 数値比較
```

---

## Phase 4: 最小コンパイラ

**目的**: S式からWasmへのコンパイル

### 4.1 AST定義
```lisp
(defstruct ast-literal value type)
(defstruct ast-symbol name)
(defstruct ast-if test then else)
(defstruct ast-progn forms)
(defstruct ast-lambda params body)
(defstruct ast-call func args)
(defstruct ast-let bindings body)
```

### 4.2 コンパイル環境
```lisp
(defstruct compile-env
  locals      ;; ローカル変数 → Wasmローカルインデックス
  closures    ;; クロージャ変数
  tail-pos    ;; 末尾位置フラグ
  blocks)     ;; block/return-from用
```

### 4.3 コアフォーム
```lisp
;; 特殊形式
quote    → リテラル生成
if       → Wasm if/else
progn    → 順次評価
lambda   → クロージャ生成
let/let* → ローカル束縛

;; 関数呼び出し
(f args...) → クロージャディスパッチ
```

### 4.4 グローバル定義
```lisp
(defun name (params) body)
  → シンボルの$functionスロットに設定

(setq var value)
  → シンボルの$valueスロットに設定
```

---

## Phase 5: クロージャと関数呼び出し

**目的**: 第一級関数の完全サポート

### 5.1 自由変数解析
```lisp
(lambda (x) (+ x y))
;; y は自由変数 → 環境にキャプチャ
```

### 5.2 クロージャ変換
```
Lisp: (lambda (x) (+ x y))
       ↓
Wasm: (func $lambda_1 (param $env anyref) (param $x anyref) ...)
      ;; $env から y を取得
```

### 5.3 環境構造体
```wat
;; 環境は anyref の配列
(type $env (array (mut anyref)))

;; アクセス
array.get $env index  ;; 自由変数取得
```

### 5.4 関数呼び出しプロトコル
```
1. クロージャから$code_N取得（アリティに応じて）
2. 環境を第1引数として渡す
3. call_ref で呼び出し
```

---

## Phase 6: 制御フロー

**目的**: 非局所脱出の実装

### 6.1 block/return-from
```lisp
(block name
  (return-from name value))
```
```wat
;; Wasm Exception Handling使用
(try_table (catch $block_tag ...)
  ...)
(throw $block_tag value)
```

### 6.2 tagbody/go
```lisp
(tagbody
 start
  (go end)
 end
  (return nil))
```
```wat
;; Wasm loopとbrで実装
(loop $tagbody
  (block $start ...)
  (block $end ...)
  (br $tagbody))
```

### 6.3 catch/throw
```lisp
(catch 'error
  (throw 'error value))
```
```wat
;; 動的タグ → ランタイムハッシュテーブル
;; try_table + throw
```

### 6.4 unwind-protect
```lisp
(unwind-protect
    protected-form
  cleanup-forms...)
```
```wat
;; try_table catch_all で実装
;; cleanup後に rethrow
```

---

## Phase 7: 動的スコープ（スペシャル変数）

**目的**: Common Lisp固有の動的束縛

### 7.1 シャローバインディング
```
シンボル.$value = 現在の値（グローバル）
バインディングスタック = 退避値のスタック
```

### 7.2 束縛プロトコル
```
(let ((*special* new-val)) ...)

1. 古い値をバインディングスタックにpush
2. シンボル.$valueを新しい値で上書き
3. unwind-protectで復元を保証
```

### 7.3 defvar/defparameter
```lisp
(defvar *name* initial-value)
(defparameter *name* value)
;; シンボルをスペシャル変数としてマーク
```

---

## Phase 8: マクロシステム

**目的**: コンパイル時コード変換

### 8.1 defmacro
```lisp
(defmacro when (test &body body)
  `(if ,test (progn ,@body)))
```

### 8.2 バッククォート展開
```lisp
`(a ,b ,@c)
→ (list 'a b (append c nil))
```

### 8.3 マクロ展開フェーズ
```
1. マクロ呼び出し検出
2. マクロ関数を（ホストLispで）実行
3. 展開結果を再帰的に展開
4. 展開完了後にコンパイル
```

---

## Phase 9: リーダー

**目的**: S式テキストの解析

### 9.1 字句解析
```
トークン種別:
- NUMBER: 123, -45, 3.14
- SYMBOL: foo, +, *special*
- STRING: "hello"
- CHAR: #\a
- LPAREN, RPAREN, DOT
- QUOTE, BACKQUOTE, COMMA, COMMA-AT
```

### 9.2 S式パーサー
```
atom        → number | symbol | string | char
list        → '(' {sexpr}* ['.' sexpr] ')'
quoted      → '\'' sexpr
backquoted  → '`' sexpr
```

### 9.3 シンボルインターン
```lisp
(intern "FOO" package) → シンボル
;; パッケージ内のハッシュテーブルで管理
```

---

## Phase 10: REPL・統合

**目的**: 対話的実行環境

### 10.1 基本REPL
```
Read  → S式リーダー
Eval  → コンパイル + 実行
Print → オブジェクトプリンタ
Loop  → 繰り返し
```

### 10.2 JavaScript連携
```javascript
// ブラウザ/Node.jsでの実行
const clysm = await WebAssembly.instantiate(wasmModule, imports);
clysm.exports.eval("(+ 1 2)");  // → 3
```

### 10.3 エラー処理
```lisp
;; 基本的なcondition system
(error "message")
(handler-case expr (error (e) ...))
```

---

## 実装優先順位

```
┌─────────────────────────────────────────────────────────┐
│  Phase 0-1: 基盤（必須）                                  │
│  ├─ Nix環境、ASDFシステム                                │
│  └─ Wasmエミッタ、LEB128                                │
├─────────────────────────────────────────────────────────┤
│  Phase 2-3: コアデータ型（必須）                          │
│  ├─ WasmGC型定義                                        │
│  └─ Fixnum、Cons、Symbol                                │
├─────────────────────────────────────────────────────────┤
│  Phase 4-5: コンパイラコア（必須）                        │
│  ├─ AST、if/progn/lambda                                │
│  └─ クロージャ変換                                      │
├─────────────────────────────────────────────────────────┤
│  Phase 6: 制御フロー                                     │
│  └─ block/return-from、catch/throw                      │
├─────────────────────────────────────────────────────────┤
│  Phase 7: 動的スコープ                                   │
│  └─ スペシャル変数                                      │
├─────────────────────────────────────────────────────────┤
│  Phase 8-9: 言語完成                                     │
│  ├─ マクロシステム                                      │
│  └─ リーダー                                            │
├─────────────────────────────────────────────────────────┤
│  Phase 10: 統合                                          │
│  └─ REPL、JavaScript連携                                │
└─────────────────────────────────────────────────────────┘
```

---

## 技術的判断のまとめ

| 項目 | 選択 | 根拠 |
|------|------|------|
| 外部依存 | なし | セルフホスティング準備 |
| Fixnum | i31ref | ヒープ割り当て不要、GC圧力軽減 |
| NIL | シングルトンstruct | シンボルとしての機能維持 |
| 関数呼び出し | アリティディスパッチ | 頻出パターン最適化 |
| 非局所脱出 | try_table + throw | Wasm EH活用 |
| 末尾再帰 | return_call | スタック消費なし |
| 動的スコープ | シャローバインディング | O(1)アクセス |
| 多値 | **延期** | 第1レイヤーでは単値のみ |
| CLOS | **延期** | 構造体のみサポート |

---

## セルフホスティング戦略

```
Phase 1-10: SBCLでコンパイラ実装
    ↓
Phase 11: Clysm標準ライブラリ実装
    ↓
Phase 12: ClysmでClysmコンパイラを再実装
    ↓
セルフホスティング達成
```

---

## 参考資料

- specs/spec.md - 第1レイヤー仕様書
- resources/Wasm Common Lisp 動的コンパイル戦略.pdf
- resources/WebAssembly テキスト・バイナリ表現調査.pdf
