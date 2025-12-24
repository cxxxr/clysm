# Clysm実装計画: WebAssembly GCターゲットCommon Lispコンパイラ

**作成日**: 2025-12-21
**ステータス**: Draft
**憲法バージョン**: 1.0.0

## 概要

本ドキュメントは、WebAssembly GCをターゲットとしたCommon Lispコンパイラ「Clysm」の
段階的実装計画を定義する。憲法（constitution.md）の8原則に準拠し、TDDとNix-First
ワークフローを厳格に遵守する。

---

## 1. フェーズ分割: 段階的実装計画

### Phase 0: 基盤構築 (Foundation)

**目標**: 開発環境とWasmバイナリ生成基盤の確立

#### 成果物

1. **Nix Flakes環境**
   - `flake.nix`: SBCL, wasm-tools, wasmtime, wat2wasmを含むdevShell
   - `nix flake check`によるCI基盤

2. **Wasmバイナリエミッタ (低レベル)**
   - LEB128エンコーダ/デコーダ
   - セクション構造生成器（ID 0-13の正しい順序）
   - マジックナンバー・ヘッダ出力

3. **WAT生成器 (デバッグ用)**
   - S式形式のWAT出力
   - wasm-tools validateとの統合テスト

#### 検証基準

- [ ] `(module)` の空モジュールがwasm-tools validateを通過
- [ ] 単純な `(func (result i32) (i32.const 42))` が実行可能
- [ ] `nix flake check` がパス

---

### Phase 1: 最小Lispサブセット (Lisp-0)

**目標**: 即値演算と基本的な関数定義が動作する最小処理系

#### サポートするフォーム

```lisp
(+ 1 2)                    ; Fixnum算術
(- 10 3)                   ; 減算
(* 2 3)                    ; 乗算
(< x 0)                    ; 比較
(if (< x 0) -1 1)          ; 条件分岐
(let ((x 10)) x)           ; レキシカル変数
(let* ((x 1) (y x)) y)     ; 逐次束縛
(defun add (a b) (+ a b))  ; トップレベル関数
(add 1 2)                  ; 関数呼び出し
```

#### 型マッピング (WasmGC)

```wat
;; Phase 1で定義する型
(type $fixnum (ref i31))  ;; 31ビット整数

(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))))

(type $symbol (struct
  (field $name (ref $string))
  (field $value (mut anyref))
  (field $function (mut anyref))
  (field $plist (mut anyref))
  (field $package (mut anyref))))
```

#### 検証基準

- [ ] `(+ 1 (* 2 3))` => 7
- [ ] `(defun f (x) x)` + `(f 42)` => 42
- [ ] 10個以上のFixnum算術テストがパス
- [ ] 生成されたWasmがwasmtimeで実行可能

---

### Phase 2: クロージャと再帰 (Lisp-1)

**目標**: ファーストクラス関数とTail Call最適化

#### サポートするフォーム

```lisp
(lambda (x) (+ x 1))          ; 無名関数
(funcall f arg)               ; 間接呼び出し
(apply f args)                ; リスト適用
(labels ((f (x) ...)) ...)    ; ローカル関数
;; 末尾再帰
(defun fact (n)
  (labels ((iter (n acc)
             (if (= n 0) acc
                 (iter (1- n) (* n acc)))))
    (iter n 1)))
```

#### クロージャ構造体（憲法 III準拠）

```wat
(type $closure (struct
  (field $code_0 (ref null $func_0))  ;; 0引数用
  (field $code_1 (ref null $func_1))  ;; 1引数用
  (field $code_2 (ref null $func_2))  ;; 2引数用
  (field $code_N (ref null $func_N))  ;; 汎用（リスト渡し）
  (field $env (mut anyref))))
```

#### 実装タスク

1. 自由変数の収集（Free Variable Analysis）
2. 環境構造体の生成
3. 関数本体での環境参照への変換
4. アリティに応じたエントリーポイント生成
5. 末尾位置の検出（Tail Position Analysis）
6. `return_call` / `return_call_ref` への変換

#### 検証基準

- [ ] `(funcall (lambda (x) (+ x 1)) 10)` => 11
- [ ] `(fact 100)` がスタックオーバーフローなし
- [ ] クロージャによる変数キャプチャが正常動作

---

### Phase 3: 制御フローと例外処理 (Lisp-2)

**目標**: 非局所脱出とunwind-protect

#### サポートするフォーム

```lisp
(block name (return-from name 42))
(tagbody loop (go loop))
(catch 'tag (throw 'tag value))
(unwind-protect form cleanup-form)
```

#### Exception Handling実装（憲法 IV準拠）

```wat
;; block/return-from の変換
(try_table (catch $block_tag $handler)
  ;; block本体
  (throw $block_tag value))

;; unwind-protect の変換
(try_table (catch_all $cleanup)
  protected-form)
$cleanup
  cleanup-form
  (rethrow 0)
```

#### 検証基準

- [ ] `(block nil (return-from nil 42))` => 42
- [ ] `(catch 'foo (throw 'foo 123))` => 123
- [ ] `(unwind-protect (error) (cleanup))` でcleanupが実行される

---

### Phase 4: 動的スコープとリーダー (Lisp-3)

**目標**: スペシャル変数とS式読み込み

#### スペシャル変数（憲法 V準拠: シャローバインディング）

```lisp
(defvar *x* 10)
(let ((*x* 20))     ; 動的束縛
  (foo))            ; *x* は 20
;; *x* は 10 に復元
```

#### 実装戦略

```wat
;; バインディングスタック
(global $binding_stack (mut (ref null $binding_frame)))

(type $binding_frame (struct
  (field $symbol (ref $symbol))
  (field $old_value anyref)
  (field $next (ref null $binding_frame))))
```

1. シンボルの`$value`フィールドに現在値を保持
2. 束縛時: 古い値をバインディングスタック（Trail）にプッシュ
3. 復元時: `try_table`のcleanup節で古い値をポップして書き戻し

#### S式リーダー

- 文字ストリームからのトークン化
- 再帰下降パーサー
- シンボルインターン（パッケージシステムの基礎）

#### 検証基準

- [ ] `(read-from-string "(+ 1 2)")` => `(+ 1 2)`
- [ ] スペシャル変数の動的束縛が正しく動作
- [ ] 基本的なREPLが動作

---

### Phase 5: マクロシステム (Lisp-4)

**目標**: defmacroとバッククォート

#### サポートするフォーム

```lisp
(defmacro when (test &body body)
  `(if ,test (progn ,@body)))

(defmacro unless (test &body body)
  `(if (not ,test) (progn ,@body)))
```

#### 実装タスク

1. マクロ関数の登録（コンパイル時環境）
2. フォーム走査時のマクロ呼び出し検出
3. マクロ関数実行（ホストSBCLまたはWasm内インタプリタ）
4. 展開結果の再帰的処理
5. バッククォート（`quasiquote` / `unquote` / `unquote-splicing`）の変換

#### 検証基準

- [ ] `(when t 1 2 3)` => 3
- [ ] `defmacro`で定義したマクロが正しく展開される
- [ ] ネストしたバッククォートが正常動作

---

### Phase 6: Eval/JIT (Lisp-5)

**目標**: 動的コンパイル（憲法 VI準拠）

#### Tier 1: Wasm内インタプリタ

```lisp
(defun eval-form (form env)
  (cond
    ((self-evaluating-p form) form)
    ((symbolp form) (lookup form env))
    ((consp form)
     (case (car form)
       (quote (cadr form))
       (if (eval-if form env))
       (lambda (make-closure form env))
       (t (apply-function (eval (car form) env)
                          (mapcar (lambda (x) (eval x env))
                                  (cdr form))))))))
```

#### Tier 2: 動的Wasmモジュール生成

1. S式 -> AST -> Wasmバイナリ（Uint8Array）生成
2. ホストの`WebAssembly.instantiate()`呼び出し
3. メインランタイムのGCヒープをインポート
4. 新モジュールの`funcref`をシンボル関数スロットに登録

#### 動的リンク

```wat
;; 動的モジュールのインポート
(import "runtime" "heap" (global $heap (ref $gc_heap)))
(import "runtime" "intern" (func $intern (param (ref $string)) (result (ref $symbol))))
```

#### 検証基準

- [ ] `(eval '(+ 1 2))` => 3
- [ ] `(compile nil '(lambda (x) (+ x 1)))` がコンパイル済み関数を返す
- [ ] Tier 1/Tier 2の切り替えが正常動作

---

### Phase 7: CLOS基盤 (Lisp-6)

**目標**: 基本的なオブジェクトシステム

#### サポートするフォーム

```lisp
(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y)))

(defmethod distance ((p point))
  (sqrt (+ (expt (point-x p) 2)
           (expt (point-y p) 2))))
```

#### インスタンス構造体

```wat
(type $instance (struct
  (field $class (ref $standard-class))
  (field $slots (ref $slot_vector))))

(type $slot_vector (array (mut anyref)))
```

#### 総称関数ディスパッチ

1. キャッシュベースの高速パス（クラス -> メソッド）
2. キャッシュミス時の`compute-applicable-methods`
3. メソッド結合（:before, :after, :around）

#### 検証基準

- [ ] `(make-instance 'point :x 3 :y 4)` がインスタンスを生成
- [ ] `(distance p)` がメソッドディスパッチを実行
- [ ] 継承とメソッドオーバーライドが動作

---

### Phase 8: 標準ライブラリ (Lisp-7)

**目標**: ANSI Common Lisp準拠に向けた拡充

Phase 8は複数のサブフェーズに分割され、独立して実装・テスト可能。

#### Phase 8A: シーケンス関数 ✅ 完了

**ステータス**: 完了 (007-sequence-functions)

- map, reduce, remove, find, position等
- 高階関数とシーケンス操作

#### Phase 8B: 数値塔 ✅ 完了

**ステータス**: 完了 (010-numeric-tower)

- Bignum（GMP風の多倍長整数）
- Ratio（有理数）
- Float（IEEE754倍精度）
- Complex（複素数）
- 数学関数（sqrt, sin, cos等）

#### Phase 8C: FFI基盤 🔜 次フェーズ

**目標**: ホスト環境との相互運用基盤

**重要**: Phase 8D (WASI I/O) の**前提条件**として推奨

```lisp
;; FFI基本API案
(ffi:define-foreign-function "console_log"
  ((:string message)) :void)

(ffi:call-host "fd_write" fd buffer len)
```

**実装内容**:
1. Wasm Import/Export宣言生成
2. 型マーシャリング（Lisp ↔ Wasm値）
3. 外部関数呼び出しラッパー
4. コールバック（Wasm → Lisp）サポート

**代替アプローチ**: FFIを先に実装することで、I/O機能をホスト側に委譲し、
憲法の線形メモリ禁止原則を完全に遵守可能。

#### Phase 8D: WASI Stream I/O ⏸️ 中断

**ステータス**: 中断 (011-wasi-stream-io)

**⚠️ 憲法逸脱**: 本フェーズはLinear Memory使用を必要とする（後述）

**実装内容**:
- 標準入出力: write-char, write-string, read-char, read-line
- フォーマット: format（~A, ~S, ~D, ~%, ~~ディレクティブ）
- ファイルI/O: open, close, with-open-file
- 標準ストリーム: *standard-input*, *standard-output*, *error-output*

**WASI統合**:
- WASI Preview1 API使用 (fd_read, fd_write, path_open, fd_close)
- 64KB線形メモリ（iovec転送用）
- $stream WasmGC構造体によるストリーム表現

#### Phase 8E: パッケージシステム

**目標**: ANSI準拠のパッケージ管理

- defpackage, in-package, export, import
- CL, CL-USER, KEYWORDパッケージ
- シンボルインターン・ルックアップ

#### Phase 8F: 条件システム

**目標**: ANSI準拠のエラー処理

- condition, restart定義
- handler-bind, handler-case
- invoke-restart, restart-case
- 標準条件タイプ（error, warning等）

---

### 憲法逸脱ドキュメント: Phase 8D Linear Memory使用

**逸脱原則**: I. WasmGC-First型システム設計
> 「線形メモリ（Linear Memory）への依存は禁止され...」

**逸脱原則**: セキュリティ制約
> 「線形メモリへの直接アクセスは禁止（MUST NOT）」

**正当化**:

WASI Preview1 APIは、データ転送にiovec構造体を必要とし、これは線形メモリ上に
配置しなければならない。これはWASI仕様の制約であり、回避不可能である。

```wat
;; WASI fd_write シグネチャ
(import "wasi_snapshot_preview1" "fd_write"
  (func $fd_write (param i32 i32 i32 i32) (result i32)))
;; パラメータはすべて線形メモリオフセット
```

**緩和策**:

1. **隔離**: 線形メモリ使用は0-64KBの固定領域に限定
2. **カプセル化**: Lispコードから線形メモリへの直接アクセスは不可
3. **最小化**: I/Oバッファのみに使用、オブジェクト格納には使用しない
4. **将来対応**: WASI Preview2 (wit-bindgen) でComponent Model移行予定

**代替オプション**:

FFI経由でホスト側I/O実装を呼び出すことで、線形メモリ依存を完全に排除可能。
ただし、この場合はスタンドアロンWasm実行（wasmtime単体）が制限される。

---

## 2. アーキテクチャ設計: コンパイラモジュール構成

```
src/clysm/
├── reader/               # S式リーダー
│   ├── tokenizer.lisp    # 字句解析
│   ├── parser.lisp       # 構文解析
│   └── package.lisp      # シンボルインターン
│
├── compiler/             # コンパイラ本体
│   ├── ast.lisp          # 抽象構文木定義
│   ├── analyzer.lisp     # 意味解析
│   │   ├── free-vars.lisp    # 自由変数解析
│   │   ├── tail-call.lisp    # 末尾呼び出し検出
│   │   └── type-infer.lisp   # 型推論（最適化用）
│   ├── transform/        # AST変換
│   │   ├── closure.lisp      # クロージャ変換
│   │   ├── cps.lisp          # CPS変換（オプション）
│   │   └── macro.lisp        # マクロ展開
│   └── codegen/          # コード生成
│       ├── wasm-ir.lisp      # Wasm中間表現
│       ├── type-section.lisp # Type Section生成
│       ├── func-section.lisp # Function/Code Section
│       └── gc-types.lisp     # WasmGC型定義
│
├── runtime/              # ランタイムサポート
│   ├── objects.lisp      # オブジェクト表現
│   ├── gc-bridge.lisp    # GC連携
│   ├── special-vars.lisp # シャローバインディング
│   └── multi-value.lisp  # 多値バッファ
│
├── backend/              # バイナリ出力
│   ├── wasm-emit.lisp    # Wasmバイナリエミッタ
│   ├── leb128.lisp       # LEB128エンコーディング
│   ├── sections.lisp     # セクション構造
│   └── wat-print.lisp    # WAT出力（デバッグ用）
│
├── eval/                 # 動的評価
│   ├── interpreter.lisp  # Tier 1インタプリタ
│   └── jit.lisp          # Tier 2 JITコンパイラ
│
├── clos/                 # オブジェクトシステム
│   ├── mop.lisp          # メタオブジェクトプロトコル
│   ├── dispatch.lisp     # メソッドディスパッチ
│   └── slot-access.lisp  # スロットアクセス
│
└── test/                 # テストスイート
    ├── unit/             # 単体テスト
    ├── contract/         # 契約テスト（Wasm検証）
    └── integration/      # 統合テスト
```

---

## 3. ブートストラップ戦略

### ステージ1: ホストSBCLによるクロスコンパイル

```
SBCL (ホスト) --> Clysm Compiler --> Wasm Module
                        |
                        v
                   [wasmtime実行]
```

- コンパイラ本体はSBCLで実行
- 生成されたWasmをwasmtimeで検証・実行
- この段階ではREPLは使用不可

### ステージ2: 最小ランタイムの自己ホスト準備

- Wasm内インタプリタ（Tier 1 Eval）を実装
- コンパイラのコア部分をClysm自身で書き直し可能に

### ステージ3: セルフホスティング（長期目標）

```
Clysm (Wasm) --> Clysm Compiler (Wasm) --> Wasm Module
```

---

## 4. 優先順位マップ

```
Phase 0 [基盤]
    │
    ▼
Phase 1 [Fixnum, 関数, 条件分岐]  ◀── 最小動作可能
    │
    ▼
Phase 2 [クロージャ, Tail Call]   ◀── 関数型プログラミング可能
    │
    ▼
Phase 3 [例外, unwind-protect]    ◀── 堅牢なエラー処理
    │
    ▼
Phase 4 [スペシャル変数, リーダー] ◀── 対話的開発の基礎
    │
    ▼
Phase 5 [マクロ]                  ◀── Lisp的抽象化
    │
    ▼
Phase 6 [Eval/JIT]                ◀── 完全な動的性
    │
    ▼
Phase 7 [CLOS]                    ◀── オブジェクト指向
    │
    ▼
Phase 8 [標準ライブラリ]          ◀── ANSI準拠
    ├── 8A [シーケンス関数] ✅
    ├── 8B [数値塔] ✅
    ├── 8C [FFI基盤] 🔜 ◀── ホスト連携
    ├── 8D [WASI Stream I/O] ⏸️ ◀── 標準入出力・ファイル（中断）
    ├── 8E [パッケージ]
    └── 8F [条件システム]
```

---

## 5. リスク分析と対策

### 高リスク

| リスク | 影響度 | 対策 |
|--------|--------|------|
| WasmGC仕様の不安定性 | 高 | wasmtimeを参照実装として追従。仕様変更時の抽象層を設ける |
| アリティディスパッチの性能 | 高 | 0-2引数の専用エントリーポイントで高速化。ベンチマーク駆動で調整 |
| 動的JITのモジュール間リンク | 高 | インポート/エクスポートの設計を早期に検証。Guile Hoot参照 |

### 中リスク

| リスク | 影響度 | 対策 |
|--------|--------|------|
| Bignum実装の複雑さ | 中 | 初期はi31ref範囲内に限定。段階的に拡張 |
| CLOS MOPの完全実装 | 中 | PCL/Closerベースの実装を参考に最小限から開始 |
| マルチスレッド対応 | 中 | 当面シングルスレッド前提。将来のWasm Threadsに備えた設計 |

### 低リスク

| リスク | 影響度 | 対策 |
|--------|--------|------|
| テキスト形式(WAT)の互換性 | 低 | バイナリ直接生成を主とし、WATはデバッグ用 |
| ブラウザ間の差異 | 低 | wasmtimeでの動作を優先。ブラウザ対応は後続フェーズ |

---

## 6. マイルストーンと成功指標

### M0: 開発環境確立 (Phase 0完了)

- [ ] `nix develop`で全ツールが利用可能
- [ ] 空モジュールの生成・検証パス
- [ ] CI/CDパイプライン構築

### M1: 最小Lisp動作 (Phase 1完了)

- [ ] `(+ 1 2)` => 3 の実行
- [ ] `(defun f (x) x)` + `(f 42)` => 42
- [ ] 10個以上のFixnum算術テストがパス

### M2: 関数型プログラミング (Phase 2完了)

- [ ] `(funcall (lambda (x) x) 42)` => 42
- [ ] `(fact 100)` がスタックオーバーフローなし
- [ ] クロージャによる変数キャプチャ

### M3: 堅牢な制御フロー (Phase 3完了)

- [ ] `block/return-from` の動作
- [ ] `unwind-protect` の保証

### M4: 対話的開発基盤 (Phase 4完了)

- [ ] REPLの基本動作
- [ ] `(read)` + `(eval)` + `(print)` のサイクル
- [ ] スペシャル変数の正常動作

### M5: Lisp的抽象化 (Phase 5完了)

- [ ] `defmacro` による構文拡張
- [ ] 標準的な制御マクロ（when, unless, dolist等）

### M6: 完全な動的性 (Phase 6完了)

- [ ] `(eval '(+ 1 2))` => 3
- [ ] `(compile)` によるJITコンパイル
- [ ] Tier 1/Tier 2の切り替え

### M7: オブジェクト指向 (Phase 7完了)

- [ ] `defclass` / `make-instance`
- [ ] `defmethod` / 総称関数ディスパッチ
- [ ] 継承とメソッド結合

### M8: 標準準拠 (Phase 8完了)

**サブマイルストーン**:

#### M8A: シーケンス関数 ✅ 完了
- [x] map, mapcar, mapc実装
- [x] reduce, remove, find, position実装
- [x] 高階シーケンス関数テストパス

#### M8B: 数値塔 ✅ 完了
- [x] Bignum（多倍長整数）実装
- [x] Ratio（有理数）実装
- [x] Float（IEEE754）実装
- [x] Complex（複素数）実装
- [x] 数学関数（sqrt, sin, cos等）

#### M8C: FFI基盤
- [ ] Wasm Import/Export宣言生成
- [ ] 型マーシャリング実装
- [ ] 外部関数呼び出しラッパー

#### M8D: WASI Stream I/O ⏸️ 中断
- [ ] write-char, write-string実装
- [ ] read-char, read-line実装
- [ ] format基本ディレクティブ
- [ ] open, close, with-open-file
- [ ] wasmtimeでI/O動作確認

#### M8E: パッケージシステム
- [ ] defpackage, in-package実装
- [ ] シンボルインターン・エクスポート

#### M8F: 条件システム
- [ ] condition/restart基盤
- [ ] handler-bind, handler-case

**最終目標**:
- [ ] ANSI CL準拠テストスイートの一定割合パス
- [ ] 実用的なアプリケーション構築可能

---

## 7. 技術的詳細

### WasmGC型マッピング一覧

| Lisp型 | WasmGC型 | 生成命令 |
|--------|----------|---------|
| Fixnum | `(ref i31)` | `ref.i31` / `i31.get_s` |
| Cons | `(ref $cons)` | `struct.new $cons` |
| Symbol | `(ref $symbol)` | `struct.new $symbol` |
| Closure | `(ref $closure)` | `struct.new $closure` |
| String | `(ref $string)` (array i8) | `array.new` |
| Vector | `(ref $vector)` (array anyref) | `array.new` |
| NIL | シングルトン`(ref $nil)` | グローバル参照 |
| UNBOUND | センチネル`(ref $unbound)` | グローバル参照 |

### クロージャ変換例

```lisp
;; 入力
(lambda (x y) (+ x y free-var))

;; クロージャ変換後（概念）
(struct.new $closure
  null                   ; 0引数はnull
  null                   ; 1引数はnull
  (ref.func $entry_2)    ; 2引数エントリーポイント
  (ref.func $entry_N)    ; 汎用エントリーポイント
  (struct.new $env       ; 環境
    free-var))
```

### 動的コンパイルフロー

```
[Lispフォーム] --> [AST] --> [Wasmバイナリ (Uint8Array)]
                                    │
                                    ▼
                    [ホスト: WebAssembly.instantiate()]
                                    │
                                    ▼
                    [新モジュール funcref取得]
                                    │
                                    ▼
                    [シンボル $function スロットに登録]
```

---

## 8. 参照ドキュメント

- `.specify/memory/constitution.md` - プロジェクト憲法
- `resources/Wasm Common Lisp 動的コンパイル戦略.pdf` - 技術詳細
- `resources/WebAssembly テキスト・バイナリ表現調査.pdf` - Wasm仕様参照

---

## 変更履歴

| バージョン | 日付 | 変更内容 |
|------------|------|----------|
| 0.1.0 | 2025-12-21 | 初版作成 |
| 0.2.0 | 2025-12-24 | Phase 8をサブフェーズ(8A-8F)に分割、8A/8B完了、憲法逸脱ドキュメント追加 |
| 0.2.1 | 2025-12-24 | Phase 8D (WASI Stream I/O) を中断 |
