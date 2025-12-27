# Clysm実装計画: WebAssembly GCターゲットCommon Lispコンパイラ

**作成日**: 2025-12-21
**更新日**: 2025-12-27
**ステータス**: Phase 8完了 → Self-Hosting開始
**憲法バージョン**: 1.0.0

## 概要

本ドキュメントは、WebAssembly GCをターゲットとしたCommon Lispコンパイラ「Clysm」の
段階的実装計画を定義する。憲法（constitution.md）の8原則に準拠し、TDDとNix-First
ワークフローを厳格に遵守する。

**最終目標**: セルフホスティング — Clysm自身がClysm Compilerをコンパイル可能にする

---

## 1. フェーズ分割: 段階的実装計画

### Phase 0: 基盤構築 (Foundation) ✅ 完了

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

- [x] `(module)` の空モジュールがwasm-tools validateを通過
- [x] 単純な `(func (result i32) (i32.const 42))` が実行可能
- [x] `nix flake check` がパス

---

### Phase 1: 最小Lispサブセット (Lisp-0) ✅ 完了

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

- [x] `(+ 1 (* 2 3))` => 7
- [x] `(defun f (x) x)` + `(f 42)` => 42
- [x] 10個以上のFixnum算術テストがパス
- [x] 生成されたWasmがwasmtimeで実行可能

---

### Phase 2: クロージャと再帰 (Lisp-1) ✅ 完了

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

#### 検証基準

- [x] `(funcall (lambda (x) (+ x 1)) 10)` => 11
- [x] `(fact 100)` がスタックオーバーフローなし
- [x] クロージャによる変数キャプチャが正常動作

---

### Phase 3: 制御フローと例外処理 (Lisp-2) ✅ 完了

**目標**: 非局所脱出とunwind-protect

#### サポートするフォーム

```lisp
(block name (return-from name 42))
(tagbody loop (go loop))
(catch 'tag (throw 'tag value))
(unwind-protect form cleanup-form)
```

#### 検証基準

- [x] `(block nil (return-from nil 42))` => 42
- [x] `(catch 'foo (throw 'foo 123))` => 123
- [x] `(unwind-protect (error) (cleanup))` でcleanupが実行される

---

### Phase 4: 動的スコープとリーダー (Lisp-3) ✅ 完了

**目標**: スペシャル変数とS式読み込み

#### 検証基準

- [x] `(read-from-string "(+ 1 2)")` => `(+ 1 2)`
- [x] スペシャル変数の動的束縛が正しく動作
- [x] 基本的なREPLが動作

---

### Phase 5: マクロシステム (Lisp-4) ✅ 完了

**目標**: defmacroとバッククォート

#### サポートするフォーム

```lisp
(defmacro when (test &body body)
  `(if ,test (progn ,@body)))

(defmacro unless (test &body body)
  `(if (not ,test) (progn ,@body)))
```

#### 検証基準

- [x] `(when t 1 2 3)` => 3
- [x] `defmacro`で定義したマクロが正しく展開される
- [x] ネストしたバッククォートが正常動作

---

### Phase 6: Eval/JIT (Lisp-5) ✅ 完了

**目標**: 動的コンパイル（憲法 VI準拠）

**ステータス**: 完了 (017-eval-jit-compile)

#### 検証基準

- [x] `(eval '(+ 1 2))` => 3
- [x] `(compile nil '(lambda (x) (+ x 1)))` がコンパイル済み関数を返す
- [x] Tier 1/Tier 2の切り替えが正常動作

---

### Phase 7: CLOS基盤 (Lisp-6) ✅ 完了

**目標**: 基本的なオブジェクトシステム

**ステータス**: 完了 (026-clos-foundation)

#### サポートするフォーム

```lisp
(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y)))

(defmethod distance ((p point))
  (sqrt (+ (expt (point-x p) 2)
           (expt (point-y p) 2))))
```

#### 検証基準

- [x] `(make-instance 'point :x 3 :y 4)` がインスタンスを生成
- [x] `(distance p)` がメソッドディスパッチを実行
- [x] 継承とメソッドオーバーライドが動作

---

### Phase 8: 標準ライブラリ (Lisp-7) ✅ 完了

**目標**: ANSI Common Lisp準拠に向けた拡充

| サブフェーズ | ステータス | 機能領域 |
|-------------|-----------|---------|
| Phase 8A | ✅ 完了 | シーケンス関数 (007) |
| Phase 8B | ✅ 完了 | 数値塔 (010) |
| Phase 8C | ✅ 完了 | FFI基盤 (027) |
| Phase 8D | ⏸️ 中断 | WASI Stream I/O (FFI代替で実装済み) |
| Phase 8E | ✅ 完了 | パッケージシステム (013) |
| Phase 8F | ✅ 完了 | 条件システム (014) |
| Phase 8G | ✅ 完了 | 数値アクセサ (019) |
| Phase 8H | ✅ 完了 | Setf/汎変数 (028) |
| Phase 8I | ✅ 完了 | LOOPマクロ (029) |

---

## 2. セルフホスティングフェーズ (Phase 9-13)

### Phase 9: セルフホスティング基盤 (Lisp-8) 🔜 次フェーズ

**目標**: コンパイラが使用するCL機能のうち、未実装のものを補完

#### 9A: 型システム拡張

コンパイラは`typecase`/`etypecase`を892箇所で使用。これが最大のブロッカー。

```lisp
;; 実装が必要な型分岐マクロ
(typecase obj
  (fixnum ...)
  (string ...)
  (cons ...))

(etypecase obj        ; exhaustive（網羅的）版
  (fixnum ...)
  (string ...))

(check-type x fixnum) ; 型検証
```

**タスク**:
- [ ] `typecase` マクロ実装（`cond` + `typep` 展開）
- [ ] `etypecase` マクロ実装（エラー付き）
- [ ] `ctypecase` マクロ実装（再試行可能）
- [ ] `check-type` マクロ実装

#### 9B: 分解束縛

```lisp
;; destructuring-bind: マクロ引数解析で9箇所使用
(destructuring-bind (a b &rest rest) list
  ...)
```

**タスク**:
- [ ] `destructuring-bind` マクロ実装
- [ ] `&optional`, `&rest`, `&key`, `&body` サポート
- [ ] ネスト構造分解サポート

#### 9C: 高度なdefmacro

現在の`defmacro`はホストSBCLで実行。Wasm内でマクロ展開するには拡張が必要。

**タスク**:
- [ ] `defmacro` をAST内で直接パース可能に
- [ ] マクロラムダリスト完全サポート（`&whole`, `&environment`）
- [ ] コンパイル時マクロ展開器

#### 9D: FORMAT基盤

コンパイラは93箇所で`format`を使用（エラーメッセージ、デバッグ出力）。

```lisp
;; 必要なディレクティブ
(format nil "~A ~S ~D ~%" obj obj num)
(format nil "~{~A~^, ~}" list)
```

**必須ディレクティブ**:
- `~A` (aesthetic)
- `~S` (standard / prin1形式)
- `~D` (decimal integer)
- `~%` (newline)
- `~&` (fresh-line)
- `~~` (literal tilde)

**拡張ディレクティブ** (優先度中):
- `~{~}` (iteration)
- `~[~]` (conditional)
- `~?` (recursive processing)

**タスク**:
- [ ] FORMAT基本ディレクティブ実装
- [ ] FORMAT反復ディレクティブ実装
- [ ] FORMAT条件ディレクティブ実装

#### 検証基準

- [ ] `(typecase 42 (fixnum :fix) (t :other))` => :fix
- [ ] `(destructuring-bind (a b) '(1 2) (+ a b))` => 3
- [ ] `(format nil "~A + ~A = ~D" 1 2 3)` => "1 + 2 = 3"
- [ ] コンパイラの`func-section.lisp`内の`etypecase`が動作

---

### Phase 10: SBCL非依存化 (Lisp-9)

**目標**: ホスト固有機能の排除

#### 10A: IEEE 754 ビット抽出

コンパイラはSBCL固有関数で浮動小数点リテラルをWasmバイナリにエンコード。

```lisp
;; 現在の実装 (SBCL依存)
(sb-kernel:single-float-bits 3.14)  ; => ビットパターン
(sb-kernel:double-float-bits 3.14d0)

;; 置換が必要
(sb-int:with-float-traps-masked (:overflow :invalid)
  (/ 1.0 0.0))  ; => +Infinity
```

**代替実装オプション**:

1. **FFI経由 (推奨)**: ホストJavaScript/wasmtimeのDataView APIを使用
   ```lisp
   (ffi:call-host "float64-to-bits" 3.14d0) ; => i64
   ```

2. **純Lisp実装**: IEEE 754手動エンコード（複雑だが移植性高）

3. **コンパイル時定数テーブル**: 一般的な浮動小数点定数を事前計算

**タスク**:
- [ ] `float-to-bits` FFI関数実装
- [ ] `bits-to-float` FFI関数実装
- [ ] 浮動小数点トラップのポータブル処理
- [ ] `compiler/compiler.lisp:1118-1135` の書き換え

#### 10B: UTF-8エンコーディング (Babel代替)

```lisp
;; 現在: babel:string-to-octets を6箇所で使用
(babel:string-to-octets "hello" :encoding :utf-8)

;; 代替: Clysm内蔵UTF-8エンコーダ
(clysm:string-to-utf8-octets "hello")
```

**タスク**:
- [ ] `string-to-utf8-octets` 関数実装
- [ ] `utf8-octets-to-string` 関数実装
- [ ] サロゲートペア対応
- [ ] Babel依存箇所の置換

#### 10C: ファイルシステムアクセス

コンパイラは`with-open-file`を13箇所で使用。

**実装戦略**:
- ブラウザ環境: Virtual FSまたはIndexedDB
- wasmtime環境: WASI Preview2経由
- 開発時: ホストFFI経由

**タスク**:
- [ ] `open-file` FFI実装
- [ ] `close-file` FFI実装
- [ ] `read-file-contents` FFI実装
- [ ] `write-file-contents` FFI実装
- [ ] `with-open-file` マクロ（FFIラッパー）

#### 検証基準

- [ ] `(float-to-bits 3.14d0)` が正しいIEEE 754ビットパターンを返す
- [ ] `(string-to-utf8-octets "日本語")` が正しいバイト列を返す
- [ ] ファイル読み書きがwasmtime上で動作
- [ ] SBCL固有関数の使用箇所がゼロ

---

### Phase 11: コンパイラサブセット検証 (Lisp-10)

**目標**: コンパイラの各モジュールがClysm自身でコンパイル可能か検証

#### 11A: モジュール別コンパイル可能性分析

| モジュール | ファイル数 | 主要依存機能 | 難易度 |
|-----------|-----------|-------------|-------|
| `backend/` | 4 | loop, defstruct, format | 低 |
| `reader/` | 4 | loop, cond, push | 低 |
| `compiler/ast.lisp` | 1 | defstruct :include, typecase | 中 |
| `compiler/codegen/` | 5 | etypecase, 再帰, format | 高 |
| `runtime/` | 4 | defstruct, loop | 低 |
| `clos/` | 8 | defclass, defmethod | 中 |
| `conditions/` | 6 | handler-case, restart-case | 中 |

#### 11B: 段階的コンパイル

```
Step 1: backend/leb128.lisp     (依存なし)
Step 2: backend/sections.lisp   (leb128依存)
Step 3: reader/tokenizer.lisp   (依存なし)
Step 4: reader/parser.lisp      (tokenizer依存)
Step 5: compiler/ast.lisp       (依存なし)
Step 6: compiler/codegen/*      (ast依存)
Step 7: compiler/compiler.lisp  (全依存)
```

**タスク**:
- [ ] 各モジュールの依存関係グラフ作成
- [ ] `blessed-subset.lisp` 定義（自己コンパイル可能なCLサブセット）
- [ ] 段階1-3のコンパイル成功確認
- [ ] 段階4-7のコンパイル成功確認

#### 11C: 互換性テストスイート

```lisp
;; コンパイラ自身のテスト
(deftest compiler-compiles-leb128
  (let ((wasm (compile-file "src/clysm/backend/leb128.lisp")))
    (is (valid-wasm-p wasm))))
```

**タスク**:
- [ ] モジュール単位コンパイルテスト
- [ ] 出力Wasm検証テスト
- [ ] 実行時動作テスト

#### 検証基準

- [ ] `backend/` 全ファイルがClysm自身でコンパイル可能
- [ ] `reader/` 全ファイルがClysm自身でコンパイル可能
- [ ] `compiler/` 全ファイルがClysm自身でコンパイル可能
- [ ] 生成されたWasmがwasm-tools validateを通過

---

### Phase 12: クロスコンパイル (Lisp-11)

**目標**: SBCLでClysm Compilerをコンパイルし、Wasm版Clysm Compilerを生成

#### 12A: ブートストラップ環境構築

```
┌─────────────┐    コンパイル     ┌─────────────────┐
│ SBCL (Host) │ ───────────────→ │ Clysm.wasm (T0) │
│ + Clysm CL  │                   │ (Stage 0)       │
└─────────────┘                   └─────────────────┘
```

**タスク**:
- [ ] 全コンパイラソースのファイルリスト生成
- [ ] ロード順序の決定（依存関係ソート）
- [ ] 単一Wasmモジュールへの統合
- [ ] ランタイム初期化コード生成

#### 12B: Stage 0コンパイラ生成

```bash
# Stage 0: SBCLでコンパイル
sbcl --load "build/bootstrap.lisp" --eval "(build-clysm-wasm)"
# => dist/clysm-stage0.wasm
```

**タスク**:
- [ ] `build/bootstrap.lisp` スクリプト作成
- [ ] コンパイル時環境の分離
- [ ] デバッグシンボル出力オプション
- [ ] `dist/clysm-stage0.wasm` 生成

#### 12C: Stage 0検証

```lisp
;; Stage 0コンパイラで簡単なプログラムをコンパイル
(run-wasm "clysm-stage0.wasm"
  :input "(compile nil '(lambda (x) (+ x 1)))")
;; => 有効なWasmバイナリ
```

**タスク**:
- [ ] 基本算術コンパイルテスト
- [ ] 関数定義コンパイルテスト
- [ ] 制御フローコンパイルテスト
- [ ] SBCL版との出力比較

#### 検証基準

- [ ] `clysm-stage0.wasm` がwasmtimeで実行可能
- [ ] Stage 0コンパイラが`(+ 1 2)`をコンパイル可能
- [ ] Stage 0の出力がSBCL版と同一（ビット単位）

---

### Phase 13: 完全セルフホスティング (Lisp-12)

**目標**: Wasm版ClysmsがClysm自身をコンパイル可能

#### 13A: Stage 1コンパイラ生成

```
┌─────────────────┐    コンパイル     ┌─────────────────┐
│ Clysm.wasm (T0) │ ───────────────→ │ Clysm.wasm (T1) │
│ (Stage 0)       │                   │ (Stage 1)       │
└─────────────────┘                   └─────────────────┘
```

**タスク**:
- [ ] Stage 0でコンパイラソースを読み込み
- [ ] Stage 1 Wasmバイナリ生成
- [ ] Stage 0とStage 1の比較

#### 13B: 固定点検証

```
T0 (SBCL生成) → T1 (T0生成) → T2 (T1生成)
                              ↓
                         T1 == T2 ならば固定点達成
```

**タスク**:
- [ ] Stage 2コンパイラ生成
- [ ] Stage 1とStage 2のビット単位比較
- [ ] 固定点達成の確認

#### 13C: 開発ワークフロー確立

```bash
# セルフホスティング開発サイクル
./clysm compile src/clysm/**/*.lisp -o clysm-next.wasm
./clysm-next compile tests/**/*.lisp -o tests.wasm
wasmtime tests.wasm
```

**タスク**:
- [ ] CLIインターフェース実装
- [ ] インクリメンタルコンパイル
- [ ] エラーリカバリ
- [ ] REPLからのコンパイル

#### 検証基準

- [ ] Stage 1コンパイラが生成可能
- [ ] Stage 1 == Stage 2 (固定点)
- [ ] SBCLなしでClysm開発が可能
- [ ] 新機能がセルフホスト環境で開発可能

---

## 3. アーキテクチャ設計: コンパイラモジュール構成

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
├── lib/                  # 標準ライブラリ
│   ├── macros.lisp       # 標準マクロ（LOOP等）
│   └── setf-expanders.lisp # SETF展開器
│
├── ffi/                  # Foreign Function Interface
│   ├── marshalling.lisp  # 型変換
│   ├── import-gen.lisp   # インポート生成
│   └── export-gen.lisp   # エクスポート生成
│
└── conditions/           # 条件システム
    ├── types.lisp        # 条件型定義
    ├── handlers.lisp     # ハンドラ
    └── restarts.lisp     # リスタート
```

---

## 4. ブートストラップ戦略

### ステージ1: ホストSBCLによるクロスコンパイル ✅ 現在

```
SBCL (ホスト) --> Clysm Compiler --> Wasm Module
                        |
                        v
                   [wasmtime実行]
```

- コンパイラ本体はSBCLで実行
- 生成されたWasmをwasmtimeで検証・実行

### ステージ2: Stage 0生成 (Phase 12)

```
SBCL (ホスト)
      |
      v
┌─────────────────────────────────┐
│ Clysm Compiler (全ソース)        │
│ - reader/*.lisp                 │
│ - compiler/*.lisp               │
│ - backend/*.lisp                │
│ - runtime/*.lisp                │
│ - ...                           │
└─────────────────────────────────┘
      |
      v
  clysm-stage0.wasm
```

### ステージ3: セルフホスティング (Phase 13)

```
clysm-stage0.wasm
      |
      | (自身のソースをコンパイル)
      v
clysm-stage1.wasm
      |
      | (自身のソースをコンパイル)
      v
clysm-stage2.wasm
      |
      v
 stage1.wasm == stage2.wasm → 固定点達成!
```

---

## 5. 優先順位マップ

```
Phase 0-8 [言語機能] ✅ 完了
    │
    ▼
Phase 9 [セルフホスティング基盤]     ◀── 次フェーズ
    ├── 9A [typecase/etypecase] 🎯 最優先
    ├── 9B [destructuring-bind]
    ├── 9C [高度なdefmacro]
    └── 9D [FORMAT]
    │
    ▼
Phase 10 [SBCL非依存化]
    ├── 10A [IEEE 754ビット抽出] 🎯 ブロッカー
    ├── 10B [UTF-8エンコーディング]
    └── 10C [ファイルI/O]
    │
    ▼
Phase 11 [コンパイラサブセット検証]
    ├── 11A [モジュール別分析]
    ├── 11B [段階的コンパイル]
    └── 11C [互換性テスト]
    │
    ▼
Phase 12 [クロスコンパイル]
    ├── 12A [ブートストラップ環境]
    ├── 12B [Stage 0生成]
    └── 12C [Stage 0検証]
    │
    ▼
Phase 13 [完全セルフホスティング]     ◀── 最終目標
    ├── 13A [Stage 1生成]
    ├── 13B [固定点検証]
    └── 13C [開発ワークフロー]
```

---

## 6. リスク分析と対策

### 高リスク (セルフホスティング固有)

| リスク | 影響度 | 対策 |
|--------|--------|------|
| SBCL依存の見落とし | 高 | 静的解析で`sb-*`パッケージ使用を検出。CI に組み込み |
| IEEE 754エンコードの精度 | 高 | 参照実装との比較テスト。edge case (subnormal, NaN) を網羅 |
| コンパイラ自己参照バグ | 高 | Stage 0/1/2の差分分析。バイナリdiff |
| メモリ使用量超過 | 中 | wasmtimeのメモリ制限設定。GC頻度調整 |
| ブートストラップ時間 | 中 | インクリメンタルコンパイル。キャッシュ機構 |

### 中リスク

| リスク | 影響度 | 対策 |
|--------|--------|------|
| `etypecase`の網羅性 | 中 | 型チェッカーでコンパイル時検証 |
| FORMAT実装の複雑さ | 中 | 必須ディレクティブのみ初期実装。段階拡張 |
| ファイルI/O抽象化 | 中 | 環境別バックエンド。統一インターフェース |

---

## 7. マイルストーンと成功指標

### M0-M8: 言語機能 ✅ 完了

（既存マイルストーン - すべて達成済み）

### M9: セルフホスティング基盤 (Phase 9完了)

- [ ] `typecase`/`etypecase` が動作
- [ ] `destructuring-bind` が動作
- [ ] 基本FORMAT (`~A ~S ~D ~%`) が動作
- [ ] コンパイラの50%がClysm自身でコンパイル可能

### M10: SBCL非依存 (Phase 10完了)

- [ ] `sb-kernel:*-float-bits` 代替実装完了
- [ ] `sb-int:with-float-traps-masked` 代替完了
- [ ] Babel依存排除完了
- [ ] SBCL固有コード使用箇所 = 0

### M11: コンパイラ検証 (Phase 11完了)

- [ ] 全コンパイラモジュールがClysm自身でコンパイル可能
- [ ] 生成Wasmが全てvalidation通過
- [ ] 基本機能の動作確認テスト全パス

### M12: クロスコンパイル成功 (Phase 12完了)

- [ ] `clysm-stage0.wasm` 生成成功
- [ ] Stage 0が`(+ 1 2)`をコンパイル可能
- [ ] Stage 0とSBCL版の出力が一致

### M13: セルフホスティング達成 (Phase 13完了)

- [ ] Stage 1生成成功
- [ ] Stage 1 == Stage 2 (固定点)
- [ ] SBCLなしで新機能開発可能
- [ ] セルフホスト環境でのCI/CD確立

---

## 8. 技術的詳細

### セルフホスティングに必要なCL機能マトリックス

| 機能 | コンパイラ使用数 | Clysm状態 | Phase |
|------|-----------------|----------|-------|
| `defstruct` | 184 | ✅ (CLOS経由) | - |
| `loop` | 121 | ✅ | 8I |
| `format` | 93 | ❌ | 9D |
| `typecase` | 892* | ❌ | 9A |
| `defmacro` | 36 | 部分的 | 9C |
| `destructuring-bind` | 9 | ❌ | 9B |
| `handler-case` | 多数 | ✅ | 8F |
| `with-open-file` | 13 | ❌ | 10C |

*`etypecase`と`check-type`含む

### SBCL依存箇所

```lisp
;; compiler/ast.lisp:699
(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero)
  ...)

;; compiler/compiler.lisp:1118
(sb-kernel:single-float-bits f)

;; compiler/compiler.lisp:1127
(sb-kernel:double-float-bits f)
```

### Babel依存箇所

```lisp
;; 6箇所で使用
(babel:string-to-octets str :encoding :utf-8)
```

---

## 9. 参照ドキュメント

- `.specify/memory/constitution.md` - プロジェクト憲法
- `.specify/memory/self-hosting-analysis.md` - セルフホスティング分析 (新規作成予定)
- `resources/Wasm Common Lisp 動的コンパイル戦略.pdf` - 技術詳細
- `resources/WebAssembly テキスト・バイナリ表現調査.pdf` - Wasm仕様参照

---

## 変更履歴

| バージョン | 日付 | 変更内容 |
|------------|------|----------|
| 0.1.0 | 2025-12-21 | 初版作成 |
| 0.2.0 | 2025-12-24 | Phase 8をサブフェーズ(8A-8F)に分割、8A/8B完了、憲法逸脱ドキュメント追加 |
| 0.2.1 | 2025-12-24 | Phase 8D (WASI Stream I/O) を中断 |
| 0.3.0 | 2025-12-25 | 020-ansi-test: ANSIテストハーネス基盤完成 |
| 0.3.1 | 2025-12-26 | 022-wasm-import-optimization: FFI Import条件付き出力完了 |
| 0.4.0 | 2025-12-27 | Phase 0-8完了確認。Phase 8G-8I追加 |
| **1.0.0** | **2025-12-27** | **セルフホスティングフェーズ (Phase 9-13) 追加。最終目標をセルフホスティングに設定** |
