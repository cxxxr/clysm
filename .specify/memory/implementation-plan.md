# Clysm実装計画: WebAssembly GCターゲットCommon Lispコンパイラ

**作成日**: 2025-12-21
**更新日**: 2025-12-29
**ステータス**: Phase 13 インフラ完了 → ANSI CL関数プリミティブ実装が必要
**憲法バージョン**: 1.0.0
**ANSI準拠率**: 23.4% (219/936テスト)

## ⚠️ 重要: セルフホスティング検証結果 (2025-12-28)

**Fixed-Point検証は形式的に成功**しているが、**実質的なセルフホスティングは未達成**。

### 検証結果

```json
{
  "status": "ACHIEVED",
  "stage1": { "size_bytes": 17, "valid": true },
  "stage2": { "size_bytes": 17, "valid": true },
  "comparison": { "identical": true }
}
```

### 問題点

| ステージ | サイズ | 実際の内容 |
|---------|--------|-----------|
| Stage 0 | 275 bytes | 型定義 + **スタブ関数のみ** |
| Stage 1 | 17 bytes | **空モジュール** `(module)` |
| Stage 2 | 17 bytes | **空モジュール** `(module)` |

**Stage 0の実態**:
```wat
(export "compile_form" (func 0))
(func (;0;) (param externref) (result externref)
  ref.null extern  ;; ← 何もしない
)
```

### 結論

- ブートストラップ**インフラ**は完成（検証スクリプト、ステージ生成パイプライン）
- しかし**コンパイラロジック**がWasmに実装されていない
- `Stage 1 == Stage 2 == 空` は自明な固定点であり、意味がない
- **Phase 13は「インフラ完了」であり「セルフホスティング達成」ではない**

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

### Phase 9: セルフホスティング基盤 (Lisp-8) ✅ 完了

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
- [x] `typecase` マクロ実装（`cond` + `typep` 展開）
- [x] `etypecase` マクロ実装（エラー付き）
- [x] `ctypecase` マクロ実装（再試行可能）
- [x] `check-type` マクロ実装

#### 9B: 分解束縛

```lisp
;; destructuring-bind: マクロ引数解析で9箇所使用
(destructuring-bind (a b &rest rest) list
  ...)
```

**タスク**:
- [x] `destructuring-bind` マクロ実装
- [x] `&optional`, `&rest`, `&key`, `&body` サポート
- [x] ネスト構造分解サポート

#### 9C: 高度なdefmacro ✅ 完了 (Feature 042)

現在の`defmacro`はホストSBCLで実行。Wasm内でマクロ展開するには拡張が必要。

**タスク**:
- [x] `defmacro` をAST内で直接パース可能に
- [x] マクロラムダリスト完全サポート（`&whole`, `&environment`）
- [x] コンパイル時マクロ展開器

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
- [x] FORMAT基本ディレクティブ実装
- [x] FORMAT反復ディレクティブ実装
- [x] FORMAT条件ディレクティブ実装

#### 検証基準

- [x] `(typecase 42 (fixnum :fix) (t :other))` => :fix
- [x] `(destructuring-bind (a b) '(1 2) (+ a b))` => 3
- [x] `(format nil "~A + ~A = ~D" 1 2 3)` => "1 + 2 = 3"
- [x] コンパイラの`func-section.lisp`内の`etypecase`が動作

---

### Phase 10: SBCL非依存化 (Lisp-9) ✅ 完了

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
- [x] `float-to-bits` FFI関数実装
- [x] `bits-to-float` FFI関数実装
- [x] 浮動小数点トラップのポータブル処理
- [x] `compiler/compiler.lisp:1118-1135` の書き換え

#### 10B: UTF-8エンコーディング (Babel代替)

```lisp
;; 現在: babel:string-to-octets を6箇所で使用
(babel:string-to-octets "hello" :encoding :utf-8)

;; 代替: Clysm内蔵UTF-8エンコーダ
(clysm:string-to-utf8-octets "hello")
```

**タスク**:
- [x] `string-to-utf8-octets` 関数実装
- [x] `utf8-octets-to-string` 関数実装
- [x] サロゲートペア対応
- [x] Babel依存箇所の置換

#### 10C: ファイルシステムアクセス ✅ 完了 (Feature 035)

コンパイラは`with-open-file`を13箇所で使用。

**実装戦略**:
- ブラウザ環境: Virtual FSまたはIndexedDB
- wasmtime環境: WASI Preview2経由
- 開発時: ホストFFI経由

**タスク**:
- [x] `open-file` FFI実装
- [x] `close-file` FFI実装
- [x] `read-file-contents` FFI実装
- [x] `write-file-contents` FFI実装
- [x] `with-open-file` マクロ（FFIラッパー）

#### 検証基準

- [x] `(float-to-bits 3.14d0)` が正しいIEEE 754ビットパターンを返す
- [x] `(string-to-utf8-octets "日本語")` が正しいバイト列を返す
- [x] ファイル読み書きがwasmtime上で動作
- [x] SBCL固有関数の使用箇所がゼロ

---

### Phase 11: コンパイラサブセット検証 (Lisp-10) ✅ 完了 (Feature 036)

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
- [x] 各モジュールの依存関係グラフ作成
- [x] `blessed-subset.lisp` 定義（自己コンパイル可能なCLサブセット）
- [x] 段階1-3のコンパイル成功確認
- [x] 段階4-7のコンパイル成功確認

#### 11C: 互換性テストスイート

```lisp
;; コンパイラ自身のテスト
(deftest compiler-compiles-leb128
  (let ((wasm (compile-file "src/clysm/backend/leb128.lisp")))
    (is (valid-wasm-p wasm))))
```

**タスク**:
- [x] モジュール単位コンパイルテスト
- [x] 出力Wasm検証テスト
- [x] 実行時動作テスト

#### 検証基準

- [x] `backend/` 全ファイルがClysm自身でコンパイル可能
- [x] `reader/` 全ファイルがClysm自身でコンパイル可能
- [x] `compiler/` 全ファイルがClysm自身でコンパイル可能
- [x] 生成されたWasmがwasm-tools validateを通過

---

### Phase 12: クロスコンパイル (Lisp-11) ✅ 完了 (Feature 037-040)

**目標**: SBCLでClysm Compilerをコンパイルし、Wasm版Clysm Compilerを生成

#### 12A: ブートストラップ環境構築

```
┌─────────────┐    コンパイル     ┌─────────────────┐
│ SBCL (Host) │ ───────────────→ │ Clysm.wasm (T0) │
│ + Clysm CL  │                   │ (Stage 0)       │
└─────────────┘                   └─────────────────┘
```

**タスク**:
- [x] 全コンパイラソースのファイルリスト生成
- [x] ロード順序の決定（依存関係ソート）
- [x] 単一Wasmモジュールへの統合
- [x] ランタイム初期化コード生成

#### 12B: Stage 0コンパイラ生成

```bash
# Stage 0: SBCLでコンパイル
sbcl --load "build/bootstrap.lisp" --eval "(build-clysm-wasm)"
# => dist/clysm-stage0.wasm
```

**タスク**:
- [x] `build/bootstrap.lisp` スクリプト作成
- [x] コンパイル時環境の分離
- [x] デバッグシンボル出力オプション
- [x] `dist/clysm-stage0.wasm` 生成

#### 12C: Stage 0検証

```lisp
;; Stage 0コンパイラで簡単なプログラムをコンパイル
(run-wasm "clysm-stage0.wasm"
  :input "(compile nil '(lambda (x) (+ x 1)))")
;; => 有効なWasmバイナリ
```

**タスク**:
- [x] 基本算術コンパイルテスト
- [x] 関数定義コンパイルテスト
- [x] 制御フローコンパイルテスト
- [x] SBCL版との出力比較

#### 検証基準

- [x] `clysm-stage0.wasm` がwasmtimeで実行可能
- [x] Stage 0コンパイラが`(+ 1 2)`をコンパイル可能
- [x] Stage 0の出力がSBCL版と同一（ビット単位）

---

### Phase 13: 完全セルフホスティング (Lisp-12) ⚠️ インフラのみ完了

**目標**: Wasm版ClysmsがClysm自身をコンパイル可能

**現状**: ブートストラップインフラは完成したが、Stage 0に実際のコンパイラロジックが未実装

#### 13A: Stage 1コンパイラ生成 ⚠️ インフラのみ

```
┌─────────────────┐    コンパイル     ┌─────────────────┐
│ Clysm.wasm (T0) │ ───────────────→ │ Clysm.wasm (T1) │
│ (Stage 0)       │                   │ (Stage 1)       │
└─────────────────┘                   └─────────────────┘
```

**タスク**:
- [x] Stage 0でコンパイラソースを読み込み（インフラ）
- [x] Stage 1 Wasmバイナリ生成（インフラ）
- [x] Stage 0とStage 1の比較（インフラ）
- [ ] **Stage 0にコンパイラロジック実装** ← 未完了

#### 13B: 固定点検証 ⚠️ 形式的達成のみ

```
T0 (SBCL生成) → T1 (T0生成) → T2 (T1生成)
                              ↓
                         T1 == T2 ならば固定点達成
```

**現状の問題**:
```
T0 (275 bytes, スタブ) → T1 (17 bytes, 空) → T2 (17 bytes, 空)
                                              ↓
                                    T1 == T2 (自明な固定点)
```

**タスク**:
- [x] Stage 2コンパイラ生成（インフラ）
- [x] Stage 1とStage 2のビット単位比較（インフラ）
- [ ] **非自明な固定点達成** ← 未完了

#### 13C: 開発ワークフロー確立 ❌ 未完了

```bash
# セルフホスティング開発サイクル（目標）
./clysm compile src/clysm/**/*.lisp -o clysm-next.wasm
./clysm-next compile tests/**/*.lisp -o tests.wasm
wasmtime tests.wasm
```

**タスク**:
- [x] CLIインターフェース実装（インフラ）
- [ ] **実際のコンパイル動作** ← 未完了
- [ ] インクリメンタルコンパイル
- [ ] エラーリカバリ
- [ ] REPLからのコンパイル

#### 検証基準

- [x] Stage 1コンパイラが生成可能（形式的）
- [x] Stage 1 == Stage 2 (自明な固定点)
- [ ] **Stage 0が実際にコードをコンパイル可能** ← 未達成
- [ ] SBCLなしでClysm開発が可能 ← 未達成
- [ ] 新機能がセルフホスト環境で開発可能 ← 未達成

#### 13D: 真のセルフホスティング達成 🎯 次の最優先タスク

**アプローチ**: SBCL上のClysmコンパイラでClysm自身をWasmにコンパイル

```
┌─────────────────────────────────────────────────────────────┐
│ Step 1: SBCL + Clysmコンパイラ                               │
│   └── Clysmコンパイラ全体をWasmにコンパイル                   │
│         └── dist/clysm-stage1.wasm (完全なコンパイラ)         │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ Step 2: Node.js + host-shim + Stage 1                        │
│   └── Stage 1でClysm自身をコンパイル                          │
│         └── dist/clysm-stage2.wasm                           │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ Step 3: 固定点検証                                           │
│   └── Stage 1 == Stage 2 (バイト単位一致)                     │
│         └── セルフホスティング達成！                          │
└─────────────────────────────────────────────────────────────┘
```

**現在のブロッカー**: コンパイル率12.9% (主要モジュール46ファイル分析)

**分析結果** (2025-12-29):
- 分析ファイル: 46 (compiler/, backend/, reader/, runtime/, clos/, conditions/, eval/, lib/)
- コンパイル成功: 112フォーム
- コンパイル失敗: 756フォーム
- **コンパイル率: 12.9%**

#### ブロッカー分析（2025-12-29 修正）

> **重要な訂正**: 以前の分析では「内部関数をプリミティブ登録すべき」としていたが、
> これは誤りである。`make-ast-literal`等はdefstructが生成する関数、
> `encode-unsigned-leb128`等は通常のdefunであり、プリミティブ登録すべきではない。
> 真の問題は、これらの関数が使用する**ANSI CL標準関数**がプリミティブとして未実装であること。

#### 根本原因: ANSI CL関数の未実装

**具体例: `encode-unsigned-leb128`** (backend/leb128.lisp):

```lisp
(defun encode-unsigned-leb128 (value)
  (let ((bytes '()))
    (loop
      (let ((byte (logand value #x7f)))      ;; ✅ logand: 実装済み
        (setf value (ash value -7))          ;; ✅ ash: 実装済み
        (if (zerop value)                    ;; ✅ zerop: 実装済み
            (progn (push byte bytes) (return))
            (push (logior byte #x80) bytes))))
    (coerce (nreverse bytes) '(vector ...))))  ;; ❌ coerce: 未実装！
```

この関数は普通のdefunだが、最終行で`coerce`を使用している。
`coerce`がプリミティブとして未実装のため、コンパイルに失敗する。

**具体例: `make-ast-literal`** (compiler/ast.lisp):

```lisp
(defstruct (ast-literal (:include ast-node) ...)
  (value nil :type t)
  (literal-type nil :type keyword))
;; → defstructマクロがmake-ast-literal, ast-literal-value等を生成
;; → 生成されたコードは配列アクセス(aref/svref)を使用
;; → aref/svrefがプリミティブとして未実装のため失敗
```

#### ブロッカー分類（修正版）

**カテゴリ1: ANSI CL関数の未実装（根本原因）**

| 関数 | ANSI CL仕様 | 使用箇所 | 影響範囲 |
|-----|------------|---------|---------|
| `coerce` | §17.3 | leb128.lisp, 多数 | 型変換全般 |
| `aref` | §15.2 | 配列アクセス全般 | defstruct展開後のコード |
| `svref` | §15.2 | simple-vector用 | defstruct展開後のコード |
| `schar` | §16.3 | 文字列アクセス | 文字列処理 |
| `elt` | §17.3 | 汎用シーケンス | シーケンス処理 |
| `make-string` | §16.3 | 文字列生成 | 文字列処理 |
| `concatenate` | §17.3 | シーケンス結合 | 文字列・リスト処理 |
| `subseq` | §17.3 | 部分シーケンス | 多数 |

**カテゴリ2: マクロ展開（カテゴリ1に依存）**

| マクロ | 失敗数 | 状態 | 備考 |
|-------|-------|------|------|
| `defstruct` | 90件 | 展開される | 展開後コードがaref/svref使用で失敗 |
| `define-condition` | 17件 | 展開される | defstruct同様 |
| `in-package` | 49件 | 無視可 | コンパイル時ディレクティブ |
| `defpackage` | 9件 | 無視可 | コンパイル時ディレクティブ |
| `declaim` | 3件 | 無視可 | コンパイル時ディレクティブ |

**カテゴリ3: 制御構造の拡張**

| 機能 | 失敗数 | 対応方針 |
|-----|-------|---------|
| `handler-case` | 3件 | Wasm try-table変換 |
| LOOP hash-table | 40件 | `being the hash-keys`パターン |
| `labels` 相互再帰 | 6件 | 関数インデックス前方参照 |
| `values` 特殊形式 | 18件 | AST処理追加 |
| `the` 型宣言 | 18件 | 無視または型情報活用 |

**カテゴリ4: グローバル変数（ランタイム初期化）**

| 変数 | 用途 | 対応方針 |
|-----|------|---------|
| `*current-package*` | パッケージシステム | defvar + 初期化 |
| `*standard-output*` | I/O | FFI経由初期化 |
| `*macro-registry*` | マクロ展開 | ハッシュテーブル初期化 |
| その他55種類 | 各種 | 順次定義 |

#### 優先順位付きタスク（2025-12-29 修正）

> **方針変更**: 以前は「内部関数をプリミティブ登録」としていたが、
> 正しいアプローチは「ANSI CL標準関数をプリミティブ実装」である。
> 内部関数（make-ast-literal, encode-unsigned-leb128等）は
> 通常のdefunとしてコンパイルされるべきであり、
> そのためにはこれらが使用するANSI CL関数を先に実装する必要がある。

**Phase 13D-1: ANSI CL配列・シーケンス関数 (P1 - 最優先)**

defstruct展開後のコード、leb128.lisp等がコンパイルできない根本原因を解決。

```lisp
;; 実装が必要なプリミティブ（ANSI CL仕様準拠）
aref     ;; 配列要素アクセス → Wasm array.get
svref    ;; simple-vector要素アクセス → Wasm array.get
schar    ;; 文字列文字アクセス → Wasm array.get (i8)
elt      ;; 汎用シーケンスアクセス → typecaseで分岐
coerce   ;; 型変換 → リスト→配列等の変換
```

- [ ] `aref` / `svref` 実装 (array.get命令)
- [ ] `(setf aref)` / `(setf svref)` 実装 (array.set命令)
- [ ] `schar` / `(setf schar)` 実装
- [ ] `elt` / `(setf elt)` 実装 (typecase分岐)
- [ ] `coerce` 実装 (list→vector, vector→list等)

**Phase 13D-2: ANSI CL シーケンス操作関数 (P1)**

```lisp
;; 実装が必要なプリミティブ
subseq       ;; 部分シーケンス抽出
concatenate  ;; シーケンス結合
make-string  ;; 文字列生成
make-array   ;; 配列生成（拡張）
copy-seq     ;; シーケンスコピー
```

- [ ] `subseq` 実装
- [ ] `concatenate` 実装
- [ ] `make-string` 実装
- [ ] `make-array` 拡張（:initial-element, :initial-contents）
- [ ] `copy-seq` 実装

**Phase 13D-3: コンパイル時ディレクティブ処理 (P1)**

コンパイル時に評価・無視すべきフォームの処理。

- [ ] `in-package` → コンパイル時評価、AST生成なし
- [ ] `defpackage` → コンパイル時評価、AST生成なし
- [ ] `declaim` → コンパイル時評価（最適化ヒント）、AST生成なし
- [ ] `proclaim` → 同上

**Phase 13D-4: グローバル変数定義 (P2)**

58種類のグローバル変数を定義。

- [ ] ランタイムレジストリ (`*macro-registry*`, `*function-registry*` 等)
- [ ] パッケージシステム (`*current-package*`, `*packages*` 等)
- [ ] I/Oストリーム (`*standard-output*`, `*standard-input*`)
- [ ] 条件システム (`*restart-clusters*`, `*handler-clusters*`)

**Phase 13D-5: LOOP拡張 (P2)**

- [ ] hash-tableイテレーション (`being the hash-keys`)
- [ ] 複合変数分解 (`for (key val) being...`)
- [ ] `with` 句完全サポート

**Phase 13D-6: 制御構造拡張 (P2)**

- [ ] `handler-case` → Wasm try-table変換
- [ ] `labels`/`flet` 相互再帰
- [ ] `values` 特殊形式のAST処理
- [ ] `the` 型宣言（無視または型情報活用）

#### 依存関係グラフ

```
Phase 13D-1: aref/svref/coerce実装
    │
    ├──→ defstruct展開後のコードがコンパイル可能に
    │         │
    │         └──→ make-ast-literal, ast-literal-value等が動作
    │
    └──→ encode-unsigned-leb128等がコンパイル可能に
              │
              └──→ backend/leb128.lisp 全体が動作

Phase 13D-2: subseq/concatenate実装
    │
    └──→ reader, compiler の文字列処理が動作

Phase 13D-3: コンパイル時ディレクティブ
    │
    └──→ in-package等による失敗61件が解決

Phase 13D-4-6: 並行して実装可能
```

#### Stage 1生成・検証タスク

**Phase 13D-7: Stage 1生成**
- [ ] SBCL上でClysmコンパイラ全体をロード
- [ ] 全モジュール（約45,000行）をWasmにコンパイル
- [ ] 単一Wasmモジュールとして出力
- [ ] `wasm-tools validate` パス

**Phase 13D-8: Stage 1実行環境**
- [ ] Node.js host-shim でStage 1を実行
- [ ] FFI経由でファイルI/O提供
- [ ] Stage 1の `compile_form` が動作確認

**Phase 13D-9: 固定点達成**
- [ ] Stage 1でClysm自身をコンパイル → Stage 2
- [ ] Stage 1 == Stage 2 (バイト単位一致)
- [ ] `./scripts/verify-fixpoint.sh` でACHIEVED

#### 検証基準
```bash
# Step 1: SBCL上でStage 1生成
sbcl --load build/stage1-gen.lisp
wasm-tools validate dist/clysm-stage1.wasm
ls -la dist/clysm-stage1.wasm  # 数百KB以上

# Step 2: Node.js上でStage 1実行
node host-shim/stage1-host.js --mode compile \
  --stage1 dist/clysm-stage1.wasm \
  --output dist/clysm-stage2.wasm

# Step 3: 固定点検証
./scripts/verify-fixpoint.sh --json
# => {"status": "ACHIEVED", ...}
```

#### 成功指標

| 指標 | 現在 | 目標 |
|-----|------|------|
| コンパイル率 | 12.9% (112/868) | 80%+ |
| Stage 1サイズ | 17バイト (スタブ) | 100KB+ |
| 固定点 | 未達成 | Stage 1 == Stage 2 |

---

## 3. ANSI準拠率向上フェーズ (Phase 14-17)

**現在の準拠率**: 23.4% (219/936テスト)
**テストスイート**: pfdietz/ansi-test (~20,000テスト、31カテゴリ)

### Phase 14: 数値・算術強化 (ANSI-Numbers) 🔜 次フェーズ

**目標**: 数値関連テスト準拠率 50%+

現在の`numbers`カテゴリは92テストファイルを持つ最大カテゴリ。

#### 14A: 基本算術関数拡張

```lisp
;; 現在サポート済み
(+ - * / mod rem floor ceiling truncate round)

;; 追加が必要
(abs signum max min gcd lcm expt log exp sqrt)
(sin cos tan asin acos atan sinh cosh tanh)
(ash logand logior logxor lognot logcount)
(complex realpart imagpart conjugate phase)
```

**タスク**:
- [ ] 三角関数群 (sin, cos, tan, asin, acos, atan)
- [ ] 双曲線関数群 (sinh, cosh, tanh, asinh, acosh, atanh)
- [ ] ビット演算関数 (ash, logand, logior, logxor, lognot)
- [ ] 複素数演算 (complex, realpart, imagpart)
- [ ] 数学関数 (exp, log, sqrt, expt)

#### 14B: 数値型述語強化

```lisp
;; 追加が必要な述語
(plusp minusp zerop oddp evenp)
(integer-length logbitp logtest)
(byte byte-size byte-position ldb dpb)
```

**タスク**:
- [ ] 符号述語 (plusp, minusp, zerop)
- [ ] パリティ述語 (oddp, evenp)
- [ ] ビット操作 (byte, ldb, dpb, mask-field)

#### 14C: 数値変換・フォーマット

```lisp
(parse-integer "123") ; => 123
(write-to-string 42 :base 16) ; => "2A"
(float 3/2) ; => 1.5
(rationalize 0.5) ; => 1/2
```

**タスク**:
- [ ] `parse-integer` 完全実装
- [ ] `float`, `rational`, `rationalize` 変換
- [ ] 基数変換 (2進, 8進, 16進)

#### 検証基準

- [ ] `(sin (/ pi 2))` => 1.0 (許容誤差 1e-10)
- [ ] `(ash 1 10)` => 1024
- [ ] `(logand #xFF00 #x0FF0)` => #x0F00
- [ ] numbers カテゴリ 50%+ パス

---

### Phase 15: シーケンス・コレクション強化 (ANSI-Sequences)

**目標**: シーケンス関連テスト準拠率 60%+

#### 15A: リスト操作拡張

```lisp
;; 追加が必要
(butlast nbutlast last nth nthcdr)
(adjoin pushnew intersection union set-difference)
(subsetp member member-if member-if-not)
(assoc assoc-if rassoc rassoc-if)
(pairlis acons copy-alist)
```

**タスク**:
- [ ] リスト末尾操作 (butlast, last, nth, nthcdr)
- [ ] 集合演算 (intersection, union, set-difference, subsetp)
- [ ] 連想リスト操作 (assoc, rassoc, pairlis)

#### 15B: シーケンス汎用関数

```lisp
;; 追加が必要
(count count-if count-if-not)
(position position-if position-if-not)
(find find-if find-if-not)
(remove-duplicates delete-duplicates)
(mismatch search)
(substitute substitute-if nsubstitute nsubstitute-if)
(replace fill)
```

**タスク**:
- [ ] 検索関数 (count, position, find) 完全実装
- [ ] 置換関数 (substitute, replace)
- [ ] 重複削除 (remove-duplicates, delete-duplicates)
- [ ] 比較関数 (mismatch, search)

#### 15C: 配列操作

```lisp
;; 追加が必要
(array-rank array-dimension array-dimensions)
(array-total-size array-row-major-index)
(adjust-array make-array :adjustable)
(row-major-aref)
```

**タスク**:
- [ ] 配列属性アクセサ完全実装
- [ ] 多次元配列サポート強化
- [ ] 調整可能配列 (adjustable-array-p)

#### 検証基準

- [ ] `(count 1 '(1 2 1 3 1))` => 3
- [ ] `(intersection '(1 2 3) '(2 3 4))` => (2 3) or (3 2)
- [ ] `(remove-duplicates '(a b a c b))` => (A B C) or equivalent
- [ ] sequences カテゴリ 60%+ パス
- [ ] cons カテゴリ 50%+ パス

---

### Phase 16: 文字列・文字強化 (ANSI-Strings)

**目標**: 文字列・文字テスト準拠率 70%+

#### 16A: 文字関数

```lisp
;; 追加が必要
(char-upcase char-downcase)
(alpha-char-p digit-char-p alphanumericp)
(upper-case-p lower-case-p both-case-p)
(graphic-char-p standard-char-p)
(char-name name-char digit-char)
(char-int int-char)
```

**タスク**:
- [ ] ケース変換 (char-upcase, char-downcase)
- [ ] 文字分類述語完全実装
- [ ] 文字名変換 (char-name, name-char)

#### 16B: 文字列操作

```lisp
;; 追加が必要
(string-upcase string-downcase string-capitalize)
(nstring-upcase nstring-downcase nstring-capitalize)
(string-trim string-left-trim string-right-trim)
(string= string< string> string<= string>=)
(string-equal string-lessp string-greaterp)
```

**タスク**:
- [ ] 文字列ケース変換完全実装
- [ ] 文字列トリム関数
- [ ] 文字列比較関数完全実装

#### 16C: パース・構築

```lisp
(make-string length :initial-element char)
(string object) ; coercion
(schar simple-string element-type)
```

**タスク**:
- [ ] 文字列構築関数
- [ ] 文字列強制変換

#### 検証基準

- [ ] `(string-upcase "hello")` => "HELLO"
- [ ] `(string-trim " " " test ")` => "test"
- [ ] `(alpha-char-p #\a)` => T
- [ ] strings カテゴリ 70%+ パス
- [ ] characters カテゴリ 80%+ パス

---

### Phase 17: 制御フロー・評価強化 (ANSI-Control)

**目標**: 制御フロー関連テスト準拠率 50%+

#### 17A: LOOP拡張

現在のLOOPは基本パターンをサポート。拡張が必要。

```lisp
;; 追加が必要なLOOP機能
loop with clause
loop initially/finally
loop named
loop thereis/never/always
loop maximize/minimize
loop hash-table iteration
```

**タスク**:
- [ ] LOOP with句完全サポート
- [ ] LOOP initially/finally句
- [ ] LOOP集約関数 (maximize, minimize, sum, count)
- [ ] LOOP終了条件 (thereis, never, always)

#### 17B: イテレーション

```lisp
;; 強化が必要
(dotimes (i 10) ...)
(dolist (x list) ...)
(do ((i 0 (1+ i))) ((= i 10)) ...)
(do* ...)
```

**タスク**:
- [ ] DO/DO*完全実装
- [ ] 結果フォーム評価の修正
- [ ] 複数変数宣言サポート

#### 17C: 条件分岐

```lisp
;; 追加が必要
(case key
  ((:a :b) 1)
  (otherwise 2))

(ecase ...)
(ccase ...)
```

**タスク**:
- [ ] CASE完全実装 (キーリスト、otherwise)
- [ ] ECASE, CCASE実装
- [ ] 型分岐 (typecase) 完全テスト

#### 検証基準

- [ ] LOOP with句がANSIテストをパス
- [ ] `(do ((i 0 (1+ i)) (sum 0)) ((= i 5) sum) (incf sum i))` => 10
- [ ] data-and-control-flow カテゴリ 40%+ パス
- [ ] iteration カテゴリ 50%+ パス

---

## 4. アーキテクチャ設計: コンパイラモジュール構成

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
Phase 9-13 [セルフホスティング] ⚠️ インフラのみ完了
    ├── 9A-D [基盤機能] ✅
    ├── 10A-C [SBCL非依存化] ✅
    ├── 11A-C [コンパイラ検証] ✅
    ├── 12A-C [クロスコンパイル] ✅
    ├── 13A-C [固定点インフラ] ✅
    └── 13D [真のセルフホスティング] ◀── 🎯 最優先
        ├── 最小Lispインタプリタ実装
        ├── Stage 0コンパイルロジック
        └── 非自明な固定点達成
    │
    ▼
Phase 14 [数値・算術強化]
    ├── 14A [算術関数拡張]
    ├── 14B [数値型述語]
    └── 14C [数値変換]
    │
    ▼
Phase 15 [シーケンス強化]
    ├── 15A [リスト操作]
    ├── 15B [汎用シーケンス関数]
    └── 15C [配列操作]
    │
    ▼
Phase 16 [文字列・文字強化]
    ├── 16A [文字関数]
    ├── 16B [文字列操作]
    └── 16C [パース・構築]
    │
    ▼
Phase 17 [制御フロー強化]
    ├── 17A [LOOP拡張]
    ├── 17B [イテレーション]
    └── 17C [条件分岐]
    │
    ▼
Phase 18 [I/O・ストリーム]          ◀── 長期目標
    ├── 18A [出力ストリーム]
    ├── 18B [入力ストリーム]
    └── 18C [ファイルストリーム]
    │
    ▼
Phase 19 [CLOS完全準拠]
    ├── 19A [MOP拡張]
    ├── 19B [ジェネリック関数]
    └── 19C [メソッドコンビネーション]
```

### ANSI準拠率ロードマップ

| Phase | 目標準拠率 | 主要カテゴリ |
|-------|-----------|-------------|
| 現在 | 23.4% | 基本算術、型述語 |
| Phase 14完了 | 35% | numbers 50%+ |
| Phase 15完了 | 45% | sequences 60%+, cons 50%+ |
| Phase 16完了 | 55% | strings 70%+, characters 80%+ |
| Phase 17完了 | 60% | iteration 50%+, control-flow 40%+ |
| Phase 18完了 | 70% | streams 基本サポート |
| Phase 19完了 | 80% | objects/CLOS 60%+ |

---

## 6. リスク分析と対策

### 高リスク (ANSI準拠)

| リスク | 影響度 | 対策 |
|--------|--------|------|
| テスト結果検証の制限 | 高 | 現在fixnum/booleanのみ。cons, symbol比較を段階追加 |
| I/Oカテゴリの大規模さ | 高 | 基本出力→入力→ファイルの順で段階実装 |
| CLOS完全準拠の複雑さ | 高 | 基本機能を先行、MOP拡張は後回し |
| テストスイート実行時間 | 中 | 並列実行実装。カテゴリ別実行 |
| 浮動小数点精度問題 | 中 | 許容誤差を明確化。IEEE 754準拠テスト |

### 高リスク (セルフホスティング - 未解決)

| リスク | 影響度 | 対策 | 状態 |
|--------|--------|------|------|
| Stage 0にコンパイルロジックがない | 最高 | 最小Lispインタプリタ実装 | ❌ 未解決 |
| 非自明な固定点未達成 | 高 | 段階的機能追加 | ❌ 未解決 |
| Wasm上でのLisp実行の複雑さ | 高 | インタプリタ方式で段階的に | ⏳ 検討中 |

### 中リスク (セルフホスティングインフラ - 解決済み)

| リスク | 影響度 | 対策 | 状態 |
|--------|--------|------|------|
| SBCL依存の見落とし | 高 | 静的解析で検出 | ✅ 解決済み |
| IEEE 754エンコードの精度 | 高 | 参照実装比較 | ✅ 解決済み |
| 検証インフラの不備 | 中 | スクリプト整備 | ✅ 解決済み |

### 低リスク

| リスク | 影響度 | 対策 |
|--------|--------|------|
| `etypecase`の網羅性 | 低 | 型チェッカーで検証済み |
| FORMAT実装の複雑さ | 低 | 基本ディレクティブ実装済み |
| ファイルI/O抽象化 | 低 | FFI経由で動作中 |

---

## 7. マイルストーンと成功指標

### M0-M8: 言語機能 ✅ 完了

（既存マイルストーン - すべて達成済み）

### M9: セルフホスティング基盤 (Phase 9完了) ✅

- [x] `typecase`/`etypecase` が動作
- [x] `destructuring-bind` が動作
- [x] 基本FORMAT (`~A ~S ~D ~%`) が動作
- [x] コンパイラの50%がClysm自身でコンパイル可能

### M10: SBCL非依存 (Phase 10完了) ✅

- [x] `sb-kernel:*-float-bits` 代替実装完了
- [x] `sb-int:with-float-traps-masked` 代替完了
- [x] Babel依存排除完了
- [x] SBCL固有コード使用箇所 = 0

### M11: コンパイラ検証 (Phase 11完了) ✅

- [x] 全コンパイラモジュールがClysm自身でコンパイル可能
- [x] 生成Wasmが全てvalidation通過
- [x] 基本機能の動作確認テスト全パス

### M12: クロスコンパイル成功 (Phase 12完了) ✅

- [x] `clysm-stage0.wasm` 生成成功
- [x] Stage 0が`(+ 1 2)`をコンパイル可能
- [x] Stage 0とSBCL版の出力が一致

### M13: セルフホスティングインフラ (Phase 13A-C) ⚠️ インフラのみ

- [x] Stage 1生成成功（形式的）
- [x] Stage 1 == Stage 2 (自明な固定点: 空 == 空)
- [ ] Stage 0が実際にコンパイル可能 ← 未達成
- [ ] SBCLなしで新機能開発可能 ← 未達成

### M13D: 真のセルフホスティング (Phase 13D) 🎯 次の最優先

- [ ] Stage 0に最小Lispインタプリタ実装
- [ ] `(+ 1 2)` が Stage 0 でコンパイル可能
- [ ] Stage 1 > 1KB (非空モジュール)
- [ ] Stage 1 == Stage 2 (非自明な固定点)
- [ ] SBCLなしでClysm開発可能

### M14: 数値強化達成 (Phase 14完了)

- [ ] 三角関数・双曲線関数が動作
- [ ] ビット演算関数が動作
- [ ] ANSI準拠率 35%+
- [ ] numbers カテゴリ 50%+ パス

### M15: シーケンス強化達成 (Phase 15完了)

- [ ] 集合演算が動作
- [ ] シーケンス汎用関数が動作
- [ ] ANSI準拠率 45%+
- [ ] sequences カテゴリ 60%+ パス

### M16: 文字列強化達成 (Phase 16完了)

- [ ] 文字列操作関数が動作
- [ ] 文字分類述語が動作
- [ ] ANSI準拠率 55%+
- [ ] strings カテゴリ 70%+ パス

### M17: 制御フロー強化達成 (Phase 17完了)

- [ ] LOOP拡張機能が動作
- [ ] DO/DO*完全動作
- [ ] ANSI準拠率 60%+
- [ ] iteration カテゴリ 50%+ パス

### M18: I/O基盤達成 (Phase 18完了)

- [ ] 基本ストリーム出力が動作
- [ ] FORMAT完全実装
- [ ] ANSI準拠率 70%+
- [ ] streams カテゴリ 基本サポート

### M19: CLOS完全準拠 (Phase 19完了)

- [ ] MOP拡張完了
- [ ] メソッドコンビネーション動作
- [ ] ANSI準拠率 80%+
- [ ] objects カテゴリ 60%+ パス

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
| 1.0.0 | 2025-12-27 | セルフホスティングフェーズ (Phase 9-13) 追加。最終目標をセルフホスティングに設定 |
| 2.0.0 | 2025-12-28 | Phase 9-13完了。ANSI準拠率向上フェーズ (Phase 14-19) 追加。現在23.4%から80%を目標に設定 |
| 2.1.0 | 2025-12-28 | セルフホスティング検証結果を反映。Phase 13は「インフラ完了」に修正（Stage 0がスタブのみで実際のコンパイルロジックなし）。Phase 13Dとして真のセルフホスティング達成タスクを追加 |
| **2.2.0** | **2025-12-29** | **Phase 13Dブロッカー分析を大幅修正。「内部関数をプリミティブ登録」は誤りであり、「ANSI CL標準関数（coerce, aref, svref等）をプリミティブ実装」が正しいアプローチと判明。make-ast-literal等はdefstructが生成する関数、encode-unsigned-leb128等は通常のdefunであり、これらが使用するANSI CL関数が未実装であることが根本原因。依存関係グラフを追加** |
