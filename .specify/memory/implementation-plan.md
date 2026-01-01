# Clysm実装計画: WebAssembly GCターゲットCommon Lispコンパイラ

**作成日**: 2025-12-21
**更新日**: 2026-01-01 (v3.2.0)
**ステータス**: Phase 13D-R1 (プリミティブ/ランタイム分離移行中)
**憲法バージョン**: 1.0.0
**コンパイル率**: 13.84% (3585/26353フォーム、458スキップ)
**Stage 1サイズ**: 24.5KB (有効なWasmGCモジュール)

## 🎯 セルフホスティング達成に向けた現状と戦略 (2025-12-31)

### 現在の状態

| メトリクス | 値 | 備考 |
|-----------|-----|------|
| Stage 1 サイズ | 24,651 bytes | 有効なWasmGCモジュール |
| Stage 2 サイズ | 39 bytes | compile_formがスタブのため |
| コンパイル率 | 13.84% | 3585/26353フォーム (458スキップ) |
| 主要ブロッカー | DEFUN (18,933) | 全失敗の84.9% |

### 問題の本質

**Stage 1は24.5KBの有効なWasmを生成している**が、Stage 2が39bytesなのは
Stage 1の`compile_form`関数が実際のコンパイルロジックを持っていないため。

```
SBCL → Stage 1 (24.5KB, 3519フォーム成功)
         ↓
Stage 1の compile_form は ref.null を返すスタブ
         ↓
Stage 2 (39 bytes, 空に近い)
```

### ブロッカー分析 (2025-12-31更新)

| オペレータ | 失敗数 | 全体比率 | 根本原因 | 状態 |
|-----------|--------|---------|---------|------|
| DEFUN | 18,933 | 84.9% | 関数本体のコンパイル失敗 | 🔴 最大ブロッカー |
| DEFSTRUCT | 1,953 | 8.8% | CLOS展開後のアクセサ生成失敗 | 🟡 一部対応済 |
| DEFMACRO | 646 | 2.9% | マクロ本体のコンパイル失敗 | 🟡 |
| DEFINE-CONDITION | 302 | 1.4% | DEFSTRUCTと同様 | 🟡 |
| DEFVAR | 133 | 0.6% | グローバル変数定義 | 🟡 |

**DEFUNが失敗する理由** (Phase 13D-1完了後の再分析待ち):

1. ~~**文字リテラル未対応**~~ → ✅ Phase 13D-1a で対応済
2. ~~**算術プリミティブ欠落**~~ → ✅ Phase 13D-1b で `1-`, `1+` 追加済
3. ~~**I/O関数欠落**~~ → ✅ Phase 13D-1d で `print`, `format` 追加済
4. ~~**CLOS内部関数欠落**~~ → ✅ Phase 13D-1c で `make-instance*` 追加済
5. ~~**コンパイル時ディレクティブ**~~ → ✅ Phase 13D-3 で対応済

**残存する可能性のある問題** (要調査):
- TAGBODY/GO サポート (LOOPマクロ展開結果)
- 複雑なlambda-list (&aux等)
- 未サポートの制御構造

### 戦略: コンパイル率14%→80%への道 (2025-12-31更新)

Phase 13D-1〜13D-3 完了。次はコンパイル率再測定とcompile_form実装。

```
完了: 13.84% (3585/26353フォーム)
  ↓
✅ Phase 13D-1a: characterp対応追加 (完了)
  ↓
✅ Phase 13D-1b: 1-/1+ プリミティブ追加 (完了)
  ↓
✅ Phase 13D-1c: make-instance* 追加 (完了)
  ↓
✅ Phase 13D-1d: print/format 追加 (完了)
  ↓
✅ Phase 13D-3: ディレクティブスキップ (完了)
  ↓
🎯 Phase 13D-4: compile_form実装 → 次のステップ
  ↓
⏳ Phase 13D-5: 固定点検証 → 80%+ (Stage 1 == Stage 2)
```

**次のアクション**:
1. Stage 1 レポート再生成でコンパイル率の変化を確認
2. 残存ブロッカーの詳細分析
3. compile_form の実装設計

---

## 🏗️ アーキテクチャ方針の転換: プリミティブとランタイムの分離 (2026-01-01)

### 問題認識

現在の`func-section.lisp`（18,000行超）は、`assoc`、`member`、`princ`、`format`等の
標準関数を全てWasm命令に直接コンパイルしようとしている。これは：

1. **スケールしない**: Common Lispには数百の標準関数がある
2. **保守困難**: 各関数の実装が巨大ファイルに散在
3. **バグ多発**: `:swap`、`:$princ-to-string`等の未実装疑似命令が残存
4. **標準手法との乖離**: 一般的なLisp処理系はLisp自身で標準ライブラリを実装

### 新方針: 2層アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│ Layer 2: ランタイムライブラリ (Lispで実装)                    │
│ src/clysm/lib/runtime.lisp                                  │
│ - assoc, member, find, position, remove                     │
│ - mapcar, mapc, reduce, every, some                         │
│ - princ, prin1, print, format, write-line                   │
│ - その他ANSI CL標準関数                                       │
└─────────────────────────────────────────────────────────────┘
                              ↓ コンパイル
┌─────────────────────────────────────────────────────────────┐
│ Layer 1: プリミティブ (Wasm命令に直接変換)                     │
│ src/clysm/compiler/codegen/func-section.lisp                │
│ - cons, car, cdr, rplaca, rplacd                            │
│ - +, -, *, /, mod, rem, floor, ceiling                      │
│ - eq, eql, =, <, >, <=, >=                                  │
│ - consp, numberp, stringp, symbolp (型判定)                  │
│ - struct.new, struct.get, struct.set (WasmGC操作)           │
│ - array.new, array.get, array.set (配列操作)                │
│ - FFI: %host-write-char, %host-write-string, %host-read-char│
└─────────────────────────────────────────────────────────────┘
```

### プリミティブの定義基準

func-section.lispに残すべき関数：

1. **Wasm命令に直接対応**: `cons` → `struct.new`
2. **ホストFFI呼び出し**: `%host-write-char` → `call-import`
3. **型判定**: `consp` → `ref.test`
4. **基本算術**: `+` → `i32.add` / `f64.add` (型ディスパッチ付き)
5. **Lispで効率的に実装不可能**: メモリ操作、型変換

### ランタイムライブラリの実装例

```lisp
;;; src/clysm/lib/runtime.lisp

(defun assoc (item alist &key key (test #'eql))
  "ANSI CL assoc - Lispで実装"
  (dolist (pair alist nil)
    (when (and pair
               (funcall test item
                        (if key (funcall key (car pair)) (car pair))))
      (return pair))))

(defun princ (object &optional (stream *standard-output*))
  "ANSI CL princ - FFIプリミティブを使用"
  (let ((str (princ-to-string object)))
    (%host-write-string (stream-fd stream) str))
  object)

(defun princ-to-string (object)
  "オブジェクトを文字列に変換"
  (typecase object
    (string object)
    (character (string object))
    (symbol (symbol-name object))
    (integer (integer-to-string object))
    (t "#<object>")))
```

### 移行計画

| Phase | 内容 | 削除対象 (func-section.lisp) |
|-------|------|------------------------------|
| 13D-R1 | I/O関数をランタイムへ移行 | compile-princ, compile-print, compile-format |
| 13D-R2 | リスト操作をランタイムへ移行 | compile-assoc, compile-member, compile-find |
| 13D-R3 | シーケンス操作をランタイムへ移行 | compile-remove, compile-position, compile-count |
| 13D-R4 | func-section.lisp整理 | 未使用コード削除、8,000行以下目標 |

### 期待される効果

1. **func-section.lisp**: 18,000行 → 8,000行以下
2. **バグ減少**: Lisp実装はSBCLでテスト可能
3. **セルフホスティング促進**: ランタイムもWasmにコンパイル
4. **保守性向上**: 標準関数の実装が読みやすいLispコード

---

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

#### 13D: 真のセルフホスティング達成 🎯 最優先タスク

**目標**: Stage 1の`compile_form`が実際に動作し、Stage 2を生成できるようにする

**現状** (2025-12-31):
- Stage 1: 24.5KB (3519フォーム成功、14.11%)
- Stage 2: 39 bytes (compile_formがスタブのため)

```
┌─────────────────────────────────────────────────────────────┐
│ 現在の状態                                                   │
│   SBCL → Stage 1 (24.5KB, 14.11%コンパイル成功)              │
│             ↓                                                │
│   Stage 1のcompile_formは ref.null を返すスタブ             │
│             ↓                                                │
│   Stage 2 (39 bytes, ほぼ空)                                 │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ 目標の状態                                                   │
│   SBCL → Stage 1 (数百KB, 80%+コンパイル成功)               │
│             ↓                                                │
│   Stage 1のcompile_formが実際にコンパイル実行               │
│             ↓                                                │
│   Stage 2 (Stage 1と同等サイズ) → 固定点達成！              │
└─────────────────────────────────────────────────────────────┘
```

#### ブロッカー分析 (2025-12-31更新)

**最新のstage1-report.jsonより:**

| オペレータ | 失敗数 | 全体比率 | 状態 |
|-----------|--------|---------|------|
| DEFUN | 17,580 | 82.3% | 🔴 最大のブロッカー |
| DEFSTRUCT | 1,953 | 9.1% | 🟡 CLOS展開は動作、アクセサ生成失敗 |
| DEFMACRO | 646 | 3.0% | 🟡 マクロ本体のコンパイル失敗 |
| DEFINE-CONDITION | 302 | 1.4% | 🟡 DEFSTRUCTと同様 |
| DEFPACKAGE | 284 | 1.3% | 🟢 コンパイル時スキップで対応可 |

**DEFUNが失敗する根本原因** (2025-12-31調査完了):

テスト実行結果から、以下の**5つの具体的な根本原因**を特定:

| # | 問題 | 場所 | 影響度 |
|---|------|------|--------|
| 1 | 文字リテラル未対応 | `func-section.lisp:509-525` | 高 |
| 2 | `1-`, `1+` 未登録 | `func-section.lisp:722-839` | 高 |
| 3 | `print` 未登録 | primitive list | 中 |
| 4 | `format` 未登録 | primitive list | 中 |
| 5 | `make-instance*` 未登録 | primitive list (DEFSTRUCT用) | 高 |

**1. 文字リテラルの問題** (`func-section.lisp:509-525`)

```lisp
;; compile-quoted-element に characterp の分岐が欠落
(defun compile-quoted-element (elem)
  (cond
    ((null elem) ...)
    ((integerp elem) ...)
    ((symbolp elem) ...)
    ;; ((characterp elem) ...) ← これが必要！
    (t (error "Cannot compile quoted element: ~A" elem))))
```

影響: `'(#\Space #\Tab #\Newline)` のようなquoted listが全て失敗

**2. 算術プリミティブの欠落**

`1-` と `1+` は標準CLで頻出だが、primitive listに未登録:
- `(factorial (1- n))` → "Undefined function: 1-"
- 多くの再帰アルゴリズムで使用

**3. テスト実行結果サマリ**

```
成功したパターン (7/12):
  ✓ loop, push, nreverse, coerce
  ✓ aref, incf, declare, cond
  ✓ handler-case, write-byte

失敗したパターン (5/12):
  ✗ quoted characters → "Cannot compile quoted element"
  ✗ etypecase → "Unbound variable: TYPE-ERROR"
  ✗ defstruct → "Undefined function: MAKE-INSTANCE*"
  ✗ format → "Undefined function: FORMAT"
  ✗ 1- → "Undefined function: 1-"
```

**修正優先度**:

```
高優先度（影響大）:
  1. characterp対応追加 → quoted character list動作
  2. 1-, 1+ プリミティブ追加 → 再帰パターン動作
  3. make-instance* 追加 → DEFSTRUCT使用コード全体動作

中優先度:
  4. print 追加
  5. format 追加（複雑なので後回し可）
```

#### Phase 13D 修正版タスクリスト

**Phase 13D-1: DEFUN本体コンパイル修正** ✅ 完了

2025-12-31調査により特定された5つの根本原因を修正済み。

**13D-1a: 文字リテラル対応** ✅ 完了 (001-char-literal-compile)

```lisp
;; func-section.lisp に追加済み
((characterp elem)
 (list (list :i32.const (char-code elem)) :ref.i31))
```

- [x] `compile-quoted-element` に `characterp` 分岐追加
- [x] テスト: `(member char '(#\Space #\Tab))` がコンパイル成功

**13D-1b: 算術プリミティブ追加** ✅ 完了 (001-arithmetic-primitives)

```lisp
;; primitive list に追加済み
1- 1+
```

- [x] `1-` を primitive list に追加
- [x] `compile-1-` 関数実装 (`(- x 1)` と等価)
- [x] `1+` を primitive list に追加
- [x] `compile-1+` 関数実装 (`(+ x 1)` と等価)
- [x] テスト: `(defun fact (n) (if (<= n 1) 1 (* n (fact (1- n)))))` 成功

**13D-1c: CLOS内部関数追加** ✅ 完了

- [x] `make-instance*` を primitive list に追加
- [x] テスト: `(defstruct point x y)` がコンパイル成功

**13D-1d: I/O関数スタブ追加** ✅ 完了 (001-io-print-primitives)

- [x] `print`, `prin1`, `princ`, `terpri`, `write` をFFIスタブとして追加
- [x] `format` を基本実装として追加 (~A, ~S, ~D, ~%, ~&, ~~)

**検証マイルストーン**:
- [x] Phase 13D-1a〜1d 全て完了
- [ ] Stage 1 レポート再生成でコンパイル率向上確認 (要実行)

**Phase 13D-2: DEFSTRUCT Wasmコンパイル** ✅ 完了 (001-defstruct-wasm-compile)

```lisp
;; DEFSTRUCTマクロ → DEFCLASS展開は動作
;; アクセサ関数の生成: make-*, *-p, copy-*, slot-*
;; :conc-name, :include, :predicate, :copier, :constructor, :read-only サポート
```

- [x] DEFSTRUCTマクロのDEFCLASS展開
- [x] コンストラクタ(make-*)生成
- [x] アクセサ関数生成
- [x] :include継承サポート
- [x] アクセサ内のaref/svref対応

**Phase 13D-3: コンパイル時ディレクティブスキップ** ✅ 完了 (002-compile-time-directives)

- [x] `in-package` → :skipped 返却(コード生成なし)
- [x] `defpackage` → :skipped 返却
- [x] `declaim` → :skipped 返却
- [x] `proclaim` → :skipped 返却
- [x] Stage 1 レポートに skipped カウント追加 (458フォーム)

**Phase 13D-4: compile_form実装** 🎯 次のステップ

Stage 1がStage 2を生成できるよう、compile_form関数を実装。

```wat
;; 現在のスタブ
(func $compile_form (param $form externref) (result externref)
  ref.null extern)  ;; ← 何もしない

;; 目標の実装
(func $compile_form (param $form externref) (result externref)
  ;; Lispフォームを受け取り、Wasmバイトコードを返す
  ;; コンパイラのcompile-form関数を呼び出す
  ...)
```

**実装要件**:
1. S式入力の読み込み (Node.js FFI経由)
2. compile-to-wasm の呼び出し
3. Wasmバイト列の返却
4. エラーハンドリング

**前提条件**:
- コンパイル率が十分に高い (50%+ 目標)
- Stage 1 ランタイムが動作する (パッケージ、シンボル、I/O)

- [ ] compile_formエントリポイントの実装
- [ ] フォーム読み込み→AST変換→コード生成パイプライン
- [ ] 結果のWasmバイナリ返却
- [ ] host-shim からの呼び出しテスト

**Phase 13D-5: Stage 2生成・固定点検証**

- [ ] Stage 1でClysm全体をコンパイル
- [ ] Stage 2バイナリ生成
- [ ] Stage 1 == Stage 2 検証
- [ ] `./scripts/verify-fixpoint.sh` で ACHIEVED

**Phase 13D-6: 残存ブロッカー対応** (必要に応じて)

コンパイル率が目標に達しない場合の追加対応:

- [ ] TAGBODY/GO サポート (LOOPマクロ展開結果)
- [ ] BLOCK/RETURN-FROM 強化
- [ ] 複雑なlambda-list (&aux, supplied-p)
- [ ] FORMAT 拡張ディレクティブ (~{~}, ~[~])

#### 検証マイルストーン

| マイルストーン | コンパイル率 | Stage 1サイズ | 基準 | 状態 |
|---------------|-------------|---------------|------|------|
| M1: Phase 13D-1〜3完了 | 14% | 24.5KB | 基盤修正 | ✅ 完了 |
| M2: 再測定・分析 | ??% | ??KB | 効果確認 | 🎯 次 |
| M3: backend/完全動作 | 25% | 40KB | leb128.lisp等 | ⏳ |
| M4: reader/完全動作 | 35% | 60KB | tokenizer, parser | ⏳ |
| M5: compiler/完全動作 | 50% | 100KB | ast, codegen | ⏳ |
| M6: compile_form動作 | 70% | 200KB | Stage 2生成可能 | ⏳ |
| M7: 固定点達成 | 80%+ | 250KB+ | Stage 1 == Stage 2 | ⏳ |

#### 成功指標 (2025-12-31更新)

| 指標 | 現在 | 目標 | 備考 |
|-----|------|------|------|
| コンパイル率 | 13.84% (3585/26353) | 80%+ | Phase 13D-1〜3完了後再測定待ち |
| Stage 1サイズ | 24,651 bytes | 250KB+ | |
| Stage 2サイズ | 39 bytes | Stage 1と同等 | |
| 固定点 | NOT_ACHIEVED | ACHIEVED | |
| スキップ済み | 458 forms | - | ディレクティブ |

---

## 3. ANSI Tests成功率向上戦略

**目標**: ANSI準拠率を23.4% → 60%+へ向上（自己ホスティング達成と並行）

**原則**: **Clysmコンパイラで使われている機能ほど実装優先度が高い**

これにより、ANSIテスト成功率向上とセルフホスティング達成の両方を同時に進める。

### 3.1 優先度判定基準

| 優先度 | 基準 | Clysmコンパイラでの使用数 |
|--------|------|------------------------|
| P1 (最優先) | コンパイラコア機能で頻出 | 500回以上 |
| P2 (高) | コンパイラで多用 | 100-499回 |
| P3 (中) | コンパイラで使用 | 10-99回 |
| P4 (低) | ANSI準拠のみ、コンパイラ非使用 | 0-9回 |

### 3.2 P1: 最優先実装 (コンパイラコア機能)

これらの機能はClysmコンパイラの全モジュールで使用され、セルフホスティングの**必須条件**。

| 機能カテゴリ | 関数/マクロ | コンパイラ使用数 | ANSIテストカテゴリ | 実装状態 |
|-------------|------------|----------------|------------------|---------|
| **リスト操作** | cons, car, cdr, list, append | 1,200+ | cons | ✅ 基本済 |
| **シーケンス** | length, elt, subseq, concatenate | 600+ | sequences | ⚠️ 一部 |
| **変数束縛** | let, let*, setq, setf | 1,300+ | data-and-control-flow | ✅ 基本済 |
| **条件分岐** | if, cond, when, unless, case | 700+ | data-and-control-flow | ✅ 基本済 |
| **反復** | loop, dolist, dotimes, do | 600+ | iteration | ⚠️ loop一部 |
| **ハッシュテーブル** | make-hash-table, gethash, (setf gethash) | 310+ | hash-tables | ⚠️ 一部 |
| **配列アクセス** | aref, svref, (setf aref), make-array | 200+ | arrays | ❌ 未実装 |
| **等価性** | eq, eql, equal, equalp | 326+ | data-and-control-flow | ✅ 実装済 |

#### P1タスク詳細

**P1-1: 配列プリミティブ** (最重要 - defstruct展開後コードに必須)

```lisp
;; コンパイラで200+箇所使用
(aref array index)           ;; → Wasm array.get
(svref simple-vector index)  ;; → Wasm array.get
(setf (aref array index) val);; → Wasm array.set
(make-array dimensions ...)  ;; → Wasm array.new
```

- [ ] `aref` / `(setf aref)` プリミティブ実装
- [ ] `svref` / `(setf svref)` プリミティブ実装
- [ ] `make-array` 完全実装 (:initial-element, :initial-contents)
- [ ] ANSIテスト: arrays カテゴリ 50%+

**P1-2: シーケンス基盤** (コンパイラ文字列処理に必須)

```lisp
;; コンパイラで600+箇所使用
(length sequence)            ;; ✅ 実装済
(elt sequence index)         ;; → typecase分岐
(subseq sequence start end)  ;; → 新配列にコピー
(concatenate 'string ...)    ;; → 文字列結合
(coerce list 'vector)        ;; → 型変換
```

- [ ] `elt` / `(setf elt)` プリミティブ実装
- [ ] `subseq` プリミティブ実装
- [ ] `concatenate` プリミティブ実装
- [ ] `coerce` プリミティブ実装
- [ ] ANSIテスト: sequences カテゴリ 40%+

**P1-3: ハッシュテーブル完全実装** (コンパイラレジストリに必須)

```lisp
;; コンパイラで310+箇所使用
(make-hash-table :test 'eq)  ;; ⚠️ :test対応要
(gethash key table default)  ;; ⚠️ default対応要
(remhash key table)          ;; ❌ 未実装
(maphash fn table)           ;; ❌ 未実装
(clrhash table)              ;; ❌ 未実装
(hash-table-count table)     ;; ❌ 未実装
```

- [ ] `gethash` default引数対応
- [ ] `remhash` 実装
- [ ] `maphash` 実装
- [ ] `clrhash` 実装
- [ ] `hash-table-count` 実装
- [ ] LOOP `being the hash-keys` 対応
- [ ] ANSIテスト: hash-tables カテゴリ 60%+

**P1-4: LOOP完全実装** (コンパイラで168箇所使用)

```lisp
;; コンパイラで頻出するLOOPパターン
(loop for x in list ...)           ;; ✅ 基本済
(loop for i from 0 below n ...)    ;; ✅ 基本済
(loop for k being the hash-keys of ht ...) ;; ❌ 未実装
(loop with var = init ...)         ;; ⚠️ 一部
(loop collect x into result ...)   ;; ⚠️ 一部
(loop finally (return result))     ;; ⚠️ 一部
```

- [ ] `being the hash-keys/hash-values` 実装
- [ ] `with` 句完全サポート
- [ ] `finally` 句完全サポート
- [ ] `into` アキュムレータ
- [ ] ANSIテスト: iteration (loop) カテゴリ 50%+

### 3.3 P2: 高優先度実装 (コンパイラで多用)

| 機能カテゴリ | 関数/マクロ | コンパイラ使用数 | ANSIテストカテゴリ | 実装状態 |
|-------------|------------|----------------|------------------|---------|
| **リスト検索** | member, assoc, find, position | 200+ | cons, sequences | ⚠️ 一部 |
| **マッピング** | mapcar, mapc, mapcan | 200+ | cons | ⚠️ mapcarのみ |
| **型検査** | typep, type-of, typecase | 100+ | types | ⚠️ 一部 |
| **文字列** | string=, string<, char-code | 100+ | strings, characters | ⚠️ 一部 |
| **プロパティ** | getf, get, (setf getf) | 118+ | symbols | ⚠️ 一部 |
| **述語** | listp, consp, symbolp, stringp | 150+ | data-and-control-flow | ✅ 基本済 |

#### P2タスク詳細

**P2-1: リスト検索関数** (コンパイラ解析で使用)

```lisp
;; コンパイラで200+箇所使用
(member item list :test #'eq)    ;; ⚠️ :test対応要
(assoc key alist :test #'eq)     ;; ⚠️ :test対応要
(find item sequence :key #'fn)   ;; ❌ 未実装
(position item sequence ...)     ;; ❌ 未実装
```

- [ ] `member` :test, :key キーワード対応
- [ ] `assoc` / `rassoc` :test, :key キーワード対応
- [ ] `find` / `find-if` / `find-if-not` 実装
- [ ] `position` / `position-if` / `position-if-not` 実装
- [ ] ANSIテスト: cons (assoc/member) 60%+

**P2-2: マッピング関数** (コンパイラAST処理で使用)

```lisp
;; コンパイラで200+箇所使用
(mapcar #'fn list)      ;; ✅ 基本済
(mapc #'fn list)        ;; ❌ 未実装
(mapcan #'fn list)      ;; ❌ 未実装
(maplist #'fn list)     ;; ❌ 未実装
```

- [ ] `mapc` 実装 (副作用用mapcar)
- [ ] `mapcan` 実装 (結果をnconc)
- [ ] `maplist` / `mapl` 実装
- [ ] ANSIテスト: cons (map系) 70%+

**P2-3: 文字列操作** (エラーメッセージ、I/O処理)

```lisp
;; コンパイラで100+箇所使用
(string= str1 str2)           ;; ⚠️ 一部
(char str index)              ;; → schar
(string-upcase str)           ;; ❌ 未実装
(parse-integer str)           ;; ❌ 未実装
```

- [ ] `string=`, `string<`, `string>` 完全実装
- [ ] `char` / `schar` 実装
- [ ] `string-upcase` / `string-downcase` 実装
- [ ] `parse-integer` 実装
- [ ] ANSIテスト: strings カテゴリ 50%+

**P2-4: プロパティリスト** (CLOS、条件システム)

```lisp
;; コンパイラで118+箇所使用
(getf plist key)              ;; ⚠️ 一部
(get symbol key)              ;; ⚠️ 一部
(setf (getf plist key) val)   ;; ❌ 未実装
```

- [ ] `getf` 完全実装 (default引数)
- [ ] `(setf getf)` 実装
- [ ] `get` / `(setf get)` 実装
- [ ] `remprop` 実装
- [ ] ANSIテスト: symbols (plist) 60%+

### 3.4 P3: 中優先度実装 (コンパイラで使用)

| 機能カテゴリ | 関数/マクロ | コンパイラ使用数 | ANSIテストカテゴリ |
|-------------|------------|----------------|------------------|
| **数値関数** | abs, max, min, mod, rem | 50+ | numbers |
| **リスト構築** | push, pop, nconc, nreverse | 80+ | cons |
| **フォーマット** | format ~A ~S ~D | 93+ | printer |
| **制御フロー** | return-from, block, tagbody | 50+ | data-and-control-flow |
| **条件** | handler-case, restart-case | 20+ | conditions |
| **関数** | apply, funcall, function | 90+ | data-and-control-flow |

### 3.5 P4: 低優先度 (ANSI準拠のみ)

コンパイラでは使用されないが、ANSIテスト成功率のために実装。

| 機能カテゴリ | ANSIテストカテゴリ | 備考 |
|-------------|------------------|------|
| 三角関数 (sin, cos, tan) | numbers | 数学ライブラリ |
| 双曲線関数 (sinh, cosh) | numbers | 数学ライブラリ |
| 複素数 (complex, realpart) | numbers | 数値塔 |
| ファイル関数 (probe-file等) | files | FFI依存 |
| 環境関数 (get-decoded-time等) | environment | FFI依存 |

### 3.6 ANSIテスト成功率目標

| マイルストーン | 目標準拠率 | 主要タスク |
|--------------|-----------|-----------|
| M-ANSI-1 | 30% | P1-1 (配列), P1-2 (シーケンス) |
| M-ANSI-2 | 40% | P1-3 (ハッシュテーブル), P1-4 (LOOP) |
| M-ANSI-3 | 50% | P2-1〜P2-4 |
| M-ANSI-4 | 60% | P3全体 |

### 3.7 実装順序（依存関係考慮）

```
Week 1-2: P1-1 配列プリミティブ
    │
    ├──→ defstruct展開後コードがコンパイル可能に
    │
    ▼
Week 3-4: P1-2 シーケンス基盤
    │
    ├──→ 文字列処理、リスト→配列変換が動作
    │
    ▼
Week 5-6: P1-3 ハッシュテーブル完全実装
    │
    ├──→ コンパイラレジストリが完全動作
    │
    ▼
Week 7-8: P1-4 LOOP完全実装
    │
    ├──→ ハッシュテーブルイテレーション
    │
    ▼
Week 9-12: P2-1〜P2-4 高優先度
    │
    ├──→ ANSI準拠率 50%達成
    │
    ▼
Week 13+: P3, P4 中〜低優先度
    │
    └──→ ANSI準拠率 60%+達成
```

### 3.8 ANSIテストカテゴリ別目標

| カテゴリ | 現在 | 目標 | 優先度 | 根拠 |
|---------|------|------|--------|------|
| cons | 45% | 80% | P1 | car/cdr/append=1,200+使用 |
| sequences | 20% | 60% | P1 | length/elt=600+使用 |
| arrays | 5% | 50% | P1 | aref=200+使用 |
| hash-tables | 10% | 70% | P1 | gethash=310+使用 |
| iteration | 30% | 60% | P1 | loop=168+使用 |
| strings | 15% | 50% | P2 | string処理=100+使用 |
| characters | 20% | 60% | P2 | char-code=11+使用 |
| symbols | 30% | 60% | P2 | getf=118+使用 |
| numbers | 40% | 60% | P3 | 数値演算=50+使用 |
| data-and-control-flow | 35% | 55% | P1 | if/cond=700+使用 |
| conditions | 25% | 45% | P3 | handler-case=20+使用 |
| types | 20% | 50% | P2 | typep=71+使用 |

---

## 4. ANSI準拠率向上フェーズ (Phase 14-17)

**現在の準拠率**: 23.4% (219/936テスト)
**テストスイート**: pfdietz/ansi-test (~20,000テスト、31カテゴリ)

### Phase 14: 数値・算術強化 (ANSI-Numbers) ✅ 完了 (002-numeric-functions, 001-numeric-format)

**目標**: 数値関連テスト準拠率 50%+ → **達成**

#### 14A: 基本算術関数拡張 ✅ 完了

```lisp
;; 実装済み
(+ - * / mod rem floor ceiling truncate round)
(abs signum max min gcd lcm expt log exp sqrt)
(sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh)
(ash logand logior logxor lognot logcount integer-length)
```

**完了タスク**:
- [x] 三角関数群 (sin, cos, tan, asin, acos, atan)
- [x] 双曲線関数群 (sinh, cosh, tanh, asinh, acosh, atanh)
- [x] ビット演算関数 (ash, logand, logior, logxor, lognot, logcount, integer-length)
- [x] 数学関数 (exp, log, sqrt, expt, abs, signum)

#### 14B: 数値型述語強化 ✅ 完了

```lisp
;; 実装済み
(plusp minusp zerop oddp evenp)
(integer-length logbitp logtest)
```

**完了タスク**:
- [x] 符号述語 (plusp, minusp, zerop)
- [x] パリティ述語 (oddp, evenp)
- [x] ビット検査 (integer-length)

#### 14C: 数値変換・フォーマット ✅ 完了 (001-numeric-format)

```lisp
;; 実装済み
(parse-integer "123")           ; => 123
(write-to-string 42 :base 16)   ; => "2A"
(float 3/2)                     ; => 1.5
(rational 1.5)                  ; => 3/2
(rationalize 0.1)               ; => 連分数展開で正確な有理数
```

**完了タスク**:
- [x] `parse-integer` 完全実装
- [x] `float`, `rational` 変換
- [x] `rationalize` 連分数アルゴリズム実装
- [x] `write-to-string` :base 2-36対応 (整数、ratio、float)

#### 検証基準 ✅ 達成

- [x] `(sin (/ pi 2))` => 1.0 (許容誤差 1e-10)
- [x] `(ash 1 10)` => 1024
- [x] `(logand #xFF00 #x0FF0)` => #x0F00
- [x] `(rationalize 0.1)` => 1/10

---

### Phase 15: シーケンス・コレクション強化 (ANSI-Sequences) ⚠️ 一部完了

**目標**: シーケンス関連テスト準拠率 60%+

#### 15A: リスト操作拡張 ⚠️ 一部完了 (001-ansi-list-ops)

```lisp
;; 実装済み
(butlast nbutlast last nth nthcdr)
(adjoin pushnew intersection union set-difference)
(subsetp member member-if member-if-not)
(assoc assoc-if rassoc rassoc-if)
(pairlis acons copy-alist)
```

**完了タスク**:
- [x] リスト末尾操作 (butlast, last, nth, nthcdr)
- [x] 集合演算 (intersection, union, set-difference, subsetp)
- [x] 連想リスト操作 (assoc, rassoc, pairlis)

#### 15B: シーケンス汎用関数 ✅ 完了 (001-ansi-sequence-functions)

```lisp
;; 実装済み
(count count-if count-if-not)
(position position-if position-if-not)
(find find-if find-if-not)
(remove-duplicates delete-duplicates)
(mismatch search)
(substitute substitute-if nsubstitute nsubstitute-if)
(replace fill)
```

**完了タスク**:
- [x] 検索関数 (count, position, find) 完全実装
- [x] 置換関数 (substitute, replace)
- [x] 重複削除 (remove-duplicates, delete-duplicates)
- [x] 比較関数 (mismatch, search)

#### 15C: 配列操作 ✅ 完了 (001-ansi-array-ops)

```lisp
;; 実装済み
(array-rank array-dimension array-dimensions)
(array-total-size array-row-major-index)
(row-major-aref (setf row-major-aref))
(adjustable-array-p adjust-array)
```

**完了タスク**:
- [x] 配列属性アクセサ完全実装 (array-rank, array-dimension, array-dimensions, array-total-size)
- [x] 多次元配列サポート ($mdarray型, インデックス28)
- [x] row-major-aref / (setf row-major-aref)
- [x] 調整可能配列 (adjustable-array-p, adjust-array)

#### 15D: 配列プリミティブ ✅ 完了 (001-ansi-array-primitives)

```lisp
;; 実装済み
(aref svref schar elt)
((setf aref) (setf svref) (setf schar) (setf elt))
(coerce)
```

**完了タスク**:
- [x] `aref` / `(setf aref)` プリミティブ実装
- [x] `svref` / `(setf svref)` プリミティブ実装
- [x] `schar` / `(setf schar)` 実装
- [x] `elt` / `(setf elt)` 実装
- [x] `coerce` 実装

#### 検証基準 ✅ 達成

- [x] `(count 1 '(1 2 1 3 1))` => 3
- [x] `(intersection '(1 2 3) '(2 3 4))` => (2 3) or (3 2)
- [x] `(remove-duplicates '(a b a c b))` => (A B C) or equivalent
- [x] `(aref #(1 2 3) 1)` => 2
- [x] `(array-dimensions #2A((1 2) (3 4)))` => (2 2)

---

### Phase 16: 文字列・文字強化 (ANSI-Strings) ✅ 完了

**目標**: 文字列・文字テスト準拠率 70%+ → **達成**

#### 16A: 文字関数 ✅ 完了 (001-ansi-char-functions)

```lisp
;; 実装済み
(char-upcase char-downcase)
(alpha-char-p digit-char-p alphanumericp)
(upper-case-p lower-case-p both-case-p)
(graphic-char-p standard-char-p)
(char-name name-char digit-char)
(char-int)
```

**完了タスク**:
- [x] ケース変換 (char-upcase, char-downcase)
- [x] 文字分類述語完全実装 (alpha-char-p, digit-char-p, alphanumericp, graphic-char-p, standard-char-p, both-case-p, upper-case-p, lower-case-p)
- [x] 文字名変換 (char-name, name-char)
- [x] 数字変換 (digit-char, char-int)

#### 16B: 文字列操作 ✅ 完了 (001-ansi-string-trim)

```lisp
;; 実装済み
(string-upcase string-downcase string-capitalize)
(nstring-upcase nstring-downcase nstring-capitalize)
(string-trim string-left-trim string-right-trim)
;; :start/:end キーワードサポート
```

**完了タスク**:
- [x] 文字列ケース変換完全実装 (string-upcase, string-downcase, string-capitalize)
- [x] 破壊的ケース変換 (nstring-upcase, nstring-downcase, nstring-capitalize)
- [x] 文字列トリム関数 (string-trim, string-left-trim, string-right-trim)
- [x] :start, :end キーワード引数サポート

#### 16C: パース・構築 ✅ 完了 (001-ansi-sequence-operations)

```lisp
;; 実装済み
(make-string length :initial-element char)
(subseq string start end)
(concatenate 'string ...)
```

**完了タスク**:
- [x] `make-string` 完全実装
- [x] `subseq` 文字列サポート
- [x] `concatenate` 文字列サポート

#### 検証基準 ✅ 達成

- [x] `(string-upcase "hello")` => "HELLO"
- [x] `(string-trim " " " test ")` => "test"
- [x] `(alpha-char-p #\a)` => T
- [x] `(char-name #\Newline)` => "Newline"
- [x] `(nstring-capitalize "hello world")` => "Hello World"

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
│       ├── func-section.lisp # プリミティブのみ (cons,car,+,-,eq等)
│       ├── globals.lisp      # グローバル変数生成
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
├── lib/                  # 標準ライブラリ (Lispで実装)
│   ├── runtime.lisp      # ランタイム関数 (princ, assoc, member等)
│   ├── io.lisp           # I/O関数 (format, write-line等)
│   ├── sequences.lisp    # シーケンス関数 (find, remove, position等)
│   ├── list-ops.lisp     # リスト操作 (assoc, member, mapcar等)
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

## 5. 優先順位マップ (2025-12-31更新)

```
Phase 0-8 [言語機能] ✅ 完了
    │
    ▼
Phase 9-13 [セルフホスティング基盤] ✅ インフラ完了
    ├── 9A-D [基盤機能] ✅
    ├── 10A-C [SBCL非依存化] ✅
    ├── 11A-C [コンパイラ検証] ✅
    ├── 12A-C [クロスコンパイル] ✅
    └── 13A-C [固定点インフラ] ✅
    │
    ▼
Phase 14 [数値・算術強化] ✅ 完了
    ├── 14A [算術関数拡張] ✅
    ├── 14B [数値型述語] ✅
    └── 14C [数値変換] ✅
    │
    ▼
Phase 15 [シーケンス強化] ⚠️ 一部完了
    ├── 15A [リスト操作] ✅
    ├── 15B [汎用シーケンス関数] ✅
    ├── 15C [配列操作] ✅
    └── 15D [配列プリミティブ] ✅
    │
    ▼
Phase 16 [文字列・文字強化] ✅ 完了
    ├── 16A [文字関数] ✅
    ├── 16B [文字列操作] ✅
    └── 16C [パース・構築] ✅
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 13D [真のセルフホスティング] ◀── 🎯 現在の最優先       │
│     ├── 13D-1 [DEFUN本体コンパイル修正] 🔴 最優先            │
│     ├── 13D-2 [DEFSTRUCT Wasmコンパイル] ✅ 完了             │
│     ├── 13D-3 [コンパイル時ディレクティブ] ⏳                │
│     ├── 13D-4 [compile_form実装] ⏳                         │
│     └── 13D-5 [固定点達成] ⏳                               │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
Phase 17 [制御フロー拡張]          ◀── Phase 13D完了後
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

### セルフホスティング達成ロードマップ

| マイルストーン | コンパイル率 | Stage 1 | 状態 |
|---------------|-------------|---------|------|
| 現在 | 14.11% | 24.5KB | 🔴 |
| DEFUN修正完了 | 40% | 60KB | ⏳ |
| DEFSTRUCT完全動作 | 55% | 100KB | ⏳ |
| compile_form実装 | 70% | 200KB | ⏳ |
| **固定点達成** | **80%+** | **250KB+** | 🎯 |

### ANSI準拠率ロードマップ

| Phase | 目標準拠率 | 主要カテゴリ | 状態 |
|-------|-----------|-------------|------|
| Phase 14 | 35% | numbers 50%+ | ✅ 完了 |
| Phase 15 | 45% | sequences 60%+, cons 50%+ | ⚠️ 一部 |
| Phase 16 | 55% | strings 70%+, characters 80%+ | ✅ 完了 |
| Phase 13D完了 | 60% | セルフホスティング達成 | 🎯 目標 |
| Phase 17完了 | 70% | iteration 50%+, control-flow 50%+ | ⏳ |
| Phase 18完了 | 75% | streams 基本サポート | ⏳ |
| Phase 19完了 | 80% | objects/CLOS 60%+ | ⏳ |

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
| 2.2.0 | 2025-12-29 | Phase 13Dブロッカー分析を大幅修正。「内部関数をプリミティブ登録」は誤りであり、「ANSI CL標準関数（coerce, aref, svref等）をプリミティブ実装」が正しいアプローチと判明。make-ast-literal等はdefstructが生成する関数、encode-unsigned-leb128等は通常のdefunであり、これらが使用するANSI CL関数が未実装であることが根本原因。依存関係グラフを追加 |
| 2.3.0 | 2025-12-29 | 「ANSI Tests成功率向上戦略」セクション(§3)を追加。Clysmコンパイラでの機能使用頻度に基づく優先度判定基準(P1〜P4)を導入。P1最優先: 配列(aref/svref)、シーケンス(elt/subseq/coerce)、ハッシュテーブル(maphash/remhash)、LOOP完全実装。P2高優先: リスト検索(find/position)、マッピング(mapc/mapcan)、文字列操作(string-upcase)、プロパティ(setf getf)。ANSIテストカテゴリ別目標を設定(cons 80%、sequences 60%、arrays 50%等) |
| 2.4.0 | 2025-12-30 | Phase 13D-2 ANSI CL Sequence Operations MVP完了 (001-ansi-sequence-operations)。subseq, concatenate, make-string, make-array拡張, copy-seq, array.copy instruction (全てstring-only)。vector/list対応はPhase 13D-2bとして延期 |
| **3.0.0** | **2025-12-31** | **大規模更新: セルフホスティング達成を最優先目標として再構成。Phase 14(数値強化)✅完了、Phase 15(シーケンス)一部完了、Phase 16(文字列・文字)✅完了を反映。Phase 13Dを全面改訂: DEFUNブロッカー(17,580フォーム、82%)が最大の問題と特定。stage1-report.jsonの最新データ(14.11%コンパイル率、24.5KB Stage 1)を反映。検証マイルストーン(M1-M7)と具体的な成功指標を追加。優先順位マップを更新しPhase 13Dを最優先に配置** |
| **3.1.0** | **2025-12-31** | **DEFUN調査完了: 5つの具体的根本原因を特定。(1) `compile-quoted-element`にcharacterp欠落 (func-section.lisp:509-525)、(2) `1-`/`1+`プリミティブ未登録、(3) `print`/`format`未登録、(4) `make-instance*`未登録 (DEFSTRUCT用)、(5) etypecase TYPE-ERROR参照問題。Phase 13D-1を13D-1a〜13D-1dに細分化し、具体的なコード修正箇所と検証テストを追加。test-defun-compile.lisp/test-leb128-compile.lispによるテスト結果を文書化 (成功7/12、失敗5/12)** |
| **3.2.0** | **2026-01-01** | **アーキテクチャ方針転換: プリミティブとランタイムライブラリの分離を決定。func-section.lisp (18,000行) の問題点を認識 — 全ての標準関数をWasm命令に直接変換しようとするアプローチはスケールしない。新方針: (1) Layer 1 (プリミティブ): cons, car, cdr, +, -, eq等のみをfunc-section.lispで実装、(2) Layer 2 (ランタイム): assoc, member, princ, format等はsrc/clysm/lib/にLispで実装し、コンパイラでWasmにコンパイル。移行計画 (Phase 13D-R1〜R4) を追加。目標: func-section.lisp 8,000行以下** |
