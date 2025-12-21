# Implementation Plan: Clysm - WebAssembly GC Common Lisp Compiler

**Branch**: `001-clysm-compiler` | **Date**: 2025-12-21 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-clysm-compiler/spec.md`
**Constitution**: v1.0.0 (.specify/memory/constitution.md)

## Summary

WebAssembly GCをターゲットとしたCommon Lispコンパイラ「Clysm」の段階的実装。
ホストSBCLによるクロスコンパイル方式で、Phase 0（基盤）からPhase 7（CLOS）まで
8フェーズで構築する。TDD厳守・Nix-First・WasmGC-First型システムを原則とする。

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+) - コンパイラ本体、WAT/Wasm - 出力
**Primary Dependencies**:
- SBCL: ホストコンパイラ・開発環境
- wasm-tools: Wasm検証・操作
- wasmtime: Wasm実行環境（WasmGC対応版 v27+）
- Rove: Common Lispテストフレームワーク

**Storage**: N/A（ファイルI/Oのみ、WASI経由）

**Testing**:
- Rove: Common Lisp単体テスト
- wasm-tools validate: 契約テスト（生成バイナリ検証）
- wasmtime: 統合テスト（実行結果検証）

**Target Platform**: WebAssembly (WasmGC + tail-call + EH proposals)
**Project Type**: Single（コンパイラプロジェクト）

**Performance Goals** (from Constitution):
| 指標 | 目標値 |
|------|--------|
| Fixnum算術 | ネイティブi32の1.5倍以内 |
| コンス生成 | 10M cells/sec以上 |
| 関数呼び出し | 間接呼び出し5ns以内 |
| JITコンパイル | 1000行/100ms以内 |

**Constraints**:
- Wasmサンドボックス境界を破らない
- 線形メモリ直接アクセス禁止
- 全コミットで`nix flake check`パス必須
- 全Wasmバイナリは`wasm-tools validate`パス必須

**Scale/Scope**: ANSI Common Lisp準拠を目指す段階的実装（Phase 0-7）

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### 原則準拠マトリクス

| 原則 | ID | 適用 | 実装方針 |
|------|-----|------|----------|
| I. WasmGC-First型システム | CONST-I | PASS | anyref/i31ref/struct/array使用、線形メモリ禁止 |
| II. Lispオブジェクト表現 | CONST-II | PASS | NIL=シングルトン構造体、UNBOUND=センチネル |
| III. 関数・クロージャ | CONST-III | PASS | 多形ディスパッチ構造体（$code_0/1/2/N + $env） |
| IV. Wasm制御フロー活用 | CONST-IV | PASS | return_call + try_table/throw |
| V. シャローバインディング | CONST-V | PASS | シンボル$value + バインディングスタック |
| VI. 段階的動的コンパイル | CONST-VI | PASS | Tier1=インタプリタ、Tier2=動的Wasm生成 |
| VII. TDD非交渉 | CONST-VII | **CRITICAL** | Red-Green-Refactor厳守、テスト先行 |
| VIII. Nix-First | CONST-VIII | **CRITICAL** | flake.nix必須、全コミットでcheck通過 |

### 必須ゲート

- [ ] **G1**: flake.nix存在 & `nix develop`成功
- [ ] **G2**: テストフレームワーク（Rove）動作
- [ ] **G3**: wasm-tools & wasmtime がdevShellに含まれる
- [ ] **G4**: 空Wasmモジュールがvalidate通過

## Project Structure

### Documentation (this feature)

```text
specs/001-clysm-compiler/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── wasm-sections.md # Wasmセクション構造契約
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/
├── backend/              # Wasmバイナリ出力
│   ├── leb128.lisp       # LEB128エンコーディング
│   ├── sections.lisp     # セクション構造
│   ├── wasm-emit.lisp    # バイナリエミッタ
│   └── wat-print.lisp    # WAT出力（デバッグ用）
│
├── compiler/             # コンパイラ本体
│   ├── ast.lisp          # 抽象構文木
│   ├── analyzer/         # 意味解析
│   │   ├── free-vars.lisp
│   │   ├── tail-call.lisp
│   │   └── type-infer.lisp
│   ├── transform/        # AST変換
│   │   ├── closure.lisp
│   │   └── macro.lisp
│   └── codegen/          # コード生成
│       ├── wasm-ir.lisp
│       ├── type-section.lisp
│       ├── func-section.lisp
│       └── gc-types.lisp
│
├── reader/               # S式リーダー
│   ├── tokenizer.lisp
│   ├── parser.lisp
│   └── package.lisp
│
├── runtime/              # ランタイムサポート
│   ├── objects.lisp
│   ├── gc-bridge.lisp
│   ├── special-vars.lisp
│   └── multi-value.lisp
│
├── eval/                 # 動的評価
│   ├── interpreter.lisp
│   └── jit.lisp
│
├── clos/                 # オブジェクトシステム
│   ├── mop.lisp
│   ├── dispatch.lisp
│   └── slot-access.lisp
│
└── clysm.asd             # ASDFシステム定義

tests/
├── contract/             # Wasm構造検証
│   ├── leb128-test.lisp
│   ├── sections-test.lisp
│   └── wasm-validate-test.lisp
├── integration/          # コンパイル→実行検証
│   ├── arithmetic-test.lisp
│   ├── closure-test.lisp
│   └── control-flow-test.lisp
└── unit/                 # 単体テスト
    ├── ast-test.lisp
    ├── analyzer-test.lisp
    └── codegen-test.lisp
```

**Structure Decision**: implementation-plan.mdのアーキテクチャ設計に準拠したSingleプロジェクト構成

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| 多層コンパイラ構造 | CL→AST→IR→Wasm変換の複雑さ | 直接変換では最適化困難 |
| 2段階Eval (Tier1/2) | 憲法VI準拠：即応性とJIT最適化の両立 | 単一方式では要件未達 |

---

## Phase Implementation Plan

### Critical Path

```
[Phase 0: 基盤] ─────────────────────────────────────────────────────►
       │
       ├── Nix環境構築 ──► LEB128エミッタ ──► セクション生成 ──► 空モジュール検証
       │
       ▼
[Phase 1: Lisp-0] ─────────────────────────────────────────────────►
       │
       ├── Fixnum/i31ref ──► 算術演算 ──► 条件分岐 ──► let束縛 ──► defun
       │
       ▼
[Phase 2: Lisp-1] ─────────────────────────────────────────────────►
       │
       ├── クロージャ構造体 ──► 自由変数解析 ──► 環境キャプチャ ──► Tail Call
       │
       ▼
[Phase 3: Lisp-2] ─────────────────────────────────────────────────►
       │
       ├── Exception Handling ──► block/return-from ──► unwind-protect
       │
       ▼
[Phase 4: Lisp-3] ─────────────────────────────────────────────────►
       │
       ├── シャローバインディング ──► S式リーダー ──► REPL基盤
       │
       ▼
[Phase 5: Lisp-4] ─────────────────────────────────────────────────►
       │
       ├── マクロ展開器 ──► バッククォート ──► defmacro
       │
       ▼
[Phase 6: Lisp-5] ─────────────────────────────────────────────────►
       │
       ├── Tier1インタプリタ ──► Tier2 JIT ──► 動的リンク
       │
       ▼
[Phase 7: Lisp-6] ─────────────────────────────────────────────────►
       │
       └── クラス定義 ──► インスタンス生成 ──► メソッドディスパッチ
```

---

## Phase 0: 基盤構築 (Foundation)

**目標**: 開発環境とWasmバイナリ生成基盤の確立
**MVP**: 空Wasmモジュールの生成・検証

### タスク

#### P0-T1: Nix Flakes環境構築

**依存**: なし
**完了定義 (DoD)**:
- [ ] `flake.nix` が存在
- [ ] `nix develop` でSBCL, wasm-tools, wasmtime利用可能
- [ ] `nix flake check` がパス
- [ ] CI用GitHub Actions設定（optional）

**テスト先行**:
```bash
# 期待: 各コマンドがバージョン情報を出力
sbcl --version
wasm-tools --version
wasmtime --version
```

#### P0-T2: ASDFシステム定義

**依存**: P0-T1
**DoD**:
- [ ] `clysm.asd` が存在
- [ ] `(asdf:load-system :clysm)` が成功
- [ ] Roveテストフレームワーク統合
- [ ] `(rove:run :clysm/tests)` が実行可能

#### P0-T3: LEB128エンコーダ/デコーダ

**依存**: P0-T2
**DoD**:
- [ ] `encode-unsigned-leb128` 関数が正しく動作
- [ ] `encode-signed-leb128` 関数が正しく動作
- [ ] 境界値テスト（0, 127, 128, -1, -64, -65等）がパス
- [ ] 単体テスト5件以上

**テスト先行例**:
```lisp
(deftest test-leb128-encode-unsigned
  (ok (equalp #(0) (encode-unsigned-leb128 0)))
  (ok (equalp #(127) (encode-unsigned-leb128 127)))
  (ok (equalp #(128 1) (encode-unsigned-leb128 128)))
  (ok (equalp #(255 127) (encode-unsigned-leb128 16383))))
```

#### P0-T4: Wasmセクション構造生成

**依存**: P0-T3
**DoD**:
- [ ] セクションID 0-13を正しい順序で出力
- [ ] マジックナンバー `\0asm` と バージョン `\01\00\00\00` 出力
- [ ] 空Type/Function/Codeセクション生成
- [ ] 契約テスト: `wasm-tools validate` パス

#### P0-T5: 空モジュール生成・検証

**依存**: P0-T4
**DoD**:
- [ ] `(emit-empty-module)` が有効なWasmバイナリを生成
- [ ] `wasm-tools validate output.wasm` がパス
- [ ] `wasm-tools print output.wasm` がWATを出力
- [ ] 統合テスト1件以上

**検証コマンド**:
```bash
wasm-tools validate output.wasm && echo "PASS"
```

#### P0-T6: WAT出力（デバッグ用）

**依存**: P0-T4
**DoD**:
- [ ] 内部IRからWATテキスト形式を出力
- [ ] 出力WATが`wat2wasm`でバイナリ化可能
- [ ] 単体テスト3件以上

---

## Phase 1: 最小Lispサブセット (Lisp-0)

**目標**: Fixnum算術と基本的な関数定義が動作
**MVP**: `(+ 1 2)` => 3 の実行

### 型定義（WasmGC）

```wat
;; Phase 1で定義する型
(type $nil (struct))  ;; NILシングルトン
(type $unbound (struct))  ;; UNBOUNDセンチネル

(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))))

(type $symbol (struct
  (field $name (ref $string))
  (field $value (mut anyref))
  (field $function (mut anyref))
  (field $plist (mut anyref))
  (field $package (mut anyref))))

(type $string (array (mut i8)))
```

### タスク

#### P1-T1: WasmGC型定義生成

**依存**: P0-T5
**DoD**:
- [ ] Type Section に $nil, $unbound, $cons, $symbol 型定義
- [ ] i31ref がFixnumとして利用可能
- [ ] `wasm-tools validate` パス
- [ ] 型定義テスト5件以上

#### P1-T2: NIL/UNBOUNDシングルトン

**依存**: P1-T1
**DoD**:
- [ ] Global Section に NIL/UNBOUND インスタンス定義
- [ ] `ref.eq` による等価性判定が動作
- [ ] NILがWasm nullではないことを検証
- [ ] 単体テスト3件以上

#### P1-T3: Fixnum算術演算

**依存**: P1-T2
**DoD**:
- [ ] `+`, `-`, `*`, `/`（整数除算）のコンパイル
- [ ] i31ref ⇔ i32 変換（`ref.i31`, `i31.get_s`）
- [ ] `(+ 1 2)` => 3 が wasmtime で実行可能
- [ ] 算術テスト10件以上

**テスト先行**:
```lisp
(deftest test-fixnum-add
  (ok (= 3 (compile-and-run '(+ 1 2))))
  (ok (= 7 (compile-and-run '(+ 1 (* 2 3)))))
  (ok (= 0 (compile-and-run '(- 5 5)))))
```

#### P1-T4: 比較演算・条件分岐

**依存**: P1-T3
**DoD**:
- [ ] `<`, `>`, `<=`, `>=`, `=` のコンパイル
- [ ] `if` フォームのコンパイル（then/else両方）
- [ ] 結果がNIL/Tではなく分岐値を返す
- [ ] 条件分岐テスト5件以上

#### P1-T5: レキシカル変数（let/let*）

**依存**: P1-T4
**DoD**:
- [ ] `let` による並列束縛
- [ ] `let*` による逐次束縛
- [ ] ローカル変数のスタック割り当て
- [ ] 変数テスト5件以上

#### P1-T6: トップレベル関数（defun）

**依存**: P1-T5
**DoD**:
- [ ] `defun` による関数定義
- [ ] 関数シンボルへの登録
- [ ] 関数呼び出しの正常動作
- [ ] `(defun f (x) x)` + `(f 42)` => 42
- [ ] 関数テスト5件以上

#### P1-T7: Phase 1統合テスト

**依存**: P1-T6
**DoD**:
- [ ] 10個以上のFixnum算術テストがパス
- [ ] 5個以上の関数定義・呼び出しテストがパス
- [ ] 全生成Wasmが`wasm-tools validate`パス
- [ ] `nix flake check` パス

---

## Phase 2: クロージャと再帰 (Lisp-1)

**目標**: ファーストクラス関数とTail Call最適化
**MVP**: `(funcall (lambda (x) (+ x 1)) 10)` => 11

### クロージャ構造体（憲法III準拠）

```wat
(type $func_0 (func (param (ref $closure)) (result anyref)))
(type $func_1 (func (param (ref $closure) anyref) (result anyref)))
(type $func_2 (func (param (ref $closure) anyref anyref) (result anyref)))
(type $func_N (func (param (ref $closure) (ref $list)) (result anyref)))

(type $closure (struct
  (field $code_0 (ref null $func_0))
  (field $code_1 (ref null $func_1))
  (field $code_2 (ref null $func_2))
  (field $code_N (ref null $func_N))
  (field $env (mut anyref))))
```

### タスク

#### P2-T1: クロージャ構造体定義

**依存**: P1-T7
**DoD**:
- [ ] $closure 型定義がType Sectionに出力
- [ ] 各アリティ用関数型（$func_0/1/2/N）定義
- [ ] `wasm-tools validate` パス
- [ ] 型テスト3件以上

#### P2-T2: 自由変数解析（Free Variable Analysis）

**依存**: P2-T1
**DoD**:
- [ ] ASTから自由変数を収集
- [ ] レキシカルスコープの正しい判定
- [ ] ネストしたlambdaでの解析
- [ ] 解析テスト5件以上

#### P2-T3: 環境構造体生成

**依存**: P2-T2
**DoD**:
- [ ] 自由変数を格納する環境struct生成
- [ ] クロージャの$envフィールドに設定
- [ ] 環境アクセスの正常動作
- [ ] 環境テスト3件以上

#### P2-T4: lambda/funcall実装

**依存**: P2-T3
**DoD**:
- [ ] `lambda` フォームのコンパイル
- [ ] `funcall` による間接呼び出し
- [ ] アリティに応じたエントリーポイント選択
- [ ] `(funcall (lambda (x) (+ x 1)) 10)` => 11
- [ ] lambdaテスト5件以上

#### P2-T5: 末尾呼び出し最適化

**依存**: P2-T4
**DoD**:
- [ ] 末尾位置の検出（Tail Position Analysis）
- [ ] `return_call` / `return_call_ref` への変換
- [ ] 末尾再帰でスタック成長なし
- [ ] `(fact 100)` がスタックオーバーフローなし
- [ ] TCOテスト3件以上

#### P2-T6: labels/flet実装

**依存**: P2-T5
**DoD**:
- [ ] `labels` によるローカル関数（再帰可能）
- [ ] `flet` によるローカル関数（非再帰）
- [ ] 相互再帰のサポート
- [ ] ローカル関数テスト5件以上

#### P2-T7: Phase 2統合テスト

**依存**: P2-T6
**DoD**:
- [ ] クロージャ変数キャプチャテスト5件以上
- [ ] 末尾再帰テスト（大きなN値）3件以上
- [ ] 全生成Wasmが`wasm-tools validate`パス
- [ ] `nix flake check` パス

---

## Phase 3: 制御フローと例外処理 (Lisp-2)

**目標**: 非局所脱出とunwind-protect
**MVP**: `(block nil (return-from nil 42) 0)` => 42

### Exception Handling（憲法IV準拠）

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

### タスク

#### P3-T1: Exception Handling基盤

**依存**: P2-T7
**DoD**:
- [ ] Tag定義（Exception Section）
- [ ] `try_table` / `throw` 命令生成
- [ ] 基本的な例外捕捉動作
- [ ] EHテスト3件以上

#### P3-T2: block/return-from実装

**依存**: P3-T1
**DoD**:
- [ ] `block` フォームのコンパイル
- [ ] `return-from` による非局所脱出
- [ ] ネストしたblockの正しい脱出
- [ ] block/return-fromテスト5件以上

#### P3-T3: tagbody/go実装

**依存**: P3-T2
**DoD**:
- [ ] `tagbody` によるラベル定義
- [ ] `go` によるジャンプ
- [ ] 複数ラベルの正しいディスパッチ
- [ ] tagbody/goテスト3件以上

#### P3-T4: catch/throw実装

**依存**: P3-T2
**DoD**:
- [ ] `catch` フォームのコンパイル
- [ ] `throw` による動的例外
- [ ] タグのシンボルマッチング
- [ ] catch/throwテスト3件以上

#### P3-T5: unwind-protect実装

**依存**: P3-T4
**DoD**:
- [ ] `unwind-protect` のコンパイル
- [ ] 正常終了時のクリーンアップ実行
- [ ] 例外発生時のクリーンアップ実行
- [ ] クリーンアップ後の例外再投入
- [ ] unwind-protectテスト5件以上

#### P3-T6: Phase 3統合テスト

**依存**: P3-T5
**DoD**:
- [ ] 制御フローテスト10件以上
- [ ] ネスト例外処理テスト3件以上
- [ ] 全生成Wasmが`wasm-tools validate`パス
- [ ] `nix flake check` パス

---

## Phase 4: 動的スコープとリーダー (Lisp-3)

**目標**: スペシャル変数とS式読み込み
**MVP**: 基本的なREPL動作

### シャローバインディング（憲法V準拠）

```wat
(global $binding_stack (mut (ref null $binding_frame)))

(type $binding_frame (struct
  (field $symbol (ref $symbol))
  (field $old_value anyref)
  (field $next (ref null $binding_frame))))
```

### タスク

#### P4-T1: バインディングスタック実装

**依存**: P3-T6
**DoD**:
- [ ] $binding_frame 型定義
- [ ] グローバルスタック管理
- [ ] push/pop操作
- [ ] スタックテスト3件以上

#### P4-T2: スペシャル変数（defvar/defparameter）

**依存**: P4-T1
**DoD**:
- [ ] `defvar` / `defparameter` のコンパイル
- [ ] シンボル$valueフィールドへの値設定
- [ ] 動的束縛時の値保存・復元
- [ ] スペシャル変数テスト5件以上

#### P4-T3: 動的束縛（let with special）

**依存**: P4-T2
**DoD**:
- [ ] スペシャル変数の動的let束縛
- [ ] `unwind-protect`による自動復元
- [ ] ネスト動的束縛の正しい動作
- [ ] 動的束縛テスト5件以上

#### P4-T4: トークナイザ（字句解析）

**依存**: P3-T6
**DoD**:
- [ ] 文字ストリームからトークン抽出
- [ ] シンボル、数値、文字列の認識
- [ ] 特殊文字（括弧、クォート等）の処理
- [ ] トークナイザテスト10件以上

#### P4-T5: S式パーサー（構文解析）

**依存**: P4-T4
**DoD**:
- [ ] 再帰下降パーサー実装
- [ ] リスト、ドット対、クォート処理
- [ ] エラー位置報告
- [ ] パーサーテスト10件以上

#### P4-T6: シンボルインターン

**依存**: P4-T5
**DoD**:
- [ ] パッケージ基盤（COMMON-LISP, KEYWORD）
- [ ] シンボルインターン処理
- [ ] `intern` / `find-symbol` 動作
- [ ] インターンテスト5件以上

#### P4-T7: REPL基盤

**依存**: P4-T3, P4-T6
**DoD**:
- [ ] Read-Eval-Printループ動作
- [ ] エラー時の継続
- [ ] 簡易プロンプト表示
- [ ] REPLテスト3件以上

#### P4-T8: Phase 4統合テスト

**依存**: P4-T7
**DoD**:
- [ ] `(read-from-string "(+ 1 2)")` が正しくパース
- [ ] スペシャル変数の動的束縛が正常動作
- [ ] 全生成Wasmが`wasm-tools validate`パス
- [ ] `nix flake check` パス

---

## Phase 5: マクロシステム (Lisp-4)

**目標**: defmacroとバッククォート
**MVP**: `(when t 1 2 3)` => 3

### タスク

#### P5-T1: コンパイル時環境

**依存**: P4-T8
**DoD**:
- [ ] マクロ関数登録機構
- [ ] コンパイル時シンボルテーブル
- [ ] 環境テスト3件以上

#### P5-T2: マクロ展開器

**依存**: P5-T1
**DoD**:
- [ ] フォーム走査時のマクロ検出
- [ ] マクロ関数の実行（ホストSBCL）
- [ ] 展開結果の再帰処理
- [ ] 展開テスト5件以上

#### P5-T3: バッククォート処理

**依存**: P5-T2
**DoD**:
- [ ] `quasiquote` (`) の変換
- [ ] `unquote` (,) の変換
- [ ] `unquote-splicing` (,@) の変換
- [ ] ネストバッククォート
- [ ] バッククォートテスト5件以上

#### P5-T4: defmacro実装

**依存**: P5-T3
**DoD**:
- [ ] `defmacro` フォームのコンパイル
- [ ] `&body`, `&rest`, `&optional` 対応
- [ ] マクロ関数のコンパイル時登録
- [ ] defmacroテスト5件以上

#### P5-T5: 標準マクロ実装

**依存**: P5-T4
**DoD**:
- [ ] `when`, `unless` マクロ
- [ ] `cond` マクロ
- [ ] `dolist`, `dotimes` マクロ
- [ ] 標準マクロテスト10件以上

#### P5-T6: Phase 5統合テスト

**依存**: P5-T5
**DoD**:
- [ ] `(when t 1 2 3)` => 3
- [ ] ユーザー定義マクロの正常動作
- [ ] 全生成Wasmが`wasm-tools validate`パス
- [ ] `nix flake check` パス

---

## Phase 6: Eval/JIT (Lisp-5)

**目標**: 動的コンパイル（憲法VI準拠）
**MVP**: `(eval '(+ 1 2))` => 3

### 2段階実行モデル

- **Tier 1**: Wasm内インタプリタ（即応性優先）
- **Tier 2**: 動的Wasmモジュール生成（JIT）

### タスク

#### P6-T1: Tier 1インタプリタ

**依存**: P5-T6
**DoD**:
- [ ] S式評価器（Wasmで実装）
- [ ] 特殊フォーム処理（quote, if, lambda等）
- [ ] 関数適用処理
- [ ] インタプリタテスト10件以上

#### P6-T2: eval基盤

**依存**: P6-T1
**DoD**:
- [ ] `eval` 関数の実装
- [ ] 環境引数のサポート
- [ ] `(eval '(+ 1 2))` => 3
- [ ] evalテスト5件以上

#### P6-T3: Tier 2 Wasmバイナリ生成

**依存**: P6-T2
**DoD**:
- [ ] S式からWasmバイナリ（Uint8Array）生成
- [ ] インメモリコンパイル
- [ ] 生成バイナリの検証
- [ ] バイナリ生成テスト5件以上

#### P6-T4: 動的モジュールインスタンス化

**依存**: P6-T3
**DoD**:
- [ ] ホスト`WebAssembly.instantiate()`呼び出し
- [ ] ランタイムインポートの設定
- [ ] funcref取得
- [ ] インスタンス化テスト3件以上

#### P6-T5: 動的リンク

**依存**: P6-T4
**DoD**:
- [ ] メインランタイムGCヒープ共有
- [ ] シンボル関数スロットへのホットパッチ
- [ ] 関数テーブルインポート
- [ ] リンクテスト3件以上

#### P6-T6: compile関数実装

**依存**: P6-T5
**DoD**:
- [ ] `compile` 関数の実装
- [ ] Tier 1/Tier 2切り替えロジック
- [ ] コンパイル済み関数の返却
- [ ] compileテスト5件以上

#### P6-T7: Phase 6統合テスト

**依存**: P6-T6
**DoD**:
- [ ] `(eval '(+ 1 2))` => 3
- [ ] `(compile nil '(lambda (x) (+ x 1)))` が関数返却
- [ ] 全生成Wasmが`wasm-tools validate`パス
- [ ] `nix flake check` パス

---

## Phase 7: CLOS基盤 (Lisp-6)

**目標**: 基本的なオブジェクトシステム
**MVP**: `(make-instance 'point :x 3 :y 4)` がインスタンス生成

### インスタンス構造体

```wat
(type $instance (struct
  (field $class (ref $standard-class))
  (field $slots (ref $slot_vector))))

(type $slot_vector (array (mut anyref)))

(type $standard-class (struct
  (field $name (ref $symbol))
  (field $superclasses (ref $list))
  (field $slots (ref $slot_definition_list))
  (field $precedence_list (ref $list))))
```

### タスク

#### P7-T1: クラスメタオブジェクト

**依存**: P6-T7
**DoD**:
- [ ] $standard-class 型定義
- [ ] クラス名・スーパークラス・スロット管理
- [ ] クラス継承リスト計算
- [ ] メタオブジェクトテスト5件以上

#### P7-T2: defclass実装

**依存**: P7-T1
**DoD**:
- [ ] `defclass` フォームのコンパイル
- [ ] スロット定義（:initarg, :accessor等）
- [ ] クラスメタオブジェクト生成
- [ ] defclassテスト5件以上

#### P7-T3: make-instance実装

**依存**: P7-T2
**DoD**:
- [ ] `make-instance` 関数の実装
- [ ] スロット初期化（:initarg, :initform）
- [ ] インスタンス構造体生成
- [ ] make-instanceテスト5件以上

#### P7-T4: スロットアクセス

**依存**: P7-T3
**DoD**:
- [ ] `slot-value` の実装
- [ ] `(setf slot-value)` の実装
- [ ] アクセサ関数生成
- [ ] スロットアクセステスト5件以上

#### P7-T5: 総称関数基盤

**依存**: P7-T4
**DoD**:
- [ ] `defgeneric` フォーム
- [ ] メソッドテーブル構造
- [ ] ディスパッチ基盤
- [ ] 総称関数テスト3件以上

#### P7-T6: defmethod実装

**依存**: P7-T5
**DoD**:
- [ ] `defmethod` フォームのコンパイル
- [ ] 特殊化子（specializer）処理
- [ ] メソッド登録
- [ ] defmethodテスト5件以上

#### P7-T7: メソッドディスパッチ

**依存**: P7-T6
**DoD**:
- [ ] クラスベースのメソッド選択
- [ ] キャッシュベースの高速パス
- [ ] 継承を考慮したディスパッチ
- [ ] ディスパッチテスト5件以上

#### P7-T8: メソッド結合

**依存**: P7-T7
**DoD**:
- [ ] `:before`, `:after` メソッド
- [ ] `:around` メソッドと `call-next-method`
- [ ] 結合テスト5件以上

#### P7-T9: Phase 7統合テスト

**依存**: P7-T8
**DoD**:
- [ ] `(make-instance 'point :x 3 :y 4)` 成功
- [ ] `(distance p)` が正しい値を返す
- [ ] 継承とメソッドオーバーライド動作
- [ ] 全生成Wasmが`wasm-tools validate`パス
- [ ] `nix flake check` パス

---

## Risk Mitigation

### 高リスク項目の早期着手

| リスク | 対策 | 着手Phase |
|--------|------|-----------|
| WasmGC仕様の不安定性 | wasmtimeを参照実装として追従、抽象層設置 | Phase 0 |
| アリティディスパッチ性能 | 0-2引数専用エントリーポイント、ベンチマーク駆動 | Phase 2 |
| 動的JITモジュールリンク | インポート/エクスポート設計の早期検証 | Phase 6 |

### 技術検証タスク

- **P0-TV1**: wasmtimeでのWasmGC機能確認
- **P2-TV1**: return_call/return_call_ref の動作検証
- **P3-TV1**: try_table/throw の動作検証
- **P6-TV1**: 動的モジュールインスタンス化のPoC

---

## Success Metrics

| Milestone | Phase | Criteria |
|-----------|-------|----------|
| M0: 環境確立 | 0 | `nix develop` + 空モジュール検証 |
| M1: 最小Lisp | 1 | `(+ 1 2)` => 3 実行 |
| M2: 関数型 | 2 | クロージャ + TCO動作 |
| M3: 制御フロー | 3 | block/return-from + unwind-protect |
| M4: 対話的開発 | 4 | REPL基本動作 |
| M5: マクロ | 5 | defmacro + when/unless |
| M6: 動的性 | 6 | eval + compile |
| M7: OOP | 7 | CLOS基本動作 |

---

## References

- `.specify/memory/constitution.md` - プロジェクト憲法 v1.0.0
- `.specify/memory/implementation-plan.md` - 詳細実装計画
- `resources/Wasm Common Lisp 動的コンパイル戦略.pdf`
- `resources/WebAssembly テキスト・バイナリ表現調査.pdf`
