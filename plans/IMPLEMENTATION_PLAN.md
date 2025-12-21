# WebAssembly Common Lisp 実装プラン

## プロジェクト名: CLYSM (Common Lisp for WASM)

---

## 1. プロジェクト概要

### 1.1 目標
WebAssembly GC (Wasm GC) をターゲットとした、ブラウザネイティブなCommon Lisp処理系の実装。

### 1.2 設計思想
Richard P. Gabriel氏の「4層モデル」に基づき、Wasmの静的な制約とCommon Lispの動的な柔軟性を両立させる階層的アーキテクチャを採用する。

### 1.3 なぜWasm GCか

| 従来手法 (線形メモリ) | Wasm GC |
|---|---|
| 独自GC実装が必要 | ホストGCに委譲 |
| DOM参照で循環メモリリーク | 言語間サイクルを自動回収 |
| バイナリサイズ大 | 軽量なランタイム |
| JSとの統合が複雑 | externref で直接連携 |

---

## 2. アーキテクチャ: 4層モデル

```
┌─────────────────────────────────────────────────────────────┐
│  第4層: エピ言語的機能 / 環境 (Epilinguistic)               │
│  - JITコンパイラ (Lisp → Wasm バイナリ生成)                 │
│  - DOM/JS インターフェース                                   │
│  - REPL & デバッガ                                          │
├─────────────────────────────────────────────────────────────┤
│  第3層: ライブラリ (Library)                                │
│  - CLOS (Common Lisp Object System)                         │
│  - 標準関数 (format, read, print, mapcar...)                │
│  - シーケンス、ハッシュテーブル                              │
├─────────────────────────────────────────────────────────────┤
│  第2層: 言語学的層 (Linguistic Layer)                       │
│  - 関数呼び出しプロトコル                                    │
│  - 動的スコープ (Special Variables)                         │
│  - 非局所脱出 (block/return-from, catch/throw)              │
│  - 多値返却 (Multiple Values)                               │
├─────────────────────────────────────────────────────────────┤
│  第1層: カーネル言語 (Kernel Language)                      │
│  - Wasm GC 型定義 (struct/array/i31ref)                     │
│  - 再定義不可能なプリミティブ                                │
│  - メモリ管理 (ホストGCへの委譲)                            │
└─────────────────────────────────────────────────────────────┘
```

---

## 3. 技術的決定事項

### 3.1 データ構造マッピング

| Common Lisp型 | Wasm GC型 | 戦略 |
|---|---|---|
| Fixnum | `i31ref` | 31ビット整数、ヒープ割り当てゼロ |
| Character | `i31ref` | Unicodeコードポイント（タグビットで区別） |
| Cons | `struct` | `car`, `cdr` を持つmutable構造体 |
| Symbol | `struct` | name, value, function, plist, package |
| Closure | `struct` | func_ref + 環境(struct/array) |
| Bignum | `struct` + `array i64` | 任意精度整数 |
| Float | `struct` + `f64` | ボックス化（局所的にはアンボックス） |
| Simple-Vector | `array anyref` | 直接マッピング |
| String | `array i8` | UTF-8エンコーディング |
| Instance (CLOS) | `struct` | class参照 + slotsベクタ |

### 3.2 NILの表現

**決定**: シングルトン構造体として実装（Wasm null は使わない）

理由:
- NILはシンボルでありリスト終端でもある二重性を持つ
- `ref.eq` による高速な等価性判定が可能
- シンボルとしてのプロパティ（name, plist等）にアクセス可能

### 3.3 関数呼び出し戦略

**決定**: 多形ディスパッチ構造体（ハイブリッド方式）

```wat
(type $closure (struct
  (field $code_0 (ref null $func_0))   ;; 0引数用
  (field $code_1 (ref null $func_1))   ;; 1引数用
  (field $code_2 (ref null $func_2))   ;; 2引数用
  (field $code_3 (ref null $func_3))   ;; 3引数用
  (field $code_N (ref null $func_N))   ;; 汎用（リスト渡し）
  (field $env (mut anyref))            ;; クロージャ環境
))
```

理由:
- 頻出する少数引数の呼び出しを高速化
- 可変長引数は汎用スロットにフォールバック
- `call_ref` の型不一致を回避

### 3.4 動的スコープ（Special Variables）

**決定**: シャローバインディング + トレイル

```
バインディング時:
1. 現在の symbol.$value を「バインディングスタック」にプッシュ
2. symbol.$value を新しい値で上書き

参照時:
- 単に symbol.$value を読むだけ（O(1)）

アンバインド時:
- try_table で確実に古い値を復元
```

### 3.5 非局所脱出

**決定**: Wasm Exception Handling (`try_table`, `throw`, `catch`)

```
block/return-from → throw でタグ付き例外を送出
catch/throw      → 同様にタグ付き例外
unwind-protect   → catch_all + rethrow
```

### 3.6 末尾呼び出し

**決定**: `return_call` 命令を使用（Wasm 3.0）

理由:
- 主要ブラウザでサポート済み（Chrome, Firefox, Safari 18.2+）
- 無限再帰も安全に実行可能

### 3.7 多値返却

**決定**: プライマリ値 + グローバルバッファ

```
1. 第1値: 関数の戻り値として返却
2. 第2値以降: グローバルな「多値バッファ」に格納
3. multiple-value-bind: 呼び出し直後にバッファを確認
```

### 3.8 動的コンパイル (Eval/JIT)

**決定**: 2段階（Tiered）実行モデル

```
Tier 1: Wasm内インタプリタ
- REPL、一度きりのコード向け
- S式を再帰的に評価
- 即応性優先

Tier 2: 動的Wasmモジュール生成
- compile 関数呼び出し時
- ホットパス検出時
- WebAssembly.instantiate() 経由でロード
```

---

## 4. マイルストーン計画

### Milestone 1: カーネル・ランタイム構築 ✅ 完了

**目標**: Wasm GCを利用したLispデータ構造と基本操作の実装

**成果物**: `kernel.wasm` + JSブリッジ

#### 1.1 データ構造の型定義 (WAT)

```wat
;; Universal Type
(type $any (ref null any))

;; Cons Cell
(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))
))

;; Symbol
(type $symbol (struct
  (field $name (ref $string))      ;; 印字名（不変）
  (field $value (mut anyref))      ;; 値セル
  (field $function (mut anyref))   ;; 関数セル
  (field $plist (mut anyref))      ;; プロパティリスト
  (field $package (mut anyref))    ;; パッケージ
))

;; Fixnum: i31ref を直接使用

;; String
(type $string (array (mut i8)))
```

#### 1.2 カーネル・プリミティブ

| 関数 | 説明 | Wasm命令 |
|---|---|---|
| `cons` | Consセル生成 | `struct.new $cons` |
| `car` / `cdr` | アクセサ | `struct.get $cons $car` |
| `rplaca` / `rplacd` | 破壊的更新 | `struct.set` |
| `eq` | ポインタ等価 | `ref.eq` |
| `consp` | 型述語 | `ref.test (ref $cons)` |
| `symbolp` | 型述語 | `ref.test (ref $symbol)` |
| `fixnump` | 型述語 | `ref.test (ref i31)` |
| `+`, `-`, `=`, `<` | Fixnum演算 | `i31.get_s`, i32演算 |
| `make-symbol` | シンボル生成 | `struct.new $symbol` |
| `symbol-value` | 値取得 | `struct.get $symbol $value` |

#### 1.3 JSブリッジ

```javascript
// kernel.js
export async function loadLispKernel() {
  const wasm = await WebAssembly.instantiate(kernelBytes, imports);

  // 特殊オブジェクトの初期化
  const NIL = wasm.exports.make_nil();
  const T = wasm.exports.intern("T");
  const UNBOUND = wasm.exports.make_unbound_marker();

  return {
    exports: wasm.exports,
    NIL, T, UNBOUND,
    intern: (name) => wasm.exports.intern(name),
    printer: (obj) => formatLispObject(obj),
  };
}
```

#### 1.4 検証コード

```javascript
const kernel = await loadLispKernel();

// Consセル作成
const cell = kernel.exports.cons(
  kernel.exports.to_fixnum(1),
  kernel.exports.to_fixnum(2)
);
console.log(kernel.printer(cell)); // (1 . 2)

// Fixnum演算
const sum = kernel.exports.add(
  kernel.exports.to_fixnum(10),
  kernel.exports.to_fixnum(20)
);
console.log(kernel.printer(sum)); // 30
```

#### 1.5 実装状況 (2025-12-21)

| 項目 | 状態 |
|---|---|
| Fixnum (タグビット LSB=0) | ✅ |
| Character (タグビット LSB=1) | ✅ |
| Cons / car / cdr / rplaca / rplacd | ✅ |
| Symbol (NIL, T) / symbolp / null | ✅ |
| String (UTF-8) / schar / schar_set | ✅ |
| Vector / svref / svset | ✅ |
| eq / Fixnum算術・比較 | ✅ |
| JSブリッジ (loadKernel, LispKernel) | ✅ |
| テスト (63 tests, 100% pass) | ✅ |

**技術的な学び**:
- wabt (wat2wasm) は Wasm GC 非対応 → **wasm-tools** を使用
- `anyref` に `ref.eq` は直接使えない → `ref.cast (ref eq)` が必要
- Node.js 22 では `--experimental-wasm-gc` フラグ不要

---

### Milestone 2: 言語学的層（インタプリタ）

**目標**: 制御構造と動的機能を持つ最小限のLispインタプリタ

**成果物**: `eval.wasm`（Wasm内インタプリタ）

#### 2.1 特殊形式

| 形式 | 実装方法 |
|---|---|
| `quote` | そのまま返却 |
| `if` | 条件分岐 |
| `lambda` | クロージャ構造体生成 |
| `let` / `let*` | 環境拡張 |
| `setq` | 環境更新 |
| `progn` | 逐次評価 |
| `block` / `return-from` | Wasm EH (`try_table`, `throw`) |
| `tagbody` / `go` | 同上 |
| `catch` / `throw` | 同上 |
| `unwind-protect` | `catch_all` + `rethrow` |

#### 2.2 動的スコープの実装

```wat
;; バインディングスタック（グローバル）
(global $binding_stack (mut (ref $array)) (array.new_default ...))
(global $binding_sp (mut i32) (i32.const 0))

;; bind_special: スペシャル変数をバインド
(func $bind_special (param $sym (ref $symbol)) (param $val anyref)
  ;; 1. 古い値をスタックにプッシュ
  (array.set $binding_stack
    (global.get $binding_sp)
    (struct.get $symbol $value (local.get $sym)))
  (global.set $binding_sp
    (i32.add (global.get $binding_sp) (i32.const 1)))

  ;; 2. 新しい値をセット
  (struct.set $symbol $value (local.get $sym) (local.get $val))
)

;; unbind_special: 復元
(func $unbind_special (param $sym (ref $symbol))
  (global.set $binding_sp
    (i32.sub (global.get $binding_sp) (i32.const 1)))
  (struct.set $symbol $value
    (local.get $sym)
    (array.get $binding_stack (global.get $binding_sp)))
)
```

#### 2.3 検証コード

```lisp
;; 動作確認用S式（JSのReaderでパース → Wasmで評価）
(let ((x 10))
  (+ x 20))
;; => 30

((lambda (x y) (+ x y)) 3 4)
;; => 7

(block foo
  (+ 1 (return-from foo 42) 3))
;; => 42
```

---

### Milestone 3: JITコンパイラ

**目標**: Lisp S式からWasmバイナリを動的生成

**成果物**: `compiler.lisp` → Wasmにコンパイル済み

#### 3.1 コンパイラのブートストラップ

```
Phase 1: JSで書いた最小限コンパイラでLispコンパイラをコンパイル
Phase 2: LispコンパイラがLispコンパイラ自身をコンパイル（セルフホスティング）
```

#### 3.2 コンパイルパイプライン

```
S式 (list)
    ↓
マクロ展開
    ↓
CPS変換 / ANF変換
    ↓
クロージャ変換
    ↓
Wasmバイナリ生成 (Uint8Array)
    ↓
WebAssembly.instantiate()
    ↓
func_ref をシンボルにリンク
```

#### 3.3 最適化

- 末尾呼び出しの `return_call` 変換
- 既知関数のインライン化
- 型推論によるアンボックス化（Fixnum, Float）
- 定数畳み込み

---

### Milestone 4: 標準ライブラリ

**目標**: Common Lisp標準関数の実装

#### 4.1 優先実装

| カテゴリ | 関数 |
|---|---|
| リスト | `list`, `append`, `reverse`, `mapcar`, `member` |
| シーケンス | `length`, `elt`, `subseq`, `find`, `position` |
| 述語 | `atom`, `listp`, `numberp`, `stringp` |
| 算術 | `+`, `-`, `*`, `/`, `mod`, `abs` |
| 比較 | `=`, `/=`, `<`, `>`, `<=`, `>=` |
| I/O | `read`, `print`, `format` |
| 制御 | `funcall`, `apply`, `values` |

#### 4.2 ハッシュテーブル

```wat
(type $hash-table (struct
  (field $buckets (ref $array))  ;; バケット配列
  (field $count (mut i32))       ;; エントリ数
  (field $test (ref $closure))   ;; 等価関数
))
```

---

### Milestone 5: CLOS

**目標**: Common Lisp Object Systemの実装

#### 5.1 データ構造

```wat
;; Standard Class
(type $standard-class (struct
  (field $name (ref $symbol))
  (field $direct-superclasses (ref $cons))  ;; リスト
  (field $direct-slots (ref $cons))
  (field $class-precedence-list (ref $cons))
  (field $effective-slots (ref $array))
))

;; Instance
(type $instance (struct
  (field $class (ref $standard-class))
  (field $slots (ref $array))  ;; スロット値ベクタ
))

;; Generic Function
(type $generic-function (struct
  (field $name (ref $symbol))
  (field $lambda-list anyref)
  (field $methods (ref $cons))       ;; メソッドリスト
  (field $dispatch-cache (ref $hash-table))
))
```

#### 5.2 メソッドディスパッチ

```
1. 引数のクラスを取得
2. ディスパッチキャッシュを検索
3. ヒット → メソッドを呼び出し
4. ミス → compute-applicable-methods → キャッシュに登録
```

---

### Milestone 6: 環境統合

**目標**: ブラウザ環境との完全な統合

#### 6.1 DOM/JS インターフェース

```lisp
;; externref を利用したDOM操作
(defun get-element-by-id (id)
  (%js-call "document.getElementById" id))

(defun set-inner-html (element html)
  (%js-set element "innerHTML" html))

;; イベントハンドラ
(defun add-event-listener (element event handler)
  (%js-call-method element "addEventListener" event handler))
```

#### 6.2 REPL

```
- Web Worker上で実行（UIをブロックしない）
- 履歴管理
- 補完機能
- エラー表示とスタックトレース
```

---

## 5. 開発ロードマップ

```
Phase 1: カーネル (M1) ✅ 完了
├── 型定義 (WAT) ✅
├── プリミティブ関数 ✅
├── JSブリッジ ✅
└── 単体テスト ✅

Phase 2: インタプリタ (M2)
├── 特殊形式
├── 動的スコープ
├── 非局所脱出
└── 統合テスト

Phase 3: コンパイラ (M3)
├── S式→Wasm変換
├── セルフホスティング
├── 最適化パス
└── ベンチマーク

Phase 4: ライブラリ (M4)
├── 標準関数
├── シーケンス
├── I/O
└── 準拠テスト (ANSI CL)

Phase 5: CLOS (M5)
├── クラス/インスタンス
├── 総称関数
├── メソッド結合
└── MOP (部分的)

Phase 6: 環境 (M6)
├── DOM統合
├── REPL
├── デバッガ
└── ドキュメント
```

---

## 6. 技術スタック

| 用途 | 技術 |
|---|---|
| カーネル実装 | WAT (WebAssembly Text) |
| ブリッジ | JavaScript / TypeScript |
| ビルドツール | ~~wat2wasm (wabt)~~, **wasm-tools** |
| テスト | Node.js 22 + 組み込みテストランナー |
| CI | GitHub Actions |
| ドキュメント | Markdown |

---

## 7. 先行事例・参考資料

- **Guile Hoot**: Scheme on WebAssembly (Spritely Institute)
- **Schism**: Self-hosting Scheme to Wasm compiler
- **Gabriel, R.P.**: "Lisp: Good News, Bad News, How to Win Big" (4層モデル)
- **Wasm GC Proposal**: https://github.com/WebAssembly/gc
- **Wasm Exception Handling**: https://github.com/WebAssembly/exception-handling

---

## 8. リスクと対策

| リスク | 対策 |
|---|---|
| Wasm GCのブラウザ互換性 | 主要ブラウザ(Chrome/Firefox/Safari)で検証 |
| tail_call未サポート環境 | トランポリンへのフォールバック |
| パフォーマンス問題 | プロファイリング、ホットパスのJIT化 |
| 仕様の複雑さ | 段階的実装、サブセットからの開始 |

---

## 9. 次のアクション

1. ~~**プロジェクト初期化**: ディレクトリ構造、ビルドシステム構築~~ ✅
2. ~~**M1着手**: WAT手書きでCons/Symbol/Fixnumの型定義~~ ✅
3. ~~**テスト環境**: Node.js + wasm-tools でWasm GCモジュールの実行環境~~ ✅
4. ~~**JSブリッジ**: 最小限のReader/Printer実装~~ ✅

### Milestone 2 実装順序（詳細は `plans/MILESTONE_2_PLAN.md` 参照）

5. **Phase 2.1**: シンボルテーブルと `intern` 関数
   - 文字列ハッシュ関数（kernel.wat）
   - ハッシュテーブル実装
   - パッケージ構造体
   - `intern` / `find_symbol` 関数

6. **Phase 2.2**: Reader（S式パーサー）
   - Tokenizer（js/reader.js）
   - Parser（数値、シンボル、リスト、クォート）

7. **Phase 2.3**: Printer（オブジェクト表示）
   - 型別表示関数（js/printer.js）
   - リスト表示

8. **Phase 2.4**: クロージャと環境
   - クロージャ構造体（$closure）
   - 環境フレーム（$env_frame）
   - 環境操作関数

9. **Phase 2.5**: eval関数（基本）
   - 自己評価オブジェクト、シンボル評価
   - 特殊形式: quote, if, progn, setq
   - 特殊形式: lambda, let, let*
   - 関数呼び出し（apply）

10. **Phase 2.6**: 動的スコープ（スペシャル変数）
    - バインディングスタック
    - `bind_special` / `unbind_special`

11. **Phase 2.7**: 非局所脱出
    - Wasm EH タグ定義
    - block / return-from
    - catch / throw
    - unwind-protect

12. **Phase 2.8**: 組み込み関数
    - 基本関数（car, cdr, cons, eq, atom, etc.）
    - リスト関数（list, append, reverse, length）
    - 高階関数（funcall, apply, mapcar）

---

*このプランは調査結果に基づく初期設計であり、実装を進める中で適宜更新する。*
