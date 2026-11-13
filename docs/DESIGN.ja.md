# Clysm 設計書 — 仕様書との対応

本書は `Wasm向けLispコンパイラ開発仕様.pdf` の各節が、Clysm のどこでどのように実装されているかをまとめたものです。

## 全体構成

```
Lisp ソース ──(src/reader.lisp)──▶ フォーム
            ──(src/frontend.lisp)──▶ AST（マクロ展開・変数解決・自由変数解析）
            ──(src/codegen.lisp, src/prims.lisp)──▶ Wasm 命令列（記号ラベル）
            ──(src/driver.lisp)──▶ モジュール組み立て
            ──(src/wasm.lisp)──▶ WasmGC バイナリ
```

* コンパイラ本体は可搬な Common Lisp で書かれ、SBCL 上で動くクロスコンパイラとして起動します。さらにコンパイラ自身を Clysm でコンパイルして Wasm 上で動かすこともでき、第 2 世代と第 3 世代の出力はバイト単位で一致します（セルフホスト、`tools/build-selfhost.sh`）。
* ランタイムライブラリ（`runtime/*.lisp`）は Lisp で書かれ、ユーザプログラムと一緒に 1 つの Wasm モジュールにコンパイルされます（全体コンパイル）。
* Wasm バイナリは外部ツールに頼らず自前のエンコーダで生成します。

## WebAssembly GC の型階層（仕様: 基礎アーキテクチャと型階層）

* 汎用値（ユニタイプ）は `(ref null eq)`。
* 型判定は `ref.test` / `br_on_cast` / `ref.cast` で行い、ポインタのタグビット演算は使いません（`src/prims.lisp` の `defprim consp` など）。
* すべての型を 1 つの再帰型グループ（rec group）に置くため、構造が同一の型（文字列と bignum の桁配列など）も別の型として区別されます（`src/wasm.lisp` の `encode-module`）。

## データ構造のマッピング（仕様: Common Lispデータ構造のWasmGCマッピング設計）

| Lisp | WasmGC 表現 | 実装 |
|------|------------|------|
| fixnum | `i31ref`（値域 [-2^29, 2^29)） | `src/layout.lisp` |
| character | `i31ref`、上位ビットのタグ `01`（値 = 2^29 + コードポイント） | 同上 |
| NIL / T | シンボル構造体の単一インスタンス（不変グローバル） | `src/driver.lisp` `symbol-global-init` |
| cons | `struct { car: mut eqref, cdr: mut eqref }` | `src/layout.lisp` |
| symbol | `struct { name, package, value(mut), function(mut), plist(mut), hash, flags }` | 同上 |
| string | `(array (mut i32))`（コードポイント列） | 同上 |
| simple-vector | `(array (mut eqref))` | 同上 |
| 調整可能・多次元配列 | ヘッダ構造体（フィルポインタ・次元・変位）＋単純配列 | `runtime/seq.lisp` |
| bignum | `struct { sign: i32, digits: (array i32) }`（基数 2^28） | `runtime/numbers.lisp` |
| ratio / complex / single-float / double-float | それぞれ専用 struct | 同上 |
| package | `$package` struct（シンボル表はハッシュテーブル） | `runtime/package.lisp` |
| closure | `struct { entry0..entry4, entryN, env, name }` | `src/codegen.lisp` |
| 構造体・コンディション・CLOS インスタンス | `struct { layout(クラス), slots(array) }` | `runtime/class0.lisp` |
| JS オブジェクト | `struct { externref }` のラッパー | `runtime/js.lisp` |

* キーワードシンボルは起動時に `value` スロットへ自身が格納されます（`%init-packages`）。
* シンボルとパッケージは、コンパイラが生成したシンボル表から起動時に一括で構築・インターンされます。シンボル名のハッシュ値はコンパイル時に計算して構造体に埋め込みます。

## 呼出規約（仕様: 実行時スタックと呼出規約）

仕様の推奨どおり **ハイブリッド渡し** を採用しています。

* 引数が 4 個以下の呼び出しは、クロージャの `entry_k` を `call_ref` し、引数を Wasm のネイティブパラメータとして渡します。必須引数のみの関数では、対応するエントリが関数本体そのものです。
* 5 個以上の引数、`apply`、`multiple-value-call` は、引数配列（引数スライス）を受け取る `entryN` を使います。
* `&optional` / `&key` を持つ関数は **固定インデックス配置** の引数配列を受け取る本体としてコンパイルされます。呼び出し側でキーワードが静的に判明していれば、その配置で直接配列を作って呼びます（仕様の「配列の固定インデックスに再配置」最適化）。動的な呼び出しの場合は汎用エントリがキーを探索して同じ配置に変換します（`compile-keyed-lam`）。
* 同一プログラム内で定義された関数への呼び出しは直接 `call` になります（CLHS 3.2.2.3）。

## 多値（仕様: 多値の処理と限界）

* 第一値は Wasm の戻り値、全値はグローバルな多値バッファ（`mv-buf`）と個数（`mv-count`）に格納します。
* 多値を必要とする文脈で、呼び出し以外の式は個数を 1 に設定します。末尾位置の呼び出しは多値をそのまま透過させます。

## 動的スコープ（仕様: シャローバインディング）

* シンボルの `value` スロットが現在値を保持し、束縛時に旧値を WasmGC 配列によるバインディングスタックへ積みます（`:bind` / `:unbind-to` ヘルパー）。
* 通常終了・`catch`・`unwind-protect`・非局所 `return-from`/`go` のすべての出口で、保存した深さまでバインディングスタックを巻き戻します。

## 制御構造と例外処理（仕様: 大域脱出と例外処理）

* `block`/`return-from` は Wasm の `block` と `br`、`tagbody`/`go` は `loop` + `br_table` によるディスパッチに変換します（関数内で静的に解決）。
* クロージャ境界や `unwind-protect` をまたぐ脱出では、実行時に一意のタグオブジェクトを作り、単一の例外タグ `$lisp-throw(tag, value)` を `throw` します。受け側は `try_table` の `catch_ref` で捕捉し、タグを比較して一致しなければ `throw_ref` で再送出します。
* `catch`/`throw` も同じ仕組みで、存在しないタグへの `throw` は `control-error` になるよう有効な catch タグの動的リストを保持します。
* `unwind-protect` は `catch_all_ref` で捕捉してクリーンアップ（多値を保存）を実行し、`throw_ref` で脱出を再開します。

## 末尾呼び出し（仕様: 末尾呼び出し最適化と継続）

* 末尾位置の呼び出しは `return_call` / `return_call_ref` になります。動的束縛中や `try_table` の内側など、末尾呼び出しが意味を変える場所では通常の呼び出しにします。
* 第一級継続は CL が要求しないため実装していません（仕様の結論どおり）。

## メモリ管理（仕様: メモリ管理とガベージコレクションの統合）

* すべてのオブジェクトは `struct.new` / `array.new` で確保し、回収はホスト VM の GC に任せます。独自の GC は持ちません。
* JavaScript のオブジェクトは `externref` なので Lisp データに直接格納できず、`$foreign` ラッパー構造体に包みます。`JS` パッケージが値を相互変換し、Lisp 関数を JS のコールバックとして渡すこともできます（エクスポート `lisp_apply` 経由）。

## 実行中イメージ内でのコンパイル

REPL イメージにはコンパイラが埋め込まれており（`tools/build-repl.lisp`）、`compile`・`load`・REPL の各フォームは **新しい Wasm モジュールにコンパイルされ、稼働中のイメージとリンクして** 実行されます（`src/live.lisp`）。

* 基本型は全モジュールで同一の rec group として出力されるため、WasmGC の struct/array/関数型がモジュール間で共有されます（追加の型は単独 group）。
* 多値バッファ・バインディングスタック・例外タグはイメージがエクスポートし、新モジュールがインポートします。
* シンボルとリテラルは、インスタンス化時に「稼働中のオブジェクトそのもの」のベクタとして渡されます。
* このライブモードでは、コンパイラはマクロ・スペシャル宣言・定数をイメージ自身から取得します。

`(fib 27)` はインタプリタ比で約 175 倍高速です。ブラウザ版にもコンパイラが入っており、ページで入力したコードはブラウザ内で Wasm にコンパイルされます。

## ランタイムとツール

* マクロ展開関数はコンパイル時にホスト側で実行されると同時に、イメージにもコンパイルされます。そのため `eval`・`macroexpand`・REPL からもすべてのマクロが使えます。
* `eval`/`load` はイメージ内のインタプリタで実行されます（`runtime/eval.lisp`）。REPL は Node.js 上でもブラウザ上でも動きます。
* テストは同じプログラムを SBCL と Clysm で実行して出力を比較します（`tools/run-tests.sh`）。事前コンパイル・インタプリタ実行（`INTERP=1`）・実行時ネイティブコンパイル（`NATIVE=1`）・セルフホストコンパイラによるコンパイル（`SELFHOST=1`）の 4 モードがあります。
