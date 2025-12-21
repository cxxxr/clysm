リサーチ結果とご提示いただいた「4層モデル」（Richard P. Gabriel氏の論文で言及されるLispシステムの実装哲学）に基づき、WebAssembly (Wasm) GCをターゲットとしたCommon Lisp処理系の実装プランを提案します。  
このモデルは、Wasmの静的な制約（型定義の不変性など）と、Common Lispの動的な柔軟性を両立させるのに非常に適しています。

### ---

**WebAssembly Common Lisp 実装プラン（4層アーキテクチャ）**

このプランでは、下層をWasmの静的な型システムで固め、上層に行くほどLisp自身の機能で動的な振る舞いを実現する構成をとります。

#### **第1層：カーネル言語 (Kernel Language)**

役割: Wasm GCの型システムへのマッピングと、再定義不可能なプリミティブの提供。  
この層は「Lispマシン」としてのハードウェア定義に近い部分であり、Wasmモジュールとして静的にコンパイルされます。

* **データ構造の定義 (Wasm GC Structs/Arrays):**  
  * **Universal Type:** すべてのオブジェクトは anyref (または (ref eq)) として扱います。  
  * **Immediate Values:** fixnum や character は i31ref を使用し、ヒープ割り当てを回避します 1。

  * **Cons Cell:** car, cdr (mutable anyref) を持つ struct 3。

  * **Symbol:** 値 (value)、関数 (function)、プロパティリスト (plist)、パッケージ (package)、名前 (name) を持つ struct 4。

  * **Closure:** 関数ポインタ (func\_ref) と環境 (struct または array) をペアにした struct 6。

* **メモリ管理:**  
  * WasmネイティブのGC（Allocations）に全面的に委譲します。手動のGC実装は行いません。  
* **基本操作 (Primitives):**  
  * car, cdr, rplaca, rplacd, eq, \+ (fixnum) などの「再定義不要」な命令をWasm関数として実装・エクスポートします。

#### **第2層：言語学的層 (Linguistic Layer)**

役割: カーネルの機能を組み合わせて「Lispのセマンティクス（意味論）」を形成する層。  
Wasmには存在しない「動的な制御」や「スコープ」をここで実装します。実装言語はWAT (WebAssembly Text) または、第1層を利用できる最小限のブートストラップ用Lisp（Pre-Lisp）です。

* **関数呼び出しプロトコル (Function Protocol):**  
  * Wasmの call は静的型付けが厳密なため、すべてのLisp関数を統一的なシグネチャ（例: 引数の配列と環境を受け取る）でラップするか、アリティ（引数数）ごとに異なるエントリポイントを持つ構造体を用意します 8。

  * **末尾呼び出し:** ブラウザでサポートが進んでいる return\_call 命令を使用し、スタックオーバーフローを防ぎます 9。

* **動的スコープ (Special Variables):**  
  * Wasmには動的スコープがないため、\*\*「Shallow Binding with Trail」\*\*戦略を採用します。  
  * 各シンボル構造体の value フィールドを直接書き換える際、古い値を「トレイルスタック（グローバルなWasm array）」に退避させます。スコープを抜ける際に try\_table (Wasm Exception Handling) を用いて確実に値を復元します 11。

* **非局所脱出 (Non-local Exits):**  
  * block/return-from や tagbody/go は、Wasmの tag と throw/catch 機構を用いて実装します。これによりスタックの巻き戻し（Unwinding）をWasm VMに任せることができます 14。

#### **第3層：ライブラリ (Library)**

役割: Common Lispの標準機能の提供。  
この層は、第1層・第2層で構築された機能を使って Common Lisp自身で記述 され、Wasmへとコンパイルされます。

* **データ構造の拡張:**  
  * **Sequence / Hash Table:** Wasmの array をラップする形で、Lispのベクタやハッシュテーブルを実装します。  
  * **CLOS (Common Lisp Object System):** クラスはメタデータを持つ struct、インスタンスはスロットのベクタを持つ struct として表現します。メソッドディスパッチは、ジェネリック関数内でハッシュテーブル（キャッシュ）を検索するロジックとして実装されます 15。

* **標準関数:**  
  * format, read, print, mapcar などの高レベル関数群。これらは動的に再定義可能（Redefinable）であるべきため、関数スロット経由で呼び出されます。

#### **第4層：エピ言語的機能 / 環境 (Epilinguistic / Environment)**

**役割:** 言語の外側にある「環境」としての機能。Webブラウザとの接点や、コード生成（コンパイラ）そのものです。

* **JITコンパイラ (In-Browser Compiler):**  
  * LispのS式 (list) を受け取り、Wasmのバイナリ形式 (Uint8Array) を生成するコンパイラをLisp自身で実装します 17。

  * 生成したバイナリを WebAssembly.instantiate でブラウザにロードさせ、生成された関数オブジェクトを現在の環境（第1層のシンボルなど）にリンクさせます。これが compile や eval の実体となります 19。

* **DOM / JS インターフェース:**  
  * Wasm GCの externref を利用して、LispオブジェクトとしてDOMノードを直接保持・操作します。JSのオブジェクトとLispのオブジェクトを相互に参照させても、Wasm GCがサイクルを回収できるためメモリリークしません 15。

* **REPL & Debugger:**  
  * Web Worker上でREPLを動作させ、メインスレッド（UI）をブロックせずにLispコードを対話的に実行・コンパイルする環境を提供します。

### **開発のステップ**

1. **Bootstrapping (Kernel):** 手書きのWATまたはRust/Zig等で、Consセルと最小限のメモリ操作（Kernel）のみを持つWasmモジュールを作成する。  
2. **Core Semantics (Linguistic):** その上で動く最小限のインタプリタを実装し、動的スコープと関数呼び出しをテストする。  
3. **Self-Hosting Compiler (Epilinguistic/Library):** そのインタプリタ上で動く「Lisp to Wasmコンパイラ」を書き、自分自身をコンパイルして高速化する。  
4. **Standard Library:** コンパイラが動けば、あとは既存のCommon Lispコード（SBCLのライブラリ部など）を移植・コンパイルしていく。