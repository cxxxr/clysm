# Feature Specification: Clysm - WebAssembly GC Common Lisp Compiler

**Feature Branch**: `001-clysm-compiler`
**Created**: 2025-12-21
**Status**: Draft
**Input**: 実装計画（.specify/memory/implementation-plan.md）に基づくCommon LispからWebAssembly GCへのコンパイラ実装

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Lispコードのコンパイルと実行 (Priority: P1)

言語処理系開発者として、Common LispコードをWebAssemblyバイナリにコンパイルし、
wasmtimeで実行できるようにしたい。これにより、ブラウザやサーバーレス環境で
Lispプログラムを動作させることができる。

**Why this priority**: コンパイラの最も基本的な機能であり、他のすべての機能の前提となる

**Independent Test**: Lispソースファイルを入力として与え、有効なWasmバイナリが生成され、
wasmtimeで正しい結果が出力されることを確認できる

**Acceptance Scenarios**:

1. **Given** 算術式 `(+ 1 2)` を含むLispソース,
   **When** コンパイラでコンパイルして実行する,
   **Then** 結果として `3` が返される

2. **Given** 関数定義 `(defun add (a b) (+ a b))` と呼び出し `(add 10 20)`,
   **When** コンパイルして実行する,
   **Then** 結果として `30` が返される

3. **Given** 条件分岐 `(if (< 5 10) 'yes 'no)`,
   **When** コンパイルして実行する,
   **Then** 結果として `yes` が返される

---

### User Story 2 - 再現可能な開発環境 (Priority: P1)

開発者として、Nix Flakesを使って一貫した開発環境をセットアップしたい。
これにより、チームメンバー間で環境の差異による問題を防ぎ、CIでも同じ環境で
テストを実行できる。

**Why this priority**: 開発の土台となる環境構築は最初に必要

**Independent Test**: `nix develop`コマンドで開発シェルに入り、必要なツール
（SBCL、wasm-tools、wasmtime）がすべて利用可能であることを確認できる

**Acceptance Scenarios**:

1. **Given** 新規クローンしたリポジトリ,
   **When** `nix develop`を実行する,
   **Then** SBCL、wasm-tools、wasmtimeがパスに含まれる

2. **Given** 開発環境,
   **When** `nix flake check`を実行する,
   **Then** すべてのチェックがパスする

---

### User Story 3 - クロージャと高階関数 (Priority: P2)

Lispプログラマーとして、ラムダ式を作成し、変数をキャプチャするクロージャを
使いたい。これにより、関数型プログラミングのパラダイムを活用できる。

**Why this priority**: クロージャはLispの中核機能であり、多くのパターンの基礎となる

**Independent Test**: クロージャを生成・呼び出しするコードをコンパイルし、
キャプチャされた変数が正しく参照されることを確認できる

**Acceptance Scenarios**:

1. **Given** 自由変数をキャプチャするラムダ `(let ((x 10)) (lambda (y) (+ x y)))`,
   **When** 結果のクロージャを `5` で呼び出す,
   **Then** 結果として `15` が返される

2. **Given** 末尾再帰的な階乗関数,
   **When** `(fact 100)` を実行する,
   **Then** スタックオーバーフローなしで正しい結果が返される

---

### User Story 4 - 非局所脱出と例外処理 (Priority: P2)

Lispプログラマーとして、`block/return-from`や`catch/throw`を使った
非局所脱出を利用したい。また、`unwind-protect`でリソースの確実な解放を
保証したい。

**Why this priority**: 堅牢なエラー処理と制御フローはプロダクション品質に必須

**Independent Test**: 非局所脱出を含むコードをコンパイルし、正しい値が返され、
クリーンアップコードが実行されることを確認できる

**Acceptance Scenarios**:

1. **Given** `(block foo (return-from foo 42) 0)`,
   **When** コンパイルして実行する,
   **Then** 結果として `42` が返される

2. **Given** `(unwind-protect (error "test") (cleanup))` でcleanupが副作用を持つ,
   **When** 実行する,
   **Then** エラーが発生してもcleanupが必ず実行される

---

### User Story 5 - 対話的開発（REPL） (Priority: P3)

Lispプログラマーとして、REPLで対話的にコードを入力・評価し、結果を
即座に確認したい。これにより、探索的なプログラミングと迅速なフィードバックが得られる。

**Why this priority**: REPLはLisp開発体験の核心だが、コンパイラが先に必要

**Independent Test**: REPLを起動し、式を入力して正しい結果が表示されることを確認できる

**Acceptance Scenarios**:

1. **Given** REPLを起動した状態,
   **When** `(+ 1 2)` を入力してEnterを押す,
   **Then** `3` が表示され、次のプロンプトが現れる

2. **Given** REPLで `(defun f (x) (* x 2))` を定義した後,
   **When** `(f 21)` を入力する,
   **Then** `42` が表示される

---

### User Story 6 - マクロによる構文拡張 (Priority: P3)

Lispプログラマーとして、`defmacro`を使って新しい構文を定義したい。
バッククォートとアンクォートを使って、コードテンプレートを簡潔に記述したい。

**Why this priority**: マクロはLispの表現力の源だが、基本機能の後に実装

**Independent Test**: マクロを定義し、展開結果が正しく動作することを確認できる

**Acceptance Scenarios**:

1. **Given** `(defmacro when (test &body body) \`(if ,test (progn ,@body)))` を定義,
   **When** `(when t 1 2 3)` を評価する,
   **Then** 結果として `3` が返される

---

### User Story 7 - オブジェクト指向プログラミング (Priority: P4)

Lispプログラマーとして、CLOSを使ってクラスを定義し、総称関数と
メソッドでポリモーフィズムを実現したい。

**Why this priority**: CLOSは高度な機能であり、基本機能が安定してから実装

**Independent Test**: クラス定義、インスタンス生成、メソッド呼び出しが正しく動作することを確認できる

**Acceptance Scenarios**:

1. **Given** `(defclass point () ((x :initarg :x) (y :initarg :y)))` を定義,
   **When** `(make-instance 'point :x 3 :y 4)` を実行する,
   **Then** pointインスタンスが生成される

2. **Given** pointクラスにdistanceメソッドを定義,
   **When** インスタンスに対して `(distance p)` を呼び出す,
   **Then** 正しい距離が計算される

---

### Edge Cases

- 31ビット整数（Fixnum）の範囲を超える計算が発生した場合の動作
- 深い再帰呼び出し（1000レベル以上）でのスタック消費
- 循環参照を持つデータ構造のGC処理
- 不正なS式入力に対するリーダーのエラー処理
- マクロ展開の無限ループ検出
- 動的JITコンパイル時のモジュールリンクエラー

## Requirements *(mandatory)*

### Functional Requirements

#### 基盤（Phase 0）

- **FR-001**: システムはLEB128形式で整数をエンコード/デコードできなければならない
- **FR-002**: システムはWasmバイナリのセクション構造（ID 0-13）を正しい順序で生成できなければならない
- **FR-003**: システムは生成したWasmバイナリがwasm-tools validateをパスすることを保証しなければならない

#### コンパイル（Phase 1-2）

- **FR-004**: システムはFixnum（31ビット整数）の四則演算をコンパイルできなければならない
- **FR-005**: システムは条件分岐（if）をコンパイルできなければならない
- **FR-006**: システムはレキシカル変数（let, let*）をコンパイルできなければならない
- **FR-007**: システムはトップレベル関数定義（defun）をコンパイルできなければならない
- **FR-008**: システムはクロージャを生成し、自由変数をキャプチャできなければならない
- **FR-009**: システムは末尾呼び出しを最適化し、スタックを消費しない実行を保証しなければならない

#### 制御フロー（Phase 3）

- **FR-010**: システムはblock/return-fromによる非局所脱出をサポートしなければならない
- **FR-011**: システムはcatch/throwによる動的な例外処理をサポートしなければならない
- **FR-012**: システムはunwind-protectによるクリーンアップ保証を提供しなければならない

#### 動的機能（Phase 4-6）

- **FR-013**: システムはスペシャル変数（動的スコープ）をシャローバインディングで実装しなければならない
- **FR-014**: システムはS式リーダーでLispソースを読み込めなければならない
- **FR-015**: システムはdefmacroによるマクロ定義をサポートしなければならない
- **FR-016**: システムはevalによる実行時評価をサポートしなければならない
- **FR-017**: システムはcompileによる実行時コンパイルをサポートしなければならない

#### オブジェクトシステム（Phase 7）

- **FR-018**: システムはdefclassによるクラス定義をサポートしなければならない
- **FR-019**: システムはdefmethodによるメソッド定義をサポートしなければならない
- **FR-020**: システムは総称関数のディスパッチを実行できなければならない

### Key Entities

- **Fixnum**: 31ビット符号付き整数。WasmGCのi31refで表現
- **Cons**: car/cdrの2スロットを持つペア。リストの基本構成要素
- **Symbol**: 名前、値、関数、プロパティリスト、パッケージへの参照を持つ識別子
- **Closure**: コード参照と環境を持つ関数オブジェクト。アリティ別エントリーポイントを保持
- **NIL**: シングルトンオブジェクト。偽、空リスト、シンボルとしての多重役割
- **UNBOUND**: シンボルの未束縛状態を表すセンチネル値
- **Instance**: CLOSインスタンス。クラス参照とスロットベクタを保持

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 基本的なLispプログラム（算術、関数定義、条件分岐）が5秒以内にコンパイル・実行できる
- **SC-002**: 末尾再帰プログラムが10,000回以上の再帰でもスタックオーバーフローしない
- **SC-003**: 生成されるWasmバイナリはwasm-tools validateを100%パスする
- **SC-004**: `nix flake check`がすべての環境で一貫してパスする
- **SC-005**: REPLでの式評価が入力から結果表示まで1秒以内に完了する
- **SC-006**: 100個以上の単体テストが実装され、すべてパスする
- **SC-007**: コンパイラはFixnum範囲内の計算でネイティブ整数演算の2倍以内の性能を達成する
- **SC-008**: クロージャ呼び出しは間接呼び出しとして10ns以内で実行される

## Assumptions

- ターゲットランタイムはWasm 3.0（WasmGC、tail-call、EH）をサポートする
- 開発者はNix Package Managerがインストールされた環境を使用する
- 初期実装ではシングルスレッド実行を前提とする
- Fixnum範囲を超える整数（Bignum）は後続フェーズで対応する
- ブラウザ対応よりもwasmtimeでの動作を優先する
