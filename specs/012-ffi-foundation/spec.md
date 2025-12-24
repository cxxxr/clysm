# Feature Specification: FFI Foundation (Foreign Function Interface)

**Feature Branch**: `012-ffi-foundation`
**Created**: 2025-12-24
**Status**: Draft
**Input**: User description: "Phase 8C: FFI基盤（Foreign Function Interface）を実装する。目標はホスト環境（JavaScript/wasmtime）との相互運用基盤の確立。主な機能: (1) Wasm Import/Export宣言の自動生成、(2) Lisp型とWasm値の双方向マーシャリング（fixnum, float, string, boolean, anyref）、(3) ffi:define-foreign-function マクロによるホスト関数宣言、(4) ffi:call-host による外部関数呼び出し、(5) Lisp関数のWasmエクスポートとホストからのコールバック。これにより線形メモリを使わずにホストI/O機能を呼び出せるようになり、憲法のWasmGC-First原則を維持しながらI/O機能を実現可能。wasmtimeおよびJavaScript環境で動作確認すること。"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - ホスト関数の呼び出し (Priority: P1)

言語処理系開発者として、Lispコードからホスト環境（JavaScript/wasmtime）が提供する
関数を呼び出したい。これにより、コンソール出力、時刻取得、乱数生成など
Wasm単体では実現できないI/O機能をLispプログラムから利用できる。

**Why this priority**: FFIの最も基本的な機能であり、I/O実現の前提となる

**Independent Test**: ホスト関数を宣言し、Lispから呼び出して正しい結果が返されることを確認できる

**Acceptance Scenarios**:

1. **Given** ホスト側で`host.log`関数（文字列を引数に取る）が定義されている,
   **When** Lispから`(ffi:call-host "host.log" "Hello from Lisp")`を実行する,
   **Then** ホスト側のログ関数が呼び出され、"Hello from Lisp"が出力される

2. **Given** ホスト側で`host.random`関数（0-1の浮動小数点を返す）が定義されている,
   **When** Lispから`(ffi:call-host "host.random")`を実行する,
   **Then** 0.0以上1.0未満の浮動小数点数がLisp値として返される

3. **Given** `(ffi:define-foreign-function host-add "host.add" (:fixnum :fixnum) :fixnum)`で宣言,
   **When** Lispから`(host-add 10 20)`を実行する,
   **Then** ホスト側の加算関数が呼び出され、`30`が返される

---

### User Story 2 - Lisp関数のエクスポート (Priority: P1)

言語処理系開発者として、Lispで定義した関数をホスト環境から呼び出せるように
エクスポートしたい。これにより、ホストアプリケーションがLispで書かれた
ビジネスロジックをコールバックとして利用できる。

**Why this priority**: 双方向の相互運用にはエクスポートも必須であり、ホストからの
コールバック（イベントハンドラなど）の実現に必要

**Independent Test**: Lisp関数をエクスポートし、ホストから呼び出して正しい結果が
返されることを確認できる

**Acceptance Scenarios**:

1. **Given** `(ffi:export-function 'calculate-tax ...)`でLisp関数をエクスポート,
   **When** ホスト側（JavaScript）から`wasmExports.calculateTax(1000)`を呼び出す,
   **Then** Lisp関数が実行され、計算結果がホストに返される

2. **Given** 複数のLisp関数がエクスポートされている,
   **When** ホスト側でWasmインスタンスのエクスポートを列挙する,
   **Then** すべてのエクスポート関数が正しい名前で利用可能である

---

### User Story 3 - 型マーシャリング (Priority: P2)

言語処理系開発者として、Lisp型とWasm/ホスト型の間で自動的な型変換が
行われてほしい。これにより、型変換のボイラープレートコードを書く必要がなく、
シームレスなデータ交換が可能になる。

**Why this priority**: 型安全性とユーザビリティの両立に必要だが、基本的な
関数呼び出し機能の後に実装可能

**Independent Test**: 各型のマーシャリングが正しく行われることを個別にテストできる

**Acceptance Scenarios**:

1. **Given** Lispのfixnum値`42`,
   **When** ホスト関数に引数として渡す,
   **Then** Wasm i32値`42`に正しく変換される

2. **Given** ホストからの浮動小数点値`3.14`,
   **When** Lispに戻り値として受け取る,
   **Then** Lisp floatオブジェクトとして正しく構築される

3. **Given** Lispの文字列`"こんにちは"`,
   **When** ホスト関数に渡す,
   **Then** UTF-8でエンコードされた文字列としてホストに渡される
   （線形メモリではなく、WasmGCのarray型を使用）

4. **Given** Lisp値`t`または`nil`,
   **When** ホスト関数にboolean引数として渡す,
   **Then** Wasm i32の`1`または`0`に変換される

5. **Given** 変換できない型（例：複雑なCLOSオブジェクト）,
   **When** ホスト関数に渡そうとする,
   **Then** 適切なエラーが報告される

---

### User Story 4 - Import/Export宣言の自動生成 (Priority: P2)

言語処理系開発者として、FFI宣言からWasmのimport/exportセクションが
自動的に生成されてほしい。これにより、手動でWATを書く必要がなく、
コンパイル結果が正しいインターフェースを持つ。

**Why this priority**: 開発者体験の向上に重要だが、手動生成でも動作は可能

**Independent Test**: FFI宣言を含むコードをコンパイルし、生成されたWasmバイナリの
import/exportセクションを検証できる

**Acceptance Scenarios**:

1. **Given** `(ffi:define-foreign-function ...)`宣言を含むコード,
   **When** コンパイルしてWasmバイナリを生成する,
   **Then** 対応するimportエントリがimportセクションに含まれる

2. **Given** `(ffi:export-function ...)`宣言を含むコード,
   **When** コンパイルしてWasmバイナリを生成する,
   **Then** 対応するexportエントリがexportセクションに含まれる

3. **Given** 複数のFFI宣言,
   **When** コンパイルする,
   **Then** `wasm-tools validate`がパスする正しいバイナリが生成される

---

### User Story 5 - 複数ホスト環境での動作 (Priority: P3)

言語処理系開発者として、生成されたWasmモジュールがwasmtimeとブラウザ
（JavaScript）の両方で動作することを確認したい。これにより、サーバーサイドと
クライアントサイドの両方でLispコードを再利用できる。

**Why this priority**: ポータビリティは重要だが、単一環境での動作確認が先

**Independent Test**: 同じWasmバイナリをwasmtimeとJavaScript環境の両方で
実行し、FFI呼び出しが正しく動作することを確認できる

**Acceptance Scenarios**:

1. **Given** FFI呼び出しを含むコンパイル済みWasmモジュール,
   **When** wasmtimeで実行する,
   **Then** ホスト関数が正しく呼び出され、結果が得られる

2. **Given** 同じWasmモジュール,
   **When** ブラウザのJavaScriptで実行する,
   **Then** 同等の結果が得られる

---

### Edge Cases

- ホスト関数が存在しない場合のエラーハンドリング
- ホスト関数が例外をスローした場合の伝播
- 引数の数や型が一致しない場合の検出
- 再帰的なコールバック（Lisp→ホスト→Lisp）の処理
- マーシャリング不可能な型（循環参照を持つ構造体など）の検出
- NULL/undefined値の取り扱い

## Requirements *(mandatory)*

### Functional Requirements

#### FFI宣言マクロ

- **FR-001**: システムは`ffi:define-foreign-function`マクロでホスト関数を宣言できなければならない
  - 構文: `(ffi:define-foreign-function lisp-name "host.module.name" (arg-types...) return-type)`
- **FR-002**: システムは`ffi:export-function`マクロでLisp関数をエクスポート可能にしなければならない
  - 構文: `(ffi:export-function 'function-name :as "exportName" :signature (types...))`
- **FR-003**: システムは`ffi:call-host`関数で動的にホスト関数を呼び出せなければならない
  - 構文: `(ffi:call-host "host.function" arg1 arg2 ...)`

#### 型マーシャリング

- **FR-004**: システムはfixnum（i31ref）とWasm i32の双方向変換をサポートしなければならない
- **FR-005**: システムはLisp floatとWasm f64の双方向変換をサポートしなければならない
- **FR-006**: システムはLisp文字列とWasmGC array（UTF-8バイト列）の双方向変換をサポートしなければならない
  - 線形メモリは使用しない（WasmGC-First原則に準拠）
- **FR-007**: システムはLisp boolean（t/nil）とWasm i32（1/0）の双方向変換をサポートしなければならない
- **FR-008**: システムはanyref型を通じて未変換のLispオブジェクトをホストに渡せなければならない
- **FR-009**: システムは変換不可能な型に対して適切なエラーを報告しなければならない

#### Wasm Import/Export生成

- **FR-010**: システムはFFI宣言からWasm importセクションのエントリを自動生成しなければならない
- **FR-011**: システムはFFI宣言からWasm exportセクションのエントリを自動生成しなければならない
- **FR-012**: システムは生成されたimport/exportが`wasm-tools validate`をパスすることを保証しなければならない

#### ホスト環境対応

- **FR-013**: システムはwasmtime環境でFFI機能が正しく動作しなければならない
- **FR-014**: システムはJavaScript環境（ブラウザ/Node.js）でFFI機能が正しく動作しなければならない
- **FR-015**: システムはホスト関数呼び出し時のエラーをLispコンディションとして報告しなければならない

### Key Entities

- **ForeignFunctionDecl**: ホスト関数宣言。モジュール名、関数名、引数型リスト、戻り型を保持
- **ExportDecl**: エクスポート宣言。Lisp関数シンボル、エクスポート名、シグネチャを保持
- **MarshalType**: マーシャリング対象の型識別子（:fixnum, :float, :string, :boolean, :anyref）
- **ImportEntry**: Wasmインポートセクションのエントリ（モジュール名、フィールド名、型）
- **ExportEntry**: Wasmエクスポートセクションのエントリ（名前、エクスポート種別、インデックス）

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 基本的なFFI呼び出し（ホスト関数→Lisp）が100μs以内で完了する
- **SC-002**: エクスポート関数へのホストからのコールバックが100μs以内で完了する
- **SC-003**: 5種類の基本型（fixnum, float, string, boolean, anyref）のマーシャリングが正しく動作する
- **SC-004**: 生成されるWasmバイナリは`wasm-tools validate`を100%パスする
- **SC-005**: wasmtimeとJavaScript（Node.js）の両環境で同一テストスイートがパスする
- **SC-006**: FFI関連の単体テストが30件以上実装され、すべてパスする
- **SC-007**: 線形メモリを使用せずにすべてのFFI機能が動作する（WasmGC-First原則準拠）

## Assumptions

- ホスト環境はWasmGC（GC proposal）をサポートする
- wasmtime 27.0以上、またはWasmGC対応ブラウザ（Chrome 119+、Firefox 120+）を使用
- ホスト側で提供する関数は事前に定義されており、インターフェースは既知
- 初期実装では同期的な関数呼び出しのみをサポート（async/awaitは後続フェーズ）
- Component Modelへの完全対応は将来の拡張として、本フェーズではCore Wasm importに集中
