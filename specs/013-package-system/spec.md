# Feature Specification: ANSI Common Lispパッケージシステム

**Feature Branch**: `013-package-system`
**Created**: 2025-12-24
**Status**: Draft
**Input**: Phase 8E: パッケージシステムを実装する。ANSI Common Lisp準拠のパッケージ管理が目標。defpackage、in-package、export、import、use-packageをサポートし、CL、CL-USER、KEYWORDの標準パッケージを提供する。シンボルインターン（find-symbol、intern）、パッケージ修飾子（cl:car、:keyword）のリーダー対応も含む。

## User Scenarios & Testing *(mandatory)*

### User Story 1 - パッケージの定義と切り替え (Priority: P1)

Lispプログラマとして、`defpackage` でパッケージを定義し、`in-package` でカレントパッケージを切り替えたい。
これにより、モジュール化されたコードを記述し、名前空間の衝突を防ぐことができる。

**Why this priority**: パッケージ定義はパッケージシステムの中核機能であり、他のすべての機能の前提条件となる。

**Independent Test**: `defpackage` でパッケージを定義し、`in-package` で切り替えた後、シンボルが正しいパッケージに属することを確認する。

**Acceptance Scenarios**:

1. **Given** システムが初期化されている, **When** `(defpackage :my-package)` を評価する, **Then** パッケージ `MY-PACKAGE` が作成される
2. **Given** パッケージ `MY-PACKAGE` が存在する, **When** `(in-package :my-package)` を評価する, **Then** `*package*` が `MY-PACKAGE` パッケージに設定される
3. **Given** `MY-PACKAGE` がカレントパッケージである, **When** シンボル `FOO` を読み込む, **Then** シンボルは `MY-PACKAGE::FOO` としてインターンされる
4. **Given** パッケージが存在しない, **When** `(defpackage :new-pkg (:use :cl))` を評価する, **Then** `CL` パッケージのエクスポートシンボルを使用可能なパッケージが作成される

---

### User Story 2 - シンボルのエクスポートとインポート (Priority: P1)

Lispプログラマとして、パッケージからシンボルをエクスポートし、他のパッケージでインポートして使いたい。
これにより、公開APIと内部実装を明確に分離できる。

**Why this priority**: エクスポート/インポートはパッケージ間でのシンボル共有の基本機構であり、モジュール性の核心。

**Independent Test**: パッケージAでシンボルをエクスポートし、パッケージBでインポートして、修飾子なしでアクセスできることを確認する。

**Acceptance Scenarios**:

1. **Given** パッケージ `PKG-A` にシンボル `PUBLIC-FN` がある, **When** `(export 'public-fn :pkg-a)` を評価する, **Then** `PUBLIC-FN` が外部シンボルになる
2. **Given** `PKG-A:PUBLIC-FN` がエクスポートされている, **When** パッケージ `PKG-B` で `(import 'pkg-a:public-fn)` を評価する, **Then** `PKG-B` から修飾子なしで `PUBLIC-FN` にアクセスできる
3. **Given** `PKG-A` がシンボルをエクスポートしている, **When** `(defpackage :pkg-c (:use :pkg-a))` を評価する, **Then** `PKG-A` の全エクスポートシンボルが `PKG-C` で使用可能になる
4. **Given** シンボルがエクスポートされていない, **When** 他パッケージからシングルコロン `pkg-a:internal` でアクセスする, **Then** エラーが発生する

---

### User Story 3 - パッケージ修飾子によるシンボル参照 (Priority: P1)

Lispプログラマとして、`cl:car` や `my-pkg::internal` のようなパッケージ修飾子を使ってシンボルを参照したい。
これにより、名前の衝突を解決し、明示的なパッケージ参照が可能になる。

**Why this priority**: パッケージ修飾子はANSI Common Lispの標準構文であり、異なるパッケージ間でのシンボル参照に必須。

**Independent Test**: `cl:car` が `CL` パッケージの `CAR` シンボルを参照し、正しく評価できることを確認する。

**Acceptance Scenarios**:

1. **Given** リーダーが初期化されている, **When** `cl:car` を読み込む, **Then** `CL` パッケージのエクスポートシンボル `CAR` が返される
2. **Given** リーダーが初期化されている, **When** `pkg::internal` を読み込む, **Then** `PKG` パッケージの内部シンボル `INTERNAL` が返される（存在しなければ作成）
3. **Given** リーダーが初期化されている, **When** `:keyword` を読み込む, **Then** `KEYWORD` パッケージのシンボル `KEYWORD` が返される
4. **Given** リーダーが初期化されている, **When** `::double-colon-start` を読み込む, **Then** エラーが発生する（無効な構文）

---

### User Story 4 - 標準パッケージの利用 (Priority: P2)

Lispプログラマとして、`CL`、`CL-USER`、`KEYWORD` の標準パッケージがシステム起動時に利用可能であることを期待する。
これにより、ANSI Common Lispのプログラムを実行できる。

**Why this priority**: 標準パッケージはANSI Common Lispの必須要件であり、既存Lispコードとの互換性に必要。

**Independent Test**: システム起動時に `(find-package :cl)` が `CL` パッケージを返すことを確認する。

**Acceptance Scenarios**:

1. **Given** システムが初期化されている, **When** `(find-package :cl)` を評価する, **Then** `COMMON-LISP` パッケージが返される
2. **Given** システムが初期化されている, **When** `(find-package :cl-user)` を評価する, **Then** `COMMON-LISP-USER` パッケージが返される
3. **Given** システムが初期化されている, **When** `(find-package :keyword)` を評価する, **Then** `KEYWORD` パッケージが返される
4. **Given** `CL-USER` がカレントパッケージである, **When** `car` を読み込む, **Then** `CL:CAR` が参照される（CLをuseしているため）

---

### User Story 5 - シンボルインターンとルックアップ (Priority: P2)

Lispプログラマとして、`intern` でシンボルを明示的にインターンし、`find-symbol` で検索したい。
これにより、シンボルの動的な生成と検索が可能になる。

**Why this priority**: intern/find-symbolはメタプログラミングやDSL構築で重要な基盤機能。

**Independent Test**: `(intern "FOO" :my-pkg)` でシンボルを作成し、`(find-symbol "FOO" :my-pkg)` で見つかることを確認する。

**Acceptance Scenarios**:

1. **Given** パッケージ `MY-PKG` が存在する, **When** `(intern "NEW-SYM" :my-pkg)` を評価する, **Then** シンボル `MY-PKG::NEW-SYM` が作成され返される
2. **Given** シンボル `MY-PKG::EXISTING` が存在する, **When** `(intern "EXISTING" :my-pkg)` を評価する, **Then** 既存のシンボルが返される（新規作成されない）
3. **Given** シンボル `MY-PKG::SYM` が存在する, **When** `(find-symbol "SYM" :my-pkg)` を評価する, **Then** シンボルと `:INTERNAL` または `:EXTERNAL` が多値で返される
4. **Given** シンボルが存在しない, **When** `(find-symbol "NONEXISTENT" :my-pkg)` を評価する, **Then** `NIL` と `NIL` が多値で返される

---

### User Story 6 - use-packageによるパッケージ継承 (Priority: P3)

Lispプログラマとして、`use-package` で他のパッケージのエクスポートシンボルを継承したい。
これにより、複数のパッケージの機能を組み合わせることができる。

**Why this priority**: use-packageは大規模なコードベースでのパッケージ構成に使用される応用機能。

**Independent Test**: `(use-package :pkg-a :pkg-b)` の後、`PKG-B` から `PKG-A` のエクスポートシンボルにアクセスできることを確認する。

**Acceptance Scenarios**:

1. **Given** パッケージ `PKG-A` がシンボル `FOO` をエクスポートしている, **When** パッケージ `PKG-B` で `(use-package :pkg-a)` を評価する, **Then** `PKG-B` から修飾子なしで `FOO` にアクセスできる
2. **Given** `PKG-A` と `PKG-B` が同名シンボル `CONFLICT` をエクスポートしている, **When** パッケージ `PKG-C` で両方を use しようとする, **Then** 名前衝突エラーが発生する
3. **Given** パッケージ `PKG-A` を use している, **When** `(unuse-package :pkg-a)` を評価する, **Then** `PKG-A` のシンボルへの継承アクセスが解除される

---

### Edge Cases

- 存在しないパッケージへの `in-package` はエラーを発生させる
- `defpackage` で既存パッケージ名を指定した場合、再定義の警告を出す
- 空文字列 `""` のパッケージ名は無効
- `KEYWORD` パッケージのシンボルは常にエクスポートされ、自己評価する
- パッケージ修飾子でパッケージ名のみ（`pkg:`の後にシンボル名なし）はエラー
- ダブルコロン `::` で始まるシンボル（`::foo`）はエラー
- `NIL` という名前のシンボルは `COMMON-LISP:NIL` であり、どのパッケージでも `NIL` は `CL:NIL` を参照する
- `T` という名前のシンボルは `COMMON-LISP:T` であり、どのパッケージでも `T` は `CL:T` を参照する
- 循環的なパッケージ use（A uses B, B uses A）は許可される（名前衝突がなければ）

## Requirements *(mandatory)*

### Functional Requirements

#### パッケージ管理

- **FR-001**: システムは `defpackage` マクロでパッケージを定義できなければならない
- **FR-002**: `defpackage` は `:use`, `:export`, `:import-from`, `:shadow`, `:nicknames` オプションをサポートしなければならない
- **FR-003**: `in-package` マクロでカレントパッケージ（`*package*`）を切り替えられなければならない
- **FR-004**: `make-package` 関数でプログラム的にパッケージを作成できなければならない
- **FR-005**: `find-package` 関数でパッケージ名またはニックネームからパッケージを検索できなければならない
- **FR-006**: `list-all-packages` 関数で全パッケージのリストを取得できなければならない
- **FR-007**: `delete-package` 関数でパッケージを削除できなければならない
- **FR-008**: `rename-package` 関数でパッケージ名を変更できなければならない

#### シンボル操作

- **FR-009**: `export` 関数でシンボルを外部シンボルにできなければならない
- **FR-010**: `unexport` 関数でシンボルを内部シンボルに戻せなければならない
- **FR-011**: `import` 関数で他パッケージのシンボルをインポートできなければならない
- **FR-012**: `shadowing-import` 関数で名前衝突を上書きしてインポートできなければならない
- **FR-013**: `shadow` 関数でシャドウイングシンボルを作成できなければならない
- **FR-014**: `use-package` 関数で他パッケージのエクスポートシンボルを継承できなければならない
- **FR-015**: `unuse-package` 関数でパッケージの継承を解除できなければならない
- **FR-016**: `intern` 関数で文字列からシンボルをインターン（作成または検索）できなければならない
- **FR-017**: `find-symbol` 関数で文字列からシンボルを検索できなければならない（作成しない）
- **FR-018**: `unintern` 関数でシンボルをパッケージから除去できなければならない

#### 標準パッケージ

- **FR-019**: システム起動時に `COMMON-LISP` パッケージ（ニックネーム `CL`）が存在しなければならない
- **FR-020**: システム起動時に `COMMON-LISP-USER` パッケージ（ニックネーム `CL-USER`）が存在し、`CL` を use していなければならない
- **FR-021**: システム起動時に `KEYWORD` パッケージが存在しなければならない
- **FR-022**: 起動時の `*package*` は `CL-USER` でなければならない

#### リーダー拡張

- **FR-023**: リーダーはシングルコロン形式 `pkg:symbol` をパースし、パッケージの外部シンボルを参照できなければならない
- **FR-024**: リーダーはダブルコロン形式 `pkg::symbol` をパースし、パッケージの内部シンボルを参照できなければならない
- **FR-025**: リーダーはコロン開始形式 `:keyword` をパースし、`KEYWORD` パッケージのシンボルを参照できなければならない
- **FR-026**: 外部シンボル参照でシンボルがエクスポートされていない場合、リーダーはエラーを報告しなければならない

#### パッケージ情報取得

- **FR-027**: `package-name` 関数でパッケージ名を取得できなければならない
- **FR-028**: `package-nicknames` 関数でパッケージのニックネームリストを取得できなければならない
- **FR-029**: `package-use-list` 関数で use しているパッケージリストを取得できなければならない
- **FR-030**: `package-used-by-list` 関数で use されているパッケージリストを取得できなければならない
- **FR-031**: `packagep` 型述語を提供しなければならない
- **FR-032**: `symbol-package` 関数でシンボルのホームパッケージを取得できなければならない

### Key Entities

- **Package**: シンボルの名前空間。名前、ニックネーム、use-list、外部シンボルテーブル、内部シンボルテーブルを持つ
- **Symbol**: 名前（文字列）とホームパッケージへの参照を持つオブジェクト。外部/内部の可視性を持つ
- **Package Qualifier**: `pkg:sym` または `pkg::sym` 形式のシンボル参照構文
- **Keyword**: `KEYWORD` パッケージに属する自己評価シンボル。`:name` 形式で表記

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `defpackage` と `in-package` でパッケージを定義・切り替えできるテストがパスする
- **SC-002**: `export` / `import` / `use-package` でシンボルを共有できるテストがパスする
- **SC-003**: パッケージ修飾子 `cl:car`, `pkg::internal`, `:keyword` がリーダーで正しくパースされるテストがパスする
- **SC-004**: 標準パッケージ `CL`, `CL-USER`, `KEYWORD` がシステム起動時に存在するテストがパスする
- **SC-005**: `intern` / `find-symbol` でシンボルの作成と検索ができるテストがパスする
- **SC-006**: エクスポートされていないシンボルへのシングルコロンアクセスがエラーになるテストがパスする
- **SC-007**: 名前衝突検出と `shadow` / `shadowing-import` による解決がテストでパスする
- **SC-008**: キーワードシンボルが自己評価し、常にエクスポートされているテストがパスする

## Assumptions

- パッケージ名とシンボル名は大文字小文字を保持するが、比較時は大文字に正規化する（ANSI CL標準）
- シンボルテーブルはハッシュテーブルで実装し、文字列キーで高速検索を提供する
- `CL` パッケージには現在実装済みの関数・マクロのシンボルのみをエクスポートする
- 未実装のCL関数への参照は未束縛シンボルとなる（エラーではない）
- Clysm3はLisp-1なので、関数と変数は同一の名前空間を共有する

## Dependencies

- **Reader/Tokenizer** (`reader/tokenizer.lisp`): パッケージ修飾子のトークン化対応が必要
- **Reader/Parser** (`reader/parser.lisp`): パッケージ修飾付きシンボルのパース対応が必要
- **後続機能への影響**:
  - `load` / `compile-file`: ファイル内の `in-package` によるパッケージ切り替え
  - マクロ展開: シンボルのパッケージ情報保持
  - プリンタ: パッケージ修飾子付きシンボル出力
