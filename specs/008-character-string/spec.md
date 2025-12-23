# Feature Specification: Common Lisp文字型と文字列操作

**Feature Branch**: `008-character-string`
**Created**: 2025-12-23
**Status**: Draft
**Input**: Common Lisp文字型(character)と文字列型(string)の実装。Phase 8標準ライブラリの基盤機能。

## Clarifications

### Session 2025-12-23

- Q: `code-char`に無効なコードポイントが渡された場合の動作は？ → A: NILを返す（ANSI CL準拠）

## User Scenarios & Testing *(mandatory)*

### User Story 1 - 文字リテラルの読み込みと基本操作 (Priority: P1)

Lispプログラマとして、`#\a` や `#\Space` のような文字リテラルをソースコードに記述し、
文字の比較や変換を行いたい。これにより、テキスト処理の基本単位を扱えるようになる。

**Why this priority**: 文字型は文字列型の基盤であり、すべての文字列操作は文字単位の処理に依存する。

**Independent Test**: `#\a` をコンパイル・実行し、`char-code` で文字コードを取得できることを確認する。

**Acceptance Scenarios**:

1. **Given** リーダーが初期化されている, **When** `#\a` を読み込む, **Then** 文字 `a` を表すオブジェクトが返される
2. **Given** 文字 `#\A` がある, **When** `(char-code #\A)` を評価する, **Then** 整数 `65` が返される
3. **Given** 整数 `97` がある, **When** `(code-char 97)` を評価する, **Then** 文字 `#\a` が返される
4. **Given** 文字 `#\a` と `#\b` がある, **When** `(char< #\a #\b)` を評価する, **Then** 真が返される
5. **Given** 文字 `#\a` がある, **When** `(char-upcase #\a)` を評価する, **Then** 文字 `#\A` が返される

---

### User Story 2 - 文字列リテラルと基本アクセス (Priority: P1)

Lispプログラマとして、`"hello"` のような文字列リテラルを記述し、
文字列の長さを取得したり、特定位置の文字にアクセスしたりしたい。

**Why this priority**: 文字列はテキスト処理の中心であり、ほぼすべてのプログラムで使用される。

**Independent Test**: `"hello"` をコンパイルし、`(char "hello" 0)` で `#\h` を取得できることを確認する。

**Acceptance Scenarios**:

1. **Given** リーダーが初期化されている, **When** `"hello"` を読み込む, **Then** 文字列オブジェクトが返される
2. **Given** 文字列 `"hello"` がある, **When** `(length "hello")` を評価する, **Then** 整数 `5` が返される
3. **Given** 文字列 `"hello"` がある, **When** `(char "hello" 1)` を評価する, **Then** 文字 `#\e` が返される
4. **Given** 文字列 `"hello"` がある, **When** `(schar "hello" 4)` を評価する, **Then** 文字 `#\o` が返される

---

### User Story 3 - 文字列の比較 (Priority: P2)

Lispプログラマとして、2つの文字列を比較して等しいかどうか、
また辞書順での大小関係を判定したい。

**Why this priority**: 文字列比較は条件分岐やソートの基盤となる重要な機能。

**Independent Test**: `(string= "abc" "abc")` が真を返し、`(string= "abc" "ABC")` が偽を返すことを確認する。

**Acceptance Scenarios**:

1. **Given** 文字列 `"abc"` と `"abc"` がある, **When** `(string= "abc" "abc")` を評価する, **Then** 真が返される
2. **Given** 文字列 `"abc"` と `"ABC"` がある, **When** `(string= "abc" "ABC")` を評価する, **Then** 偽が返される
3. **Given** 文字列 `"abc"` と `"ABC"` がある, **When** `(string-equal "abc" "ABC")` を評価する, **Then** 真が返される（大文字小文字無視）
4. **Given** 文字列 `"apple"` と `"banana"` がある, **When** `(string< "apple" "banana")` を評価する, **Then** 真が返される

---

### User Story 4 - 文字列の生成と変換 (Priority: P2)

Lispプログラマとして、新しい文字列を生成したり、
既存の文字列の大文字/小文字を変換したりしたい。

**Why this priority**: 文字列の動的生成と変換は実用的なテキスト処理に必須。

**Independent Test**: `(make-string 5 :initial-element #\x)` が `"xxxxx"` を返すことを確認する。

**Acceptance Scenarios**:

1. **Given** 引数 `5` と初期文字 `#\x` がある, **When** `(make-string 5 :initial-element #\x)` を評価する, **Then** 文字列 `"xxxxx"` が返される
2. **Given** 文字列 `"Hello"` がある, **When** `(string-upcase "Hello")` を評価する, **Then** 文字列 `"HELLO"` が返される
3. **Given** 文字列 `"HELLO"` がある, **When** `(string-downcase "HELLO")` を評価する, **Then** 文字列 `"hello"` が返される
4. **Given** 文字列 `"hello world"` がある, **When** `(string-capitalize "hello world")` を評価する, **Then** 文字列 `"Hello World"` が返される

---

### User Story 5 - 部分文字列と連結 (Priority: P3)

Lispプログラマとして、文字列の一部を切り出したり、
複数の文字列を連結して新しい文字列を作成したりしたい。

**Why this priority**: 文字列操作の応用機能として、基本機能の上に構築される。

**Independent Test**: `(subseq "hello" 1 4)` が `"ell"` を返すことを確認する。

**Acceptance Scenarios**:

1. **Given** 文字列 `"hello"` がある, **When** `(subseq "hello" 1 4)` を評価する, **Then** 文字列 `"ell"` が返される
2. **Given** 文字列 `"hello"` がある, **When** `(subseq "hello" 2)` を評価する, **Then** 文字列 `"llo"` が返される
3. **Given** 文字列 `"hello"` と `"world"` がある, **When** `(concatenate 'string "hello" " " "world")` を評価する, **Then** 文字列 `"hello world"` が返される

---

### User Story 6 - 文字述語 (Priority: P3)

Lispプログラマとして、文字が英字か、数字か、空白かなどを判定したい。

**Why this priority**: パーサーやバリデーションなど、文字分類が必要な処理で使用される。

**Independent Test**: `(alpha-char-p #\a)` が真を返し、`(alpha-char-p #\1)` が偽を返すことを確認する。

**Acceptance Scenarios**:

1. **Given** 文字 `#\a` がある, **When** `(alpha-char-p #\a)` を評価する, **Then** 真が返される
2. **Given** 文字 `#\5` がある, **When** `(digit-char-p #\5)` を評価する, **Then** 整数 `5` が返される
3. **Given** 文字 `#\a` がある, **When** `(digit-char-p #\a)` を評価する, **Then** 偽（NIL）が返される
4. **Given** 文字 `#\a` がある, **When** `(alphanumericp #\a)` を評価する, **Then** 真が返される
5. **Given** 文字 `#\Space` がある, **When** `(alphanumericp #\Space)` を評価する, **Then** 偽が返される

---

### Edge Cases

- 空文字列 `""` に対する `length` は `0` を返す
- 空文字列に対する `(char "" 0)` はエラーを発生させる（インデックス範囲外）
- 範囲外インデックスへのアクセスはエラーを発生させる
- Unicode文字（非ASCII）`#\あ` のような文字の正しい処理
- エスケープシーケンス `#\Newline`, `#\Tab`, `#\Space` の正しいパース
- 文字列内のエスケープ `"\n"`, `"\t"`, `"\\"`, `"\""` の正しいパース
- 負のインデックスはエラーを発生させる
- `subseq` の終了位置が開始位置より前の場合はエラーを発生させる

## Requirements *(mandatory)*

### Functional Requirements

#### 文字型 (Character)

- **FR-001**: システムはUnicodeコードポイントを表現する文字型を提供しなければならない
- **FR-002**: リーダーは `#\x` 形式（単一文字）の文字リテラルをパースできなければならない
- **FR-003**: リーダーは `#\Space`, `#\Newline`, `#\Tab`, `#\Return` の名前付き文字をパースできなければならない
- **FR-004**: `char-code` 関数は文字からUnicodeコードポイント（整数）を返さなければならない
- **FR-005**: `code-char` 関数は有効なUnicodeコードポイント（0x0〜0x10FFFF、サロゲート範囲除く）から文字を返し、無効な値にはNILを返さなければならない
- **FR-006**: 文字比較関数 `char=`, `char/=`, `char<`, `char>`, `char<=`, `char>=` を提供しなければならない
- **FR-007**: 大文字小文字無視の比較関数 `char-equal`, `char-lessp`, `char-greaterp` 等を提供しなければならない
- **FR-008**: ケース変換関数 `char-upcase`, `char-downcase` を提供しなければならない
- **FR-009**: 文字述語 `alpha-char-p`, `digit-char-p`, `alphanumericp`, `upper-case-p`, `lower-case-p` を提供しなければならない
- **FR-010**: `characterp` 型述語を提供しなければならない

#### 文字列型 (String)

- **FR-011**: システムはUTF-8エンコードされた文字列型を提供しなければならない
- **FR-012**: リーダーは `"..."` 形式の文字列リテラルをパースできなければならない
- **FR-013**: 文字列リテラル内のエスケープシーケンス `\n`, `\t`, `\\`, `\"` をパースできなければならない
- **FR-014**: `length` 関数は文字列の文字数を返さなければならない
- **FR-015**: `char` および `schar` 関数は指定位置の文字を返さなければならない
- **FR-016**: `make-string` 関数は指定長の新しい文字列を生成しなければならない
- **FR-017**: `string` 関数はシンボルや文字から文字列への変換を提供しなければならない
- **FR-018**: 文字列比較関数 `string=`, `string/=`, `string<`, `string>`, `string<=`, `string>=` を提供しなければならない
- **FR-019**: 大文字小文字無視の比較関数 `string-equal`, `string-lessp`, `string-greaterp` 等を提供しなければならない
- **FR-020**: ケース変換関数 `string-upcase`, `string-downcase`, `string-capitalize` を提供しなければならない
- **FR-021**: `subseq` 関数は文字列に対して部分文字列を返さなければならない
- **FR-022**: `concatenate` 関数は `'string` 結果型で複数の文字列を連結できなければならない
- **FR-023**: `stringp` 型述語を提供しなければならない

### Key Entities

- **Character**: Unicodeコードポイントを表す不変のスカラー値。21ビット範囲（0x0〜0x10FFFF）
- **String**: 文字の順序付きシーケンス。UTF-8でエンコードされ、ランダムアクセス可能
- **Character Name**: `Space`, `Newline`, `Tab` などの標準的な名前付き文字

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 全ての文字比較関数（6種）が正しい結果を返すテストがパスする
- **SC-002**: 全ての文字述語（5種）が正しい結果を返すテストがパスする
- **SC-003**: 全ての文字列比較関数（6種 × 2 = 12種）が正しい結果を返すテストがパスする
- **SC-004**: 文字リテラル `#\a`, `#\Space`, `#\Newline` 等がリーダーで正しくパースされるテストがパスする
- **SC-005**: 文字列リテラル `"hello"`, `"line\nbreak"` 等がリーダーで正しくパースされるテストがパスする
- **SC-006**: Unicode文字（日本語など）を含む文字列操作が正しく動作するテストがパスする
- **SC-007**: 空文字列と境界条件のテストがパスする
- **SC-008**: 既存のシーケンス関数（`length`, `subseq`）が文字列に対しても動作するテストがパスする

## Assumptions

- 文字型はi31ref（31ビット即値）に収まるUnicodeコードポイントとして表現する
- 文字列はUTF-8バイト配列として内部表現する
- マルチバイト文字を含む文字列の `length` は文字数（バイト数ではない）を返す
- 文字列の文字アクセス（`char`, `schar`）はO(n)の計算量を許容する（UTF-8のため）
- Lisp-1名前空間に従い、関数と変数は同じ名前空間を共有する

## Dependencies

- **依存なし**: 本機能は基盤機能であり、他の機能に依存しない
- **後続機能への影響**:
  - ストリームI/O（文字単位の読み書き）
  - `format` 関数（文字列操作を多用）
  - パス名（文字列ベース）
  - プリンタ（文字列への変換）
