# Feature Specification: ANSI CL Type Predicates and Numeric Predicates

**Feature Branch**: `023-type-predicates`
**Created**: 2025-12-26
**Status**: Draft
**Input**: User description: "ANSI CL型述語と数値述語の実装 - ANSIテストスイートのパス率向上のための型述語・数値述語実装"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Type Checking Predicates (Priority: P1)

Lispプログラマとして、値の型を確認できる述語関数を使いたい。これにより、型に応じた条件分岐や入力検証が可能になる。

**Why this priority**: 型述語はCommon Lispの最も基本的な機能であり、他の多くの関数やマクロがこれらに依存している。ANSIテストスイートでも広く使用されている。

**Independent Test**: 各型述語を単独でテスト可能。例えば `(integerp 42)` が `T` を返し、`(integerp 3.14)` が `NIL` を返すことを確認できる。

**Acceptance Scenarios**:

1. **Given** 整数値42, **When** `(integerp 42)` を評価, **Then** Tを返す
2. **Given** 浮動小数点数3.14, **When** `(integerp 3.14)` を評価, **Then** NILを返す
3. **Given** 有理数2/3, **When** `(numberp 2/3)` を評価, **Then** Tを返す
4. **Given** 倍精度浮動小数点数1.5d0, **When** `(floatp 1.5d0)` を評価, **Then** Tを返す
5. **Given** 有理数3/4, **When** `(rationalp 3/4)` を評価, **Then** Tを返す
6. **Given** 複素数#C(1 2), **When** `(complexp #C(1 2))` を評価, **Then** Tを返す
7. **Given** シンボルfoo, **When** `(symbolp 'foo)` を評価, **Then** Tを返す
8. **Given** 関数#'car, **When** `(functionp #'car)` を評価, **Then** Tを返す
9. **Given** 文字#\a, **When** `(characterp #\a)` を評価, **Then** Tを返す

---

### User Story 2 - Numeric Predicates (Priority: P1)

Lispプログラマとして、数値の性質（ゼロか、正か、負か、奇数か、偶数か）を確認できる述語関数を使いたい。これにより、数値に基づく条件分岐が簡潔に書ける。

**Why this priority**: 数値述語はループ制御、再帰の終了条件、数学的アルゴリズムで頻繁に使用される。ANSIテストで426回以上参照されている。

**Independent Test**: 各数値述語を単独でテスト可能。例えば `(zerop 0)` が `T` を返し、`(zerop 1)` が `NIL` を返すことを確認できる。

**Acceptance Scenarios**:

1. **Given** 整数0, **When** `(zerop 0)` を評価, **Then** Tを返す
2. **Given** 浮動小数点数0.0, **When** `(zerop 0.0)` を評価, **Then** Tを返す
3. **Given** 整数1, **When** `(zerop 1)` を評価, **Then** NILを返す
4. **Given** 正の整数5, **When** `(plusp 5)` を評価, **Then** Tを返す
5. **Given** 負の整数-3, **When** `(plusp -3)` を評価, **Then** NILを返す
6. **Given** 負の整数-5, **When** `(minusp -5)` を評価, **Then** Tを返す
7. **Given** 正の整数3, **When** `(minusp 3)` を評価, **Then** NILを返す
8. **Given** 奇数7, **When** `(oddp 7)` を評価, **Then** Tを返す
9. **Given** 偶数8, **When** `(evenp 8)` を評価, **Then** Tを返す

---

### User Story 3 - Signum Function (Priority: P2)

Lispプログラマとして、数値の符号を取得する関数を使いたい。これにより、数値を-1、0、1のいずれかに正規化できる。

**Why this priority**: signumはANSIテストで3478回参照されており、多くの数値演算テストで使用されている。ただし、型述語と数値述語より若干優先度が低い。

**Independent Test**: signumを単独でテスト可能。例えば `(signum -42)` が `-1` を返すことを確認できる。

**Acceptance Scenarios**:

1. **Given** 負の整数-42, **When** `(signum -42)` を評価, **Then** -1を返す
2. **Given** 整数0, **When** `(signum 0)` を評価, **Then** 0を返す
3. **Given** 正の整数100, **When** `(signum 100)` を評価, **Then** 1を返す
4. **Given** 負の浮動小数点数-3.14, **When** `(signum -3.14)` を評価, **Then** -1.0を返す
5. **Given** 正の浮動小数点数2.5, **When** `(signum 2.5)` を評価, **Then** 1.0を返す

---

### User Story 4 - ANSI Test Compatibility (Priority: P1)

ANSIテストスイートの実行者として、numbersカテゴリとconsカテゴリのテストがより多くパスすることを期待する。

**Why this priority**: プロジェクトの主目的はANSI Common Lisp準拠であり、テストパス率の向上は進捗の客観的指標となる。

**Independent Test**: ANSIテストスイートを実行し、パス率を測定可能。

**Acceptance Scenarios**:

1. **Given** 現在のnumbersカテゴリパス率1.3%, **When** 型述語・数値述語を実装, **Then** パス率が10%以上になる
2. **Given** 現在のconsカテゴリパス率1.0%, **When** 型述語・数値述語を実装, **Then** パス率が5%以上になる
3. **Given** 実装された述語関数, **When** wasmtimeで実行, **Then** 正しい結果（T/NIL/Fixnum）を返す

---

### Edge Cases

- 整数0に対するzeropはTを返すが、plusp/minuspはNILを返す
- 浮動小数点数0.0に対するzeropはTを返す（IEEE 754準拠）
- 負のゼロ(-0.0)に対するzeropはTを返す
- oddp/evenpは整数のみに適用可能（非整数はエラー）
- signumは入力の型を保持（整数入力→整数出力、浮動小数点入力→浮動小数点出力）
- NILはシンボルでもあり、リストでもある（symbolpはT、listpもT）

## Requirements *(mandatory)*

### Functional Requirements

#### 型述語

- **FR-001**: System MUST provide `integerp` that returns T for fixnum and bignum, NIL otherwise
- **FR-002**: System MUST provide `numberp` that returns T for any numeric type (integer, ratio, float, complex), NIL otherwise
- **FR-003**: System MUST provide `floatp` that returns T for floating-point numbers, NIL otherwise
- **FR-004**: System MUST provide `rationalp` that returns T for integers and ratios, NIL otherwise
- **FR-005**: System MUST provide `complexp` that returns T for complex numbers, NIL otherwise
- **FR-006**: System MUST provide `symbolp` that returns T for symbols, NIL otherwise
- **FR-007**: System MUST provide `functionp` that returns T for functions (closures), NIL otherwise
- **FR-008**: System MUST provide `characterp` that returns T for characters, NIL otherwise

#### 数値述語

- **FR-009**: System MUST provide `zerop` that returns T if number equals zero, NIL otherwise (supports integers and floats)
- **FR-010**: System MUST provide `plusp` that returns T if number is positive, NIL otherwise
- **FR-011**: System MUST provide `minusp` that returns T if number is negative, NIL otherwise
- **FR-012**: System MUST provide `oddp` that returns T if integer is odd, NIL otherwise (integer-only)
- **FR-013**: System MUST provide `evenp` that returns T if integer is even, NIL otherwise (integer-only)
- **FR-014**: System MUST provide `signum` that returns -1, 0, or 1 for integers, and -1.0, 0.0, or 1.0 for floats

#### 戻り値

- **FR-015**: All predicates MUST return T (represented as i31:1) for true conditions
- **FR-016**: All predicates MUST return NIL (represented as i31:-2147483648) for false conditions
- **FR-017**: signum MUST return a fixnum (-1, 0, 1) for integer input, float for float input

### Key Entities

- **Fixnum**: 31ビット符号付き整数（i31ref）
- **Bignum**: 多倍長整数（$bignum構造体）
- **Ratio**: 有理数（$ratio構造体、分子/分母）
- **Float**: IEEE 754倍精度浮動小数点数（$float構造体）
- **Complex**: 複素数（$complex構造体、実部/虚部）
- **Symbol**: シンボル（$symbol構造体）
- **Character**: 文字（$char構造体）
- **Closure**: クロージャ/関数（$closure構造体）

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: ANSIテストスイートのnumbersカテゴリパス率が10%以上になる（現在1.3%）
- **SC-002**: ANSIテストスイートのconsカテゴリパス率が5%以上になる（現在1.0%）
- **SC-003**: 実装されたすべての述語関数がwasmtimeで正しく実行できる
- **SC-004**: 各述語関数に対して少なくとも5つのユニットテストがパスする
- **SC-005**: 既存のテストスイート（`nix flake check`）が引き続きパスする
- **SC-006**: 型ディスパッチのオーバーヘッドが最小限（各述語の実行時間が1ms未満）

## Scope

### In Scope

1. **型述語**: integerp, numberp, floatp, rationalp, complexp, symbolp, functionp, characterp
2. **数値述語**: zerop, plusp, minusp, oddp, evenp
3. **数値関数**: signum
4. **ユニットテスト**: 各関数に対する包括的なテスト
5. **ANSIテスト検証**: パス率の測定と報告

### Out of Scope

- **keywordp**: パッケージシステムへの依存が必要
- **vectorp, arrayp, stringp**: 複合型の完全実装が必要
- **typep, type-of**: 型システム全体の設計が必要
- **realp**: 今後の拡張として検討（rationalp + floatp）

## Assumptions

- WasmGCの `ref.test` 命令で型判別が可能
- 既存の型構造体（$bignum, $ratio, $float, $complex, $symbol, $char, $closure）が定義済み
- T/NILのi31表現（1/-2147483648）が既存コードで使用されている
- 浮動小数点演算にはf64命令が使用可能

## Dependencies

- **010-numeric-tower**: 数値型（Bignum, Ratio, Float, Complex）の実装
- **022-wasm-import-optimization**: wasmtime直接実行のためのI/O分析
- **020-ansi-test**: テストハーネス基盤
