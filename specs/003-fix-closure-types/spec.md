# Feature Specification: Closure Type Index Fix

**Feature Branch**: `003-fix-closure-types`
**Created**: 2025-12-23
**Status**: Draft
**Input**: User description: "closure/lambda/funcall の型インデックス不整合を修正する - gc-types.lisp と compiler.lisp の型配置の不整合によりクロージャテストが全て失敗している問題を解決"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic Lambda Execution (Priority: P1)

開発者がコンパイラを使用して、引数なしの lambda 式を funcall で実行できる。

**Why this priority**: 最も基本的なクロージャ機能であり、他の全てのクロージャ機能の基盤となる。この機能が動作しなければ、flet/labels/高階関数など全ての機能が使えない。

**Independent Test**: `(funcall (lambda () 42))` をコンパイル・実行し、42 が返ることを確認する。

**Acceptance Scenarios**:

1. **Given** lambda 式 `(lambda () 42)` を含むコード, **When** funcall で実行する, **Then** 42 が返る
2. **Given** 複数の引数を持つ lambda `(lambda (a b) (+ a b))`, **When** funcall で (10, 20) を渡して実行する, **Then** 30 が返る
3. **Given** 0引数、1引数、2引数、3引数の lambda, **When** それぞれの arity に対応する funcall を実行する, **Then** 全て正しい結果を返す

---

### User Story 2 - Closure Variable Capture (Priority: P1)

開発者が let で束縛した変数を lambda 内からキャプチャして参照できる。

**Why this priority**: クロージャの本質的機能であり、実用的な Common Lisp プログラムに必須。

**Independent Test**: `(let ((x 10)) (funcall (lambda () x)))` をコンパイル・実行し、10 が返ることを確認する。

**Acceptance Scenarios**:

1. **Given** let で変数 x を 10 に束縛, **When** lambda 内から x を参照して funcall で実行, **Then** 10 が返る
2. **Given** 複数の変数 x=10, y=20 を let で束縛, **When** lambda 内から両方を参照して加算, **Then** 30 が返る
3. **Given** ネストした let で外側の変数 x と内側の変数 y, **When** 内側の lambda から両方を参照, **Then** 両方の値が正しくキャプチャされる

---

### User Story 3 - Local Function Definitions (Priority: P2)

開発者が flet/labels を使用してローカル関数を定義し、呼び出せる。

**Why this priority**: flet/labels は lambda の応用であり、US1/US2 が動作すれば実現可能。再帰的なローカル関数（labels）は多くのアルゴリズムで必要。

**Independent Test**: `(flet ((f (x) (+ x 1))) (f 10))` をコンパイル・実行し、11 が返ることを確認する。

**Acceptance Scenarios**:

1. **Given** flet で関数 f を定義, **When** f を呼び出す, **Then** 正しい結果が返る
2. **Given** labels で再帰関数 fact を定義, **When** (fact 5) を呼び出す, **Then** 120 が返る
3. **Given** flet 内で外部変数をキャプチャする関数, **When** その関数を呼び出す, **Then** キャプチャした値が参照できる

---

### Edge Cases

- lambda の arity が 4 以上の場合、汎用関数型（func_N）で正しく処理される
- キャプチャする自由変数が 0 個の場合、env フィールドが null になる
- ネストした funcall で内側の lambda が外側の lambda の引数をキャプチャする場合

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: コンパイラ MUST gc-types.lisp の型インデックス定数と compiler.lisp の型セクション生成を一致させる
- **FR-002**: コンパイラ MUST 0引数関数（func_0）に対して正しい型インデックスで call_ref を生成する
- **FR-003**: コンパイラ MUST 1引数関数（func_1）に対して正しい型インデックスで call_ref を生成する
- **FR-004**: コンパイラ MUST 2引数関数（func_2）に対して正しい型インデックスで call_ref を生成する
- **FR-005**: コンパイラ MUST 3引数関数（func_3）に対して正しい型インデックスで call_ref を生成する
- **FR-006**: コンパイラ MUST N引数関数（func_N、N>=4）に対して正しい型インデックスで call_ref を生成する
- **FR-007**: 修正後も既存の特別変数テスト（002-special-vars-compiler）が全て成功し続ける
- **FR-008**: 修正後も既存の算術・条件分岐・バインディング等のテストが全て成功し続ける

### Key Entities

- **Type Index Constants**: gc-types.lisp で定義される型インデックス定数（+type-func-0+ 等）
- **Type Section**: compiler.lisp で生成される Wasm モジュールの型セクション
- **Closure Struct**: code_0, code_1, code_2, code_N, env フィールドを持つクロージャ構造体
- **Function Type**: 各 arity に対応する関数型定義（param/result の組み合わせ）

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: tests/integration/closure-test.lisp の全テスト（現在16件）が成功する
- **SC-002**: tests/integration/special-var-test.lisp の全テストが引き続き成功する
- **SC-003**: `(funcall (lambda () 42))` が 42 を返す（現在は 1 を返す）
- **SC-004**: `(funcall (lambda (x) x) 42)` が 42 を返す（現在は 1 を返す）
- **SC-005**: 全テストスイート実行時、closure/tco 以外のテストでリグレッションが発生しない
- **SC-006**: gc-types.lisp の型インデックス定数と compiler.lisp の型セクション生成が完全に一致する

## Assumptions

- `+type-binding-frame+` (type 8) は将来の US5（例外安全な動的バインディング）で使用予定だが、現時点では compiler.lisp で生成されていない
- 修正方針として「gc-types.lisp の定数を compiler.lisp の実際の配置に合わせる」アプローチを採用する（理由: 影響範囲が限定的で、既存のコード生成ロジックを変更しない）
- `+type-instance+` (type 6) と `+type-standard-class+` (type 7) は CLOS 用に予約されているが、現在は空の struct として生成されている
