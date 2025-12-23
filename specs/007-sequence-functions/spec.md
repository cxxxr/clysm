# Feature Specification: Common Lisp Sequence Functions

**Feature Branch**: `007-sequence-functions`
**Created**: 2025-12-23
**Status**: Draft
**Input**: User description: "Common Lispシーケンス関数のWasmGCコンパイラ実装 - Phase 8a"

## Overview

Phase 8aとして、Common Lisp標準シーケンス関数をClysmコンパイラに実装する。
既存のcons/car/cdr/list基盤（006-cons-list-ops）を活用し、リストに対する
基本操作、高階関数、探索・フィルタ関数を追加する。

### Prerequisites (実装済み)

- cons, car, cdr, list, consp, null, atom, listp, nth, nthcdr
- funcall, apply, lambda, クロージャ変換
- WasmGC の anyref/i31ref 型システム

### Scope Constraints

- 対象: リスト（cons cell）のみ（vectorは後続フェーズ）
- キーワード引数: 段階的に追加（初期は :key nil, :test #'eql をデフォルト）

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Basic List Operations (Priority: P1)

Lispプログラマーとして、リストの長さ取得、連結、逆順化などの基本操作を使用して、
データ構造を操作したい。

**Why this priority**: これらは他の高階関数の実装基盤となり、最も頻繁に使用される基本操作である。

**Independent Test**: `(length '(1 2 3))` => 3、`(append '(1 2) '(3 4))` => (1 2 3 4) などの単体テストで検証可能。

**Acceptance Scenarios**:

1. **Given** リスト `(1 2 3)`、**When** `(length list)` を呼び出す、**Then** 3 が返る
2. **Given** 空リスト `nil`、**When** `(length nil)` を呼び出す、**Then** 0 が返る
3. **Given** リスト `(1 2)` と `(3 4)`、**When** `(append '(1 2) '(3 4))` を呼び出す、**Then** `(1 2 3 4)` が返る
4. **Given** リスト `(1 2 3)`、**When** `(reverse list)` を呼び出す、**Then** `(3 2 1)` が返る（元リストは不変）
5. **Given** リスト `(1 2 3)`、**When** `(copy-list list)` を呼び出す、**Then** 同じ要素を持つ新しいリストが返る

---

### User Story 2 - Higher-Order Functions (Priority: P1)

Lispプログラマーとして、mapcar/reduce などの高階関数を使用して、
リストの各要素に対する変換や集約処理を簡潔に記述したい。

**Why this priority**: 関数型プログラミングの中核であり、多くのアルゴリズムの基盤となる。

**Independent Test**: `(mapcar #'1+ '(1 2 3))` => (2 3 4)、`(reduce #'+ '(1 2 3 4))` => 10 で検証可能。

**Acceptance Scenarios**:

1. **Given** リスト `(1 2 3)` と関数 `#'1+`、**When** `(mapcar #'1+ list)` を呼び出す、**Then** `(2 3 4)` が返る
2. **Given** 空リストと関数、**When** `(mapcar fn nil)` を呼び出す、**Then** `nil` が返る
3. **Given** リスト `(1 2 3 4)` と関数 `#'+`、**When** `(reduce #'+ list)` を呼び出す、**Then** 10 が返る
4. **Given** リスト `(1 2 3)` と初期値 10、**When** `(reduce #'+ list :initial-value 10)` を呼び出す、**Then** 16 が返る
5. **Given** リスト `(1 2 3)` と副作用関数、**When** `(mapc fn list)` を呼び出す、**Then** 各要素に関数が適用され、元リストが返る

---

### User Story 3 - Search and Filter Functions (Priority: P2)

Lispプログラマーとして、find/remove/count などの探索・フィルタ関数を使用して、
条件に合う要素を効率的に検索・抽出したい。

**Why this priority**: データ処理の実用性を大幅に向上させる。P1の高階関数があれば基本的な処理は可能だが、
これらがあると記述が格段に簡潔になる。

**Independent Test**: `(find 2 '(1 2 3))` => 2、`(remove 2 '(1 2 3 2))` => (1 3) で検証可能。

**Acceptance Scenarios**:

1. **Given** リスト `(1 2 3)`、**When** `(find 2 list)` を呼び出す、**Then** 2 が返る
2. **Given** リスト `(1 2 3)` で要素が存在しない場合、**When** `(find 5 list)` を呼び出す、**Then** `nil` が返る
3. **Given** リスト `(1 2 3 4)`、**When** `(find-if #'evenp list)` を呼び出す、**Then** 2 が返る
4. **Given** リスト `(1 2 3 2)`、**When** `(remove 2 list)` を呼び出す、**Then** `(1 3)` が返る（元リストは不変）
5. **Given** リスト `(1 2 3 2)`、**When** `(count 2 list)` を呼び出す、**Then** 2 が返る
6. **Given** リスト `(1 2 3)`、**When** `(position 2 list)` を呼び出す、**Then** 1 が返る（0-indexed）

---

### User Story 4 - List Membership and Association (Priority: P3)

Lispプログラマーとして、member/assoc などのリスト所属・連想リスト関数を使用して、
データの存在確認や連想検索を行いたい。

**Why this priority**: 便利だが、find/remove で代替可能な部分も多い。

**Independent Test**: `(member 2 '(1 2 3))` => (2 3)、`(assoc 'a '((a . 1) (b . 2)))` => (a . 1) で検証可能。

**Acceptance Scenarios**:

1. **Given** リスト `(1 2 3)`、**When** `(member 2 list)` を呼び出す、**Then** `(2 3)` が返る
2. **Given** 連想リスト `((a . 1) (b . 2))`、**When** `(assoc 'a alist)` を呼び出す、**Then** `(a . 1)` が返る
3. **Given** 連想リスト `((1 . a) (2 . b))`、**When** `(rassoc 'a alist)` を呼び出す、**Then** `(1 . a)` が返る

---

### User Story 5 - Quantifier Predicates (Priority: P3)

Lispプログラマーとして、every/some などの量化述語を使用して、
リスト全体に対する条件判定を簡潔に記述したい。

**Why this priority**: 便利だが、reduce や再帰で代替可能。

**Independent Test**: `(every #'numberp '(1 2 3))` => t、`(some #'evenp '(1 3 5))` => nil で検証可能。

**Acceptance Scenarios**:

1. **Given** リスト `(1 2 3)` と述語 `#'numberp`、**When** `(every pred list)` を呼び出す、**Then** `t` が返る
2. **Given** リスト `(1 2 3)` と述語 `#'evenp`、**When** `(some pred list)` を呼び出す、**Then** 2（最初に真となった値）が返る
3. **Given** リスト `(1 3 5)` と述語 `#'evenp`、**When** `(notany pred list)` を呼び出す、**Then** `t` が返る

---

### Edge Cases

- **空リスト**: すべての関数は `nil` に対して適切に動作すること（length => 0, mapcar => nil, find => nil）
- **単一要素リスト**: `(1)` に対して正しく動作すること
- **ネストしたリスト**: `((1 2) (3 4))` のようなネストしたリストを要素として扱えること
- **破壊的操作の影響**: nreverse は元リストを破壊するが、reverse は破壊しないこと
- **nil要素**: リスト内に `nil` 要素がある場合（`(1 nil 3)`）も正しく処理すること
- **循環リスト**: 無限ループにならないようエラー処理またはドキュメント化（初期実装では未サポートとする）

## Requirements *(mandatory)*

### Functional Requirements

#### Tier 1: 基本シーケンス関数

- **FR-001**: System MUST provide `length` function that returns the number of elements in a list
- **FR-002**: System MUST provide `append` function that non-destructively concatenates lists
- **FR-003**: System MUST provide `reverse` function that non-destructively reverses a list
- **FR-004**: System MUST provide `nreverse` function that destructively reverses a list
- **FR-005**: System MUST provide `last` function that returns the last cons cell of a list
- **FR-006**: System MUST provide `butlast` function that returns all but the last n elements
- **FR-007**: System MUST provide `copy-list` function that creates a shallow copy of a list

#### Tier 2: 高階シーケンス関数

- **FR-008**: System MUST provide `mapcar` function that applies a function to each element and returns results as a list
- **FR-009**: System MUST provide `mapc` function that applies a function to each element for side effects
- **FR-010**: System MUST provide `maplist` function that applies a function to successive cdrs
- **FR-011**: System MUST provide `reduce` function with support for `:initial-value` keyword argument

#### Tier 3: 探索・フィルタ関数

- **FR-012**: System MUST provide `find` function that returns the first element matching a value
- **FR-013**: System MUST provide `find-if` function that returns the first element satisfying a predicate
- **FR-014**: System MUST provide `position` function that returns the index of matching element
- **FR-015**: System MUST provide `position-if` function that returns the index of element satisfying predicate
- **FR-016**: System MUST provide `remove` function that non-destructively removes matching elements
- **FR-017**: System MUST provide `remove-if` function that non-destructively removes elements satisfying predicate
- **FR-018**: System MUST provide `remove-if-not` function that keeps only elements satisfying predicate
- **FR-019**: System MUST provide `count` function that counts matching elements
- **FR-020**: System MUST provide `count-if` function that counts elements satisfying predicate

#### Tier 4: その他（オプション）

- **FR-021**: System SHOULD provide `member` function that returns the tail starting with matching element
- **FR-022**: System SHOULD provide `assoc` function for association list lookup by key
- **FR-023**: System SHOULD provide `rassoc` function for association list lookup by value
- **FR-024**: System SHOULD provide `subst` function for tree substitution
- **FR-025**: System SHOULD provide `every`, `some`, `notany`, `notevery` quantifier predicates

#### Keyword Argument Support (段階的実装)

- **FR-026**: Functions accepting `:test` MUST default to `#'eql` when not specified
- **FR-027**: Functions accepting `:key` MUST default to `nil` (identity) when not specified
- **FR-028**: `reduce` MUST support `:initial-value` keyword argument
- **FR-029**: `reduce` SHOULD support `:from-end` keyword argument in future iteration

### Key Entities

- **List**: cons cellの連鎖で構成されるシーケンス。car/cdrでアクセス。nilで終端。
- **Association List (alist)**: `((key1 . value1) (key2 . value2) ...)` 形式のキーバリューペアリスト
- **Predicate Function**: 1引数を受け取りt/nilを返す関数（find-if, remove-if等で使用）
- **Key Function**: 要素から比較対象の値を抽出する関数（:key引数で指定）

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Tier 1-3の全関数（20関数）がwasmtimeで正常に実行可能
- **SC-002**: 各関数に対して最低5つの単体テストが存在し、すべてパス
- **SC-003**: 既存テスト（list-test.lisp, closure-test.lisp）が引き続き100%パス
- **SC-004**: 空リスト、単一要素、100要素以上のリストでも正しく動作
- **SC-005**: 高階関数（mapcar, reduce）でクロージャを引数として渡した場合に正しく動作
- **SC-006**: `nix flake check` がパス

### Acceptance Criteria

- すべてのTier 1-3関数が ANSI Common Lisp 仕様に準拠した動作をする
- 生成されるWasmバイナリが wasm-tools validate を通過する
- パフォーマンス: 1000要素リストに対するmapcarが1秒以内に完了

## Assumptions

- 循環リストは初期実装ではサポートしない（無限ループ防止のガード実装は行わない）
- :start, :end キーワード引数は将来の拡張として後回し
- 複数リストを引数に取るmapcar（例: `(mapcar #'+ '(1 2) '(3 4))`）は初期実装ではサポートしない
- :from-end t の最適化は初期実装では行わない（単純にreverseしてから処理）
