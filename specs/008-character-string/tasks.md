# Tasks: Common Lisp文字型と文字列操作

**Input**: Design documents from `/specs/008-character-string/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md
**Branch**: `008-character-string`

**Tests**: TDD必須（Constitution VII）- テストファースト

**Organization**: タスクはユーザーストーリー順に整理。各ストーリーは独立して実装・テスト可能。

## Format: `[ID] [P?] [Story] Description`

- **[P]**: 並列実行可能（異なるファイル、依存なし）
- **[Story]**: 所属ユーザーストーリー（US1, US2, ...）
- 全パスは `src/clysm/` または `tests/` からの相対パス

---

## Phase 1: Setup

**Purpose**: 既存構造の確認とテストファイル準備

- [X] T001 既存の$string型定義を確認 in src/clysm/compiler/codegen/gc-types.lisp
- [X] T002 [P] 文字テストファイルを作成 in tests/unit/character-test.lisp
- [X] T003 [P] 文字統合テストファイルを作成 in tests/integration/character-test.lisp
- [X] T004 [P] 文字列統合テストファイルを作成 in tests/integration/string-test.lisp

---

## Phase 2: Foundational (UTF-8ヘルパー)

**Purpose**: UTF-8エンコード/デコードの基盤（全文字列操作が依存）

**⚠️ CRITICAL**: 文字列操作はすべてUTF-8処理に依存

- [X] T005 UTF-8バイト長判定ヘルパーを実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T006 UTF-8デコードヘルパー（1文字）を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T007 UTF-8エンコードヘルパー（1文字）を実装 in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: UTF-8基盤完了 - ユーザーストーリー実装開始可能

---

## Phase 3: User Story 1 - 文字リテラルの読み込みと基本操作 (Priority: P1) 🎯 MVP

**Goal**: `#\a`, `#\Space` 等の文字リテラルと基本的な文字操作

**Independent Test**: `(char-code #\A)` が `65` を返すことを確認

### Tests for User Story 1

- [X] T008 [P] [US1] 文字リテラル `#\a` のトークナイザーテスト in tests/unit/tokenizer-test.lisp
- [X] T009 [P] [US1] 名前付き文字 `#\Space` `#\Newline` のテスト in tests/unit/tokenizer-test.lisp
- [X] T010 [P] [US1] char-code/code-char 統合テスト in tests/integration/character-test.lisp
- [X] T011 [P] [US1] 文字比較関数(char=, char<等)統合テスト in tests/integration/character-test.lisp
- [X] T012 [P] [US1] ケース変換(char-upcase/downcase)統合テスト in tests/integration/character-test.lisp

### Implementation for User Story 1

- [X] T013 [US1] `#\x` 単一文字リテラルのトークナイズを実装 in src/clysm/reader/tokenizer.lisp
- [X] T014 [US1] 名前付き文字テーブル(Space,Newline,Tab,Return)を実装 in src/clysm/reader/tokenizer.lisp
- [X] T015 [US1] :character トークンのパース処理を追加 in src/clysm/reader/parser.lisp
- [X] T016 [US1] AST :character リテラル型を追加 in src/clysm/compiler/ast.lisp
- [X] T017 [US1] 文字リテラルのコード生成(i31ref)を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T018 [US1] compile-char-code を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T019 [US1] compile-code-char を実装（無効値はNIL返却）in src/clysm/compiler/codegen/func-section.lisp
- [X] T020 [P] [US1] compile-char= を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T021 [P] [US1] compile-char/= を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T022 [P] [US1] compile-char< を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T023 [P] [US1] compile-char> を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T024 [P] [US1] compile-char<= を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T025 [P] [US1] compile-char>= を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T026 [P] [US1] compile-char-equal を実装（case-insensitive）in src/clysm/compiler/codegen/func-section.lisp
- [X] T027 [P] [US1] compile-char-lessp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T028 [P] [US1] compile-char-greaterp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T029 [P] [US1] compile-char-not-lessp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T030 [P] [US1] compile-char-not-greaterp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T031 [US1] compile-char-upcase を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T032 [US1] compile-char-downcase を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T033 [US1] compile-characterp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T034 [US1] プリミティブリストに文字関数を追加 in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: User Story 1完了 - 文字リテラルと基本操作が独立動作可能

---

## Phase 4: User Story 2 - 文字列リテラルと基本アクセス (Priority: P1)

**Goal**: `"hello"` 文字列の長さ取得と文字アクセス

**Independent Test**: `(char "hello" 0)` が `#\h` を返すことを確認

### Tests for User Story 2

- [X] T035 [P] [US2] 文字列リテラルパーステスト（既存確認）in tests/unit/tokenizer-test.lisp
- [X] T036 [P] [US2] エスケープシーケンス(\n,\t,\\,\")テスト in tests/unit/tokenizer-test.lisp
- [X] T037 [P] [US2] length文字列テスト（ASCII/Unicode）in tests/integration/string-test.lisp
- [X] T038 [P] [US2] char/scharアクセステスト in tests/integration/string-test.lisp
- [X] T039 [P] [US2] stringpテスト in tests/integration/string-test.lisp

### Implementation for User Story 2

- [X] T040 [US2] 既存文字列リテラル処理を確認 in src/clysm/reader/tokenizer.lisp
- [X] T041 [US2] compile-string-length を実装（UTF-8文字数カウント）in src/clysm/compiler/codegen/func-section.lisp
- [X] T042 [US2] compile-string-char を実装（UTF-8デコード付きアクセス）in src/clysm/compiler/codegen/func-section.lisp
- [X] T043 [US2] compile-schar を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T044 [US2] compile-stringp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T045 [US2] length関数を文字列対応に拡張 in src/clysm/compiler/codegen/func-section.lisp
- [X] T046 [US2] インデックス範囲外エラー処理を追加 in src/clysm/compiler/codegen/func-section.lisp
- [X] T047 [US2] プリミティブリストに文字列基本関数を追加 in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: User Story 2完了 - 文字列長と文字アクセスが独立動作可能

---

## Phase 5: User Story 3 - 文字列の比較 (Priority: P2)

**Goal**: `string=`, `string<` 等による文字列比較

**Independent Test**: `(string= "abc" "abc")` が真を返すことを確認

### Tests for User Story 3

- [X] T048 [P] [US3] string=/string/=テスト in tests/integration/string-test.lisp
- [X] T049 [P] [US3] string</string>/string<=/string>=テスト in tests/integration/string-test.lisp
- [X] T050 [P] [US3] string-equal/string-lesspテスト（case-insensitive）in tests/integration/string-test.lisp

### Implementation for User Story 3

- [X] T051 [P] [US3] compile-string= を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T052 [P] [US3] compile-string/= を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T053 [P] [US3] compile-string< を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T054 [P] [US3] compile-string> を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T055 [P] [US3] compile-string<= を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T056 [P] [US3] compile-string>= を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T057 [P] [US3] compile-string-equal を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T058 [P] [US3] compile-string-not-equal を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T059 [P] [US3] compile-string-lessp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T060 [P] [US3] compile-string-greaterp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T061 [P] [US3] compile-string-not-lessp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T062 [P] [US3] compile-string-not-greaterp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T063 [US3] プリミティブリストに文字列比較関数を追加 in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: User Story 3完了 - 文字列比較が独立動作可能

---

## Phase 6: User Story 4 - 文字列の生成と変換 (Priority: P2)

**Goal**: `make-string`, `string-upcase` 等による文字列生成・変換

**Independent Test**: `(make-string 5 :initial-element #\x)` が `"xxxxx"` を返すことを確認

### Tests for User Story 4

- [X] T064 [P] [US4] make-stringテスト in tests/integration/string-test.lisp
- [X] T065 [P] [US4] string変換テスト（シンボル/文字から）in tests/integration/string-test.lisp
- [X] T066 [P] [US4] string-upcase/downcase/capitalizeテスト in tests/integration/string-test.lisp

### Implementation for User Story 4

- [X] T067 [US4] compile-make-string を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T068 [US4] compile-string（designator変換）を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T069 [US4] compile-string-upcase を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T070 [US4] compile-string-downcase を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T071 [US4] compile-string-capitalize を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T072 [US4] プリミティブリストに文字列生成/変換関数を追加 in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: User Story 4完了 - 文字列生成・変換が独立動作可能

---

## Phase 7: User Story 5 - 部分文字列と連結 (Priority: P3)

**Goal**: `subseq`, `concatenate` による文字列操作

**Independent Test**: `(subseq "hello" 1 4)` が `"ell"` を返すことを確認

### Tests for User Story 5

- [X] T073 [P] [US5] subseq文字列テスト in tests/integration/string-test.lisp
- [X] T074 [P] [US5] concatenate 'stringテスト in tests/integration/string-test.lisp
- [X] T075 [P] [US5] エッジケーステスト（空文字列、範囲エラー）in tests/integration/string-test.lisp

### Implementation for User Story 5

- [X] T076 [US5] compile-string-subseq を実装（UTF-8対応）in src/clysm/compiler/codegen/func-section.lisp
- [X] T077 [US5] subseq関数を文字列対応に拡張 in src/clysm/compiler/codegen/func-section.lisp
- [X] T078 [US5] compile-concatenate-string を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T079 [US5] concatenate関数を文字列結果型対応に拡張 in src/clysm/compiler/codegen/func-section.lisp
- [X] T080 [US5] subseq範囲エラー処理を追加 in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: User Story 5完了 - 部分文字列と連結が独立動作可能

---

## Phase 8: User Story 6 - 文字述語 (Priority: P3)

**Goal**: `alpha-char-p`, `digit-char-p` 等の文字分類述語

**Independent Test**: `(alpha-char-p #\a)` が真を返すことを確認

### Tests for User Story 6

- [X] T081 [P] [US6] alpha-char-pテスト in tests/integration/character-test.lisp
- [X] T082 [P] [US6] digit-char-pテスト（基数対応含む）in tests/integration/character-test.lisp
- [X] T083 [P] [US6] alphanumericpテスト in tests/integration/character-test.lisp
- [X] T084 [P] [US6] upper-case-p/lower-case-pテスト in tests/integration/character-test.lisp

### Implementation for User Story 6

- [X] T085 [US6] compile-alpha-char-p を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T086 [US6] compile-digit-char-p を実装（オプション基数対応）in src/clysm/compiler/codegen/func-section.lisp
- [X] T087 [US6] compile-alphanumericp を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T088 [US6] compile-upper-case-p を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T089 [US6] compile-lower-case-p を実装 in src/clysm/compiler/codegen/func-section.lisp
- [X] T090 [US6] プリミティブリストに文字述語を追加 in src/clysm/compiler/codegen/func-section.lisp

**Checkpoint**: User Story 6完了 - 文字述語が独立動作可能

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: 全ストーリー横断の改善

- [X] T091 [P] Unicode文字（日本語）の統合テスト in tests/integration/string-test.lisp
- [X] T092 [P] エッジケース（空文字列、境界条件）統合テスト in tests/integration/string-test.lisp
- [X] T093 全テスト実行と結果確認
- [X] T094 wasm-tools validateでWasm出力を検証
- [X] T095 quickstart.md の全例をREPLで検証

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup ─────────────────────────────────────────────────┐
                                                                │
Phase 2: Foundational (UTF-8ヘルパー) ←─────────────────────────┘
    │
    ▼ (UTF-8基盤完了後、US1とUS2は並列可能)
    │
    ├──▶ Phase 3: US1 (文字リテラル) 🎯 MVP
    │
    └──▶ Phase 4: US2 (文字列基本アクセス)
              │
              ▼ (US1, US2完了後)
              │
              ├──▶ Phase 5: US3 (文字列比較)
              │
              └──▶ Phase 6: US4 (文字列生成/変換)
                        │
                        ▼ (US3, US4完了後)
                        │
                        ├──▶ Phase 7: US5 (subseq/concatenate)
                        │
                        └──▶ Phase 8: US6 (文字述語)
                                  │
                                  ▼
                             Phase 9: Polish
```

### User Story Dependencies

| Story | Depends On | Can Parallelize With |
|-------|------------|----------------------|
| US1 | Foundational | US2 |
| US2 | Foundational | US1 |
| US3 | US1, US2 | US4 |
| US4 | US1, US2 | US3 |
| US5 | US3, US4 | US6 |
| US6 | US1 | US5 |

### Parallel Opportunities

**Phase 3-4 並列実行（US1 + US2）:**
```bash
# 異なるテストファイル - 並列可能
Task: T008-T012 (US1テスト) || Task: T035-T039 (US2テスト)

# 異なる関数実装 - 並列可能
Task: T020-T030 (文字比較関数) || Task: T041-T044 (文字列アクセス関数)
```

**Phase 5 内並列実行（US3）:**
```bash
# 全比較関数は独立 - 12関数並列可能
Task: T051-T062 (string=, string/=, string<, ... 12関数)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Phase 1: Setup完了
2. Phase 2: Foundational完了（UTF-8ヘルパー）
3. Phase 3: User Story 1完了
4. **STOP and VALIDATE**: `(char-code #\A)` → `65` を確認
5. MVP完了 - 文字リテラルが動作

### Incremental Delivery

1. Setup + Foundational → UTF-8基盤完了
2. + US1 → 文字リテラル・基本操作完了（MVP）
3. + US2 → 文字列アクセス完了
4. + US3 + US4 → 文字列比較・生成完了
5. + US5 + US6 → 全機能完了

### Task Count Summary

| Phase | Tasks | Parallel |
|-------|-------|----------|
| Setup | 4 | 3 |
| Foundational | 3 | 0 |
| US1 | 27 | 16 |
| US2 | 13 | 5 |
| US3 | 16 | 14 |
| US4 | 9 | 3 |
| US5 | 8 | 3 |
| US6 | 10 | 4 |
| Polish | 5 | 2 |
| **Total** | **95** | **50** |

---

## Notes

- [P] タスク = 異なるファイル、依存なし
- [US*] ラベル = ユーザーストーリーへのトレーサビリティ
- 各ストーリーは独立して完了・テスト可能
- テストが失敗することを確認してから実装
- 論理的なグループごとにコミット
- チェックポイントで各ストーリーの独立動作を検証
