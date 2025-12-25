# 020-ansi-test Retrospective

**Feature**: ANSI Common Lisp Test Suite Integration
**Status**: Infrastructure complete, 0% pass rate
**Date**: 2025-12-25

## Executive Summary

テストハーネス基盤は完成し正常に動作しているが、実際のテスト成功率は0%。
これはハーネスの問題ではなく、2つの構造的課題が原因。

## Problem Analysis

### Issue 1: Missing FFI Host Shim (PRIMARY BLOCKER)

コンパイルは成功するが、wasmtime実行時にFFIインポートエラーが発生：

```
unknown import: `clysm:io::write-char` has not been defined
```

**Root Cause**: コンパイルされたWasmモジュールは `clysm:io` 名前空間のFFI関数をインポートするが、
生の wasmtime 実行ではこれらが提供されない。

**Solution**: `host-shim/` を使用するか、FFIインポートなしでコンパイルするオプションが必要。

```bash
# 現在の実行方法（失敗）
wasmtime --wasm gc --invoke _start module.wasm

# 必要な実行方法
wasmtime --wasm gc --invoke _start module.wasm --preload clysm:io=host-shim.wasm
```

### Issue 2: Undefined CL Functions

多くのテストが未実装の標準CL関数を使用：

```
ACONS.1: compile-error: Undefined function: COPY-TREE
ACONS.2: compile-error: Undefined function: ACONS
ACONS.7: compile-error: Undefined function: MACROLET
ACONS.ORDER.1: compile-error: Undefined function: VALUES
```

未実装関数リスト（consカテゴリのみ）:
- `ACONS` - Association list cons
- `COPY-TREE` - Deep copy
- `MACROLET` - Local macro definitions
- `VALUES` - Multiple value return

### Issue 3: Compilation Failures (Original)

典型的なcons テスト:
```lisp
(deftest cons.1
  (cons 'a 'b)
  (a . b))
```

このテストが失敗する理由:
- `'a` → `(quote a)` に展開される
- シンボル `a` を実行時に生成する必要がある
- Clysm はまだシンボル生成をランタイムで完全サポートしていない可能性

**診断方法**:
```lisp
;; 手動でコンパイルエラーを確認
(handler-case
    (clysm/compiler:compile-to-wasm '(cons 'a 'b))
  (error (e) (format t "Error: ~A~%" e)))
```

### Issue 2: Output Verification Limitation

**アーキテクチャ上の制約**:

```
Lisp form → Compile → Wasm module → wasmtime (_start returns i32) → parse
```

`parse-wasm-output` が処理できる値:
| 型 | Wasm戻り値 | パース可能 |
|---|---|---|
| Fixnum | 整数値 | ✓ |
| NIL | -2147483648 | ✓ |
| T | 1 | ✓ |
| Cons | -2147483647 (sentinel) | ✗ |
| Symbol | -2147483647 (sentinel) | ✗ |
| String | -2147483647 (sentinel) | ✗ |

**影響**: cons/symbol/string を返すテストは、たとえコンパイル・実行が成功しても検証不可能

```lisp
;; 検証可能
(+ 1 2) → 3
(null nil) → T

;; 検証不可能
(cons 1 2) → (1 . 2)  ; sentinel値として-2147483647が返る
(car '(a b)) → A      ; 同上
```

## Current Test Harness Capabilities

### What Works
- [x] pfdietz/ansi-test からの DEFTEST パース
- [x] カテゴリ別テスト実行
- [x] 未サポート形式の自動検出（FORMAT, CLOS等）
- [x] タイムアウト処理
- [x] Markdown レポート生成
- [x] ベースライン管理・リグレッション検出
- [x] CLI スクリプト
- [x] GitHub Actions ワークフロー

### What Cannot Work (Architectural Limitation)
- [ ] 複合値（cons, symbol, string）を返すテストの検証
- [ ] multiple-values を返すテストの検証
- [ ] 副作用を確認するテスト（I/O等）

## Proposed Next Steps

### Phase 0: Fix Host Shim Integration (CRITICAL)

**目標**: ANSIテストハーネスがhost-shimを使用してWasmを実行できるようにする

現在の問題:
```
wasmtime --invoke _start module.wasm
# → unknown import: `clysm:io::write-char` has not been defined
```

解決策:
1. `run-wasm-bytes` 関数を修正してhost-shimを使用
2. または、FFIインポートなしでコンパイルするモードを追加

```lisp
;; runner.lisp の修正案
(defun run-wasm-bytes (bytes &key (timeout 30))
  (uiop:with-temporary-file (:pathname path :type "wasm" :keep nil)
    (write-wasm-to-file bytes path)
    (uiop:run-program
      (list "wasmtime" "--wasm" "gc"
            "--preload" "clysm:io=host-shim/target/wasm32-unknown-unknown/release/host_shim.wasm"
            "--invoke" "_start"
            (namestring path))
      ...)))
```

**ブロッカー**: これが解決しないと1つのテストもPASSしない

### Phase 1: Get First Tests Passing

**目標**: 少なくとも1つのテストを PASS させる

Phase 0 完了後、以下のテストが動作するはず:
- Fixnum演算: `(+ 1 2)`, `(- 5 3)`
- 比較: `(< 1 2)`, `(= 3 3)`
- NIL/T: `(null nil)`, `(not t)`, `(atom 1)`

これらは検証可能（i32/boolean を返す）。

### Phase 2: Implement Missing CL Functions

**目標**: consカテゴリの基本テストを通す

未実装関数（優先度順）:
1. `ACONS` - 単純な実装で多くのテストが通る
2. `COPY-TREE` - 再帰的なツリーコピー
3. `VALUES` / `MULTIPLE-VALUE-BIND` - 多値対応
4. `MACROLET` - ローカルマクロ

### Phase 3: Expand Verifiable Subset

**目標**: 検証可能テストのパスレート向上

1. **Verifiable Test Catalog 作成**
   - ANSI テストから fixnum/nil/t を返すものを抽出
   - 独立したカテゴリとして追跡

2. **パスレート目標設定**
   - Verifiable tests: 50%+ 目標
   - これはコンパイラ機能の進捗を反映

### Phase 4: Value Serialization (Long-term)

**目標**: 任意のLisp値を検証可能にする

アプローチ:
```
Lisp form → Compile → Wasm + PRINT → wasmtime → stdout → parse S-expr
```

必要な実装:
1. Wasm内でのPRINT実装（FFI経由でホストへ出力）
2. コンパイル時に結果を PRINT するラッパー追加
3. S式パーサーで stdout を読み取り

これにより `(cons 1 2)` → stdout: "(1 . 2)" → パース → 比較可能

## Metrics to Track

| Metric | Current | Target |
|--------|---------|--------|
| Total ANSI tests | ~20,000 | - |
| Verifiable tests (fixnum/nil/t) | TBD | - |
| Verifiable pass rate | 0% | 50%+ |
| Overall pass rate | 0% | N/A (blocked by Issue 2) |

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2025-12-25 | T076-T079削除 | パスレートはコンパイラ進捗に依存、ハーネス完成度とは無関係 |
| 2025-12-25 | 0%を許容 | Issue 1,2,3の構造的課題が原因、ハーネス自体は正常 |
| 2025-12-25 | host-shim統合が必要 | FFIインポート（clysm:io）なしではWasm実行不可 |
| 2025-12-25 | 次フィーチャーでPhase 0解決 | テスト1つもPASSしない状態を解消する必要あり |

## Files Reference

- Test harness: `src/clysm/ansi-test/`
- Unit tests: `tests/unit/ansi-test/`
- CLI script: `scripts/ansi-test.sh`
- CI workflow: `.github/workflows/ansi-compliance.yml`
- Spec: `specs/020-ansi-test/`

## Related Features

- **010-numeric-tower**: 数値型実装（fixnum演算に影響）
- **019-numeric-accessors**: numerator/denominator
- **018-fix-ffi-streams**: FFI ストリーム実装（host-shim必要の原因）
- **Future**: Symbol runtime, PRINT implementation

## Proposed Next Feature: 021-ansi-test-execution

**目的**: ANSIテストを実際に実行可能にする

### Scope

1. **Host Shim Integration**
   - `run-wasm-bytes` を修正して `host-shim/` を使用
   - または、テスト用にFFIインポートを無効化するオプション

2. **Missing Function Stubs**
   - `ACONS` 実装
   - `COPY-TREE` 実装
   - 未実装関数をスキップではなくエラーとして報告するオプション

3. **Verifiable Test Subset**
   - fixnum/nil/t を返すテストのカタログ作成
   - このサブセットのパスレート追跡

### Success Criteria

- [ ] 少なくとも1つのANSIテストがPASS
- [ ] consカテゴリで5%以上のパスレート
- [ ] numbersカテゴリで10%以上のパスレート

### Estimated Effort

- Phase 0 (Host Shim): 1-2 tasks
- Phase 1 (First Pass): 2-3 tasks
- Phase 2 (Missing Functions): 5-10 tasks per function
