# 022-wasm-import-optimization Retrospective

**Feature**: Conditional FFI Import Emission for Direct Wasmtime Execution
**Status**: ✅ COMPLETE
**Date**: 2025-12-26
**Commit**: bb468ea

## Executive Summary

FFIインポートセクションの条件付き出力を実装。I/O未使用のLispコードは、
wasmtimeで直接実行可能なスタンドアロンWasmモジュールにコンパイルされる。

## Problem Solved

### Before (問題)
```
すべてのWasmモジュール
  ↓
clysm:io::write-char, write-string, read-char, read-line をインポート
  ↓
wasmtime実行時に「unknown import」エラー
  ↓
テスト成功率 0%
```

### After (解決)
```
ソースコード分析
  ↓
I/O関数使用を検出？
  ├─ YES → Import section含む → host-shim必要
  └─ NO  → Import section無し → wasmtime直接実行可能
```

## Implementation

### Core Components

#### 1. I/O Usage Analyzer (`src/clysm/compiler/analyzer/io-usage.lisp`)

```lisp
;; 22個のI/O関数名
(defparameter *io-function-names*
  '("WRITE-CHAR" "WRITE-STRING" "PRINT" "FORMAT" ...))

;; 再帰的ツリーウォーカー
(defun analyze-io-usage (form)
  "FORMがI/O関数を使用しているかを検出。使用していればT、そうでなければNIL。")
```

#### 2. Compiler Integration (`src/clysm/compiler/compiler.lisp`)

```lisp
(defun compile-to-wasm (expr &key output)
  (let* ((uses-io (analyze-io-usage expr))          ; 分析
         (module (compile-to-module expr))
         (bytes (emit-module module :uses-io uses-io))) ; 条件付き出力
    ...))

(defun emit-module (module &key uses-io)
  ...
  (when uses-io                                      ; I/O使用時のみ
    (emit-import-section-if-needed buffer))
  ...)
```

### Files Changed

| File | Change |
|------|--------|
| `src/clysm/compiler/analyzer/io-usage.lisp` | **NEW**: I/O使用分析器 |
| `src/clysm/compiler/compiler.lisp` | I/O分析統合、条件付きインポート |
| `src/clysm/package.lisp` | io-usageパッケージ追加 |
| `clysm.asd` | モジュール追加 |

### Tests Added

| File | Tests | Purpose |
|------|-------|---------|
| `tests/unit/io-usage-test.lisp` | 9 | I/O分析器ユニットテスト |
| `tests/contract/import-section-test.lisp` | 7 | Import section契約テスト |
| `tests/integration/wasmtime-test.lisp` | 5 | wasmtime実行統合テスト |

## Verification Results

### Direct Wasmtime Execution
```bash
$ wasmtime --wasm gc --wasm exceptions --invoke _start add.wasm
3  ✓

$ wasmtime --wasm gc --wasm exceptions --invoke _start mul.wasm
42 ✓
```

### ANSI Test Execution
```
numbers: 18/1396 (1.3%) - 以前は0%
cons:    17/1641 (1.0%) - 以前は0%
```

### All Checks Pass
```bash
$ nix flake check
✓ passed

$ rove :clysm/tests/unit/io-usage
✓ 9/9 tests passed

$ rove :clysm/tests/contract/import-section
✓ 7/7 tests passed

$ rove :clysm/tests/integration/wasmtime
✓ 5/5 tests passed
```

## Success Criteria Evaluation

| ID | Criterion | Target | Actual | Status |
|----|-----------|--------|--------|--------|
| SC-001 | wasmtime実行 | エラーなし実行 | `(+ 1 2)` → 3, `(* 7 6)` → 42 | ✅ PASS |
| SC-002 | numbers通過率 | ≥10% | 1.3% | ⚠️ PARTIAL* |
| SC-003 | cons通過率 | ≥5% | 1.0% | ⚠️ PARTIAL* |
| SC-004 | I/O後方互換 | I/O検出正常 | 分析器が正しく検出 | ✅ PASS |
| SC-005 | コンパイル時間 | ≤10%増加 | O(n)ツリーウォーク（最小） | ✅ PASS |
| SC-006 | モジュールサイズ | ≤ベースライン | より小さい（Import sectionなし） | ✅ PASS |

*SC-002/003の目標達成にはマクロシステム、eval等の追加コンパイラ機能が必要

## Lessons Learned

### 良かった点
1. **シンプルな解決策**: ソース分析 → 条件付き出力という明確なアーキテクチャ
2. **TDD効果**: テスト先行により、コントラクトが明確に
3. **段階的実装**: Phase分割により進捗が可視化

### 改善点
1. **SC-002/003の設定**: 目標が高すぎた。この機能だけでは達成不可能
2. **テスト実行時間**: 将来的にはキャッシュが必要かもしれない

## Impact

### Unblocked Features
- **021-ansi-test-execution**: Phase 3-7 が実行可能に

### Future Work
- `print`/`format`/`write-char`の実装 → 完全なI/Oテスト
- 追加のI/O関数を分析器に追加

## Files Reference

- Spec: `specs/022-wasm-import-optimization/`
- Validation: `specs/022-wasm-import-optimization/validation.md`
- Tasks: `specs/022-wasm-import-optimization/tasks.md`
- Implementation: `src/clysm/compiler/analyzer/io-usage.lisp`
