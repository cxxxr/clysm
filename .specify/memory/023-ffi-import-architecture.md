# 023-ffi-import-architecture

**Feature**: FFI Import Architecture for Static and Dynamic Calls
**Status**: DESIGN
**Date**: 2025-12-31
**Related**: 022-wasm-import-optimization

## Problem Statement

### 発見された問題

Feature 022 で I/O 使用分析を実装したが、実際には機能していなかった：

```lisp
;; compiler.lisp:44 - uses-io を渡している
(bytes (emit-module module :uses-io uses-io))

;; compiler.lisp:222 - しかし uses-io は使われていない！
(defun emit-module (module &key uses-io)  ; uses-io は参照されない
  ...
  (emit-import-section-if-needed buffer functions))  ; *ffi-environment* 全体を見る
```

### 根本原因

```
システムロード時:
  streams/ffi-io.lisp が読み込まれる
    ↓
  define-foreign-function で I/O関数が *ffi-environment* に登録
    ↓
  グローバルに蓄積される

コンパイル時:
  compile-to-wasm '(+ 1 2)
    ↓
  emit-import-section-if-needed が *ffi-environment* 全体を見る
    ↓
  全FFIをインポートセクションに含める（使用有無に関わらず）
    ↓
  wasmtime で「unknown import」エラー
```

### Common Lisp の動的性

静的解析だけでは不十分：

```lisp
;; 静的に検出可能
(write-char #\A)              ; 直接呼び出し
(funcall 'write-char #\A)     ; リテラルシンボル

;; 静的に検出不可能
(funcall (intern "WRITE-CHAR") #\A)   ; 実行時にシンボル生成
(funcall (get-handler request) data)  ; 実行時に関数決定
(apply fn args)                       ; fn は実行時に決まる
```

Wasm のインポートはモジュールインスタンス化時に全て解決される必要があり、実行時に追加できない。

## Solution Architecture

### 2層アーキテクチャ

```
┌─────────────────────────────────────────────────────────┐
│                    静的呼び出し                          │
│  (write-char #\A)                                       │
│         │                                               │
│         ▼                                               │
│  ┌─────────────────┐     直接インポート                 │
│  │ (call $write-char) │  → 高速、インライン可能          │
│  └─────────────────┘                                   │
│                                                         │
├─────────────────────────────────────────────────────────┤
│                    動的呼び出し                          │
│  (funcall (intern "WRITE-CHAR") #\A)                   │
│         │                                               │
│         ▼                                               │
│  ┌─────────────────────────┐                           │
│  │ (call $dynamic-call ...) │ → ホスト経由              │
│  └─────────────────────────┘                           │
└─────────────────────────────────────────────────────────┘
```

### コンポーネント設計

#### 1. FFI使用分析器の拡張

```lisp
;; analyzer/ffi-usage.lisp (新規または io-usage.lisp 拡張)

(defun analyze-ffi-usage (form)
  "展開済みフォームから使用されるFFI関数を検出。
   Returns: (values used-ffis has-dynamic-call-p)
   - used-ffis: 静的に検出されたFFI関数名のリスト
   - has-dynamic-call-p: funcall/apply の動的使用があるか"
  ...)

(defun detect-dynamic-call-p (form)
  "funcall/apply が動的シンボルで使用されているか検出"
  ;; (funcall 'known-symbol ...) は静的
  ;; (funcall expr ...) where expr is not a quoted symbol → 動的
  ...)
```

#### 2. dynamic-call インポート

```lisp
;; 1つの汎用インポートで全動的呼び出しに対応
(import "clysm:runtime" "dynamic-call"
        (func $dynamic-call (param $name anyref) (param $args anyref)
                            (result anyref)))
```

ホスト側実装:
```javascript
// host-shim/runtime.js
function dynamicCall(funcName, args) {
  const fn = runtime.resolveFunction(funcName);
  if (!fn) {
    throw new Error(`Unknown function: ${funcName}`);
  }
  return fn.apply(null, args);
}

// 全FFI関数を登録
runtime.registerFunction("WRITE-CHAR", writeChar);
runtime.registerFunction("SIN", Math.sin);
// ...
```

#### 3. コンパイラ統合

```lisp
(defun compile-to-wasm (expr &key output extra-exports)
  (let* ((expanded (macroexpand-all ... expr))
         ;; FFI使用分析
         (ffi-analysis (analyze-ffi-usage expanded))
         (used-ffis (ffi-analysis-used-ffis ffi-analysis))
         (needs-dynamic-call (ffi-analysis-dynamic-p ffi-analysis))
         ;; コンパイル
         (module (compile-to-module expanded ...)))
    ;; 使用FFIのみをインポート
    (emit-module module
                 :used-ffis used-ffis
                 :include-dynamic-call needs-dynamic-call)))
```

#### 4. emit-module の修正

```lisp
(defun emit-module (module &key used-ffis include-dynamic-call)
  ...
  ;; 静的に検出されたFFIのみインポート
  (emit-ffi-imports buffer used-ffis)
  ;; 動的呼び出しが必要な場合のみ dynamic-call をインポート
  (when include-dynamic-call
    (emit-dynamic-call-import buffer))
  ...)
```

### コンパイルモード

```lisp
;; 最小モード: 静的検出のFFIのみ、動的呼び出しはエラー
(compile-to-wasm expr :ffi-mode :minimal)

;; 完全モード: 動的呼び出し対応、ホスト環境必須
(compile-to-wasm expr :ffi-mode :full)

;; 自動: 動的呼び出しを検出したら :full (デフォルト)
(compile-to-wasm expr :ffi-mode :auto)
```

## Implementation Plan

### Phase 1: 022 の修正 (Critical)

`uses-io` パラメータを実際に使用するよう修正：

```lisp
;; Before
(defun emit-module (module &key uses-io)
  ...
  (emit-import-section-if-needed buffer functions))

;; After
(defun emit-module (module &key uses-io)
  ...
  (when uses-io
    (emit-import-section-if-needed buffer functions)))
```

または、より良いアプローチ：

```lisp
(defun emit-module (module &key used-ffis)
  ...
  (when used-ffis
    (emit-selected-ffi-imports buffer used-ffis)))
```

### Phase 2: FFI使用分析器

1. `analyze-io-usage` を `analyze-ffi-usage` に一般化
2. 動的呼び出し検出を追加
3. 使用FFIリストを返すよう変更

### Phase 3: 動的呼び出しサポート

1. `$dynamic-call` インポート生成
2. ホストシム実装 (`host-shim/runtime.js`)
3. `funcall`/`apply` のコード生成を修正

### Phase 4: テスト環境整備

1. `compile-and-run` をFFI環境に依存しないよう修正
2. 動的呼び出しテスト用のホスト環境設定

## Test Strategy

### Unit Tests

```lisp
;; FFI使用分析
(deftest analyze-ffi-usage-test
  ;; 静的検出
  (testing "detects direct FFI calls"
    (let ((result (analyze-ffi-usage '(write-char #\A))))
      (ok (member '%host-write-char (ffi-analysis-used-ffis result)))))

  ;; 動的検出
  (testing "detects dynamic funcall"
    (let ((result (analyze-ffi-usage '(funcall (intern "FOO") x))))
      (ok (ffi-analysis-dynamic-p result))))

  ;; 静的funcall
  (testing "quoted symbol is static"
    (let ((result (analyze-ffi-usage '(funcall 'write-char #\A))))
      (ok (not (ffi-analysis-dynamic-p result)))
      (ok (member '%host-write-char (ffi-analysis-used-ffis result))))))
```

### Contract Tests

```lisp
;; インポートセクション生成
(deftest import-section-contract
  (testing "no FFI usage → no imports"
    (let ((wasm (compile-to-wasm '(+ 1 2))))
      (ok (not (has-import-section-p wasm)))))

  (testing "static FFI → only used imports"
    (let ((wasm (compile-to-wasm '(sin 1.0))))
      (ok (has-import-p wasm "clysm:math" "sin"))
      (ok (not (has-import-p wasm "clysm:io" "write-char")))))

  (testing "dynamic call → includes dynamic-call import"
    (let ((wasm (compile-to-wasm '(funcall (intern "FOO") x))))
      (ok (has-import-p wasm "clysm:runtime" "dynamic-call")))))
```

### Integration Tests

```lisp
;; wasmtime 直接実行
(deftest wasmtime-execution
  (testing "simple arithmetic runs standalone"
    (ok (= 3 (compile-and-run '(+ 1 2)))))

  (testing "static FFI with host-shim"
    (ok (approx= 0.0 (compile-and-run-with-shim '(sin 0.0)))))

  (testing "dynamic call with runtime"
    (ok (= 42 (compile-and-run-with-runtime
               '(funcall (intern "IDENTITY") 42))))))
```

## Success Criteria

| ID | Criterion | Target |
|----|-----------|--------|
| SC-001 | `(+ 1 2)` が wasmtime 単体で実行可能 | エラーなし |
| SC-002 | 静的FFI使用時、使用FFIのみインポート | 未使用FFIなし |
| SC-003 | 動的呼び出し検出が正確 | False positive なし |
| SC-004 | 動的呼び出しがホスト経由で動作 | 正しい結果 |
| SC-005 | コンパイル時間オーバーヘッド | ≤15%増加 |

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2025-12-31 | 2層アーキテクチャ採用 | 静的呼び出しの性能維持と動的呼び出しの柔軟性を両立 |
| 2025-12-31 | `$dynamic-call` 単一インポート | インポート数を最小化しつつ任意の動的呼び出しに対応 |
| 2025-12-31 | `:ffi-mode` オプション | ユースケースに応じた最適化を可能に |

## References

- Feature 022: `specs/022-wasm-import-optimization/`
- FFI実装: `src/clysm/ffi/`
- I/O分析器: `src/clysm/compiler/analyzer/io-usage.lisp`
- ホストシム: `host-shim/`
