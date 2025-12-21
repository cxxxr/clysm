# Research: Clysm - WebAssembly GC Common Lisp Compiler

**Date**: 2025-12-21
**Status**: Complete
**Plan Reference**: [plan.md](./plan.md)

## Overview

Phase 0で解決すべき技術的不確実性と調査結果のドキュメント。

---

## 1. WasmGC Proposal Status

### Decision
wasmtime v27+ をリファレンス実装として採用。WasmGC (GC Proposal) はPhase 4 (Final)。

### Rationale
- wasmtimeは2024年後半にWasmGC完全サポートを達成
- Chrome/Firefox/Safariも2024年中にサポート開始
- 仕様の安定性が確認されている

### Alternatives Considered
- **V8直接使用**: Node.js依存が増加、Nix統合が複雑化
- **Binaryen**: WasmGCサポートが不完全（却下）
- **wasm3**: GCサポートなし（却下）

### Key Findings

WasmGC主要命令（wasmtime v27+で確認済み）:

| 命令 | 用途 | 検証状態 |
|------|------|----------|
| `struct.new` | 構造体生成 | OK |
| `struct.get` / `struct.set` | フィールドアクセス | OK |
| `array.new` | 配列生成 | OK |
| `ref.i31` / `i31.get_s` | i31ref操作 | OK |
| `ref.cast` / `ref.test` | 型チェック・キャスト | OK |
| `ref.eq` | 参照等価性 | OK |

---

## 2. Tail Call Proposal Status

### Decision
`return_call` / `return_call_ref` を採用。wasmtime v27+でサポート確認。

### Rationale
- Tail Call Proposalは2023年にPhase 4到達
- 主要ブラウザでサポート済み
- Common Lispの末尾再帰最適化に必須

### Implementation Notes

```wat
;; 末尾呼び出し例
(func $fact-iter (param $n i32) (param $acc i32) (result i32)
  (if (i32.eqz (local.get $n))
    (then (return (local.get $acc)))
    (else
      (return_call $fact-iter
        (i32.sub (local.get $n) (i32.const 1))
        (i32.mul (local.get $n) (local.get $acc))))))
```

### Verification Command
```bash
wasmtime --wasm tail-call test.wasm
```

---

## 3. Exception Handling Proposal Status

### Decision
`try_table` / `throw` を採用（EH Proposal Phase 4）。

### Rationale
- 2024年にPhase 4到達
- `block/return-from`、`catch/throw`、`unwind-protect`に必須
- wasmtime v27+でサポート

### Implementation Pattern

```wat
;; block/return-from の変換パターン
(tag $block_exit (param anyref))

(block $handler (result anyref)
  (try_table (catch $block_exit $handler)
    ;; block本体
    (throw $block_exit (ref.i31 (i32.const 42)))
  )
)

;; unwind-protect の変換パターン
(try_table (catch_all $cleanup)
  ;; protected-form
)
$cleanup
  ;; cleanup-form
  (rethrow 0)
```

### Alternatives Considered
- **トランポリン方式**: パフォーマンス劣化（却下）
- **setjmp/longjmp模倣**: 線形メモリ依存（憲法違反）

---

## 4. LEB128 Encoding

### Decision
Common Lisp実装で符号付き/符号なしLEB128エンコーダを自作。

### Rationale
- Wasmバイナリの基本エンコーディング
- 外部ライブラリ依存を避ける
- テスト容易性

### Implementation Reference

```lisp
(defun encode-unsigned-leb128 (value)
  "Encode unsigned integer to LEB128 byte vector."
  (let ((bytes '()))
    (loop
      (let ((byte (logand value #x7f)))
        (setf value (ash value -7))
        (if (zerop value)
            (progn (push byte bytes) (return))
            (push (logior byte #x80) bytes))))
    (coerce (nreverse bytes) '(vector (unsigned-byte 8)))))

(defun encode-signed-leb128 (value)
  "Encode signed integer to LEB128 byte vector."
  (let ((bytes '())
        (negative (< value 0)))
    (loop
      (let* ((byte (logand value #x7f))
             (value (ash value -7))
             (sign-bit (logand byte #x40)))
        (if (or (and (zerop value) (zerop sign-bit))
                (and (= value -1) (not (zerop sign-bit))))
            (progn (push byte bytes) (return))
            (push (logior byte #x80) bytes))
        (setf value value)))
    (coerce (nreverse bytes) '(vector (unsigned-byte 8)))))
```

### Test Vectors

| Value | Unsigned LEB128 | Signed LEB128 |
|-------|-----------------|---------------|
| 0 | `#(0)` | `#(0)` |
| 127 | `#(127)` | `#(255 0)` |
| 128 | `#(128 1)` | `#(128 1)` |
| -1 | N/A | `#(127)` |
| -64 | N/A | `#(64)` |
| -65 | N/A | `#(191 127)` |

---

## 5. Wasm Binary Format

### Decision
セクションID順序（0-13）を厳守した直接バイナリ生成。

### Section Order (Mandatory)

| ID | Section | Description |
|----|---------|-------------|
| 0 | Custom | カスタムセクション（名前等） |
| 1 | Type | 関数型定義 |
| 2 | Import | インポート宣言 |
| 3 | Function | 関数インデックス→型インデックス |
| 4 | Table | テーブル定義 |
| 5 | Memory | メモリ定義 |
| 6 | Global | グローバル変数 |
| 7 | Export | エクスポート宣言 |
| 8 | Start | スタート関数 |
| 9 | Element | テーブル初期化 |
| 10 | Code | 関数コード本体 |
| 11 | Data | データセグメント |
| 12 | DataCount | データセグメント数 |
| 13 | Tag | 例外タグ（EH Proposal） |

### Module Header

```
0x00 0x61 0x73 0x6d  ; Magic: "\0asm"
0x01 0x00 0x00 0x00  ; Version: 1
```

### Validation Command

```bash
wasm-tools validate output.wasm
wasm-tools print output.wasm  # WAT変換確認
```

---

## 6. Nix Flakes Best Practices

### Decision
devShellにSBCL, wasm-tools, wasmtimeを含むflake.nix構成。

### Rationale
- 憲法VIII準拠（Nix-First）
- 再現可能ビルドの保証
- CI/ローカル環境の統一

### Recommended flake.nix Structure

```nix
{
  description = "Clysm - WebAssembly GC Common Lisp Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sbcl
            wasmtime
            wasm-tools
            # wat2wasm is part of wabt
            wabt
          ];
        };

        checks.default = pkgs.runCommand "clysm-check" {
          buildInputs = [ pkgs.sbcl ];
        } ''
          cd ${self}
          sbcl --load run-tests.lisp
          touch $out
        '';
      });
}
```

### Required Tools Version

| Tool | Minimum Version | Reason |
|------|-----------------|--------|
| SBCL | 2.4+ | Unicode, thread stability |
| wasmtime | 27+ | WasmGC complete support |
| wasm-tools | 1.0.60+ | GC validation support |
| wabt | 1.0.34+ | wat2wasm with GC |

---

## 7. SBCL + Rove Testing

### Decision
Roveをテストフレームワークとして採用。

### Rationale
- モダンなCommon Lispテストフレームワーク
- 良好なエラーレポート
- ASDF統合が容易

### Setup

```lisp
;; clysm.asd
(defsystem "clysm"
  :depends-on ("alexandria" "babel")
  :components (...)
  :in-order-to ((test-op (test-op "clysm/tests"))))

(defsystem "clysm/tests"
  :depends-on ("clysm" "rove")
  :components ((:module "tests"
                :components ((:file "leb128-test")
                             (:file "sections-test"))))
  :perform (test-op (o c) (symbol-call :rove :run c)))
```

### Test Example

```lisp
(defpackage :clysm/tests/leb128
  (:use :cl :rove :clysm/backend/leb128))
(in-package :clysm/tests/leb128)

(deftest test-unsigned-leb128
  (ok (equalp #(0) (encode-unsigned-leb128 0)))
  (ok (equalp #(127) (encode-unsigned-leb128 127)))
  (ok (equalp #(128 1) (encode-unsigned-leb128 128))))
```

---

## 8. Guile Hoot Reference

### Decision
Guile Hoot（Scheme→Wasm GCコンパイラ）を設計参考とする。

### Rationale
- 同じLisp方言ファミリー
- WasmGCターゲット
- オープンソースで実装詳細を確認可能

### Key Insights from Hoot

1. **クロージャ表現**: 環境をWasm structで表現
2. **Tail Call**: `return_call`命令を積極活用
3. **動的リンク**: モジュール間のGCヒープ共有パターン
4. **継続**: one-shot delimited continuationsの実装

### Reference
- https://gitlab.com/spritely/guile-hoot
- https://spritely.institute/news/guile-hoot-v0.1.0-released.html

---

## 9. Performance Benchmarking Strategy

### Decision
マイクロベンチマーク + 統合ベンチマークの2層構成。

### Benchmark Categories

1. **Fixnum算術**: i31ref操作のオーバーヘッド測定
2. **コンス生成**: GC割り当て速度
3. **関数呼び出し**: call_ref間接呼び出しレイテンシ
4. **クロージャキャプチャ**: 環境構造体生成コスト

### Measurement Tools

```bash
# wasmtime profiling
wasmtime run --profile=perfmap benchmark.wasm

# Chrome DevTools (ブラウザテスト時)
# Performance タブで Wasm 実行プロファイリング
```

### Target Metrics (from Constitution)

| Metric | Target | Measurement |
|--------|--------|-------------|
| Fixnum arithmetic | < 1.5x native i32 | Microbenchmark |
| Cons creation | > 10M cells/sec | GC stress test |
| Function call | < 5ns (indirect) | call_ref benchmark |

---

## 10. Security Considerations

### Decision
憲法セキュリティ制約を全フェーズで遵守。

### Constraints

1. **サンドボックス維持**: Wasmサンドボックス境界を絶対に破らない
2. **線形メモリ禁止**: GCヒープのみ使用、直接メモリアクセスなし
3. **WASI経由アクセス**: 外部リソースはWASIインターフェースのみ
4. **動的コード生成**: `WebAssembly.instantiate()`のみ許可

### Verification

- [ ] 生成Wasmに`memory`セクションがないこと確認
- [ ] WASIインポート以外の外部依存がないこと確認
- [ ] `wasm-tools validate`で型安全性を保証

---

## Summary of Resolved Items

| Item | Status | Decision |
|------|--------|----------|
| WasmGC runtime | Resolved | wasmtime v27+ |
| Tail call support | Resolved | return_call/return_call_ref |
| Exception handling | Resolved | try_table/throw (EH Phase 4) |
| LEB128 encoding | Resolved | Custom CL implementation |
| Binary format | Resolved | Direct generation, section order |
| Build system | Resolved | Nix Flakes + ASDF |
| Testing | Resolved | Rove framework |
| Reference impl | Resolved | Guile Hoot patterns |

---

## Next Steps

1. Phase 0タスク実行開始
2. flake.nix作成 (P0-T1)
3. LEB128実装とテスト (P0-T3)
4. 空モジュール生成検証 (P0-T5)
