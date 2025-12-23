# Implementation Plan: Tagbody/Go

**Branch**: `004-tagbody-go` | **Date**: 2025-12-23 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/004-tagbody-go/spec.md`

## Summary

tagbody/go の完全実装。Wasm の制御フロー制約（block は前方ジャンプのみ、loop は後方ジャンプのみ）に対して、3つのコンパイル戦略を使い分ける：
1. **Sequential**: go なし → 単純な逐次実行
2. **Simple Loop**: 単一タグへの後方ジャンプのみ → Wasm `loop` で直接実装
3. **Dispatch Loop**: 複雑なジャンプパターン → `loop` + `br_table` によるディスパッチ

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compiler generates Wasm binaries)
**Testing**: rove
**Target Platform**: WebAssembly with GC (wasmtime)
**Project Type**: single
**Performance Goals**: tagbody/go のオーバーヘッドは最小限に（Simple Loop 戦略では br_table 不要）
**Constraints**: Wasm 制御フロー制約（block は前方ジャンプのみ、loop は後方ジャンプのみ）
**Scale/Scope**: 10,000回以上のループでもスタックオーバーフローなし

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. WasmGC-First型システム設計 | ✅ Pass | tagbody は NIL を返す（ref.null none）、型システムに変更なし |
| II. Lispオブジェクト表現規約 | ✅ Pass | tagbody は NIL（シングルトン構造体）を返す仕様 |
| III. 関数・クロージャ実装戦略 | ✅ N/A | tagbody/go は関数定義ではなく制御フロー |
| IV. Wasm制御フロー活用 | ✅ Pass | block/loop/br_table は標準 Wasm 命令、制御フロー制約を正しく活用 |
| V. シャローバインディング | ✅ N/A | tagbody/go はスペシャル変数と独立 |
| VI. 段階的動的コンパイル | ✅ N/A | コンパイル時機能 |
| VII. テスト駆動開発（TDD） | ✅ Required | テストファーストで各戦略を実装 |
| VIII. Nix-Firstワークフロー | ✅ Pass | 既存の flake.nix を使用 |

**Gate Result**: ✅ PASS - すべての関連原則に準拠

## Project Structure

### Documentation (this feature)

```text
specs/004-tagbody-go/
├── spec.md              # 機能仕様
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
└── func-section.lisp    # compile-tagbody, compile-go 修正

tests/
├── unit/
│   └── tagbody-test.lisp        # 新規作成：戦略判定テスト
└── integration/
    └── control-flow-test.lisp   # tagbody/go テスト追加
```

**Structure Decision**: Single project structure。既存の `src/clysm/compiler/codegen/func-section.lisp` を修正し、テストは `tests/` 配下に配置。

---

## Problem Analysis

### Wasm Control Flow Constraints

```
┌─────────────────────────────────────────────────────┐
│ Wasm block: 前方ジャンプのみ（ブロック終端へ）      │
│ Wasm loop:  後方ジャンプのみ（ループ先頭へ）        │
│                                                     │
│ tagbody/go: 任意のタグへの前方・後方ジャンプが必要  │
└─────────────────────────────────────────────────────┘
```

### Example Problem Case

```lisp
(tagbody
B
  (go A)      ;; 前方ジャンプ
  (f)
A
  (go B))     ;; 後方ジャンプ
```

このコードは単純な `block` や `loop` のネストでは表現不可能。

---

## Solution: Three Compilation Strategies

### Strategy 1: Sequential (go なし)

**対象パターン:**
```lisp
(tagbody
  (setq x 1)
 A
  (setq x 2)
 B
  (setq x 3))
```

**Wasm 出力:**
```wat
;; (setq x 1)
;; (setq x 2)
;; (setq x 3)
(ref.null none)  ;; NIL
```

### Strategy 2: Simple Loop (単一タグ、後方ジャンプのみ)

**対象パターン:**
```lisp
(tagbody
 LOOP
   (do-something)
   (when condition (go LOOP)))
```

**Wasm 出力:**
```wat
(loop $LOOP
  ;; (do-something)
  (drop)
  ;; (when condition (go LOOP))
  ...
  (br $LOOP)  ;; go LOOP
  ...
)
(ref.null none)  ;; NIL
```

**メリット:**
- $pc 変数不要
- br_table 不要
- 最も効率的
- `loop`/`do`/`dolist`/`dotimes` マクロの内部実装に最適

### Strategy 3: Dispatch Loop (複雑なジャンプパターン)

**対象パターン:**
- 複数タグ
- 前方ジャンプあり
- 前方・後方混在

**Wasm 出力パターン:**
```wat
(local $pc i32)
(local.set $pc (i32.const 0))     ;; 開始インデックス

(block $exit
  (loop $dispatch
    ;; ネストした block（セグメント数分、逆順）
    (block $seg_N-1
      (block $seg_N-2
        ...
          (block $seg_0
            (br_table $seg_0 $seg_1 ... $seg_N-1 $exit (local.get $pc))
          ) ;; $seg_0 ends → セグメント0 開始
          ;; セグメント0 のコード
          ;; (go TAG) → (local.set $pc INDEX) (br $dispatch)
          ;; フォールスルー↓
        ...
      ) ;; $seg_N-2 ends
      ;; セグメントN-2 のコード
    ) ;; $seg_N-1 ends
    ;; セグメントN-1 のコード
    (br $exit)  ;; 全セグメント完了
  )
)
(ref.null none)  ;; NIL
```

---

## Strategy Selection Algorithm

```lisp
(defun analyze-tagbody-strategy (segments)
  "Determine compilation strategy for tagbody."
  (let* ((tags (remove nil (mapcar #'car segments)))
         (go-targets (collect-go-targets segments)))
    (cond
      ;; go がない → 単純な逐次実行
      ((null go-targets) :sequential)

      ;; 単一タグへの後方ジャンプのみ
      ((and (= (length tags) 1)
            (= (length go-targets) 1)
            (eq (first tags) (first go-targets))
            (all-goes-are-backward-p segments (first tags)))
       :simple-loop)

      ;; それ以外 → dispatch パターン
      (t :dispatch))))
```

**判定マトリクス:**

| 条件 | 戦略 |
|------|------|
| go なし | :sequential |
| 単一タグ、後方 go のみ | :simple-loop |
| 単一タグ、前方 go あり | :dispatch |
| 複数タグ | :dispatch |

---

## br Depth Calculation

### Dispatch Pattern Depth Table

```
構造:
  (block $exit           ;; depth +1 (from loop)
    (loop $dispatch      ;; target for go
      (block $seg_N-1    ;; depth +1
        (block $seg_N-2  ;; depth +1
          ...
          (block $seg_0  ;; depth +1
            br_table
          )
          ;; seg_0 code
        )
        ;; seg_N-2 code
      )
      ;; seg_N-1 code
    )
  )

位置から $dispatch への br 深度:
  - br_table 直後:     num_segments + 1
  - segment 0 code:    num_segments
  - segment 1 code:    num_segments - 1
  ...
  - segment N-1 code:  1
```

**計算式:**
```lisp
dispatch-depth = (- num-segments segment-index)
```

---

## Risk Analysis

| リスク | 影響度 | 対策 |
|--------|--------|------|
| br_table のインデックス計算ミス | 高 | テストケースで全パターン検証 |
| デッドコード処理漏れ | 中 | go 後は unreachable または生成停止 |
| ネストした tagbody の深度計算 | 中 | コンテキストのスタック管理 |
| block/return-from との相互作用 | 中 | block-depth と tagbody-context の分離 |

---

## Appendix: AST Structure Reference

```lisp
;; ast.lisp より
(defstruct (ast-tagbody (:include ast-node) (:conc-name ast-tagbody-))
  "Tagbody node for goto-based control flow."
  (tags nil :type list)      ; List of tag symbols in order
  (segments nil :type list)) ; ((tag . (forms...)) ...) where tag is symbol or nil

(defstruct (ast-go (:include ast-node) (:conc-name ast-go-))
  "Go node for transfer to a tag."
  (tag nil :type symbol))
```

## Appendix: Current Implementation Reference

```lisp
;; func-section.lisp (現在の実装)
(defun compile-tagbody (ast env)
  "Compile tagbody.
   MVP implementation: just compile all forms sequentially.
   Go support requires loop/dispatch mechanism."
  ...)

(defun compile-go (ast env)
  "Compile go.
   MVP: Not fully implemented - signals error."
  (declare (ignore env))
  (let ((tag (clysm/compiler/ast:ast-go-tag ast)))
    (error "go is not yet implemented (tag: ~A). Tagbody without go works." tag)))
```
