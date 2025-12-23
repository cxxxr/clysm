# Implementation Plan: Tagbody/Go

**Branch**: `004-tagbody-go` | **Date**: 2025-12-23 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/004-tagbody-go/spec.md`

## Summary

tagbody/go の完全実装。Wasm の制御フロー制約（block は前方ジャンプのみ、loop は後方ジャンプのみ）に対して、2つのコンパイル戦略を使い分ける：
1. **Simple Loop**: 単一タグへの後方ジャンプのみ → Wasm `loop` で直接実装
2. **Dispatch Loop**: 複雑なジャンプパターン → `loop` + `br_table` によるディスパッチ

## Technical Context

**Language/Version**: Common Lisp (SBCL 2.4+)
**Primary Dependencies**: alexandria, babel, trivial-gray-streams, rove (testing)
**Storage**: N/A (compiler generates Wasm binaries)
**Testing**: rove
**Target Platform**: WebAssembly with GC (wasmtime)
**Project Type**: single
**Performance Goals**: tagbody/go のオーバーヘッドは最小限に
**Constraints**: Wasm 制御フロー制約（block/loop の方向性）

## Constitution Check

- [x] TDD: テストファーストで実装
- [x] Phase 3 制御フロー: tagbody/go は implementation-plan.md の Phase 3 に該当
- [x] WasmGC 準拠: br_table は標準 Wasm 命令

## Project Structure

### Documentation (this feature)

```text
specs/004-tagbody-go/
├── spec.md              # 機能仕様
├── plan.md              # This file
└── tasks.md             # 実装タスク（後で作成）
```

### Source Code (repository root)

```text
src/clysm/compiler/codegen/
└── func-section.lisp    # compile-tagbody, compile-go 修正

tests/
├── unit/
│   └── tagbody-test.lisp        # 新規作成
└── integration/
    └── control-flow-test.lisp   # tagbody/go テスト追加
```

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

## Solution: Two Compilation Strategies

### Strategy 1: Simple Loop (単一タグ、後方ジャンプのみ)

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

---

### Strategy 2: Dispatch Loop (複雑なジャンプパターン)

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

**Example Compilation:**

入力:
```lisp
(tagbody
B
  (go A)
  (f)
A
  (go B))
```

AST segments:
```
segments = ((B . ((go A) (f)))    ;; index 0
            (A . ((go B))))       ;; index 1
```

Wasm 出力:
```wat
(local $pc i32)
(local.set $pc (i32.const 0))

(block $exit
  (loop $dispatch
    (block $seg_A                  ;; br depth 1 → index 1
      (block $seg_B                ;; br depth 2 → index 0
        (br_table 2 1 0 (local.get $pc))
        ;; pc=0 → br 2 → $seg_B end → seg 0 開始
        ;; pc=1 → br 1 → $seg_A end → seg 1 開始
        ;; pc>=2 → br 0 → $exit
      ) ;; $seg_B ends

      ;; Segment 0 (B) のコード:
      (local.set $pc (i32.const 1))  ;; go A → pc=1
      (br 2)                          ;; → $dispatch
      ;; (f) は unreachable（go後なので生成不要）

    ) ;; $seg_A ends

    ;; Segment 1 (A) のコード:
    (local.set $pc (i32.const 0))    ;; go B → pc=0
    (br 1)                            ;; → $dispatch

    (br 0)                            ;; → $exit
  )
)
(ref.null none)
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

## Implementation Tasks

### Phase 1: Data Structures

#### T1.1: tagbody-context 構造体定義

```lisp
(defstruct tagbody-context
  "Context for compiling tagbody/go."
  (strategy nil :type keyword)        ; :sequential, :simple-loop, :dispatch
  (tags nil :type list)               ; ((tag . segment-index) ...)
  (pc-local nil :type (or null fixnum)) ; $pc のローカル変数インデックス
  (loop-depth nil :type (or null fixnum)) ; loop $dispatch への br 深度
  (tag-symbol nil :type (or null symbol))) ; simple-loop 用の単一タグ
```

#### T1.2: compilation-env 拡張

```lisp
(defstruct (compilation-env ...)
  ...
  (tagbody-context nil :type (or null tagbody-context)))
```

#### T1.3: copy-compilation-env 更新

tagbody-context を適切にコピー（ネスト対応）

---

### Phase 2: Strategy Analysis

#### T2.1: collect-go-targets 実装

```lisp
(defun collect-go-targets (segments)
  "Collect all go target tags from segments."
  ...)
```

#### T2.2: all-goes-are-backward-p 実装

```lisp
(defun all-goes-are-backward-p (segments tag)
  "Check if all goes to TAG are backward jumps."
  ...)
```

#### T2.3: analyze-tagbody-strategy 実装

戦略判定のメイン関数。

---

### Phase 3: Sequential Strategy

#### T3.1: compile-tagbody-sequential 実装

go がない場合の単純な逐次実行。

```lisp
(defun compile-tagbody-sequential (segments env)
  "Compile tagbody without go as sequential execution."
  ...)
```

---

### Phase 4: Simple Loop Strategy

#### T4.1: compile-tagbody-simple-loop 実装

単一タグへの後方ジャンプのみの最適化パターン。

```lisp
(defun compile-tagbody-simple-loop (segments env)
  "Compile tagbody with single backward-jump tag as simple loop."
  ...)
```

#### T4.2: compile-go-simple 実装

simple-loop 戦略用の go コンパイル。

```lisp
(defun compile-go-simple (tag env)
  "Compile go for simple loop pattern."
  ...)
```

---

### Phase 5: Dispatch Strategy

#### T5.1: compile-tagbody-dispatch 実装

複雑なジャンプパターン用のディスパッチループ。

```lisp
(defun compile-tagbody-dispatch (segments env)
  "Compile tagbody using dispatch loop pattern."
  ...)
```

#### T5.2: compile-tagbody-segment 実装

各セグメントのコンパイル（go 後のデッドコード処理含む）。

```lisp
(defun compile-tagbody-segment (forms env)
  "Compile segment forms, stopping after go."
  ...)
```

#### T5.3: compile-go-dispatch 実装

dispatch 戦略用の go コンパイル。

```lisp
(defun compile-go-dispatch (tag env)
  "Compile go for dispatch pattern."
  ...)
```

---

### Phase 6: Main Entry Point

#### T6.1: compile-tagbody リファクタリング

戦略に応じてディスパッチする。

```lisp
(defun compile-tagbody (ast env)
  "Compile tagbody - dispatches to appropriate strategy."
  (let* ((segments (clysm/compiler/ast:ast-tagbody-segments ast))
         (strategy (analyze-tagbody-strategy segments)))
    (ecase strategy
      (:sequential (compile-tagbody-sequential segments env))
      (:simple-loop (compile-tagbody-simple-loop segments env))
      (:dispatch (compile-tagbody-dispatch segments env)))))
```

#### T6.2: compile-go リファクタリング

コンテキストに応じて適切な go コンパイルを呼び出す。

```lisp
(defun compile-go (ast env)
  "Compile go based on enclosing tagbody strategy."
  (let ((context (cenv-tagbody-context env)))
    (unless context
      (error "go: not inside a tagbody"))
    (ecase (tagbody-context-strategy context)
      (:simple-loop (compile-go-simple (ast-go-tag ast) env))
      (:dispatch (compile-go-dispatch (ast-go-tag ast) env)))))
```

---

### Phase 7: Tests

#### T7.1: Unit Tests (tests/unit/tagbody-test.lisp)

- 戦略判定のテスト
- AST パースのテスト

#### T7.2: Integration Tests (tests/integration/control-flow-test.lisp)

```lisp
;; TC1: 単純後方ジャンプ
(deftest tagbody-simple-loop
  (ok (= 5 (run-wasm
            '(let ((x 0))
               (tagbody
                LOOP
                  (setq x (+ x 1))
                  (if (< x 5) (go LOOP)))
               x)))))

;; TC2: 10000回ループ（スタックオーバーフロー確認）
(deftest tagbody-deep-loop
  (ok (= 10000 (run-wasm
                '(let ((x 0))
                   (tagbody
                    LOOP
                      (setq x (+ x 1))
                      (if (< x 10000) (go LOOP)))
                   x)))))

;; TC3: 前方ジャンプ
(deftest tagbody-forward-jump
  (ok (= 1 (run-wasm
            '(let ((x 0))
               (tagbody
                  (go SKIP)
                  (setq x 999)
                SKIP
                  (setq x 1))
               x)))))

;; TC4: 複合ジャンプ
(deftest tagbody-complex-jump
  (ok (= 111 (run-wasm
              '(let ((x 0))
                 (tagbody
                  B (setq x (+ x 1))
                    (go A)
                    (setq x 999)
                  A (setq x (+ x 10))
                    (if (< x 100) (go B)))
                 x)))))

;; TC5: フォールスルー
(deftest tagbody-fallthrough
  (ok (= 111 (run-wasm
              '(let ((x 0))
                 (tagbody
                  A (setq x 1)
                  B (setq x (+ x 10))
                  C (setq x (+ x 100)))
                 x)))))

;; TC6: タグなし（go なし）
(deftest tagbody-no-tags
  (ok (= 3 (run-wasm
            '(let ((x 0))
               (tagbody
                  (setq x 1)
                  (setq x (+ x 2)))
               x)))))
```

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

## Implementation Order

```
Phase 1: Data Structures
  └─ T1.1 → T1.2 → T1.3

Phase 2: Strategy Analysis
  └─ T2.1 → T2.2 → T2.3

Phase 3: Sequential Strategy (最も単純)
  └─ T3.1

Phase 4: Simple Loop Strategy (loop マクロのため優先)
  └─ T4.1 → T4.2

Phase 5: Dispatch Strategy (最も複雑)
  └─ T5.1 → T5.2 → T5.3

Phase 6: Entry Point Integration
  └─ T6.1 → T6.2

Phase 7: Tests
  └─ T7.1 → T7.2 (各 Phase と並行して TDD)
```

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
;; func-section.lisp:1356-1387 (現在の実装)
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
