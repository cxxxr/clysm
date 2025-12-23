# Data Model: Sequence Functions

**Feature**: 007-sequence-functions
**Date**: 2025-12-23

## Overview

シーケンス関数は既存のデータモデル（cons cell, closure, symbol）を活用する。
新規エンティティの追加は不要。

---

## Existing Entities (再利用)

### Cons Cell

```wat
(type $cons (struct
  (field $car (mut anyref))
  (field $cdr (mut anyref))))
```

| Field | Type | Description |
|-------|------|-------------|
| $car | mut anyref | リスト要素またはキー（alist） |
| $cdr | mut anyref | 次のcons cellまたはnil |

**使用関数**: 全シーケンス関数

### Closure

```wat
(type $closure (struct
  (field $code_0 (ref null $func_0))
  (field $code_1 (ref null $func_1))
  (field $code_2 (ref null $func_2))
  (field $code_N (ref null $func_N))
  (field $env (mut anyref))))
```

**使用関数**: mapcar, mapc, maplist, reduce, find-if, remove-if, count-if, every, some

### NIL Singleton

```wat
(type $nil (struct))
(global $nil (ref $nil) (struct.new $nil))
```

**役割**: リスト終端、空リスト、偽値

---

## Function Signatures

### Tier 1: 基本シーケンス関数

| Function | Signature | Return Type |
|----------|-----------|-------------|
| length | (list) → i32 | fixnum |
| append | (list list) → list | list |
| reverse | (list) → list | list |
| nreverse | (list) → list | list (mutated) |
| last | (list &optional n) → list | cons or nil |
| butlast | (list &optional n) → list | list |
| copy-list | (list) → list | list |

### Tier 2: 高階シーケンス関数

| Function | Signature | Return Type |
|----------|-----------|-------------|
| mapcar | (fn list) → list | list |
| mapc | (fn list) → list | original list |
| maplist | (fn list) → list | list |
| reduce | (fn list &key initial-value) → any | any |

### Tier 3: 探索・フィルタ関数

| Function | Signature | Return Type |
|----------|-----------|-------------|
| find | (item list) → any | element or nil |
| find-if | (pred list) → any | element or nil |
| position | (item list) → i32 or nil | fixnum or nil |
| position-if | (pred list) → i32 or nil | fixnum or nil |
| remove | (item list) → list | list |
| remove-if | (pred list) → list | list |
| remove-if-not | (pred list) → list | list |
| count | (item list) → i32 | fixnum |
| count-if | (pred list) → i32 | fixnum |

### Tier 4: その他（オプション）

| Function | Signature | Return Type |
|----------|-----------|-------------|
| member | (item list) → list | tail or nil |
| assoc | (key alist) → cons | pair or nil |
| rassoc | (value alist) → cons | pair or nil |
| subst | (new old tree) → tree | tree |
| every | (pred list) → bool | t or nil |
| some | (pred list) → any | first true or nil |
| notany | (pred list) → bool | t or nil |
| notevery | (pred list) → bool | t or nil |

---

## State Transitions

### 非破壊的操作

```
Input List: (1 2 3)
     ↓
Operation: reverse
     ↓
Output: (3 2 1)  ← 新規リスト
Input:  (1 2 3)  ← 不変
```

### 破壊的操作

```
Input List: (1 2 3)
     ↓
Operation: nreverse
     ↓
Output: (3 2 1)  ← 同じcons cells、リンク変更
Input:  (1)      ← 元の先頭は末尾に
```

---

## Validation Rules

### 型制約

| 引数位置 | 期待型 | 検証 |
|---------|--------|------|
| list引数 | list (cons or nil) | Phase 8c以降 |
| fn引数 | function (closure) | Phase 8c以降 |
| pred引数 | function (closure) | Phase 8c以降 |
| item引数 | any | N/A |

### 値制約

| 関数 | 制約 | 違反時動作 |
|------|------|-----------|
| reduce (no :initial-value) | list非空 | 未定義（後続対応） |
| last/butlast n引数 | 非負整数 | 未定義 |

---

## Relationships

```
List ←──contains──→ Element (any)
  │
  └── terminated-by ──→ NIL

Closure ←──applied-to──→ Element
  │
  └── returns ──→ Result

Association List (alist)
  │
  ├── key ──→ car of cons
  └── value ──→ cdr of cons (or cadr for ((k v) ...))
```

---

## WasmGC Type Mapping Summary

| Lisp Type | WasmGC Type | Notes |
|-----------|-------------|-------|
| List | (ref null $cons) or $nil | anyref compatible |
| Fixnum | (ref i31) | 31-bit signed integer |
| Function | (ref $closure) | Multi-arity dispatch |
| Symbol | (ref $symbol) | For :test, :key keywords |
| NIL | (ref $nil) | Singleton |
| T | (ref $t) | Singleton |
