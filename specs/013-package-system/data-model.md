# Data Model: ANSI Common Lispパッケージシステム

**Date**: 2025-12-24
**Feature**: 013-package-system

## Entities

### 1. Package

パッケージはシンボルの名前空間を提供するオブジェクト。

```lisp
;; Package 構造（plist表現）
(:name           string            ; 正式名（例: "COMMON-LISP"）
 :nicknames      (list string)     ; ニックネーム（例: ("CL")）
 :internal-symbols hash-table      ; 内部シンボル（string → symbol）
 :external-symbols hash-table      ; 外部シンボル（string → symbol）
 :use-list       (list package)    ; useしているパッケージ
 :used-by-list   (list package)    ; useされているパッケージ
 :shadowing-symbols (list symbol)) ; シャドウイングシンボル
```

**属性**:

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| name | string | パッケージの正式名 | 非空、大文字正規化 |
| nicknames | list of string | 別名リスト | 各名前はユニーク |
| internal-symbols | hash-table | 内部シンボルテーブル | キー: 大文字化シンボル名 |
| external-symbols | hash-table | 外部シンボルテーブル | キー: 大文字化シンボル名 |
| use-list | list of package | 継承元パッケージ | 循環参照可能 |
| used-by-list | list of package | 継承先パッケージ | 自動更新 |
| shadowing-symbols | list of symbol | 名前衝突解決用シンボル | |

**バリデーションルール**:
- パッケージ名は空文字列不可
- 同名パッケージの重複登録不可
- ニックネームは他パッケージの名前/ニックネームと重複不可

### 2. Symbol（拡張）

既存のシンボル構造に `home-package` 参照を追加。

```lisp
;; Symbol 構造（概念モデル）
(name          string            ; シンボル名（例: "CAR"）
 home-package  (or package null) ; ホームパッケージ（未インターン時はnil）
 value         any               ; 変数としての値
 function      any               ; 関数としての値（Lisp-2の場合）
 plist         list)             ; プロパティリスト
```

**属性**:

| Field | Type | Description | Constraints |
|-------|------|-------------|-------------|
| name | string | シンボル名 | 大文字正規化（標準readtable） |
| home-package | package or nil | シンボルの所有パッケージ | unintern後はnil |

**シンボル可視性状態**:
- `:internal` - パッケージの内部シンボル
- `:external` - パッケージの外部シンボル（エクスポート済み）
- `:inherited` - use-list経由でアクセス可能
- `nil` - パッケージに存在しない

### 3. Qualified Symbol Token（新規）

リーダーがパッケージ修飾子付きシンボルを表現するトークン。

```lisp
;; トークン形式
(:qualified-symbol
 :package-name string  ; パッケージ名（例: "CL"）
 :symbol-name  string  ; シンボル名（例: "CAR"）
 :external     boolean ; t=外部参照(:)、nil=内部参照(::)
 :line         integer ; ソース行番号
 :column       integer) ; ソース列番号
```

### 4. Standard Packages

システム起動時に存在する標準パッケージ。

| Package Name | Nicknames | Initial Use-List | Notes |
|--------------|-----------|------------------|-------|
| COMMON-LISP | (CL) | () | CL仕様シンボルをエクスポート |
| COMMON-LISP-USER | (CL-USER) | (COMMON-LISP) | 初期*package* |
| KEYWORD | () | () | 全シンボル自動エクスポート |

## Relationships

```
┌──────────────┐         ┌──────────────┐
│   Package    │◄────────│    Symbol    │
│              │ home-   │              │
│ internal-    │ package │ name         │
│ symbols ────►├────────►│              │
│ external-    │         └──────────────┘
│ symbols ────►│
│              │
│ use-list ───►├─────┐
│ used-by-list │◄────┤ (相互参照)
│              │     │
└──────────────┘     │
       ▲             │
       └─────────────┘
```

**関係性**:
1. **Package → Symbol** (1:N): パッケージは複数のシンボルを所有
2. **Symbol → Package** (N:1): シンボルは1つのホームパッケージを持つ（または未インターン）
3. **Package ↔ Package** (M:N): use-list/used-by-listで相互参照

## State Transitions

### Package Lifecycle

```
[Not Exists] ─────make-package────► [Active] ─────delete-package────► [Deleted]
                                        │
                                        │ rename-package
                                        ▼
                                    [Renamed]
                                    (still Active)
```

### Symbol Visibility Transitions

```
[Not Interned] ──intern──► [Internal] ──export──► [External]
                               │                      │
                               │ unintern             │ unexport
                               ▼                      ▼
                          [Uninterned]           [Internal]
```

### Package-Symbol Membership

```
[Not Present]
     │
     ├──── intern ────► [Present as Internal]
     │                         │
     │                         ├── export ──► [Present as External]
     │                         │                    │
     │                         │                    ├── unexport ──► [Present as Internal]
     │                         │                    │
     │                         │                    └── unintern ──► [Not Present]
     │                         │
     │                         └── unintern ──► [Not Present]
     │
     ├──── import ────► [Present as Internal] (home-package unchanged)
     │
     └──── use-package ──► [Accessible as Inherited] (not directly present)
                                │
                                └── unuse-package ──► [Not Accessible]
```

## Data Volume Assumptions

- パッケージ数: 10〜100（典型的なアプリケーション）
- CLパッケージのシンボル数: 978（ANSI CL仕様）、実装済みサブセット
- ユーザーパッケージのシンボル数: 10〜10,000（パッケージ当たり）
- シンボル検索: O(1)（ハッシュテーブル）
- use-list検索: O(n)（n = use-listの長さ、通常 < 10）
