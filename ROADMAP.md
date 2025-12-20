# clysm ANSI準拠化 ロードマップ

## 1. プロジェクト概要

### 1.1 目標

ANSI Common Lisp準拠のWebAssemblyコンパイラを作成する。
追加目標: **セルフコンパイル（ブートストラップ）**を達成する。

### 1.2 現状（2025年12月）

| 項目 | 状況 |
|------|------|
| コンパイラコア | 基本構造完成（AST -> IR -> WASM） |
| 実行環境 | WasmGC + Node.js |
| メモリモデル | 線形メモリ上のcons cell（8バイト/cell） |
| テスト基盤 | ANSIテストスイート統合済み（20,000+テスト） |
| ユニットテスト | 216テストパス |
| ブートストラップ | 基盤機能実装中 |

### 1.3 実装済み機能

**Phase 1 完了:**
- 算術: `+`, `-`, `*`, `/`, `mod`, `rem`, `1+`, `1-`
- 比較: `<`, `>`, `<=`, `>=`, `=`, `/=`（多引数対応）
- 論理: `not`, `null`, `zerop`, `plusp`, `minusp`
- ビット: `logand`, `logior`, `logxor`, `ash`
- 制御: `if`, `when`, `unless`, `cond`, `and`, `or`, `case`, `ecase`
- バインディング: `let`, `let*`, `setq`, `setf`
- 関数: `defun`, `lambda`, `funcall`
- ブロック: `block`, `return-from`, `return`
- ループ: `dotimes`, `dolist`, `loop`（基本形）

**Phase 2 完了:**
- リスト: `cons`, `car`, `cdr`, `list`, `list*`, `first`-`fourth`, `nth`, `nthcdr`
- 破壊操作: `rplaca`, `rplacd`
- 述語: `eq`, `eql`, `consp`, `atom`, `listp`, `numberp`, `symbolp`
- リスト操作: `append`, `reverse`, `nreverse`, `member`, `assoc`, `last`, `length`, `butlast`, `copy-list`
- 高階関数: `mapcar`, `mapc`, `reduce`

**Phase 2.5 完了:**
- 数学: `abs`, `max`, `min`, `evenp`, `oddp`, `gcd`, `lcm`
- 丸め: `floor`, `ceiling`, `truncate`, `round`
- 多値: `values`, `multiple-value-bind`
- 分配束縛: `destructuring-bind`
- ローカル関数: `labels`
- タグ: `tagbody`, `go`

**ブートストラップ基盤:**
- 構造体: `defstruct`（コンストラクタ、アクセサ、述語、setf対応）
- グローバル: `defparameter`, `defconstant`, `defvar`
- ハッシュ表: `make-hash-table`, `gethash`, `sethash`, `remhash`, `clrhash`, `hash-table-count`
- エラー: `error`（WASMのunreachableにコンパイル）
- setf: `car`, `cdr`, `first`-`third`, `nth`, `gethash`, 構造体アクセサ
- マクロ: `defmacro`（ホスト展開戦略）、バッククォート
- 文字列: `string=`, `string-downcase`, `string-upcase`, `string-append`, `schar`, `char-code`, `code-char`

---

## 2. フェーズ計画

### Phase 2.5: 基盤強化 [完了]

**目標**: ANSIテスト100パス
**達成**: ユニットテスト200パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| `abs` | 高 | [x] |
| `max`, `min` | 高 | [x] |
| `evenp`, `oddp` | 高 | [x] |
| `numberp`, `integerp` | 中 | [x] |
| 比較関数の多引数対応 (<, >, <=, >=, =, /=) | 高 | [x] |
| `gcd`, `lcm` | 中 | [x] |
| `floor`, `ceiling`, `truncate`, `round` | 中 | [x] |
| `multiple-values`基礎（values, m-v-bind） | 高 | [x] |
| `length`の完全実装 | 中 | [x] |
| `signum` | 低 | [ ] |

---

### Phase Bootstrap: セルフコンパイル [進行中]

**目標**: Clysmコンパイラ自身をコンパイル可能にする

| タスク | 優先度 | 状態 |
|--------|--------|------|
| `defstruct` | 高 | [x] |
| `defparameter`, `defconstant` | 高 | [x] |
| `hash-table`（alist実装） | 高 | [x] |
| `setf`（汎用） | 高 | [x] |
| `error` | 中 | [x] |
| `case`, `ecase` | 中 | [x] |
| `labels` | 中 | [x] |
| `loop`（基本形） | 中 | [x] |
| `destructuring-bind` | 中 | [x] |
| `multiple-value-bind` | 中 | [x] |
| `defmacro`, `macroexpand` | 高 | [x] |
| バッククォート (`, ,, ,@) | 高 | [x] |
| 文字列操作 | 中 | [x] |
| `format` | 中 | [x] |
| CLOS除去（defstruct化） | 高 | [x] |

**マイルストーン**: ミニコンパイラのコンパイル成功 ✓

---

### Phase 3: シンボルとパッケージ基礎

**目標**: ANSIテスト200パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| シンボルのメモリ表現 | 高 | [ ] |
| シンボルテーブル（intern） | 高 | [ ] |
| `symbol-name`, `symbol-value`, `symbol-function` | 高 | [ ] |
| `symbolp` | 中 | [ ] |
| キーワードシンボル | 中 | [ ] |
| 基本パッケージ構造（CL, CL-USER） | 低 | [ ] |

**マイルストーン**: `(eq 'a 'a)` => `T`

---

### Phase 4: 文字と文字列

**目標**: ANSIテスト350パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| characterの値表現 | 高 | [ ] |
| `char-code`, `code-char` | 高 | [ ] |
| stringのメモリ表現（UTF-8） | 高 | [ ] |
| `make-string`, `string` | 高 | [ ] |
| `char`, `schar` | 高 | [ ] |
| `stringp`, `characterp` | 中 | [ ] |
| `string=`, `string<`等 | 中 | [ ] |

**マイルストーン**: `(string= "hello" "hello")` => `T`

---

### Phase 5: 配列とベクタ

**目標**: ANSIテスト500パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| simple-vectorのメモリ表現 | 高 | [ ] |
| `make-array`（1次元） | 高 | [ ] |
| `aref`, `svref` | 高 | [ ] |
| `length`（配列対応） | 高 | [ ] |
| `arrayp`, `vectorp` | 中 | [ ] |
| 多次元配列 | 低 | [ ] |

**マイルストーン**: `(aref (make-array 10) 5)` => `0`

---

### Phase 6: シーケンス操作

**目標**: ANSIテスト1,000パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| `mapcar` | 高 | [x] |
| `mapc`, `maplist`, `mapl` | 高 | [x] (mapc完了) |
| `reduce` | 高 | [x] |
| `find`, `position`, `count` | 中 | [ ] |
| `remove`, `delete` | 中 | [ ] |
| `append`, `nconc` | 中 | [x] (append完了) |
| `reverse`, `nreverse` | 中 | [x] |
| `sort`, `stable-sort` | 低 | [ ] |

**マイルストーン**: `(mapcar #'1+ '(1 2 3))` => `(2 3 4)` ✓

---

### Phase 7: 高度な制御構造

**目標**: ANSIテスト2,000パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| `tagbody`/`go` | 高 | [x] |
| `loop`基本形 | 高 | [x] |
| `loop`拡張（for, collect等） | 中 | [x] (SBCL経由) |
| `catch`/`throw` | 中 | [ ] |
| `unwind-protect` | 中 | [ ] |
| `prog`, `prog*` | 低 | [ ] |

**マイルストーン**: loopを使ったANSIテストがパス ✓

---

### Phase 8: コンディションシステム

**目標**: ANSIテスト5,000パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| condition型階層 | 高 | [ ] |
| `error`, `signal` | 高 | [ ] |
| `handler-case` | 高 | [ ] |
| `handler-bind` | 中 | [ ] |
| `restart-case` | 中 | [ ] |
| `invoke-restart` | 低 | [ ] |

**マイルストーン**: `(handler-case (error "test") (error () :caught))` => `:caught`

---

### Phase 9: 数値の拡張

**目標**: ANSIテスト7,000パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| float（f64使用） | 高 | [ ] |
| 浮動小数点演算 | 高 | [ ] |
| ratio（分数） | 中 | [ ] |
| bignum | 中 | [ ] |
| complex | 低 | [ ] |
| `coerce` | 低 | [ ] |

**マイルストーン**: `(+ 1.5 2.5)` => `4.0`

---

### Phase 10: CLOS基礎

**目標**: ANSIテスト10,000パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| `defstruct`拡張 | 高 | [ ] |
| `defclass` | 高 | [ ] |
| `make-instance` | 高 | [ ] |
| `slot-value` | 高 | [ ] |
| `defgeneric` | 中 | [ ] |
| `defmethod` | 中 | [ ] |
| メソッドディスパッチ | 中 | [ ] |
| method-combination | 低 | [ ] |

---

### Phase 11: I/O基礎

**目標**: ANSIテスト15,000パス

| タスク | 優先度 | 状態 |
|--------|--------|------|
| `write-to-string` | 高 | [ ] |
| `princ`, `prin1`, `print` | 高 | [ ] |
| `format`基本形（~A, ~S, ~D, ~%） | 高 | [ ] |
| `format`拡張 | 中 | [ ] |
| `read-from-string` | 低 | [ ] |
| ストリーム基礎 | 低 | [ ] |

---

## 3. 依存関係

```
Phase 2.5 (基盤強化)
    |
    +---> Phase 3 (シンボル) ---> Phase 4 (文字列) ---> Phase 11 (I/O)
    |           |
    |           +---> Phase 5 (配列) ---> Phase 6 (シーケンス)
    |
    +---> Phase 7 (制御) ---> Phase 8 (コンディション)
    |
    +---> Phase 9 (数値拡張)
    |
    +---> Phase 10 (CLOS) ---> Phase 8 (コンディション)
```

---

## 4. リスク分析

| リスク | 確率 | 影響 | 対策 |
|--------|------|------|------|
| WasmGC制約によるCLOS困難 | 中 | 高 | 線形メモリで代替、struct.new活用 |
| tagbody/goのWASM表現 | 中 | 高 | br_table、CPS変換 |
| bignumの効率的実装 | 高 | 中 | JavaScript BigInt連携 |
| multiple-valuesの表現 | 中 | 中 | グローバル配列+カウンタ |
| ストリームI/O | 高 | 低 | JavaScript連携 |

---

## 5. 成功基準

| 期間 | テストパス | カバー機能 |
|------|-----------|-----------|
| 3ヶ月 | 500 | 数値、リスト、基本制御 |
| 6ヶ月 | 5,000 | シンボル、配列、loop |
| 12ヶ月 | 15,000 | CLOS、コンディション |

---

## 6. TDDワークフロー

```lisp
;; 1. 失敗テストを特定
(clysm/ansi-tests:test-form '(abs -5) :expected 5)
;; => :fail

;; 2. primitives.lispに実装追加
(define-primitive abs (args env)
  (let ((arg-code (compile-form (first args) env)))
    `(,@arg-code
      ;; if negative, negate
      ...)))

;; 3. 再テスト
(clysm/ansi-tests:test-form '(abs -5) :expected 5)
;; => :pass

;; 4. カテゴリテスト実行
(clysm/ansi-tests:run-category :numbers :filter :simple)
```

---

## 7. 進捗記録

| 日付 | フェーズ | テストパス | 備考 |
|------|---------|-----------|------|
| 2025-12-20 | 2.0 | 1 | ANSIテスト統合完了 |
| 2025-12-20 | 2.5 | 24 | abs, max, min, evenp, oddp, gcd, lcm, floor, ceiling, truncate, round, 多引数比較 |
| 2025-12-20 | 2.5 | 178 | labels, tagbody/go, loop, mapcar/mapc/reduce |
| 2025-12-20 | 2.5 | 180 | values, multiple-value-bind, destructuring-bind |
| 2025-12-20 | Bootstrap | 200 | hash-table, setf, error, ブートストラップテスト追加 |
| 2025-12-20 | Bootstrap | 205 | defmacro, バッククォート（ホスト展開戦略） |
| 2025-12-20 | Bootstrap | 216 | 文字列プリミティブ、CLOS除去確認完了 |

---

## 8. 参考資料

- [ANSI Common Lisp仕様](http://www.lispworks.com/documentation/HyperSpec/Front/)
- [ansi-test](https://gitlab.common-lisp.net/ansi-test/ansi-test)
- [WebAssembly仕様](https://webassembly.github.io/spec/)
- [WasmGC提案](https://github.com/AaronRotworksGC/gc)
