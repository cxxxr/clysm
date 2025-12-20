# Clysm セルフホスティング計画

## 目標

Clysmコンパイラ自身をWASMにコンパイルし、Node.js上で完全なREPLを実現する。

```
現在:  SBCL (コンパイル) → WASM (実行)
目標:  Node.js + WASM (コンパイル + 実行)
```

## 現状分析

### 実装済み機能
- 基本算術・比較・論理演算
- 制御構造 (if, let, cond, block, tagbody)
- リスト操作 (cons, car, cdr, mapcar, reduce)
- 関数 (defun, lambda, labels, closures)
- 構造体 (defstruct)
- ハッシュ表 (make-hash-table, gethash, sethash)
- マクロ (defmacro - ホスト展開)
- 文字列基本操作

### 未実装で必要な機能

| 機能 | 使用箇所 | 優先度 | 複雑度 |
|------|---------|--------|--------|
| Reader (S式パーサー) | REPL必須 | 最高 | 高 |
| UTF-8エンコーディング | データセクション | 高 | 中 |
| format (基本) | エラーメッセージ | 高 | 中 |
| intern (実行時) | シンボル生成 | 高 | 中 |
| loop (完全版) | エンコーダ | 中 | 高 |
| catch/throw | エラー処理 | 中 | 中 |
| unwind-protect | リソース管理 | 中 | 中 |

---

## フェーズ計画

### Phase S1: Reader実装 [進行中]

REPLの最重要コンポーネント。S式を読み込む機能。

**タスク:**
1. [x] 文字入力ストリーム (make-reader-state, peek-char, read-char等)
2. [x] 数値パーサー (整数の読み込み)
3. [x] リストパーサー (ネスト対応)
4. [ ] シンボルパーサー (現在はハッシュコードのみ)
5. [ ] 文字列リテラルパーサー
6. [ ] クォート/バッククォート処理
7. [ ] コメント処理 (; と #|...|#)

**実装済み:**
- 文字列リテラルのコンパイル
- reader-state構造体とプリミティブ
- whitespace-char-p, digit-char-p, alpha-char-p, symbol-constituent-p
- 再帰的なread-form, read-list実装

**マイルストーン:** `(read-from-string "(1 2 3)")` → リスト ✓

---

### Phase S2: 文字列・シンボル強化 [1-2週間]

**タスク:**
1. 文字列リテラルのWASM内構築
2. UTF-8エンコード/デコード (flexi-streams代替)
3. `intern` - 実行時シンボル生成
4. `symbol-name`, `symbol-package`
5. `string-to-symbol`, `symbol-to-string`

**マイルストーン:** `(intern "HELLO")` → シンボル HELLO

---

### Phase S3: Format基本実装 [1週間]

**タスク:**
1. フォーマット文字列パーサー
2. `~A` - aesthetic出力
3. `~S` - standard出力
4. `~D` - 10進数
5. `~%` - 改行
6. `~~` - チルダエスケープ

**マイルストーン:** `(format nil "x=~A" 42)` → "x=42"

---

### Phase S4: 高度な制御構造 [1-2週間]

**タスク:**
1. `catch` / `throw`
2. `unwind-protect`
3. `handler-case` (基本)
4. スタック巻き戻し機構

**マイルストーン:** エラーからの復帰が可能に

---

### Phase S5: Loop完全版 [2週間]

現在SBCLのloop展開に依存。自前で実装。

**タスク:**
1. `for var from n to m`
2. `for var in list`
3. `collect`, `append`, `sum`, `count`
4. `while`, `until`
5. `finally`

**マイルストーン:** 複雑なloopがコンパイル可能

---

### Phase S6: コンパイラWASM化 [2-3週間]

コンパイラソースをClysm自身でコンパイル。

**タスク:**
1. コンパイラソースの依存性分析
2. 必要な追加プリミティブ実装
3. 段階的コンパイルテスト
4. バイナリ比較検証

**検証手順:**
```bash
# Stage 1: SBCLでコンパイラをコンパイル
clysm-compile src/ → stage1.wasm

# Stage 2: Stage1でコンパイラをコンパイル
node run-wasm.js stage1.wasm src/ → stage2.wasm

# Stage 3: 比較
diff stage1.wasm stage2.wasm  # 同一であれば成功
```

---

### Phase S7: Node.js REPL [1週間]

**タスク:**
1. Node.js入出力バインディング
2. REPLループ実装
3. 履歴・編集機能
4. エラー表示

**最終成果物:**
```bash
$ node clysm-repl.js
Clysm REPL v1.0
> (+ 1 2)
3
> (defun square (x) (* x x))
SQUARE
> (square 5)
25
```

---

## 依存関係図

```
Phase S1 (Reader)
    ↓
Phase S2 (文字列/シンボル) ←──┐
    ↓                        │
Phase S3 (Format) ───────────┘
    ↓
Phase S4 (制御構造)
    ↓
Phase S5 (Loop)
    ↓
Phase S6 (コンパイラWASM化)
    ↓
Phase S7 (Node.js REPL)
```

---

## 実装優先順位

### 必須 (ブロッカー)
1. **Reader** - なければREPL不可能
2. **UTF-8** - 文字列データ必須
3. **intern** - シンボル生成必須

### 重要 (品質向上)
4. **format** - エラーメッセージ
5. **catch/throw** - エラー処理
6. **loop完全版** - コード簡潔化

### あれば良い (後回し可)
7. **unwind-protect** - リソース管理
8. **handler-case** - 高度なエラー処理

---

## リスク分析

| リスク | 確率 | 影響 | 対策 |
|--------|------|------|------|
| Reader複雑すぎ | 中 | 高 | 段階的実装、最小限から開始 |
| WASM制約 | 中 | 高 | JavaScript連携で回避 |
| 循環依存 | 低 | 高 | 依存グラフ分析 |
| 性能問題 | 中 | 中 | ボトルネック分析後最適化 |

---

## 成功基準

1. **Phase S1-S3完了**: 基本的なREPLが動作
2. **Phase S4-S5完了**: 複雑なプログラムがコンパイル可能
3. **Phase S6完了**: コンパイラ自身をコンパイル可能
4. **Phase S7完了**: Node.js単体でREPL動作

---

## 次のアクション

1. Phase S1 (Reader) の詳細設計
2. 文字入力プリミティブの追加
3. トークナイザの実装開始
