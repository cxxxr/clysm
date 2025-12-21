# Milestone 2（方針②）: Wasm内インタプリタへ回帰する実装プラン

## 方針決定（このドキュメントの目的）

- **決定**: `js/eval.js` による評価器を撤回し、**Phase 2.5/2.6/2.7（eval・動的スコープ・非局所脱出）をWasm側へ実装**する  
- **JSの責務**: `js/reader.js` / `js/printer.js` / `WebAssembly.instantiate()`（＋必要ならエラーブリッジ）に限定する  
- **ゴール**: `reader -> (Wasm) eval -> printer` が成立し、既存テストをWasm evalに切り替えて維持できる状態にする

## 前提・制約（重要）

- **GC型の共有**の観点から、当面は **単一モジュール（`src/kernel/kernel.wat` → `build/kernel.wasm`）に eval を統合**する  
  - 理由: モジュール分割（`eval.wasm`）はGC型の共有/型インポートが絡み、設計・検証コストが高い  
  - 将来（Milestone 3以降）に型共有が安定したら分割を再検討
- `kernel.wat` には既に環境/クロージャ/プリミティブ型があるため、**既存表現を最大限流用**して移行する（大改造は避ける）

## 成果物（Doneの定義）

- `src/kernel/kernel.wat` に以下が実装され、`wasm-tools parse` でビルドできる
  - `eval`（トップレベル評価エントリ）
  - `apply`（関数適用、プリミティブ/解釈クロージャ対応）
  - 動的スコープ（シャローバインディング + トレイル）
  - 非局所脱出（`block/return-from`・`catch/throw`・`unwind-protect`）
- `js/reader.js` と `js/printer.js` のまま `test/eval.test.js` が通る（Evaluator依存を除去）

---

## 作業計画（移行を止めずに進める順序）

### Phase 0: 土台の整理（小さく安全に始める）

1. **現状の振る舞い固定**
   - `npm test` を基準にし、以後の変更で回帰が分かる状態を維持
2. **Wasm EH（Exception Handling）利用可否の確認**
   - Node.js / wasm-tools で `try_table` / `throw` / `catch` が動くか確認
   - 必要なら `package.json` の test 実行フラグを追加（例: EH関連フラグ）

**受け入れ条件**
- 既存テストが “変更前” に全てパスしている（回帰検知ラインがある）

---

### Phase 1: Wasm eval の最小骨格（まずは動くものを作る）

1. `kernel.wat` に **evalのエントリ関数**を追加
   - 例: `(export "eval") (param anyref) (result anyref)`（envは当面 `NIL` を暗黙使用）
2. **自己評価**（Fixnum/String/Character/NIL/T/Keyword）だけをWasmで実装
3. **シンボル評価**（暫定でグローバル値セルのみ）をWasmで実装

**受け入れ条件**
- `test/eval.test.js` の「Self-evaluating objects」「Quote（'xの展開はReader側）」相当がWasm経由で通る

---

### Phase 2: リスト評価（特殊形式と関数呼び出しの枠組み）

1. `eval_list` を追加し、以下を分岐できるようにする
   - 特殊形式（`quote`, `if`, `progn`, `setq`, `function`, `lambda`, `let`, `let*`, `defun`, `defvar`）
   - 通常の関数呼び出し
2. **特殊形式シンボルの確定**
   - 起動時に `QUOTE`/`IF`/... のシンボルを `intern_default` で生成し、グローバル参照として保持（比較コスト削減）
3. **評価器内部apply**（後述 Phase 3 で実体化）を呼べるようにフックだけ作る

**受け入れ条件**
- `quote/if/progn/setq` が最低限動作し、既存テストの該当セクションがWasm経由で通る

---

### Phase 3: 関数の表現（解釈クロージャをWasmで持つ）

> ここが “JS評価器撤回” の中核。`lambda` を返せなければ先に進めない。

1. **解釈クロージャ型の導入**
   - `js/InterpretedClosure` 相当をWasm GC structで定義（例: params/body/env/name/arity）
2. **`(lambda (params) body*)` の評価**
   - 返り値は解釈クロージャ
3. **`apply` の実装**
   - primitive: 既存 `$primitive` の `code` を `call_ref`
   - 解釈クロージャ: 引数束縛 → `progn` 評価
4. **`defun` / `function` / `funcall` / `apply`**
   - `defun` はシンボルの `function` セルへ解釈クロージャを格納
   - `funcall`/`apply` はWasm内の `apply` に統一

**受け入れ条件**
- 既存テストの「defun」「lambda」「funcall/apply」「高階関数（apply-twice）」までWasm経由で通る

---

### Phase 4: レキシカル環境（現状の簡易方式をWasmへ移植）

1. **環境表現は現状踏襲**
   - `env_frame.bindings` を `(sym0 val0 sym1 val1 ...)` のベクタとして扱い、線形探索
2. `let` / `let*` の実装
   - lexical: env_frame に束縛
   - special: Phase 5 の動的束縛へ（先に “lexicalだけ” で暫定実装→後でspecial対応に拡張しても良い）

**受け入れ条件**
- 既存テストの「Let」関連がWasm経由で通る

---

### Phase 5: 動的スコープ（シャローバインディング + トレイル）

1. **special判定**
   - 最低限は `*foo*` 規約 + `defvar` でspecialマーク（マークは plist か専用テーブル）
2. **バインディングスタック（Trail）**
   - `(symbol, oldValue)` を積むスタックをWasm側に用意（vector + sp、必要ならリサイズ）
3. **束縛の確実な復元**
   - 正常系: body評価後に pop/restore
   - 異常系: `unwind-protect` / EH と組み合わせて必ず復元（Phase 7 と合わせて完成）

**受け入れ条件**
- 既存テストの「動的スコープ（defvar + let special）」がWasm経由で通る

---

### Phase 6: 非局所脱出（Wasm EHで実装）

1. **Wasm EH タグ設計**
   - `block/return-from` 用タグ（payload: 識別子 + 値）
   - `catch/throw` 用タグ（payload: catch-tag + 値）
2. **`block` / `return-from`**
   - blockは “脱出先識別子” を動的に管理（動的スタック or env拡張）
   - return-from は識別子を解決して `throw`
3. **`catch` / `throw`**
   - catch は `try_table` で該当タグのみ捕捉し値を返す
4. **`unwind-protect`**
   - catch_all（または同等構造）で cleanup を実行し、必要なら rethrow

**受け入れ条件**
- `block/return-from` と `catch/throw` と `unwind-protect` の統合テストが追加され、Wasm経由で通る
- Phase 5 の special バインドが非局所脱出でも漏れなく復元される

---

### Phase 7: JS評価器の撤回（依存を断つ）

1. `test/eval.test.js` から `Evaluator` 依存を削除
   - `kernel.exports.eval(...)` のような **Wasm eval 呼び出し**へ置換
2. `js/eval.js` は以下のどちらかに整理
   - **削除**（最も明確）  
   - **legacyとして隔離**（`js/legacy/` 等に移し、テスト/実行系から参照しない）
3. `plans/MILESTONE_2_PLAN.md` を方針②に合わせて更新（少なくとも “JS評価器採用” の撤回を明記）

**受け入れ条件**
- JS側に “評価ロジック” が残っていない（Reader/Printer/instantiate 以外は補助的なブリッジのみ）
- `npm test` が全てパス

---

## 追加方針（迷ったときの決め事）

- **最初は単純に**: 速度より正しさ。最適化（多形ディスパッチ、TCO、多値など）は Milestone 3 以降へ送る
- **データ構造は増やしすぎない**: `env_frame`・`symbol`・`cons` を中心に、追加structは “解釈クロージャ” と “制御スタック用” に絞る
- **テスト駆動で移行**: JS Eval の振る舞いを “テスト” として残し、Wasm側へ段階移行する

## 未スコープ（Milestone 2では扱わない）

- マクロ（`defmacro`）、コンパイラ、JIT（動的モジュール生成）
- 多値返却（`values` / `multiple-value-bind`）
- `tagbody/go`、CLOS、型推論、最適化全般

