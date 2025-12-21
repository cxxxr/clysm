<!--
Sync Impact Report
==================
Version change: N/A → 1.0.0 (MAJOR: Initial constitution)
Modified principles: N/A (Initial creation)
Added sections:
  - Core Principles (8 principles)
  - パフォーマンス・セキュリティ・相互運用制約
  - 開発ワークフローと検証プロセス
  - Governance
Removed sections: N/A
Templates requiring updates:
  - .specify/templates/plan-template.md: ⚠ pending (Constitution Check要更新)
  - .specify/templates/spec-template.md: ⚠ pending (要件セクション要確認)
  - .specify/templates/tasks-template.md: ⚠ pending (フェーズ構成要確認)
Follow-up TODOs: None
-->

# Clysm Constitution

WebAssembly GCをターゲットとしたCommon Lispコンパイラ実装のための設計原則と開発規約

## Core Principles

### I. WasmGC-First型システム設計

すべてのLispオブジェクトはWasm GCの型階層を活用して表現しなければならない（MUST）。
線形メモリ（Linear Memory）への依存は禁止され、ホストVMのGCにメモリ管理を委譲する。

**必須要件**:
- `anyref`をユニバーサル・ルート型として使用
- Fixnumは`i31ref`で表現（ヒープ割り当てゼロ）
- Consセル、シンボル、クロージャは`struct`で定義
- ベクタ・配列は`array`で定義
- 型検査は`ref.test`、ダウンキャストは`ref.cast`を使用

**根拠**: ホストGCとの統合により、言語間循環参照の自動解決、バイナリサイズ削減、
起動時間短縮を実現する。

### II. Lispオブジェクト表現規約

Common Lispの特殊オブジェクトは、仕様準拠性を最優先とした表現戦略を採用しなければ
ならない（MUST）。

**NIL表現**:
- NILはWasmの`null`として扱わない（MUST NOT）
- NILは特異なシングルトン構造体として実装
- 型チェックは`ref.eq`によるポインタ等価性判定で実施

**UNBOUND表現**:
- シンボルの未束縛状態を表すセンチネルオブジェクト`UNBOUND`を定義
- Lispレベルからはアクセス不可能な内部専用オブジェクト
- シンボルの`$value`スロット初期値として設定

**根拠**: NILはシンボルであり、リスト終端であり、偽（False）でもある特殊性を持つ。
Wasmのnullでは名前やプロパティリストへのアクセス特例処理が複雑化する。

### III. 関数・クロージャ実装戦略

すべてのLisp関数はクロージャ変換を経て、コードと環境を分離した構造体として
表現しなければならない（MUST）。

**クロージャ構造体**:
```wat
(type $closure (struct
  (field $code_0 (ref null $func_0))  ;; 0引数用
  (field $code_1 (ref null $func_1))  ;; 1引数用
  (field $code_2 (ref null $func_2))  ;; 2引数用
  (field $code_N (ref null $func_N))  ;; 汎用（リスト渡し）
  (field $env (mut anyref))
))
```

**アリティディスパッチ**:
- 頻出する少数引数（0-2）の呼び出しを高速化するため、多形ディスパッチ構造体を採用
- 対応スロットがnullの場合は汎用`$code_N`にフォールバック

**多値バッファ**:
- 第1値（Primary Value）は関数の戻り値として返却
- 第2値以降はグローバル変数「多値バッファ」に格納
- `multiple-value-bind`等は関数復帰直後にバッファを確認

**根拠**: Wasmの`call_ref`は厳密に型付けされており、動的なアリティに対応するには
構造化されたディスパッチ機構が必要。

### IV. Wasm制御フロー活用

末尾呼び出し最適化と例外処理はWasmネイティブ機能を活用しなければならない（MUST）。

**末尾呼び出し（Tail Call）**:
- 末尾位置にある関数呼び出しは`return_call`に変換
- トランポリン方式はフォールバックとしてのみ考慮

**例外処理（Exception Handling）**:
- `block/return-from`は`throw`命令にマッピング
- `block`構文入口には`try_table`を配置
- `unwind-protect`は`try_table`の`catch_all`節で実装
- cleanup処理後に例外を`rethrow`

**根拠**: Wasm 3.0のtail_callとEHプロポーザルは主要ブラウザでサポートされており、
スタックオーバーフロー回避と非局所脱出の効率的実装が可能。

### V. シャローバインディングによる動的スコープ

スペシャル変数（動的スコープ変数）はシャローバインディング方式で実装しなければ
ならない（MUST）。

**実装戦略**:
1. 各シンボル構造体は現在値を保持する`$value`フィールドを持つ
2. 束縛時: 現在の`$value`をバインディングスタック（Trail）にプッシュし、新値で上書き
3. 参照時: シンボルの`$value`フィールドを直接読み取り（O(1)）
4. 復元時: `unwind-protect`（`try_table`）で古い値をポップし書き戻し

**根拠**: Wasmはスタック検査（Stack Walking）を許可していないため、自前実装が必要。
シャローバインディングはレキシカル変数とほぼ同等のO(1)アクセス速度を実現する。

### VI. 段階的動的コンパイル（Tiered Eval/JIT）

`eval`と`compile`は2段階実行モデルで実装しなければならない（MUST）。

**Tier 1: Wasm内インタプリタ**:
- S式評価器をWasmで実装
- REPL対話実行、一度限りのコードに使用
- コンパイルオーバーヘッドなし、即応性優先

**Tier 2: 動的Wasmモジュール生成（JIT）**:
1. インメモリ・コンパイル: S式からWasmバイナリ（Uint8Array）を生成
2. モジュール実体化: ホストの`WebAssembly.instantiate()`を呼び出し
3. 動的リンク: メインランタイムのGCヒープ・関数テーブルをインポート
4. ホットパッチ: 新モジュールの`func_ref`をシンボル関数スロットに登録

**根拠**: Wasmはハーバードアーキテクチャを採用し、実行時コード生成を直接禁止。
モジュールインスタンス化による「正当な」JIT手法でセキュリティサンドボックスを維持。

### VII. テスト駆動開発（TDD）（非交渉）

すべての機能実装はTDDサイクルに従わなければならない（MUST）。

**必須プロセス**:
1. テスト作成 → ユーザー承認 → テスト失敗確認 → 実装
2. Red-Green-Refactorサイクルを厳格に遵守
3. 各コミットはすべてのテストがパスした状態であること

**テスト種別**:
- **単体テスト**: 個別関数・マクロの動作検証
- **契約テスト**: Wasm出力のセクション構造・型検証
- **統合テスト**: コンパイル→実行→結果検証の一連フロー

**根拠**: Common Lispコンパイラは複雑な相互依存を持ち、回帰バグのリスクが高い。
TDDにより仕様の明文化と品質保証を同時達成する。

### VIII. Nix-Firstワークフロー

ビルド・開発環境はNix Flakesで完全に管理しなければならない（MUST）。

**必須コマンド**:
- `nix develop`: 開発シェルへの進入
- `nix build`: 再現可能なビルド実行
- `nix flake check`: CI/ローカルでの品質ゲート

**flake.nix要件**:
- すべての依存関係をflake inputsで宣言
- devShellにwasm-tools、wat2wasm、wasmtimeを含める
- checksにテスト実行、lint、型検査を含める

**グリーンビルド保証**:
- `nix flake check`がパスしないコミットはマージ禁止（MUST NOT）
- CIは必ずNix環境で実行

**根拠**: WebAssemblyツールチェーンはバージョン依存が強く、再現可能なビルドが必須。
Nixにより「私の環境では動く」問題を根絶する。

## パフォーマンス・セキュリティ・相互運用制約

### パフォーマンス要件

| 指標 | 目標値 | 測定方法 |
|------|--------|----------|
| Fixnum算術 | ネイティブi32演算の1.5倍以内 | マイクロベンチマーク |
| コンス生成 | 10M cells/sec以上 | GCストレステスト |
| 関数呼び出し | 間接呼び出し5ns以内 | call_refベンチマーク |
| JITコンパイル | 1000行/100ms以内 | コンパイル時間測定 |

### セキュリティ制約

- Wasmサンドボックスの境界を絶対に破らない（MUST NOT）
- 線形メモリへの直接アクセスは禁止（MUST NOT）
- 外部リソースアクセスはWASIインターフェース経由のみ
- 動的コード生成は`WebAssembly.instantiate()`のみ許可

### 相互運用要件（WASI/Component Model）

**WASI Preview 2準拠**:
- ファイルI/Oは`wasi:filesystem`経由
- 標準入出力は`wasi:cli`経由
- ネットワークは`wasi:http`経由（将来対応）

**Component Model対応**:
- WIT（WebAssembly Interface Type）でインターフェース定義
- Canonical ABIに準拠した型マッピング
- 共有ナッシング（Shared-Nothing）アーキテクチャ維持

## 開発ワークフローと検証プロセス

### WAT/WASM検証

すべての生成Wasmバイナリは`wasm-tools`で検証しなければならない（MUST）。

**検証項目**:
- セクション順序（ID 1-13の昇順）
- LEB128エンコーディングの正当性
- 型システム整合性（WasmGC型階層）
- 関数シグネチャ一致

**検証コマンド**:
```bash
wasm-tools validate output.wasm
wasm-tools print output.wasm  # WAT変換による可読性確認
```

### コードレビュー基準

- シニアCommon Lispエンジニアの視点で設計妥当性を検証
- ANSI Common Lisp仕様との整合性確認
- Wasm GC仕様との整合性確認
- パフォーマンス影響の定量評価

### 言語要件

- コード内コメント: 英語
- ドキュメント・Issue・PR: 日本語
- コミットメッセージ: 英語（Conventional Commits形式）

## Governance

本憲法はプロジェクトの最上位規範であり、他のすべてのガイドラインに優先する。

**改定プロセス**:
1. 改定提案のIssue作成（日本語）
2. 技術的根拠と影響範囲の明示
3. 最低1名のシニアエンジニアによるレビュー
4. 全テストパス確認後にマージ

**バージョニング**:
- MAJOR: 原則の削除・根本的再定義
- MINOR: 新原則追加・重要な拡張
- PATCH: 明確化・誤字修正・非意味的変更

**コンプライアンス**:
- すべてのPR/レビューは本憲法への準拠を検証すること
- 複雑性の追加は明示的な正当化が必要
- ランタイムガイダンスは`.specify/`配下のドキュメントを参照

**Version**: 1.0.0 | **Ratified**: 2025-12-21 | **Last Amended**: 2025-12-21
