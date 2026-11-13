# Clysm UI フレームワーク マニュアル

Clysm UI は、Common Lisp で書いたプログラムを WebAssembly にコンパイルして、ブラウザやデスクトップ（Electron）で動く GUI アプリケーションにするためのフレームワークです。LispWorks の CAPI と、McCLIM（CLIM）を参考にしています。

このマニュアルは2部に分かれています。

- **ユーザーガイド**（日本語）: 概念と使い方を、動く例とともに順に説明します。
- **リファレンス**（英語）: 公開しているすべての関数・マクロ・クラス・変数の説明です。ソースの docstring から生成しています。

## ユーザーガイド

1. [はじめに](guide/01-introduction.md) — フレームワークの全体像
2. [使い始める](guide/02-getting-started.md) — プロジェクトの作成、ビルド、実行
3. [インターフェース](guide/03-interfaces.md) — ウィンドウを定義して表示する
4. [状態と画面の更新](guide/04-state.md) — モデル、`computed`、`binding`
5. [ペイン](guide/05-panes.md) — テキスト、ボタン、リスト、表、ツリーなど
6. [レイアウト](guide/06-layouts.md) — ペインの並べ方
7. [コマンドとメニュー](guide/07-commands-and-menus.md) — 操作の定義、メニューバー、キー
8. [プレゼンテーション](guide/08-presentations.md) — 画面上のオブジェクトとコマンドの引数
9. [ダイアログ](guide/09-dialogs.md) — 問い合わせ、ファイル、クリップボード、エラー
10. [描画](guide/10-graphics.md) — 図形とグラフ
11. [ストリームペインとインタラクタ](guide/11-streams.md) — `format` で書く画面、コマンド行
12. [ウィンドウ](guide/12-windows.md) — ページの中の複数のウィンドウ
13. [コンポーネントとマークアップ](guide/13-components.md) — 下の層と、新しいペインの作り方
14. [デスクトップアプリケーション](guide/14-desktop.md) — Electron で動かす
15. [開発とテスト](guide/15-development.md) — REPL での開発、ブラウザなしのテスト、バックエンド
16. [見た目](guide/16-styling.md) — テーマと CSS
17. [付録: CAPI・CLIM との対応](guide/17-capi-and-clim.md)

## リファレンス

- [シンボル索引](reference/index.md) — すべての公開シンボル
- [Markup, components and state](reference/core.md)
- [Commands and menu items](reference/commands.md)
- [Panes, layouts, menus and interfaces](reference/panes.md)
- [Presentations and popup menus](reference/presentations.md)
- [Dialogs, files and the clipboard](reference/dialogs.md)
- [Drawing and graphs](reference/graphics.md)
- [Stream panes and the interactor](reference/streams.md)
- [The DOM backend](reference/dom.md)
- [The desktop backend](reference/desktop.md)
- [The S-expression backend](reference/sexp.md)

## このマニュアルについて

- ガイドの例のうち `;;; example:` で始まるものは、`make test-ui` で実際にコンパイルして実行し、表示される内容を `tests/ui/examples.expected` と照合しています。
- リファレンスは `tools/ui-doc.py` で生成します。ソースの docstring を変えたら再生成してください。`make test-ui` は、公開シンボルに説明のないものがあるときと、リファレンスが古いときに失敗します。
