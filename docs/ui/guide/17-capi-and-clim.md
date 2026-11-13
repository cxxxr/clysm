# 17. 付録: CAPI・CLIM との対応

[目次](../README.md) | 前: [見た目](16-styling.md)

## 17.1 CAPI

| CAPI | Clysm UI | 説明 |
|---|---|---|
| `define-interface`、`display`、`contain`、`quit-interface` | 同じ名前 | [インターフェース](03-interfaces.md) |
| `title-pane`、`display-pane`、`text-input-pane`、`password-pane`、`multi-line-text-input-pane`、`editor-pane`、`text-input-choice` | 同じ名前 | `editor-pane` は等幅のプレーンテキスト。[ペイン](05-panes.md) |
| `push-button`、`check-button`、`radio-button`、`push-button-panel`、`radio-button-panel`、`check-button-panel` | 同じ名前 | |
| `list-panel`、`multi-column-list-panel`、`tree-view`、`option-pane`、`graph-pane` | 同じ名前 | 表は見出しで並べ替えられる |
| `slider`、`progress-bar`、`toolbar`、`browser-pane`、`output-pane` | 同じ名前 | |
| `column-layout`、`row-layout`、`grid-layout`、`tab-layout`、`switchable-layout`、`simple-layout`、`pinboard-layout`、`docking-layout` | 同じ名前 | [レイアウト](06-layouts.md) |
| `:items`、`:print-function`、`:interaction`、`:selection-callback`、`:action-callback`、`:callback-type`、`choice-selected-item(s)` | 同じ | |
| `:menus`、`:menu-bar`、`menu`、`menu-item`、ニーモニック、accelerator、`:pane-menu` | 同じ（項目は `menu-entry`） | [コマンドとメニュー](07-commands-and-menus.md) |
| `display-dialog`、`exit-dialog`、`abort-dialog`、`popup-confirmer`、`display-message`、`prompt-for-string`、`prompt-for-number`、`prompt-for-confirmation`、`prompt-with-list`、`prompt-for-file`、`prompt-for-color` | 同じ名前（`:continuation` で答えを受け取る） | [ダイアログ](09-dialogs.md) |
| `gp:draw-line` などのグラフィックスポート、`with-graphics-state`、変換 | `draw-line` など、同じ名前のマクロ | 図形の木。[描画](10-graphics.md) |
| `:input-model` | `:press-callback`、`:motion-callback`、`:release-callback` | |
| `set-clipboard`、`clipboard` | 同じ名前 | |
| 複数のトップレベルウィンドウ | `display ... :window t` | ページの中のウィンドウ。[ウィンドウ](12-windows.md) |
| `:visible-min-width` などの大きさの指定 | `:width`、`:min-width`、`:stretch` など | CSS の長さ |

## 17.2 CLIM

| CLIM | Clysm UI | 説明 |
|---|---|---|
| `define-application-frame`、ペイン、レイアウト | `define-interface` | |
| `define-command`（型付きの引数）、コマンドテーブル | `define-command`、`:table` | [コマンドとメニュー](07-commands-and-menus.md) |
| `present`、`with-output-as-presentation`、`define-presentation-type`、presentation のメソッド | 同じ名前（`present-object` が present のメソッド） | [プレゼンテーション](08-presentations.md) |
| `accept`、入力文脈、選べるプレゼンテーションのハイライト | `accept` | クリック、またはダイアログ・コマンド行で入力 |
| `accepting-values` | `prompt-for-values` | |
| `define-presentation-to-command-translator`、プレゼンテーションのメニュー、`define-drag-and-drop-translator` | 同じ名前 | |
| ストリームペイン: `format`、`present :stream`、`with-text-style`、`window-clear` | `stream-pane`、同じ名前 | [ストリームペインとインタラクタ](11-streams.md) |
| インタラクタ（名前でコマンドを打つ、引数の入力と補完） | `interactor-pane`、`accept-from-string` | コマンドパレットもある |
| ポインタの説明ペイン | `:pointer-documentation t` | |
| 表示関数、インクリメンタルな再表示、`updating-output` | `application-pane`、`stream-pane` の `:display-function` | すべてのペインが、読んだ状態に従って差分で描き直される |
| `formatting-table`、`formatting-row`、`formatting-cell`、`formatting-item-list` | 同じ名前（ストリームペイン） | |
| `draw-line*` など、`with-drawing-options`、変換 | `draw-line` など、`with-graphics-state` | |
| `format-graph-from-roots` | `graph-pane` | |
| `menu-choose`、`notify-user` | `popup-menu`・`prompt-with-list`、`display-message` | |
| デバッガのリスタート | リスタートを選ぶダイアログ | [ダイアログ](09-dialogs.md) |

## 17.3 含まれないもの

ページやブラウザが担うので、次のものは含まれていません。

- フォント、色、カーソルの細かい指定（CSS で行う）
- スクロールの制御（ペインは必要に応じてスクロールする）
- 印刷
- テキスト編集のコマンド（入力欄はブラウザの編集機能を使う）
- プロセスとスレッド（ページは1本のスレッドで動く。待つ処理は `js:await` と `wait-for-answer`）

## 17.4 動作環境

| 環境 | 確認したもの | 備考 |
|---|---|---|
| Chromium | 130 | 古い版では `--js-flags=--experimental-wasm-exnref` が要る。処理の途中で待つ機能（JSPI）にはさらに `--experimental-wasm-jspi` が要る版もある |
| Firefox | 154、159 | |
| WebKit（Safari の エンジン） | Safari 27.2 相当 | ページからのクリップボードの読み取りは許されないため、貼り付けのダイアログになる |
| Electron | 45 | |

JSPI がない環境では、エラーのリスタートは `prompt` で尋ね、`wait-for-answer` は使えません。それ以外の機能は同じように動きます。
