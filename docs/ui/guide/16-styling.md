# 16. 見た目

[目次](../README.md) | 前: [開発とテスト](15-development.md) | 次: [付録: CAPI・CLIM との対応](17-capi-and-clim.md)

ペインの見た目は `ui/ui.css` で決まります。ページから読み込んでください（雛形の `index.html` は読み込んでいます）。

```html
<link rel="stylesheet" href="ui.css">
```

## 16.1 ライトとダーク

色はシステムの設定に合わせて、明るい配色と暗い配色が切り替わります。どちらかに固定するには、ページの CSS で `color-scheme` を指定します。

```css
:root { color-scheme: dark; }      /* いつも暗い配色 */
```

## 16.2 テーマを変える

色や大きさは CSS のカスタムプロパティです。`ui.css` の後で上書きすると、アプリケーション全体の見た目が変わります。

```css
:root {
  --ui-accent: light-dark(#0f766e, #5eead4);
  --ui-accent-fg: light-dark(#ffffff, #06231f);
  --ui-selected: light-dark(#d5efec, #17403c);
  --ui-radius: 2px;
  --ui-font: 14px/1.5 "Noto Sans JP", sans-serif;
}
```

| プロパティ | 意味 |
|---|---|
| `--ui-font` | 文字（CSS の `font` の値） |
| `--ui-bg` | ウィンドウの背景 |
| `--ui-surface` | リスト、入力欄、メニューの背景 |
| `--ui-fg` | 文字の色 |
| `--ui-muted` | 控えめな文字（ラベルなど）の色 |
| `--ui-line` | 線と枠の色 |
| `--ui-hover` | ポインタが乗ったときの背景 |
| `--ui-accent` | 強調の色（既定のボタン、選択、フォーカス） |
| `--ui-accent-fg` | 強調の色の上の文字の色 |
| `--ui-selected` | 選ばれた項目の背景 |
| `--ui-radius` | 角の丸み |
| `--ui-gap` | ペインの間隔 |
| `--ui-control-height` | ボタンや入力欄の高さ |
| `--ui-shadow` | メニューやダイアログの影 |

`light-dark(明るいとき, 暗いとき)` で、配色ごとに値を変えられます。

## 16.3 クラス

ペインの要素には、種類ごとのクラスが付いています。個別の見た目は、このクラスか、ペインの `:class` で加えたクラスに CSS を書いて変えます。

| クラス | 付く要素 |
|---|---|
| `ui-pane` | すべてのペイン |
| `ui-title-pane`、`ui-push-button`、`ui-list-panel`、`ui-tree-view`… | ペインの種類（クラス名に `ui-` を付けたもの） |
| `ui-column-layout`、`ui-row-layout`、`ui-grid-layout`… | レイアウトの種類 |
| `ui-interface` | インターフェース |
| `ui-item`、`selected` | 選択ペインの項目と、選ばれた項目 |
| `ui-default` | 既定のボタン |
| `ui-titled`、`ui-title`、`ui-frame` | ラベル付きのペインと、そのラベル、枠 |
| `ui-presentation`、`ui-acceptable` | プレゼンテーションと、いま引数として選べるもの |
| `ui-menu-bar`、`ui-menu`、`ui-menu-item` | メニュー |
| `ui-dialog`、`ui-top-window` | ダイアログとウィンドウ |
| `ui-pointer-documentation` | ポインタの説明行 |

```lisp
(make-instance 'push-button :text "削除" :class "danger")
```

```css
.ui-push-button.danger { background: #b8433a; border-color: #b8433a; color: white; }
```

## 16.4 描画の色

`output-pane` の図形の色は、描画関数の `:foreground` などで決めます。`"var(--ui-accent)"` のように書くと、テーマの色を使えます。文字の色（既定）は `currentColor` で、配色に合わせて変わります。図形に `:class` を付けて CSS で色を決めることもできます。
