# 6. レイアウト

[目次](../README.md) | 前: [ペイン](05-panes.md) | 次: [コマンドとメニュー](07-commands-and-menus.md)

レイアウトは、ほかのペインを並べるペインです。インターフェースの `:layouts` に書くか、`make-instance` で作ります。

## 6.1 記述

レイアウトの **記述**（`:description`、`layout-description`）は、並べるものの並びです。要素には次のものを書けます。

- ペインやレイアウトの名前（`define-interface` の中）
- ペインやレイアウトのインスタンス
- 文字列（そのまま表示されます）
- `nil`（グリッドの空いたセル）
- `:separator`（区切り線。行と列のレイアウト）
- `:divider`（ユーザーがドラッグして動かせる境界。行と列のレイアウト）

`define-interface` の `:layouts` では、記述は評価される式です。名前のリストは `'(a b c)` と書きます。

## 6.2 行と列

`column-layout` は縦に、`row-layout` は横に並べます。

| 初期化引数 | 意味 |
|---|---|
| `:ratios` | 空いた場所の分け方。記述の要素ごとの重み（数か `nil`）のリスト |
| `:adjust` | 並びと直角の方向の揃え方: `:start`、`:center`、`:end`、`:stretch` |
| `:gap` | ペインの間隔（ピクセルか CSS の長さ） |

`:ratios` がないときは、`:stretch` が真のペインが空いた場所を分け合います。リスト、表、木、レイアウト、描画ペインは既定で `:stretch` が真なので、ボタンや欄は必要な大きさに、リストは残りの大きさになります。

```lisp
;;; example: rows-and-columns
(define-interface mail-window ()
  ()
  (:panes
   (folders list-panel :items '("受信" "送信" "下書き"))
   (messages list-panel :items '("会議の件" "見積もり"))
   (body display-pane :text "本文")
   (reply push-button :text "返信")
   (forward push-button :text "転送"))
  (:layouts
   (main row-layout '(folders :divider right) :ratios '(1 nil 3))
   (right column-layout '(messages :separator body buttons) :ratios '(1 nil 2 nil))
   (buttons row-layout '(reply forward) :adjust :end :gap 4)))

(display (make-instance 'mail-window))
```

`:divider` を置くと、その前のペインの大きさをユーザーがドラッグで変えられます。

## 6.3 グリッド

`grid-layout` は、`:columns` 列の表の形に並べます。フォームを作るのに便利です。`:ratios` は列ごとの重みで、既定では最後の列が残りの幅を使います。

```lisp
;;; example: grid-layout
(define-interface address-form ()
  ()
  (:panes
   (name text-input-pane)
   (mail text-input-pane :placeholder "name@example.jp")
   (newsletter check-button :text "お知らせを受け取る"))
  (:layouts
   (form grid-layout '("名前" name "メール" mail nil newsletter) :columns 2)))

(display (make-instance 'address-form))
```

## 6.4 タブと切り替え

`tab-layout` はタブを並べ、選ばれたタブのペインを表示します。記述の各要素は `(タブの見出し ペイン)` です。タブの選択は `choice` と同じように `choice-selected-item` で読み書きします（項目は記述の要素です）。

`switchable-layout` は、記述のペインのうち1つ（`:visible-child`、既定は最初）を表示します。`computed` と組み合わせると、状態によって表示を切り替えられます。

```lisp
;;; example: tabs-and-switching
(define-interface settings ()
  ((advanced :initform nil :accessor advanced))
  (:panes
   (general title-pane :text "一般の設定")
   (show-advanced check-button :text "詳しい設定を表示" :selected (binding (advanced interface)))
   (simple title-pane :text "（詳しい設定は隠れています）")
   (details title-pane :text "詳しい設定"))
  (:layouts
   (tabs tab-layout '(("一般" general) ("詳細" detail-page)))
   (detail-page column-layout '(show-advanced switch))
   (switch switchable-layout '(simple details)
           :visible-child (computed (if (advanced interface) 'details 'simple)))))

(display (make-instance 'settings))
```

## 6.5 そのほかのレイアウト

- `simple-layout` は、ペインを1つだけ持つレイアウトです。`:title-position :frame` と組み合わせて、ペインを枠で囲むのに使います。
- `pinboard-layout` は、ペインを指定した位置に置きます。記述の要素は `(ペイン x y)` か `(ペイン x y 幅 高さ)` です（ピクセル）。
- `docking-layout` は、見出し付きのペインを並べ、ユーザーがペインを別のウィンドウに取り出せるようにします。記述の要素は `(見出し ペイン)` です。取り出したウィンドウを閉じると元に戻ります（[ウィンドウ](12-windows.md)）。

```lisp
;;; example: other-layouts
(define-interface workbench ()
  ()
  (:panes
   (files list-panel :items '("a.lisp" "b.lisp"))
   (notes multi-line-text-input-pane)
   (output title-pane :text "出力"))
  (:layouts
   (main column-layout '(docks framed))
   (docks docking-layout '(("ファイル" files) ("メモ" notes)) :orientation :row)
   (framed simple-layout '(output) :title "結果" :title-position :frame)))

(display (make-instance 'workbench))
```

## 6.6 大きさを決める

- `:width`、`:height` で大きさを固定します。数はピクセル、文字列は CSS の長さ（`"20em"`、`"50%"`）です。
- `:min-width` などで範囲を決めます。
- `:stretch` を `t` にすると、行や列の空いた場所を使います。数を書くと、その重みで分け合います。
- ラベル（`:title`）付きのペインは、ラベルとペインをまとめた要素の大きさになります。

インターフェースは表示した要素（既定は `#app`）いっぱいに広がります。ページ全体に表示するには、`#app` の高さをページの高さにしてください（雛形の `index.html` はそうなっています）。
