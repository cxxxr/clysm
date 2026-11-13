# 12. ウィンドウ

[目次](../README.md) | 前: [ストリームペインとインタラクタ](11-streams.md) | 次: [コンポーネントとマークアップ](13-components.md)

ページの中に、移動や大きさの変更ができるウィンドウを開けます。CAPI の複数のトップレベルウィンドウにあたります。

## 12.1 ウィンドウを開く

`display` に `:window t` を渡すと、インターフェースをアプリケーションの上のウィンドウに表示します。

```lisp
;;; example: windows
(define-interface note-window ()
  ()
  (:panes (text multi-line-text-input-pane :text "メモ"))
  (:default-initargs :title "メモ"))

(display (make-instance 'note-window) :window t :x 40 :y 40 :width 300 :height 200)
(display (make-instance 'note-window :title "もう1つのメモ") :window t)
```

| 引数 | 意味 |
|---|---|
| `:x`、`:y` | 左上の位置（ピクセル）。省くと、前のウィンドウから少しずらした位置 |
| `:width`、`:height` | 大きさ（既定は 480 × 320） |

ユーザーは次の操作ができます。

- 見出しをドラッグして移動する
- 右下の角をドラッグして大きさを変える
- 見出しのダブルクリックか □ ボタンで、ページいっぱいに広げる（もう一度で戻す）
- × ボタンで閉じる
- クリックしたウィンドウが一番上に来る

## 12.2 プログラムから操作する

| 関数 | 意味 |
|---|---|
| `(interface-window interface)` | インターフェースのウィンドウ（なければ `nil`） |
| `window-x`、`window-y`、`window-width`、`window-height` | 位置と大きさ（`setf` できる） |
| `window-maximized` | 広げているか（`setf` できる） |
| `(raise-interface interface)` | 一番上にする |
| `(tile-windows)` | ウィンドウを並べて画面を埋める |
| `(cascade-windows)` | ウィンドウを少しずつずらして重ねる |
| `(quit-interface interface)` | 閉じる |
| `*windows*` | 開いているウィンドウのリスト |

```lisp
;;; continued
(tile-windows)
(setf (window-width (interface-window (first *interfaces*))) 400)
```

ウィンドウを閉じると、インターフェースの `:destroy-callback` が呼ばれます。

## 12.3 ドッキング

`docking-layout` は、見出し付きのペインを並べるレイアウトです。見出しの ⇱ ボタンを押すと、そのペインがウィンドウに取り出されます。ウィンドウを閉じると、ペインは元の場所に戻ります。ペインそのものが移動するので、中の状態（入力した文字や選択）は保たれます。

```lisp
;;; example: docking
(define-interface ide ()
  ()
  (:panes
   (files list-panel :items '("main.lisp" "util.lisp"))
   (editor editor-pane :text "(defun main ())")
   (output title-pane :text "出力"))
  (:layouts
   (main docking-layout '(("ファイル" files) ("エディタ" editor) ("出力" output))
         :orientation :row)))

(display (make-instance 'ide))
```

## 12.4 ウィンドウとダイアログの違い

| | ウィンドウ（`display :window t`） | モーダルでないダイアログ（`display-dialog :modal nil`） | ダイアログ（`display-dialog`） |
|---|---|---|---|
| 移動・大きさの変更 | できる | できない | できない |
| ほかの操作 | できる | できる | できない |
| 答え（継続） | なし | あり | あり |

デスクトップ版でも、ウィンドウはアプリケーションのウィンドウの中に開きます（OS の別ウィンドウにはなりません）。
