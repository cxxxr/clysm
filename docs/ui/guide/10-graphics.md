# 10. 描画

[目次](../README.md) | 前: [ダイアログ](09-dialogs.md) | 次: [ストリームペインとインタラクタ](11-streams.md)

## 10.1 output-pane

`output-pane` は、`:display-callback` の関数が描いたものを表示するペインです。関数はペインを引数に呼ばれ、その中で描画関数を呼びます。関数が読んだ状態が変わると、呼び直されて描き直されます。

```lisp
;;; example: output-pane
(defstate *points* '((20 80) (60 30) (100 60) (140 20)))

(contain (make-instance 'output-pane
                        :view-width 160 :view-height 100
                        :display-callback
                        (lambda (pane)
                          (draw-rectangle pane 0 0 160 100 :fill "white")
                          (draw-lines pane *points* :foreground "steelblue" :thickness 2)
                          (dolist (point *points*)
                            (draw-circle pane (first point) (second point) 3 :filled t))
                          (draw-string pane "売上" 80 95 :anchor :middle))))
```

描いたものはピクセルではなく図形（SVG）です。描き直しは、前の図形と比べて違うものだけを変えます。

### 座標

`:view-width` と `:view-height` を指定すると、描画の座標は 0 からその値までになり、ペインの大きさに合わせて拡大・縮小されます（縦横の比は保たれます）。指定しないと、座標はペインのピクセルです。

## 10.2 描画関数

| 関数 | 描くもの |
|---|---|
| `(draw-line pane x1 y1 x2 y2)` | 線分 |
| `(draw-lines pane points)` | 折れ線。`points` は `(x y x y ...)` か `((x y) ...)` |
| `(draw-polygon pane points)` | 多角形 |
| `(draw-rectangle pane x y width height &key radius)` | 長方形（`:radius` で角を丸める） |
| `(draw-circle pane x y radius)` | 円 |
| `(draw-ellipse pane x y x-radius y-radius)` | 楕円 |
| `(draw-arc pane x y radius start-angle sweep-angle)` | 円弧（`:filled` で扇形）。角度はラジアン |
| `(draw-path pane data)` | SVG のパスの記法で書いた図形（`"M0 0 L10 10 Z"`） |
| `(draw-string pane string x y &key anchor baseline font-size font-weight)` | 文字列 |
| `(draw-image pane url x y width height)` | 画像 |

`draw-string` の `:anchor` は `x` が文字列の `:start`（左端、既定）、`:middle`、`:end` のどれか、`:baseline` は `y` が `:alphabetic`（ベースライン、既定）、`:middle`、`:top` のどれかを決めます。

## 10.3 グラフィックス状態

描画関数は、次のキーワード引数で描き方を決めます。

| 引数 | 意味 |
|---|---|
| `:foreground` | 色（CSS の色。既定は文字の色） |
| `:filled` | 真なら中を塗ります（線を描く代わりに） |
| `:fill` | 輪郭を描く図形の中の色 |
| `:thickness` | 線の太さ |
| `:dashes` | 破線の長さのリスト（`'(4 2)`） |
| `:opacity` | 不透明度（0〜1） |
| `:class` | 図形の CSS クラス |

`with-graphics-state` は、その中の描画関数の既定値を決めます。

```lisp
(with-graphics-state (pane :foreground "red" :thickness 2)
  (draw-line pane 0 0 100 100)          ; 赤、太さ 2
  (draw-line pane 0 100 100 0 :foreground "blue"))
```

色に `"var(--ui-accent)"` と書くと、テーマの色（[見た目](16-styling.md)）を使えます。

## 10.4 変換とグループ

| マクロ | 意味 |
|---|---|
| `(with-graphics-translation (pane dx dy) ...)` | 中で描くものを `dx`、`dy` ずらす |
| `(with-graphics-rotation (pane angle [x y]) ...)` | `x`、`y` を中心に回す（ラジアン） |
| `(with-graphics-scale (pane sx [sy]) ...)` | 拡大・縮小する |
| `(with-drawing-group (pane &key class) ...)` | 中で描くものを1つの図形にまとめる |

## 10.5 図形をプレゼンテーションにする

描画関数は描いた図形を返すので、`with-output-as-presentation` でプレゼンテーションにできます。複数の図形は `with-drawing-group` でまとめます。

```lisp
;;; example: drawing-presentations
(defmodel city ()
  ((name :initarg :name :accessor city-name)
   (x :initarg :x :accessor city-x)
   (y :initarg :y :accessor city-y)))

(define-presentation-type city () :description "都市")

(defstate *cities* (list (make-instance 'city :name "東京" :x 140 :y 60)
                         (make-instance 'city :name "大阪" :x 80 :y 80)))

(define-command (show-city :label "都市を表示") ((city 'city))
  (format t "~A~%" (city-name city)))

(contain (make-instance 'output-pane
                        :view-width 200 :view-height 120
                        :display-callback
                        (lambda (pane)
                          (dolist (city *cities*)
                            (with-output-as-presentation (city 'city)
                              (with-drawing-group (pane)
                                (draw-circle pane (city-x city) (city-y city) 5 :filled t)
                                (draw-string pane (city-name city) (city-x city) (- (city-y city) 9)
                                             :anchor :middle)))))))
```

都市を右クリックすると「都市を表示」が出ます。`show-city` をメニューから呼ぶと、都市の図形がハイライトされてクリックで選べます。

## 10.6 ポインタの入力

`output-pane` の `:press-callback`、`:motion-callback`、`:release-callback` は、ポインタが押された、動いた、離されたときに、ペインと描画の座標 `x`、`y` を引数に呼ばれます。

```lisp
;;; example: drawing-input
(defstate *strokes* '())

(contain (make-instance 'output-pane
                        :view-width 200 :view-height 200
                        :press-callback (lambda (pane x y)
                                          (declare (ignore pane))
                                          (push (list x y) *strokes*))
                        :display-callback (lambda (pane)
                                            (dolist (point *strokes*)
                                              (draw-circle pane (first point) (second point) 2
                                                           :filled t)))))
```

## 10.7 graph-pane

`graph-pane` は、項目とその子を箱と線で結んだ図を自動で配置します。CAPI の graph-pane、CLIM の `format-graph-from-roots` にあたります。選択ペインなので、項目の選択、ダブルクリック、プレゼンテーションは `list-panel` と同じように使えます。

```lisp
;;; example: graph-pane
(defun subclasses (name)
  (cdr (assoc name '((:pane :choice :layout) (:choice :list-panel :tree-view) (:layout :row-layout)))))

(contain (make-instance 'graph-pane
                        :roots '(:pane)
                        :children-function #'subclasses
                        :print-function #'string-downcase
                        :direction :left-right))
```

`:direction` は `:left-right`（左から右、既定）か `:top-down`（上から下）です。複数の経路でたどり着く項目は1つだけ表示されます。
