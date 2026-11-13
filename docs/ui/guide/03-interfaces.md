# 3. インターフェース

[目次](../README.md) | 前: [使い始める](02-getting-started.md) | 次: [状態と画面の更新](04-state.md)

インターフェースは、ペインをレイアウトで並べた1つの画面です。CAPI の interface にあたります。ページ全体をインターフェースにするのがふつうですが、ページの一部やダイアログ、ページ内のウィンドウにもなります。

## 3.1 define-interface

```lisp
(define-interface name (superclass...)
  (slot...)
  (:panes (pane-name pane-class initarg...)...)
  (:layouts (layout-name layout-class description initarg...)...)
  (:menus (menu-name title (item...) initarg...)...)
  (:menu-bar menu-name...)
  (:default-initargs ...)
  (:documentation "..."))
```

`define-interface` は、`defmodel`（[状態と画面の更新](04-state.md)）にペインとレイアウトの定義を加えたものです。

- **スロット**は `defclass` と同じように書きます。ペインのスロットと同じく観測されるので、画面がスロットに追従します。
- **`:panes`** の各要素は、ペインの名前、クラス、`make-instance` に渡す初期化引数です。インスタンスを作るたびにペインが作られ、名前のスロットに入ります。
- **`:layouts`** の各要素は、レイアウトの名前、クラス、記述（並べるペインの名前のリスト）、初期化引数です。最初のレイアウトがインターフェースに表示されます。
- **`:menus`** と **`:menu-bar`** は [コマンドとメニュー](07-commands-and-menus.md) で説明します。
- ペインとレイアウトの初期化引数に `:accessor` や `:reader` を書くと、そのスロットのアクセサになります。

```lisp
;;; example: interface-basics
(define-interface login-window ()
  ((attempts :initform 0 :accessor login-attempts))
  (:panes
   (user text-input-pane :title "ユーザー名" :accessor login-user)
   (password password-pane :title "パスワード")
   (status title-pane
           :text (computed (if (zerop (login-attempts interface))
                               ""
                               (format nil "~D 回失敗しました" (login-attempts interface)))))
   (ok push-button :text "ログイン" :default-p t
       :enabled (computed (plusp (length (text-input-pane-text user))))
       :callback (lambda (data interface)
                   (declare (ignore data))
                   (incf (login-attempts interface)))))
  (:layouts
   (main column-layout '(user password buttons status))
   (buttons row-layout '(ok) :adjust :end))
  (:default-initargs :title "ログイン"))

(display (make-instance 'login-window))
```

### 初期化引数の式

ペインとレイアウトの初期化引数は、インターフェースのインスタンスを作るときに評価されます。その中では次の名前が使えます。

- `interface`: 作っているインターフェース自身。
- インターフェースのスロット名: その値（ペインも含みます）。

ペインは書いた順に作られるので、初期化引数の式の中で直接使えるのは、それより前のペインです。`(computed ...)` の中身は後で（表示するときに）評価されるので、すべてのペインを使えます。上の例で `ok` の `:enabled` が `user` を参照しているのはこのためです。

### レイアウトがない場合

`:layouts` を書かないと、ペインを書いた順に縦に並べます。

```lisp
;;; example: interface-without-layouts
(define-interface note ()
  ()
  (:panes
   (heading title-pane :text "メモ")
   (body multi-line-text-input-pane :text "ここに書く")))

(display (make-instance 'note))
```

## 3.2 表示する

| 関数 | 説明 |
|---|---|
| `(display interface &key container window x y width height)` | インターフェースを表示し、そのインターフェースを返します。 |
| `(contain pane &rest initargs)` | ペインを1つ、そのために作ったインターフェースで表示します。 |
| `(quit-interface interface)` | 表示をやめます。ペインを渡すと、そのペインのインターフェースを閉じます。 |

`display` の `container` は、表示する場所です。DOM バックエンドでは CSS セレクタ（既定は `"#app"`）か DOM の要素です。`:window t` を渡すと、ページの中に移動できるウィンドウとして表示します（[ウィンドウ](12-windows.md)）。ダイアログとして表示するには `display-dialog` を使います（[ダイアログ](09-dialogs.md)）。

インターフェースのタイトル（`:title`、`interface-title`）は、ページに表示したときはブラウザのウィンドウのタイトルに、ウィンドウやダイアログのときはその見出しになります。

`contain` は、ペインを1つ試しに表示するときに便利です。

```lisp
;;; example: contain
(contain (make-instance 'list-panel :items '("りんご" "みかん" "ぶどう"))
         :title "果物")
```

`contain` の初期化引数はインターフェースに渡されます（`:title`、`:menu-bar` など）。`:container` で表示する場所を変えられます。

## 3.3 ペインを使う

ペインはインターフェースのスロットに入っているので、`with-slots` やアクセサ、`find-pane` で取り出せます。

```lisp
;;; example: find-pane
(define-interface counter ()
  ()
  (:panes
   (value title-pane :text "0")
   (up push-button :text "+1"
       :callback (lambda (data interface)
                   (declare (ignore data))
                   (let ((value (find-pane interface 'value)))
                     (setf (title-pane-text value)
                           (princ-to-string (1+ (parse-integer (title-pane-text value))))))))))

(display (make-instance 'counter))
```

ペインのアクセサを `setf` すると、その場で画面が変わります。ただし、この例のように値をペインの中だけに持つより、インターフェースのスロットに持って `computed` で表示する方が、プログラムが単純になります（[状態と画面の更新](04-state.md)）。

ペインから自分のインターフェースへは `pane-interface` でたどれます。

## 3.4 共通の初期化引数

すべてのペインとインターフェースは、クラス `pane` の初期化引数を受け付けます。

| 初期化引数 | 意味 |
|---|---|
| `:title` | ペインと一緒に表示するラベル |
| `:title-position` | ラベルの位置: `:top`（上、既定）、`:left`（左）、`:frame`（枠で囲む） |
| `:enabled` | 偽にすると使えなくなります（灰色になる） |
| `:visible` | 偽にすると隠れます |
| `:help` | ツールチップの文字列 |
| `:class` | 要素に加える CSS クラス |
| `:width`、`:height` | 大きさ: ピクセル数か CSS の長さ（`"20em"` など） |
| `:min-width`、`:min-height`、`:max-width`、`:max-height` | 大きさの範囲 |
| `:stretch` | 行や列の空いた場所を使うか: `t` か重み（数） |
| `:pane-menu` | 右クリックのメニュー（[コマンドとメニュー](07-commands-and-menus.md)） |

インターフェース独自の初期化引数は次のとおりです。

| 初期化引数 | 意味 |
|---|---|
| `:layout` | 表示するペインかレイアウト、またはその名前 |
| `:menu-bar` | メニューバー（`t` でコマンドのメニュー） |
| `:command-table` | このインターフェースが表示されている間だけ有効なコマンドテーブルの名前 |
| `:pointer-documentation` | 真にすると、ポインタの位置でできる操作の説明行を下に表示します |
| `:destroy-callback` | 閉じられたときにインターフェースを引数に呼ばれる関数 |

## 3.5 コールバック

ボタンやリストなど、ユーザーの操作に応えるペインはコールバックを持ちます。コールバックの引数は、ペインの `:callback-type` で決まります。

| `:callback-type` | 引数 |
|---|---|
| `(:data :interface)`（既定） | データとインターフェース |
| `:data` | データだけ |
| `:interface` | インターフェースだけ |
| `:element` | ペイン自身だけ |
| `:item-element` など | キーワードを `-` でつないだものは、その順の引数 |
| `:none` | 引数なし |

データはペインによって違います。ボタンなら `:data`（既定は文字列）、チェックボタンなら選ばれているかどうか、リストなら選ばれた項目、テキスト欄なら文字列です。

```lisp
;;; example: callback-types
(define-interface callbacks-demo ()
  ((log :initform '() :accessor demo-log))
  (:panes
   (a push-button :text "A" :callback-type :data
      :callback (lambda (data) (format t "押されました: ~A~%" data)))
   (b push-button :text "B" :data :b :callback-type :none
      :callback (lambda () (format t "B~%")))
   (fruits list-panel :items '(:apple :orange)
           :callback-type :item-interface
           :selection-callback (lambda (item interface)
                                 (push item (demo-log interface))))
   (shown title-pane :text (computed (format nil "~S" (demo-log interface))))))

(display (make-instance 'callbacks-demo))
```

## 3.6 閉じる

`quit-interface` はインターフェースを画面から外し、`:destroy-callback` を呼びます。ページに表示したインターフェースを閉じると、そのページの `#app` は空になります。

```lisp
;;; example: quit
(define-interface closable ()
  ()
  (:panes
   (close push-button :text "閉じる"
          :callback (lambda (data interface)
                      (declare (ignore data))
                      (quit-interface interface))))
  (:default-initargs
   :destroy-callback (lambda (interface)
                       (declare (ignore interface))
                       (format t "閉じました~%"))))

(display (make-instance 'closable))
```

画面に表示されているインターフェースは、新しいものから順に `*interfaces*` に入っています。
