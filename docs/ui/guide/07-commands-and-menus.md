# 7. コマンドとメニュー

[目次](../README.md) | 前: [レイアウト](06-layouts.md) | 次: [プレゼンテーション](08-presentations.md)

アプリケーションの操作（保存する、削除する、名前を変える）は **コマンド** として定義します。1つのコマンドを、メニュー、ボタン、ツールバー、キー、右クリックのメニュー、コマンドパレット、コマンド行、REPL のどこからでも呼べます。

## 7.1 define-command

```lisp
(define-command name lambda-list body...)
(define-command (name :label "..." :menu "..." :key "..." :separator t :enabled form :table 'name)
    lambda-list
  body...)
```

`define-command` は、`name` という関数を定義し、それをコマンドとして登録します。

| オプション | 意味 |
|---|---|
| `:label` | メニュー項目やボタンの文字列（既定は名前から作ります） |
| `:menu` | 入るメニューの見出し。コマンドのメニューバーに出ます |
| `:key` | キー。`"CmdOrCtrl+S"`、`"Alt+N"`、`"CmdOrCtrl+Shift+Backspace"` など |
| `:separator` | 真なら、メニューでこの項目の上に区切り線を引きます |
| `:enabled` | 使えるときに真になる式。使えないとき、メニュー項目やボタンは灰色になり、キーも効きません |
| `:table` | コマンドテーブルの名前（7.5） |

`:enabled` の式は必要なときに評価され、読んだ状態は観測されます。状態が変わると、ボタンやメニューの有効・無効も変わります。

```lisp
;;; example: define-command
(defstate *document* "")
(defstate *saved* "")

(define-command (save-document :menu "ファイル" :label "保存" :key "CmdOrCtrl+S"
                               :enabled (string/= *document* *saved*))
    ()
  (setf *saved* *document*)
  (format t "保存しました~%"))

(define-command (clear-document :menu "編集" :label "すべて消去" :separator t
                                :enabled (plusp (length *document*)))
    ()
  (setf *document* ""))

(contain (make-instance 'column-layout
                        :description (list (make-instance 'multi-line-text-input-pane
                                                          :text (binding *document*))
                                           (make-instance 'toolbar
                                                          :items '(save-document clear-document))))
         :menu-bar t)
(install-commands *backend*)
```

`install-commands` は、コマンドのキーを有効にします（デスクトップ版ではメニューバーも作ります）。プログラムの最後に1回呼んでください。

## 7.2 コマンドを呼ぶ

| 方法 | 書き方 |
|---|---|
| プログラムから（REPL を含む） | `(save-document)`。ただの関数呼び出しで、有効かどうかは確かめません |
| ユーザーの操作として | `(invoke-command 'save-document)`。有効なときだけ呼び、エラーはダイアログで表示し、足りない引数はユーザーに尋ねます |
| ボタン | `(make-instance 'push-button :command 'save-document)`。ラベルと有効・無効がコマンドに従います |
| ツールバー | `(make-instance 'toolbar :items '(save-document :separator (paint :red)))` |
| マークアップのボタン | `(command-button 'save-document)`、または `:on-click (command save-document)` |
| メニュー | `:menu` を指定するか、インターフェースのメニューに名前を書く（7.4） |
| キー | `:key` |

コマンドが有効かどうかは `(command-enabled-p 'save-document)` で調べられます。`(find-command 'save-document)` はコマンドのオブジェクトを返し、`command-label` などで中身を読めます。

### 引数のあるコマンド

コマンドは引数を取れます。`(paint :red)` のように、ボタンやツールバーでは引数を一緒に書きます。

```lisp
(define-command paint (color)
  (setf *color* color))

(make-instance 'push-button :text "赤" :command '(paint :red))
```

必須の引数に型を書くと、引数なしで呼ばれたときにユーザーに尋ねるようになります。これは [プレゼンテーション](08-presentations.md) の章で説明します。

```lisp
(define-command (rename-file :menu "ファイル") ((file 'file) (name 'string :prompt "新しい名前"))
  ...)
```

## 7.3 コマンドのメニューバー

インターフェースに `:menu-bar t` を渡すと、`:menu` を持つコマンドから作ったメニューバーが付きます。メニューはコマンドを定義した順に並びます。

ブラウザ版ではページの上部に、デスクトップ版ではアプリケーションのメニューバーに表示されます。

## 7.4 インターフェースのメニュー

インターフェースごとのメニューは、`define-interface` の `:menus` と `:menu-bar` で定義します。

```lisp
;;; example: interface-menus
(define-command (zoom-in :label "拡大" :key "CmdOrCtrl+=") () (format t "拡大~%"))
(define-command (zoom-out :label "縮小" :key "CmdOrCtrl+-") () (format t "縮小~%"))

(define-interface viewer ()
  ((wrap :initform nil :accessor wrap)
   (dirty :initform t :accessor dirty))
  (:panes
   (text multi-line-text-input-pane :text "本文" :pane-menu 'view-menu))
  (:menus
   (file-menu "ファイル(&F)"
              (("保存(&S)" :key "CmdOrCtrl+S"
                           :enabled (computed (dirty interface))
                           :callback (lambda (data interface)
                                       (declare (ignore data))
                                       (setf (dirty interface) nil)))
               export-menu
               :separator
               ("閉じる(&C)" :callback (lambda (data interface)
                                         (declare (ignore data))
                                         (quit-interface interface)))))
   (export-menu "書き出し(&E)"
                (("テキスト(&T)" :data :text :callback-type :data
                                 :callback (lambda (format) (format t "~A で書き出し~%" format)))
                 ("HTML(&H)" :data :html :callback-type :data
                             :callback (lambda (format) (format t "~A で書き出し~%" format)))))
   (view-menu "表示(&V)"
              (("折り返す(&W)" :checked (binding (wrap interface)) :key "Alt+W")
               :separator
               zoom-in zoom-out)))
  (:menu-bar file-menu view-menu)
  (:default-initargs :title "ビューア"))

(display (make-instance 'viewer))
(install-commands *backend*)
```

### メニュー

`:menus` の各要素は `(名前 見出し (項目...) 初期化引数...)` です。メニューはインターフェースのスロット（`名前`）に入ります。見出しの `&` の次の文字は **ニーモニック** で、下線付きで表示され、Alt とその文字でメニューが開きます。

項目には次のものを書けます。

| 項目 | 意味 |
|---|---|
| `("見出し" 初期化引数...)` | メニュー項目（`menu-entry`） |
| メニューの名前 | そのメニューが、この項目から横に開きます（サブメニュー） |
| コマンドの名前 | コマンドの項目（見出し、キー、有効・無効はコマンドに従います） |
| `(コマンド 引数...)` | 引数付きのコマンドの項目 |
| `:separator` | 区切り線 |

### メニュー項目の初期化引数

| 初期化引数 | 意味 |
|---|---|
| `:callback` | 選ばれたときに呼ばれる関数（`:callback-type` が既定ならデータとインターフェース） |
| `:data` | コールバックのデータ（既定は見出し） |
| `:key` | キー |
| `:enabled` | 偽なら灰色になり選べません |
| `:checked` | チェックマーク。`:none`（既定）以外にすると、選ぶたびに切り替わり、コールバックは新しい状態をデータとして呼ばれます |

`:enabled` と `:checked` には `computed` と `binding` を使えます。上の例の「折り返す」は、チェックマークがインターフェースの `wrap` スロットそのものになっています。

### メニューバー

`(:menu-bar file-menu view-menu)` は、メニューバーに並べるメニューの名前です。`:commands` を書くと、その位置にコマンドのメニュー（7.3）が入ります。

### ペインのメニュー

ペインの `:pane-menu` は右クリックで出るメニューです。メニュー、メニューの名前、またはペインを引数にポップアップメニューの項目のリストを返す関数を書けます。

### キー

キーの書き方は Electron の accelerator と同じです。修飾キーは `CmdOrCtrl`（macOS では Command、ほかでは Ctrl）、`Ctrl`、`Alt`（`Option`）、`Shift` で、`+` でつなぎます。キーはコマンド、メニュー項目の順に探されます。デスクトップ版では、メニューにある項目のキーはネイティブのメニューが処理します。

## 7.5 コマンドテーブル

`define-command` に `:table 'name` を書いたコマンドは、`:command-table 'name` を持つインターフェースが表示されている間だけ、メニューに出てキーが効きます。インターフェースごとに使えるコマンドを分けるのに使います。

```lisp
;;; example: command-tables
(define-command (rotate-image :menu "画像" :key "CmdOrCtrl+R" :table 'image-editor) ()
  (format t "回転~%"))

(define-interface image-editor ()
  ()
  (:panes (canvas title-pane :text "画像"))
  (:default-initargs :command-table 'image-editor :menu-bar t))

(display (make-instance 'image-editor))
```

## 7.6 ポップアップメニュー

`(popup-menu items x y)` は、ページの位置 `x`、`y` にメニューを出します。項目は、メニュー項目（`make-menu-item`）、コマンドの名前、`(コマンド 引数...)`、`:separator` です。

```lisp
(popup-menu (list (make-menu-item "コピー" (lambda () (copy-selection)))
                  :separator
                  'delete-selection)
            (event-get *backend* event "clientX") (event-get *backend* event "clientY"))
```

`(make-menu-item label function &key enabled key checked items)` の `items` に項目のリストを渡すと、サブメニューになります。

画面上のオブジェクトを右クリックしたときのメニューは、プレゼンテーションが自動で作ります（[プレゼンテーション](08-presentations.md)）。

## 7.7 コマンドパレット

`command-palette`（Ctrl+Shift+P）は、使えるコマンドを名前の一部で探して実行するダイアログです。コマンドとして定義されているので、メニューやボタンにも置けます。コマンド行で名前と引数を打つ **インタラクタ** は [ストリームペインとインタラクタ](11-streams.md) で説明します。
