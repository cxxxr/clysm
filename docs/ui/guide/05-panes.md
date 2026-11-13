# 5. ペイン

[目次](../README.md) | 前: [状態と画面の更新](04-state.md) | 次: [レイアウト](06-layouts.md)

ペインは画面の部品です。すべてのペインはクラス `pane` のサブクラスで、[共通の初期化引数](03-interfaces.md#34-共通の初期化引数)（`:title`、`:enabled`、`:width` など）を受け付けます。この章では種類ごとに説明します。初期化引数とアクセサの完全な一覧は [リファレンス](../reference/panes.md) にあります。

| 分類 | ペイン |
|---|---|
| 文字 | `title-pane`、`display-pane` |
| 入力 | `text-input-pane`、`password-pane`、`multi-line-text-input-pane`、`editor-pane`、`text-input-choice` |
| ボタン | `push-button`、`check-button`、`radio-button` |
| 選択 | `list-panel`、`multi-column-list-panel`、`tree-view`、`option-pane`、`radio-button-panel`、`check-button-panel`、`push-button-panel` |
| 範囲 | `slider`、`progress-bar` |
| その他 | `toolbar`、`application-pane`、`image-pane`、`browser-pane`、`color-pane` |
| 描画 | `output-pane`、`graph-pane`（[描画](10-graphics.md)） |
| ストリーム | `stream-pane`、`interactor-pane`（[ストリームペインとインタラクタ](11-streams.md)） |

## 5.1 文字を表示する

`title-pane` は1行の文字列、`display-pane` は複数行の文字列を表示します。`:text` には文字列のほか、数などを渡すと印字した形で表示します。

```lisp
;;; example: text-panes
(contain (make-instance 'column-layout
                        :description (list (make-instance 'title-pane :text "見出し")
                                           (make-instance 'display-pane
                                                          :text (format nil "1行目~%2行目"))
                                           (make-instance 'title-pane :text 42))))
```

## 5.2 文字を入力する

`text-input-pane` は1行の入力欄です。

| 初期化引数 | 意味 |
|---|---|
| `:text` | 欄の文字列。入力すると変わります |
| `:placeholder` | 空のときに薄く表示する文字列 |
| `:read-only` | 真なら書き換えられません |
| `:callback` | Enter が押されたときに、文字列をデータとして呼ばれます |
| `:change-callback` | 入力のたびに、文字列をデータとして呼ばれます |
| `:navigates` | 選択ペイン（リストなど）。この欄で上下の矢印キーを押すと、そのペインの選択が動きます |

`password-pane` は入力した文字を隠します。`multi-line-text-input-pane` は複数行（`:rows` で高さ）、`editor-pane` は等幅フォントの大きな入力欄です。`text-input-choice` は、`:items` の文字列から選ぶこともできる入力欄です。

```lisp
;;; example: text-input
(define-interface search-box ()
  ((log :initform "" :accessor search-log))
  (:panes
   (query text-input-pane :placeholder "検索語を入れて Enter"
          :callback-type :data
          :callback (lambda (text) (format t "検索: ~A~%" text)))
   (password password-pane :title "パスワード")
   (color text-input-choice :title "色" :items '("赤" "緑" "青") :text "赤")
   (notes multi-line-text-input-pane :title "メモ" :rows 3)
   (code editor-pane :title "コード" :text "(defun hello () 'world)" :rows 4)))

(display (make-instance 'search-box))
```

日本語入力（IME）で変換を確定する Enter は、`:callback` を呼びません。

## 5.3 ボタン

`push-button` は押すボタンです。

| 初期化引数 | 意味 |
|---|---|
| `:text` | ラベル |
| `:data` | コールバックに渡すデータ（既定はラベル） |
| `:callback` | 押されたときに呼ばれる関数 |
| `:command` | コマンドの名前か `(名前 引数...)`。押すとそのコマンドを実行し、ラベルとコマンドの有効・無効もコマンドに従います（[コマンドとメニュー](07-commands-and-menus.md)） |
| `:default-p` | ダイアログの既定のボタンとして強調します |

`check-button` はチェックボックスです。`:selected`（`button-selected`）が状態で、コールバックは選ばれているかどうかをデータとして呼ばれます。

`radio-button` は、同じインターフェースで同じ `:group` を持つものの中から1つだけが選ばれるボタンです。選ばれたときに、ラベルをデータとしてコールバックが呼ばれます。選択肢をまとめて作るなら `radio-button-panel`（5.4）が便利です。

```lisp
;;; example: buttons
(define-interface buttons-demo ()
  ((bold :initform nil :accessor bold)
   (size :initform "中" :accessor size))
  (:panes
   (ok push-button :text "OK" :default-p t
       :callback (lambda (data interface)
                   (declare (ignore interface))
                   (format t "~A が押されました~%" data)))
   (bold-check check-button :text "太字" :selected (binding (bold interface)))
   (small radio-button :text "小" :group :size
          :callback (lambda (text interface) (setf (size interface) text)))
   (medium radio-button :text "中" :group :size :selected t
           :callback (lambda (text interface) (setf (size interface) text)))
   (state title-pane :text (computed (format nil "太字: ~:[いいえ~;はい~]、大きさ: ~A"
                                             (bold interface) (size interface)))))
  (:layouts
   (main column-layout '(ok bold-check sizes state))
   (sizes row-layout '(small medium))))

(display (make-instance 'buttons-demo))
```

## 5.4 選択ペイン

項目を表示し、その中からユーザーが選ぶペインは、すべてクラス `choice` のサブクラスです。共通の初期化引数は次のとおりです。

| 初期化引数 | 意味 |
|---|---|
| `:items` | 項目のリスト。項目はどんなオブジェクトでもかまいません |
| `:print-function` | 項目を表示する文字列（またはマークアップ）を返す関数。既定は `princ` した文字列 |
| `:test` | 項目を比べる関数（既定は `eql`） |
| `:interaction` | `:single-selection`（1つ、既定）か `:multiple-selection`（複数） |
| `:selected-item`、`:selected-items` | 最初に選ばれている項目 |
| `:selection-callback` | 項目が選ばれたときに、その項目をデータとして呼ばれます |
| `:action-callback` | 項目がダブルクリックされたとき（または Enter）に呼ばれます |
| `:presentation-type` | 項目をプレゼンテーションにするときの型（[プレゼンテーション](08-presentations.md)） |

選ばれている項目は `choice-selected-item`（1つ）と `choice-selected-items`（リスト）で読み書きします。項目は `collection-items` で読み書きします。

### list-panel

縦に並んだリストです。矢印キー、Home、End で選択が動き、Ctrl（Cmd）でクリックすると追加、Shift でクリックすると範囲選択になります（複数選択のとき）。`:filter` を真（またはプレースホルダの文字列）にすると、上に絞り込みの欄が付きます。

```lisp
;;; example: list-panel
(define-interface fruit-chooser ()
  ((chosen :initform nil :accessor chosen))
  (:panes
   (fruits list-panel
           :items '(:apple :banana :cherry :grape :melon)
           :print-function #'string-capitalize
           :filter "絞り込み"
           :selection-callback (lambda (item interface) (setf (chosen interface) item))
           :action-callback (lambda (item interface)
                              (declare (ignore interface))
                              (format t "~A をダブルクリック~%" item)))
   (shown title-pane :text (computed (format nil "選択: ~A" (chosen interface))))))

(display (make-instance 'fruit-chooser))
```

### multi-column-list-panel

列のある表です。`:columns` は列の指定のリストで、各列は `(:title 見出し :width 幅 :adjust 揃え :sort 並べ替え)` です。`:column-function` は、項目からその行の列の値のリストを返す関数です。`:sort` を `t` にした列は、見出しをクリックすると昇順、降順、元の順と並べ替わります（数は大きさで、ほかは文字列で比べます）。`:sort` に関数を書くと、項目からその関数が返す値で並べます。

```lisp
;;; example: multi-column-list-panel
(contain (make-instance 'multi-column-list-panel
                        :items '(("Ada" 36 "London") ("Grace" 85 "New York") ("Alan" 41 "Manchester"))
                        :columns '((:title "名前" :width 120 :sort t)
                                   (:title "年齢" :adjust :right :sort t)
                                   (:title "都市"))
                        :column-function #'identity))
```

### tree-view

木です。`:roots` が最上位の項目、`:children-function` が項目からその子のリストを返す関数です。`:expandp-function` は、最初から開いておく項目について真を返す関数です。ユーザーは三角形のクリック、ダブルクリック、左右の矢印キーで開閉します。プログラムからは `(setf (tree-view-expanded-p tree item) t)` で開きます。

```lisp
;;; example: tree-view
(defun children (path)
  (cdr (assoc path '(("/" "/usr" "/home") ("/usr" "/usr/bin" "/usr/lib") ("/home" "/home/ada"))
              :test #'string=)))

(contain (make-instance 'tree-view
                        :roots '("/")
                        :children-function #'children
                        :expandp-function (lambda (path) (string= path "/"))
                        :test #'string=))
```

### option-pane

ドロップダウンメニューで1つを選びます。何も選んでいなければ最初の項目が選ばれています。

### radio-button-panel、check-button-panel、push-button-panel

項目ごとにボタンを並べます。`radio-button-panel` は1つ、`check-button-panel` は複数を選びます。`push-button-panel` は押した項目をデータとして `:selection-callback` を呼びます（選択の状態は持ちません）。`:orientation` は `:row`（横、既定）か `:column`（縦）です。

```lisp
;;; example: button-panels
(define-interface options-demo ()
  ()
  (:panes
   (size option-pane :title "大きさ" :items '(:small :medium :large)
         :print-function #'string-downcase :selected-item :medium)
   (color radio-button-panel :title "色" :items '("赤" "緑" "青") :test #'equal)
   (extras check-button-panel :title "追加" :items '(:cheese :bacon) :orientation :column
           :print-function #'string-downcase)
   (actions push-button-panel :items '("保存" "破棄") :callback-type :data
            :selection-callback (lambda (item) (format t "~A~%" item)))))

(display (make-instance 'options-demo))
```

## 5.5 範囲

`slider` と `progress-bar` は、`:start` から `:end` の間の値（`:value`、`range-value`）を表示します。`slider` はユーザーが動かせて（`:step` 刻み）、動かすたびに値をデータとして `:callback` が呼ばれます。

```lisp
;;; example: ranges
(defstate *volume* 30)

(contain (make-instance 'column-layout
                        :description (list (make-instance 'slider :start 0 :end 100
                                                                  :value (binding *volume*))
                                           (make-instance 'progress-bar :start 0 :end 100
                                                                        :value (computed *volume*)))))
```

## 5.6 その他のペイン

### toolbar

コマンドのボタンを横に並べます。`:items` には、コマンドの名前、`(コマンド 引数...)`、ペイン、区切りの `:separator` を並べます。

### application-pane

`:display-function` が返すマークアップを表示するペインです。関数はペインを引数に呼ばれ、読んだ状態が変わると呼び直されます。マークアップについては [コンポーネントとマークアップ](13-components.md) を見てください。

```lisp
;;; example: application-pane
(defstate *messages* '("こんにちは" "元気ですか"))

(contain (make-instance 'application-pane
                        :display-function
                        (lambda (pane)
                          (declare (ignore pane))
                          (html (:ul (mapcar (lambda (message) (:li message)) *messages*))))))
```

### image-pane、browser-pane、color-pane

`image-pane` は `:url` の画像を、`browser-pane` は `:url` のページを表示します。`image-pane` の `:description` は画像の代わりの説明です。`color-pane` は色を選ぶ欄で、`:color`（`"#3366cc"` の形）が値、変わると色をデータとして `:callback` が呼ばれます。

## 5.7 ペインの状態を変える

ペインのアクセサを `setf` すると、画面はすぐ（`flush` のときに）変わります。

```lisp
(setf (button-text button) "停止")
(setf (pane-enabled button) nil)               ; 使えなくする
(setf (pane-visible panel) nil)                ; 隠す
(setf (collection-items list) new-items)       ; 項目を入れ替える
(setf (choice-selected-item list) item)        ; 選ぶ
(setf (title-pane-text title) "完了")
```

ペインの状態をアプリケーションのデータに合わせたいときは、`setf` で同期するより、初期化引数に `computed` や `binding` を書く方が確実です（[状態と画面の更新](04-state.md)）。
