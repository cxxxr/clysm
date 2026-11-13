# 13. コンポーネントとマークアップ

[目次](../README.md) | 前: [ウィンドウ](12-windows.md) | 次: [デスクトップアプリケーション](14-desktop.md)

ペインは、この章で説明する下の層の上に作られています。HTML の要素を直接並べたいとき、新しい種類のペインを作るとき、ペインを使わない軽いページを作るときに、この層を使います。

## 13.1 マークアップ

`html` マクロの中（と `defcomponent`、`application-pane` の表示関数、`render-pane` のメソッドの中）では、キーワードで始まるリストが HTML の要素になります。

```lisp
(:tag :property value ... child ...)
```

```lisp
;;; example: markup
(defstate *todos* '("牛乳" "パン"))

(contain (make-instance 'application-pane
                        :display-function
                        (lambda (pane)
                          (declare (ignore pane))
                          (html
                            (:div :class "todo"
                              (:h2 "買い物")
                              (:ul (mapcar (lambda (todo) (:li todo)) *todos*))
                              (when (null *todos*)
                                (:p "何もありません"))
                              (:p (length *todos*) " 件"))))))
```

- 要素の名前の後に、プロパティとその値を並べます。値は評価されます。
- 子は、要素、文字列、数（文字列にして表示）、`nil`（何もない）、子のリスト（展開されます）です。
- それ以外の部分はふつうの Lisp なので、条件は `if`・`when`、繰り返しは `mapcar`・`loop` で書きます。

### プロパティ

| プロパティ | 意味 |
|---|---|
| `:class`、`:id`、`:title`、`:value`、`:checked`、`:disabled`、`:hidden`… | 要素のプロパティ（名前を小文字にしたもの）。値が `nil` なら外します |
| `:style`、`:aria-label` など `-` を含む名前 | 属性 |
| `:on-click`、`:on-input`、`:on-keydown` など `:on-` で始まる名前 | イベントのリスナー。イベントを引数に呼ばれます |
| `:on-mount` | 要素が画面に置かれたときに、バックエンドのノード（DOM の要素）を引数に呼ばれます |
| `:key` | 兄弟の中で要素を見分けるキー（13.3） |

リスナーの中では、イベントのプロパティを `(event-get *backend* event "key")` で、イベントが起きた要素のプロパティを `(event-target-get *backend* event "value")` で読めます。こう書くと S 式バックエンドでもテストできます。DOM だけで動かすなら `(js:get event "key")` でもかまいません。

## 13.2 コンポーネント

`defcomponent` は、マークアップを返す関数と、画面上のインスタンスのクラスを定義します。

```lisp
;;; example: defcomponent
(defmodel todo ()
  ((text :initarg :text :accessor todo-text)
   (done :initarg :done :initform nil :accessor todo-done)))

(defstate *todos* (list (make-instance 'todo :text "牛乳を買う")
                        (make-instance 'todo :text "手紙を書く")))

(defcomponent todo-item (todo &state (hover nil))
  (:li :class (if (todo-done todo) "done" nil)
       :on-mouseenter (lambda (event) (declare (ignore event)) (setf hover t))
       :on-mouseleave (lambda (event) (declare (ignore event)) (setf hover nil))
    (:input :type "checkbox" :checked (todo-done todo)
            :on-change (lambda (event)
                         (declare (ignore event))
                         (setf (todo-done todo) (not (todo-done todo)))))
    (todo-text todo)
    (when hover
      (:button :on-click (lambda (event)
                           (declare (ignore event))
                           (setf *todos* (remove todo *todos*)))
               "×"))))

(defcomponent todo-list ()
  (:div
    (:ul (mapcar (lambda (todo) (keyed todo (todo-item todo))) *todos*))
    (:p (count-if-not #'todo-done *todos*) " 件残っています")))

(mount (todo-list) "#app")
```

- 引数リストの `&state` の後の変数（`hover`）は、画面上のインスタンスごとの状態です。`setf` すると、そのコンポーネントが描き直されます。
- 本体の中では、`self` がインスタンスを指します。
- コンポーネントは、読んだ状態が変わったときと、引数が変わった（`equal` でない）ときに描き直されます。引数が同じなら、親が描き直されても描き直されません。

`(mount node container)` は、コンポーネント（やマークアップ）を `container` に置き、`unmount` に渡すノードを返します。

### メソッド

コンポーネントのクラスには、`mounted`（画面に置かれた後）と `unmounted`（外された後）のメソッドを定義できます。描画は総称関数 `render` です。

```lisp
(defmethod mounted ((self todo-item))
  (format t "置かれました~%"))
```

## 13.3 キーと描き直し

描き直しでは、新しいマークアップを前のものと比べ、違うところだけをページに反映します。兄弟の要素は順番で対応付けられますが、`keyed` か `:key` でキーを付けると、キーで対応付けられます。並べ替えたり途中に挿入したりするリストでは、キーを付けると、コンポーネントの状態（`&state`）や入力欄の内容が正しい項目に付いて移動します。

```lisp
(mapcar (lambda (todo) (keyed todo (todo-item todo))) *todos*)
```

## 13.4 新しい種類のペインを作る

ペインは、`pane` のサブクラス（`defmodel`）と、そのペインを表示するマークアップを返す `render-pane` のメソッドで作ります。

```lisp
;;; example: new-pane
(defmodel rating-pane (pane callbacks)
  ((value :initarg :value :initform 0 :accessor rating-value)
   (maximum :initarg :maximum :initform 5 :accessor rating-maximum)
   (callback :initarg :callback :initform nil :accessor rating-callback))
  (:documentation "星の数で評価を入力するペイン。"))

(defmethod render-pane ((pane rating-pane))
  (html
    (:span :class "rating"
      (loop for i from 1 to (rating-maximum pane)
            collect (let ((i i))
                      (:button :type "button" :class "star"
                               :on-click (lambda (event)
                                           (declare (ignore event))
                                           (setf (rating-value pane) i)
                                           (when (rating-callback pane)
                                             (funcall (rating-callback pane) i)))
                        (if (<= i (rating-value pane)) "★" "☆")))))))

(defstate *score* 3)

(contain (make-instance 'column-layout
                        :description (list (make-instance 'rating-pane :value (binding *score*)
                                                                       :title "評価")
                                           (make-instance 'title-pane
                                                          :text (computed (format nil "~D 点" *score*))))))
```

- `render-pane` が返すのは要素1つです。共通の初期化引数（`:title`、`:enabled`、`:width` など）はフレームワークが処理するので、メソッドで扱う必要はありません。
- スロットは観測されるので、`(setf (rating-value pane) i)` だけで表示が変わり、`computed` や `binding` も使えます。
- `callbacks` を継承すると、`:callback-type` を受け付けます。コールバックを `:callback-type` に従って呼ぶには、ペインのクラスに合わせてデータとインターフェースを渡してください（組み込みのペインは内部の関数 `call-callback` を使っています）。
- 画面に置かれたときと外されたときに何かするには、`pane-mounted` と `pane-unmounted` のメソッドを定義します。

### ペインをマークアップに入れる

`(pane-node pane)` は、ペインを表示するノードを返します。マークアップの中にペインを置くのに使います。

```lisp
(html (:div :class "sidebar" (pane-node my-list) (pane-node my-button)))
```

## 13.5 フレームワークを使わない部分との共存

ページにはほかの HTML を置いてかまいません。`display` や `mount` は、指定した要素の中だけを管理します。JavaScript の値やライブラリは、Clysm の `JS` パッケージ（`js:global`、`js:call`、`js:get`、`js:set`、`js:await`）で使えます（Clysm の README の「JavaScript interoperability」）。
