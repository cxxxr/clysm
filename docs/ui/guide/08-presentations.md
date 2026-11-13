# 8. プレゼンテーション

[目次](../README.md) | 前: [コマンドとメニュー](07-commands-and-menus.md) | 次: [ダイアログ](09-dialogs.md)

**プレゼンテーション** は、画面に表示したものと、それが表す Lisp のオブジェクトの組です。CLIM から借りた考え方で、次のことができるようになります。

- 型付きの引数を持つコマンドを引数なしで呼ぶと、その型のオブジェクトが画面上でハイライトされ、クリックすると引数になります。
- 画面上のオブジェクトを右クリックすると、そのオブジェクトに使えるコマンドのメニューが出ます。
- オブジェクトのクリックやドラッグ＆ドロップに、コマンドを割り当てられます。

## 8.1 プレゼンテーションを作る

```lisp
(present object [type])                                 ; オブジェクトを表示するマークアップ
(with-output-as-presentation (object [type]) markup)    ; マークアップをプレゼンテーションにする
```

選択ペイン（リスト、表、木、グラフ）では、`:presentation-type` を指定すると項目がプレゼンテーションになります。`:presentation-type t` は項目の型（`presentation-type-of`）を、関数は項目ごとの型を使います。描画（[描画](10-graphics.md)）やストリームペイン（[ストリームペインとインタラクタ](11-streams.md)）の中でも作れます。

```lisp
;;; example: presentations
(defmodel book ()
  ((title :initarg :title :accessor book-title)
   (author :initarg :author :accessor book-author)))

(define-presentation-type book () :description "本")

(defmethod present-object ((book book) type)
  (book-title book))

(defstate *books* (list (make-instance 'book :title "吾輩は猫である" :author "夏目漱石")
                        (make-instance 'book :title "銀河鉄道の夜" :author "宮沢賢治")))

(define-interface library ()
  ()
  (:panes
   (shelf list-panel :items (computed *books*) :print-function #'book-title
          :presentation-type 'book)
   (recommended application-pane
                :display-function (lambda (pane)
                                    (declare (ignore pane))
                                    (html (:p "おすすめ: " (present (first *books*) 'book)))))))

(display (make-instance 'library))
```

`present-object` は、オブジェクトをどう表示するかを決める総称関数です。文字列かマークアップを返します。

## 8.2 プレゼンテーションの型

プレゼンテーションの型は、クラスの名前か型指定子（`string`、`integer`、`(member :red :green)` など）です。`define-presentation-type` で、型に上位の型と説明を付けられます。

```lisp
(define-presentation-type book () :description "本")
(define-presentation-type ebook (book) :description "電子書籍")   ; ebook は book の一種
```

説明（`:description`）は、引数を尋ねるときのメッセージに使われます。型の関係は `presentation-subtypep` で調べられます。

## 8.3 型付きの引数を持つコマンド

`define-command` の必須の引数を `(名前 型 :prompt "...")` と書くと、その引数は型を持ちます。

```lisp
;;; example: typed-arguments
(defmodel book ()
  ((title :initarg :title :accessor book-title)))

(define-presentation-type book () :description "本")

(defstate *books* (list (make-instance 'book :title "吾輩は猫である")
                        (make-instance 'book :title "銀河鉄道の夜")))

(define-command (remove-book :menu "本" :label "削除…")
    ((book 'book :prompt "削除する本をクリックしてください"))
  (setf *books* (remove book *books*)))

(define-command (rename-book :menu "本" :label "名前を変える…")
    ((book 'book) (title 'string :prompt "新しい題名"))
  (setf (book-title book) title))

(contain (make-instance 'list-panel :items (computed *books*) :print-function #'book-title
                                    :presentation-type 'book)
         :menu-bar t)
```

このコマンドをメニューから（引数なしで）選ぶと、次のように進みます。

1. 画面の下にメッセージ（`:prompt`）が出て、画面上の `book` のプレゼンテーションがすべてハイライトされます。
2. ユーザーがどれかをクリックすると、それが引数になります。Escape か「キャンセル」で取り消せます。
3. 次の引数が `string`、`integer`、`number`、`boolean`、`(member ...)` なら、ダイアログで尋ねます。
4. 引数がそろうと、コマンドが実行されます。

インタラクタ（[ストリームペインとインタラクタ](11-streams.md)）が表示されているときは、引数はコマンド行で尋ねられ、打ち込むかクリックして答えます。

プログラムから引数を渡して呼ぶときは、尋ねません: `(invoke-command 'remove-book book)`。

### accept

`(accept type &key prompt continuation)` は、型 `type` のオブジェクトをユーザーに尋ね、答えを `continuation` に渡します。コマンドの引数を尋ねるのもこの関数です。すぐに戻るので、続きの処理は `continuation` に書きます。

```lisp
(accept 'book :prompt "本を選んでください"
              :continuation (lambda (book) (format t "~A~%" (book-title book))))
```

型ごとの尋ね方は、総称関数 `accept-presentation-type` のメソッドで決まります。`string` などにはダイアログで尋ねるメソッドがあり、それ以外は画面上のクリックで答えます。`(cancel-accept)` で取り消し、`(accepting-p)` で尋ねている最中かどうかを調べられます。

## 8.4 右クリックのメニュー

プレゼンテーションを右クリックすると、そのオブジェクトに使えるものを集めたメニューが出ます。

- そのオブジェクトの型に合う **トランスレータ**（8.5）
- 最初の引数の型がそのオブジェクトに合う、有効なコマンド

上の例では、本を右クリックすると「削除…」と「名前を変える…」が出ます。選ぶと、その本を最初の引数としてコマンドが実行されます（残りの引数は尋ねます）。

## 8.5 トランスレータ

`define-presentation-to-command-translator` は、プレゼンテーションに対する操作をコマンドに結びつけます。

```lisp
(define-presentation-to-command-translator open-book
    (book show-book :gesture :select :label "開く" :tester (lambda (book) (book-available-p book)))
    (object)
  (list object))
```

| 部分 | 意味 |
|---|---|
| `book` | 対象のプレゼンテーションの型 |
| `show-book` | 呼ぶコマンド |
| `:gesture` | `:select`（クリック。既定）か `:menu`（メニューだけ） |
| `:label` | メニューでの見出し（既定はコマンドの見出し） |
| `:tester` | オブジェクトを引数に、使えるときに真を返す関数 |
| `(object)` と本体 | 本体は、コマンドに渡す引数のリストを返します |

`:gesture :select` のトランスレータがあると、オブジェクトのクリックでそのコマンドが実行されます。

## 8.6 ドラッグ＆ドロップ

`define-drag-and-drop-translator` は、ある型のプレゼンテーションを別の型のプレゼンテーションにドロップしたときの操作を定義します。

```lisp
;;; example: drag-and-drop
(defmodel item ()
  ((name :initarg :name :accessor item-name)
   (place :initarg :place :accessor item-place)))

(define-presentation-type item ())
(define-presentation-type place ())

(defmethod present-object ((item item) type)
  (item-name item))

(defstate *items* (list (make-instance 'item :name "鍵" :place "机")
                        (make-instance 'item :name "財布" :place "鞄")))

(define-command move-item ((item 'item) (place 'place))
  (setf (item-place item) place))

(define-drag-and-drop-translator drop-item-on-place
    (item place move-item :tester (lambda (item place) (string/= (item-place item) place)))
    (object destination)
  (list object destination))

(contain (make-instance 'application-pane
                        :display-function
                        (lambda (pane)
                          (declare (ignore pane))
                          (html (:div (mapcar (lambda (place)
                                                (with-output-as-presentation (place 'place)
                                                  (:section (:h3 place)
                                                    (mapcar (lambda (item)
                                                              (when (string= (item-place item) place)
                                                                (present item 'item)))
                                                            *items*))))
                                              '("机" "鞄")))))))
```

ドラッグできるプレゼンテーションは、ドロップ先の型のトランスレータがあるものだけです。ドロップ先は、トランスレータが使える（`:tester` が真の）ときだけドロップを受け付けます。

## 8.7 ポインタの説明行

インターフェースに `:pointer-documentation t` を渡すと、下に1行の説明が付き、ポインタがプレゼンテーションの上にあるとき、そこでできる操作（クリックで実行されるコマンド、右クリックのメニュー、ドラッグ）が表示されます。文言は `*pointer-documentation-labels*` で変えられます。

```lisp
(setf *pointer-documentation-labels* '("クリック: ~A" "右クリック: メニュー" "ドラッグ: 移動"))
```

## 8.8 打ち込まれたオブジェクトを読む

インタラクタのコマンド行では、引数を文字列で打ち込めます。アプリケーションの型を打ち込めるようにするには、`accept-from-string` のメソッドを定義します。`presentation-type-choices` のメソッドを定義すると、Tab で補完できます。

```lisp
(defmethod accept-from-string ((name (eql 'book)) type string)
  (let ((book (find string *books* :key #'book-title :test #'string-equal)))
    (values book (and book t))))

(defmethod presentation-type-choices ((name (eql 'book)) type)
  (mapcar #'book-title *books*))
```
