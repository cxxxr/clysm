# 11. ストリームペインとインタラクタ

[目次](../README.md) | 前: [描画](10-graphics.md) | 次: [ウィンドウ](12-windows.md)

## 11.1 stream-pane

`stream-pane` は文字の出力ストリームです。`format`、`princ`、`print`、`terpri` などで書いたものが、そのまま表示されます。CLIM のペインと同じ考え方です。

```lisp
;;; example: stream-pane
(defvar *output* (make-instance 'stream-pane))

(contain *output*)
(format *output* "こんにちは~%")
(format *output* "1 + 2 = ~D~%" (+ 1 2))
(with-text-style (*output* :bold t :color "crimson")
  (princ "注意" *output*))
```

書くたびに、新しく書いた部分だけが画面に加わります。表示は最後の行が見えるようにスクロールします。`(window-clear pane)` で全部消えます。

### 書けるもの

| 書き方 | 結果 |
|---|---|
| `format`、`write-string`、`print` など | 文字 |
| `(present object type :stream pane)` | プレゼンテーション |
| `(with-output-as-presentation (pane object type) ...)` | 中で書いたものをプレゼンテーションにする |
| `(with-text-style (pane :bold t :italic t :color c :size s :class k) ...)` | 文字の書式 |
| `(formatting-table (pane) ...)` | 表（中で `formatting-row`、`formatting-cell`） |
| `(formatting-item-list (pane :columns n) ...)` | 項目を `n` 列に並べる（中で `formatting-cell`） |
| `(write-markup pane function)` | マークアップ（`function` が返す） |

```lisp
;;; example: formatting-table
(defmodel planet ()
  ((name :initarg :name :accessor planet-name)
   (moons :initarg :moons :accessor planet-moons)))

(define-presentation-type planet () :description "惑星")

(defmethod present-object ((planet planet) type)
  (planet-name planet))

(defvar *planets* (list (make-instance 'planet :name "地球" :moons 1)
                        (make-instance 'planet :name "火星" :moons 2)
                        (make-instance 'planet :name "木星" :moons 95)))

(defvar *pane* (make-instance 'stream-pane))
(contain *pane*)

(formatting-table (*pane*)
  (dolist (planet *planets*)
    (formatting-row (*pane*)
      (formatting-cell (*pane*) (present planet 'planet :stream *pane*))
      (formatting-cell (*pane* :align :right) (princ (planet-moons planet) *pane*)))))
```

### 表示関数

`:display-function` を指定すると、ペインはその関数が書いたものを表示します。関数はペインを引数に呼ばれ、読んだ状態が変わると、表示を消して呼び直されます。CLIM のアプリケーションペインの表示関数にあたります。

```lisp
;;; example: display-function
(defstate *queue* '("印刷" "保存"))

(contain (make-instance 'stream-pane
                        :display-function
                        (lambda (pane)
                          (format pane "待っている仕事: ~D 件~%" (length *queue*))
                          (dolist (job *queue*)
                            (format pane "・~A~%" job)))))
```

## 11.2 interactor-pane

`interactor-pane` は、ストリームペインの下にコマンド行を持つペインです。CLIM のインタラクタにあたります。

- コマンドを名前と引数で打って Enter で実行します。名前はコマンドの見出し（`:label` から `…` を除いたもの）か、関数名の `-` を空白にしたものです。
- 引数は空白で区切ります。空白を含む引数は `"..."` で囲みます。
- Tab で、コマンドの名前や引数を補完します。補完の候補は行の上に出ます。
- 上下の矢印キーで、前に打った行を呼び出します。
- コマンドが `*standard-output*` に書いたものは、インタラクタに表示されます。

```lisp
;;; example: interactor
(defstate *names* '("Ada" "Grace"))

(define-presentation-type person-name () :description "人")

(defmethod accept-from-string ((name (eql 'person-name)) type string)
  (let ((found (find string *names* :test #'string-equal)))
    (values found (and found t))))

(defmethod presentation-type-choices ((name (eql 'person-name)) type)
  *names*)

(define-command (greet :label "あいさつ") ((name 'person-name) (times 'integer :prompt "何回？"))
  (dotimes (i times)
    (format t "こんにちは、~A さん~%" name)))

(define-command (list-names :label "一覧") ()
  (dolist (name *names*)
    (present name 'person-name)
    (terpri)))

(contain (make-instance 'interactor-pane :prompt "コマンド:"))
```

このインタラクタでは、次のように打てます。

```
コマンド: 一覧
コマンド: あいさつ Ada 2
コマンド: あいさつ
```

最後の行のように引数が足りないと、コマンド行のプロンプトが引数の `:prompt` に変わり、そこで打つように求められます。プレゼンテーション（「一覧」の出力の名前など）をクリックして答えることもできます。Escape で取り消せます。

### 引数を打ち込めるようにする

文字列から引数を作るのは、総称関数 `accept-from-string` です。`string`、`integer`、`number`、`boolean`、`(member ...)` にはメソッドがあります。アプリケーションの型には、上の例のようにメソッドを定義します。`presentation-type-choices` のメソッドは、補完の候補を返します。

### インタラクタがあるときの引数の尋ね方

インタラクタが画面に表示されている間は、メニューやボタンから呼んだコマンドの引数も、ダイアログではなくインタラクタのコマンド行で尋ねます。インタラクタを隠すと（`:visible nil`）、ダイアログに戻ります。
