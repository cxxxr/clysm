# 4. 状態と画面の更新

[目次](../README.md) | 前: [インターフェース](03-interfaces.md) | 次: [ペイン](05-panes.md)

このフレームワークでは、画面を書き換える命令を出しません。画面は状態から計算され、状態が変わると、それを読んでいた部分が自動的に描き直されます。この章では、その仕組みと使い方を説明します。

## 4.1 モデル: defmodel

`defmodel` は `defclass` と同じ書き方でクラスを定義します。違いは、スロットの読み書きが観測されることです。

```lisp
;;; example: defmodel
(defmodel task ()
  ((title :initarg :title :accessor task-title)
   (done :initarg :done :initform nil :accessor task-done)))

(defvar *task* (make-instance 'task :title "牛乳を買う"))

(contain (make-instance 'check-button
                        :text (computed (task-title *task*))
                        :selected (binding (task-done *task*))))

(setf (task-title *task*) "パンを買う")     ; 画面のチェックボタンの文字が変わる
```

画面を描いている間にモデルのスロットを読むと、その部分（コンポーネント）がスロットに記録されます。後でスロットに `setf` すると、記録された部分が「描き直しが必要」になり、少し後（イベントの処理が終わったとき）にまとめて描き直されます。描き直しは、新しい描画結果を前のものと比べて、違うところだけをページに反映します。

ペイン、メニュー、インターフェースはすべて `defmodel` で定義されたクラスです。

### スロットの変更として見えるもの

観測されるのは、スロットへの代入です。スロットに入っているリストや配列の中身を書き換えても、変更とはみなされません。

```lisp
(push item (todo-items list))            ; 見える: スロットに新しいリストを代入する
(setf (todo-items list) (remove x (todo-items list)))   ; 見える
(setf (car (todo-items list)) item)      ; 見えない: リストの中身を書き換えている
(sort (todo-items list) #'<)             ; 見えない（しかも元のリストを壊す）
```

リストを変えるときは、新しいリストを作ってスロットに代入してください。

## 4.2 大域的な状態: defstate

`defstate` は、観測される大域変数を定義します。

```lisp
;;; example: defstate
(defstate *temperature* 20 "今の温度")

(contain (make-instance 'title-pane
                        :text (computed (format nil "~D 度" *temperature*))))

(incf *temperature* 5)          ; 表示が「25 度」になる
```

`defstate` で定義した名前はシンボルマクロなので、`setf`、`incf`、`push` などがそのまま使えます。ただし `let` で束縛すると、それはただの局所変数になります（動的束縛にはなりません）。

## 4.3 computed: ほかの状態から計算される値

モデルのスロット（ペインのスロットを含む）には、`(computed form)` を入れられます。スロットを読むたびに `form` が評価され、その値がスロットの値になります。`form` が読んだ状態が変わると、そのスロットを読んでいた部分も描き直されます。

```lisp
;;; example: computed
(define-interface temperature-converter ()
  ((celsius :initform 20 :accessor celsius))
  (:panes
   (input slider :start -20 :end 40
          :value (binding (celsius interface)))
   (shown title-pane
          :text (computed (format nil "~D ℃ = ~,1F ℉"
                                  (celsius interface)
                                  (+ 32 (* 9/5 (celsius interface))))))))

(display (make-instance 'temperature-converter))
```

`computed` のスロットに `setf` すると、`computed` は普通の値で置き換えられます。

## 4.4 binding: ほかの場所と同じ値

`(binding place)` をスロットに入れると、そのスロットは `place` そのものになります。スロットを読むと `place` を読み、スロットに書くと `place` に書きます。

上の例で、スライダーの `:value` は `(binding (celsius interface))` です。スライダーを動かすと、スライダーの値の代わりにインターフェースの `celsius` スロットが変わり、`shown` の表示が変わります。逆に `(setf (celsius converter) 30)` とすると、スライダーも動きます。

`binding` は、モデルのスロットを編集するフォームを作るときに特に便利です。

```lisp
;;; example: binding-form
(defmodel person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)
   (admin :initarg :admin :initform nil :accessor person-admin)))

(defstate *person* (make-instance 'person :name "Ada" :age 36))

(define-interface person-editor ()
  ()
  (:panes
   (name text-input-pane :text (binding (person-name *person*)))
   (age slider :start 0 :end 120 :value (binding (person-age *person*)))
   (admin check-button :text "管理者" :selected (binding (person-admin *person*)))
   (summary display-pane
            :text (computed (format nil "~A（~D 歳）~:[~;・管理者~]"
                                    (person-name *person*) (person-age *person*)
                                    (person-admin *person*)))))
  (:layouts
   (main grid-layout '("名前" name "年齢" age nil admin "" summary) :columns 2)))

(display (make-instance 'person-editor))
```

`*person*` に別の人を代入すると、フォームの欄はすべてその人のスロットを指すようになります。`binding` の式は使われるたびに評価されるからです。

## 4.5 autorun: 状態に追従する処理

`(autorun function)` は、`function` をすぐに呼び、その後は `function` が読んだ状態が変わるたびに呼び直します。画面以外のもの（保存、タイトル、ログ）を状態に追従させるのに使います。`stop` に返り値を渡すと止まります。

```lisp
;;; example: autorun
(defstate *items* '())

(defvar *saver*
  (autorun (lambda ()
             ;; *items* が変わるたびに呼ばれる
             (format t "保存: ~S~%" *items*))))

(push "a" *items*)
(push "b" *items*)
(flush)            ; ふつうはイベントの後に自動で呼ばれる
(stop *saver*)
```

## 4.6 更新のタイミング: flush

状態が変わっても、描き直しはすぐには行われません。変更は記録され、いまのイベント（クリックやキー入力）の処理が終わった後に `flush` がまとめて描き直します。そのため、1つのイベントの中で何度スロットを変えても、描き直しは1回です。

ふつうは `flush` を呼ぶ必要はありません。ブラウザの外（テストや REPL）では、自分で `(flush)` を呼ぶと画面が最新になります。開発版の REPL は評価のたびに `flush` を呼びます。

描き直しの最中に、描画が読んでいる状態を書き換えると、描き直しが繰り返されます。100 回を超えるとエラー（「rendering keeps changing the state it reads」）になります。描画の中では状態を書き換えないでください。

## 4.7 まとめ

| やりたいこと | 書き方 |
|---|---|
| アプリケーションのデータを持つ | `defmodel` のスロット、`define-interface` のスロット、`defstate` |
| 表示をデータから計算する | ペインの初期化引数に `(computed form)` |
| 欄やボタンをデータに直結する | `(binding place)` |
| データが変わったら何かする | `autorun` |
| 画面を変える | データを `setf` する（ペインのアクセサでもよい） |
