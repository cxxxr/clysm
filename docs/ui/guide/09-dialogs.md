# 9. ダイアログ

[目次](../README.md) | 前: [プレゼンテーション](08-presentations.md) | 次: [描画](10-graphics.md)

## 9.1 待たないダイアログ

ページの中では、関数の途中でユーザーの答えを待てません。そのため、ダイアログを出す関数はすぐに戻り、答えが出たときに **継続**（`:continuation` に渡した関数）を呼びます。継続は、答えと、答えがあったかどうか（キャンセルされたら偽）の2つを引数に呼ばれます。

```lisp
;;; example: confirmation
(defstate *files* '("a.txt" "b.txt"))

(defun delete-all ()
  (prompt-for-confirmation "すべてのファイルを削除しますか？"
                           :continuation (lambda (yes okp)
                                           (when (and okp yes)
                                             (setf *files* '())))))

(delete-all)
```

ボタンの文字は `*ok-label*`、`*yes-label*`、`*no-label*`、`*abort-label*`（キャンセル）で変えられます。

```lisp
(setf *ok-label* "OK" *yes-label* "はい" *no-label* "いいえ" *abort-label* "キャンセル")
```

## 9.2 用意されたダイアログ

| 関数 | 尋ねるもの | 継続の引数 |
|---|---|---|
| `(display-message control &rest args)` | なし（`format` したメッセージを表示） | — |
| `(prompt-for-confirmation message)` | はい・いいえ | 真偽値、答えたか |
| `(prompt-for-string message &key text placeholder ok-check)` | 文字列 | 文字列、答えたか |
| `(prompt-for-number message &key value integer)` | 数（`:integer t` なら整数） | 数、答えたか |
| `(prompt-with-list items message &key print-function selected-item)` | リストの1つ | 項目、答えたか |
| `(prompt-for-values fields &key title message)` | 複数の値を1つのフォームで | 値のリスト、答えたか |
| `(prompt-for-color message &key color)` | 色（`"#rrggbb"`） | 色、答えたか |
| `(popup-confirmer pane message &key value-function ok-check)` | 任意のペインで | `value-function` の値、答えたか |

どの関数も `:title` と `:continuation` を受け付けます。`ok-check` は値を引数に取る関数で、真を返すときだけ OK を押せます。

```lisp
;;; example: prompt-for-values
(prompt-for-values '(("名前" string :default "Ada")
                     ("年齢" integer)
                     ("管理者" boolean)
                     ("色" (member :red :green :blue) :default :green))
                   :title "新しいユーザー"
                   :continuation (lambda (values okp)
                                   (when okp
                                     (destructuring-bind (name age admin color) values
                                       (format t "~A ~D ~A ~A~%" name age admin color)))))
```

`prompt-for-values` の各フィールドは `(ラベル 型 :default 値)` で、型は `string`、`integer`、`number`、`boolean`、`(member ...)` です。整数の欄に整数でないものが入っている間は OK を押せません。

```lisp
;;; example: popup-confirmer
(popup-confirmer (make-instance 'slider :start 1 :end 10 :value 3)
                 "いくつ作りますか？"
                 :value-function #'range-value
                 :ok-check #'plusp
                 :continuation (lambda (count okp)
                                 (when okp (format t "~D 個~%" count))))
```

## 9.3 インターフェースをダイアログにする

どんなインターフェースも `display-dialog` でダイアログにできます。ダイアログの中のコールバックは、`(exit-dialog value)` で答え、`(abort-dialog)` でキャンセルします。Escape でもキャンセルになります。

```lisp
;;; example: display-dialog
(define-interface login-dialog ()
  ()
  (:panes
   (user text-input-pane :title "ユーザー名")
   (password password-pane :title "パスワード")
   (cancel push-button :text "キャンセル" :callback-type :none :callback #'abort-dialog)
   (ok push-button :text "ログイン" :default-p t
       :enabled (computed (plusp (length (text-input-pane-text user))))
       :callback (lambda (data interface)
                   (declare (ignore data interface))
                   (exit-dialog (list (text-input-pane-text user)
                                      (text-input-pane-text password))))))
  (:layouts
   (main column-layout '(user password buttons))
   (buttons row-layout '(cancel ok) :adjust :end))
  (:default-initargs :title "ログイン"))

(display-dialog (make-instance 'login-dialog)
                :continuation (lambda (answer okp)
                                (if okp
                                    (format t "ユーザー ~A~%" (first answer))
                                    (format t "キャンセル~%"))))
```

ダイアログは重ねて出せます。`exit-dialog` と `abort-dialog` は一番上のモーダルなダイアログに働きます。

### モーダルでないダイアログ

`(display-dialog interface :modal nil)` は、アプリケーションを使いながら開いておけるダイアログ（道具箱など）になります。閉じるボタンが付きます。移動や大きさの変更ができるウィンドウが必要なら、[ウィンドウ](12-windows.md) を使ってください。

## 9.4 ファイル

`(prompt-for-file &key accept continuation)` は、読み込むファイルをユーザーに選ばせ、ファイル名と中身（文字列）を継続に渡します。キャンセルされると両方とも `nil` です。`:accept` はファイルの種類（`".csv,.txt"` など）です。

```lisp
(prompt-for-file :accept ".csv"
                 :continuation (lambda (name text)
                                 (when name (import-csv text))))
```

ブラウザでは、ページはファイルを書き込めません（ダウンロードはできますが、このフレームワークには含まれていません）。デスクトップ版では、ネイティブのダイアログとファイルの読み書きが使えます（[デスクトップアプリケーション](14-desktop.md)）。

## 9.5 クリップボード

`(set-clipboard text)` はクリップボードに文字列を書き、`(clipboard continuation)` はクリップボードの文字列を継続に渡します。ページがクリップボードを読むことをブラウザが許さないとき（Safari など）は、貼り付け用のダイアログが出て、ユーザーが貼り付けた文字列が渡されます。そのメッセージは `*paste-prompt*` です。

```lisp
(define-command copy-address ((contact 'contact))
  (set-clipboard (contact-email contact)))
```

## 9.6 エラーとリスタート

リスナー、コマンド、描画の中で起きたエラーは、アプリケーションを止めません。エラーのメッセージと、その時点で有効なリスタートがユーザーに示され、選ばれたリスタートが実行されます。

```lisp
;;; example: restarts
(defstate *status* "")

(define-command (save :label "保存") ()
  (setf *status*
        (restart-case (error "ディスクがいっぱいです")
          (retry () :report "もう一度試す" "再試行しました")
          (save-elsewhere () :report "別の場所に保存する" "別の場所に保存しました"))))

(contain (make-instance 'column-layout
                        :description (list (make-instance 'push-button :command 'save)
                                           (make-instance 'title-pane :text (computed *status*)))))
```

「保存」を押すと、「ディスクがいっぱいです」というメッセージに、「もう一度試す」「別の場所に保存する」「キャンセル」のボタンが付いたダイアログが出ます。選んだリスタートが実行され、コマンドの続きが動きます。「キャンセル」は処理を中止します（その文字は `*abort-label*`）。

- ブラウザでは、ページの中のダイアログで尋ねます。その間、エラーを起こした処理は止まって待ち、ページのほかの部分は動き続けます。これは WebAssembly のスタックを中断する機能（JSPI）を使っています。JSPI のないブラウザでは `prompt` で番号を尋ねます。
- デスクトップ版では、ボタンがリスタートのネイティブのメッセージボックスで尋ねます。
- 選べるリスタートがないエラーは、メッセージだけを表示します。

描画の中でエラーが起きたコンポーネントは、前の表示のまま残ります。

### with-error-dialog

`(with-error-dialog body...)` は、本体の中で起きたエラーをこのように扱います。リスナーやコマンドは自動でその中で動くので、ふつうは書く必要はありません。タイマーなど、フレームワークの外から呼ばれる処理を書くときに使います。

## 9.7 処理の途中で答えを待つ

エラーのリスタートと同じ仕組みで、自分の処理の途中でユーザーの答えを待つこともできます。`(wait-for-answer function)` は、答えを受け取る関数を引数に `function` を呼び、その関数が呼ばれるまで待って答えを返します。

```lisp
(defun ask-name ()
  (wait-for-answer
   (lambda (answer)
     (prompt-for-string "名前は？"
                        :continuation (lambda (name okp) (funcall answer (and okp name)))))))
```

待てるのは、ブラウザのイベント（クリックなど）から直接呼ばれた処理の中で、JSPI があるときだけです。`(can-wait-p *backend*)` で確かめられます。待てない場所では、継続を使ってください。
