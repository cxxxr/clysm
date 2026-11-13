# 15. 開発とテスト

[目次](../README.md) | 前: [デスクトップアプリケーション](14-desktop.md) | 次: [見た目](16-styling.md)

## 15.1 REPL で開発する

`./build.sh --dev`（またはフレームワークの `tools/build-ui-dev.sh OUT.wasm APP.lisp...`）は、コンパイラとフレームワークとプログラムを含む開発用のイメージを作ります。そのページには REPL が付き、評価した式はページの中でコンパイルされて、動いているプログラムを変えます。

- **コンポーネント**やペインの `render-pane` を定義し直すと、すぐに表示が変わります。インスタンスの状態は保たれます。
- **関数**を定義し直すと、それ以降はその定義が使われます。開発用のイメージの関数はインライン展開されないので、どこから呼ばれていても新しい定義になります。評価のたびにすべてのコンポーネントが描き直されます。
- **`defmodel` や `define-interface`** を定義し直すと、既存のインスタンスに新しいスロットが加わります。
- **コマンド**を定義し直すと、メニューやボタンに反映されます。

```
APP> *interfaces*
APP> (setf (button-text (find-pane (first *interfaces*) 'ok)) "送信")
APP> (defmethod render-pane ((pane rating-pane)) ...)
```

REPL の出力はページの下に表示されます。開発用のイメージは通常のものより大きいので、公開するときは `--dev` なしでビルドしてください。

## 15.2 ブラウザなしでテストする

`ui/sexp.lisp` の S 式バックエンドを使うと、ブラウザなしで画面をテストできます。画面は S 式の木になり、変更はすべて記録されます。

```lisp
;;; example: testing
(defmodel counter ()
  ((value :initform 0 :accessor counter-value)))

(define-interface counter-window ()
  ((counter :initform (make-instance 'counter) :reader window-counter))
  (:panes
   (shown title-pane :text (computed (princ-to-string (counter-value (window-counter interface)))))
   (up push-button :text "+1" :callback (lambda (data interface)
                                          (declare (ignore data))
                                          (incf (counter-value (window-counter interface)))))))

(let* ((*backend* (make-instance 'sexp-backend))
       (root (make-root))
       (window (display (make-instance 'counter-window) :container root)))
  (fire (find-node root "button") "click")         ; リスナーを呼び、flush する
  (fire (find-node root "button") "click")
  (assert (equal (node-text (find-node root "span")) "2"))
  (assert (= (counter-value (window-counter window)) 2))
  (backend-log *backend*)                           ; ここまでの変更の記録
  (quit-interface window))
```

| 関数 | 意味 |
|---|---|
| `(make-root)` | 表示する場所になるノード |
| `(named-root "#app")` | 名前の付いた場所（`display` の既定の `"#app"` はこれになる） |
| `(to-sexp node)` | ノードの木を `(:tag (property value...) child...)` の形にする |
| `(find-nodes root tag &key class)`、`(find-node root tag &key class index)` | 要素を探す |
| `(node-text node)` | ノードの中の文字列 |
| `(fire node event &rest arguments)` | リスナーを呼び、`flush` する |
| `(fire-global backend event &rest arguments)` | ページ全体のリスナー（キーなど）を呼ぶ |
| `(backend-log backend)` | 前に呼んでからの変更の記録 |
| `(overlay-container backend)` | ダイアログ、ウィンドウ、ポップアップメニューのある場所 |

イベントはプロパティリストで渡します。`(fire node "keydown" '(:key "Enter"))`、`(fire node "input" '(:value "abc"))` のように書くと、`event-get` と `event-target-get` がそこから読みます。

### ユーザーの答え

S 式バックエンドでは、ユーザーが答える部分を関数で代わりに行います。

| スロット | 意味 |
|---|---|
| `backend-chooser` | エラーのリスタートを選ぶ関数（エラーとリスタートを引数に、選んだリスタートか `nil` を返す） |
| `backend-waiter` | `wait-for-answer` で待つときに、答えを出す操作（ダイアログのボタンの `fire` など）を行う関数 |
| `backend-file` | `prompt-for-file` で選ばれるファイルの `(名前 中身)` |
| `backend-clipboard` | クリップボードの中身 |

フレームワーク自身のテスト（`tests/ui/ui.lisp`、`tests/ui/pane.lisp`）は、この方法で書かれています。`make test-ui` で実行します。

## 15.3 デバッグ

- `print` や `format t` の出力は、ブラウザの開発ツールのコンソールに出ます（開発版では REPL に出ます）。
- エラーはダイアログに表示され、コンソールにも出ます。
- `(js:log value...)` は JavaScript の値もそのままコンソールに出します。

## 15.4 バックエンドを作る

バックエンドは、総称関数のプロトコルを実装したクラスです。必要なメソッドは次のとおりです（説明は [リファレンス](../reference/core.md) にあります）。

- 画面: `make-element-node`、`make-text-node`、`set-node-text`、`set-property`、`remove-property`、`set-listener`、`insert-node`、`remove-node`、`replace-node`、`node-follows-p`、`request-flush`、`resolve-container`
- エラー: `choose-restart`
- ペインなど: `event-get`、`event-target-get`、`stop-event`、`node-call`、`overlay-container`、`set-global-listener`、`set-title`、`previous-sibling-size`、`event-position`、`viewport-size`
- 任意（既定のメソッドあり）: `reveal-selection`、`native-menus-p`、`capture-pointer`、`begin-drag`、`can-wait-p`、`wait-for`
- そのほか: `install-commands`、`choose-file`、`write-clipboard`、`read-clipboard`

`ui/sexp.lisp`（200 行ほど）が最小の実装の例です。
