# エディタ部品 (ui/editor/)

Common Lisp だけで書いたコードエディタの部品です。CodeMirror のような JavaScript のエディタを使わずに、ブラウザや Electron の上でテキストを編集できます。UI フレームワーク（`ui/`）のペイン `editor:code-editor` として使います。

```lisp
(defvar *editor*
  (make-instance 'editor:code-editor
                 :text "(defun hello () 42)"
                 :language "lisp"          ; "lisp" か "text"、または LANGUAGE のインスタンス
                 :keymap :standard         ; :standard か :emacs、または keymap
                 :wrap t                   ; 長い行をペインの幅で折り返す
                 :change-callback (lambda (pane) (print (editor:code-editor-text pane)))
                 :save-callback (lambda (pane) ...)))   ; Mod-S、C-x C-s

(ui:mount (ui:html (:div (ui:pane-node *editor*))) "#app")
```

ページには `ui/ui.css` と `ui/editor/editor.css` を読み込みます。動く例は `examples/editor/`、これを使った Common Lisp の IDE は `examples/ide/` です。

```
$ examples/editor/build.sh && python3 -m http.server -d build/editor     # ブラウザ
$ examples/editor/build-desktop.sh && (cd build/editor-desktop && npm start)   # Electron
$ (cd build/editor-desktop && npx electron . ~/foo.lisp)                       # ファイルを開いて起動
```

テストは、モデルが `tests/editor/run.sh`（Clysm と SBCL）、デスクトップ版が `tests/editor/desktop.sh` です。後者は Electron の中で実際のキー入力とマウス操作を送り、ファイルの保存とシステムのクリップボードまで確かめます（`SCREENSHOT=file.png` で画面を保存）。

## できること

- シンタックスハイライト（Common Lisp: コメント、`#| |#`、文字列、文字、キーワード、数、定義名、括弧の深さによる色分け）
- 行番号、現在行、対応する括弧の強調
- 選択（Shift と矢印、マウスのドラッグ、ダブルクリックで単語、トリプルクリックで行）
- Undo / Redo（続けて打った文字は1つにまとまる）
- Lisp の字下げ（Return、Tab）、S 式単位の移動、コメントの切り替え、行の移動と複製
- 括弧と `"` の自動補完
- コピー、切り取り、貼り付け（ブラウザのクリップボード）、Emacs のキルリング
- IME（日本語入力）。変換中の文字はカーソルの位置に出ます
- 検索（`code-editor-find`）
- 大きなファイル: 画面に見えている行だけをページに置くので、数万行でも1キー数ミリ秒です
- 折り返し表示（`:wrap t`）。空白か全角文字の前後で折り返し、上下の矢印は見た目の行ごとに動きます
- 読み取り専用モード

## 構成

| ファイル | 内容 | 依存 |
|---|---|---|
| `buffer.lisp` | テキスト（行のベクタ）、位置、選択、Undo の履歴 | CL のみ |
| `syntax.lisp` | 言語: 行ごとのトークン化と状態のキャッシュ、括弧、字下げ、S 式 | CL のみ |
| `commands.lisp` | コマンド（`define-editor-command`）とキーマップ | CL のみ |
| `view.lisp` | ペイン `code-editor`: 描画、キーボード、マウス、IME、クリップボード | `ui/`、`JS` |
| `editor.css` | 見た目（色はカスタムプロパティ） | |

最初の3つは移植可能な Common Lisp で、ページを知りません。SBCL でも Clysm でも同じテストが通ります（`tests/editor/run.sh`）。

`view.lisp` は JavaScript のコードを1行も書いていません。ページには DOM のメソッドを `js:call`・`js:get` で呼ぶだけで触ります。

- 行は UI フレームワークのコンポーネントで、引数（行の文字列、トークン、行の中の選択、カーソルの列）が変わったときだけ描き直されます。見えている行（と上下の数行）だけを置き、全体の高さの要素でスクロールバーを作ります。
- 見た目の行（折り返した行）の高さは一定なので、マウスの y 座標から行は割り算で求まります。折り返すときは、どこで切るかをブラウザに任せずに Lisp が決めます（`wrap-string`: 文字の幅は半角 1、全角 2 で、ペインの幅は文字の幅を測って求めます）。そのため、各行が何段になるかがページに聞かなくてもわかり、数万行でも見えている行だけを置けます。列はブラウザ（`caretPositionFromPoint`）に聞くので、全角文字の幅も正しく扱えます。
- キーは、カーソルの位置に置いた見えない textarea が受け取ります。修飾キー付きのキーや文字のないキーは keydown でキーマップを引き、文字は input イベント、IME の確定文字は compositionend で受け取ります。

## プログラムからの操作

| 関数 | 意味 |
|---|---|
| `(code-editor-text pane)`、`(setf (code-editor-text pane) text)` | テキスト（setf すると Undo の履歴は消える） |
| `(code-editor-language pane)`、`setf` | 言語 |
| `(code-editor-keymap pane)`、`setf` | `:standard`、`:emacs` |
| `(code-editor-read-only pane)`、`setf` | 読み取り専用 |
| `(code-editor-wrap pane)`、`setf` | 折り返し表示 |
| `(code-editor-cursor pane)` | カーソルの行と列（0 から）の2値 |
| `(code-editor-select pane l1 c1 l2 c2)` | 選択して見えるようにスクロール |
| `(code-editor-find pane string &key backward)` | 次（前）を選択 |
| `(code-editor-run pane 'editor::toggle-comment)` | コマンドを実行 |
| `(code-editor-focus pane)` | キーボードをエディタに（まだページにないペインなら、置かれたときに） |
| `(code-editor-cursor-rect pane)` | カーソルの画面上の位置（左、上、下）の3値。補完の一覧などを出す場所 |
| `(code-editor-message pane)` | 最後のコマンドのメッセージ（"Mark set" など） |
| `(code-editor-editor pane)` | モデル（`editor:editor`）。`editor-modified`、`mark-saved`、`undo` など |

これらは観測される状態を読むので、`ui:computed` やコンポーネントの中で使うと、編集に合わせて表示が変わります（`examples/editor/` のステータス行）。

## 拡張

コマンドは `define-editor-command` で定義し、`define-key` でキーに割り当てます。キーは `"C-x C-s"` のような文字列で、修飾キーは `C-`（Control）、`M-`（Alt/Option）、`s-`（Command）、`S-`（Shift）の順です。`Mod-` は Mac では Command、ほかでは Control です。

```lisp
(editor:define-editor-command upcase-region (e)
  "選択を大文字にする。"
  (editor:replace-selection e (string-upcase (editor:selected-text e))))

(defvar *my-keymap* (editor::make-keymap :parent editor:*standard-keymap*))
(editor:define-key *my-keymap* "Mod-u" 'upcase-region)
(setf (editor:code-editor-keymap *editor*) *my-keymap*)
```

言語は `editor:language` のサブクラスと、`tokenize-line`（行と開始時の状態から、トークン `(start end class)` のリストと終了時の状態を返す）、`compute-indentation` のメソッドで作ります。トークンのクラス `"foo"` は、ページでは CSS のクラス `ce-foo` になります。
