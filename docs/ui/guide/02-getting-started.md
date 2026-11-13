# 2. 使い始める

[目次](../README.md) | 前: [はじめに](01-introduction.md) | 次: [インターフェース](03-interfaces.md)

## 2.1 必要なもの

- Clysm のチェックアウト（このリポジトリ）と SBCL。コンパイラは SBCL の上で動きます。
- 実行するブラウザ。WebAssembly の GC と例外処理に対応したもの（最近の Chrome、Firefox、Safari）。動作を確かめた版は [付録](17-capi-and-clim.md#174-動作環境) にあります。
- ページを配信する HTTP サーバー（例では Python の `http.server` を使います）。
- デスクトップアプリケーションにするなら、npm（Electron を入れます）。

## 2.2 プロジェクトを作る

`tools/new-ui-app.sh` で、どこにでも新しいアプリケーションのディレクトリを作れます。

```
$ ~/src/clysm/tools/new-ui-app.sh ~/work/hello "Hello"
made /home/me/work/hello: build it with /home/me/work/hello/build.sh
```

できるファイルは次のとおりです。

| ファイル | 内容 |
|---|---|
| `app.lisp` | プログラム本体 |
| `index.html` | プログラムを読み込むページ |
| `build.sh` | `build/` にビルドする（`--dev` で REPL 付き） |
| `build-desktop.sh` | デスクトップアプリケーションを `build-desktop/` に作る |
| `desktop/package.json` | デスクトップアプリケーションの名前とウィンドウの大きさ |

スクリプトは、環境変数 `CLYSM`（なければ雛形を作った Clysm のディレクトリ）から Clysm を探します。

## 2.3 最初のプログラム

`app.lisp` には、ボタンを押した回数を表示するプログラムが入っています。

```lisp
;;; example: first-program
(define-interface main-window ()
  ((count :initform 0 :accessor click-count))
  (:panes
   (message title-pane
            :text (computed (format nil "~D clicks" (click-count interface))))
   (button push-button :text "Click me"
           :callback (lambda (data interface)
                       (declare (ignore data))
                       (incf (click-count interface)))))
  (:layouts
   (main column-layout '(message button)))
  (:default-initargs :title "Hello"))

(display (make-instance 'main-window))
```

上から順に見ていきます。

- `define-interface` はウィンドウのクラスを定義します。`defclass` と同じようにスロットを持てます（ここでは `count`）。
- `:panes` には、ウィンドウに置く部品（ペイン）を並べます。`message` は1行の文字列を表示する `title-pane`、`button` はボタン（`push-button`）です。
- `message` の `:text` は `(computed ...)` です。式の値がそのまま表示され、式が読んだスロット（`click-count`）が変わると表示も変わります。式の中では、`interface` がこのウィンドウ自身を、`message` や `button` がほかのペインを指します。
- ボタンの `:callback` は、押されたときにデータ（ボタンの文字列）とインターフェースを引数に呼ばれます。
- `:layouts` は、ペインの並べ方です。`column-layout` は縦に並べます。
- `display` はインターフェースをページの `#app` 要素に表示します。

## 2.4 ビルドして動かす

```
$ cd ~/work/hello
$ ./build.sh
built build/ — serve it with e.g.: python3 -m http.server -d build
$ python3 -m http.server -d build
```

ブラウザで `http://localhost:8000/` を開きます。ページは HTTP で配信する必要があります（`file:` では WebAssembly を読み込めません）。

ビルドは、フレームワークのファイルと `app.lisp` を1つの WebAssembly モジュール（`build/app.wasm`）にコンパイルし、ページに必要なファイル（`index.html`、`ui.css`、`clysm.mjs` など）を `build/` に置きます。コンパイルのエラーは画面に出ます。

## 2.5 REPL 付きで開発する

```
$ ./build.sh --dev
```

開発版のページには REPL が付き、動いているプログラムをその場で書き換えられます。

```
HELLO> (setf (click-count (first *interfaces*)) 100)
HELLO> (define-interface main-window () ...)      ; 変えた定義を評価する
```

関数やクラスを定義し直すと、画面にすぐ反映されます。詳しくは [開発とテスト](15-development.md) を見てください。開発版はコンパイラを含むので、ファイルが通常版より 2 MB ほど大きく、読み込みに少し時間がかかります。

## 2.6 デスクトップアプリケーションにする

```
$ ./build-desktop.sh
$ (cd build-desktop && npm start)
```

最初の1回は Electron をダウンロードします。デスクトップ版では、メニューがアプリケーションのメニューバーに出て、ファイルの読み書きやネイティブのダイアログが使えます。[デスクトップアプリケーション](14-desktop.md) を見てください。

## 2.7 ファイルを分ける

プログラムが大きくなったら、ファイルを分けて `build.sh` のコンパイル行に並べます。後のファイルは前のファイルの定義を使えます。

```
"$CLYSM/bin/clysm" compile "$UI/ui.lisp" ... "$UI/desktop.lisp" \
  model.lisp commands.lisp windows.lisp main.lisp -o build/app.wasm
```

フレームワークのファイルは、この順にコンパイルします: `ui.lisp`、`command.lisp`、`present.lisp`、`pane.lisp`、`dialog.lisp`、`graphics.lisp`、`stream.lisp`、`dom.lisp`、`desktop.lisp`。ブラウザでしか動かさないなら `desktop.lisp` は省けます。

## 2.8 パッケージ

フレームワークの名前はすべてパッケージ `UI` にあります。アプリケーションのパッケージで `UI` を `:use` するのがふつうです。

```lisp
(defpackage "HELLO"
  (:use "CL" "UI"))
```

`UI` は `interface`、`display`、`present`、`window`、`range`、`stop` などの短い名前を公開しているので、ほかのパッケージと名前がぶつかるときは `:use` せずに `ui:` を付けて書いてください。
