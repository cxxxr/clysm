# 14. デスクトップアプリケーション

[目次](../README.md) | 前: [コンポーネントとマークアップ](13-components.md) | 次: [開発とテスト](15-development.md)

同じプログラムを、Electron を使ったデスクトップアプリケーションとして動かせます。

## 14.1 仕組み

`ui/electron/` は、アプリケーションに固有のものを何も含まない Electron の殻です。`main.js` がウィンドウを開いてページ（`index.html`）を表示し、`preload.js` がページに `desktop` オブジェクトを渡します。プログラムの側では `ui/desktop.lisp` が、`desktop` があればバックエンドを `desktop-backend` にします。

| | ブラウザ | デスクトップ |
|---|---|---|
| バックエンド | `dom-backend` | `desktop-backend`（`dom-backend` のサブクラス） |
| `(desktop-p)` | 偽 | 真 |
| メニューバー | ページの上部（`:menu-bar` を指定したとき） | アプリケーションのメニューバー |
| エラーのリスタート | ページ内のダイアログ | ネイティブのメッセージボックス |
| ファイル | `prompt-for-file` で読むだけ | 読み書き、ネイティブのダイアログ |

`desktop.lisp` はブラウザでは何もしないので、両方で動かすプログラムは常に `desktop.lisp` と一緒にコンパイルしてかまいません。`(desktop-p)` で分岐できます。

## 14.2 作る

`tools/new-ui-app.sh` で作ったプロジェクトなら、`./build-desktop.sh` で `build-desktop/` に作られます。

```
$ ./build-desktop.sh
$ (cd build-desktop && npm start)
```

`build-desktop/` には、ビルドしたページ、`main.js`、`preload.js`、`desktop/package.json` が入ります。名前とウィンドウの大きさは `desktop/package.json` で決めます。

```json
{
  "name": "my-app",
  "productName": "My App",
  "main": "main.js",
  "window": {
    "width": 800, "height": 600, "minWidth": 400, "minHeight": 300,
    "backgroundColor": { "light": "#f4f4f2", "dark": "#1b1c1e" }
  },
  "scripts": { "start": "electron ." },
  "devDependencies": { "electron": "45.3.1" }
}
```

配布用のパッケージ（インストーラ）を作るには、`build-desktop/` に electron-builder などのツールを加えてください。

## 14.3 メニューバー

`(install-commands *backend* :edit-label "編集" :view-label "表示" :quit-label "終了")` は、アプリケーションのメニューバーを作ります。

- インターフェースのメニュー（`:menu-bar`）と、`:menu` を持つコマンドのメニュー
- その後に、標準の「編集」（コピー、貼り付けなど）と「表示」（再読み込み、開発ツール、拡大など）のメニュー
- 最初のメニューの最後に「終了」

メニューバーは状態に追従します。項目の有効・無効やチェックマークが変わると、メニューバーも変わります。キーはネイティブのメニューが処理します。

## 14.4 ファイルとダイアログ

| 関数 | 意味 |
|---|---|
| `(read-file path)` | テキストファイルの中身。読めなければ `nil` |
| `(write-file path text)` | ファイルに書く。書けなければ `desktop-file-error` を通知 |
| `(user-data-file name)` | アプリケーション用のデータのディレクトリにあるファイル `name` のパス |
| `(choose-file-to-open &key title filters)` | 開くファイルを選ばせ、パスを返す（キャンセルなら `nil`） |
| `(choose-file-to-save &key title default-name filters)` | 保存するファイルの名前を選ばせ、パスを返す |
| `(message-box message &key detail buttons type cancel)` | メッセージボックス。押されたボタンの番号を返す |
| `(add-recent-document path)` | 最近使ったファイルに加える |

これらのダイアログは、ブラウザのダイアログと違って答えを返すまで待ちます（Electron の同期呼び出しです）。`filters` は `(("TODO リスト" "json") ("テキスト" "txt" "md"))` の形です。

```lisp
(define-command (save-as :menu "ファイル" :label "名前を付けて保存…" :key "CmdOrCtrl+Shift+S"
                         :enabled (desktop-p))
    ()
  (let ((path (choose-file-to-save :default-name "notes.txt" :filters '(("テキスト" "txt")))))
    (when path
      (restart-case (write-file path *text*)
        (retry () :report "もう一度保存する" (save-as))))))
```

書き込みに失敗すると `desktop-file-error` が通知され、リスタート（「もう一度保存する」とキャンセル）を選ぶメッセージボックスが出ます。

`examples/todo/todo.lisp` は、ブラウザでは `localStorage` に、デスクトップではファイルに保存する例です。

## 14.5 テストのための設定

環境変数 `CLYSM_USER_DATA` を設定すると、`user-data-file` のディレクトリがそこに変わります。テストで実際のデータを汚さないために使います。
