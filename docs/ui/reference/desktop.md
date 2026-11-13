# The desktop backend

*Generated from `ui/desktop.lisp` by `tools/ui-doc.py`; do not edit.*

<a id="desktop-backend"></a>
### desktop-backend

*Class*

Superclasses: [`dom-backend`](dom.md#dom-backend)

The backend of a page in the desktop application (ui/electron): the
DOM backend with a native menu bar, native dialogs and files.  Loading
desktop.lisp makes it `*backend*` when the program runs there.


<a id="desktop-p"></a>
### desktop-p

*Function*

```lisp
(desktop-p)
```

True if the program runs in the desktop application.


<a id="desktop-file-error"></a>
### desktop-file-error

*Condition*

Superclasses: `error`

Signalled by `write-file` when a file cannot be written.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| path | `:path` |  | [`desktop-file-error-path`](#desktop-file-error) | the file |
| message | `:message` |  | [`desktop-file-error-message`](#desktop-file-error) | why it could not be written |


<a id="read-file"></a>
### read-file

*Function*

```lisp
(read-file path)
```

The contents of the text file `path`, or `nil` if it cannot be read.


<a id="write-file"></a>
### write-file

*Function*

```lisp
(write-file path text)
```

Write `text` to the file `path`; signal `desktop-file-error` if that fails.


<a id="user-data-file"></a>
### user-data-file

*Function*

```lisp
(user-data-file name)
```

The path of the file `name` in the directory for the application's own data.


<a id="add-recent-document"></a>
### add-recent-document

*Function*

```lisp
(add-recent-document path)
```

Add the file `path` to the recent documents of the system (the menu of the
application in the dock or the task bar).


<a id="choose-file-to-open"></a>
### choose-file-to-open

*Function*

```lisp
(choose-file-to-open &key title filters)
```

Ask for a file to open; `filters` is a list of (name extension...).
Return its path, or `nil`.


<a id="choose-file-to-save"></a>
### choose-file-to-save

*Function*

```lisp
(choose-file-to-save &key title default-name filters)
```

Ask for a file name to save to; return the path, or `nil`.


<a id="message-box"></a>
### message-box

*Function*

```lisp
(message-box message &key detail (buttons '("OK")) (type "info") (cancel (1- (length buttons))))
```

Show `message` (and `detail`) with `buttons`; return the index of the chosen
button (`cancel`, by default the last, if the box is closed).  `type` is
"info", "warning", "error" or "question".
