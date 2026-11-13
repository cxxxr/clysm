# Dialogs, files and the clipboard

*Generated from `ui/dialog.lisp` by `tools/ui-doc.py`; do not edit.*

**Contents**

- [`*ok-label*`](#xok-labelx), [`*yes-label*`](#xyes-labelx), [`*no-label*`](#xno-labelx), [`display-dialog`](#display-dialog), [`exit-dialog`](#exit-dialog), [`abort-dialog`](#abort-dialog), [`display-message`](#display-message), [`prompt-for-confirmation`](#prompt-for-confirmation), [`prompt-for-string`](#prompt-for-string), [`prompt-for-number`](#prompt-for-number), [`prompt-with-list`](#prompt-with-list), [`prompt-for-values`](#prompt-for-values), [`prompt-for-color`](#prompt-for-color), [`popup-confirmer`](#popup-confirmer), [`prompt-for-file`](#prompt-for-file), [`set-clipboard`](#set-clipboard), [`clipboard`](#clipboard), [`*paste-prompt*`](#xpaste-promptx)
- The command palette: every command by its name (as in the interactor: [`*command-palette-prompt*`](#xcommand-palette-promptx), [`command-palette`](#command-palette)

<a id="xok-labelx"></a>
### *ok-label*

*Variable*

Initial value: `"OK"`

The label of the button that answers a dialog.


<a id="xyes-labelx"></a>
### *yes-label*

*Variable*

Initial value: `"Yes"`

The label of the button that says yes.


<a id="xno-labelx"></a>
### *no-label*

*Variable*

Initial value: `"No"`

The label of the button that says no.


<a id="display-dialog"></a>
### display-dialog

*Function*

```lisp
(display-dialog interface &key continuation (modal t))
```

Show `interface` as a dialog.  When it is answered with `exit-dialog`,
`continuation` is called with the value and T; when it is cancelled
(`abort-dialog`, the Escape key), with `nil` and `nil`.  With `modal` false it is a
window above the application, which can be used meanwhile, with a button
that closes it (`quit-interface` does too).


<a id="exit-dialog"></a>
### exit-dialog

*Function*

```lisp
(exit-dialog value &optional (dialog (top-dialog)))
```

Answer the dialog (the topmost one) with `value`.


<a id="abort-dialog"></a>
### abort-dialog

*Function*

```lisp
(abort-dialog &optional (dialog (top-dialog)))
```

Cancel the dialog (the topmost one).


<a id="display-message"></a>
### display-message

*Function*

```lisp
(display-message control &rest arguments)
```

Show the message made by `format` from `control` and `arguments` until the user closes it.


<a id="prompt-for-confirmation"></a>
### prompt-for-confirmation

*Function*

```lisp
(prompt-for-confirmation message &key continuation title)
```

Ask the yes-or-no question `message`; `continuation` is called with the
answer (true or false) and T, or with `nil` and `nil` if the dialog was cancelled.


<a id="prompt-for-string"></a>
### prompt-for-string

*Function*

```lisp
(prompt-for-string message &key (text "") continuation title placeholder (ok-check (constantly t)))
```

Ask for a string, initially `text`; `continuation` is called with it and T, or
with `nil` and `nil`.  The dialog can be answered while `ok-check`, a function of
the string, returns true.


<a id="prompt-for-number"></a>
### prompt-for-number

*Function*

```lisp
(prompt-for-number message &key (value nil) continuation title (integer nil))
```

Ask for a number (an integer if `integer` is true), like `prompt-for-string`.


<a id="prompt-with-list"></a>
### prompt-with-list

*Function*

```lisp
(prompt-with-list items message &key continuation title print-function (selected-item (first items)))
```

Ask for one of `items`; `continuation` is called with it and T, or with `nil` and `nil`.


<a id="prompt-for-values"></a>
### prompt-for-values

*Function*

```lisp
(prompt-for-values fields &key continuation title message)
```

Ask for several values in one dialog.  `fields` is a list of
(label type &key default), `type` being `string`, `integer`, `number`, `boolean` or
(`member` ...).  `continuation` is called with the list of the values and T, or
with `nil` and `nil`; the dialog can be answered when every value is one of its type.


<a id="prompt-for-color"></a>
### prompt-for-color

*Function*

```lisp
(prompt-for-color message &key (color "#000000") continuation title)
```

Ask for a colour (like "#3366cc"); `continuation` is called with it and T, or with `nil` and `nil`.


<a id="popup-confirmer"></a>
### popup-confirmer

*Function*

```lisp
(popup-confirmer pane message &key title continuation (value-function #'identity) (ok-check (constantly t)))
```

Show `pane` in a dialog with the buttons OK and Cancel.  `continuation` is
called with what `value-function` returns for the pane and T, or with `nil` and
`nil`; OK is enabled while `ok-check`, a function of that value, returns true.


<a id="prompt-for-file"></a>
### prompt-for-file

*Function*

```lisp
(prompt-for-file &key continuation accept)
```

Let the user choose a file to read (`accept`: the kinds of files, like
".json,.txt"); `continuation` is called with its name and its text, or with
`nil` and `nil`.


<a id="set-clipboard"></a>
### set-clipboard

*Function*

```lisp
(set-clipboard text)
```

Put the string `text` on the clipboard.


<a id="clipboard"></a>
### clipboard

*Function*

```lisp
(clipboard continuation)
```

Call `continuation` with the text on the clipboard.  Where the browser does
not let a page read the clipboard, the user is asked to paste it into a
dialog (and `continuation` gets `nil` if that is cancelled).


<a id="xpaste-promptx"></a>
### *paste-prompt*

*Variable*

Initial value: `"Paste the text here (Ctrl+V), then press OK"`

What the dialog says that asks the user to paste the clipboard.


## The command palette: every command by its name (as in the interactor

<a id="xcommand-palette-promptx"></a>
### *command-palette-prompt*

*Variable*

Initial value: `"Type the name of a command"`

The placeholder of the field of the command palette.


<a id="command-palette"></a>
### command-palette

*Command*

```lisp
(command-palette)
```

Show a dialog in which a command is chosen by typing part of its label;
Enter (or a double click) invokes it, which asks for its arguments.

Options: `:label "Command palette…" :key "CmdOrCtrl+Shift+P"`
