# Commands and menu items

*Generated from `ui/command.lisp` by `tools/ui-doc.py`; do not edit.*

**Contents**

- [`command`](#command), [`*command-tables*`](#xcommand-tablesx), [`command-active-p`](#command-active-p), [`*commands*`](#xcommandsx), [`find-command`](#find-command), [`define-command`](#define-command), [`command-enabled-p`](#command-enabled-p), [`*command-output*`](#xcommand-outputx), [`invoke-command`](#invoke-command), [`command`](#command), [`command-button`](#command-button), [`command-menus`](#command-menus), [`parse-key`](#parse-key), [`command-for-key`](#command-for-key)
- Menu items: what a menu shows, for the menu bar of the page (pane.lisp),: `menu-item`, [`parse-mnemonic`](#parse-mnemonic), [`application-menus`](#application-menus), [`*key-handlers*`](#xkey-handlersx), [`key-action`](#key-action)

<a id="command"></a>
### command

*Class*

What `define-command` defines (see `find-command`).

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| name | `:name` |  | [`command-name`](#command) | the symbol that names it and its function |
| label | `:label` |  | [`command-label`](#command) | the text of its menu item or button |
| menu | `:menu` |  | [`command-menu`](#command) | the title of the menu it is in, or `nil` |
| key | `:key` |  | [`command-key`](#command) | its key, like "CmdOrCtrl+Shift+S", or `nil` |
| separator | `:separator` |  | [`command-separator`](#command) | true if a line is drawn above it in its menu |
| enabled | `:enabled` |  | `command-enabled` | `nil`, or a function that says whether it can be invoked |
| parameters | `:parameters` | `nil` | [`command-parameters`](#command) | the required parameters: (name type prompt), `type` `nil` if it has none |
| table | `:table` | `nil` | [`command-table`](#command) | `nil`, or the name of the command table it belongs to |


<a id="xcommand-tablesx"></a>
### *command-tables*

*State*

Initial value: `nil`

The names of the command tables in use: those of the interfaces on the screen.


<a id="command-active-p"></a>
### command-active-p

*Function*

```lisp
(command-active-p command)
```

True if `command` is offered to the user (in menus, by its key): it has no
command table, or its table is in use.


<a id="xcommandsx"></a>
### *commands*

*State*

Initial value: `nil`

The commands, in the order of their definition.


<a id="find-command"></a>
### find-command

*Function*

```lisp
(find-command name)
```

The command named `name` (a symbol), or `nil`.


<a id="define-command"></a>
### define-command

*Macro*

```lisp
(define-command name-and-options lambda-list &body body)
```

Define the function `name` as a command.  `name-and-options` is `name` or

```lisp
(name :label string :menu string :key string :separator boolean :enabled form
      :table symbol)
```

`label` is the text of its menu item or button, `menu` the title of the menu it
is in, `key` its key (like "CmdOrCtrl+Shift+S"), `separator` true to draw a line
above it in the menu.  The command can be invoked while `enabled`, a form
evaluated when needed (reading state in it is observed), is true.  With
`table`, the name of a command table, the command is in menus and has its key
only while an interface with that `:command-table` is on the screen.

A required parameter of `lambda-list` may be (`name` `type` &key `prompt`), where
`type` is evaluated: when the command is invoked without that argument, the
user is asked for it (see `accept` in present.lisp) — by clicking an object
presented with that type or, for strings, numbers and (`member` ...), in a
dialog.


<a id="command-enabled-p"></a>
### command-enabled-p

*Function*

```lisp
(command-enabled-p name)
```

True if the command `name` can be invoked now: its `:enabled` form is true.
State read by that form is observed, like in a component.


<a id="xcommand-outputx"></a>
### *command-output*

*Variable*

Initial value: `nil`

`nil`, or the stream that is `*standard-output*` while a command runs (the
interactor pane of stream.lisp, if there is one on the screen).


<a id="invoke-command"></a>
### invoke-command

*Function*

```lisp
(invoke-command name &rest arguments)
```

Call the command `name` with `arguments` if it is enabled, as the user does:
an error is shown with the restarts to choose from, and the user is asked
for the missing arguments that have a type (the command is then called
later, when they are known).


<a id="command"></a>
### command

*Macro*

```lisp
(command name &rest arguments)
```

A function (for an `:on`-<`event`> property) that invokes the command `name`
with the values of `arguments`, whatever it is called with.


<a id="command-button"></a>
### command-button

*Function*

```lisp
(command-button name &rest arguments)
```

A button with the label of the command `name` that invokes it with
`arguments`, disabled while the command is.


<a id="command-menus"></a>
### command-menus

*Function*

```lisp
(command-menus)
```

The menus: a list of (title command...), in the order of definition.


<a id="parse-key"></a>
### parse-key

*Function*

```lisp
(parse-key string)
```

The key `string`, like "CmdOrCtrl+Shift+S", as (key modifier...): the key
in upper case and the modifiers among `:ctrl` (also Cmd), `:alt` and `:shift`, sorted.


<a id="command-for-key"></a>
### command-for-key

*Function*

```lisp
(command-for-key key)
```

The enabled command whose key is `key` (as returned by `parse-key`), or `nil`.


## Menu items: what a menu shows, for the menu bar of the page (pane.lisp),

<a id="menu-item"></a>
### menu-item

*Structure*

Constructor: `(:constructor make-menu-item (label function &key (enabled t) key (checked :none) items))`

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| label |  |  | [`menu-item-label`](#menu-item) | a string; the character after & is its mnemonic |
| function |  |  | [`menu-item-function`](#menu-item) | a string; the character after & is its mnemonic |
| enabled |  |  | [`menu-item-enabled`](#menu-item) | called without arguments when the item is chosen |
| key |  |  | [`menu-item-key`](#menu-item) | the text of its key |
| checked |  |  | [`menu-item-checked`](#menu-item) | the text of its key |
| items |  |  | [`menu-item-items`](#menu-item) | `:none`, or whether the item has a check mark |


<a id="parse-mnemonic"></a>
### parse-mnemonic

*Function*

```lisp
(parse-mnemonic label)
```

`label` without the & that marks its mnemonic, and the position of the mnemonic or `nil`.


<a id="application-menus"></a>
### application-menus

*Function*

```lisp
(application-menus)
```

The menus of the application as ((title item...)...), the items being menu
items and `:separator`.


<a id="xkey-handlersx"></a>
### *key-handlers*

*Variable*

Initial value: `nil`

Functions of a key (as of `parse-key`) that return `nil` or a function to call
for it: what has keys besides the commands (the items of menus).


<a id="key-action"></a>
### key-action

*Function*

```lisp
(key-action key &key (command-predicate (constantly t)) (handlers t))
```

What the key `key` does: `nil` or a function without arguments.  It invokes
the enabled command with that key if it satisfies `command-predicate`; otherwise
(if `handlers`) it is what one of `*key-handlers*` returns.
