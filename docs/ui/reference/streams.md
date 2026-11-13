# Stream panes and the interactor

*Generated from `ui/stream.lisp` by `tools/ui-doc.py`; do not edit.*

**Contents**

- Output records: what was written: [`stream-pane`](#stream-pane), [`window-clear`](#window-clear)
- Writing more than characters: [`with-output-as-presentation`](presentations.md#with-output-as-presentation), [`write-markup`](#write-markup), [`with-text-style`](#with-text-style), [`formatting-table`](#formatting-table), [`formatting-row`](#formatting-row), [`formatting-cell`](#formatting-cell), [`formatting-item-list`](#formatting-item-list)
- Showing the records: [`present`](presentations.md#present)
- Reading an object that the user typed: [`accept-from-string`](#accept-from-string), [`presentation-type-choices`](#presentation-type-choices)
- The interactor: [`interactor-pane`](#interactor-pane)

## Output records: what was written

<a id="stream-pane"></a>
### stream-pane

*Model class*

Superclasses: [`pane`](panes.md#pane), `gray:fundamental-character-output-stream`

A pane that is a character output stream.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| display-function | `:display-function` | `nil` | [`stream-pane-display-function`](#stream-pane) | a function of the pane that writes what it shows; it is called again when state it read has changed.  Without it, the pane shows what was written to it since `window-clear`. |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](panes.md#pane)

Default initargs: `:stretch t`


<a id="window-clear"></a>
### window-clear

*Function*

```lisp
(window-clear pane)
```

Forget what was written to the stream pane `pane`.


## Writing more than characters

<a id="with-output-as-presentation"></a>
### with-output-as-presentation

*Macro*

```lisp
(with-output-as-presentation (first &optional second third) &body body)
```

(with-output-as-presentation (object [type]) markup...) is the element of
the markup (evaluated as by HTML) as a presentation of `object` with the
presentation type `type` (evaluated; by default `presentation-type-of` the object).
(with-output-as-presentation (stream object [type]) form...) makes what the
forms write to the stream pane `stream` a presentation of `object`.


<a id="write-markup"></a>
### write-markup

*Function*

```lisp
(write-markup pane function)
```

Write to the stream pane `pane` what `function` returns (a node of HTML, or a
list of nodes) whenever the pane is shown.


<a id="with-text-style"></a>
### with-text-style

*Macro*

```lisp
(with-text-style (pane &rest style &key bold italic color size class) &body body)
```

What `body` writes to the stream pane `pane` is `bold`, `italic`, of the `color` (a
CSS colour), `size` (a CSS length or a number of pixels) or has the CSS `class`.


<a id="formatting-table"></a>
### formatting-table

*Macro*

```lisp
(formatting-table (pane) &body body)
```

What `body` writes to the stream pane `pane` with `formatting-row` and
`formatting-cell` is a table.


<a id="formatting-row"></a>
### formatting-row

*Macro*

```lisp
(formatting-row (pane) &body body)
```

A row of the `formatting-table`: its cells are what `body` writes with `formatting-cell`.


<a id="formatting-cell"></a>
### formatting-cell

*Macro*

```lisp
(formatting-cell (pane &key (align :left)) &body body)
```

A cell of a table or item list: what `body` writes, ALIGNed `:left`, `:right` or `:center`.


<a id="formatting-item-list"></a>
### formatting-item-list

*Macro*

```lisp
(formatting-item-list (pane &key (columns 3)) &body body)
```

What `body` writes with `formatting-cell` is laid out in `columns` columns.


## Showing the records

<a id="present"></a>
### present

*Function*

```lisp
(present object &optional type &key stream)
```

A node that shows `object` (as `present-object` says) and is a presentation
of it with the presentation type `type`.  With `stream`, a stream pane, write
that to it instead.


## Reading an object that the user typed

<a id="accept-from-string"></a>
### accept-from-string

*Generic function*

```lisp
(accept-from-string name type string)
```

The object of the presentation type `type` (whose name is `name`) that
`string` denotes, and true — or `nil` and `nil` if it denotes none.  Methods for
the types of an application let the user type its objects in the interactor.


<a id="presentation-type-choices"></a>
### presentation-type-choices

*Generic function*

```lisp
(presentation-type-choices name type)
```

The strings the user may type for an object of the presentation
type `type`: what the interactor offers as completions.


## The interactor

<a id="interactor-pane"></a>
### interactor-pane

*Model class*

Superclasses: [`stream-pane`](#stream-pane)

A stream pane with a line in which the user types commands: the name
of a command (its label, or the name of its function) and its arguments,
strings with spaces in double quotes.  The Tab key completes, the arrows
recall what was entered.  While an argument is asked for (`accept`), the line
asks for it: the user types it or clicks a presentation.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| prompt | `:prompt` | `"Command:"` | [`interactor-prompt`](#interactor-pane) | what is shown in front of the line |
| input |  | `""` | [`interactor-input`](#interactor-pane) | the text in the line |

Inherited initargs: `:display-function` from [`stream-pane`](#stream-pane); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](panes.md#pane)
