# The S-expression backend

*Generated from `ui/sexp.lisp` by `tools/ui-doc.py`; do not edit.*

<a id="sexp-backend"></a>
### sexp-backend

*Class*

A backend whose display is a tree of S-expressions, for tests: it
records every change (`backend-log`), `fire` calls listeners, and the user's
answers are functions (`backend-chooser`, `backend-waiter`, `backend-file`).

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| chooser | `:chooser` | `nil` | [`backend-chooser`](#sexp-backend) | `nil`, or a function of an error and its restarts that returns the restart the "user" chooses (`nil`: abort) |
| waiter | `:waiter` | `nil` | [`backend-waiter`](#sexp-backend) | `nil`, or a function called by `wait-for` in place of the user: it does what gives the answer (`fire` of a button of the dialog) |
| file | `:file` | `nil` | [`backend-file`](#sexp-backend) | (name text) of the file the "user" chooses, and the clipboard |
| clipboard |  | `nil` | [`backend-clipboard`](#sexp-backend) | the text on its clipboard |


<a id="backend-log"></a>
### backend-log

*Function*

```lisp
(backend-log backend)
```

The operations on the display since the last call, oldest first.


<a id="make-root"></a>
### make-root

*Function*

```lisp
(make-root)
```

A new node of the S-expression backend to `mount` or `display` into.


<a id="named-root"></a>
### named-root

*Function*

```lisp
(named-root name)
```

The root (as of `make-root`) for the container `name`, a string: what a
program that displays into "#app" uses on this backend.


<a id="named-roots"></a>
### named-roots

*Function*

```lisp
(named-roots)
```

The names and roots of `named-root`, as (name . root)..., oldest first.


<a id="fire-global"></a>
### fire-global

*Function*

```lisp
(fire-global backend event &rest arguments)
```

Call the global listeners of `backend` for `event`, then bring the display up to date.


<a id="node-text"></a>
### node-text

*Function*

```lisp
(node-text node)
```

The text under `node` of the display.


<a id="to-sexp"></a>
### to-sexp

*Function*

```lisp
(to-sexp node)
```

`node` of the display as (:tag (property value...) child...), text as strings.


<a id="find-nodes"></a>
### find-nodes

*Function*

```lisp
(find-nodes root tag &key class)
```

The elements under `root` with the tag `tag` (and, if given, the `:class` property `class`).


<a id="find-node"></a>
### find-node

*Function*

```lisp
(find-node root tag &key class (index 0))
```

The element number `index` of `find-nodes` (by default the first), or `nil`.


<a id="fire"></a>
### fire

*Function*

```lisp
(fire node event &rest arguments)
```

Call the listener of `node` for `event` with `arguments`, then bring the display up to date.
