# Markup, components and state

*Generated from `ui/ui.lisp` by `tools/ui-doc.py`; do not edit.*

**Contents**

- The backend protocol: [`*backend*`](#xbackendx), [`make-element-node`](#make-element-node), [`make-text-node`](#make-text-node), [`set-node-text`](#set-node-text), [`set-property`](#set-property), [`remove-property`](#remove-property), [`set-listener`](#set-listener), [`insert-node`](#insert-node), [`remove-node`](#remove-node), [`replace-node`](#replace-node), [`node-follows-p`](#node-follows-p), [`request-flush`](#request-flush), [`resolve-container`](#resolve-container), [`event-get`](#event-get), [`event-target-get`](#event-target-get), [`stop-event`](#stop-event), [`node-call`](#node-call), [`overlay-container`](#overlay-container), [`set-global-listener`](#set-global-listener), [`set-title`](#set-title), [`reveal-selection`](#reveal-selection), [`event-position`](#event-position), [`native-menus-p`](#native-menus-p), [`can-wait-p`](#can-wait-p), [`wait-for`](#wait-for), [`capture-pointer`](#capture-pointer), [`viewport-size`](#viewport-size), [`begin-drag`](#begin-drag), [`install-commands`](#install-commands), [`choose-file`](#choose-file), [`write-clipboard`](#write-clipboard), [`read-clipboard`](#read-clipboard), [`previous-sibling-size`](#previous-sibling-size)
- Errors: the user chooses a restart: [`choose-restart`](#choose-restart), [`*error-message-function*`](#xerror-message-functionx), [`*paste-function*`](#xpaste-functionx), [`*restart-chooser*`](#xrestart-chooserx), [`*abort-label*`](#xabort-labelx), [`call-with-error-dialog`](#call-with-error-dialog), [`with-error-dialog`](#with-error-dialog)
- Observers: what runs again when the state it read changes: [`mounted`](#mounted), [`unmounted`](#unmounted), [`flush`](#flush), [`autorun`](#autorun), [`stop`](#stop)
- Models: classes whose slots are observed: [`model-class`](#model-class), [`model`](#model), [`computed`](#computed), [`computed`](#computed), [`binding`](#binding), [`defmodel`](#defmodel), [`defstate`](#defstate)
- Nodes: the description of the display that components return: [`keyed`](#keyed)
- Markup: (:tag :property value ... child ...): [`html`](#html)
- Components: [`component`](#component), [`render`](#render), [`defcomponent`](#defcomponent)
- Putting nodes on the display, and changing it to a new description: [`refresh`](#refresh), [`mount`](#mount), [`unmount`](#unmount)
- Waiting for the user inside a computation: [`wait-for-answer`](#wait-for-answer)

## The backend protocol

<a id="xbackendx"></a>
### *backend*

*Variable*

Initial value: `nil`

The backend used by `mount`.


<a id="make-element-node"></a>
### make-element-node

*Generic function*

```lisp
(make-element-node backend tag)
```

Make an element; `tag` is a lowercase string.


<a id="make-text-node"></a>
### make-text-node

*Generic function*

```lisp
(make-text-node backend string)
```

Make a node of text that shows `string`.


<a id="set-node-text"></a>
### set-node-text

*Generic function*

```lisp
(set-node-text backend node string)
```

Make the node of text `node` show `string`.


<a id="set-property"></a>
### set-property

*Generic function*

```lisp
(set-property backend node name value)
```

Set the property `name` (a keyword) of `node` to `value`, which is not `nil`.


<a id="remove-property"></a>
### remove-property

*Generic function*

```lisp
(remove-property backend node name old-value)
```

Undo `set-property` of `name`; `old-value` is the value it had (or `nil`).


<a id="set-listener"></a>
### set-listener

*Generic function*

```lisp
(set-listener backend node event function)
```

Call `function` with the event when `event` (a string like "click")
happens on `node`.  Called once for every event of a node.


<a id="insert-node"></a>
### insert-node

*Generic function*

```lisp
(insert-node backend parent node before)
```

Make `node` the child of `parent` in front of the child `before`
(at the end if `before` is `nil`), moving it if it is in the tree.


<a id="remove-node"></a>
### remove-node

*Generic function*

```lisp
(remove-node backend node)
```

Take `node` out of the tree.


<a id="replace-node"></a>
### replace-node

*Generic function*

```lisp
(replace-node backend old new)
```

Put the node `new` where the node `old` is, and take `old` out of the tree.


<a id="node-follows-p"></a>
### node-follows-p

*Generic function*

```lisp
(node-follows-p backend node next)
```

True if the sibling after `node` is `next` (`nil`: `node` is the last child).


<a id="request-flush"></a>
### request-flush

*Generic function*

```lisp
(request-flush backend)
```

Arrange for `flush` to be called soon (once the current event is handled).


<a id="resolve-container"></a>
### resolve-container

*Generic function*

```lisp
(resolve-container backend container)
```

The node for the `container` argument of `mount`.


<a id="event-get"></a>
### event-get

*Generic function*

```lisp
(event-get backend event name)
```

The property `name` (a string: "key", "clientX", "shiftKey"...) of `event`.


<a id="event-target-get"></a>
### event-target-get

*Generic function*

```lisp
(event-target-get backend event name)
```

The property `name` ("value", "checked", "selectedIndex") of the node `event` happened on.


<a id="stop-event"></a>
### stop-event

*Generic function*

```lisp
(stop-event backend event)
```

`event` has been handled: no default action, and no other listener.


<a id="node-call"></a>
### node-call

*Generic function*

```lisp
(node-call backend node method &rest arguments)
```

Call `method` (a string: "focus", "showModal"...) of the backend's `node`.


<a id="overlay-container"></a>
### overlay-container

*Generic function*

```lisp
(overlay-container backend)
```

The node above everything else: for dialogs and popup menus.


<a id="set-global-listener"></a>
### set-global-listener

*Generic function*

```lisp
(set-global-listener backend event function)
```

Call `function` with the event when `event` happens anywhere.


<a id="set-title"></a>
### set-title

*Generic function*

```lisp
(set-title backend string)
```

Set the title of the window.


<a id="reveal-selection"></a>
### reveal-selection

*Generic function*

```lisp
(reveal-selection backend event)
```

Once the display is up to date, scroll the selected item of the
node `event` happened on into view.


<a id="event-position"></a>
### event-position

*Generic function*

```lisp
(event-position backend event)
```

The position X, Y of the pointer `event` in the coordinates of the
drawing (an `output-pane`) it happened on.


<a id="native-menus-p"></a>
### native-menus-p

*Generic function*

```lisp
(native-menus-p backend)
```

True if the menus of the commands are shown outside the page.


<a id="can-wait-p"></a>
### can-wait-p

*Generic function*

```lisp
(can-wait-p backend)
```

True if `wait-for` can wait now.


<a id="wait-for"></a>
### wait-for

*Generic function*

```lisp
(wait-for backend function)
```

Call `function` with a function of one argument, the answer, and
return the answer once that has been called — later, when the user has
done something; meanwhile the display works.  Only if `can-wait-p`.


<a id="capture-pointer"></a>
### capture-pointer

*Generic function*

```lisp
(capture-pointer backend event)
```

The node that has the listener of the pointer `event` gets the
following events of that pointer.


<a id="viewport-size"></a>
### viewport-size

*Generic function*

```lisp
(viewport-size backend)
```

The width and height of the window.


<a id="begin-drag"></a>
### begin-drag

*Generic function*

```lisp
(begin-drag backend event text)
```

The drag that `event` starts carries `text` (for other applications).


<a id="install-commands"></a>
### install-commands

*Generic function*

```lisp
(install-commands backend &key &allow-other-keys)
```

Make the commands available to the user in the way of
`backend`: in a browser by their keys, in the desktop application also in the
menu bar.


<a id="choose-file"></a>
### choose-file

*Generic function*

```lisp
(choose-file backend continuation &key accept)
```

Let the user choose a file to read (`accept`: what kinds, like
".json,.txt") and call `continuation` with its name and its text, or with `nil`
and `nil` if none was chosen.


<a id="write-clipboard"></a>
### write-clipboard

*Generic function*

```lisp
(write-clipboard backend text)
```

Put `text` on the clipboard.


<a id="read-clipboard"></a>
### read-clipboard

*Generic function*

```lisp
(read-clipboard backend continuation)
```

Call `continuation` with the text on the clipboard (`nil` if it cannot be read).


<a id="previous-sibling-size"></a>
### previous-sibling-size

*Generic function*

```lisp
(previous-sibling-size backend event)
```

The width and height of the node in front of the one the pointer
`event` happened on, which gets the following events of that pointer.


## Errors: the user chooses a restart

<a id="choose-restart"></a>
### choose-restart

*Generic function*

```lisp
(choose-restart backend condition restarts)
```

Tell the user about `condition` and ask which of `restarts` (never
empty, the last is `abort`) to invoke; return it, or `nil` for `abort`.


<a id="xerror-message-functionx"></a>
### *error-message-function*

*Variable*

Initial value: `nil`

`nil`, or a function of a string that shows the message of an error that
has no restart to choose (dialog.lisp sets it).


<a id="xpaste-functionx"></a>
### *paste-function*

*Variable*

Initial value: `nil`

`nil`, or a function of a continuation that asks the user to paste the text
of the clipboard and calls the continuation with it (or `nil`): what a backend
does that is not allowed to read the clipboard itself (dialog.lisp sets it).


<a id="xrestart-chooserx"></a>
### *restart-chooser*

*Variable*

Initial value: `nil`

`nil`, or a function of an error and its restarts (more than one) that asks
the user in the page which to invoke and returns it (dialog.lisp sets it).
It is called where the backend can wait (`can-wait-p`).


<a id="xabort-labelx"></a>
### *abort-label*

*Variable*

Initial value: `"Cancel"`

How the `abort` restart of `with-error-dialog` is described to the user.


<a id="call-with-error-dialog"></a>
### call-with-error-dialog

*Function*

```lisp
(call-with-error-dialog function)
```

Call `function` as `with-error-dialog` evaluates its body.


<a id="with-error-dialog"></a>
### with-error-dialog

*Macro*

```lisp
(with-error-dialog &body body)
```

Evaluate `body`; if it signals an error, the user is shown the error and
chooses one of the active restarts.  The restart `abort` established here
returns `nil` from the form.


## Observers: what runs again when the state it read changes

<a id="mounted"></a>
### mounted

*Generic function*

```lisp
(mounted component)
```

Called when the nodes of `component` have been put on the display.


<a id="unmounted"></a>
### unmounted

*Generic function*

```lisp
(unmounted component)
```

Called when `component` has been removed from the display.


<a id="flush"></a>
### flush

*Function*

```lisp
(flush)
```

Bring the display up to date with the state: run the observers whose
state changed (parents before the components inside them).


<a id="autorun"></a>
### autorun

*Function*

```lisp
(autorun function)
```

Call `function` now, and again (in `flush`) whenever state it read has
changed.  `stop` of the value ends that.


<a id="stop"></a>
### stop

*Function*

```lisp
(stop reaction)
```

Stop the function that `autorun` returned `reaction` for: it is not called again.


## Models: classes whose slots are observed

<a id="model-class"></a>
### model-class

*Class*

Superclasses: `standard-class`

The metaclass of models: reading a slot of an instance records the
running observer, and writing it invalidates the observers that read it.


<a id="model"></a>
### model

*Class*

The superclass of the classes `defmodel` defines: their slots are observed.


<a id="computed"></a>
### computed

*Structure*

What the macros `computed` and `binding` put into a slot of a model.

Constructor: `(:constructor make-computed (function &optional setter))`


<a id="computed"></a>
### computed

*Macro*

```lisp
(computed form)
```

A value for a slot of a model: the slot then has the value of `form`,
evaluated whenever the slot is read.  Setting the slot replaces it.


<a id="binding"></a>
### binding

*Macro*

```lisp
(binding place)
```

A value for a slot of a model: the slot then is `place`; reading it reads
`place` and setting it sets `place`.


<a id="defmodel"></a>
### defmodel

*Macro*

```lisp
(defmodel name superclasses slots &rest options)
```

Like `defclass`, for a class whose slots are observed: components that read
a slot of an instance are rendered again when the slot is set.


<a id="defstate"></a>
### defstate

*Macro*

```lisp
(defstate name &optional value documentation)
```

Define `name` as an observed global place with the initial `value`: components
that read it are rendered again when it is set (`setf`, `push`, `incf`...).  `name`
is a symbol macro, not a special variable: `let` of it is a lexical binding.


## Nodes: the description of the display that components return

<a id="keyed"></a>
### keyed

*Function*

```lisp
(keyed key node)
```

`node` with the key `key`.  Among the children of an element, a node with a
key takes over the display (and the component state) of the previous child
with an `equal` key, wherever that was.


## Markup: (:tag :property value ... child ...)

<a id="html"></a>
### html

*Macro*

```lisp
(html &body body)
```

Evaluate `body`, in which a list that starts with a keyword is markup:

```lisp
(:tag :property value ... child ...)
```

makes an element.  Properties are those of the backend's nodes (`:class`,
`:value`, `:hidden`...; `nil` removes one), `:on`-<`event`> a function called with the
event, `:on-mount` a function called with the backend's node when it is on the
display, `:key` the key (see `keyed`).  A child is a node, a string or number
(text), `nil` (nothing) or a list of children.


## Components

<a id="component"></a>
### component

*Class*

Superclasses: `observer`

The superclass of the classes of components (`defcomponent`): an
instance is a component on the display.


<a id="render"></a>
### render

*Generic function*

```lisp
(render component)
```

The node that describes `component` now.


<a id="defcomponent"></a>
### defcomponent

*Macro*

```lisp
(defcomponent name lambda-list &body body)
```

Define the component `name`: a function with `lambda-list` that returns a node
for the markup, and the class `name` of its instances on the display.
`body` is evaluated as by HTML, whenever state it read has changed or the
arguments are different, and returns one node.

After `&state` in `lambda-list` come variables, as `var` or (`var` `initial-value`),
that keep their values as long as the instance is on the display; setting
one renders the component again.  `self` is the instance.


## Putting nodes on the display, and changing it to a new description

<a id="refresh"></a>
### refresh

*Function*

```lisp
(refresh)
```

Render every component on the display again (after functions that they
call were redefined).


<a id="mount"></a>
### mount

*Function*

```lisp
(mount node container &key (backend *backend*))
```

Put `node` (the value of a component function or of HTML) on the display
as the last child of `container`; from then on it follows the state.
Return the node, for `unmount`.


<a id="unmount"></a>
### unmount

*Function*

```lisp
(unmount node &key (backend *backend*))
```

Take `node`, which `mount` returned, off the display; its components are gone.


## Waiting for the user inside a computation

<a id="wait-for-answer"></a>
### wait-for-answer

*Function*

```lisp
(wait-for-answer function)
```

Call `function` with a function of one argument, the answer; return the
answer once that has been called.  `function` shows something (a dialog) whose
listeners give the answer; until then this computation waits while the
display works, as if it were not running: listeners, rendering and `flush`
start afresh.  Only where (can-wait-p *backend*).
