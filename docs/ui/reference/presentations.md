# Presentations and popup menus

*Generated from `ui/present.lisp` by `tools/ui-doc.py`; do not edit.*

**Contents**

- The overlay: [`*windows*`](#xwindowsx), [`*accept-prompt*`](#xaccept-promptx), [`cancel-accept`](#cancel-accept), [`accepting-p`](#accepting-p), [`popup-menu`](#popup-menu)
- Presentation types: [`define-presentation-type`](#define-presentation-type), [`presentation-type-of`](#presentation-type-of), [`presentation-subtypep`](#presentation-subtypep)
- ACCEPT: ask the user for an object: [`accept-presentation-type`](#accept-presentation-type), [`accept`](#accept)
- Translators: what a gesture on a presentation does: [`define-presentation-to-command-translator`](#define-presentation-to-command-translator)
- Presentations: [`define-drag-and-drop-translator`](#define-drag-and-drop-translator), [`*pointer-documentation-labels*`](#xpointer-documentation-labelsx), [`presenting`](#presenting), [`with-output-as-presentation`](#with-output-as-presentation), [`present-object`](#present-object), [`present`](#present)

## The overlay

<a id="xwindowsx"></a>
### *windows*

*State*

Initial value: `nil`

The windows on the screen (pane.lisp), in the order they were made.


<a id="xaccept-promptx"></a>
### *accept-prompt*

*Variable*

Initial value: `"Select ~A"`

The format control of the prompt of `accept` for a presentation type (its description).


<a id="cancel-accept"></a>
### cancel-accept

*Function*

```lisp
(cancel-accept)
```

Give up what `accept` is waiting for.


<a id="accepting-p"></a>
### accepting-p

*Function*

```lisp
(accepting-p)
```

True while the user is asked to choose a presentation.


<a id="popup-menu"></a>
### popup-menu

*Function*

```lisp
(popup-menu items x y)
```

Show a menu of `items` at the position X, Y of the window.  An item is a
menu item (`make-menu-item` label function &key enabled key), the name of a
command, a list (command argument...) or `:separator`.


## Presentation types

<a id="define-presentation-type"></a>
### define-presentation-type

*Macro*

```lisp
(define-presentation-type name &optional supertypes &key description)
```

Define `name` as a presentation type that is a subtype of `supertypes`.
`description` is how it is called in a prompt.  (The name of a class is a
presentation type without this; it may still be given a description.)


<a id="presentation-type-of"></a>
### presentation-type-of

*Function*

```lisp
(presentation-type-of object)
```

The presentation type `object` is presented with by default.


<a id="presentation-subtypep"></a>
### presentation-subtypep

*Function*

```lisp
(presentation-subtypep type supertype)
```

True if the presentation type `type` is `supertype` or a subtype of it: by the
supertypes of `define-presentation-type`, or as `subtypep` says.


## ACCEPT: ask the user for an object

<a id="accept-presentation-type"></a>
### accept-presentation-type

*Generic function*

```lisp
(accept-presentation-type name type prompt continuation)
```

Ask the user for an object of the presentation type `type`, whose
name is `name` (`type` itself, or its first element), and call `continuation`
with it.  The default method lets the user click a presentation;
dialog.lisp has methods for strings, numbers, `boolean` and `member`.


<a id="accept"></a>
### accept

*Function*

```lisp
(accept type &key prompt continuation)
```

Ask the user for an object of the presentation type `type` and call
`continuation` with it (nothing is called if the user cancels).  Returns at
once.  `prompt` is the text shown meanwhile.


## Translators: what a gesture on a presentation does

<a id="define-presentation-to-command-translator"></a>
### define-presentation-to-command-translator

*Macro*

```lisp
(define-presentation-to-command-translator name (type command &key (gesture :select) label tester) (object) &body body)
```

On the gesture `gesture` (`:select` is a click, `:menu` only the menu) on a
presentation whose type is a subtype of `type`, call `command` with the
arguments that `body` returns as a list, `object` being the presented object.
The translator is in the menu of the presentation (with `label`, by default
the label of the command), and applies when the command is enabled and
`tester`, a function of the object, returns true.


## Presentations

<a id="define-drag-and-drop-translator"></a>
### define-drag-and-drop-translator

*Macro*

```lisp
(define-drag-and-drop-translator name (from-type to-type command &key tester) (object destination) &body body)
```

When a presentation whose type is a subtype of `from-type` is dragged onto
one whose type is a subtype of `to-type`, call `command` with the arguments that
`body` returns as a list, `object` and `destination` being the two objects.  It
applies when the command is enabled and `tester`, a function of the two
objects, returns true.


<a id="xpointer-documentation-labelsx"></a>
### *pointer-documentation-labels*

*Variable*

Initial value: `'("Click: ~A" "Right click: menu" "Drag to drop")`

The format controls of the pointer documentation: for the command of a
click (its label), for the menu and for dragging.


<a id="presenting"></a>
### presenting

*Function*

```lisp
(presenting object type node)
```

Make the element `node` (the value of markup) a presentation of `object` with
the presentation type `type` (`nil`: `presentation-type-of`); return `node`.


<a id="with-output-as-presentation"></a>
### with-output-as-presentation

*Macro*

```lisp
(with-output-as-presentation (object &optional type) &body markup)
```

The element of `markup` (evaluated as by HTML) as a presentation of `object`
with the presentation type `type` (evaluated; by default `presentation-type-of`
the object).


<a id="present-object"></a>
### present-object

*Generic function*

```lisp
(present-object object type)
```

What shows `object` as the presentation type `type`: a string, a node
or a list of them.


<a id="present"></a>
### present

*Function*

```lisp
(present object &optional type)
```

A node that shows `object` (as `present-object` says) and is a presentation
of it with the presentation type `type`.
