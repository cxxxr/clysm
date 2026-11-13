# Drawing and graphs

*Generated from `ui/graphics.lisp` by `tools/ui-doc.py`; do not edit.*

**Contents**

- [`output-pane`](#output-pane)
- The graphics state: [`with-graphics-state`](#with-graphics-state), [`draw-line`](#draw-line), [`draw-lines`](#draw-lines), [`draw-polygon`](#draw-polygon), [`draw-rectangle`](#draw-rectangle), [`draw-circle`](#draw-circle), [`draw-ellipse`](#draw-ellipse), [`draw-arc`](#draw-arc), [`draw-path`](#draw-path), [`draw-string`](#draw-string), [`draw-image`](#draw-image)
- Groups and transformations: [`with-drawing-group`](#with-drawing-group), [`with-graphics-translation`](#with-graphics-translation), [`with-graphics-rotation`](#with-graphics-rotation), [`with-graphics-scale`](#with-graphics-scale)
- Graphs (the graph pane of CAPI, FORMAT-GRAPH-FROM-ROOTS of CLIM): [`graph-pane`](#graph-pane), [`graph-pane-roots`](#graph-pane-roots), [`(setf graph-pane-roots)`](#graph-pane-roots)

<a id="output-pane"></a>
### output-pane

*Model class*

Superclasses: [`pane`](panes.md#pane), [`callbacks`](panes.md#callbacks)

A pane for drawing.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| display-callback | `:display-callback` | `nil` | [`output-pane-display-callback`](#output-pane) | a function of the pane that draws with the functions below |
| view-width | `:view-width` | `nil` | [`output-pane-view-width`](#output-pane) | the size of the drawing in its own coordinates; it is scaled to the pane.  Without it, the coordinates are the pixels of the pane. |
| view-height | `:view-height` | `nil` | [`output-pane-view-height`](#output-pane) | (see `view-width`) |
| press-callback | `:press-callback` | `nil` | [`output-pane-press-callback`](#output-pane) | functions of the pane and the position X, Y in the drawing where the pointer was pressed, moved and released |
| motion-callback | `:motion-callback` | `nil` | [`output-pane-motion-callback`](#output-pane) | called with the pane and X, Y when the pointer moves on the drawing |
| release-callback | `:release-callback` | `nil` | [`output-pane-release-callback`](#output-pane) | called with the pane and X, Y when the pointer is released |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](panes.md#pane); `:callback-type` from [`callbacks`](panes.md#callbacks)

Default initargs: `:stretch t`


## The graphics state

<a id="with-graphics-state"></a>
### with-graphics-state

*Macro*

```lisp
(with-graphics-state (pane &rest state) &body body)
```

Draw what `body` draws with `state` (`:foreground`, `:thickness`...) as the
defaults of the drawing functions.


<a id="draw-line"></a>
### draw-line

*Function*

```lisp
(draw-line pane x1 y1 x2 y2 &rest options)
```

Draw a line from X1, Y1 to X2, Y2.


<a id="draw-lines"></a>
### draw-lines

*Function*

```lisp
(draw-lines pane points &rest options)
```

Draw the line through `points` (x y x y..., or a list of (x y)).


<a id="draw-polygon"></a>
### draw-polygon

*Function*

```lisp
(draw-polygon pane points &rest options)
```

Draw the closed line through `points` (x y x y..., or a list of (x y)).


<a id="draw-rectangle"></a>
### draw-rectangle

*Function*

```lisp
(draw-rectangle pane x y width height &rest options &key radius &allow-other-keys)
```

Draw a rectangle; `radius` rounds its corners.


<a id="draw-circle"></a>
### draw-circle

*Function*

```lisp
(draw-circle pane x y radius &rest options)
```

Draw a circle around X, Y; with `:filled`, a disc.


<a id="draw-ellipse"></a>
### draw-ellipse

*Function*

```lisp
(draw-ellipse pane x y x-radius y-radius &rest options)
```

Draw an ellipse around X, Y with the radii `x-radius` and `y-radius`.


<a id="draw-arc"></a>
### draw-arc

*Function*

```lisp
(draw-arc pane x y radius start-angle sweep-angle &rest options)
```

Draw the arc of the circle around X, Y that starts at `start-angle` and
goes on for `sweep-angle` (radians, from the x axis towards the y axis); with
`:filled`, the sector.


<a id="draw-path"></a>
### draw-path

*Function*

```lisp
(draw-path pane data &rest options)
```

Draw the path `data`, in the notation of SVG paths ("M0 0 L10 10 Z").


<a id="draw-string"></a>
### draw-string

*Function*

```lisp
(draw-string pane string x y &rest options &key font-size font-weight (anchor :start) (baseline :alphabetic) &allow-other-keys)
```

Draw `string` at X, Y: `anchor` says whether that is its `:start`, `:middle` or
`:end`, `baseline` whether it is its `:alphabetic` baseline, its `:middle` or its `:top`.


<a id="draw-image"></a>
### draw-image

*Function*

```lisp
(draw-image pane url x y width height &rest options)
```

Draw the image at URL in the rectangle X, Y, `width`, `height`.


## Groups and transformations

<a id="with-drawing-group"></a>
### with-drawing-group

*Macro*

```lisp
(with-drawing-group (pane &key class) &body body)
```

What `body` draws as one shape; its node is returned, so that

```lisp
(with-output-as-presentation (object) (with-drawing-group (pane) ...))
```

makes a presentation of several shapes.


<a id="with-graphics-translation"></a>
### with-graphics-translation

*Macro*

```lisp
(with-graphics-translation (pane dx dy) &body body)
```

Draw what `body` draws moved by `dx`, `dy`.


<a id="with-graphics-rotation"></a>
### with-graphics-rotation

*Macro*

```lisp
(with-graphics-rotation (pane angle &optional (x 0) (y 0)) &body body)
```

Draw what `body` draws turned by `angle` (radians) around X, Y.


<a id="with-graphics-scale"></a>
### with-graphics-scale

*Macro*

```lisp
(with-graphics-scale (pane sx &optional (sy sx)) &body body)
```

Draw what `body` draws scaled by `sx` and `sy`.


## Graphs (the graph pane of CAPI, FORMAT-GRAPH-FROM-ROOTS of CLIM)

<a id="graph-pane"></a>
### graph-pane

*Model class*

Superclasses: [`choice`](panes.md#choice)

The items `:roots` and what `children-function` returns for them, as
boxes joined by edges.  An item that is reached several times is shown once.
The items are selected and activated as in a list, and are presentations if
the pane has a `:presentation-type`.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| children-function | `:children-function` | `(constantly nil)` | [`graph-pane-children-function`](#graph-pane) | a function of an item that returns the items its edges lead to |
| direction | `:direction` | `:left-right` | [`graph-pane-direction`](#graph-pane) | `:left-right` or `:top-down` |

Inherited initargs: `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](panes.md#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](panes.md#pane); `:callback-type` from [`callbacks`](panes.md#callbacks)

Default initargs: `:stretch t`


<a id="graph-pane-roots"></a>
### graph-pane-roots

*Function*

```lisp
(graph-pane-roots pane)
```

The items the graph `pane` starts from (its `collection-items`).


<a id="setf-graph-pane-roots"></a>
### (setf graph-pane-roots)

*Function*

```lisp
(setf (graph-pane-roots pane) roots)
```
