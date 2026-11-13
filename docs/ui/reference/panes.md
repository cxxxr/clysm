# Panes, layouts, menus and interfaces

*Generated from `ui/pane.lisp` by `tools/ui-doc.py`; do not edit.*

**Contents**

- Panes: [`pane`](#pane), [`render-pane`](#render-pane), [`pane-mounted`](#pane-mounted), [`pane-unmounted`](#pane-unmounted), [`pane-node`](#pane-node), [`callbacks`](#callbacks)
- Text: [`title-pane`](#title-pane), [`display-pane`](#display-pane), [`text-input-pane`](#text-input-pane), [`multi-line-text-input-pane`](#multi-line-text-input-pane)
- Buttons: [`push-button`](#push-button), [`check-button`](#check-button)
- Choices: panes that show items, some of which are selected: [`choice`](#choice), [`choice-selected-items`](#choice-selected-items), [`choice-selected-item`](#choice-selected-item), [`(setf choice-selected-items)`](#choice-selected-items), [`(setf choice-selected-item)`](#choice-selected-item), [`list-panel`](#list-panel), [`multi-column-list-panel`](#multi-column-list-panel), [`option-pane`](#option-pane), [`radio-button-panel`](#radio-button-panel), [`check-button-panel`](#check-button-panel), [`tree-view`](#tree-view), [`tree-view-roots`](#tree-view-roots), [`(setf tree-view-roots)`](#tree-view-roots), [`tree-view-expanded-p`](#tree-view-expanded-p), [`(setf tree-view-expanded-p)`](#tree-view-expanded-p)
- Ranges: [`range`](#range), [`slider`](#slider), [`progress-bar`](#progress-bar)
- Markup in a pane: [`application-pane`](#application-pane)
- Commands in panes: [`toolbar`](#toolbar)
- Menus (the :MENUS and :MENU-BAR of DEFINE-INTERFACE): [`menu`](#menu), [`menu-entry`](#menu-entry), [`interface-menus`](#interface-menus), [`*interfaces*`](#xinterfacesx)
- Layouts: [`layout`](#layout), [`column-layout`](#column-layout), [`row-layout`](#row-layout), [`grid-layout`](#grid-layout), [`switchable-layout`](#switchable-layout), [`tab-layout`](#tab-layout)
- More panes: [`password-pane`](#password-pane), [`editor-pane`](#editor-pane), [`radio-button`](#radio-button), [`push-button-panel`](#push-button-panel), [`text-input-choice`](#text-input-choice), [`browser-pane`](#browser-pane), [`image-pane`](#image-pane), [`color-pane`](#color-pane), [`simple-layout`](#simple-layout), [`pinboard-layout`](#pinboard-layout), [`docking-layout`](#docking-layout)
- Interfaces: [`interface`](#interface), [`interface-title`](#interface-title), [`(setf interface-title)`](#interface-title), [`find-pane`](#find-pane), [`define-interface`](#define-interface), [`window`](#window), [`interface-window`](#interface-window), [`raise-interface`](#raise-interface), [`cascade-windows`](#cascade-windows), [`tile-windows`](#tile-windows), [`display`](#display), [`contain`](#contain), [`quit-interface`](#quit-interface)

## Panes

<a id="pane"></a>
### pane

*Model class*

Something on the screen.  `title` is a label shown with it, `help` a
tooltip, `class` additional CSS classes.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| name | `:name` | `nil` | [`pane-name`](#pane) | its name in its interface (see `find-pane`) |
| interface | `:interface` | `nil` | [`pane-interface`](#pane) | the interface it belongs to |
| title | `:title` | `nil` | [`pane-title`](#pane) | a label shown with it, or `nil` |
| title-position | `:title-position` | `:top` | [`pane-title-position`](#pane) | `:top`, `:left` or `:frame` (a box around the pane) |
| enabled | `:enabled` | `t` | [`pane-enabled`](#pane) | false to show it disabled: the user cannot use it |
| visible | `:visible` | `t` | [`pane-visible`](#pane) | false to hide it |
| help | `:help` | `nil` | [`pane-help`](#pane) | the text of its tooltip, or `nil` |
| class | `:class` | `nil` | [`pane-class`](#pane) | more CSS classes of its element, or `nil` |
| width | `:width` | `nil` | [`pane-width`](#pane) | its width: a number of pixels or a CSS length |
| height | `:height` | `nil` | [`pane-height`](#pane) | its height: a number of pixels or a CSS length |
| min-width | `:min-width` | `nil` | [`pane-min-width`](#pane) | the smallest width it gets |
| min-height | `:min-height` | `nil` | [`pane-min-height`](#pane) | the smallest height it gets |
| max-width | `:max-width` | `nil` | [`pane-max-width`](#pane) | the largest width it gets |
| max-height | `:max-height` | `nil` | [`pane-max-height`](#pane) | the largest height it gets |
| stretch | `:stretch` | `nil` | [`pane-stretch`](#pane) | whether it takes the free space of its row or column: T, or a weight |
| pane-menu | `:pane-menu` | `nil` | [`pane-menu`](#pane) | the menu of the pane (right click): a menu, the name of one of the interface, or a function of the pane that returns the items of a popup menu (see `popup-menu`) |


<a id="render-pane"></a>
### render-pane

*Generic function*

```lisp
(render-pane pane)
```

The element (markup) that shows `pane` now.


<a id="pane-mounted"></a>
### pane-mounted

*Generic function*

```lisp
(pane-mounted pane)
```

Called when `pane` has been put on the display.


<a id="pane-unmounted"></a>
### pane-unmounted

*Generic function*

```lisp
(pane-unmounted pane)
```

Called when `pane` has been removed from the display.


<a id="pane-node"></a>
### pane-node

*Function*

```lisp
(pane-node pane &optional flex)
```

The node that shows `pane`, for markup: (:div (pane-node my-pane)).


<a id="callbacks"></a>
### callbacks

*Model class*

A pane with callbacks, which are called with what `callback-type`
says: by default the data and the interface.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| callback-type | `:callback-type` | `'(:data :interface)` | [`pane-callback-type`](#callbacks) | what the callbacks are called with: a list of `:data`, `:item`, `:interface` and `:element` (the pane), a keyword made of them like `:data-interface`, or `:none` |


## Text

<a id="title-pane"></a>
### title-pane

*Model class*

Superclasses: [`pane`](#pane)

A line of text.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| text | `:text` | `""` | [`title-pane-text`](#title-pane) | the text: a string, or a number or other object that is printed |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="display-pane"></a>
### display-pane

*Model class*

Superclasses: [`pane`](#pane)

Text of several lines that the user reads.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| text | `:text` | `""` | [`display-pane-text`](#display-pane) | the text; a newline in it starts a new line |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="text-input-pane"></a>
### text-input-pane

*Model class*

Superclasses: [`pane`](#pane), [`callbacks`](#callbacks)

A line of text that the user edits.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| text | `:text` | `""` | [`text-input-pane-text`](#text-input-pane) | the text in the field; it changes as the user types |
| placeholder | `:placeholder` | `nil` | [`text-input-pane-placeholder`](#text-input-pane) | the text shown, dimmed, while the field is empty |
| password | `:password` | `nil` | [`text-input-pane-password`](#text-input-pane) | true to hide what is typed |
| read-only | `:read-only` | `nil` | [`text-input-pane-read-only`](#text-input-pane) | true if the user cannot change the text |
| callback | `:callback` | `nil` | [`text-input-pane-callback`](#text-input-pane) | called with the text when the user presses Enter |
| change-callback | `:change-callback` | `nil` | [`text-input-pane-change-callback`](#text-input-pane) | called with the text whenever the user changes it |
| navigates | `:navigates` | `nil` | [`text-input-pane-navigates`](#text-input-pane) | a choice (a list) whose selection the arrow keys move while the user types here |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="multi-line-text-input-pane"></a>
### multi-line-text-input-pane

*Model class*

Superclasses: [`text-input-pane`](#text-input-pane)

Text of several lines that the user edits.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| rows | `:rows` | `4` | [`text-input-pane-rows`](#multi-line-text-input-pane) | how many lines high it is |

Inherited initargs: `:text`, `:placeholder`, `:password`, `:read-only`, `:callback`, `:change-callback`, `:navigates` from [`text-input-pane`](#text-input-pane); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


## Buttons

<a id="push-button"></a>
### push-button

*Model class*

Superclasses: [`pane`](#pane), [`callbacks`](#callbacks)

A button.  Its callback is called with its `data` (by default its text).

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| text | `:text` | `nil` | [`button-text`](#push-button) | the label (by default that of its command) |
| data | `:data` | `nil` | [`button-data`](#push-button) | what the callback gets as the data (by default the text) |
| callback | `:callback` | `nil` | [`button-callback`](#push-button) | called with the data when the button is pressed |
| command | `:command` | `nil` | [`button-command`](#push-button) | the name of a command, or (name argument...): the button invokes it, has its label and is enabled while the command is |
| default-p | `:default-p` | `nil` | [`button-default-p`](#push-button) | true for the button that answers its dialog: it is emphasized |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="check-button"></a>
### check-button

*Model class*

Superclasses: [`pane`](#pane), [`callbacks`](#callbacks)

A check box.  Its callback is called with whether it is selected.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| text | `:text` | `""` | [`button-text`](#push-button) | the label |
| selected | `:selected` | `nil` | [`button-selected`](#check-button) | whether it is checked |
| callback | `:callback` | `nil` | [`button-callback`](#push-button) | called with whether it is checked when the user changes it |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


## Choices: panes that show items, some of which are selected

<a id="choice"></a>
### choice

*Model class*

Superclasses: [`pane`](#pane), [`callbacks`](#callbacks)

The superclass of the panes that show items of which the user selects
some: lists, tables, trees, option panes, button panels, tabs, graphs.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| items | `:items` | `nil` | [`collection-items`](#choice) | the items: any objects |
| print-function | `:print-function` | `nil` | [`collection-print-function`](#choice) | a function of an item that returns its text (or a node) |
| test | `:test` | `#'eql` | [`collection-test`](#choice) | how items are compared (selection, keys of the display) |
| interaction | `:interaction` | `:single-selection` | [`choice-interaction`](#choice) | `:single-selection` or `:multiple-selection` |
| selection | `:selected-item`, `:selected-items` | `nil` | [`choice-selection`](#choice) | the selected item (or `nil`), with `:multiple-selection` the list of them |
| selection-callback | `:selection-callback` | `nil` | [`choice-selection-callback`](#choice) | called with the item that was selected |
| action-callback | `:action-callback` | `nil` | [`choice-action-callback`](#choice) | called with the item that was double-clicked (or Enter pressed on) |
| presentation-type | `:presentation-type` | `nil` | [`choice-presentation-type`](#choice) | if true, the items are presentations of this type (T: `presentation-type-of`; a function: of what it returns for the item) |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="choice-selected-items"></a>
### choice-selected-items

*Generic function*

```lisp
(choice-selected-items choice)
```

The list of the selected items of `choice`.


<a id="choice-selected-item"></a>
### choice-selected-item

*Generic function*

```lisp
(choice-selected-item choice)
```

The selected item of `choice` (the first, with multiple selection), or `nil`.


<a id="setf-choice-selected-items"></a>
### (setf choice-selected-items)

*Function*

```lisp
(setf (choice-selected-items choice) items)
```

Select the `items` of `choice` (with single selection, the first of them).


<a id="setf-choice-selected-item"></a>
### (setf choice-selected-item)

*Function*

```lisp
(setf (choice-selected-item choice) item)
```

Select `item` of `choice`, and nothing else (`nil`: nothing).


<a id="list-panel"></a>
### list-panel

*Model class*

Superclasses: [`choice`](#choice)

A list of items.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| filter | `:filter` | `nil` | [`list-panel-filter`](#list-panel) | true to show a field above the list: only the items whose text contains what the user typed there are shown |
| filter-text |  | `""` | [`list-panel-filter-text`](#list-panel) | what the user typed in the field of the filter |

Inherited initargs: `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)

Default initargs: `:stretch t`


<a id="multi-column-list-panel"></a>
### multi-column-list-panel

*Model class*

Superclasses: [`list-panel`](#list-panel)

A list of items with several columns.  A click on the title of a
column with `:sort` sorts the items by it (again: the other way round, then
not at all): `:sort` T compares what the column shows, a function what it
returns for an item — numbers by size, everything else as text.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| columns | `:columns` | `nil` | [`list-panel-columns`](#multi-column-list-panel) | a list of (:title string :width size :adjust :left/:right/:center) |
| column-function | `:column-function` | `nil` | [`list-panel-column-function`](#multi-column-list-panel) | a function of an item that returns the list of what its columns show |
| sort-column | `:sort-column` | `nil` | [`list-panel-sort-column`](#multi-column-list-panel) | the index of the column the items are sorted by (`nil`: as they are) |
| sort-descending | `:sort-descending` | `nil` | [`list-panel-sort-descending`](#multi-column-list-panel) | true if they are sorted from the largest to the smallest |

Inherited initargs: `:filter` from [`list-panel`](#list-panel); `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="option-pane"></a>
### option-pane

*Model class*

Superclasses: [`choice`](#choice)

A menu that shows the selected one of its items.

Inherited initargs: `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="radio-button-panel"></a>
### radio-button-panel

*Model class*

Superclasses: [`choice`](#choice)

A button for every item, one of which is selected.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| orientation | `:orientation` | `:row` | [`button-panel-orientation`](#radio-button-panel) | `:row` or `:column` |

Inherited initargs: `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="check-button-panel"></a>
### check-button-panel

*Model class*

Superclasses: [`radio-button-panel`](#radio-button-panel)

A check box for every item.

Inherited initargs: `:orientation` from [`radio-button-panel`](#radio-button-panel); `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)

Default initargs: `:interaction :multiple-selection`


<a id="tree-view"></a>
### tree-view

*Model class*

Superclasses: [`choice`](#choice)

The items `:roots` and what `children-function` returns for them, as a tree.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| children-function | `:children-function` | `(constantly nil)` | [`tree-view-children-function`](#tree-view) | a function of an item that returns the items under it |
| expandp-function | `:expandp-function` | `(constantly nil)` | [`tree-view-expandp-function`](#tree-view) | a function of an item: whether it is open before the user says so |

Inherited initargs: `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)

Default initargs: `:stretch t`


<a id="tree-view-roots"></a>
### tree-view-roots

*Function*

```lisp
(tree-view-roots pane)
```

The items at the top of the tree `pane` (its `collection-items`).


<a id="setf-tree-view-roots"></a>
### (setf tree-view-roots)

*Function*

```lisp
(setf (tree-view-roots pane) roots)
```


<a id="tree-view-expanded-p"></a>
### tree-view-expanded-p

*Function*

```lisp
(tree-view-expanded-p pane item)
```

True if `item` of the tree `pane` is open: the items under it are shown.
`setf` of it opens or closes it.


<a id="setf-tree-view-expanded-p"></a>
### (setf tree-view-expanded-p)

*Function*

```lisp
(setf (tree-view-expanded-p pane item) open)
```


## Ranges

<a id="range"></a>
### range

*Model class*

Superclasses: [`pane`](#pane), [`callbacks`](#callbacks)

The superclass of the panes that show a number between `start` and `end`.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| start | `:start` | `0` | [`range-start`](#range) | the smallest value |
| end | `:end` | `100` | [`range-end`](#range) | the largest value |
| value | `:value` | `0` | [`range-value`](#range) | the value |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="slider"></a>
### slider

*Model class*

Superclasses: [`range`](#range)

A number between `start` and `end` that the user sets.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| step | `:step` | `1` | [`slider-step`](#slider) | the steps in which the value changes |
| callback | `:callback` | `nil` | [`slider-callback`](#slider) | called with the value when the user moves the slider |

Inherited initargs: `:start`, `:end`, `:value` from [`range`](#range); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="progress-bar"></a>
### progress-bar

*Model class*

Superclasses: [`range`](#range)

How far something is, from `start` to `end`.

Inherited initargs: `:start`, `:end`, `:value` from [`range`](#range); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


## Markup in a pane

<a id="application-pane"></a>
### application-pane

*Model class*

Superclasses: [`pane`](#pane)

A pane that shows the markup its `display-function` returns (see
HTML and `present`); it is called again when state it read has changed.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| display-function | `:display-function` | `(constantly nil)` | [`application-pane-display-function`](#application-pane) | a function of the pane that returns what it shows: a node or a list of nodes |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)

Default initargs: `:stretch t`


## Commands in panes

<a id="toolbar"></a>
### toolbar

*Model class*

Superclasses: [`pane`](#pane)

A row of buttons for commands.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| items | `:items` | `nil` | [`toolbar-items`](#toolbar) | the names of commands, (name argument...), panes and `:separator` |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


## Menus (the :MENUS and :MENU-BAR of DEFINE-INTERFACE)

<a id="menu"></a>
### menu

*Model class*

A menu of an interface.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| name | `:name` | `nil` | [`menu-name`](#menu) | its name in its interface |
| interface | `:interface` | `nil` | [`menu-interface`](#menu) | the interface it belongs to |
| title | `:title` | `""` | [`menu-title`](#menu) | the character after & is the mnemonic: Alt and it opens the menu |
| items | `:items` | `nil` | [`menu-items`](#menu) | menu entries, `:separator`, menus (or their names in the interface: they open beside this one), names of commands and (command argument...) |
| enabled | `:enabled` | `t` | [`menu-enabled`](#menu) | false to show it disabled |


<a id="menu-entry"></a>
### menu-entry

*Model class*

Superclasses: [`callbacks`](#callbacks)

An entry of a menu.  Like the slots of panes, `enabled` and `checked` can
be (`computed` form) or (`binding` place).

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| interface | `:interface` | `nil` | [`menu-entry-interface`](#menu-entry) | the interface it belongs to: its callback gets it |
| title | `:title` | `""` | [`menu-entry-title`](#menu-entry) | the text; the character after & is its mnemonic |
| data | `:data` | `nil` | [`menu-entry-data`](#menu-entry) | what the callback gets as the data (by default the title) |
| callback | `:callback` | `nil` | [`menu-entry-callback`](#menu-entry) | called with the data (by default the title; for an entry with a check mark, whether it is checked now) and the interface |
| enabled | `:enabled` | `t` | [`menu-entry-enabled`](#menu-entry) | false to show it disabled |
| key | `:key` | `nil` | [`menu-entry-key`](#menu-entry) | like "CmdOrCtrl+E" |
| checked | `:checked` | `:none` | [`menu-entry-checked`](#menu-entry) | `:none`, or whether the entry has a check mark; choosing it changes that |

Inherited initargs: `:callback-type` from [`callbacks`](#callbacks)


<a id="interface-menus"></a>
### interface-menus

*Function*

```lisp
(interface-menus interface)
```

The menus of the menu bar of `interface` as ((title item...)...): the menus
that its `:menu-bar` names, and for T or `:commands` in it those of the commands.


<a id="xinterfacesx"></a>
### *interfaces*

*State*

Initial value: `nil`

The interfaces on the screen, the latest first.


## Layouts

<a id="layout"></a>
### layout

*Model class*

Superclasses: [`pane`](#pane)

A pane that arranges the panes of its `description`.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| description | `:description` | `nil` | [`layout-description`](#layout) | the panes: a pane, the name of a pane of the interface or a string; in rows and columns also `:divider` (the user moves it) and `:separator` |
| ratios | `:ratios` | `nil` | [`layout-ratios`](#layout) | how the free space is divided: a weight (or `nil`) for every element of the description; without it, the panes that stretch share it |
| adjust | `:adjust` | `nil` | [`layout-adjust`](#layout) | where the panes are across the direction of the layout: `nil` (each kind of pane as usual), `:start`, `:center`, `:end` or `:stretch` |
| gap | `:gap` | `nil` | [`layout-gap`](#layout) | the space between the panes: pixels or a CSS length (`nil`: the usual) |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)

Default initargs: `:stretch t`


<a id="column-layout"></a>
### column-layout

*Model class*

Superclasses: [`layout`](#layout)

The panes one below the other.

Inherited initargs: `:description`, `:ratios`, `:adjust`, `:gap` from [`layout`](#layout); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="row-layout"></a>
### row-layout

*Model class*

Superclasses: [`layout`](#layout)

The panes side by side.

Inherited initargs: `:description`, `:ratios`, `:adjust`, `:gap` from [`layout`](#layout); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="grid-layout"></a>
### grid-layout

*Model class*

Superclasses: [`layout`](#layout)

The panes in rows of `columns` columns.  `ratios` are the weights of the
columns (`nil`: as wide as needed); by default the last column takes the rest.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| columns | `:columns` | `2` | [`grid-layout-columns`](#grid-layout) | how many panes there are in a row |

Inherited initargs: `:description`, `:ratios`, `:adjust`, `:gap` from [`layout`](#layout); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="switchable-layout"></a>
### switchable-layout

*Model class*

Superclasses: [`layout`](#layout)

Shows one of its panes, `visible-child` (by default the first).

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| visible-child | `:visible-child` | `nil` | [`switchable-layout-visible-child`](#switchable-layout) | the pane it shows (or the name of one in the interface) |

Inherited initargs: `:description`, `:ratios`, `:adjust`, `:gap` from [`layout`](#layout); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="tab-layout"></a>
### tab-layout

*Model class*

Superclasses: [`choice`](#choice), [`layout`](#layout)

A tab for every item — by default an item is (title pane) — and the
pane of the selected one.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| visible-child-function | `:visible-child-function` | `#'second` | [`tab-layout-visible-child-function`](#tab-layout) | a function of an item that returns its pane (or the name of one) |

Inherited initargs: `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks); `:description`, `:ratios`, `:adjust`, `:gap` from [`layout`](#layout)

Default initargs: `:print-function #'first`


## More panes

<a id="password-pane"></a>
### password-pane

*Model class*

Superclasses: [`text-input-pane`](#text-input-pane)

A text input pane that hides what is typed.

Inherited initargs: `:text`, `:placeholder`, `:password`, `:read-only`, `:callback`, `:change-callback`, `:navigates` from [`text-input-pane`](#text-input-pane); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)

Default initargs: `:password t`


<a id="editor-pane"></a>
### editor-pane

*Model class*

Superclasses: [`multi-line-text-input-pane`](#multi-line-text-input-pane)

Text of many lines that the user edits, in a fixed-width font.

Inherited initargs: `:rows` from [`multi-line-text-input-pane`](#multi-line-text-input-pane); `:text`, `:placeholder`, `:password`, `:read-only`, `:callback`, `:change-callback`, `:navigates` from [`text-input-pane`](#text-input-pane); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)

Default initargs: `:rows 12 :class "ui-editor-pane" :stretch t`


<a id="radio-button"></a>
### radio-button

*Model class*

Superclasses: [`check-button`](#check-button)

A button that is selected instead of the others of its `group` in the
same interface.  Its callback is called with its data when it is selected.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| group | `:group` | `nil` | [`radio-button-group`](#radio-button) | the buttons with the same `group` (any object) exclude each other |

Inherited initargs: `:text`, `:selected`, `:callback` from [`check-button`](#check-button); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="push-button-panel"></a>
### push-button-panel

*Model class*

Superclasses: [`choice`](#choice)

A push button for every item; the selection callback is called with
the item whose button was pressed.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| orientation | `:orientation` | `:row` | [`button-panel-orientation`](#radio-button-panel) | `:row` or `:column` |

Inherited initargs: `:items`, `:print-function`, `:test`, `:interaction`, `:selected-item`, `:selected-items`, `:selection-callback`, `:action-callback`, `:presentation-type` from [`choice`](#choice); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="text-input-choice"></a>
### text-input-choice

*Model class*

Superclasses: [`text-input-pane`](#text-input-pane)

A text input pane with a list of texts to choose from.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| items | `:items` | `nil` | [`collection-items`](#choice) | what the user can choose instead of typing |

Inherited initargs: `:text`, `:placeholder`, `:password`, `:read-only`, `:callback`, `:change-callback`, `:navigates` from [`text-input-pane`](#text-input-pane); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="browser-pane"></a>
### browser-pane

*Model class*

Superclasses: [`pane`](#pane)

A pane that shows the page at URL.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| url | `:url` | `"about:blank"` | [`browser-pane-url`](#browser-pane) | the address of the page |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)

Default initargs: `:stretch t`


<a id="image-pane"></a>
### image-pane

*Model class*

Superclasses: [`pane`](#pane)

A pane that shows the image at URL.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| url | `:url` | `nil` | [`image-pane-url`](#image-pane) | the address of the image |
| description | `:description` | `""` | [`image-pane-description`](#image-pane) | what the image shows, for those who do not see it |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="color-pane"></a>
### color-pane

*Model class*

Superclasses: [`pane`](#pane), [`callbacks`](#callbacks)

A colour that the user chooses.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| color | `:color` | `"#000000"` | [`color-pane-color`](#color-pane) | like "#3366cc" |
| callback | `:callback` | `nil` | [`color-pane-callback`](#color-pane) | called with the colour when the user changes it |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane); `:callback-type` from [`callbacks`](#callbacks)


<a id="simple-layout"></a>
### simple-layout

*Model class*

Superclasses: [`layout`](#layout)

A layout of one pane (with `:title` and `:title-position` `:frame`, a box around it).

Inherited initargs: `:description`, `:ratios`, `:adjust`, `:gap` from [`layout`](#layout); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="pinboard-layout"></a>
### pinboard-layout

*Model class*

Superclasses: [`layout`](#layout)

Its panes are where they say: an element of the description is
(pane x y) or (pane x y width height), in pixels.

Inherited initargs: `:description`, `:ratios`, `:adjust`, `:gap` from [`layout`](#layout); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


<a id="docking-layout"></a>
### docking-layout

*Model class*

Superclasses: [`layout`](#layout)

Panes with a title bar each — an element of the description is
(title pane) — that the user takes out into a window of its own (and back by
closing that window).

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| orientation | `:orientation` | `:column` | [`docking-layout-orientation`](#docking-layout) | `:column` (the panes one below the other) or `:row` |

Inherited initargs: `:description`, `:ratios`, `:adjust`, `:gap` from [`layout`](#layout); `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)


## Interfaces

<a id="interface"></a>
### interface

*Model class*

Superclasses: [`pane`](#pane)

A window: panes in a layout, with a title.  See `define-interface`.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| layout | `:layout` | `nil` | [`interface-layout`](#interface) | the pane it shows, or the name of one of its panes |
| menu-bar | `:menu-bar` | `nil` | [`interface-menu-bar`](#interface) | true to show the menus of the commands above the layout (in the desktop application they are in its menu bar instead) |
| command-table | `:command-table` | `nil` | [`interface-command-table`](#interface) | the name of a command table: its commands are offered (menus, keys) while the interface is on the screen |
| destroy-callback | `:destroy-callback` | `nil` | [`interface-destroy-callback`](#interface) | called with the interface when it is taken off the screen |
| pointer-documentation | `:pointer-documentation` | `nil` | [`interface-pointer-documentation`](#interface) | true to show, below the layout, what the pointer can do where it is |

Inherited initargs: `:name`, `:interface`, `:title`, `:title-position`, `:enabled`, `:visible`, `:help`, `:class`, `:width`, `:height`, `:min-width`, `:min-height`, `:max-width`, `:max-height`, `:stretch`, `:pane-menu` from [`pane`](#pane)

Default initargs: `:stretch t`


<a id="interface-title"></a>
### interface-title

*Function*

```lisp
(interface-title interface)
```

The title of `interface`: of the window of the page, or of its window or dialog.


<a id="setf-interface-title"></a>
### (setf interface-title)

*Function*

```lisp
(setf (interface-title interface) title)
```


<a id="find-pane"></a>
### find-pane

*Generic function*

```lisp
(find-pane interface name)
```

The pane or layout of `interface` that is named `name`, or `nil`.


<a id="define-interface"></a>
### define-interface

*Macro*

```lisp
(define-interface name superclasses slots &rest options)
```

Define the class `name` of interfaces, like `defmodel` with the options

```lisp
(:panes (name class initarg...)...)
(:layouts (name class description initarg...)...)
(:menus (name title (item...) initarg...)...)
(:menu-bar menu-name...)
```

An item of a menu is ("title" initarg...) — a `menu-entry` with `:callback`,
`:key`, `:enabled`, `:checked`... —, the name of another menu (which opens beside
it) or of a command, (command argument...) or `:separator`.  The menu bar
shows the named menus, and the menus of the commands where it says `:commands`.
Every instance has these panes and layouts, made with `make-instance` from the
initargs (`:accessor` and `:reader` name functions for the slot instead) and
kept in the slots `name`.  The initarg forms are evaluated when the instance
is made, with `interface` bound to it and its slots, also the panes made so
far, as variables.  The interface shows the first layout (or the one named
by the initarg `:layout`); without layouts, its panes in a column.


<a id="window"></a>
### window

*Model class*

A window of an interface above the application (`display` with `:window` T);
see `interface-window`.

| Slot | Initargs | Default | Accessors | Description |
|---|---|---|---|---|
| interface | `:interface` |  | [`window-interface`](#window) | the interface it shows |
| x | `:x` |  | [`window-x`](#window) | the position of its left side, in pixels from the left of the page |
| y | `:y` |  | [`window-y`](#window) | the position of its top, in pixels from the top of the page |
| width | `:width` |  | [`window-width`](#window) | its width in pixels |
| height | `:height` |  | [`window-height`](#window) | its height in pixels |
| maximized |  | `nil` | [`window-maximized`](#window) | true while it covers the page |
| z | `:z` |  | `window-z` | the window with the largest number is on top |


<a id="interface-window"></a>
### interface-window

*Function*

```lisp
(interface-window interface)
```

The window of `interface` (see the argument `:window` of `display`), or `nil`.


<a id="raise-interface"></a>
### raise-interface

*Function*

```lisp
(raise-interface interface)
```

Put the window of `interface` on top of the others.


<a id="cascade-windows"></a>
### cascade-windows

*Function*

```lisp
(cascade-windows)
```

Put the windows one behind the other, each a little further right and down.


<a id="tile-windows"></a>
### tile-windows

*Function*

```lisp
(tile-windows)
```

Put the windows side by side so that they cover the screen.


<a id="display"></a>
### display

*Function*

```lisp
(display interface &key (container "#app") window x y (width 480) (height 320))
```

Show `interface` in `container` (for the DOM, a CSS selector or a node); the
title of the window is its title.  With `window` true, show it instead in a
window of its own above the application, at X, Y with the size `width`,
`height`: the user moves it by its title, resizes it at its corner, maximizes
and closes it (see also `raise-interface`, `tile-windows`, `cascade-windows`).
Return the interface.


<a id="contain"></a>
### contain

*Function*

```lisp
(contain pane &rest initargs)
```

Show `pane` in an interface made for it (with `initargs`, like `:title`);
return the interface.


<a id="quit-interface"></a>
### quit-interface

*Function*

```lisp
(quit-interface interface)
```

Take `interface` (or the interface of a pane) off the screen.
