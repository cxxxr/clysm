# Clysm — Common Lisp on WebAssembly GC

Clysm is a Common Lisp implementation written from scratch that compiles Lisp
to **WebAssembly GC** binaries. It follows the design in
`Wasm向けLispコンパイラ開発仕様.pdf`: Lisp objects are WasmGC structs/arrays and
`i31ref` immediates, memory is managed by the host GC, non-local exits use the
Wasm exception handling proposal (`try_table` / `throw_ref`), and tail calls use
`return_call` / `return_call_ref`.

```
$ bin/clysm run tests/hello.lisp          # compile with the runtime, then run on Node.js
$ bin/clysm compile prog.lisp -o prog.wasm
$ bin/clysm repl                          # a REPL in Wasm; every form is compiled to a new Wasm module
                                          # (~/.clysmrc is loaded first, unless --no-userinit)
$ bin/clysm eval '(expt 2 100)'
$ bin/clysm script file.lisp              # LOAD a file with the in-image evaluator
```

Requirements: SBCL (hosts the cross compiler) and Node.js ≥ 22 (WasmGC,
exnref and tail calls enabled by default).

```
$ make test        # compile and run every test case, compare with SBCL
$ make test-all    # the same in all four modes (AOT, interpreter, run-time native, self-hosted)
$ make fixpoint    # the compiler compiles itself twice; the results are identical
$ make web         # browser demo in build/web
```

## Layout

| Path | Contents |
|------|----------|
| `src/wasm.lisp` | Wasm binary encoder (GC types in a single rec group, EH `try_table`, tail calls, symbolic labels) |
| `src/reader.lisp` | host-side reader for target source (target packages mirrored as host packages, backquote) |
| `src/layout.lisp` | WasmGC object layout |
| `src/frontend.lisp` | macroexpansion, special forms → AST, closure/assignment conversion analysis |
| `src/codegen.lisp` | AST → Wasm instructions (calling convention, MV, dynamic binding, non-local exits) |
| `src/prims.lisp` | inline primitives and hand-written Wasm runtime helpers |
| `src/driver.lisp` | toplevel processing (`eval-when`, `defmacro`, `defpackage`…) and module assembly |
| `runtime/*.lisp` | the Common Lisp library, written in Lisp and compiled by Clysm |
| `ui/*.lisp` | the GUI framework (see [GUI framework](#gui-framework)); `ui/electron/` is its desktop shell |
| `host/run.mjs` | Node.js host: I/O imports, runs the module in a worker with a large stack |
| `tools/run-tests.sh` | runs every `tests/cases/*.lisp` on SBCL and Clysm and compares the output |

## Object representation (spec: データ構造のWasmGCマッピング)

The universal value type is `(ref null eq)`.

* **fixnum** — `i31ref`, values in [-2^29, 2^29). **character** — `i31ref` with
  the upper-bit tag `01` (value = 2^29 + code point). Both are scalars for the GC.
* **NIL / T** — singleton `$symbol` structs held in immutable globals (NIL is a
  real symbol, so `symbol-name`/`symbol-plist` work on it).
* **cons** — `struct { car: mut eqref, cdr: mut eqref }`.
* **symbol** — `struct { name: (ref $string), package: mut (ref null $package),
  value: mut eqref, function: mut (ref null $closure), plist: mut eqref, hash: i32, flags: mut i32 }`.
  Unbound = null value slot.
* **string** — `(array (mut i32))` of code points; **simple-vector** — `(array (mut eqref))`;
  also byte, bit and `f64` vectors. Non-simple arrays (fill pointers, adjustable,
  multi-dimensional, displaced) use a header struct around a simple array.
* **bignum** — `struct { sign: i32, digits: mut (ref null (array i32)), value: i64 }`
  (base 2^28 digits). Integers beyond the fixnum range that fit in 64 bits are
  computed with i64 arithmetic and made without digits (the `value` field holds
  them); the digits are made when the general bignum code asks for them.
  **ratio**, **complex**, **single-float** (`f32`), **double-float** (`f64`) structs.
* **package** — `$package` struct with hash tables of symbols.
* **instances** (structures, conditions, CLOS objects) — `struct { layout, slots, hash: mut i32 }`
  where the layout is the class object (the hash caches the identity hash).
* **closures** — see below. **foreign objects** — `struct { externref }` wrapper.

All types live in one recursive type group, so structurally identical types
(e.g. strings vs. bignum digit arrays) remain distinct for `ref.test`. Type
predicates compile to `ref.test`, never to pointer tag arithmetic.

## Calling convention (spec: ハイブリッド渡し)

A closure is `struct { entry0..entry4, entryN, env, name }`. A call with k ≤ 4
arguments uses `call_ref` on `entry_k` with the arguments as native Wasm
parameters; more arguments, `apply`, and `multiple-value-call` use `entryN`,
which takes an argument array. For a function with only required parameters the
matching entry *is* the function body; the other fixed entries are shared
argument-count-error stubs, and `entryN` is a small unpacking adapter. Functions
with `&optional/&rest/&key` take the argument array directly (shared packing
stubs fill the fixed entries). Calls to functions defined in the same program
are compiled as direct `call`s (CLHS 3.2.2.3), and tail calls become
`return_call` / `return_call_ref`.

Closure conversion: free variables are copied into the closure's environment
array; variables that are both captured and assigned are boxed in cells.
`labels` closures are created first and their environments patched afterwards.

## Multiple values (spec: 多値リターン用バッファ)

The primary value is the Wasm return value. A global `mv-count` and a global
value buffer hold the full set; `values` fills them, and every non-call
expression in a multiple-value context sets the count to 1. Calls in tail
position pass values through untouched.

## Dynamic binding (spec: シャローバインディング)

The symbol's value slot holds the current value; binding pushes the old value
on a global binding stack. Every exit point (normal return, `catch`,
`unwind-protect` cleanup, non-local `return-from`/`go`) restores the binding
stack to its saved depth. A `let` of up to six special variables writes its
stack entries inline with one check of the stack size, and stores the old
values back from their known positions at the normal exit.

## Non-local exits (spec: Wasm EH)

`block`/`return-from` and `tagbody`/`go` inside one function compile to Wasm
`block`/`br` and a `loop` + `br_table` dispatcher. An exit through
`unwind-protect` forms of the same function sets an exit number and branches to
the innermost cleanup, which runs and then continues the exit (to the next
cleanup or the target). When the exit crosses a closure boundary, a fresh tag
object is created and the exit `throw`s the single exception tag
`$lisp-throw(tag, value)`; the target is
a `try_table` with `catch_ref` that compares the tag and otherwise rethrows with
`throw_ref`. `catch`/`throw` use the same mechanism (with a dynamic list of
active catch tags so that throwing to a missing tag signals `control-error`).
`unwind-protect` uses `catch_all_ref`, runs the cleanup (saving multiple
values), and resumes with `throw_ref`.

A Wasm exception is expensive in V8 (about 85 ns per throw plus 33 ns per
frame it unwinds; SBCL's `throw` takes about 7 ns), so an exit is not thrown
where the frames it leaves are known. Such an exit is a *return with the exit
pending*: the tag and the value are stored in two internal variables, the
function returns (through its own cleanups and bindings, like a local exit),
and the code after the call tests for a pending exit. If its tag belongs to a
catcher around the call in that function (a non-local block or tagbody, a
`catch`, a `handler-case` or `restart-case`), it branches there; if the
function itself returns its exits this way, it returns in turn; otherwise it
throws the exit as described above. The functions that return their exits
("flagging" functions) are:

* local functions (`flet`/`labels`) that are only called, never used as
  values, and contain a `return-from`/`go` to an enclosing function or a
  `throw`, or call such a function;
* *flag clones*: a direct call of a known global function with a catcher
  around it calls a copy of the function compiled this way (when it contains
  an exit and is not large), and the copy calls the copy of itself and of the
  functions it tail-calls;
* the twins of `error`, `signal`, `warn`, `cerror`, `invoke-restart` and the
  standard restart functions (`runtime/cond.lisp`, `%define-twin`), which
  direct calls use where a catcher is around the call or the caller is
  flagging; the functions themselves throw;
* the lambdas written as handlers in `handler-bind`, whose only callers are
  the signalling functions.

So `(handler-case (f x) (my-error (c) ...))` where `f` calls `error`,
`handler-bind` with a handler that does `return-from` or `invoke-restart`,
and a recursive tree walk that leaves by `return-from` or `throw`, run
without a Wasm exception. An exit from a closure called by a library
function (`(block b (maphash (lambda (k v) ... (return-from b k)) table))`),
or from several ordinary calls below the catcher, is still thrown.

`handler-case` is one `sys:%catch` (a `catch` whose tag is not recorded in
the list of active tags) whose tag is the handler cluster itself, a small
vector `#(next types clause-number)` bound to the list of clusters; `signal`
stores the number of the clause and leaves for it with the condition, and the
clauses are inline code after the catch. `restart-case` is built the same way
around its first restart object.

## The runtime

Written in Lisp (`runtime/`): macros (`setf` machinery, `loop`,
`destructuring-bind`…), lists, the numeric tower with bignums and ratios,
sequences, strings, characters, arrays, hash tables (insertion ordered),
packages, `defstruct`, the condition system with restarts, streams, the printer
(including an XP-style pretty printer that reproduces SBCL's layout), `format`,
CLOS (generic functions with standard and operator method combinations,
`defclass`, `make-instance`, `initialize-instance`, `print-object`, slot
access) with a metaobject protocol (below), the reader with readtables, an
evaluator for `eval`/`load`, and a REPL.

Macro expanders run on the host during compilation and are also compiled into
the image, so `macroexpand`, `eval` and the REPL can use every macro.

## Self-hosting

The compiler is itself written in portable Common Lisp, so Clysm can compile
itself to WebAssembly:

```
$ tools/build-selfhost.sh                       # SBCL-hosted Clysm compiles the compiler -> build/clysmc.wasm
$ node host/run.mjs build/clysmc.wasm out.wasm prog.lisp   # the compiler running in Wasm
$ node host/run.mjs build/clysmc.wasm build/clysmc2.wasm src/*.lisp ...   # it compiles itself
```

The second- and third-generation compilers are byte-for-byte identical (a
fixed point), and the whole test suite passes when compiled by the Wasm-hosted
compiler (`SELFHOST=1 tools/run-tests.sh`). Inside the image, macro expanders
and `eval-when` code are run by Clysm's own evaluator.

## Compiling inside the running image

The REPL image (`tools/build-repl.lisp`) embeds the compiler. `compile`, `load`
and every form typed at the REPL are compiled to a **new WebAssembly module
that is linked with the running image** and executed immediately:

* the base types form one rec group that is identical in every module, so
  WasmGC struct/array/function types are shared between modules;
* the multiple-value buffer, the binding stack and the exception tag are
  exported by the image and imported by each new module;
* symbols and literal objects are passed in at instantiation as a vector of the
  live objects themselves (so `eq`-ness of constants is preserved);
* in this "live" mode the compiler takes macros, special proclamations and
  constants from the image.

For example `(defun fib ...)` at the REPL produces native code: `(fib 27)` runs
about 175× faster than with the interpreter (`bin/clysm repl --interp`).
`NATIVE=1 tools/run-tests.sh` runs the whole test suite this way. The browser
demo also embeds the compiler, so code typed in the page is compiled to Wasm in
the browser.

## JavaScript interoperability (spec: externref ラッパー構造体)

JavaScript values are `externref`s, which cannot be stored in Lisp data, so they
are wrapped in a `$foreign` struct. The `JS` package converts automatically
(numbers, strings, booleans, `NIL`→`null`, vectors/lists→arrays, functions→JS
functions that call back into Lisp through the exported `lisp_apply`):

```lisp
(js:log "hello" 42)
(js:call (js:global "Math") "max" 3 9 4)          ; => 9
(js:get (js:global "Math") "PI")                  ; => 3.141592653589793d0
(js:funcall (js:eval "(f, x) => f(x) * 2") (lambda (x) (+ x 1)) 20)   ; => 42
(js:call (js:global "JSON") "stringify" (js:object "name" "clysm"))
```

`js:get` of `null` or `undefined` (`NIL`) is `NIL`; a JavaScript exception in
`js:call`, `js:funcall` or `js:new` is a Lisp error.

### A browser and desktop application

`examples/todo/` is a TODO list written entirely in Lisp with the
[UI framework](#gui-framework): `todo.lisp` describes the page with
components, defines its operations as commands, and saves through the `JS`
package. It is compiled ahead of time to `todo.wasm`, which `index.html`
loads with the browser host (`host/web/clysm.mjs`).

```
$ examples/todo/build.sh                    # build/todo/
$ python3 -m http.server -d build/todo      # open http://localhost:8000/
```

The same program is a desktop application in the Electron shell
`ui/electron/`: there it saves the list to a JSON file, its commands are in
the menu bar, files are chosen with the native dialogs, and a file that
cannot be read or written is reported with the restarts to choose from. In a
browser it uses `localStorage`.

```
$ examples/todo/build-desktop.sh            # build/todo-desktop/, installs Electron with npm
$ (cd build/todo-desktop && npm start)
```

`examples/contacts/` is an address book written with the
[panes](#panes-layouts-and-interfaces) of the framework: its window is an
interface of lists, a tree, a table, tabs and a form whose fields are the
slots of the selected contact, and its contacts are
[presentations](#presentations-and-commands-with-typed-arguments) — "Delete…"
in the menu asks the user to click a contact, and a right click on a contact
offers the commands for it.

```
$ examples/contacts/build.sh                # build/contacts/
$ python3 -m http.server -d build/contacts
$ examples/contacts/build-desktop.sh        # build/contacts-desktop/, the desktop application
```

With `--dev`, these scripts build the [development version](#development-with-a-repl),
which has a REPL in its window:

```
TODO> (push (make-instance 'todo :text "from the REPL") *todos*)
TODO> (defcomponent filter-button (filter label) ...)     ; the buttons change
```

## GUI framework

`ui/` is a framework for graphical applications. The state of an application
is ordinary CLOS objects and variables, and the screen follows them: a
component is rendered again when a slot or variable that it read is set. No
code tells the screen to update.

```lisp
(ui:defmodel todo ()                       ; DEFCLASS, with observed slots
  ((text :initarg :text :accessor todo-text)
   (done :initform nil :accessor todo-done)))

(ui:defstate *todos* nil)                  ; an observed global variable

(ui:defcomponent todo-item (todo &state (hover nil))
  (:li :class (if (todo-done todo) "done" nil)
       :on-mouseenter (lambda (e) (setf hover t))
       :on-mouseleave (lambda (e) (setf hover nil))
    (:input :type "checkbox" :checked (todo-done todo)
            :on-change (lambda (e) (setf (todo-done todo) (not (todo-done todo)))))
    (todo-text todo)
    (when hover
      (:button :on-click (lambda (e) (setf *todos* (remove todo *todos*))) "×"))))

(ui:defcomponent todo-list ()
  (:div
    (:ul (mapcar (lambda (todo) (ui:keyed todo (todo-item todo))) *todos*))
    (:p (count-if-not #'todo-done *todos*) " left")))

(ui:mount (todo-list) "#app")
(push (make-instance 'todo :text "write Lisp") *todos*)    ; the list shows it
```

* **Markup** — in the body of `defcomponent` (and in `ui:html`), a list that
  starts with a keyword is an element: `(:tag :property value ... child ...)`.
  Everything else is Lisp, so conditions and loops are `if`, `when`, `mapcar`
  or `loop`. A child is a node, a string or number, `nil` (nothing) or a list
  of children. `:on-<event>` properties are functions called with the event,
  `:on-mount` one called with the node once it is on the screen.
* **`defmodel`** is `defclass` with the metaclass `ui:model-class`, whose
  `slot-value-using-class` methods record the component that reads a slot and
  invalidate it when the slot is written. **`defstate`** defines a global place
  with the same behaviour (a symbol macro, so `setf`, `push`, `incf` work).
  Only assignments are seen: replace a list instead of modifying its conses.
* **`defcomponent`** defines a function that returns a node and a class for
  the instances on the screen; `ui:render` is a generic function, and
  `ui:mounted` / `ui:unmounted` are called when an instance appears and
  disappears. Variables after `&state` belong to the instance. A component
  called again with `equal` arguments is not rendered again, and `ui:keyed`
  children keep their instance when the list is reordered.
* **Updates** are collected and applied once after the event handler has
  returned (`ui:flush`), by comparing the new description of each invalidated
  component with the previous one and changing only what differs.
  `(ui:autorun function)` runs a function again whenever state it read
  changes (saving, the window title).
* **Backends** — the framework changes the screen through a dozen generic
  functions (`ui:make-element-node`, `ui:set-property`, `ui:insert-node`…).
  `ui/dom.lisp` implements them for the browser's DOM; `ui/sexp.lisp` builds
  S-expressions and records every change, so components are tested without a
  browser (`make test-ui`, `tests/ui/`).

An application is compiled together with the framework (`ui/desktop.lisp`
only if it is also a desktop application):

```
$ bin/clysm compile ui/ui.lisp ui/command.lisp ui/dom.lisp ui/desktop.lisp app.lisp -o app.wasm
```

### Panes, layouts and interfaces

On top of the components, `ui/pane.lisp` has ready-made panes in the manner
of CAPI: an application describes its window as an interface of panes in
layouts, without markup.

```lisp
(ui:define-interface greeter ()
  ((count :initform 0 :accessor greeter-count))             ; observed slots
  (:panes
   (name-input ui:text-input-pane :placeholder "Your name")
   (greet ui:push-button :text "Greet"
          :callback (lambda (data interface) (incf (greeter-count interface))))
   (message ui:title-pane
            :text (ui:computed (format nil "Hello, ~A! (~D)"
                                       (ui:text-input-pane-text name-input)
                                       (greeter-count ui:interface)))))
  (:layouts
   (main ui:column-layout '(top message))
   (top ui:row-layout '(name-input greet)))
  (:default-initargs :title "Greeter"))

(ui:display (make-instance 'greeter))                       ; in "#app"
```

* **Panes are models**: what a pane shows is in its slots, and the screen
  follows `setf` of an accessor — `(setf (ui:button-text button) "Again")`,
  `(setf (ui:collection-items list) items)`, `(setf (ui:pane-enabled pane) nil)`.
  A slot of any model can be given `(ui:computed form)`, a value that follows
  the state `form` reads, or `(ui:binding place)`, which makes the slot that
  place in both directions: `:text (ui:binding (contact-name contact))` is a
  field that edits the name.
* **The panes**: `title-pane`, `display-pane`, `text-input-pane`,
  `multi-line-text-input-pane`, `push-button` (with `:callback`, or
  `:command` for a [command](#commands)), `check-button`, the choices
  `list-panel`, `multi-column-list-panel`, `option-pane`, `radio-button-panel`,
  `check-button-panel` and `tree-view` (`:items`, `:print-function`,
  `:interaction`, `:selection-callback`, `:action-callback`;
  `ui:choice-selected-item`; the arrow keys select, and a click on the title
  of a table column with `:sort` sorts by it), `slider`, `progress-bar`, `toolbar` (buttons
  for commands) and `application-pane`, whose `:display-function` returns
  markup. Callbacks get what `:callback-type` says, by default the data and
  the interface.
* **The layouts**: `column-layout`, `row-layout` (with `:ratios`, and
  `:divider` in the description for a border the user drags), `grid-layout`,
  `tab-layout` and `switchable-layout`. A description names panes of the
  interface, or contains panes and strings.
* **`define-interface`** is `defmodel` plus `:panes` and `:layouts`; the
  initarg forms see `ui:interface` and the panes made before them.
  `ui:display` shows an interface in an element of the page (`ui:contain` a
  single pane), with `:menu-bar t` under the menus of the commands (in the
  desktop application they are in its menu bar instead).
* **A new kind of pane** is a class and a method of `ui:render-pane` that
  returns markup; `(ui:pane-node pane)` puts a pane into markup.
* **The look** is `ui/ui.css`, linked from the page: light and dark, and
  custom properties (`--ui-accent`, `--ui-radius`, `--ui-font`…) to change it.

```
$ bin/clysm compile ui/ui.lisp ui/command.lisp ui/present.lisp ui/pane.lisp ui/dialog.lisp \
    ui/graphics.lisp ui/dom.lisp app.lisp -o app.wasm
```

### Drawing

An `ui:output-pane` (`ui/graphics.lisp`) shows what its `:display-callback`
draws with `ui:draw-line`, `draw-lines`, `draw-rectangle`, `draw-circle`,
`draw-ellipse`, `draw-arc`, `draw-polygon`, `draw-path` and `draw-string`:

```lisp
(make-instance 'ui:output-pane
  :view-width 200 :view-height 100                 ; its coordinates, scaled to the pane
  :press-callback (lambda (pane x y) ...)          ; also :motion-callback, :release-callback
  :display-callback
  (lambda (pane)
    (ui:with-graphics-state (pane :foreground "steelblue" :thickness 2)
      (ui:draw-lines pane (route-points *route*)))
    (dolist (city *cities*)
      (ui:with-output-as-presentation (city 'city) ; a shape that is a city
        (ui:draw-circle pane (city-x city) (city-y city) 4 :filled t)))))
```

The graphics state is keyword arguments (`:foreground`, `:filled`, `:fill`,
`:thickness`, `:dashes`, `:opacity`, `:class`) with the defaults of
`ui:with-graphics-state`; `with-graphics-translation`, `-rotation` and
`-scale` transform, and `with-drawing-group` makes one shape of several. A
drawing is a tree of shapes (SVG), not pixels: the callback runs again when
state it read has changed and only the shapes that differ change, and a
shape can be a presentation like anything else on the screen.

### Presentations and commands with typed arguments

After CLIM (`ui/present.lisp`): what is shown with `ui:present` or
`ui:with-output-as-presentation` remembers the object it stands for and its
presentation type, and a command says what its arguments are.

```lisp
(ui:define-command (delete-contact :menu "Contact" :label "Delete…")
    ((contact 'contact :prompt "Click the contact to delete"))
  ...)
(ui:define-command (rename-contact :menu "Contact") ((contact 'contact) (name 'string))
  (setf (contact-name contact) name))

(ui:defcomponent favourites ()
  (:p (mapcar #'ui:present *favourites*)))                  ; contacts on the screen
(make-instance 'ui:list-panel :items *contacts* :presentation-type 'contact ...)

(ui:define-presentation-to-command-translator click-contact (contact show-contact) (object)
  (list object))                                            ; a click calls SHOW-CONTACT
```

* Invoked without its arguments (from a menu, a button, its key),
  a command asks for them one after the other (`ui:accept`): for a
  contact, the presentations of contacts anywhere on the screen are
  highlighted and a click on one supplies it (Escape cancels); a `string`,
  `integer`, `number`, `boolean` or `(member ...)` is asked for in a dialog.
  `ui:accept-presentation-type` has a method for each way of asking.
* A right click on a presentation shows the commands that apply to it: its
  translators, and the commands whose first parameter has its type.
* A presentation type is the name of a class or another type specifier;
  `ui:define-presentation-type` gives one supertypes and a description, and
  methods of `ui:present-object` say how objects are shown.

`ui:command-palette` (Ctrl+Shift+P) is a dialog in which a command is chosen
by typing part of its name — every command of the application, which then
asks for its arguments.

### Dialogs

`ui/dialog.lisp`: `ui:display-message`, `ui:prompt-for-string`,
`ui:prompt-for-number`, `ui:prompt-for-confirmation` and
`ui:prompt-with-list` show a modal dialog, and `(ui:display-dialog interface)`
makes any interface one, answered by its callbacks with
`(ui:exit-dialog value)` or `(ui:abort-dialog)`. A page cannot wait inside a
function, so these return at once and call their `:continuation` with the
answer and whether there is one:

```lisp
(ui:prompt-for-confirmation "Delete it?"
                            :continuation (lambda (yes okp) (when yes (delete-it))))
```

### Commands

A command (`ui/command.lisp`) is a function with a label and, optionally, a
menu, a key and a condition under which it is enabled:

```lisp
(ui:define-command (open-list :menu "File" :label "Open…" :key "CmdOrCtrl+O") ()
  ...)
(ui:define-command (clear-completed :menu "List" :label "Clear completed"
                                    :enabled (some #'todo-done *todos*)) ()
  (setf *todos* (remove-if #'todo-done *todos*)))
```

With `:table 'name`, a command belongs to a command table: it is in the
menus and has its key only while an interface made with
`:command-table 'name` is on the screen.

The same command is used everywhere: `(ui:command clear-completed)` is a
listener for a button (and `(ui:command-button 'clear-completed)` a button
that is disabled while the command is), `(ui:install-commands ui:*backend*)`
binds the keys in a browser and builds the menu bar of the desktop
application, and `(clear-completed)` calls it from the REPL. The `:enabled`
form is observed like the state read by a component, so buttons and menu
items follow it.

### Errors and restarts

Listeners, commands and rendering run inside `ui:with-error-dialog`. An
error there does not stop the application: the user is shown the message
and the restarts that are active — those the program established around the
failing operation, and `abort` — and the one they choose is invoked.

```lisp
(defun save ()
  (restart-case (ui:write-file *file* (todos-json))
    (retry () :report "Try again" (save))
    (save-elsewhere () :report "Save to another file…" (save-list-as))))
```

The desktop application asks with a native message box whose buttons are the
restarts, a browser with `prompt` (with `ui/dialog.lisp`, an error that has
no restart to choose is shown in a dialog of the page), and the S-expression backend calls
`(ui:backend-chooser backend)`. A component that fails to render keeps what
it showed.

### Desktop applications

`ui/electron/` is an [Electron](https://www.electronjs.org/) shell without
anything specific to an application: it opens the window described in the
application's `package.json` and shows its page. When a program runs in it,
`ui/desktop.lisp` makes `ui:desktop-backend` the backend — the DOM backend,
plus the menu bar built from the commands, native dialogs
(`ui:choose-file-to-open`, `ui:choose-file-to-save`, `ui:message-box`) and
files (`ui:read-file`, `ui:write-file`, `ui:user-data-file`). `(ui:desktop-p)`
tells whether it does, so one program can be a web page and a desktop
application.

### Development with a REPL

`tools/build-ui-dev.sh OUT.wasm APP.lisp...` builds a development image: the
compiler, the framework and the application. A page that loads it docks a
REPL (`host/web/repl.mjs`, see `examples/todo/index.html`), and what is
evaluated there is compiled in the page and changes the running application:

* a redefined component is shown at once, with the state of its instances
  (`&state` variables can be added);
* a redefined function is used from then on (the functions of a development
  image are compiled `notinline`), and every component is rendered again
  after each evaluation;
* the instances of a redefined `defmodel` class get its new slots
  (`sys::track-instances` is on);
* a command defined or changed there appears in the menu bar.

## ASDF

ASDF 3.3.7 (the latest release, with a small port in `third_party/asdf/`) is bundled:

```lisp
(require :asdf)
(push #p"/path/to/alexandria/" asdf:*central-registry*)   ; or ~/common-lisp/, the source registry...
(asdf:load-system :alexandria)          ; or (require :alexandria)
(asdf:test-system :alexandria)
```

Systems are compiled with `compile-file` and loaded into the running image.
A fasl holds the compiled Wasm modules (consecutive ordinary forms share one
module) with their constants, and the source forms as a fallback; the fasl
directory under `~/.cache/common-lisp/` names the image build, so rebuilding
Clysm recompiles systems. ASDF itself is compiled once per image build
(`~/.cache/common-lisp/clysm-asdf/`). `make test-asdf`
loads a small multi-system example (`tests/asdf/`). There are no subprocesses,
so `uiop:run-program` is not available.

## Quicklisp

Quicklisp runs on Clysm: `contrib/ql-clysm.lisp` adds Clysm to Quicklisp's
implementation layer (TCP connections, directory listing), and is installed
automatically when ASDF loads the Quicklisp client. TCP connections are
synchronous sockets provided by the Node.js host (`sys::open-tcp-stream`): the
Lisp program runs in a worker thread, which blocks with `Atomics.wait` while
the main thread performs the socket operation.

```lisp
;; an existing installation ($QUICKLISP_HOME, by default ~/quicklisp/)
(require :quicklisp)
(ql:quickload :cl-ppcre)
(ql:add-to-init-file)        ; load Quicklisp in every REPL (writes ~/.clysmrc)

;; a new installation with the bootstrap file from https://beta.quicklisp.org/quicklisp.lisp
(load "quicklisp.lisp")
(require :ql-clysm)
(quicklisp-quickstart:install :path "/path/to/quicklisp/")
```

`make test-quicklisp` installs Quicklisp from scratch into `build/` and loads
libraries downloaded from quicklisp.org (split-sequence, and `str` with its
dependencies cl-ppcre, cl-unicode, flexi-streams and cl-change-case).

## Metaobject protocol

The package `CLYSM-MOP` (like SBCL's `SB-MOP`) exports the AMOP's metaobject
classes and generic functions; `contrib/closer-mop` defines Closer to MOP's
packages (`CLOSER-MOP`/`C2MOP`, `C2CL`) on top of it. Classes of the
implementation's metaclasses are defined and accessed by fast internal code;
classes of user-defined metaclasses go through the protocol:
`validate-superclass`, `direct-slot-definition-class`, `compute-slots`,
`compute-effective-slot-definition`, `effective-slot-definition-class`,
`initialize-instance`/`reinitialize-instance` of the class metaobject (with
the metaclass's own slots and non-standard class and slot options), and
`slot-value-using-class`, `(setf slot-value-using-class)`,
`slot-boundp-using-class` and `slot-makunbound-using-class` for their
instances. Slot definitions are instances of `standard-direct-slot-definition`
and `standard-effective-slot-definition` (or subclasses). Generic functions of
user-defined generic function classes call
`compute-applicable-methods-using-classes` and `compute-applicable-methods`.
Classes with forward-referenced superclasses are not finalized. Funcallable
instances (`funcallable-standard-class`, `set-funcallable-instance-function`)
are supported; `compute-discriminating-function` and `make-method-lambda` are
not called by the implementation.

## McCLIM

[McCLIM](https://mcclim.common-lisp.dev/) runs on Clysm with its CLX backend:
`(ql:quickload :mcclim)` builds it and its ~60 dependencies from Quicklisp, and
CLIM applications open windows on an X server (Unix domain or TCP sockets of
the Node.js host), draw with TrueType fonts, and read keyboard and pointer
input. Clysm has no threads, so McCLIM runs in its single-process mode.

It needs a Quicklisp installation (`$QUICKLISP_HOME`, by default `~/quicklisp/`;
see [Quicklisp](#quicklisp) for installing it from Clysm) and an X server.

```
$ DISPLAY=:0 bin/clysm script tests/mcclim/hello.lisp     # a small application
$ make test-mcclim        # McCLIM's test suite and the application on Xvfb
```

McCLIM's own test suite (`mcclim/test`, without the thread tests) passes
except STREAM-READ-GESTURE.SMOKE, which also fails on SBCL in the
single-process mode. Portability libraries without Clysm support are provided
in `contrib/` (closer-mop, trivial-features, trivial-garbage, static-vectors,
trivial-gray-streams) and found by ASDF before the Quicklisp releases; others
get small fixups when their system is defined (`contrib/fixups.lisp`: 3bz and
pngload use their non-FFI code, Drei builds without SWANK, CLX and McCLIM get
Clysm cases for sockets, environment variables and host names, applied as
string replacements to a copy of the source under
`~/.cache/common-lisp/clysm-patches/`).

## Gray streams

User-defined streams follow the Gray proposal (package `GRAY`, like SBCL's
`SB-GRAY`): subclasses of `gray:fundamental-character-input-stream` etc. with
methods on `gray:stream-read-char`, `gray:stream-write-char`, … work with every
CL stream function; `close`, `open-stream-p`, `input-stream-p`,
`output-stream-p` and `stream-element-type` are generic functions. The
trivial-gray-streams extensions (`stream-read-sequence`, `stream-write-sequence`,
`stream-file-position`) are built in, and `contrib/trivial-gray-streams` is a
trivial-gray-streams system that ASDF finds before Quicklisp's release.

## Running in the browser

`tools/build-web.sh` (`make web`) builds `build/web/` (an image containing the
runtime, the reader, the evaluator and the compiler, plus `host/web/index.html`).
Serve `build/` over HTTP and open `web/index.html` for a REPL running entirely
in WebAssembly: forms typed in the page are compiled to Wasm in the browser,
and the host calls the exported `eval_string`.

```
$ make web
$ python3 -m http.server -d build      # open http://localhost:8000/web/
```

The terminal is `host/web/repl.mjs`. Enter evaluates a complete input and starts a new line in an incomplete one
(the image's reader decides, through `clysmRepl.complete`), ↑/↓ recall the
history (kept in `localStorage`), and the prompt shows the current package.
Standard input (`read-line`, `read`) is asked for with a dialog; Cancel is end
of file. A stack overflow is reported as an error, and the dynamic bindings it
skipped are undone.

## ANSI test suite

The community ANSI conformance suite used by SBCL, ECL, CLISP and others
([ansi-test](https://gitlab.common-lisp.net/ansi-test/ansi-test)) is included
as a git submodule in `third_party/ansi-test`.

```
$ git submodule update --init third_party/ansi-test
$ make ansi                          # all categories
$ tests/ansi/run.sh cons numbers     # selected categories
```

`tests/ansi/driver.lisp` loads the suite's `rt` framework and one category into
the REPL image (every form is compiled to Wasm at run time) and runs the tests
one by one. A test that crashes the process (a Wasm trap, out of memory, or a
hang detected by a watchdog) is recorded as `CRASH`, and `tests/ansi/run.sh`
resumes with the next test. Per-category logs and `summary.txt` are written to
`build/ansi/`.

Categories run in parallel (`ANSI_JOBS`, default 8; `files` runs first because
the `streams` tests use the directory it creates), starting from a pristine
sandbox. The latest results are in `tests/ansi/RESULTS.txt`: 21939 of 21940
tests pass (99.99%), with no crashes. The five tests with the note
`:loop-iteration-values-in-finally` (marked `:ansi-spec-problem`) are
inactive, as on SBCL: like the MIT LOOP, an arithmetic `for` variable is
stepped past its limit before the loop ends, and libraries rely on it. The one failure, SUBTYPEP-COMPLEX.8,
assumes that `(complex single-float)` is not a subtype of
`(complex (float 0.0 1.0))`; Clysm reads `(complex X)` as a set of numbers.

## Status and limitations

Implemented: all special operators, macros and functions of the COMMON-LISP
package that SBCL defines (checked by listing every fbound/bound external CL
symbol), including CLOS, the condition system with restarts, the full `loop`,
`format`, the reader and printer, packages, hash tables, structures, bignums,
ratios and complex numbers, plus `eval`, `compile`, `load` and a REPL.
Extensions: the metaobject protocol, Gray streams, package-local nicknames
(the `:local-nicknames` option of `defpackage`, and
`sys:add-package-local-nickname` & co., also through UIOP).

Limitations:

* `short-float`/`long-float` are `single-float`/`double-float`.
* File I/O goes through the host; file streams hold the file's bytes in memory
  and write them back on `finish-output`/`close`.
* The pretty printer is an XP-style streaming printer (fill/linear/miser
  newlines, `pprint-indent`, `pprint-tab`, per-line prefixes, pprint dispatch
  tables) whose standard table reproduces SBCL's layout.
* `compile-file` performs the file compiler's toplevel processing (EVAL-WHEN,
  compile-time definers, `make-load-form` externalization) and writes the
  processed forms to a `.fasl` text file that `load` compiles form by form
  (whole programs are compiled ahead of time with `bin/clysm compile`/`run`).
* Instances of a redefined class are updated only while instances are tracked:
  after `(sys::track-instances)`, redefining a class with different slots
  gives its living instances (found through JavaScript `WeakRef`s) the new
  slots at once and calls `update-instance-for-redefined-class`. Tracking
  costs a JavaScript call per instance, so it is off by default; without it,
  instances made before the redefinition keep their old slot vectors.
* No threads and no first-class continuations (not required by the standard).
* Character names are SBCL's; names above Latin-1 are read on first use from
  `data/unicode-names.txt` (`tools/gen-unicode-names.sh`).

## Performance

`tools/bench.sh [FILE]` runs a benchmark file on SBCL and on Clysm (Node.js)
and prints the time ratio of each benchmark and their geometric mean. How fast
a program is relative to SBCL depends on what it uses; one figure does not
describe it. On an x86-64 machine with SBCL 2.6 and Node.js 24, compiling whole
programs ahead of time (`bin/clysm run`), Clysm takes this many times as long
as SBCL:

| File | Programs | Geometric mean | Range |
|---|---|---|---|
| `bench/suite.lisp` | 24 benchmarks the compiler was tuned on | 1.2 | 0.1 – 2.8 |
| `bench/gabriel.lisp` | 10 Gabriel-style benchmarks, also tuned on | 1.1 | 0.8 – 1.7 |
| `bench/holdout.lisp` | 17 programs written afterwards and not tuned on | 4.5 | 1.0 – 39 |

The first two files guided the optimization work, so their figures are the
best case, not what an arbitrary program gets. `bench/holdout.lisp` mixes
programs in ordinary style with ones that use features the first two files
do not.

* **About as fast as SBCL (1-2 times), sometimes faster**: fixnum and
  double-float arithmetic, function calls and closures (direct, through
  `funcall`, `labels`), lists, strings, simple vectors of type T, sorting,
  `equal` and `eql` hash tables, structures, special variables, catch/throw,
  `handler-case`/`handler-bind`/`restart-case` with conditions signalled a
  known call away (see [Non-local exits](#non-local-exits-spec-wasm-eh)).
  The holdout programs made of these (an n-body simulation, a small Lisp
  interpreter, word counting, Dijkstra with a heap in an adjustable vector, a
  symbolic simplifier) take 1.2-2.0 times as long as with SBCL.
* **2-3 times**: generic function dispatch and `make-instance`, two-dimensional
  arrays of type T, the reader and printer, `format` to streams, hash table
  allocation, bignums, and integers between 2^29 and 2^62 (fixnums in SBCL,
  boxed here).
* **5-40 times**: what has not been optimized yet —
  specialized arrays (bit vectors 39, `double-float` two-dimensional arrays
  26, `(unsigned-byte 8)` and `fixnum` vectors 5-14); single floats and
  complex numbers, which are always boxed (8-30); `call-next-method` with
  arguments, which recomputes the applicable methods to check them (40; 5
  without arguments), and `:before`/`:around` methods (4-6); sequence
  functions outside the fast paths, such as `remove-duplicates`, which is
  quadratic; `format` directives other than `~A ~S ~D ~%`, such as `~F` and
  `~{~}` (5-10); a non-local exit that has to be a Wasm exception: from a
  closure called by a library function, or to a catcher several ordinary
  calls up (10 times SBCL's cost per exit, and more with the depth).

Code loaded at the REPL or by `load` (and so libraries loaded with ASDF or
Quicklisp) is compiled form by form, without the whole-program knowledge of
`bin/clysm run`: `bench/suite.lisp` run with `bin/clysm script` takes about
twice as long as with `bin/clysm run` (geometric mean; up to 12 times for
two-dimensional arrays, catch/throw, structures and `make-instance`).

What makes the generated code fast:

* **type inference** (`src/types.lisp`): before a function is compiled its
  body is interpreted abstractly. Every lexical variable has a type at every
  point (a set of kinds — fixnum/integer with a range, double-float split by
  sign, cons, string, structure class… — that assignments update and tests
  such as `(< i n)`, `(consp x)` or `(minusp x)` refine in each branch); loops
  are iterated to a fixed point with widening, then narrowed. The types of
  known functions' results are those of their bodies;
* **unboxed representations** (`src/typed.lisp`): a variable whose values are
  all doubles lives in an f64 local, one within 32 or 64 bits in an i32 or i64
  local; arithmetic and comparisons on such values are single Wasm
  instructions (float results that are infinite, NaN or an underflow are
  redone by the generic function, which signals as before). Typed vector,
  string, limb and structure accesses are inline without checks once the
  object's type is known;
* **specialized copies**: a direct call of a known function with fixnum or
  double arguments calls a copy compiled for those argument types;
* **inlining** (`src/inline.lisp`): calls of small known functions, of small
  local functions and of lambdas bound to variables are replaced by copies of
  their bodies before type inference;
* fixnum arithmetic and comparisons inline on untyped values too, with i64
  arithmetic for integers up to 64 bits; bignums of base 2^28 digits with
  multiplication, division (Knuth D) and shifts as Wasm kernels;
* `tagbody` loops compile to Wasm `loop`s and blocks (a `br_table` dispatch
  only for jumps back from a later segment); checked casts test inline and
  call the error function only on failure;
* functions of required and `&optional` parameters, and of required and
  `&rest` parameters, take them as Wasm parameters (callers build the rest
  list); `&key` functions get their parameter slots as Wasm parameters too;
* generic functions of one or two required parameters dispatch on the classes
  of the arguments without an argument list and call a method function of the
  arguments directly when the method does not use `call-next-method`; slot
  readers and writers made by `defclass` access the slot at its location;
  `make-instance` with a constant class and initarg keys uses a per-call-site
  constructor;
* `format` with a constant control string compiles to Lisp code (and so does
  `formatter`): every directive with its parameters, `~{...~}` as a loop,
  `~[...~]` as a `case`, `~^` as a local exit; only logical blocks
  (`~<...~:>`) and strings with errors are interpreted, and so are control
  strings that are not constants: they are parsed once and
  cached; interpreter and compiled code call the same directive functions.
  Integers are printed from a digit buffer; the shortest digits of single
  floats, and of doubles that need at most 15, are computed with exact
  double arithmetic (others by the host); a string output stream is a plain
  buffer with a fill index, and `format nil`, `princ-to-string` and
  `prin1-to-string` reuse one;
* exits through `unwind-protect` within a function branch to the cleanup
  instead of throwing; a `throw` to a `catch` of the same function is a
  branch; `(catch 'tag (f ...))` calls a copy of the known function F in
  which `(throw 'tag ...)` is a return; non-local exits of local functions,
  of functions called under a catcher, of the signalling functions and of
  `handler-bind` handlers are returns with the exit pending
  (see [Non-local exits](#non-local-exits-spec-wasm-eh)).

`tools/dump-func.lisp FILE NAME...` prints the Wasm instructions generated for
functions (by name), which helps to see what a form compiles to.

## Tests

`make test-ui` tests the GUI framework on its S-expression backend
(`tests/ui/ui.lisp`, and `tests/ui/pane.lisp` for the panes, presentations
and dialogs), compiled ahead of time and in the REPL image.

`tools/run-tests.sh` compiles every `tests/cases/*.lisp`, runs it, and compares
standard output with SBCL running the same file. `INTERP=1` runs the files with
the in-image evaluator instead, and `SELFHOST=1` compiles them with the
Wasm-hosted compiler.
