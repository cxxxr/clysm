# Clysm — the implementation in detail

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

`js:get` of `null` or `undefined` (`NIL`) is `NIL`. A JavaScript exception in
`js:call`, `js:funcall` or `js:new` is a Lisp error of type `js:js-error`
(`js:js-error-value`, `js:js-error-name`, `js:js-error-message`).

A Lisp function that JavaScript calls (one converted by `js:to-js`, an
export, a listener) throws a JavaScript `Error` when it signals an error that
it does not handle itself, unless a debugger hook is set: the error's `name`
is `LispError`, `lispType` the type of the condition, `lispCondition` the
condition, and its `stack` begins with the Lisp functions active where the
condition was signalled (the frames of the Wasm name section, without the
numbers that tell apart functions of the same name). If Lisp code further
out called that JavaScript through `js:call`, it gets the original condition
back, so `handler-case` around it sees the condition itself.

`(js:await promise)` waits until a promise is settled and returns its value
(a rejection is a Lisp error), suspending the WebAssembly stack with
JavaScript Promise Integration while JavaScript goes on. It can be called in
a function made by `(js:async-function function)` — a JavaScript function
that returns a promise — and in `(js:async function args...)`, which starts
such a function at once and returns its promise; `(js:can-await-p)` tells.

```lisp
(js:call button "addEventListener" "click"
         (js:async-function
          (lambda (event)
            (let ((response (js:await (js:call (js:global) "fetch" "/data.json"))))
              (show (js:await (js:call response "text")))))))

;; three requests at the same time
(js:await-all (list (js:async #'fetch-summary a)
                    (js:async #'fetch-summary b)
                    (js:async #'fetch-summary c)))
```

Each function of `js:async-function` runs as a *computation* of its own,
like a thread: any number of them can wait at the same time, and Lisp code
(listeners, callbacks of a stream, other computations) runs while they wait.
The dynamic bindings are shallow (the symbol holds the current value, the
binding stack the saved ones), so each computation has a binding stack of
its own (`%js-async-callback` in `runtime/js.lisp`, installed through the
exported globals `bstack` and `bsp`). When it waits, its bindings are taken
out of the symbols: walking its stack from the top, each symbol gets back the
value from outside the computation and the entry keeps the computation's
value (`%swap-bindings`); when it goes on, the same exchange from the bottom
puts them back on top of whatever values the symbols have then. So code that
runs meanwhile sees the global values (or the bindings of its own caller),
and a computation sees the bindings of the code that started it until it
first waits. The catch tags, the handlers and the restarts are special
variables, so they belong to the computation too. `js:async` and
`js:await-all` pass Lisp values through unchanged.

An exception that Wasm code cannot catch — the stack exhausted (a
`RangeError`), a trap — leaves Lisp without running its `unwind-protect`
cleanups. The host's wrappers then call the exported `lisp_recover`, which
undoes the bindings that the call made (or, for a computation, all of its own
and installs the binding stack outside it), so the instance can go on.

### Libraries for JavaScript

A program can be a library that JavaScript loads once and calls many times:

```lisp
;; outline.lisp
(defun outline (source) ...)
(js:export "outline" #'outline :type "(source: string) => string[]")
(js:export "summarize" #'summarize :async t)        ; returns a promise, may js:await
```

```
$ bin/clysm library outline.lisp -o dist/outline.wasm
; wrote dist/outline.{wasm,mjs,d.ts}: 2 exports
```

```js
import { outline, summarize } from './dist/outline.mjs';
outline(text);                       // a Lisp function, in this thread
await Promise.all(files.map(summarize));
```

`bin/clysm library` compiles the program, loads it once to read its exports,
and writes `NAME.mjs` (an ES module whose named exports are the Lisp
functions; `create(options)` makes another instance), `NAME.d.ts` (the types
given with `:type`) and the host files in `clysm/`. `host/lib.mjs` is the
loader: `load(path, options)` instantiates the module in the calling thread
(Node.js, Electron's main process), runs its toplevel forms, and returns
`{ instance, exports, eval }`; the instance lives on between calls. Options
are `args`, `stdout`/`stderr` (functions of a string), `stdin` (a function
returning the next line), `modules`, `import`, `require` and `fs`.
`loadSync` does the same synchronously, for CommonJS.

The thread of the caller has the stack of that thread (about 1 MB in
Node.js, some 15000 Lisp frames; `node --stack-size=N` raises it within the
thread's real stack, e.g. `--stack-size=7000` gives about 110000 frames on
Linux). A deeper computation can run in a worker with the stack it asks for:
`loadInWorker(path, { stackSizeMb })` of `host/worker.mjs` returns exports
that take and return cloneable values and return promises. A stack overflow
is a `RangeError` for the caller; the instance stays usable.

JavaScript modules: `(js:require "typescript")` loads a module with Node.js's
`require()` (an ES module too, when it does not wait at its toplevel),
resolved from the working directory; `(js:import "@ruby/prism")` with
`import()`, waiting with `js:await`; `(js:module "name")` is the module given
to the loader as `{ modules: { name: ... } }`.

`tests/lib/run.sh` builds `tests/lib/demo.lisp` as a library and calls it
from JavaScript.

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

The complete manual — a user guide (in Japanese) and a reference of every
exported symbol — is [docs/ui/README.md](ui/README.md).
`tools/new-ui-app.sh DIRECTORY` makes a new application anywhere.

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
  markup; also `password-pane`, `editor-pane`, `radio-button`,
  `push-button-panel`, `text-input-choice` (a field with texts to choose
  from), `color-pane`, `image-pane` and `browser-pane`. A `list-panel` with
  `:filter` has a field that narrows it. Callbacks get what `:callback-type` says, by default the data and
  the interface.
* **The layouts**: `column-layout`, `row-layout` (with `:ratios`, and
  `:divider` in the description for a border the user drags), `grid-layout`,
  `tab-layout`, `switchable-layout`, `simple-layout` and `pinboard-layout`
  (panes at given positions). A description names panes of the
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
    ui/graphics.lisp ui/stream.lisp ui/dom.lisp app.lisp -o app.wasm
```

### Menus

An interface has menus of its own, besides those of the [commands](#commands):

```lisp
(ui:define-interface editor ()
  ((wrap :initform nil :accessor editor-wrap))
  (:panes (text ui:editor-pane :pane-menu 'edit-menu))     ; the menu of a right click
  (:menus
   (file-menu "&File" (("&Save" :key "CmdOrCtrl+S" :enabled (ui:computed (dirty-p ui:interface))
                                :callback (lambda (data interface) (save interface)))
                       export-menu                          ; a menu in the menu
                       :separator
                       quit))                               ; a command
   (export-menu "&Export" (("as &Text" :data :text :callback 'export) ("as &HTML" :data :html :callback 'export)))
   (edit-menu "&Edit" (("&Wrap lines" :checked (ui:binding (editor-wrap ui:interface))))))
  (:menu-bar file-menu edit-menu :commands))               ; then the menus of the commands
```

An entry has a title (the character after `&` is its mnemonic: Alt and the
mnemonic of a menu opens it, the mnemonic of an entry chooses it), a
`:callback`, a `:key`, `:enabled`, and with `:checked` a check mark that is
a slot — all of which may be `computed` or `binding`. In the desktop
application the same menus, with their check marks and menus in menus, are
in the menu bar of the application. `:pane-menu` of a pane is a menu, the
name of one or a function that returns the items for `ui:popup-menu`.

### Windows

`(ui:display interface :window t)` shows an interface in a window above the
application: the user moves it by its title, resizes it at its corner,
maximizes it (a double click on the title) and closes it; a click brings it
to the top. `ui:raise-interface`, `ui:tile-windows` and `ui:cascade-windows`
arrange the windows, `(ui:interface-window interface)` is the window with
its `window-x`, `-y`, `-width`, `-height`, and the `:destroy-callback` of an
interface is called when it is closed. A `ui:docking-layout` shows panes
with a title bar each, `(title pane)`, that the user takes out into windows
of their own and back.

### Stream panes and the interactor

A `ui:stream-pane` (`ui/stream.lisp`) is a character output stream, as the
panes of CLIM are: what is written to it is what it shows.

```lisp
(format pane "~D contacts, the first is " (length *contacts*))
(ui:present (first *contacts*) 'contact :stream pane)       ; a presentation in the text
(ui:with-text-style (pane :bold t) (princ "Total" pane))
(ui:formatting-table (pane)
  (dolist (contact *contacts*)
    (ui:formatting-row (pane)
      (ui:formatting-cell (pane) (ui:present contact 'contact :stream pane))
      (ui:formatting-cell (pane :align :right) (princ (contact-age contact) pane)))))
```

`(ui:with-output-as-presentation (pane object type) ...)` makes what is
written a presentation, `ui:formatting-item-list` lays cells out in columns,
`ui:write-markup` writes markup and `ui:window-clear` forgets everything.
With a `:display-function`, the pane shows what that function writes and
calls it again when state it read has changed.

A `ui:interactor-pane` also has a line for commands, typed by their names
with their arguments (`Rename contact Ada "Ada Lovelace"`): Tab completes the
name of a command and what an argument can be, the arrows recall earlier
lines, and what the command prints is in the pane (`*standard-output*` is
the interactor while a command runs). An argument that is missing is asked
for in that line — whoever invoked the command, a menu too — and the user
types it or clicks a presentation, also one in the output of the interactor.
For the objects of an application to be typed, it defines how they are read:

```lisp
(defmethod ui:accept-from-string ((name (eql 'contact)) type string)
  (let ((contact (find string *contacts* :key #'contact-name :test #'string-equal)))
    (values contact (and contact t))))
(defmethod ui:presentation-type-choices ((name (eql 'contact)) type)   ; for completion
  (mapcar #'contact-name *contacts*))
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

`ui:graph-pane` draws items and what its `:children-function` returns for
them as boxes joined by edges (`:direction :left-right` or `:top-down`),
selected and activated like the items of a list, and `ui:draw-image` draws an
image.

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

* `ui:define-drag-and-drop-translator` says what dragging a presentation of
  one type onto one of another does: `(contact group move-to-group)` moves a
  contact that is dropped on a group.
* An interface with `:pointer-documentation t` has a line that says what the
  pointer can do where it is (the command of a click, the menu, dragging).

`ui:command-palette` (Ctrl+Shift+P) is a dialog in which a command is chosen
by typing part of its name — every command of the application, which then
asks for its arguments.

### Dialogs

`ui/dialog.lisp`: `ui:display-message`, `ui:prompt-for-string`,
`ui:prompt-for-number`, `ui:prompt-for-confirmation` and
`ui:prompt-with-list` show a modal dialog, and `(ui:display-dialog interface)`
makes any interface one, answered by its callbacks with
`(ui:exit-dialog value)` or `(ui:abort-dialog)`. `ui:prompt-for-values` asks for several typed values in one form,
`ui:prompt-for-color` for a colour, `ui:popup-confirmer` shows any pane with
OK and Cancel, and `ui:prompt-for-file` lets the user choose a file and gives
its name and text (in the desktop application with the native dialog);
`ui:set-clipboard` and `ui:clipboard` write and read the clipboard (in a
browser that does not let a page read it, the user pastes it into a dialog). With
`:modal nil`, `display-dialog` shows a window above the application that can
be used meanwhile. A page cannot wait inside a function, so these return at
once and call their `:continuation` with the answer and whether there is one:

```lisp
(ui:prompt-for-confirmation "Delete it?"
                            :continuation (lambda (yes okp) (when yes (delete-it))))
```

### What of CAPI and CLIM is here

| CAPI | here |
|---|---|
| `define-interface`, `display`, `contain`, `quit-interface` | the same names; a window is an element of the page, or (`display-dialog … :modal nil`) a window above it |
| `title-pane`, `display-pane`, `text-input-pane`, `password-pane`, `multi-line-text-input-pane`, `editor-pane`, `text-input-choice` | the same names (`editor-pane` is plain text in a fixed-width font) |
| `push-button`, `check-button`, `radio-button`, `push-button-panel`, `radio-button-panel`, `check-button-panel` | the same names |
| `list-panel` (`:filter`), `multi-column-list-panel` (sorted by its headers), `tree-view`, `option-pane`, `graph-pane` | the same names |
| `slider`, `progress-bar`, `toolbar`, `browser-pane`, `output-pane` | the same names |
| `column-layout`, `row-layout` (`:ratios`, `:divider`, `:separator`), `grid-layout`, `tab-layout`, `switchable-layout`, `simple-layout`, `pinboard-layout` | the same names |
| collections and choices: `:items`, `:print-function`, `:interaction`, `:selection-callback`, `:action-callback`, `:callback-type`, `choice-selected-item(s)` | the same |
| `:menus` and `:menu-bar` of `define-interface`, menus in menus, check items, mnemonics, accelerators, `:pane-menu` | the same; also commands with `:menu` and `:key`, and `popup-menu` |
| several top-level windows, `docking-layout` | `display … :window t` (moved, resized, raised, maximized, tiled), `docking-layout` |
| `display-dialog`, `exit-dialog`, `abort-dialog`, `popup-confirmer`, `display-message`, `prompt-for-string` / `-number` / `-confirmation` / `-file` / `-color`, `prompt-with-list` | the same names, with `:continuation` (as CAPI has for window-modal dialogs) |
| graphics ports: `gp:draw-line`…, `with-graphics-state`, translation, rotation, scale, `draw-image` | `ui:draw-line`…, the same macros; shapes instead of pixels |
| `:input-model` of output panes | `:press-callback`, `:motion-callback`, `:release-callback` |
| `set-clipboard`, `clipboard` | the same names |
| geometry hints (`:visible-min-width`…) | `:width`, `:height`, `:min-`, `:max-` and `:stretch`, in CSS |

| CLIM | here |
|---|---|
| `define-application-frame`, panes, layouts | `define-interface` |
| `define-command` with typed arguments, command tables | `define-command`, `:table` |
| `present`, `with-output-as-presentation`, `define-presentation-type`, presentation methods | the same names; `present-object` is the `present` method |
| `accept`, input contexts, highlighting of acceptable presentations | `accept`; a click, or a dialog for strings, numbers and members |
| `accepting-values` | `prompt-for-values` |
| `define-presentation-to-command-translator`, the presentation menu, `define-drag-and-drop-translator` | the same names |
| stream panes: `format`, `present … :stream`, `with-output-as-presentation (stream …)`, `with-text-style`, `window-clear` | `stream-pane`, the same names |
| the interactor: commands typed by name, arguments typed (with completion) or clicked | `interactor-pane`, `accept-from-string`; also `command-palette` |
| the pointer documentation pane | `:pointer-documentation t` |
| application panes with a display function, incremental redisplay, `updating-output` | `application-pane`; every pane is redisplayed incrementally, from the state it read |
| `formatting-table`, `formatting-row`, `formatting-cell`, `formatting-item-list`, output records | the same names on stream panes; elsewhere markup (`(:table …)`) |
| `draw-line*`…, `with-drawing-options`, transformations | `draw-line`…, `with-graphics-state` |
| `format-graph-from-roots` | `graph-pane` |
| `menu-choose`, `notify-user` | `popup-menu` / `prompt-with-list`, `display-message` |
| the debugger with restarts | the dialog of restarts |

Not here, because the page or the browser does it: fonts, colours and cursors
(CSS), scrolling, printing, text editing commands, and processes (a page has
one thread; `js:await` is how a computation waits).

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
restarts. A browser asks, with `ui/dialog.lisp`, in a dialog of the page that
has a button for every restart: the failing computation — a listener, a
command, even rendering — waits there until the user has chosen, while the
page goes on, and then continues with the restart. This suspends the
WebAssembly stack with [`js:await`](#javascript-interoperability-spec-externref-ラッパー構造体);
a browser that cannot do that asks with `prompt`. (`ui:wait-for-answer` is
how a program of its own waits for the user in the middle of a function.)
An error that has no restart to choose is shown in a dialog of the page, and the S-expression backend calls
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

Time relative to SBCL 2.6 on Node.js 24 (x86-64), measured by `tools/bench.sh
FILE` (best of three runs per benchmark; whole programs compiled ahead of
time unless noted):

| File | Programs | Geometric mean | Slowest | Fastest |
|---|---|---|---|---|
| `bench/suite.lisp` | 24, used for tuning | 0.94 | `b-bignum` 1.91 | `b-mandel` 0.08 |
| `bench/gabriel.lisp` | 10 Gabriel benchmarks, used for tuning | 1.01 | `g-traverse` 1.54 | `g-ctak` 0.71 |
| `bench/holdout.lisp` | 17 ordinary programs written after the first two files | 1.06 | `h-clos-methods` 1.68 | `h-mandel-complex` 0.08 |
| `bench/libs.lisp` (`tools/bench-libs.sh`) | cl-ppcre, Alexandria (its test suite and utilities), split-sequence, Babel from Quicklisp; the self-hosted compiler compiling `bench/suite.lisp` | 1.79 | `l-babel` 2.92 | `l-alexandria-tests` 0.32 |
| `bench/programs.lisp` | 63 more programs | 1.20 | `p-vector-push` 3.29 | `p-remove-duplicates` 0.03 |

Code loaded form by form into the image (`clysm script`, the REPL, `load`)
against the same file compiled as a whole program (`MODE=script tools/bench.sh
bench/suite.lisp`): geometric mean 1.13, slowest `b-special` 2.72.

The slow areas that `bench/holdout.lisp` first showed (4.5 at the time) were
fixed as whole features, and each fix was measured on programs written for
it afterwards; those are kept in further files: `bench/clos.lisp` (geometric mean 2.0, slowest `c-make-instance-plain`
5.2), `bench/control.lisp` (1.6; `v-go-closure` 66), `bench/io.lisp` (1.8;
`i-file-roundtrip` 13), `bench/library.lisp` (1.1; `v-hash-iterate` 3.4).

What is not as fast as intended, and why:

* **Code loaded into the image, single items.** The geometric mean is within
  1.3, but `b-special` (2.7), `b-call` (1.9), `b-struct` (1.35) and `b-catch`
  (1.3) are not. A call of a function with `&key` or `&rest` parameters
  defined in another form goes through the function cell and a comparison of
  the layout; `b-special` runs at 1.6 times the whole-program speed once the
  form that defines it has returned but at 2.7 while that form is still
  running (it also runs the benchmark), which is not understood yet.
* **Libraries** are within 3 but close to it (`l-babel` 2.9, `l-ppcre-replace`
  2.9, `l-split-sequence` 2.8): they are compiled to fasls file by file, where
  calls between global functions are ordinary calls and small functions are
  not copied into their callers.
* **Exits from closures called by code the compiler does not see** (a
  `return-from` or `go` out of a lambda passed to `maphash`, `map` or a user
  function) are Wasm exceptions: about 90 ns plus 30 ns per frame, against
  about 7 ns on SBCL (`v-go-closure`, `t-return-maphash`: 50–70 times).
* **Generic functions**: dispatch on EQL specializers or on many classes at
  one call site (4–5 times), `make-instance` with a class that is not a
  constant (5), generic functions with `&optional`/`&key` (3).
* **Integers between 2^29 and 2^63** are boxed when they are stored in data
  structures or passed between functions (fixnums in SBCL); **bignums** have
  28-bit digits (2–2.7 times); adjustable vectors grow by allocation of new
  storage (`p-vector-push` 3.3); `equal` hash tables with string keys (2.4).
* **Files**: octets cross to the host one call each (`i-file-roundtrip` 13).

What makes the code fast:

* **Type inference** (`src/types.lisp`) per function, flow-sensitive, with
  integer ranges; loops are iterated to a fixed point and what the earlier
  passes recorded outside a loop is undone. **Unboxed representations**
  (`src/typed.lisp`): fixnums and integers within 64 bits, double and single
  floats, complex numbers of floats (two locals), references to specialized
  vectors, two-dimensional arrays with their storage and dimensions in locals.
* **Specialized arrays**: every upgraded element type (bit, the integer types
  of 7 to 64 bits, single and double floats, characters) has its own Wasm
  array type; `aref` on an array whose element type is known (from
  `make-array` or a declaration) is inline with a bounds check, and the
  generic function is only called to signal the error.
* **Versions of a function**: a function with a loop or a recursive call
  whose parameters are used as numbers tests its arguments on entry and
  continues in a copy compiled for fixnums (or doubles). Small functions are
  copied into their callers — in the image with a test that the function has
  not been redefined.
* **Calls**: direct calls between the functions of a program; keyword
  arguments passed in fixed slots (in the image too, checked against the
  layout of the function's current definition); structure accessors inline.
* **CLOS**: generic functions of one to four required arguments dispatch and
  combine methods without argument lists; `call-next-method` is a direct
  call; call sites cache the functions for the classes of their arguments
  and read slots of readers inline; `slot-value` with a constant name and
  `make-instance` with a constant class use caches at the call site.
* **Non-local exits** that stay within the functions the compiler sees
  (`handler-case`, `restart-case`, `catch`/`throw`, `return-from` out of
  local functions, `error` under a handler one call away) are returns with a
  pending exit, not Wasm exceptions; special bindings are inline.
* **Numbers**: fixnum, 64-bit, single and double arithmetic and comparisons
  in Wasm helpers; `floor` and friends, `isqrt`, `gcd`, `ldb`, the
  irrational functions as primitives; bignum kernels in Wasm.
* **Library**: sequence functions with one implementation per family and
  direct loops over lists, simple vectors and strings; hash tables for
  `remove-duplicates` and the set functions; `typep` with a constant type,
  `format` with a constant control string (all directives), `coerce` and
  `parse-integer` compiled at the call site; string streams as buffers.

`tools/prof.sh FILE` (and `tools/prof-script.sh` for a file loaded into the
image) profile a program with the V8 CPU profiler; `tools/dump-func.lisp FILE
NAME...` prints the Wasm generated for functions.

## Tests

`make test-ui` tests the GUI framework on its S-expression backend
(`tests/ui/ui.lisp`, and `tests/ui/pane.lisp` for the panes, presentations
and dialogs), compiled ahead of time and in the REPL image.

`tools/run-tests.sh` compiles every `tests/cases/*.lisp`, runs it, and compares
standard output with SBCL running the same file. `INTERP=1` runs the files with
the in-image evaluator instead, and `SELFHOST=1` compiles them with the
Wasm-hosted compiler.
