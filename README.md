# Clysm

Clysm is a Common Lisp implementation that compiles Lisp to WebAssembly GC.
Programs run on Node.js, in a browser, or in Electron. The compiler can also
run in WebAssembly, so the REPL compiles each form to Wasm at run time, in a
browser page too.

```
$ bin/clysm eval '(expt 2 100)'
1267650600228229401496703205376
```

Demo: [Lem running in the browser](https://cxxxr.github.io/clysm/). Lem, its
libraries and the compiler are all compiled by Clysm; the page needs no
server. It needs Chrome or Edge 137 or later and is about 15 MB.

## State

* 21939 of the 21940 tests of the
  [ANSI test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test) pass.
* CLOS with the metaobject protocol, the condition system, `loop`, `format`
  and the pretty printer are implemented, as are Gray streams and
  package-local nicknames.
* ASDF and Quicklisp work. Alexandria, cl-ppcre, Babel and str load without
  changes. McCLIM runs with its CLX backend.
  [Lem](https://github.com/lem-project/lem) runs as a desktop application and
  in a browser page (`contrib/lem/`).
* The compiler compiles itself; the second and third generations are
  identical.

## Usage

Requires SBCL (the cross compiler runs on it) and Node.js 22 or later.

```
$ bin/clysm repl                          # the REPL image is built on first use
$ bin/clysm run tests/hello.lisp          # compile with the runtime and run on Node.js
$ bin/clysm compile prog.lisp -o prog.wasm
$ bin/clysm script file.lisp              # LOAD a file in the REPL image
$ make web                                # the REPL in a browser page, in build/web/
```

Libraries are loaded from an existing Quicklisp installation:

```lisp
(require :quicklisp)
(ql:quickload :cl-ppcre)
(ppcre:split "\\s+" "compiled to WebAssembly")   ; => ("compiled" "to" "WebAssembly")
```

JavaScript is called through the `JS` package. `js:await` waits for a
promise, using JavaScript Promise Integration; any number of computations
can wait at the same time, each with its own dynamic bindings:

```lisp
(js:call (js:global "Math") "max" 3 9 4)                              ; => 9
(js:funcall (js:eval "(f, x) => f(x) * 2") (lambda (x) (+ x 1)) 20)   ; => 42
(js:await (js:call (js:global) "fetch" "/data.json"))                 ; in a js:async-function
(js:await-all (list (js:async #'fetch-page 1) (js:async #'fetch-page 2)))
(js:call (js:require "node:path") "join" "a" "b")                     ; => "a/b"
```

A program can also be a library for JavaScript: functions exported with
`js:export` become the named exports of an ES module, with a `.d.ts`.

```
$ bin/clysm library outline.lisp -o dist/outline.wasm     # dist/outline.{wasm,mjs,d.ts}
```

```js
import { outline } from './dist/outline.mjs';
outline(source);              // a Lisp error is thrown as an Error named LispError
```

## Implementation

Lisp objects are WasmGC structs and arrays, and fixnums and characters are
`i31ref`s. Memory is managed by the host's GC. Type tests are `ref.test`.
Non-local exits are branches or returns where possible and Wasm exceptions
(`try_table`, `throw_ref`) otherwise. Tail calls are `return_call`. Code
compiled in the running image (`compile`, `load`, the REPL) becomes a new
module linked with the image. The Wasm binary is written by Clysm's own
encoder (`src/wasm.lisp`).

The design follows `Wasm向けLispコンパイラ開発仕様.pdf`;
[docs/DESIGN.ja.md](docs/DESIGN.ja.md) relates its sections to the code, and
[docs/INTERNALS.md](docs/INTERNALS.md) describes the implementation in detail.

## GUI

`ui/` is a GUI framework modelled on CAPI and CLIM: interfaces, panes and
layouts, commands, presentations, menus, dialogs and drawing. The screen is
updated when the slots of model objects are set.

```lisp
(ui:define-interface greeter ()
  ((count :initform 0 :accessor greeter-count))
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
   (top ui:row-layout '(name-input greet))))

(ui:display (make-instance 'greeter))
```

`examples/todo/` and `examples/contacts/` build for the browser (`build.sh`)
and for Electron (`build-desktop.sh`). With `--dev` the application has a
REPL in its window. The manual is [docs/ui/README.md](docs/ui/README.md).

## Lem

`contrib/lem/` loads Lem's core with ASDF and gives it a frontend written
with the GUI framework. Lem's editor thread becomes a computation that waits
for events with `js:await`. Lisp buffers are evaluated in the editor's own
image (`C-M-x`, `M-x lisp-repl`).

```
$ contrib/lem/build.sh                    # desktop application in build/lem-desktop/
$ (cd build/lem-desktop && npm start)
$ contrib/lem/build-web.sh                # build/lem-web/lem.html, one file for a browser
```

See [contrib/lem/README.md](contrib/lem/README.md).

## Performance

Time relative to SBCL 2.6 on Node.js 24 (x86-64), whole programs compiled
ahead of time (`tools/bench.sh FILE`, best of three runs):

| File | Programs | Geometric mean | Slowest | Fastest |
|---|---|---|---|---|
| `bench/suite.lisp` | 24, used for tuning | 0.94 | 1.91 | 0.08 |
| `bench/gabriel.lisp` | 10 Gabriel benchmarks | 1.01 | 1.54 | 0.71 |
| `bench/holdout.lisp` | 17 programs written after the tuning | 1.06 | 1.68 | 0.08 |
| `bench/programs.lisp` | 63 more programs | 1.20 | 3.29 | 0.03 |
| `bench/libs.lisp` | libraries from Quicklisp | 1.79 | 2.92 | 0.32 |

Code loaded form by form (the REPL, `load`, ASDF) is slower than a whole
program. The slow cases and their causes are in
[docs/INTERNALS.md](docs/INTERNALS.md#performance).

## Limitations

* No threads. McCLIM runs in its single-process mode.
* `short-float` and `long-float` are `single-float` and `double-float`.
* Instances of a redefined class are updated only after
  `(sys::track-instances)`.
* `js:await` needs JavaScript Promise Integration (Node.js 24; Chrome and
  Edge 137 or later).

## Source

| Path | Contents |
|---|---|
| `src/` | the compiler |
| `runtime/` | the Common Lisp library, compiled by Clysm |
| `ui/` | the GUI framework |
| `host/` | the Node.js and browser hosts |
| `contrib/` | portability libraries, patches for Quicklisp systems, Lem |
| `examples/` | applications written with the GUI framework |
| `tests/`, `bench/` | tests and benchmarks |
| `docs/` | the implementation, the design, the GUI manual |

## Tests

```
$ make test          # tests/cases/*.lisp, compared with SBCL's output
$ make test-all      # also interpreted, compiled at run time, and self-hosted
$ make fixpoint      # self-compilation reaches a fixed point
$ make ansi          # the ANSI test suite (third_party/ansi-test submodule)
$ make test-ui
$ make test-lem      # Lem in Electron and in a browser page, without a display
```
