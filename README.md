# Clysm

[![Tests](https://github.com/cxxxr/clysm/actions/workflows/tests.yml/badge.svg?branch=main)](https://github.com/cxxxr/clysm/actions/workflows/tests.yml)
[![ANSI tests](https://github.com/cxxxr/clysm/actions/workflows/ansi.yml/badge.svg?branch=main)](https://github.com/cxxxr/clysm/actions/workflows/ansi.yml)

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
server. It needs Chrome or Edge 137 or later and is about 22 MB.

## State

* 21939 of the 21940 tests of the
  [ANSI test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test) pass;
  GitHub Actions runs the suite on every push and pull request.
* CLOS with the metaobject protocol, the condition system, `loop`, `format`
  and the pretty printer are implemented, as are Gray streams and
  package-local nicknames.
* ASDF and Quicklisp work. Alexandria, cl-ppcre, Babel and str load without
  changes. McCLIM runs with its CLX backend.
  [Lem](https://github.com/lem-project/lem) runs as a desktop application and
  in a browser page (`contrib/lem/`).
* Threads (green threads) with Bordeaux Threads, sockets and subprocesses
  work on Node.js. SLIME connects to a SWANK server.
* The compiler compiles itself; the second and third generations are
  identical.

## Usage

Requires SBCL (the cross compiler runs on it) and Node.js 22 or later;
`js:await`, threads and the npm package need Node.js 24.

```
$ bin/clysm repl                          # the REPL image is built on first use
$ bin/clysm run tests/hello.lisp          # compile with the runtime and run on Node.js
$ bin/clysm compile prog.lisp -o prog.wasm
$ bin/clysm exec prog.wasm                # run a compiled module
$ bin/clysm script file.lisp              # LOAD a file in the REPL image
$ bin/clysm eval '(expt 2 100)'           # evaluate a form in the REPL image
$ bin/clysm test --system my-tests t/*.lisp   # test programs, in parallel (--watch)
$ make web                                # the REPL in a browser page, in build/web/
$ make npm                                # an npm package that needs only Node.js (no SBCL)
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

The module loads in the caller's thread (Node.js, Electron's main process)
or, for deep recursion, in a worker (`host/worker.mjs`). Stack traces name
Lisp functions by file and line. `--shake` leaves out the functions the
program does not refer to. Subprocesses are run with `uiop:run-program` and
`uiop:launch-program`.

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

`ui/editor/` is a code editor written in Lisp, a pane of the framework:
highlighting, Lisp indentation, undo, the selection, input methods and Emacs
keys, without a JavaScript editor (`examples/editor/`,
[ui/editor/README.md](ui/editor/README.md)).

`examples/ide/` is an IDE for Common Lisp made with them, in a browser and
in Electron: files and tabs, evaluation compiled to WebAssembly in a
world (the IDE's image in a Web Worker, which can be killed and started
again), a REPL, a debugger with restarts, interrupts of endless
computations, an inspector, completion, argument lists and going to
definitions ([examples/ide/README.md](examples/ide/README.md)).

## Lem

`contrib/lem/` loads Lem's core with ASDF and gives it a frontend written
with the GUI framework. Lem's editor thread becomes a computation that waits
for events with `js:await`. Lisp buffers are evaluated in the editor's own
image (`C-M-x`, `M-x lisp-repl`), or, after `M-x world-start`, in a world:
another image in a worker ([contrib/world/](contrib/world/README.md)), which
can loop or be killed without the editor.

```
$ contrib/lem/build.sh                    # desktop application in build/lem-desktop/
$ (cd build/lem-desktop && npm start)
$ contrib/lem/build-web.sh                # build/lem-web/lem.html, one file for a browser
```

See [contrib/lem/README.md](contrib/lem/README.md).

## SLIME

`clysm swank` starts a SWANK server (SLIME's, from an existing Quicklisp
installation) that Emacs connects to with `M-x slime-connect` (port 4005,
or `--port N`). The REPL, `C-c C-c`, `C-c C-k`, `M-.`, argument lists,
completion, the inspector and the debugger work; code is compiled to
WebAssembly like at Clysm's REPL. To start Clysm from Emacs with `M-x slime`:

```elisp
(setq slime-lisp-implementations
      '((clysm ("/path/to/clysm/bin/clysm" "repl")
         :init (lambda (port-file _)
                 (format "(progn (require :quicklisp) (funcall (read-from-string \"ql:quickload\") :swank :silent t) (funcall (read-from-string \"swank:start-server\") %S))\n"
                         port-file)))))
```

On Node.js 24 the server has threads of its own (green threads, below):
Emacs gets answers while an evaluation runs. Elsewhere one thread serves
the connection.

Code compiled after Emacs connected is compiled for debugging: `C-c C-c`
interrupts it (and `sleep`), and the debugger shows the arguments and local
variables of its frames (`e` evaluates in a frame). This makes it much
slower (more than ten times for a small recursive function like fib), and calls in
tail position use stack. A function declared `(optimize (debug 0))` (or
`(debug 1)`, or `(speed 3)`) is compiled without the frame's variables;
`(setq sys::*record-frames* nil)` and `(setq sys::*interrupt-checks* nil)`
turn both off for the code compiled afterwards.

## Performance

Time relative to SBCL 2.6 on Node.js 24 (x86-64), whole programs compiled
ahead of time (`tools/bench.sh FILE`, best of three runs):

| File | Programs | Geometric mean | Slowest | Fastest |
|---|---|---|---|---|
| `bench/suite.lisp` | 24, used for tuning | 0.94 | 1.91 | 0.08 |
| `bench/gabriel.lisp` | 10 Gabriel benchmarks | 1.01 | 1.54 | 0.71 |
| `bench/holdout.lisp` | 17 programs written after the tuning | 1.06 | 1.68 | 0.08 |
| `bench/programs.lisp` | 63 more programs | 1.19 | 3.15 | 0.03 |
| `bench/libs.lisp` | libraries from Quicklisp | 1.79 | 2.92 | 0.32 |

Code loaded form by form (the REPL, `load`, ASDF) is slower than a whole
program. The slow cases and their causes are in
[docs/INTERNALS.md](docs/INTERNALS.md#performance).

## Limitations

* Threads are green threads: they take turns in one thread of the host
  (no parallelism). They switch when one waits (for a lock, a condition
  variable, another thread, `sleep`, input from a socket or the terminal),
  and code compiled with interrupt checks (`(setq sys::*interrupt-checks*
  t)`, on in SLIME sessions) is preempted every 10 ms; other code runs
  until it waits. They need JavaScript Promise Integration. McCLIM runs in
  its single-process mode.
* `short-float` and `long-float` are `single-float` and `double-float`.
* Instances of a redefined class are updated for sure only after
  `(sys::track-instances)`; otherwise when their slots are next accessed,
  if the number of slots changed.
* `js:await` needs JavaScript Promise Integration (Node.js 24; Chrome and
  Edge 137 or later).

## Source

| Path | Contents |
|---|---|
| `bin/` | the `clysm` command (`clysm.mjs`: the one of the npm package) |
| `src/` | the compiler |
| `runtime/` | the Common Lisp library, compiled by Clysm |
| `lib/` | the toplevels of the REPL images (Node.js, the browser) |
| `ui/` | the GUI framework |
| `host/` | the Node.js and browser hosts |
| `contrib/` | portability libraries, patches for Quicklisp systems, Lem, worlds |
| `examples/` | applications written with the GUI framework |
| `tests/`, `bench/` | tests and benchmarks |
| `tools/` | scripts that build, test, measure and profile |
| `third_party/` | ASDF, the ANSI test suite (a submodule) |
| `data/` | the names of the Unicode characters |
| `docs/` | the implementation, the design, the GUI manual |

## Tests

```
$ make test          # tests/cases/*.lisp, compared with SBCL's output
$ make test-all      # also interpreted, compiled at run time, and self-hosted
$ make fixpoint      # self-compilation reaches a fixed point
$ make ansi          # the ANSI test suite (third_party/ansi-test submodule);
                     # tests/ansi/check.sh judges the results, as GitHub Actions does
$ make test-asdf     # a small system loaded with the bundled ASDF
$ make test-lib      # a library compiled for JavaScript, called from JavaScript
$ make test-system   # bin/clysm test with an ASDF system and its dependencies
$ make test-threads  # green threads and Bordeaux Threads
$ make test-swank    # the SWANK server and a client
$ make test-mcclim   # McCLIM on a virtual X server
$ make test-quicklisp   # installs Quicklisp from scratch (downloads) and loads libraries
$ make test-downstream  # the programs of tests/downstream/projects.txt, with this Clysm
$ make test-ui       # the GUI framework and the examples of its guide
$ make test-lem      # Lem in Electron and in a browser page, without a display
```

`test-system`, `test-threads`, `test-swank` and `test-mcclim` take their
libraries from an existing Quicklisp installation.
