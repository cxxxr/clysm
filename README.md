# Clysm

Clysm is a Common Lisp implementation, written from scratch, that compiles
Lisp to **WebAssembly GC**. The same programs run on Node.js, in a browser
page and in Electron, and the compiler itself runs in WebAssembly: the REPL
compiles every form you type to a new Wasm module, even inside a web page.

**[Try it: Lem, the Common Lisp editor, running in your browser](https://cxxxr.github.io/clysm/)**
— no server is involved; Lem, its libraries and the compiler are all
WebAssembly made by Clysm (Chrome or Edge 137 or later, about 15 MB).

```
$ bin/clysm eval '(expt 2 100)'
1267650600228229401496703205376
```

## Why it may interest you

* **Lisp objects are WebAssembly GC values.** Conses, symbols, strings,
  vectors and instances are WasmGC structs and arrays; fixnums and characters
  are `i31ref`s. There is no linear memory and no collector of Clysm's own —
  the JavaScript engine's GC manages everything.
* **It is a complete Common Lisp.** 21939 of the 21940 tests of the
  [ANSI test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test) pass.
  CLOS with the metaobject protocol, the condition system with restarts,
  `loop`, `format` and the pretty printer, Gray streams, package-local
  nicknames.
* **Real software runs on it.** ASDF and Quicklisp work; libraries such as
  Alexandria, cl-ppcre, Babel and str load unchanged. McCLIM opens windows
  on an X server. [Lem](https://github.com/lem-project/lem) runs as a desktop
  application and as the single web page linked above.
* **It is about as fast as SBCL on many programs.** Benchmarks compiled ahead
  of time take 0.9 to 1.2 times as long as with SBCL 2.6 (geometric means),
  thanks to type inference, unboxed numbers and arrays, and non-local exits
  that rarely need Wasm exceptions. Where it is slower, and why, is written
  down in [docs/INTERNALS.md](docs/INTERNALS.md#performance).
* **It compiles itself.** The compiler, running in WebAssembly, compiles its
  own source; the second and third generations are byte-for-byte identical.
  In the running image `compile`, `load` and the REPL produce native Wasm
  that is linked with the image.
* **It talks to JavaScript, and waits for it.** The `JS` package calls
  JavaScript, and `js:await` suspends Lisp code until a promise settles
  (JavaScript Promise Integration), so a Lisp program in a page can wait for
  the user or the network in the middle of a function.
* **It has a GUI framework.** `ui/`, modelled on CAPI and CLIM: interfaces,
  panes, commands, presentations, dialogs and drawing, in a browser or a
  desktop application.

## Getting started

You need SBCL (the cross compiler runs on it) and Node.js 22 or later.

```
$ git clone https://github.com/cxxxr/clysm && cd clysm
$ bin/clysm repl                          # a REPL; the image is built on first use
$ bin/clysm run tests/hello.lisp          # compile a program with the runtime and run it on Node.js
$ bin/clysm compile prog.lisp -o prog.wasm
$ bin/clysm script file.lisp              # LOAD a file into the REPL image
$ make web                                # the REPL in a browser page, in build/web/
```

In the REPL, libraries come from Quicklisp as usual:

```lisp
(require :quicklisp)                      ; an existing installation in ~/quicklisp/
(ql:quickload :cl-ppcre)
(ppcre:split "\\s+" "compiled to WebAssembly")
```

## A taste

JavaScript from Lisp:

```lisp
(js:call (js:global "Math") "max" 3 9 4)                              ; => 9
(js:funcall (js:eval "(f, x) => f(x) * 2") (lambda (x) (+ x 1)) 20)   ; => 42

;; in a function of js:async-function, called from the event loop:
(let ((response (js:await (js:call (js:global) "fetch" "/data.json"))))
  (js:await (js:call response "text")))
```

A window of the GUI framework — the display follows the state:

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

`examples/todo/` and `examples/contacts/` are applications for the browser
(`build.sh`) and for Electron (`build-desktop.sh`); with `--dev` they have a
REPL in their window that redefines the running application. The framework's
manual is [docs/ui/README.md](docs/ui/README.md).

## Lem

Lem shows what the pieces do together. Its core and its libraries, loaded
with ASDF, run in a page; the frontend is written with the GUI framework
(Lem's windows are components that follow what Lem draws); the editor, which
expects a thread of its own, waits for keys with `js:await`; and Lisp buffers
are evaluated in the editor's own image, so `C-M-x` on a `define-command`
changes the editor you are typing in.

```
$ contrib/lem/build.sh                    # the desktop application, in build/lem-desktop/
$ (cd build/lem-desktop && npm start)
$ contrib/lem/build-web.sh                # one HTML file for a browser, build/lem-web/lem.html
```

See [contrib/lem/README.md](contrib/lem/README.md).

## How it works

Clysm follows the design in `Wasm向けLispコンパイラ開発仕様.pdf`. In short:

* **Values**: the universal type is `(ref null eq)`. All types are in one
  recursive type group, so type tests are `ref.test`, never tag arithmetic.
  `NIL` is a real symbol.
* **Calls**: a closure has entry points for zero to four arguments as Wasm
  parameters and one taking an argument array; calls within a program are
  direct `call`s, tail calls are `return_call`.
* **Non-local exits**: `block`/`tagbody` within a function are Wasm branches;
  across functions an exit is usually a return with the exit pending, and a
  Wasm exception (`try_table`, `throw_ref`) only when the frames in between
  are unknown. `handler-case` and `restart-case` are built the same way.
* **Multiple values** go through a global buffer; **special variables** use
  shallow binding with a binding stack.
* **Compiling in the image**: every module shares the same type group, so a
  module compiled at run time is linked with the image and gets its live
  objects as constants.
* The Wasm binary is written by Clysm's own encoder (`src/wasm.lisp`); the
  Common Lisp library (`runtime/`) is written in Lisp and compiled by Clysm.

[docs/INTERNALS.md](docs/INTERNALS.md) describes all of this in detail, with
the performance figures and the optimizations;
[docs/DESIGN.ja.md](docs/DESIGN.ja.md) relates the sections of the design
document to the code.

## Performance

Time relative to SBCL 2.6 on Node.js 24 (x86-64), whole programs compiled
ahead of time (`tools/bench.sh FILE`, best of three runs):

| Benchmarks | Geometric mean | Slowest | Fastest |
|---|---|---|---|
| `bench/suite.lisp`, 24 programs used for tuning | 0.94 | 1.91 | 0.08 |
| `bench/gabriel.lisp`, 10 Gabriel benchmarks | 1.01 | 1.54 | 0.71 |
| `bench/holdout.lisp`, 17 programs written after the tuning | 1.06 | 1.68 | 0.08 |
| `bench/programs.lisp`, 63 more programs | 1.20 | 3.29 | 0.03 |
| `bench/libs.lisp`, libraries from Quicklisp | 1.79 | 2.92 | 0.32 |

Code loaded form by form (the REPL, `load`, ASDF) is somewhat slower than a
whole program. The slow cases — exits from closures that need Wasm
exceptions, some generic function dispatch, integers beyond the fixnum range
— are listed with their causes in
[docs/INTERNALS.md](docs/INTERNALS.md#performance).

## Limitations

* No threads (McCLIM runs in its single-process mode, Lem waits with
  `js:await`).
* No subprocesses: `uiop:run-program` is not available.
* `short-float` and `long-float` are `single-float` and `double-float`.
* Instances of a redefined class are updated only after
  `(sys::track-instances)`.
* `js:await` needs JavaScript Promise Integration: Node.js 24, Chrome and
  Edge 137 or later; not Firefox or Safari yet.

## Repository

| Path | Contents |
|---|---|
| `src/` | the compiler (reader, macro expansion, type inference, code generation, Wasm encoder) |
| `runtime/` | the Common Lisp library, compiled by Clysm |
| `ui/` | the GUI framework; `ui/electron/` is its desktop shell |
| `host/` | the Node.js and browser hosts |
| `contrib/` | portability libraries and fixups for Quicklisp systems; `contrib/lem/` |
| `examples/` | applications written with the GUI framework |
| `tests/`, `bench/` | tests and benchmarks |
| `docs/` | the implementation in detail, the design, the GUI manual |

## Tests

```
$ make test          # tests/cases/*.lisp, compared with SBCL's output
$ make test-all      # also interpreted, compiled at run time, and self-hosted
$ make fixpoint      # self-compilation reaches a fixed point
$ make ansi          # the ANSI test suite (third_party/ansi-test submodule)
$ make test-ui       # the GUI framework, without a browser
$ make test-lem      # Lem, desktop and browser page, without a display
```
