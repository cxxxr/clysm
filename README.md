# Clysm

Clysm is a Common Lisp implementation that compiles Lisp to WebAssembly GC.
Programs run on Node.js, in a browser, or in Electron. The compiler can also
run in WebAssembly, so the REPL compiles each form to Wasm at run time, in a
browser page too.

```
$ bin/clysm eval '(expt 2 100)'
1267650600228229401496703205376
```

## State

* The [ANSI test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test):
  21939 of 21940 tests pass (`tests/ansi/RESULTS.txt`).
* All functions, macros and special operators of the COMMON-LISP package that
  SBCL defines are implemented, including CLOS, the condition system, `loop`,
  `format` and the pretty printer. Extensions: the metaobject protocol, Gray
  streams, package-local nicknames.
* ASDF and Quicklisp work. McCLIM builds from Quicklisp and runs with its CLX
  backend.
* The compiler compiles itself; the second and third generations are
  identical.

## Implementation

Lisp objects are WasmGC structs and arrays, and fixnums and characters are
`i31ref`s. Memory is managed by the host's GC. Type tests are `ref.test`.
Non-local exits are branches or returns where possible and Wasm exceptions
(`try_table`, `throw_ref`) otherwise. Tail calls are `return_call`.
`js:await` uses JavaScript Promise Integration. The Wasm binary is written by
Clysm's own encoder (`src/wasm.lisp`).

The design follows `Wasm向けLispコンパイラ開発仕様.pdf`;
[docs/DESIGN.ja.md](docs/DESIGN.ja.md) relates its sections to the code, and
[docs/INTERNALS.md](docs/INTERNALS.md) describes the implementation in detail.

## Usage

Requires SBCL (the cross compiler runs on it) and Node.js 22 or later.

```
$ bin/clysm run tests/hello.lisp          # compile with the runtime and run on Node.js
$ bin/clysm compile prog.lisp -o prog.wasm
$ bin/clysm repl                          # the REPL image is built on first use
$ bin/clysm script file.lisp              # LOAD a file in the REPL image
$ make web                                # the REPL in a browser page, in build/web/
```

JavaScript is called through the `JS` package:

```lisp
(js:call (js:global "Math") "max" 3 9 4)                              ; => 9
(js:funcall (js:eval "(f, x) => f(x) * 2") (lambda (x) (+ x 1)) 20)   ; => 42
(js:await (js:call (js:global) "fetch" "/data.json"))                 ; in a js:async-function
```

## GUI

`ui/` is a GUI framework modelled on CAPI and CLIM: interfaces, panes and
layouts, commands, presentations, menus, dialogs and drawing. The screen is
updated when the slots of model objects are set. An error in an event handler
shows a dialog of its restarts.

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

## Performance

Time relative to SBCL 2.6 (Node.js 24, whole programs compiled ahead of time):

| File | Geometric mean | Range |
|---|---|---|
| `bench/suite.lisp` (24 programs, used for tuning) | 1.2 | 0.1 – 2.8 |
| `bench/gabriel.lisp` (10 programs, used for tuning) | 1.1 | 0.8 – 1.7 |
| `bench/holdout.lisp` (17 programs, not used for tuning) | 4.5 | 1.0 – 39 |

Specialized arrays, single floats, complex numbers and `call-next-method` are
slow. Code loaded form by form (the REPL, `load`, ASDF) takes about twice as
long as a whole program. `tools/bench.sh` runs the benchmarks.

## Limitations

* No threads. McCLIM runs in its single-process mode.
* No `uiop:run-program`.
* `short-float` and `long-float` are `single-float` and `double-float`.
* Instances of a redefined class are updated only after
  `(sys::track-instances)`.

## Source

| Path | Contents |
|---|---|
| `src/` | the compiler |
| `runtime/` | the Common Lisp library, compiled by Clysm |
| `ui/` | the GUI framework |
| `host/` | the Node.js and browser hosts |
| `contrib/` | portability libraries and patches for Quicklisp systems |
| `tests/`, `bench/` | tests and benchmarks |

## Tests

```
$ make test          # tests/cases/*.lisp, compared with SBCL's output
$ make test-all      # also interpreted, compiled at run time, and self-hosted
$ make fixpoint      # self-compilation reaches a fixed point
$ make ansi          # the ANSI test suite (third_party/ansi-test submodule)
$ make test-ui
```
