# Clysm — Common Lisp on WebAssembly GC

Clysm is a Common Lisp implementation, written from scratch, that compiles
Lisp to **WebAssembly GC**. The same program runs on Node.js, in a browser
tab, or as an Electron desktop application — and the compiler itself runs
in WebAssembly too, so a page can compile Lisp typed into it.

```
$ bin/clysm eval '(expt 2 100)'
1267650600228229401496703205376
```

It is not a toy subset. Clysm aims to be a complete ANSI Common Lisp:

* **21,939 of the 21,940 tests** of the community
  [ANSI test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test) pass.
* Every function, macro and special operator of the `COMMON-LISP` package
  (as SBCL defines them) is there: CLOS with a metaobject protocol, the condition system with restarts,
  `loop`, `format`, the pretty printer, bignums, ratios and complex numbers,
  `eval`, `compile`, `load` and a REPL.
* Real libraries load: **ASDF** and **Quicklisp** work, and **McCLIM** builds
  from Quicklisp with its ~60 dependencies and opens windows on an X server.
* The compiler **compiles itself**; the second- and third-generation compilers
  are byte-for-byte identical.
* On the benchmarks it was tuned on, it runs about as fast as **SBCL**
  (geometric mean 1.1–1.2× SBCL's time; see [Performance](#performance) for
  the less flattering numbers).

## Why WebAssembly GC?

A Lisp on plain WebAssembly has to bring its own garbage collector and
manage a linear memory. WasmGC lets the engine's collector manage Lisp objects
directly, and Clysm leans on the newer parts of WebAssembly for the rest:

| Lisp | WebAssembly |
|---|---|
| fixnums, characters | `i31ref` immediates |
| conses, symbols, strings, vectors, instances, closures | GC `struct`s and `array`s in one rec group; type tests are `ref.test` |
| `block`/`return-from`, `catch`/`throw`, `unwind-protect`, `handler-case` | branches where possible, otherwise exception handling (`try_table`, `throw_ref`) |
| proper tail calls | `return_call` / `return_call_ref` |
| JavaScript objects | `externref`, wrapped so Lisp data can hold them |
| waiting for a promise in the middle of a function | JavaScript Promise Integration |

The binary is produced by Clysm's own encoder — no Binaryen, no wabt, no LLVM.
The original design document is `Wasm向けLispコンパイラ開発仕様.pdf`, and
[docs/DESIGN.ja.md](docs/DESIGN.ja.md) (Japanese) maps each of its sections to
the code.

## Trying it

You need **SBCL** (it hosts the cross compiler) and **Node.js 22 or later**
(WasmGC, exception references and tail calls are on by default).

```
$ git clone https://github.com/cxxxr/clysm && cd clysm
$ bin/clysm run tests/hello.lisp       # compile a file with the runtime, run it on Node.js
$ bin/clysm repl                       # a REPL running in Wasm (built on first use; takes a while)
```

At the REPL, every form you type is compiled to a **new WebAssembly module**
that is linked into the running image and executed at once, so a `defun`
there is native code, not interpreted.

```
$ bin/clysm compile prog.lisp -o prog.wasm   # ahead-of-time compile a whole program
$ bin/clysm script file.lisp                 # LOAD a file into the REPL image
$ make web                                   # the REPL in a browser page (build/web/)
```

### Talking to JavaScript

```lisp
(js:call (js:global "Math") "max" 3 9 4)                                  ; => 9
(js:funcall (js:eval "(f, x) => f(x) * 2") (lambda (x) (+ x 1)) 20)       ; => 42

(js:call button "addEventListener" "click"
         (js:async-function
          (lambda (event)
            (let ((response (js:await (js:call (js:global) "fetch" "/data.json"))))
              (show (js:await (js:call response "text")))))))
```

`js:await` suspends the Lisp computation — dynamic bindings, handlers and all —
until the promise settles, while the page keeps running.

### Writing applications

`ui/` is a GUI framework in the spirit of LispWorks' CAPI and of CLIM:
interfaces of panes and layouts, commands with typed arguments, presentations,
menus, dialogs, drawing, and stream panes you write to with `format`. State is
ordinary CLOS objects; the screen follows `setf` of their slots without any
code telling it to update. An error in an event handler shows its **restarts**
in a dialog, and the computation waits there until the user picks one.

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

Two example applications run both in the browser and as desktop apps:

```
$ examples/todo/build.sh && python3 -m http.server -d build/todo        # http://localhost:8000/
$ examples/contacts/build-desktop.sh && (cd build/contacts-desktop && npm start)
```

Built with `--dev`, an application docks a REPL in its own window: redefine a
component, a function or a class there and the running application changes.
The full manual is [docs/ui/README.md](docs/ui/README.md) (a guide in Japanese
and a reference of every exported symbol); `tools/new-ui-app.sh DIR` starts a
new application.

## Performance

How fast a program is depends on what it uses. Compiled ahead of time and run
on Node.js 24, Clysm takes this many times as long as SBCL 2.6:

| Benchmarks | Geometric mean | Range |
|---|---|---|
| `bench/suite.lisp` — 24 programs the compiler was tuned on | 1.2 | 0.1 – 2.8 |
| `bench/gabriel.lisp` — 10 Gabriel-style benchmarks, also tuned on | 1.1 | 0.8 – 1.7 |
| `bench/holdout.lisp` — 17 programs written afterwards, not tuned on | 4.5 | 1.0 – 39 |

Fixnum and double-float arithmetic, calls and closures, lists, strings,
structures and hash tables are close to SBCL. Specialized arrays, single
floats, `call-next-method` and some `format` directives are still slow. The
compiler gets there with type inference over each function, unboxed `i32`/
`i64`/`f64` locals, specialized copies of functions, inlining, and by turning
most non-local exits into returns instead of Wasm exceptions.
`tools/bench.sh` reproduces these figures.

## Limitations

* No threads (WebAssembly in a page has one), so McCLIM runs in its
  single-process mode. No `uiop:run-program`.
* `short-float` and `long-float` are `single-float` and `double-float`.
* Instances of a redefined class are updated only after
  `(sys::track-instances)`, which costs a JavaScript call per instance.
* Code loaded form by form (the REPL, `load`, ASDF) is about twice as slow as
  a whole program compiled with `bin/clysm run`.

## Where to read next

| | |
|---|---|
| [docs/INTERNALS.md](docs/INTERNALS.md) | the implementation in detail: object layout, calling convention, non-local exits, compiling into a running image, ASDF/Quicklisp/McCLIM support, every optimization |
| [docs/DESIGN.ja.md](docs/DESIGN.ja.md) | the design document mapped to the code (Japanese) |
| [docs/ui/README.md](docs/ui/README.md) | the GUI framework manual |
| `src/` | the compiler: `reader` → `frontend` (macroexpansion, AST) → `types`/`inline` → `codegen`/`prims`/`typed` → `driver` → `wasm` (the binary encoder) |
| `runtime/` | the Common Lisp library, written in Lisp and compiled by Clysm |
| `host/` | the Node.js and browser hosts |

`tools/dump-func.lisp FILE NAME...` prints the Wasm instructions generated for
a function — a good way to see what the compiler does with your code.

## Tests

```
$ make test          # every tests/cases/*.lisp on Clysm and on SBCL; the outputs must match
$ make test-all      # the same compiled ahead of time, interpreted, compiled at run time, and by the self-hosted compiler
$ make fixpoint      # the compiler compiles itself twice; the results are identical
$ make ansi          # the ANSI test suite (git submodule update --init third_party/ansi-test first)
$ make test-ui       # the GUI framework, without a browser
```
