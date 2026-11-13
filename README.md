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
stack to its saved depth.

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

### A browser application

`examples/todo/` is a TODO list written entirely in Lisp: `todo.lisp` builds
the DOM, handles events and saves to `localStorage` through the `JS` package,
and is compiled ahead of time to `todo.wasm`, which `index.html` loads with the
browser host (`host/web/clysm.mjs`).

```
$ examples/todo/build.sh                    # build/todo/
$ python3 -m http.server -d build/todo      # open http://localhost:8000/
```

The same program is a desktop application with [Electron](https://www.electronjs.org/)
(`examples/todo/desktop/`): it saves the list to a JSON file, and the menu
creates, opens and saves lists with the native dialogs. `main.js` and
`preload.js` give the page a small synchronous API (the global `desktop`) that
`todo.lisp` calls through the `JS` package; in a browser, where it is absent,
the program uses `localStorage`.

```
$ examples/todo/build-desktop.sh            # build/todo-desktop/, installs Electron with npm
$ (cd build/todo-desktop && npm start)
```

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

Enter evaluates a complete input and starts a new line in an incomplete one
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
* No threads and no first-class continuations (not required by the standard).
* Character names are SBCL's; names above Latin-1 are read on first use from
  `data/unicode-names.txt` (`tools/gen-unicode-names.sh`).

## Performance

`tools/bench.sh [FILE]` runs a benchmark file (`bench/suite.lisp` by default,
or `bench/gabriel.lisp`) on SBCL and on Clysm (Node.js) and prints the time
ratio of each benchmark and their geometric mean. On an x86-64 machine with
SBCL 2.6 and Node.js 24, Clysm takes about 1.2 times as long as SBCL on
`bench/suite.lisp` (function calls, arrays, floats, lists, sorting, hash tables,
strings, FORMAT, the printer and reader, bignums and ratios, structures, CLOS,
special variables, non-local exits) and about 1.1 times as long on the
Gabriel-style `bench/gabriel.lisp`. Float code, function calls (`fib`, `tak`,
`takl`), catch/throw (`ctak`), sorting, strings and simple-vector loops run as
fast as with SBCL or faster; the slowest relative to SBCL, at 2-3 times, are
CLOS dispatch and `make-instance`, two-dimensional arrays of type T, the
reader, FORMAT to streams, hash tables (allocation of their storage), bignums
and integers between 2^29 and 2^62 (fixnums in SBCL, boxed here).

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
* `format` with a constant control string of `~A ~S ~D ~% ~& ~~` compiles to
  direct output calls; other control strings are parsed once and cached;
* exits through `unwind-protect` within a function branch to the cleanup
  instead of throwing; a `throw` to a `catch` of the same constant tag in the
  same function is a branch; `(catch 'tag (f ...))` calls a copy of the known
  function F in which `(throw 'tag ...)` is a return.

`tools/dump-func.lisp FILE NAME...` prints the Wasm instructions generated for
functions (by name), which helps to see what a form compiles to.

## Tests

`tools/run-tests.sh` compiles every `tests/cases/*.lisp`, runs it, and compares
standard output with SBCL running the same file. `INTERP=1` runs the files with
the in-image evaluator instead, and `SELFHOST=1` compiles them with the
Wasm-hosted compiler.
