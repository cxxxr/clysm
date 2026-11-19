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
  (base 2^32 digits, unsigned in the i32 elements; the kernels compute in
  i64, and Lisp code never reads a digit, as most are not fixnums). Integers
  beyond the fixnum range that fit in 64 bits are computed with i64
  arithmetic and made without digits (the `value` field holds them); the
  digits are made when the general bignum code asks for them.
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

A Wasm exception is expensive in V8 (about 120 ns per throw plus 33 ns per
frame it unwinds, whatever the form: `catch` or `catch_ref`, a payload or
none, flags of the engine; SBCL's `throw` takes about 7 ns), so an exit is
not thrown. It is a *return with the exit pending*: the tag and the value
are stored in the symbols `sys::%exit-tag` and `sys::%exit-value` and the
function returns null, which is no Lisp value. Every call of a Lisp
function that is not a tail call is followed by a test for null
(`checked-call` in `src/codegen.lisp`: `local.tee`, `ref.is_null` and an
`if` hinted as unlikely); on null, the code branches to the handler of the
call's context — the catchers around the call in that function (a non-local
block or tagbody, a `catch`, a `handler-case` or `restart-case`) take the
exit if its tag is theirs, else the function returns null in turn, through
its `unwind-protect` cleanups (the exit is kept in locals while they run).
A function that returns an unbound marker (null) as a value is told apart
by `sys::%exit-tag` being unbound. Tail calls pass the null through; the
catcher that takes the exit restores the binding stack. One handler serves
all the calls of a context (a function's top level, the body of each
catcher and `unwind-protect`); a direct call of a known function that
cannot leave with an exit pending (no `throw`, no exit out of a closure,
no call of an unknown function: `lam-may-exit-p`) is not tested, and after
a direct call of one that never returns normally (`error`, or a function
whose type the inference found to be empty: `lam-noreturn-p`) the code
branches to the handler at once.

So `(catch 'tag (funcall f))` where `f` throws, an exit from a closure
called by a library function, and `error` many calls below a
`handler-case` run without a Wasm exception, at about 2 ns per frame the
exit passes. The code that does not test — the Wasm helpers that call Lisp
functions (`(:callsym k)` and the direct stubs: type errors of primitives,
slow paths of arithmetic, argument count errors) and JavaScript calling
Lisp (`lisp_apply`) — throws the pending exit as the exception
`$lisp-throw(tag, value)`; the catchers also take it there (`try_table`
with `catch_ref`, comparing the tag, otherwise `throw_ref`), and
`unwind-protect` runs its cleanup on `catch_all_ref`. `catch`/`throw` keep
a dynamic list of active catch tags so that throwing to a missing tag
signals `control-error`. A module compiled in the image returns null from
its `run` when a form leaves with an exit pending, and `sys::%compile-module`
tests it like a call.

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

A simple form is evaluated instead of compiled (`simple-form` and
`simple-eval`, `lib/live-image.lisp`, as SBCL's `simple-eval-in-lexenv`): a
call of a global function whose arguments are simple, a variable, a
constant, `quote`, `function` of a name, `setq` of a special variable, `if`,
`progn`, `eval-when`, `declaim` (the compiler notes its proclamations too)
and the macros that expand to these, expanded once before anything runs:
`(swank:connection-info)`, `(micros:autodoc '...)`, a `defsystem` of an
`.asd` file (`(apply 'register-system-definition '...)`), `defpackage`,
`in-package`, `defvar`, `(load "file")`, `(require :asdf)`. Compiling such
a form took 5 to 30 ms, for a call of microseconds, and the first
compilation in an image also has V8 compile the compiler's own functions
(0.2 s from the REPL image, a few hundred ms from a heap image). The
definers of `%compile-time-definers` other than `defpackage` and
`in-package` (`defmacro`, `defstruct`, `defclass`, `deftype`...) are always
compiled, as the compiler does more with them than their expansion
does. A constant whose value a fasl cannot write (a package, a hash table)
is read from its symbol in compiled code instead of being a literal of the
module: as a literal, it made `compile-file` leave the whole form as source,
compiled again at every load (micros's `(defconstant keyword-package
(find-package :keyword))` did that for three of its functions each time Lem
loaded its Lisp mode).

A module made at run time carries only the runtime helpers that its
functions and closures call, transitively (`add-called-helpers`,
`src/live.lisp`), and only the globals (symbols, constants, closures) that
the code that can run refers to (`live-used-globals`). Before, every module
made the code of all the helpers and dropped most of them again, and kept the
symbol globals of all of them, which `RUN`, `SNAP` and `RESTORE` then filled,
saved and restored: three quarters of the code of ASDF's fasl.

Speed of the compiler (time to compile, best of three on a benchmark slot;
`tools/bench-libs.sh`'s `l-selfhost-compile` is the Wasm-hosted compiler
compiling `bench/suite.lisp` with the runtime, against the same compiler on
SBCL):

| | before (2026-11) | now | SBCL |
|---|---|---|---|
| runtime + `bench/suite.lisp`, the compiler on SBCL | 3.4 s | 1.7 s | |
| the same, the compiler in Wasm (`build/clysmc.wasm`) | 6.8 s | 3.1 s | |
| ASDF (14000 lines) compiled in the image | 9.0 s | 4.0 s | 2.6 s (`compile-file`) |
| cl-ppcre from source with ASDF, in the image | 1.95 s | 1.10 s | 0.98 s |
| 500 small `defun`s loaded from source (`clysm script`) | 1.32 s | 0.47 s | 0.10 s |

What made it slow, and what was done:

* **Typed copies of functions analyzed their callees again.** A copy of a
  function compiled for fixnum or double arguments is analyzed in type
  tables of its own, and every function it called was analyzed again in
  them, transitively: two thirds of the type inference. The return types of
  called functions now come from the tables of the compilation
  (`*ti-shared*`, `src/types.lisp`); they do not depend on the copy's
  parameters. (The code differs only where a cycle of calls is cut in
  another order.)
* **Searches that grew with the program**: closure globals were found in a
  list, and the line of each toplevel form by counting the newlines from the
  start of the file.
* **Modules made at run time** (above), and their fixed work: the
  functions of checked casts are made only when the code casts
  (`define-checked-casts :lazy`), the type section is reused from the last
  module when the types are equal, the printed names of helpers are kept,
  and the compiler's tables of lambdas, variables and nodes are the
  module's own (they were global in the image and grew with every form).
* **Forms evaluated at compile time too** (`eval-when` with
  `:compile-toplevel` and `:load-toplevel`, ASDF's `with-upgradability`)
  are compiled twice, to a module run at once and again for the fasl.
  Compiling them once, the fasl's module also being the one run
  (`%cf-compile-and-run`), saves compile time but makes each such form a
  module of its own in the fasl. With batches of modules, ASDF compiled in
  3.8 s instead of 4.8 and loaded from the cache in 0.45 s instead of 0.33;
  with a file compiled into one module, it compiles in 5.3 s instead of
  7.6 but loads in 0.51 s instead of 0.19 (4-CPU slot, interleaved).
  Loading happens at every start, compiling once per build: in a file
  compiled into one module (`*cf-unit-p*`) such a form is evaluated at
  compile time and compiled into the file's module, and only the batched
  fasl compiles it once. (The separate entries also read their own copies
  of uninterned symbols: `tests/cases/compile-file-uninterned.lisp`.)
* **Modules evaluated at once** (the REPL, `load` of a source file,
  `compile`) are made without `SNAP` and `RESTORE` unless the host records
  the modules for `save-image` (`CLYSM_SAVE_IMAGE`, `*live-snapshots*`):
  a third of the octets of a small form's module.

After the second round (the integration of 2026-11-20, whose image-compiled
code is larger: compiling a module there takes about 1.5 times as long as
before it; 4-CPU benchmark slot, medians of 4, a busy machine): ASDF in the
image 7.8 → 4.6 s (SBCL 2.8 s), cl-ppcre 2.5 → 2.05 s (SBCL 1.0 s), 500
`defun`s 0.77 → 0.65 s (SBCL 0.10 s).

Where the code of modules compiled in the image grew in that integration
(octets of the code section of ASDF's fasl, 1064 modules, with the changes
above): 3.10 MB before it; 3.46 MB with non-local exits as returns (+11.5%,
mostly about 300 octets of fixed cost per module: the helpers
`exit-pending-p` and `throw-pending-exit`, the test in `(:callsym k)`, the
symbols of the pending exit in `RUN`/`SNAP`/`RESTORE`; the tests after
calls are a small part in the image, where calls go through the function
cell); 4.55 MB with the CLOS dispatch copied into each generic function
(+32%, 1105 modules): a generic function of required parameters carries one
copy of the dispatch (about 1.4 KB, also each reader and writer of a
`defclass`), one with `&optional`/`&key`/`&rest` one per number of
arguments, up to five (one shared copy called by the entries: −6% of
ASDF's code, but `bench/clos.lisp` 2.5% slower, `c-gf-lambda-lists` 6%).
The name section is 0.57 MB of the 6.8 MB fasl, the type section (the base
types, the same in every module) 1.05 MB.

What remains (compiler in Wasm, whole program): type inference 0.8 s (its
node types are an EQ hash table keyed by conses, whose identity hash is a
call to the host: 0.3 s), code generation 0.6 s, encoding 0.75 s, macro
expansion and conversion 0.45 s. In the image, a small form costs about
0.4 ms of the compiler's time and 0.3 ms of V8's (decoding, validating and
instantiating the module); a third of the code of a small module is its
`RUN`, `SNAP` and `RESTORE`, the last two needed only by images that record
modules for `save-image`. Loading a source file compiles each form to a
module of its own; batching the `defun`s of a file would change what
`load` means (a function defined by the batch is not there while the
following forms are read and their macros expanded, unless defined first
by the interpreter, and calls within the batch would not see a
redefinition), so it is not done.

**`compile-file` compiles a whole file into one module** (`src/live.lisp`,
"Compilation units"; CLHS 3.2.2.3 lets the file compiler assume that a call
of a function defined in the same file calls that definition). Calls between
the functions of a file are direct calls, small functions are copied into
their callers, and argument and value types flow between them (fixnum
versions of a function, keyed calls in fixed slots, flag clones), as in a
whole program. Each form is converted when `compile-file` processes it, in the
environment the forms before it made. The module is cut into *segments* where
the batches of modules used to be cut (before a definer with compile-time
effects, after a `defpackage` or an `eval-when` with `:compile-toplevel`):
the constants of a segment are read from the fasl just before it runs, so a
package made by an earlier segment exists. A segment's functions are compiled
when it ends: they call the functions of the segments before them and of
their own directly, those of later segments through the symbol (they are not
defined yet). Each wasm function records its segment, and each global
(symbol, constant, closure) is set by the run of the first segment whose
functions refer to it (`global-segments`): a function runs only after the run
of its segment. The fasl has `(%fasl-unit ...)` for the module and its first
segment, which sets `sys::*fasl-unit-runner*` (a function calling the
`(:run-seg K)` functions of the module), and `(%fasl-segment K constants ...)`
for the others; when the image differs, the source of each segment is
compiled again. A form the compiler fails on is written as source; when the
module cannot be made or the constants of a segment cannot be written, the
whole file is written as source.

**Functions declaimed `inline`** keep their source in the image (the property
`sys::%inline-expansion` of the name, also written to the fasl), and calls
compiled later in the image (other files, the REPL) are replaced by a copy of
the body that sees none of the caller's lexical bindings, with `&optional`,
`&rest`, `&aux` and `&key` (constant keywords) bound by `let*`
(`inline-expansion-call`, `src/frontend.lisp`). `notinline`, global or
local, prevents it; `proclaim` records `notinline` on the symbol so that it
holds in an image that loaded the fasl.

The small functions of `COMMON-LISP` in the REPL image (required parameters,
no loop, at most 100 conses: `alphanumericp`, `char-upcase`, `fifth`...) keep
their source the same way (`runtime-inline-source`, `src/driver.lisp`, set
when the image starts: 60 KB), so code compiled in the image contains a copy
of them as a whole program does. The guarded copies of small functions
defined at the REPL (`live-inline-call`) take any lambda list (`&optional`,
`&key` with constant keywords, `&rest`): `(kfun i :c 3)` of a function
defined in an earlier form runs as fast as in a whole program.

A call of a structure's constructor compiled in the image is of the
structure's type (`*struct-constructors*`, registered by `defstruct` through
`%note-struct-inline`), so the inline accessors of the object it made do
not check its class.

## Heap images

An image that has loaded a large program (Lem: 360 fasls) can save its heap
to a file and start from it again, as `save-lisp-and-die` does: the image
and the modules compiled in it are instantiated without running their
toplevel forms, and the objects are made again from the file
(`runtime/image.lisp`, `image_load` in `src/prims.lisp`, `host/common.mjs`).
The engine cannot do it for us: WebAssembly GC objects are opaque to
JavaScript, and V8's snapshots do not hold WebAssembly.

`(sys:save-image FILE FUNCTION)` writes the heap; a host started with the
file (`CLYSM_IMAGE=FILE node host/run.mjs IMAGE.wasm`, or the option
`image` of `loadClysm`) calls FUNCTION. The image saving it must have been
started with `CLYSM_SAVE_IMAGE` set, so that the host records the modules
compiled in it (their octets go into the file; with `CLYSM_SAVE_IMAGE=fasls`
those read from fasls are named by file and position instead). The REPL
image itself starts from one that `bin/clysm` saves after its toplevel
forms (`build/clysm-native.img`, see Start-up and loading).

* **What is saved.** The objects reachable from the roots: the objects of
  the image's immutable globals (its symbols, constants and static closures,
  `:image-objects`), the values of its mutable globals (constants built at
  start, `load-time-value`s), and the globals of every module compiled in
  it (each module exports `snap`, which returns them, and `restore`, which
  sets them; `src/live.lisp`). Special variables are saved with their
  global values, not the values bound around the call (the oldest entry of
  a symbol on the binding stack). The objects of the image's globals are
  the same objects in the image started from the file: only their mutable
  fields are written.
* **Functions.** A closure's entries are references to functions, which
  cannot be written. Every function that a closure can refer to (every
  `ref.func`) is placed in the image's table of functions: the image's own
  from 0 (an active element segment), and each module's from its code base,
  an imported global that the host gives it when it instantiates it (it
  makes room for the number of functions that the custom section
  `clysm.codes` gives). A closure is saved as the places of its functions in
  the table; the host loads the modules in the same order, at the same code
  bases.
* **The format.** 32-bit words: a header (number of objects of the image, of
  the heap, of records), then each object of the heap with its kind and its
  immutable contents (made in an order in which an object's immutable
  fields refer to objects made before it: strings and vectors, then
  instances, numbers, symbols and closures), then records that set the
  mutable fields of objects (cons, vector, symbol, closure name, instance,
  cell, package, array header), so that cycles are made in two steps; then
  the roots. A reference is one word: an i31 (fixnum, character) is its
  value * 2 + 1, an object (index + 1) * 2, null 0. The heap is copied into
  the linear memory and read there by Wasm code (`image_load`). The file
  also has the build of the image it was saved by (the custom section
  `clysm.build`, a hash of the image's octets): an image built otherwise
  starts as usual, as it does when the file cannot be read or loaded (the
  host then instantiates the image again and runs its toplevel forms).
* **What cannot be saved.** JavaScript objects (`externref`): the variables
  of the runtime, the UI framework and Lem that hold some (functions made
  with `js:eval`, the page's document, caches) are defined with
  `sys::%defvar-host`, whose form is evaluated again in the image started
  from the file (`*host-variables*`); `save-image` sets them to NIL while it
  saves. Another JavaScript object signals an error that names the variable
  it is reached from, and the path to it (breadth first, so the shortest).
  `sys:*save-image-hooks*` are called before saving, and
  `sys:*restore-image-hooks*` after starting from the file (UIOP's
  `*image-restore-hook*`: the standard streams, the command line, the
  temporary directory). The hash tables whose keys are hashed by identity
  (the host's identity hashes are not saved) are rehashed when the image
  starts. Other threads and the bindings around the call are not saved.
* **Compilation hints.** The image starts with code that has never run, and
  V8 compiles each WebAssembly function the first time it is called, which
  for Clysm's large functions costs tens to hundreds of microseconds each.
  A run that starts from the file with every function instrumented
  (`instrumentWasm`: before its body, a function sets its octet in a table
  in the linear memory) learns which functions run; the modules in the file
  and the image then get a `metadata.code.compilation_priority` section
  naming them (`withCompilationHints`), and V8 started with
  `--wasm-compilation-hints` compiles them with the modules, in background
  threads.
* **Size.** The modules saved in the file are without the bodies of their
  `run` and `snap`, which are never called there (so an image started from
  a heap image cannot be saved again); `restore` uses unchecked casts.
* **Linking.** V8 spends most of the time of compiling thousands of small
  modules on each module (its copy of the image's types, its imports), and
  validates every function when it compiles a module. `host/link.mjs` links
  the modules into one: the image's types, then those of each module; the
  imported functions of all; the functions, globals and data of each module
  in turn (their indexes in the code mapped); an element segment for each
  module at its code base; and exports `restore<i>`. The host then compiles
  and instantiates one module (`header.linked`).

For Lem (cxxxr/lem's `frontends/clysm/`), the file holds 4655 modules linked into one (36
MB) and 500,000 objects (35 MB of words). Starting the desktop application,
with the machine busy with other programs (load average 3.5 to 5.8), takes
from the `lem` command to the editor on the screen 0.76 to 1.06 s (it took
3 s loading the fasls, 6 s before; a median of 0.87 s at load 4, 1.02 s at
load 5.5): Electron shows the page at 0.35 s; then
compiling the image and the module takes 0.3 s (in V8's background
threads), instantiating the module 0.03 s, making the objects 0.06 s,
restoring the modules' globals 0.03 s, and Lem draws its first frame 0.04 s
after its MAIN. Without a display (the tests' offscreen window) it is 0.6
s. Without compilation hints it was 1.85 s, the functions that ran when Lem
started (2400 of Lem, 4655 `restore`s, 700 of the image) each compiled when
first called (0.05 to 0.1 ms each); without linking 1.05 to 1.2 s.

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

A property, a method or a global named by a literal string (`(js:get node
"kind")`) is made a JavaScript string once, where it is written (compiler
macros of `js:get`, `js:set`, `js:call` and `js:global`), and kept in a cell
of that name: a heap image cannot hold JavaScript strings, so `save-image`
empties the cells and the image started from the file makes each name again
when it first uses it.

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

### Subprocesses

`runtime/process.lisp` runs programs with Node.js's `node:child_process`:
`sys::%run-program-sync` waits for the program (`spawnSync`), and
`sys::%launch-program` starts one and gives Lisp streams for its standard
input, output and error output (characters, decoded as UTF-8, or octets).
The processes are kept by the manager of `host/process.mjs`, which holds
what they write until Lisp reads it. In the Node.js host the Lisp program
runs in a worker: a call of the manager, which is in the main thread,
blocks the worker until the answer is in a shared buffer (as for TCP
sockets). In a library (`host/lib.mjs`) the manager is in the same thread,
and a read that has to wait waits with `js:await` — so it is done in a
function of `js:async-function`, and several processes can be read at the
same time. UIOP's `run-program`, `launch-program`, `wait-process`,
`process-alive-p` and `terminate-process` are built on these
(`third_party/asdf/port.py`).

```lisp
(uiop:run-program '("git" "rev-parse" "HEAD") :output :string)
(let ((p (uiop:launch-program '("git" "cat-file" "--batch") :input :stream :output :stream)))
  (write-line "HEAD:README.md" (uiop:process-info-input p))
  (finish-output (uiop:process-info-input p))
  (read-line (uiop:process-info-output p)))
```

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
A fasl holds the compiled Wasm module of the file (one module, in segments:
see "Compiling inside the running image") with its constants, and the source
forms as a fallback; the fasl
directory under `~/.cache/common-lisp/` names the image build, so rebuilding
Clysm recompiles systems.

Loading a fasl costs reading its constants and the engine's compilation of
its modules; two things made it cost much more, and are avoided:

* **A form whose constants could not be written** was kept as source and
  compiled again at every `load`. Literal hash tables and specialized arrays
  (CLHS 3.2.4.2.2 counts them as externalizable) are now written in fasls as
  `#.(sys::%fasl-hash-table 'test '(key value ...))` and
  `#.(sys::%fasl-array 'element-type 'dimensions '(element ...))`
  (`runtime/print.lisp`; the printer writes them so only with fasl syntax,
  which reads with `*read-eval*`). Babel has the tables of its single-octet
  encodings in its code: loading it took a second.
* **Very large functions.** Consecutive toplevel forms share a function, up
  to 40 of them (`*toplevel-batch*`, `src/driver.lisp`), but also no more
  than about 4000 conses of converted code (`*toplevel-max-weight*`): the
  code of each form is in that function, with locals of its own, and V8's
  baseline compiler takes time in the number of locals at every branch. The
  thirty `define-dfa` tables of Inquisitor were one function of 400 KB with
  11000 locals, which took 1.5 s to compile. A single form that large is
  still one function (`contrib/fixups.lisp` splits Babel's
  `instantiate-concrete-mappings` into one form per encoding). Locals are
  now shared by variables whose lives do not meet (see Start-up and
  loading), which is the general remedy.
* **Reading the text of a fasl.** Its forms are mostly package-qualified
  symbols. The fasl's text is decoded by the host at once
  (`%fasl-text-from-host`: `__clysm_read_bytes`, which the modules are read
  with, and `TextDecoder`) and read from a string stream, where the reader
  reads plain symbols straight from the string, now also `PKG:NAME`,
  `PKG::NAME` and `:NAME` (`%read-plain-symbol`; what it cannot intern is
  left to the reader of tokens, which signals the errors). Reading the
  13000 forms of Lem's fasls took 1.6 s, and takes 0.4 s.

ASDF itself, `contrib/asdf-hooks.lisp` and `contrib/fixups.lisp` are compiled
once per image build (`~/.cache/common-lisp/clysm-asdf/NAME-IMAGEID.fasl`,
`%load-compiled-bundled`); the fasls of other images are deleted only when a
month old, since several images (the REPL's, an application's) may be in
use: deleting them made the other image compile ASDF again, ten seconds.
`make test-asdf` loads a small multi-system example (`tests/asdf/`). There
are no subprocesses, so `uiop:run-program` is not available.

Lem (cxxxr/lem's `frontends/clysm/`) loads in 2.1 to 2.3 s on Node.js instead of 6 s, and
without ASDF: its `load-lem.lisp` loads the fasls in the order that
`prepare.lisp` noted, after registering the systems as immutable. Its
pages start from a heap image instead (see Heap images).

A heap image saved after loading systems with ASDF keeps ASDF's
configuration (UIOP's hook of dumped images, which clears it, is not
called; its hooks of restored images are, `contrib/asdf-hooks.lisp`).

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

## SWANK (SLIME)

`(ql:quickload :swank)` loads SLIME's server from Quicklisp: a fixup of the
system (`contrib/fixups.lisp`) adds Clysm to `swank-loader`'s lists and the
backend `contrib/swank/clysm.lisp` to the system. micros, Lem's fork of
SWANK, gets the same backend (`contrib/micros/clysm.lisp`); Lem on Clysm
runs it in its own image (cxxxr/lem's `frontends/clysm/src/`). `bin/clysm swank [--port
N]` loads it and serves connections (`contrib/swank/start.lisp`).

* Sockets: the Node.js host listens and accepts (`sys::open-tcp-server`,
  `sys::accept-tcp-stream`, synchronous like the client sockets).
* With green threads (below) the communication style is `:spawn`: the
  backend's threads, mailboxes and locks are those of `runtime/thread.lisp`.
  Without them (no JavaScript Promise Integration, a browser) it is `NIL`:
  `wait-for-input`
  blocks on the connection's socket (`sys::socket-wait-input`), not on the
  terminal. A request that SWANK read and queued while another was waited
  for (by the debugger) counts as input. Each request is processed with the
  connection's current I/O redirection (a patch of `process-requests`): with
  style `NIL` the requests that follow `create-repl` in the same batch would
  otherwise write to the process's streams.
* Evaluation: SWANK's `EVAL` (shadowed in the package `SWANK`) compiles to
  Wasm like the REPL; `CL:EVAL` is the interpreter.
* Argument lists: functions compiled in the image keep their lambda list
  (`sys::%arglist`, emitted by `process-defun` in live mode); the REPL image
  has those of the functions and macros of `COMMON-LISP`
  (`*record-definitions*` in `tools/build-repl.lisp`).
* Definitions (`M-.`): `load` of a source file and `compile-file` record
  where each toplevel `DEF...` form begins (`sys::%definition-sources`, octet
  offsets; a fasl has `%note-source` entries at its end); the REPL image has
  those of `COMMON-LISP` in `runtime/`. The backend converts offsets to
  character positions.
* Backtraces are the names of the Lisp frames on the JavaScript stack.
  Wasm locals cannot be read from outside, so with `sys::*record-frames*`
  (set when Emacs connects) a function compiled in the image keeps its
  variables in a frame record, `#(name names nparams parent value...)`
  (`add-debug-frame` in `src/frontend.lisp`): made on entry, it is the
  value of the special variable `sys::*debug-frame-stack*` during the
  function (so non-local exits restore it), and the values of the
  parameters and of the variables of `let`, `let*` and
  `multiple-value-bind` are stored into it when they are bound or
  assigned. The backend matches the records with the frames of the stack
  by name; frames are printed as calls, `(F 1 2)`. Copies of small
  functions into their callers are not made then.
* Interrupts (`C-c C-c`): Wasm code cannot be interrupted from outside, so
  code compiled in the image while `sys::*interrupt-checks*` is true (set
  when Emacs connects) decrements a counter shared by the modules
  (`poll_count`, exported by the image) on function entry and at the head
  of loops (`poll-code` in `src/codegen.lisp`). Every 100000 checks
  `sys::%interrupt-poll` calls `sys::*interrupt-poll-function*`. SWANK's looks
  whether data arrived on a socket (a counter in the shared buffer of the
  host's sockets, read without a call of the main thread) and, if the
  connection has input, processes the requests as with communication style
  `:SIGIO`: `:emacs-interrupt` enters the debugger where the computation
  was. Code of the runtime and code compiled before has no checks (`sleep`
  polls).

`make test-swank` runs the server and a client that speaks SLIME's protocol
(`tests/swank/client.mjs`).

## Threads

`runtime/thread.lisp` has green threads: a thread is a computation of
`js:async-function` (its own stack, which JavaScript Promise Integration
suspends, and its own dynamic bindings), and the threads take turns in the
one thread of the host, JavaScript's event loop resuming the one whose wait
is over. A thread runs until it waits: `sys::grab-mutex` of a mutex another
holds, `sys::condition-wait`, `sys::wait-on-semaphore`, `sys::join-thread`,
`sleep`, `sys::thread-yield`, or input from a socket or the terminal. Those
then do not block the host: socket reads, accepts and connections are
messages to the host's main thread answered by messages (`readAsync` in
`host/run.mjs`), the terminal is read with `fs.read`. Until a second thread
is made nothing changes: they block as before.

* The program (`sys::main`, the REPL and scripts) runs as a computation, the
  main thread, so it can wait too. The host gives computations stacks as
  deep as the worker's (`--wasm-stack-switching-stack-size` and
  `--stack-size`, set before the worker starts); elsewhere (a browser,
  where they are about 1 MB) the program does not run as one.
* A module compiled in the image is run from WebAssembly (`call_ref` of its
  `run` function, which the host's `instantiate_module` returns), not
  through JavaScript, so that its code can wait.
* A new thread starts when its creator waits, with none of its dynamic
  bindings: it sees the global values and has no handlers or restarts of
  the creator.
* Preemption: code compiled while `sys::*interrupt-checks*` is true (see
  SWANK; on in SLIME sessions) checks for interrupts on function entry and
  at the head of loops. While threads run, every 10000 checks
  (`sys::*poll-interval*`, set into the counter by `sys::%set-poll-count`)
  `sys::%thread-poll` gives the others their turn (`thread-yield`, a
  `setImmediate`, so that the host's events are processed too) when the
  thread has run for `sys::*time-slice*` (10 ms) since it last got its turn,
  or at once when one was woken, a socket received data or a `sleep` is
  over. A thread is not preempted while it compiles
  (`sys::*preemption-inhibited*`, bound around the compiler, whose state the
  threads share). With one thread nothing of this runs.
* `sys::interrupt-thread` runs a function in the thread when it waits, or
  at its next interrupt check. `sys::destroy-thread` is an interrupt that
  unwinds the thread.
* An error that the thread does not handle ends it (with a message on
  `*error-output*`), unless `*debugger-hook*` is set; `sys::join-thread`
  then signals an error.
* `contrib/bordeaux-threads` (Bordeaux Threads 1 and 2) uses them where
  JavaScript can suspend WebAssembly, unless `bt2:*thread-starter*` is set
  (Lem runs its threads from the event loop of its page that way).

What the threads share:

* The world lock (`sys::%call-with-world-lock`, a recursive mutex): one
  thread at a time compiles (the compiler's tables are global) or changes
  the class system (`ensure-class`, `finalize-inheritance`, `defgeneric`,
  `defmethod`, `add-method`, `remove-method`), and it is not preempted
  meanwhile; it may still wait (in a macro expander, a method of the
  metaobject protocol), and a thread that waits there for one that compiles
  waits for ever, as with SBCL's world lock. `load` and `compile-file` take
  it for each form they compile, not for the whole file, so loaded code may
  start threads and wait for them.
* The runtime's own operations do not wait and have no interrupt checks, so
  a thread is never switched in the middle of one: hash tables (whose tests
  are the standard ones), packages, symbols' properties, streams' buffers
  are consistent for every thread (`:synchronized` of `make-hash-table` is
  accepted and changes nothing). The caches of generic functions are
  replaced, not changed, so a thread that computes an entry while another
  changes the function stores nothing stale. What is not protected is what
  user code does between switches: changing a hash table that another
  thread iterates over (`maphash` with a function that waits), as with
  native threads.
* A socket stream's input is filled by one thread at a time (a mutex of the
  stream); a thread that reads a stream another is filling waits for it.
* Subprocesses: with threads, `uiop:run-program` (`sys::%run-program-sync`)
  starts the process and reads its output and waits for it without
  blocking the others; reads of a process's output and waits for its end
  (`uiop:launch-program`) are messages to the host's process manager.

There is no parallelism, and a thread that computes in code without
interrupt checks (the runtime, code compiled without them) keeps the others
waiting until it waits or calls code with checks. A thread that a
JavaScript callback runs in (`js:funcall` of a function that calls Lisp)
cannot wait or be preempted there. `make test-threads` runs `tests/threads/run.lisp`.

What still blocks every thread: file operations (synchronous and short),
writes to sockets (until the host accepted them), `run-program` with
`:input :interactive`, and JavaScript the program calls synchronously.

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
`(complex (float 0.0 1.0))`; like SBCL (which fails it the same way), Clysm
reads `(complex X)` as the complexes whose parts are of
`(upgraded-complex-part-type X)`, here `float`.

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
  costs a JavaScript call per instance, so it is off by default. Without it,
  an instance made before the redefinition is updated when its slots are
  next accessed, provided its number of slots tells its layout: the class
  keeps its earlier layouts, and an instance with as many slots as one of
  them (and as no other) is updated from it. An instance whose number of
  slots matches no earlier layout, or two different ones, is an error when
  accessed; one whose layout changed without its number of slots is not
  noticed. Only a class redefined with other slots after it had instances
  can have obsolete instances: the caches of slot locations (call sites of
  accessors, cached `slot-value`) use the locations of the other classes
  without a check (a redefinition empties them); for such a class the
  accessor functions, and the cache of a `slot-value` out of the code it is
  called from, compare the instance's number of slots with the class's.
* Threads are green threads (see [Threads](#threads)); no first-class
  continuations (not required by the standard).
* Pathnames are Unix namestrings; on a Windows host (Node.js's
  `process.platform` is `win32`, `sys::*windows-paths*`) a backslash in a
  namestring is a slash, `C:` is the device `"C"` and `\\server\share` the
  device `"//server/share"` of a UNC name (so the clones of a WSL
  distribution, `\\wsl$\Ubuntu\...`, have pathnames too); namestrings are
  written with slashes, which Node.js takes on Windows.
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
| `bench/libs.lisp` (`tools/bench-libs.sh`) | cl-ppcre, Alexandria (its test suite and utilities), split-sequence, Babel from Quicklisp; the self-hosted compiler compiling `bench/suite.lisp` | 1.6–1.8 | `l-ppcre-replace` 2.4–3.0 | `l-alexandria-tests` 0.3–0.5 |
| `bench/programs.lisp` | 63 more programs | 1.19 | `p-builtin-dispatch` 3.15 | `p-remove-duplicates` 0.03 |

Code loaded form by form into the image (`clysm script`, the REPL, `load`)
against the same file compiled as a whole program (`MODE=script tools/bench.sh
bench/suite.lisp`): geometric mean 1.1, slowest `b-special` 2.8 (`b-call` 1.1, was
1.9; `b-struct` 1.0, was 1.35).

The libraries' own time (Clysm's milliseconds, `tools/bench-libs.sh`, the
best of two back-to-back runs on the same 4-CPU slot against the build
without these changes, 2026-11-20, on a busy machine whose SBCL times varied
by 30%): `l-babel` 340 → 180, `l-ppcre-replace` 428 → 372, `l-ppcre-scan`
270 → 224, `l-split-sequence` 180 → 125, `l-alexandria-utils` 143 → 134;
0.80 times the time before in geometric mean. Code loaded into the image
(`MODE=script`): `bench/suite.lisp` 0.94 times the time before (`b-call`
294 → 162, `b-struct` 198 → 132), `bench/holdout.lisp` 0.95. What did it: a
file as one module, `inline` functions and the small functions of
`COMMON-LISP` copied into code compiled in the image, copies of REPL
functions of any lambda list, inline array access with 32-bit subscripts and
the range test of `fixnum` declarations (Babel), `find` in a short literal
sequence, a loop for each kind of test in `find`/`position` with `:key` or
`:test` on simple strings and vectors, and the type of a structure's
constructor.

The slow areas that `bench/holdout.lisp` first showed (4.5 at the time) were
fixed as whole features, and each fix was measured on programs written for
it afterwards; those are kept in further files: `bench/clos.lisp` (geometric mean 1.4, slowest `c-eql-dispatch`
4.0), `bench/control.lisp` (0.94, was 1.3; slowest `t-return-maphash` 4, `v-hb-nosignal` 2.9), `bench/io.lisp` (1.2–1.3, was 1.46;
`i-file-roundtrip` 1.6–2.0, was 13; `i-print-numbers`, `i-parse-ints` 2.5), `bench/library.lisp`
(1.04, was 1.12; `v-map-every` 2.8, `v-hash-iterate` 2.2, was 3.4),
`bench/vectors.lisp` (0.43, algorithmic code over simple vectors without
declarations: fannkuch, sorts written by hand, a heap, a sieve, shuffles;
slowest `x-sort-key` 1.26; in the image 1.13 of compiled). An `aref` of a
vector of known kind whose subscript has no known type (an element of a
vector, an unbounded counter) is inline with a test of the subscript, and
a parameter that starts a loop counter (`(loop for i from start ...)`)
makes its function's fixnum version be tried on entry.

What is not as fast as intended, and why:

* **Code loaded into the image, single items.** The geometric mean is within
  1.3, but `b-special` (2.8) and `b-catch` (1.4) are not. A call of a function defined in another form goes through the function
  cell unless the function is small enough to be copied (with a guard): a
  larger function is not in the caller's module, so neither the engine nor
  the compiler inlines it, and a `throw` out of it is a Wasm exception
  (`b-catch`). Calls with `&rest` arguments of functions of other files or
  forms go through the function cell and the entry that conses the list.
  `b-special` (a `let` of a special variable around a call of
  `use-sp`, a function of the form before) runs either at about 1.6 times
  the whole-program speed or at about 2.8, and stays at one of the two for
  hundreds of milliseconds; it is at 2.8 more often in the form right after
  the definition of `use-sp`, as in `bench/suite.lisp`, but a run of the
  same file can be at 1.6. Not understood yet; ruled out (2026-11): the code
  differs (it is the same function object, compiled to the same octets), the
  guarded copy of `use-sp` is not taken (no `USE-SP` frame in a profile with
  `--liftoff-only`), the engine's tiers (the two speeds stay with
  `--liftoff-only` and with `--no-liftoff`), incremental marking (without it
  the slow run moves to another form), the instantiation of the module (its
  `run` is called after it). With `(declaim (notinline use-sp))` it is
  always at the slower speed. The same loop without the call (the
  binding and a read of `*sp*`) and the same loop with `setq` in place of
  the binding (three times faster) vary between runs in the same way, so
  neither the call nor the binding is the cause.
* **Libraries** are within 2.5 to 3 (`l-ppcre-replace`, `l-split-sequence`):
  the remaining time is mostly in the runtime's sequence functions called with
  keyword arguments of variables (`position` with `:key` and `:test`
  functions), cl-ppcre's matcher closures calling each other through
  `funcall`, and calls across files and into the runtime, which are calls of
  another Wasm instance (not inlined by the engine).
* **Non-local exits** are returns with the exit pending, tested after
  every call that is not a tail call (above, "Non-local exits"): a throw
  from a closure one frame below its `catch` costs what SBCL's does
  (`t-throw-closure` 1.0, was 13; 20 frames below, 0.43, was 4.4; in the
  image 21 ms instead of 280). The test costs about 10% in recursion
  through `funcall` (`h-fib-funcall`); direct calls of functions that
  cannot exit are not tested.
* **Generic functions**: dispatch on EQL specializers (4 times: the entry
  of a class with EQL specializer objects calls a guard function shared by
  all generic functions, which then calls the method; comparing the EQL
  objects in each generic function's own dispatch (out of line or inline),
  or letting that dispatch call the method the guard selects, were slower
  still, 2026-11) or on many classes at one call site (2.2–3.7: the entries
  are searched one after the other; past 32 they move to a hash table,
  searched out of the generic function's own code, and a class keeps the
  function of each such generic function for its instances in a short
  vector of its own (`+class-dispatch+`, `%gf-class-lookup`): 40 classes
  in random order take 0.62 of the time of the hash table (whole program)
  and 0.73 (fasl), about 2.5 times SBCL. Probing the table in
  that code, past 8 entries, was twice as slow as the linear search for 12
  to 20 classes in turn or at random and gained 10% only past 32: the
  address of the method then depends on a chain of loads from the argument,
  which a mispredicted call waits for; SBCL takes 3–4 ns a call whatever
  the number of classes). Generic functions with `&optional`/`&rest`/`&key`
  (`c-gf-lambda-lists` 1.3, was 2.6) and `make-instance` with a class that
  is not a constant (`c-make-instance-plain` 1.6, was 2.7) are below. Slot
  accessors are about as fast as SBCL's (`c-accessors` 1.0, `c-with-slots`
  1.0): a call site of a reader, of `setf` of an accessor or of a `:writer`
  (of a class or a condition) keeps the slot locations of four classes and
  reads or writes the slot itself, and `slot-value` with a constant name
  compares the four entries of its cache inline. The slots of a class
  redefined with other slots after it had instances are checked first
  (`c-slot-redef` 1.5; see the limitations).
* **Integers between 2^29 and 2^63** are boxed when they are stored in data
  structures or passed between functions (fixnums in SBCL; a parameter or
  variable updated by integer arithmetic keeps them in an i64 local).
  **Bignums** have 32-bit digits against SBCL's 64: a pass over the digits
  (addition, a product by a fixnum) is 1.3–1.5 times SBCL, products of two
  large bignums and divisions 2.5–4 times (SBCL multiplies 64 by 64 bits;
  here a column of products costs about 0.5 ns a digit pair). `vector-push-extend` is inline at the call
  site and grows by `array.copy` into new storage, whose allocation is the
  remaining cost (pushing 50000 elements: 1.5); `equal` hash tables with
  string keys hash in a Wasm helper (`h-wordfreq` 1.34).
* **Files** are no slower than data in memory. A file is read with one
  `readSync` straight into the linear memory (`read_file_mem` in
  `host/jsffi.mjs`; its existence is not asked first) and written with one
  `writeFileSync`, its octets crossing at once (`js:to-octets`,
  `js:from-octets`); text is encoded and decoded in UTF-8 by Wasm loops
  (`%utf8-encode-into`, `%utf8-decode`, `%bytes-find` for `read-line`). A
  file stream keeps its octets, position and length in a Wasm struct of
  its own (`$fsbuf`, `src/layout.lisp`), which its `fast` slot holds while
  it is an open character input file: `read-char` and `peek-char` are
  inline at the call site (`%fs-fast-char`: an instance whose `fast` slot
  holds an `$fsbuf`, then three fields and an octet), as for a string input
  stream (`fast` is `:string`). `read-line`, `write-string`, `write-line`
  and `write-char` have fixed-argument paths for files. `i-file-roundtrip`
  is 1.6–2.0 times SBCL (13 before; SBCL's own time varies between 20 and
  26 ms from run to run); `i-read-lines` 1.05. A `read-char` loop over a
  4 MB file takes 1.6 ns a character, twice SBCL's time, while scanning the
  same text as a simple string with `schar` is itself three times SBCL's
  time: the remaining factor is the cost of Lisp code in Wasm, not of the
  file. Opening and closing a 55 KB file costs about 10–18 µs.

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
* **Arrays of unknown type** (a parameter without declarations) are tried
  inline for the usual kind before the helper that handles every array
  (`speculative-locate-code` in `src/typed.lisp`): `aref`, `svref` and
  their `setf` test for a simple vector, `char` and `schar` for a simple
  string, and `aref` and `char` then for a one-dimensional header (fill
  pointer, adjustable) over such storage, whose dimension the header keeps
  in a field of its own (`$mdarray` field 7, 0 unless the array is
  one-dimensional), so the test is one load; a two-dimensional `aref` tests
  for a header over a simple vector. `length` tests for a simple vector, a
  simple string, then a fill pointer; `vector-push-extend` and `vector-pop`
  are inline for vectors of elements of type T. After `svref` or `schar`
  returns, the compiler knows the array's type. `bench/arrays.lisp`
  (14 programs in undeclared style; geometric mean 1.07 before, 0.85 after;
  loaded into the image 1.02 → 0.83 of SBCL's time): `a-heap-sim` (a binary
  heap of structures in an adjustable vector) 1.22 → 0.66, `a-fp-buffer`
  1.34 → 0.73, `a-fp-stack` 1.04 → 0.78, `a-strings` 1.04 → 0.93.
* **Structure tests**: `typep` of a structure defined earlier, `typecase` and
  the predicates compare the layout inline (`%structure-typep`), and the
  class precedence list is searched only when it differs; `typecase` of a
  variable tests the variable itself, so the accessors in the clause taken
  need no check. `copy-structure` and the copiers copy the slot vector with
  `array.copy` (`%copy-instance`). `a-struct-dispatch` (`etypecase` over
  included structures) 2.08 → 1.39. Still slow: `a-struct-int` (2.3; a
  predicate that fails searches the class precedence list out of line, and
  every slot access goes through the separate slot vector) and `a-nbody`
  (1.3; doubles in slots without declarations are boxed and their
  arithmetic generic).
* **Versions of a function**: a function with a loop or a recursive call
  whose parameters are used as numbers tests its arguments on entry and
  continues in a copy compiled for fixnums (or doubles). Small functions are
  copied into their callers — in the image with a test that the function has
  not been redefined.
* **Calls**: direct calls between the functions of a program; keyword
  arguments passed in fixed slots (in the image too, checked against the
  layout of the function's current definition); structure accessors inline.
  The layout object a closure holds and a call compares (EQ) is a symbol
  named after it (`key-layout-object`), so that a call read from a fasl
  finds it too, and `compile-file` lays out the calls of a function defined
  earlier in the file (`*compiled-key-layouts*`): such calls in a fasl took
  twice as long as in loaded code.
* **CLOS**: generic functions dispatch and combine methods without argument
  lists when they have one to four *spread arguments*: the required ones,
  the optional ones (the unbound marker for one not given) and, with
  `&rest` or `&key`, the list of the arguments after them (`%fast-shape`,
  `%gf-scall`); the methods are functions of those too, and a generic
  function with `&key` checks the keywords of a call only when there are
  some, from its own dispatch (one pass with `eq`). Its function also has a
  *keyed main function* (`%gf-keyed-entry`) with the layout (required,
  optional, then its keywords): a call compiled in the image or into a fasl
  with constant keywords passes them in its slots, as for ordinary
  functions, and it calls a keyed effective method of four arguments made
  from methods that are known as functions of them (`%keyed-method-form`:
  a method finds its keywords by their slot among the generic function's,
  `call-next-method` passes all four on); a method with `&rest`,
  `call-next-method` with arguments or as a function, another method
  combination, or more than four arguments in all fall back to the list of
  the keyword arguments. `compile-file` knows the layout of the generic
  functions of the file before they are made (`*note-key-layout-hook*`).
  Calls with one or two keywords take 0.2–0.3 of their earlier time in
  loaded code and fasls (`k-key-*` in scratch tests: about SBCL's time).
  The dispatch (`%gf-callN`, `%gf-scall`, macros) is copied into the
  function of each generic function, also when that is compiled in the
  image or into a fasl, where the engine does not inline the runtime's
  functions: once for the call of the required arguments alone (without
  keyword parameters, also for the calls with the optional arguments and
  up to two rest arguments), and once into the *core* (`+gf-core+`,
  `%gf-spread-core`) that the other entries call (and once into the keyed
  main function); the readers and writers
  that `defclass` defines have none (`%define-slot-accessor`), their call
  sites keeping the slot locations. `call-next-method` is a direct call;
  call sites cache the functions for the classes of their arguments and
  read or write slots of readers and writers inline (`compile-file`
  defines their compiler macro for the rest of the file, and a loaded file
  defines it again when the form that set its flag was expanded but not
  evaluated, which left the call sites of `clysm script` without caches): the class of an
  instance is compared with four entries and the slot read at the cached
  location, without an epoch read at each call (`%bump-clos-epoch` empties
  the caches of the sites that have been used, `*gf-sites*`); readers and
  writers at about SBCL's time (`r-*` in scratch tests, fasl 0.7 of the
  earlier time); `slot-value` with a constant name and `make-instance` with a
  constant class use caches at the call site. A constructor copies a
  template of the instance's slots holding the values of constant
  initforms, then stores the arguments and the other initforms; the
  effective methods of `initialize-instance` and `shared-initialize` are
  called with the instance and the list of initargs. The `append`, `nconc`,
  `max` and `min` method combinations of two or three methods make no list
  of the values.
* **Non-local exits** (`handler-case`, `restart-case`, `catch`/`throw`,
  `return-from` and `go` out of closures, `error` under a handler any
  number of calls away) are returns with a pending exit, not Wasm
  exceptions; special bindings are inline.
* **Numbers**: fixnum, 64-bit, single and double arithmetic and comparisons
  in Wasm helpers; `floor` and friends, `isqrt`, `gcd`, `ldb`, the
  irrational functions as primitives; bignum kernels in Wasm: products by
  columns (two accumulators, no carry chain from product to product; a
  square takes each cross product once), Knuth's division, Lehmer's gcd
  (one pass over the digits per 30 bits of quotients, as CPython's), the
  remainder by a divisor below 2^32 without a quotient, decimal printing by
  10^9 a pass straight into the string; `expt` of an integer from the top
  bit down.
* **Library**: sequence functions with one implementation per family and
  direct loops over lists, simple vectors and strings; hash tables for
  `remove-duplicates` and the set functions; `typep` with a constant type,
  `format` with a constant control string (all directives), `coerce` and
  `parse-integer` compiled at the call site; string streams as buffers
  (`write-char` to one is a store at the call site). The mapping
  functions on lists (any number of lists), and `map` (to `nil` or a list)
  and the quantifiers over one sequence, with a function written in place
  are loops at the call site, where `#'name` is a direct call (`(mapcar #'+
  a b)` adds inline) and a lambda is inline. Other mapping calls stay out of
  line with `#'name` passed as a lambda of known arity (no `&rest` entry for
  `max`): a function holding several inline loops ran slower than with the
  calls (`p-seq-vector` 1.3-1.45), because V8 tiers a function up as a whole
  and inlines call targets by feedback. `concatenate` of two or three
  strings, `string-upcase`/`-downcase` and `parse-integer` are direct;
  `format` compiles `~<...~:>` on an argument to `pprint-logical-block`;
  the reader reads string streams without the accessors' type tests;
  `find-package` of a symbol remembers the last one found. `gethash` and
  `(setf gethash)` of a fixnum, a character or a symbol find the entry with
  one Wasm helper (`ht-fast-index`), which casts the table's arrays once
  (a cast at each access cost about a third of the time); `gethash` reads
  the value at the call site: a character key in an EQL table is looked
  up as fast as in SBCL. String comparisons with literal keywords take no
  keyword list; the digits of a float the host makes (a double of 16-17
  digits) cross in groups of nine (`float_part`). Hash tables are
  iterated in place (`maphash`, `loop ... hash-keys`,
  `with-hash-table-iterator` read the key and value vectors), `(setf
  gethash)` right after `gethash` of the same key in an `equal`/`equalp`
  table does not hash it again (`(incf (gethash k h 0))`), and a table
  with removed entries compacts in place. The pretty printer prints a
  list or vector of data (numbers, characters, symbols, strings and lists
  and vectors of them) in the fill style without a pretty stream: each
  element plainly, a line break before an element that does not fit, as
  Waters' algorithm decides when no element breaks inside
  (`%pp-fast-fill`, checked against the pretty stream on random trees);
  a structure that fits on the line is printed plainly.
  `string=` and `string-equal` with keyword arguments call the comparison
  with its bounds directly instead of checking keywords (a function of
  four arguments when only one string has bounds), which compares two
  simple strings in a loop of its own.

### Size and start-up

A whole program carries the runtime: `tests/hello.lisp` is 4.3 MB (1.25 MB
with gzip; the tests after calls for a pending exit, "Non-local exits",
account for 8% of it). With `--shake` (`bin/clysm compile --shake`, `bin/clysm library
--shake`) the global functions that nothing refers to by name are left out
(`shake-module` in `src/wasm.lisp`): the definition of each function is a
small Wasm function `(:install SYMBOL N)`, kept only when the symbol is
reached from the exports, the start function or the initializer of a reached
global, and the functions and globals that are then unreachable are dropped.
Hello becomes 3.6 MB, and a library starts in about 50 ms
instead of 65 (12 ms to compile the module, the rest the toplevel forms of
the runtime). Every symbol stays, so `intern` and `read` work, but a function
called only by a name made at run time (`(funcall (intern ...))`, `eval` of
new code, `~/name/` in a control string made at run time) is not there.
`SHAKE=1 tools/run-tests.sh` runs the test suite this way. What remains is
mostly reached through generic paths (the printer, `format`, CLOS dispatch,
conditions) and the names of the symbols (the global section, 0.6 MB).
For pages, the module is best served as a file of its own, which the browser
caches for every page that uses it.

`tools/prof.sh FILE` (and `tools/prof-script.sh` for a file loaded into the
image) profile a program with the V8 CPU profiler; `tools/dump-func.lisp FILE
NAME...` prints the Wasm generated for functions. For code compiled in the
image (a fasl, the REPL, `load`), `(setq clysm::*live-dump* '("NAME" ...))`
prints the Wasm of the functions of those names to `*trace-output*` as they
are compiled (`compile-file` the file again to see them).

### Start-up and loading

`tools/bench-startup.sh` (through `tools/bench-slot.sh`) times whole
processes against SBCL, best of five: starting the REPL image (`--eval t`),
`(require :asdf)`, loading `~/quicklisp/setup.lisp`, `ql:quickload` of
Alexandria, cl-ppcre and Babel whose fasls are already compiled, and a
small script (`clysm script` of a `defun` and a call). Its fasls go to a
cache of its own (`build/bench-cache/`, as `$XDG_CACHE_HOME`).

Measured 2026-11 on a slot of four CPUs (`tools/bench-slot.sh`), best of
five, milliseconds; "before" is main before this work (2026-11-20 morning),
"main" after the other start-up changes of that day (simple calls evaluated,
fewer globals and helpers in each module), "now" this work on top of it.
The machine was busy and runs differ by 10-20%; compare within a column
group, which ran one after the other on one slot:

| Item | SBCL | Before | Now | SBCL | Main | Now | Now, no heap image |
|---|---|---|---|---|---|---|---|
| `s-start` (`--eval t`) | 4 | 416 | 158 | 6 | 263 | 144 | 179 |
| `s-script` (a `defun` and a call) | 7 | 431 | 232 | 6 | 584 | 265 | 325 |
| `s-require-asdf` | 62 | 862 | 439 | 68 | 619 | 543 | 521 |
| `s-quicklisp` (`setup.lisp`) | 157 | 1301 | 715 | 185 | 1169 | 803 | 877 |
| `s-ql-alexandria` | 179 | 1454 | 791 | 192 | 1127 | 935 | 969 |
| `s-ql-cl-ppcre` | 223 | 1425 | 782 | 248 | 1368 | 983 | 1025 |
| `s-ql-babel` | 251 | 1720 | 854 | 300 | 1514 | 1074 | 1181 |
| geometric mean / SBCL | | 15.8 | 8.1 | | 12.2 | 8.8 | 9.2 |

Where the time went, and what was done:

* **Compiling a form when evaluating it would do.** The REPL compiled every
  form, `--eval t` and `(load "file")` included: the first compilation
  costs the module and V8's compilation of the compiler's functions as they
  are first called, 0.2 s. Simple forms are evaluated (see Compiling inside
  the running image); `compile-file` evaluates its compile-time forms
  (`in-package`, `defvar`'s proclamations...) the same way.
* **Locals.** Liftoff, which compiles each function when it is first
  called, takes time in the number of locals at every branch. The compiler's
  own large functions had up to 2000 locals (`emit-instr` 2038 locals took
  31 ms, `encode-module` 16, ten functions together 85 ms of the first
  compilation). `coalesce-locals` (`src/wasm.lisp`, when a function is
  encoded) gives each local the range of positions from its first to its
  last use, widened to every loop that the range enters or leaves, and
  locals of the same type whose ranges do not meet share one; a local that
  may be read before it is set (by Wasm's rule for non-nullable locals: a
  set counts until the end of its block) keeps its own, as parameters do.
  `emit-instr` has 8 locals now, and the image is 3% smaller.
* **The worker.** A V8 flag set before the worker starts (in the main
  thread or on the command line) changes the flags' hash, and Node.js then
  compiles its own JavaScript again in the worker instead of taking it from
  its code cache: 35 ms. The flags are set in the worker once it has
  started. The main thread imports only what it needs to start the worker
  (sockets and processes when the worker first asks for one).
* **Validation.** `--wasm-lazy-validation`: a function is validated when it
  is first compiled. Most of the image's functions and of a fasl's never
  run; the image compiles in 17 ms instead of 30.
* **The REPL image's toplevel forms** (making the packages, symbols,
  classes, readtables of the runtime and the compiler: 45 ms, half of it V8
  compiling the functions they call) run once: `bin/clysm` saves a heap
  image of the REPL image after them, `build/clysm-native.img` (1.8 MB,
  29000 objects; made the first time the image is used after it is built,
  0.6 s), and `repl`, `script`, `eval` and `swank` start from it
  (`CLYSM_IMAGE`; see Heap images), unless `CLYSM_IMAGE` names another,
  `CLYSM_SAVE_IMAGE` is set (an image started from a heap image cannot save
  one) or `CLYSM_NO_HEAP_IMAGE` is. Making the objects again takes 12 ms.
  `save-image` of an image that compiled no module is allowed now (the
  host says -1 modules when it does not record them).

What remains for `--eval t` (about 0.15 s): Node.js and the worker 45 ms,
compiling (17 ms) and instantiating (25 ms) the image, making the heap
image's objects (12 ms), V8 compiling the functions that run. For
`(require :asdf)`, 0.3 s more: compiling its 1064 modules (70 ms, most of
it each module's copy of the image's types; linked into one, `host/link.mjs`,
they compile in 20 ms), instantiating them (18 ms), reading the fasl's text
(37 ms), running ASDF's toplevel forms (UIOP's `define-package` 62 ms, most of it
hashing strings in `equal` hash tables: `%sxhash` 11 ms, `gethash` 7,
`%ht-find-general` 11, `find-symbol` 5).

Tried and not kept: compilation hints (`withCompilationHints`, the
functions that run at start compiled eagerly in background threads) are
not used by Node.js 24's V8 (13.6; `--experimental-wasm-compilation-hints`
changed nothing). V8 has no cache of compiled WebAssembly that Node.js
exposes (`v8.serialize` of a `WebAssembly.Module` gives 2 octets).

### Allocation, garbage collection and the engine's flags

Objects are V8 heap objects. Node.js is built without pointer compression,
so every Wasm struct has two 8-octet header words (map, hash): a cons takes
32 octets (SBCL: 16; measured), a boxed double 24, an instance about 40 plus its slot
vector (24 + 8 per slot). Nothing in the module can make these smaller.

Measured in 2026-11 (Node.js 24.21, V8 13.6; four CPUs; `bench/alloc.lisp`,
written for it: binary trees, trees of structures, lists copied over and over,
closures, boxed doubles, small vectors, and data that survives: 3 million
pushed conses, a hash table of 500000 lists, 300000 strings in a vector):

* **Garbage that dies young is cheap**: binary trees of depth 18 take 1.1–1.4
  times SBCL's time, the others 0.8–2.1 (small vectors 2.1, closures 1.7). Scavenges stay under
  12 ms (2.3 ms on `bench/gabriel.lisp`), full collections under 7 ms.
* **Data that survives is not**: building a list of 3 million conses that
  all stay alive is 4–7 times SBCL's time, almost all of it in scavenges
  that copy each object twice (into the other semi-space, then to the old
  generation), 50–100 ms each once the young generation has grown to its
  64 MB; full collections of a 400 MB heap take about 80 ms. Those are the
  longest pauses an application (an editor) sees.
* **Walking a large structure** (a tree of 2 million conses, 64 MB) is
  4–6 times SBCL's time; the same walk over a tree that fits in the cache
  is 0.9–1.5. The objects are twice as large, and probably no longer in the
  order they were made in once the scavenger has copied them.
* Run-to-run spread on this machine is large: the same program, with the
  same tier-up and inlining decisions (`--trace-wasm-inlining`), runs a
  small tree walk in 20 or in 38 ms. Flags were compared with interleaved
  runs, the best of four to six of each.

The engine's defaults were kept; each alternative was measured against them
(ratio of geometric means, below 1 is faster):

| Flag | Allocation probes | `suite` | `gabriel` | Surviving data |
|---|---|---|---|---|
| `--minor-ms` (young generation marked and swept, not copied) | 0.99–1.01 | 0.99–1.03 (`b-deriv` 1.25) | 0.97 | 0.82–0.93 (the list 0.67–0.9) |
| `--max-semi-space-size=32` | 1.04–1.14 | | | |
| `--max-semi-space-size=64` | 1.14 | | | |
| `--max-semi-space-size=8` (or 4) | 1.35–1.5 | | | |
| `--scavenger-max-new-space-capacity-mb=16` | | 1.00 | | 1.05 |
| `--single-threaded-gc` | 1.4 (binary trees 2.5) | | | |
| `--wasm-deopt`, larger `--wasm-inlining-budget` | 1.03–1.06 | | | |
| `--no-liftoff` | 1.15 (binary trees 1.3) | | | |

`--minor-ms` would help programs that build large data, but code whose
garbage all dies young (`b-deriv`, the common case) is 25% slower with it
whatever the size of the young generation, and it cannot be set from
`host/run.mjs` (`v8.setFlagsFromString('--minor-ms')` crashes the process
at the first collection) nor through `NODE_OPTIONS`: every `node` command
would need it. Startup: `--no-liftoff` makes `clysm eval t` take over 16 s
(everything compiled by TurboFan), `--no-wasm-lazy-compilation` does not
start the image at all. `clysm eval --no-userinit t`: see Start-up and loading.

Types and casts: every type is final (a type without `sub` in the binary
format is final), so `ref.test` and `ref.cast` are one comparison of the
map. Running `bench/suite.lisp` with
`--experimental-wasm-assume-ref-cast-succeeds` (every cast unchecked) is
6% faster, `--experimental-wasm-skip-null-checks` not at all: that is the
most that removing casts can give.

### Lem: where the time goes when it is used

`tools/lem-scenario.sh [LEM-DIR]` runs Lem on Clysm (cxxxr/lem's
`frontends/clysm/`, built with `CLYSM_DIR=$PWD frontends/clysm/build.sh`)
in its Electron shell without a display and drives it as a person would
(`tests/lem/scenario.js`): it opens a copy of `src/prims.lisp` (7000 lines),
scrolls it page by page, moves line by line, types 200 characters of Lisp
in the middle of a function (Return indents, a space asks for the argument
list), deletes, searches incrementally and types two commands after `M-x`.
A key's latency is from its `keydown` to the last change of the page before
the page is quiet for 50 ms. `LEM_PROFILE=1` records a CPU profile per step
and a trace (GC, style, layout, paint, V8's compilation), which
`node tools/lem-profile.mjs DIR` sorts by kind and function
(`LEM_CALLERS=NAME` shows where a function is called from).
`tools/bench-lem.sh` (run under `/tmp/clysm-bench-slot.sh` or
`taskset`) times Lem's own code without a display in SBCL and in Clysm,
and prints the ratios as `tools/bench.sh` does
(`tests/lem/buffer-bench.lisp`, loaded by `tests/lem/load-lem.lisp`): the
actions that the scenario found to take the time, on `src/prims.lisp` in a
buffer of Lem's core with the Lisp mode's syntax table and grammar and the
fake display of Lem's tests. It needs a checkout of cxxxr/lem with micros
and lem-mailbox under `build/clysm/deps/`, and Quicklisp with Lem's
libraries; Lem does not have to be built. `tools/lem-setup.sh [DEST]`
makes one once, by default in `$HOME/src/clysm-lem`, outside any checkout
of Clysm (from `LEM_SRC`, a checkout that has them, cloned with `git clone
--local`, else from GitHub), and `bench-lem.sh` takes `LEM_DIR`, else
`build/lem`, else that one; `DEST/frontends/clysm/build-offline.sh` (with
`CLYSM_DIR`) builds the desktop application there for the scenario,
without downloading. Clysm loads the systems as fasls into the REPL image
(compiled the first time for each build of the image), SBCL the same
systems with the same stubs (`frontends/clysm/stubs/`).

`tools/bench-libs.sh bench/apps.lisp` is a second comparator of the same
kind, with other libraries' real work: JSON read and written by yason and
by cl-json, a PEG grammar of arithmetic with esrap, dates parsed, moved and
printed by local-time, and `src/prims.lisp` cut into tokens by a regular
expression of cl-ppcre.

Measured (Linux, 4 CPUs of the machine, means of three runs):

| Step | Before | Now |
|---|---|---|
| start to the first frame | 0.55-0.75 s | 0.65-0.80 s |
| open the first Lisp file (loads the Lisp mode), until highlighted | 1.52 s | 0.94 s |
| a page down (`C-v`), median / max | 13 ms / 1.5 s (the first, in two runs of three: the Lisp mode was still loading) | 11 / 24 ms |
| a line down (`C-n`) | 4-5 ms | 4-5 ms |
| a key typed in Lisp mode, median / mean | 5 / 20.5 ms | 4.5 / 18.5 ms |
| the slowest key typed in Lisp mode (a space: autodoc) | 184-233 ms | 133-217 ms |
| a key of `C-s`, of `M-x` | 8-10, 5 ms | 8-9, 5 ms |

("Before" is the image as it was, with only `save-image` repaired.)

Where the time of a key goes: Lisp code 55-85 %, the browser drawing the
page (style, layout, paint) 15-40 % (6 ms of a page down), GC under 1 %
(the scavenges on the main thread: 0.5 ms a key), the JavaScript of the FFI
under 1 % (strings cross in the linear memory). Of the Lisp code of a page
down (9 ms), half is Lem's redisplay and a third its syntax highlighting of
the new lines; the GUI framework's update of the DOM is 1.7 ms. A space in
Lisp mode runs Lem's `parse-for-autodoc`, which scans the rest of the
function forward: 100-130 ms, the slowest keys of the scenario.
`tools/bench-lem.sh` gives the same code to SBCL (a 4-CPU slot, best of
three): geometric mean 3.00 times SBCL's time at first, 2.45 with
`string=` with keywords a direct call, `gethash` of characters and fixnums
in a Wasm helper, and inline declarations followed across fasls (autodoc
parses 206 against 608 ms, indenting 51 against 157, `form-offset` over
every form 154 against 441, highlighting the whole file 124 against 300,
`string-width` of every line 86 against 130, a search 27 against 59, 150
pages down 49 against 93). `tools/bench-libs.sh bench/apps.lisp`: 2.26
(cl-json 1.4-1.9, yason 2.0-2.2, cl-ppcre's tokens 2.7, local-time 2.9,
esrap 3.2). In the editor, the slowest key typed in Lisp mode now takes
60-72 ms (184-233 at first). What remains is spread: the call sites of the
readers of Lem's classes `line` and `point` (`line-length`,
`%move-to-position`, `point-change-line`, `%point<`), `char-width`, the
closures that `character-offset` passes to the function it inlines,
untyped `=` and `<` of fixnums. (On 2 CPUs the autodoc parses took 1270 ms in the editor, SBCL's still 234:
V8 needs CPUs besides the main thread's for its compilers and its GC.) The time is
spread over Lem's buffer functions (`%character-offset`,
`%skip-list-forward`, `line-length`: generic readers of the CLOS class
`line`, `length` of its string, `gethash` of characters in `char-width`).
Opening the first Lisp file is loading 1800 modules of fasls: V8 compiling
them (0.2 s) and their functions when first called (0.4 s of lazy
compilation on the main thread), reading the fasls' forms (0.15 s).
Compiling eagerly (`--no-wasm-lazy-compilation`) made it slower (1.2 s) and
the start 1.5 to 2.3 s.

## Building and distributing

`bin/clysm` needs SBCL, which hosts the cross compiler. `tools/build-npm.sh`
makes an npm package (`build/npm/`, packed as `build/clysm-VERSION.tgz`)
that needs only Node.js: the compiler compiled by itself (`clysmc.wasm`)
and the REPL image are prebuilt, and its command `clysm` (`bin/clysm.mjs`)
runs the compiler on Node.js (`clysm compile`, `library`, `run`, `test`,
`repl`). Its `version` is 0.1.N, N the number of commits, and the commit is
in `package.json`; a project pins Clysm by depending on one such package.
The compiler in Wasm takes about twice as long as on SBCL (hello: 4.9 s, 2.7 s).

A program is fastest compiled as a whole (calls between its functions are
direct and small functions are copied into their callers); files compiled
one by one (`compile-file`, ASDF in the image) are 1.8 to 2.9 times slower.
`--system NAME` (`bin/clysm.mjs test`, and `tools/system-files.lisp`, which
prints the files) compiles an ASDF system with the systems it depends on as
one program: ASDF on SBCL finds the files in their load order, reading the
system definitions with the feature `:clysm` (and without `:sbcl`), and
looks in `contrib/` before the source registry and Quicklisp. Compiling
takes about 2 seconds for the runtime plus the program's own code.

### Where functions are, in stack traces

The compiler records the file and line of each toplevel form, and the
module's custom section `clysm.sources` maps each function to the place of
the form that defines it. The hosts read it (`installSourceMap` in
`host/common.mjs`): the stack of a `LispError`, the backtrace printed for an
unhandled error, and JavaScript errors that pass through Lisp name the Lisp
frames `at NAME (file.lisp:LINE)`, the format of JavaScript's own frames,
which error trackers such as Sentry read. Files under the Clysm installation
or the working directory are named relative to it. `lib.mapStack(stack)`
(host/lib.mjs) does the same for any stack. Functions copied into their
callers and tail calls leave no frame of their own.

### Tests of programs

`contrib/clysm-test` is a small test library (`deftest`, `is`, `is-equal`,
`signals`, `skip`, `run-tests`, `run-tests-and-exit`), and `bin/clysm.mjs
test` a runner: each file is a test program, compiled as a whole after the
files of `--system`, run, and passed when it exits with status 0. Files run
in parallel (`-j N`), `--watch` runs them again when a `.lisp` or `.asd`
file of theirs changes, and `--dom` gives the programs `window` and
`document` from the project's `happy-dom`, for code that works on the DOM.
The GUI framework has a backend without a DOM for its own tests
(`tests/ui`). Code compiled as a library can also be tested from JavaScript
(vitest, `node:test`) through its exports.

```
$ node bin/clysm.mjs test --system my-app-tests tests/*.lisp
ok   tests/outline.lisp
FAIL tests/layout.lisp (exit 1)
  FAIL wide-rows
    (LAYOUT-WIDTH ROWS) is 120, expected 100
1 passed, 1 failed (3.1 s)
```

`tests/system/run.sh` tests the runner with a small system.

### Programs that depend on Clysm

Clysm changes every day, so a program written in it pins a commit (the npm
package of that commit; `lisp-implementation-version` names it) and moves
the pin when its tests pass with a newer one. The other way round,
`tests/downstream/projects.txt` lists such programs with a commit and a
test command, and `tools/test-downstream.sh` (`make test-downstream`) clones
each at its commit and runs the command with this checkout of Clysm
(`$CLYSM`), so that a change that breaks one of them is found in Clysm. A
failure there is told apart from one of the program by running the same
command with the pinned Clysm.

## Tests

`make test-ui` tests the GUI framework on its S-expression backend
(`tests/ui/ui.lisp`, and `tests/ui/pane.lisp` for the panes, presentations
and dialogs), compiled ahead of time and in the REPL image.

`tools/run-tests.sh` compiles every `tests/cases/*.lisp`, runs it, and compares
standard output with SBCL running the same file. `INTERP=1` runs the files with
the in-image evaluator instead, and `SELFHOST=1` compiles them with the
Wasm-hosted compiler.
