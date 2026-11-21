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
| `host/run.mjs` | Node.js host: I/O imports, runs the module in a worker with a large stack (an image started from a heap image: in the main thread) |
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
  A **structure** (`defstruct` without `:type`) of 1 to 32 slots keeps them
  in fields of its own: it is a `$siK` (K slots), the `$instance` fields (its
  slot vector is the vector of the structures its class includes, by depth,
  the class's `+class-inherits+`, put there by the constructor:
  `:layout-inherits`; the shared empty vector for one made otherwise, such
  as a heap image's) then K mutable fields, and `$siK` is a
  subtype of `$si(K-1)`, the first of `$instance` (`src/layout.lisp`). An
  accessor of a structure of N slots tests for `$siN` (exact for its own
  instances, a subtype for those of the structures including it) and the
  layout, else (an instance of a structure including it) the class at its
  depth in the instance's slot vector, and reads the field; a constructor
  is one `struct.new`. Generic code tests `$sfield` before using the slot
  vector of an instance (`:instance-ref`). A structure of more than 32
  slots that includes one of field slots is *big*: a `$si32` whose slot
  vector is the vector of the structures its class includes followed by an
  overflow vector of its slots past the 32nd (`%make-big-struct`; generic
  code reaches those slots through `:big-overflow`, `:sinst-size` counts
  them, copies and heap images keep them), so that every instance of a
  structure of field slots, or of one including it, is of its Wasm type
  and its accessors on an object known to be of its class are a bare cast
  (`known-struct-check-code`). Other larger structures, structures that
  include one of a slot vector, and the streams (`(sys::%slot-vector)`,
  read by fast paths that index the vector) keep a slot vector; so may the
  instances of a structure of typed slots (below: made by the interpreter,
  restored from a heap image, of a structure of more than 32 slots
  including it), whose accessors test the Wasm type even where the class
  is known. Generic code reads slot I of any instance with
  `%instance-gref` (the slot vector, else field I through a `br_table` on
  I); `%instance-ref` is for instances known to have a slot vector. A
  structure class keeps its included structures by depth in
  `+class-inherits+`, so `typep` and the accessors of an included structure
  test in constant time (`inherits-depth-check-code`); a structure's
  *shape* (slot count + 64 × depth) is a compile-time constant given to the
  accessors and predicates.
* **closures** — see below. **foreign objects** — `struct { externref }` wrapper.

All types live in one recursive type group, so structurally identical types
(e.g. strings vs. bignum digit arrays) remain distinct for `ref.test`. Type
predicates compile to `ref.test`, never to pointer tag arithmetic.

## Calling convention (spec: ハイブリッド渡し)

A closure is `struct { code, name, free variables... }`, where `code` is a
`$code` struct `{ entry0..entry4, entryN, kmain, klayout, entryA }` made once per
lambda and shared by all its closures (`static-closure-global` in
`src/codegen.lisp`: the closure without environment of a lambda is a global
whose code a new closure takes). A call with k ≤ 4
arguments uses `call_ref` on `code.entry_k` with the arguments as native Wasm
parameters; more arguments, `apply`, and `multiple-value-call` use `entryN`,
which takes an argument array. For a function with only required parameters the
matching entry *is* the function body; the other fixed entries are shared
argument-count-error stubs, and `entryN` is a small unpacking adapter. Functions
with `&optional/&rest/&key` take the argument array in `entryN`; their fixed
entries take the arguments as parameters too: a function of required and
optional parameters has an entry for each number of arguments it accepts
that passes them to its keyed main function (`kmain`, one parameter per
slot, null for a missing optional), and the entry for all of them *is* that
function; one of required parameters and `&rest` conses the rest list and
calls its main function; one with `&key` (and no `&rest`) has an entry for
each number of arguments from its required ones up to four
(`keyed-entry-body`, `(:kentry id k)`) that compares each key with its
keywords and passes the values in the slots of `kmain`, the first of a
repeated key winning, and goes to the generic entry (which signals the
error, or allows the keys) for an odd number of key arguments, a key it
does not take or `:allow-other-keys`. (The fixed entries of keyed functions
were shared stubs that made the argument vector for the generic entry,
which then searched it for each key: `(funcall f x :y 1)` of such a
function, or a call through the symbol of one the compiler did not know,
took three times SBCL's time, 1.1 now.) A global function of the runtime
taking any number of arguments or keywords can also have, for one number of
arguments, the main function of another that does the same as its entry
(`*arity-entries*` in `src/codegen.lisp`: `max`, `min`, `append`, `gcd`,
`lcm`, the character and string comparisons of two arguments): `(funcall
#'max a b)`, `(reduce #'max list)`, a `:test #'char-equal` given to a
function of the image make no rest list or keyword search (`reduce #'max`
3.6 → 1.1 times SBCL, `funcall` of `max`, `min`, `string<` 2.2–2.4 → 1.0);
`+`, `-`, `*`, `/`, the numeric comparisons and `logand`/`logior`/`logxor`
get theirs from `%fast-entries` (`runtime/numbers.lisp`), whose closure now
keeps the function's entry for `apply` and its main function taking the rest
list (`(apply #'+ list)` takes the list as the rest list: `(apply f x list)`
of a symbol naming `+` 43 → 24 ms in a probe, SBCL 13).
`multiple-value-call` with one argument form that can give several values,
the others constants or variables, calls the function's fixed entry for the
number of values when it is at most four, with them from the
multiple-value buffer (`cg-mvcall-fixed`); else, as for more forms, the
values are collected in a list and then a vector for `entryN` (a probe of
`(multiple-value-call f (floor i 3) s)` 82 → 33 ms, SBCL 13; `+` of three
arguments still conses its rest list). Calls to functions defined in the same program
are compiled as direct `call`s (CLHS 3.2.2.3), and tail calls become
`return_call` / `return_call_ref`. `apply` calls `entryA`, which takes the
arguments as a list: for a function of required parameters and `&rest` it
takes the required ones off the list and passes the rest of the list as the
rest list (CLHS 3.4.1.3 lets it share structure with the last argument of
`apply`), with no argument vector and no copy; for the others the shared
`:apply-generic` takes up to four elements to the fixed entries. `apply` of
a known global function calls its `entryA` directly, and one of required
parameters and `&rest` whose required parameters the spread arguments of
`(apply f a b ... list)` fill is called with them, only the spread
arguments beyond them consed onto the list. In the image, where global
functions are not known, the code of such a function has its main function
taking the rest list in the `kmain` field and a rest layout
(`%REST-LAYOUT n`, a symbol) in `klayout`: `(apply #'f a b list)` of a
function whose layout the compiler finds (`*live-key-layout-hook*`, or a
definition in the same compilation) compares the layout of the symbol's
current function and calls its main function the same way, or makes the
list for its `entryA` when the function has changed (`live-apply-code`). `funcall` of an object not known to be a
function tests at the call site for a closure, then for a symbol whose
function cell is set; only other objects go to the `:coerce-function` helper.

A function of 5 to 12 required parameters only (`+max-req-layout-args+`)
has its main function, which takes them as Wasm parameters, in the `kmain`
field and a "req layout" (`%REQ-LAYOUT n`, a symbol, `req-layout-object`) in
`klayout`; a call of n arguments of a function that is not known where it
is compiled (through the symbol of another file's function, `funcall`)
compares the layout and calls the main function (`call-closure-code`), and
only otherwise makes the argument vector for `entryN`. A cross-file call of
five arguments 10.3 → 8 ns (four: 7.1, SBCL 4.5 for both); esrap's `recall`
(five arguments, called for each rule) no longer allocates.

Raw clones (`raw-call-code` in `src/codegen.lisp`): a direct call of a
known function of required parameters with arguments known to be fixnums,
doubles, or integers within 64 bits computed as i64s (a mask, a variable
declared `(unsigned-byte 32)`, a call of a copy returning an i64; not one
read from data, which the copy would box again: `i64-natural-node-p`)
(`spec-signature`) calls a copy of it, `(:raw id sig)`, that
takes those arguments as `i32` / `f64` / `i64` parameters (the others as eqrefs).
When that copy cannot return with an exit pending (`lam-may-exit-p`), and
the values of its tail positions, its tail calls of itself with arguments
of the same classes left aside, are all fixnums (or all doubles, or all
integers within 64 bits), with its
types inferred for those arguments (`raw-clone-rrep`), it returns an `i32`
(an `f64`, an `i64`) and sets no `$mv-count`, and those tail calls of itself are
`return_call`s of itself. A tail call of another known function whose raw
copy for the classes of its arguments returns the same (`i32` or `f64`) is
a `return_call` of that copy, and its value counts as a fixnum (double)
leaf; any other call in a tail position would not stay a tail call, so a
function with one keeps an eqref result (its copy for raw arguments is then
a `return_call` target like the main function; mutually recursive functions
whose raw results depend on each other keep eqref results too, as the copy
being analysed counts as returning an eqref). A raw copy returning an
`i32` or `f64` of a function without free variables that does not refer
to itself as a closure takes no closure parameter (`raw-clone-closure-p`;
its locals are numbered one lower, `shift-locals-down`), so its callers do
not load the closure's global and a recursive one does not keep it across
its calls (`d-tak` 334 → 320 ms). The copies returning an eqref keep it:
without it for them too, `b-call` (which calls none of them) ran 150 →
178 ms, `length` no longer inlined by V8 into its caller, a change of the
engine's decisions over the module that was not explained. `lam-may-exit-p` takes `(length v)` of a variable
declared a simple vector or string (`inline-length-call-p`) for the
`array.len` that it is compiled to, not a call that may exit, so a
function like `triangle-count` (a `dotimes` over a declared vector) gets a
raw result. A call of a copy with a raw result is made only where one value
is wanted and not in a tail position of a function returning an eqref;
type inference gives such a call the fixnum or double type
(`raw-call-return-ty`), so the arithmetic around it stays unboxed. `tak`'s
recursive calls are `local.get`s, an `i32.sub` and a `call`, with no
`ref.i31`, `i31.get_s` or store of `$mv-count`.

Closure conversion: the values of free variables are fields of the closure
itself, in a final subtype of `$closure` per number of them (`:envclo1` …
`:envclo8`, `envclo-type` in `src/layout.lisp`; more than eight are in a
vector, `:envclov`); the function casts its closure parameter to that type
once on entry. Making a closure is thus one allocation of 8 + 4·(2 + n)
bytes: the code from the lambda's global, the name, the values. (The
closure was `struct { entry0..entry4, entryN, env, name, kmain, klayout }`
with the values in a separate vector: six `ref.func` and two allocations of
72 bytes in all for two variables; continuation-passing code and other
combinator-style code ran 1.2–2 times faster after the change.) Variables
that are both captured and assigned are boxed in cells. `labels` closures
are created first and their free variables set afterwards. The few runtime
operations that take closures of any shape (`%make-gf-closure`, heap
images: `:closure-with-code`, `:closure-env-length`, `:closure-env-ref`,
`:closure-env-set`, `:make-env-closure` in `src/prims.lisp`) test the
subtypes one after the other.

## Multiple values (spec: 多値リターン用バッファ)

The primary value is the Wasm return value. A global `mv-count` and a global
value buffer hold the full set; `values` fills them, and every non-call
expression in a multiple-value context sets the count to 1. Calls in tail
position pass values through untouched.

Every return of a single value in a multiple-value context (the end of each
function's body) stores 1 into the count. Measured before changing the
convention for it (2026-11-20): the bodies of the functions of the
benchmarks compiled without that store (and so with multiple values lost,
an upper bound of any convention that leaves it out: callers resetting the
count, a second entry for multiple-value contexts), best of three
interleaved runs on one 4-CPU slot: whole programs `bench/gabriel.lisp`
1.00 and `bench/closures.lisp` 0.99 of the time (only `k-cps-*` 0.91-0.93);
loaded into the image (an imported global) `bench/closures.lisp` 0.98,
`bench/gabriel.lisp` 0.99, `bench/suite.lisp` 0.99 (`b-fib` 0.90, `b-call`
1.04). The store is one write of a global the engine keeps no value of
across calls anyway; what a real convention would add back (a reset after
calls whose values are dropped, or a second copy of every function) would
take most of that 1-2%, so the convention was not changed.

`multiple-value-call` (and `multiple-value-list`, `nth-value` of more than
a few values, which expand to it) of forms whose number of values is known
when the form is compiled (`fixed-mvcall-form` in `src/frontend.lisp`: a
constant or a variable, `(values ...)`, a standard function of a fixed
number of values such as `floor` or `gethash` (`*cl-nvalues*`), or a call
of a function of the same file every tail of whose body returns the same
number of values, `ast-nvalues`) is rewritten into `multiple-value-bind`s
of the values and a `funcall` of them: `(multiple-value-call #'+ (f x) (f
y))` with `f` returning three values is an inline addition of six
variables, `(multiple-value-list (f x))` a `list` of three. Other forms
append their values to one vector (`:mv-append`) given to the function's
entry of any number of arguments; they were collected in a list, reversed
and copied into the vector (three allocations per value). Measured
(2026-11-21, `p-call-conv`'s lines one by one): `(multiple-value-call #'+
(mv-fn i) (mv-fn 1))` 76 → 12 ms (SBCL 20), `(length (multiple-value-list
(mv-fn i)))` 31 → 12 (SBCL 18).

## Dynamic binding (spec: シャローバインディング)

The symbol's value slot holds the current value; binding pushes the old value
on a global binding stack. Every exit point (normal return, `catch`,
`unwind-protect` cleanup, non-local `return-from`/`go`) restores the binding
stack to its saved depth. A `let` of up to six special variables writes its
stack entries inline with one check of the stack size, and stores the old
values back from their known positions at the normal exit.

The stack's top and its vector are the two fields of one structure (type
`:dynstate`), which the image holds in an immutable global, `dyn`, and every
module compiled in it imports. They were two mutable globals of the image,
which such a module imported: V8 reaches an imported mutable global through
one load more at every access, and the code cannot keep its value across a
store, so a binding in code loaded into the image took twice the time it
takes in the image (10 million bindings with a read of the variable, in
modules written for the measurement: 25 ms against 12; with the structure, 13
in both). The symbols of such a module are its globals, set by its `run`;
they are not nullable either now (they start as `NIL`, the image's
`nil_symbol`): reading a nullable global is a check more (17 ms against 14).
The hosts read the stack with the image's `get_bsp` and `get_bstack`.

A reference of a special variable is the symbol's value field with one
null test (unbound: `symbol-value` signals the error); that test is all an
unbound check costs, the same as the engine's own null check of a
non-nullable read would, so a variable known to be bound is read the same
way. A type proclaimed for a global variable (`(declaim (fixnum *end-pos*))`,
`(declaim (type simple-string *string*))`: the compiler's `*var-types*` from
`declaim`, and in the image the symbol's `sys::%var-type`, set by `proclaim`,
for code compiled after the proclamation was loaded from a fasl) or declared
around a reference (`(declare (fixnum *end-string-pos*))`) applies to its
references as to a lexical variable's (`typed-special-ref`): an integer type
is checked where it is read (`checked-value`) and the code after it computes
with a machine integer, other types are trusted as `the` is. cl-ppcre
declares its positions and its string so: its benchmarks take 0.93-0.97 of
the time before, Lem's `l-syntax-scan` 202 ms against 222.

The multiple-value count (`mv-count`) stays a mutable global of the image,
which the modules import: in the structure as well, every return that sets
it took a load more in the image's own code, and whole programs that return
values (`(floor i 7)`, `(values a b)` in a loop) were 1.3 to 1.6 times slower;
in code loaded into the image those were no faster.

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
`if` hinted as unlikely; a `br_on_non_null` out of a block around the call
was 4–10% slower in recursion through `funcall` and generic functions); on null, the code branches to the handler of the
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
around its cluster: entering it makes one simple vector, `#(next clauses
chosen restarts function...)` in the chain `*restart-clusters*`, where
CLAUSES is a constant vector that names the restarts and says where their
report, interactive and test functions are; the restart objects are made
only when they are asked for (`compute-restarts`, `find-restart` of one of
its names, a condition associated with them: `%cluster-restarts`), and
`invoke-restart` stores the number of the clause in CHOSEN. It made a
restart structure for each clause, a list of them and a cons, and set the
tag of each: `v-restart-nosignal` (a `restart-case` of two restarts and a
`with-simple-restart` that are not used) took 2.0 times SBCL's time, 1.3
now. A `handler-bind` whose handlers are all `lambda` forms makes one
closure, of the number of the type and the condition, instead of one for
each handler (kind `:one` in the cluster): a closure is a structure of ten
fields and its environment a vector. `assert` and `check-type` establish
their restarts only when the assertion fails (in the expansion, after the
test).

What the REPL, a script, a thread or a Lisp function called from
JavaScript do with an error that no handler takes is `*debugger-fallback*`,
which `invoke-debugger` calls after `*debugger-hook*` and
`*invoke-debugger-hook*`: a `handler-bind` of `error` around the code took
every condition of type `error` signalled, also one given to `signal`,
which must return when no handler of the program takes it (a script stopped
at `p-signal-warn` of `bench/programs.lisp`). A Lisp function called from
JavaScript still makes the error a JavaScript one before the handlers of
the Lisp code that called the JavaScript code, but only for the condition
that `error` or `cerror` is signalling (`*erroring*`).

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
A toplevel `defconstant` whose value is written as a number, a character or
a symbol (or another such constant of the file) is known to `compile-file`
for the rest of the file (`*cf-constants*`, consulted by the compiler's
constant hook), as SBCL knows it: the code of the file uses the value, not
the symbol's value cell (Lem's `+default-tab-size+`, the default of
`char-width`'s `:tab-size`, was read from its symbol at every call).

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
  compile time by the interpreter (a `defun` is compiled when first
  called) and compiled into the file's module only (ASDF then compiles in
  2.6 s and loads in 0.12), and only the batched
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

Third round (2026-11-21), libraries compiled from source with ASDF in a
process of their own (a fresh cache, their dependencies loaded first and not
timed; `asdf:load-system` timed inside the process, so the compiler's start
in a cold image is counted; ASDF itself is `(require :asdf)` with an empty
cache less the same with a full one, against SBCL's `compile-file` of
`third_party/asdf/asdf.lisp`; 4-CPU slot, medians of 3, milliseconds):

| library | SBCL | before | now |
|---|---|---|---|
| ASDF | 1930 | 2690 | 1780 |
| cl-ppcre | 665 | 1054 | 912 |
| Alexandria | 255 | 419 | 334 |
| Babel | 1065 | 992 | 888 |
| esrap | 345 | 352 | 314 |
| yason | 79 | 197 | 165 |
| local-time | 270 | 197 | 156 |
| split-sequence | 170 | 100 | 81 |

* **Finishing a file's module was quadratic in its segments.** For each
  segment, `live-module-finish` filtered all the symbols, constants and
  closures of the module, looking each up in an EQUAL table by a freshly
  consed `(:sym S)`: ASDF's file has 1105 segments, and this was a quarter
  of compiling it. The globals are bucketed by segment once.
* **Three walks of all the code.** `add-called-helpers`,
  `live-used-globals` and `global-segments` each walked every instruction
  of every function; they share `wasm-func-refs` (`src/wasm.lisp`), what a
  function calls, reads or sets and casts to, in order of first occurrence,
  computed once. (Most of the cost of a walk is the walk itself, about 15 ns
  an element, not the lookups.)
* **Macros compiled when defined.** The expander of each `defmacro` of a
  file (or of a form at the REPL) was compiled to a module of its own as soon
  as it was defined, though most macros are expanded a few times: 16% of
  compiling Alexandria. It is interpreted for its first seven expansions
  and compiled at the eighth (`global-macro-expander`,
  `*macro-compile-after*`).
* Type inference joins the environments of branches slot by slot, and made
  a new type for each slot even when both sides had the same one
  (`ty-union` of equal types returns the first).

The code of the modules is the same as before (the code sections of the
fasls of cl-ppcre and Quicklisp compared byte for byte), so run-time
performance is unchanged. What remains, in cl-ppcre's 0.9 s (a profile of
the process, `node --cpu-prof`): V8 outside the program (`(program)`,
mostly Liftoff compiling the compiler's functions and the new module's as
they first run) 0.25 s, garbage collection 0.18 s (scavenges copy 10-15 MB
each: the instructions of the whole file's module stay alive until it is
encoded at the end), type inference 0.15 s (environments are vectors of all
the variables of a function, copied at each branch), code generation 0.11
s, encoding 0.16 s (of it `coalesce-locals` 0.07), conversion and macro
expansion 0.1 s, printing the sources of the segments into the fasl 0.02
s. For the small libraries (yason, split-sequence) the cold compiler is
most of the time: compiling the first form of a process runs about 2 MB of
the image's code in Liftoff. Tried and not kept: a larger young generation
(`--max-semi-space-size` 32 or 64) and lower tiering budgets (3 MB, 0.8 MB)
changed nothing measurable; caching the printed form of each symbol while
writing a fasl made printing sources 10% faster, too little to matter.

Also tried and not kept (same day, interleaved on one slot, medians of 3):

* **Encoding each segment's functions when the segment ends**, with the
  indexes of functions and globals (not known until the module ends, and
  the `ref.as_non_null` after a closure's global) left as relocations
  written when it is encoded, and the branch hints moved by them; the
  instructions were dropped then. All 21608 functions of loading cl-ppcre
  were encoded so and the modules were the same octets, but compiling was
  no faster (cl-ppcre 0.98 s against 0.99, ASDF 1.87 against 1.88), nor were
  the scavenges (25 of them, 165-175 ms, against 22, 144-291). What a
  scavenge copies is the compiler's working data: about 58 MB are made
  between two of them and a tenth survives; a segment's instructions
  survive a scavenge before it ends anyway.
* **Type inference's environments**: copying them and joining them is
  about 40 ms of cl-ppcre's 125 ms of inference (the rest is mostly its
  hash tables, the node types keyed by conses); copying them by `svref`
  instead of `copy-seq` (`subseq`'s dispatch) changed nothing measurable,
  so environments that share their unchanged parts would gain 3% at most.
* **`coalesce-locals` only for functions with more locals**: its time is
  in the large functions (cl-ppcre: 59-71 ms for all, 71-77 with a minimum
  of 8 locals, 48-52 with 16), and it is a walk that takes 4.5 ns an
  instruction when the same code runs hot on instructions made together
  (a probe in a script), against about 40 ns in a real compilation: the
  cost is the memory of instructions made over the whole file and code
  still cold, not the work of the walk.

What remains (compiler in Wasm, whole program): type inference 0.8 s (its
node types are an EQ hash table keyed by conses, whose identity hash is a
call to the host: 0.3 s), code generation 0.6 s, encoding 0.75 s, macro
expansion and conversion 0.45 s. In the image, a small form costs about
0.4 ms of the compiler's time and 0.3 ms of V8's (decoding, validating and
instantiating the module); a third of the code of a small module is its
`RUN`, `SNAP` and `RESTORE`, the last two needed only by images that record
modules for `save-image`.

**Loading a source file compiles consecutive definitions into one module**
(`%load-batch`, `runtime/eval.lisp`): a `defun`, `defmethod` or
`defgeneric` form read from a file is compiled with the definitions that
follow it (up to 32 forms) and with the form after them when that one is
compiled (not a simple form: a call of the functions, a `defbench`-like
macro, a `defmacro`), so that the calls between the functions of a file are
calls within a module, which V8 inlines, with the types of the arguments and
values flowing between them (typed and raw copies, catch clones, small
functions copied into their callers). The forms still mean what they mean
one by one:

* A call from one form of the batch to a function that another form defines
  is guarded (`*batch-forms*`, `batch-guarded-p`, `src/frontend.lisp`):
  `(if (%defun-guard 'f) (direct call) (call through the function cell))`,
  the guard comparing the function of `f` with the closure of its
  definition in the module (one `ref.eq`), so a later redefinition (by
  another form, at the REPL, by `trace`, by `(setf fdefinition)`) is seen.
  A small function of an earlier form of the batch is copied into the
  caller under the same guard (`live-inline-call`; the guards of copies in
  loops are hoisted as those of other copies), a `#'f` of such a function is
  `(fdefinition 'f)`, and `(catch 'tag (f x y))` of variables tests the
  guard before the `catch`, so that the direct path may call `f`'s copy whose
  throws to the tag are returns (`batch-guarded-catch`: `g-ctak` in a script
  74 → 26 ms, the whole program 30). Calls between the functions of one form
  are direct, as when the form is compiled alone; a call compiled before
  another form defined its callee (in a macro's expansion, which the
  pre-scan of literal `defun`s does not see) makes that callee no known
  function of the module (`*batch-no-direct*`).
* A form is read before the definitions before it have run. Only a `#.`
  calling them, or a macro calling them when it is expanded, can tell: an
  error of the reader makes the batch run and the form be read again from
  its position, and an error of the compiler makes the forms be evaluated one
  by one, as before. (Only the forms of a file are batched; a stream that
  cannot be read again gets batches of methods only.)

`tests/cases/load-batch.lisp` checks redefinitions of the functions of a
batch by later forms and by `(setf fdefinition)`, forward calls, `#'f`,
SETF functions, keywords, doubles, a macro and a `#.` calling a function of
the batch, and a copy of a function returning from its block in a closure.

Why code loaded from source runs slower than the same code as a whole
program (2026-11, `MODE=script tools/bench.sh`, the emitted Wasm compared
with `*live-dump*` and `tools/dump-func.lisp`): the code of a function is
mostly the same, but

* **V8 inlines no call into another module.** A whole program is one
  module, and the engine inlines the targets of `call_ref` it has seen
  (a method called from a call site's cache, `call-next-method`, a
  `notinline` function called through its symbol); in the image each
  toplevel form is a module of its own (`x-8typecase` 2 times the whole
  program's time, `p-cnm` 1.9, `c-gf3` 1.9, the runtime's `equal` 1.7).
  `load` of a source file therefore compiles consecutive definitions
  into one module (`%load-batch`, above; first only `defmethod` and
  `defgeneric` forms, whose methods are only reached through their generic
  function: `p-cnm` 52 → 38 ms in a script, whole program 27; then
  `defun`s too, with guarded calls between the forms). A call into the
  runtime (`equal`, `length`, the dispatch of a generic function) stays a
  call into another module.
* **V8 gave a module made in the image almost no inlining budget.** The
  optimizing tier scales its inlining budget by the share of small
  functions (bodies under 50 octets) in the module (`BudgetScaleFactor`,
  `src/wasm/inlining-tree.h`): none at a quarter or less, the whole at a
  half or more, a function then growing by a factor of 5 up to 7000 octets
  instead of 2 up to 700. A whole program has the runtime's many small
  functions; a module made in the image has mostly large ones (a
  function's entries and typed copies), so `tak`'s recursive calls of its
  typed copy were inlined in a program and not in a script. The encoder
  adds as many filler functions (never called, four octets each: no
  locals, `unreachable`) as a module needs to be half small
  (`small-function-fillers`, `src/wasm.lisp`; whole programs need none):
  `b-tak` in a script 117 → 95 ms (whole program 86).
* **A store into an old object of an object made after the engine's
  last full collection takes the slow path of the write barrier**, until
  a full collection (a scavenge does not help; a 50 ms pause of the event
  loop, or `gc()`, makes the same loop twice as fast). In a loaded file
  the symbols, constants and closures of a form are new when the next
  form runs it: binding a special variable stored the symbol and its old
  value into the binding stack, made at start, at each binding
  (`b-special` in a script 2.3 times the whole program). The binding
  code now stores each only when its slot holds another object, which a
  binding in a loop finds there (`bind-specials-code`): `b-special` in a
  script 165 → 75 ms, the whole program 80 → 66. The helpers `:bind` and
  `:unbind-to` (which bind `*catch-tags*` to the tag of each `catch`, and
  more than a few specials at once) store each slot, the symbol's new value
  and the old value restored at the exit only when it holds another
  object: a recursion of `catch`es of one tag, `g-ctak`, finds them there
  (with the batches, `g-ctak` in a script 74 → 26 ms, the whole program 30
  → 25). The same test of the symbol's value in the inline binding code
  made the whole program's bindings 10% slower (`v-special-1` 21 → 25 ms),
  so it is not made there.
  Making the young generation small (`--max-semi-space-size=1`), so that
  the objects of a loaded form are promoted before it runs, made
  `t-throw-closure` in a script 26 → 15 ms but `bench/control.lisp` and
  `bench/programs.lisp` slower in both modes (whole programs 1.07 and
  1.02 times their time, scripts 1.04 and 1.06): not kept.
* What `MODE=script` also shows comes from causes described elsewhere: a
  guarded copy of a fixnum function whose value is assigned back
  (`a-lcg`, `p-hash-fn`'s `xorshift`), a loop at top level in Liftoff.

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
compiled again. A `defun` evaluated at compile time too (in `eval-when` with
`:compile-toplevel` and `:load-toplevel`: ASDF's and UIOP's
`with-upgradability`, the helpers of macros) is part of the module as well;
at compile time its name gets a function that compiles the `defun` when it
is first called (`%cf-define-lazily`), since most are never called then.
So are the functions of a `defstruct` (its constructors, predicate, copier
and accessors); the rest of its expansion is compiled and run at once. An
uninterned symbol is written as `#.(SYSTEM::%FASL-GENSYM n "NAME")`, one
symbol per number for the whole file when it is loaded, so the entries of a
fasl share it as the forms of the file did.
Other forms evaluated at compile time (`declaim`, `defmacro`, `defclass`...)
are evaluated by the interpreter then and compiled into the file's module
only (`*cf-unit-compile-time*` `:interpret`): a module of their own for each
(`%cf-compile-and-run`, compiled once and run at once) made loading ASDF's
fasl take 0.25 s instead of 0.12, and compiling them twice (run now and into
the module) made compiling ASDF take 4.4 s instead of 2.6. A form the compiler fails on is written as source; when the
module cannot be made or the constants of a segment cannot be written, the
whole file is written as source.
A `defconstant` (also as the expansion of a macro: cl-ppcre's, Alexandria's
`define-constant`) whose value can be computed at compile time and is a
number, character, symbol or string defines the constant then, as SBCL
does (`%cf-note-defconstant`; CLHS lets the value be evaluated at compile
time), so the rest of the file is compiled with its value: cl-ppcre's
`(find chr +whitespace-char-string+ :test #'char=)` is five comparisons
instead of a call of `find` with keywords. Other values (vectors, lists) are
not: a copy in the fasl would not be the object made when it is loaded. A
value form that fails then (it calls a function of the same file) is left
for the load.

**Functions declaimed `inline`** keep their source in the image (the property
`sys::%inline-expansion` of the name, also written to the fasl), and calls
compiled later in the image (other files, the REPL) are replaced by a copy of
the body that sees none of the caller's lexical bindings, with `&optional`,
`&rest`, `&aux` and `&key` (constant keywords) bound by `let*`
(`inline-expansion-call`, `src/frontend.lisp`). `notinline`, global or
local, prevents it; `proclaim` records `notinline` on the symbol so that it
holds in an image that loaded the fasl. A local `(declare (inline f))`
undoes a global or outer `notinline` in its scope (`*local-inline*`: esrap
declaims `find-chunk` inline, defines it, declaims it `notinline`, and
inlines it in `ensure-chunk` by `(locally (declare (inline find-chunk))
...)`). SETF functions are inlined the same way: the `inline` proclamation
of `(setf f)` is recorded under the function's symbol, and its source has
the block named `f` (esrap's `(setf get-cached)`, which was a call and a
tail call through two symbols for each rule).

The small functions of `COMMON-LISP` in the REPL image (required parameters,
no loop, at most 100 conses: `alphanumericp`, `char-upcase`, `fifth`...) keep
their source the same way (`runtime-inline-source`, `src/driver.lisp`, set
when the image starts: 60 KB), so code compiled in the image contains a copy
of them as a whole program does. The guarded copies of small functions
defined at the REPL (`live-inline-call`) take any lambda list (`&optional`,
`&key` with constant keywords, `&rest`): `(kfun i :c 3)` of a function
defined in an earlier form runs as fast as in a whole program. A body that
returns from the function's block (`(return-from f x)`, also in a closure)
is copied with the block; it was taken for a recursive one
(`strip-return-froms`).

How such copies are compiled (2026-11, measured on numeric probes written
for it: spectral norm with and without declarations, small double helpers
called in a loop, a loop at top level):

* **The depth of copies counts the copied bodies only.** Copies are made
  two deep (`*live-inline-depth*`); the arguments of a copied call used to
  be converted at the copy's depth too, so in `(clamp01 (lerp (hyp x 1d0)
  (sq x) 0.25d0))` the calls of `hyp` and `sq` went through the function
  cell. The body is now converted inside `(sys::%inline-copy depth-var
  ...)`, the arguments at the caller's depth (also for `inline` functions,
  `*inline-expansion-depth*`).
* **A value that is a double on one of its paths** is not boxed. The guarded
  copy is `(if guard COPY (call))`: its type is the union of the copy's
  (a double) and the call's (anything), so its double was boxed and the
  arithmetic around it was generic. An `if` (or a `let`, `progn`, or a
  `block` nothing returns from, ending in one) one of whose branches gives a
  double, arithmetic that may give one or an unboxed double variable is
  computed into a pair of locals as an unboxed double variable is
  (`cg-dual-f64`, `src/typed.lisp`: the eqref null when the f64 holds the
  value): as a leaf of a speculative arithmetic tree, as the value of a `let`
  variable used in arithmetic or comparisons once or more, as an operand of a
  comparison. `(cond ((< x 0d0) 0d0) ((> x 1d0) 1d0) (t x))` is such a value
  too. The parameters of a copy, bound to the temporaries of the arguments,
  stay unboxed through those bindings.
* **Comparisons of doubles of unknown type**: `<` `>` `<=` `>=` `=` `/=` with
  an operand that is a double (or one on one of its paths) and another that
  may be one compare f64s after testing (`spec-cmp-code`); before, a double
  failed the inline fixnum test and called the generic comparison.
* A variable declared `double-float` assigned arithmetic of unknown operands
  takes the f64 of its fast path; the temporary of an `incf` bound to
  arithmetic with a double among its operands is unboxed although it is
  used once; a variable that starts as a double and is assigned such
  arithmetic is an unboxed double variable (it needed a double among the
  operands of the assignment, which a copy's value in the pair of locals
  was not when the variable was bound).
* **A toplevel form with a loop gets copies** too (before, the code of a
  toplevel form, which runs once, got none; `*live-toplevel-loop-p*`, a scan
  of the form for the operators of iteration, bounded for circular
  literals): `(let ((s 0d0)) (dotimes (i 3000000) (incf s (poly ...))))`
  takes 33 ms instead of 63 (it stays in Liftoff, see Tiering of the engine).

Measured on these probes (`clysm script`, best of three, 4-CPU slot):

| probe | whole program | in the image, before | after |
|---|---|---|---|
| spectral norm, undeclared (`svref`, `(/ (* ..) 2)`) | 76 → 58 ms | 141 | 72 |
| spectral norm, `fixnum` and `double-float` arrays | 24 → 17 | 54 | 29 |
| small double helpers (`sq`, `hyp`, `lerp`, `clamp01`) in a loop | 17 → 15 | 188 | 35 |
| a loop at top level calling a function of an earlier form | 16 → 13 | 34 | 18 |
| mandelbrot with a helper `mag2` | 17 → 16 | 24 | 22 |
| n-body and matrix multiply over vectors of doubles | = | = | = |

(The whole program got faster from two changes that apply to it too: an
integer within 64 bits converted to a double by `f64.convert_i64_s` instead
of making the integer and calling `to-f64`, and `(/ a k)` of such an
integer by a constant computed inline when K divides it, else the generic
division, `exact-int-division-code`.)

**Loops that call nothing but copies run in a version without guards**
(`cg-tagbody`, `src/codegen.lisp`). A cold branch with a call in a loop
costs V8 about half the speed of the loop even when it is never taken (a
hand-written loop: 9-10 ms with the call in the guard's else branch, 6
without), so the guards are not merely hoisted:

* The guards of a loop that calls nothing but the copied functions (when a
  guard fails) and primitives (`loop-guards`: no `funcall`, no local
  function, no closure, no `catch`/`unwind-protect`, and only a list of
  calls of `COMMON-LISP` accessors and arithmetic) are tested once before
  the loop into locals (`*hoisted-guards*`). After the call of a failed
  guard and after the interrupt check (`poll-code`: an interrupt, SWANK, or
  another thread may redefine a function), they are tested again.
* An innermost such loop is compiled twice: a version where the guards are
  true (`*assumed-guards*`: the copy alone, no else branch, no call at all),
  entered when they all hold, and the version with the guards. Both share
  their variables; the interrupt check of the first, when an interrupt made
  a guard fail, sets the segment number and goes on in the second (compiled
  with the dispatch of `tagbody`) from the head of the same segment, where
  an iteration starts. In the first version the type of a copied call is
  the copy's (`assumed-node-ty`, `src/types.lisp`), also through the
  `let`/`block` around it and arithmetic of such doubles, so `(* (eval-a i
  j) (aref v j))` is f64 arithmetic; a `let` variable bound to a double
  there and an accumulator that starts as a double are unboxed although
  used once, and a tree with `sqrt` of doubles is speculative (a negative
  operand is tested).
* The guard reads the symbol's global without a cast (it has the type).

`tests/cases/copies-in-loops.lisp` checks a redefinition before a loop; a
redefinition from inside a loop (by `eval` in it) and by an interrupt in the
middle of a loop (`sys::*interrupt-poll-function*`) were checked by hand to
give the results of the build before.

Measured (`clysm script`, best of three, same slot, a busy machine,
before → after this round; whole program and `compile-file` + `load` in
parentheses): spectral norm 82 → 58 ms (57, 55), declared 32 → 23 (18,
18), double helpers 40 → 27 (16, 17), mandelbrot with a helper 24 → 18
(18, 18), the loop at top level 20 → 14 (14, 14), matrix multiply over
vectors of doubles 108 → 79 (78, 76); n-body unchanged; the spectral inner
loop alone 10 → 7 ms against 6 for a copy declared `inline`.
`MODE=script` geometric means, interleaved with the build before (both
noisy): `bench/suite.lisp` 1.14 → 1.09, `bench/arith.lisp` 1.16 → 1.14,
`bench/vectors.lisp` 1.03 → 1.06 (script time 1.01 of before),
`bench/holdout.lisp` 1.07 → 1.07.

What remains:

* A guarded copy of a function of fixnums: its value is still boxed as an
  `i31` where it is assigned to a variable, and a variable assigned the
  value of a copy (`(setq x (lcg x))`) has the copy's type joined with
  anything, so the copy's body sees an argument of unknown type: `a-lcg`
  in the image 26 ms, 6 as a whole program; the fixnum helpers 5-6 against
  3. The types of the loop's version come from the inference of the whole
  function, in which the else branches exist; inferring the version again
  with the guards true (as `compile-spec-clone` does for a function) would
  type such variables, but the variables would then need representations of
  their own in that version, converted where it hands over to the other.
* A loop at top level runs once, in Liftoff: 14 ms against 7 with
  `--no-liftoff` (as in a whole program, whose `main` also runs once).
  Outlining the loop into chunks called repeatedly was measured by hand and
  does not pay: a local function is inlined back (28-44 ms), and a global
  function taking the loop's variables as parameters loses their types
  (30-46 ms); outlining would need typed parameters (a version of the
  outlined function for the types the variables have).

**`dotimes` over a count held in a variable** (`runtime/boot.lisp`) whose
body uses the counter and is small (at most 120 conses) expands to `(if
(fixnump n) <the loop with n bound to a variable declared fixnum> <the
loop>)`. Where the type of `n` is known the test is a constant and only
one branch is compiled (`cg-if` and `cg-effect` compile the branch a
constant test takes and not the other; the test must be `fixnump`, which
is a primitive everywhere, not `typep`, which the runtime's own early files
call out of line); where it is not (a parameter of a local function or
closure, a free variable, which get no fixnum entry version), the counter
of the fixnum branch is an i32 compared with an i32. `p-queens` (a
`labels` function looping to its free variable `n`) 307 → 261 ms (SBCL
116); the runtime's module grows by 0.7%.

**Local functions and lambdas of a function** are copied into their calls
(`inline-calls`, `src/inline.lisp`) when their body has at most 30 nodes,
or 300 when the call is the only reference of the variable that holds the
function (and of the variable it was bound from: a lambda given to a copy
of a function): the copy then replaces the function's code, which is not
compiled, and the closure is not made. A test of a variable always bound to
a lambda is true (`(when zero-fn (funcall zero-fn))` in a copy given a
lambda), and a variable bound to such a variable that is no longer
referenced once its calls are copied is not bound, so that the lambda's
variable has no reference left either. Lem's `character-offset` and
`character-at` pass two lambdas to `%character-offset`, which they copy:
each call made two closures, used only to be compared with `nil`, and
`character-at` a third for a lambda of more than 30 nodes called once; they
make none now. `assert` signals its error and offers its `continue` restart in
`%assert-error`, a function, instead of a `restart-case` at each assertion
(about 100 Wasm instructions, a `try_table` and the binding of
`*restart-clusters*`, in every function that asserts: Lem's
`%move-to-position` asserts twice: its Wasm is a third smaller).

**Small functions of a compiled file** (a `defun` at top level, no closed-over
bindings, no loop, no macro outside `COMMON-LISP`, a body of at most 40
conses and atoms: `*fasl-inline-max-size*`, not declaimed `notinline` or
`inline`) keep their source in the fasl: `(sys::%note-fasl-inline name
lambda-list body mark)` after the `defun` puts it on the name with a mark
made when the file was compiled (the time and a hash of the source: every
session of an image starts from the same random state) and the function
that the definition made. Calls compiled later in the image (the next files
of a system, the REPL) contain a copy of the body guarded as at the REPL
(`live-inline-call`), one level deep (`*fasl-inline-max-depth*`: a copy has
no copies in it): in a fasl the guard (`%fasl-inline-guard`, five
instructions) compares the function of the name with a global of the
module, one for each callee, that a toplevel form after the first form
making such a copy sets (`%fasl-inline-guard-set`) to the function of that
definition while it is the name's function (`sys::%fasl-inline-function`
compares the mark), else to an object that is no function, and that is
null before (the call is made); a redefinition, before the caller's fasl is
loaded or after, a `trace`, or a fasl of the callee compiled again (another
mark) make it an ordinary call. Lem's buffer
code (`line-length`, `line-alive-p`, the wrappers of the readers of `point`)
crosses files at every call: `tools/bench-lem.sh` takes 0.935 of its time
(best of five interleaved, Clysm alone; `l-line-down` 0.91,
`l-type-redisplay` 0.82, `l-search` and `l-syntax-scan` unchanged); Lem's
fasls are 19.7 MB instead of 17.2, all the fasls the comparators load 56.0
instead of 53.1, and loading Lem's fasls takes 1.85 s instead of 1.78 (medians
of seven). Larger copies cost more space than they gain: 60 conses and copies
two deep made Lem's fasls 23.5 MB (with about the same time), 24 conses
18.7 MB and 0.95 of the time. `bench/apps.lisp`, `bench/libs.lisp` and
`bench/suite.lisp` and `bench/holdout.lisp` in the three modes are unchanged.
Of the 2.5 MB that Lem's fasls gained, the copies are 2.14 and the sources
0.36 (17.38 MB with nothing recorded, 17.74 with the sources and no copies);
one guard global for each callee instead of one for each copy, set by a form
of the module instead of at the first run of each copy, saves 0.08 MB.
Writing the sources after the module, apart, read only by the first
compilation that copies one, made the fasls no smaller (the text moves) and
loading them no faster (1.706 s against 1.715, medians of seven), so it was
not kept.

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
forms (`build/clysm-native.img`, see Start-up and loading). An image
started from a heap image runs in Node.js's main thread rather than a
worker (Start-up and loading).

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
  mutable fields of objects (cons, vector, symbol, closure name and free variables, instance,
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
sockets; a program in the main thread, an image started from a heap image,
has them served by a worker started when first needed). In a library (`host/lib.mjs`) the manager is in the same thread,
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
the main thread performs the socket operation (or, when the program runs in
the main thread, a worker started for that).

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
  `--stack-size`, set when the host starts, in the main thread too); elsewhere (a browser,
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
sandbox. The latest results are in `tests/ansi/RESULTS.txt`: 21931 of 21932
tests pass (99.99%), with no crashes. The five tests with the note
`:loop-iteration-values-in-finally` (marked `:ansi-spec-problem`) are
inactive, as on SBCL: like the MIT LOOP, an arithmetic `for` variable is
stepped past its limit before the loop ends, and libraries rely on it. So
are the eight with the note `:no-floating-point-underflow-by-default`,
which expect `floating-point-underflow`: like SBCL with its default traps,
Clysm does not signal underflows (see "Floating-point conditions"). The one failure, SUBTYPEP-COMPLEX.8,
assumes that `(complex single-float)` is not a subtype of
`(complex (float 0.0 1.0))`; like SBCL (which fails it the same way), Clysm
reads `(complex X)` as the complexes whose parts are of
`(upgraded-complex-part-type X)`, here `float`.

## Status and limitations

### Floating-point conditions

Float arithmetic signals what SBCL signals with its default traps, at
every safety and in declared and undeclared code alike:
`floating-point-overflow` for an infinite result (`%float-result` in
`runtime/numbers.lisp`), `division-by-zero` for a float divided by zero,
`floating-point-invalid-operation` for a zero (or a NaN) divided by zero
(SBCL's `:invalid` trap; `(/ 0 0)` of rationals is a division by zero);
an underflow is no condition (SBCL masks it by default; CLHS 12.1.4.3 and
the arithmetic functions' pages leave it to the implementation): a result
too small for a normalized float is the denormalized float or the zero
that IEEE arithmetic gives, for `* /`, `exp`, `expt`, `scale-float`,
`float`/`coerce` to a single, complex products alike. So the inline code
tests a product or quotient only for being finite: its exponent compared
with all ones (`f64-not-finite-code`; the subtraction `x - x != 0` that a
sum is tested with made `d-mandelbrot` at `(safety 1)` 1.29 times as long,
V8 compiling that comparison into two branches), with no test of a zero
and of the operands after it. `(expt float -n)` whose `float^n` is past the floats
takes the host's `pow` (`(expt 10d0 -400)` is 0, `(expt 2d0 -1074)` the
least denormalized double), as SBCL's does. Compared with SBCL 2.6 on a
probe of underflowing operations (products, quotients, sums of
denormalized floats, `exp`, `expt`, `sqrt`, `log`, `sin`, `tanh`,
`atan`, `scale-float`, `float` of tiny ratios, complex products, `cis`,
`abs` of complex): the same values bit for bit (`integer-decode-float` of
a denormalized single now gives SBCL's significand and exponent -149),
printed with fewer digits where they are denormalized (the shortest
representation that reads back). Difference left: other
operations on an infinity or a NaN (which SBCL's traps make hard to come
by) give the IEEE result, or `floating-point-overflow` when the result is
an infinity, where SBCL's `:invalid` trap signals
`floating-point-invalid-operation` for some. There is no trap mask (no equivalent of
`sb-int:with-float-traps-masked`). The ANSI tests that expect
`floating-point-underflow` (`EXP.ERROR.8`–`11`, `EXPT.ERROR.8`–`11`) carry
the note `:no-floating-point-underflow-by-default`, which an implementation
that does not signal underflows disables, as `tests/ansi/driver.lisp`
does.

Measured against the build before (2026-11-21, interleaved, best of five
on one slot): `bench/declared.lisp` at `(safety 1)` 0.97 in geometric mean
(`d-matmul` 194 → 151 ms, `d-nbody` 694 → 564, `d-mandelbrot` 229 → 246,
the one slower), at its `(safety 0)` unchanged (no float tests there; the
items within the noise of a run); `holdout` 0.98 (`h-matmul-double` 152 →
135, `h-nbody` 110 → 104), `suite` 0.99, `arith` 1.02 (`a-nbody-struct`
1.01; the items above 1.05 are of under 10 ms).

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
| `bench/libs.lisp` (`tools/bench-libs.sh`) | cl-ppcre, Alexandria (its test suite and utilities), split-sequence, Babel from Quicklisp; the self-hosted compiler compiling `bench/suite.lisp` | 1.2 | `l-babel` 2.2 (`l-ppcre-replace` 1.5) | `l-alexandria-tests` 0.2–0.5 |
| `bench/programs.lisp` | 63 more programs | 1.19 | `p-builtin-dispatch` 3.15 | `p-remove-duplicates` 0.03 |
| `bench/arith.lisp` | 13 probes of arithmetic in undeclared code (numbers in slots, lists, vectors, hash tables) | 0.56 (0.58 before the integers beyond the fixnums stayed in i64 in more places, 2026-11) | `a-sum-overflow` 2.33 (bignums beyond 2^63) | `a-simpson` 0.10 |
| `bench/declared.lisp` | 19 programs with full declarations at `(speed 3) (safety 0)` (see below) | 1.24 (1.56 before `safety` was read) | `d-particles` 3.0, `d-nbody` 2.3 | `d-k-nucleotide` 0.79 |

Code loaded form by form into the image (`clysm script`, the REPL, `load`)
against the same file compiled as a whole program (`MODE=script tools/bench.sh
bench/suite.lisp`): geometric mean 1.1, slowest `b-special` 2.8 (`b-call` 1.1, was
1.9; `b-struct` 1.0, was 1.35). In 2026-11 (after the guarded copies of
numeric helpers were made to keep their doubles unboxed, see "Compiling
inside the running image"): `bench/suite.lisp` 1.11, `bench/holdout.lisp`
1.06, `bench/arith.lisp` 1.14 (1.07 before, but only because the whole
program got faster: `a-lcg` 12 → 6 ms, `a-points` 12 → 8; in the image it is
0.97 of its time before), `bench/vectors.lisp` 1.09; numeric probes written
for it (spectral norm, double helpers in a loop) went from 1.9–11 times the
whole program to 1.2–2.3. In 2026-11-21, with the batches of `load` (guarded
direct calls between the definitions of a file), the filler functions that
give a module made in the image V8's whole inlining budget and the
binding helpers that skip stores of what a slot holds (see "Compiling inside
the running image"; script time against the same file compiled, best of
three, interleaved with the build before on one slot): `bench/suite.lisp`
1.10 → 1.09, `bench/programs.lisp` 1.12 → 1.14 (its script time 0.99 of
before; the whole programs were faster in that run), `bench/gabriel.lisp`
1.24 → 1.13, `bench/control.lisp` 1.25 → 1.21, `bench/clos.lisp` 1.16 →
1.11 (`g-ctak` 2.35 → 1.00, `g-takl` 1.56 → 1.09, `b-tak` 1.36 → 1.08,
`p-typecase` 1.49 → 0.91, `p-cnm` 1.41 → 1.00, `r-mapc-return` 1.45 →
1.01, `v-return-closures` 1.64 → 1.17, `c-cnm4` 1.44 → 1.14); compiled, the
same files against SBCL are unchanged (`suite` 0.82, `gabriel` 0.91 → 0.89,
`control` 0.76 → 0.78, within the noise). Loading 500 small `defun`s from
source takes 0.85 s instead of 1.19 (fewer modules). What remains is mostly
calls into the runtime's module (`equal`, `length`, the dispatch and
combination of generic functions: `p-method-comb` 1.9, `c-comb-all` 1.5,
`v-go-closure` 3.7), the objects of a just-loaded form being young when it
runs (`t-throw-closure` 2.2: see the write barrier), and `p-equal`'s
similar string literals, which a compiled file may coalesce and `load` may
not.

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
4.0), `bench/control.lisp` (0.94, was 1.3; slowest `t-return-maphash` 4, `v-hb-nosignal` 2.9; `t-return-maphash` about 1 since 2026-11-21), `bench/io.lisp` (1.2–1.3, was 1.46;
it afterwards; those are kept in further files: `bench/clos.lisp` (geometric mean 1.28, was 1.37; slowest `c-builtin-eql`
3.5, `c-eql-dispatch` 2.6, was 4.4), `bench/control.lisp` (0.94, was 1.3; slowest `t-return-maphash` 4, `v-hb-nosignal` 2.9), `bench/io.lisp` (1.2–1.3, was 1.46;
`i-file-roundtrip` 1.6–2.0, was 13; `i-print-numbers`, `i-parse-ints` 2.5), `bench/library.lisp`
(1.04, was 1.12; `v-map-every` 2.8, `v-hash-iterate` 2.2, was 3.4),
`bench/vectors.lisp` (0.43, algorithmic code over simple vectors without
declarations: fannkuch, sorts written by hand, a heap, a sieve, shuffles;
slowest `x-sort-key` 1.26; in the image 1.13 of compiled). An `aref` of a
vector of known kind whose subscript has no known type (an element of a
vector, an unbounded counter) is inline with a test of the subscript, and
a parameter that starts a loop counter (`(loop for i from start ...)`)
makes its function's fixnum version be tried on entry,
`bench/closures.lisp` (combinator-style code: continuation passing, parser
combinators, an interpreter compiled to closures, generators, a pattern
matcher; 1.27, was 1.50; slowest `k-memoize` 3.9, which is `equal` hashing
of argument lists, and `k-labels-escape` 1.9; 2026-11-21, after the changes
under "Functional code" below: 1.11 against 1.28 for the build before in the
same run, `k-memoize` 2.6 → 1.9).

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
* **Tiering of the engine** (measured 2026-11, Node.js 24, V8 13.6). V8
  compiles a Wasm function with Liftoff first and with TurboFan once it has
  run enough (`--wasm-tiering-budget`, about 13 MB of code executed); there
  is no on-stack replacement for Wasm, so a function called once with a hot
  loop (`main`, a toplevel form of a script) stays in Liftoff, about twice
  as slow (a nested loop run once: 42-55 ms against 24 with
  `--no-liftoff`). Real workloads are not limited by it: Lem's actions
  (`tools/bench-lem.sh`) are as fast with `--no-liftoff` (geometric mean
  2.44 against 2.38) and real libraries (`bench/apps.lisp`) are slower with
  it (2.78 against 2.11), because Liftoff code collects the feedback by
  which TurboFan inlines the targets of `call_ref`; compiling everything
  with TurboFan also costs 3 s at start (0.16 s now). A lower or higher
  tiering budget, larger inlining budgets (`--wasm-inlining-budget`,
  `-max-size`) and a larger young generation (`--max-semi-space-size=64`)
  changed `bench/apps.lisp` by 3% or less, so the default flags are kept;
  eager top-tier compilation hints (experimental) would lose the feedback as
  `--no-liftoff` does. What remains is the run-once hot loop, which only
  outlining the loop's body into a function called once per iteration of
  an enclosing loop would let tier up.
  neither the call nor the binding is the cause. Part of the cost was the
  binding itself: such code reached the binding stack through imported
  mutable globals and its symbols through nullable globals (see Dynamic
  binding); since that changed (2026-11), `b-special` loaded into the image
  takes 182 ms instead of 238, `v-special-1` 58 instead of 77, `v-special-6`
  186 instead of 264 (same slot, one run each; the bimodal spread remains).
* **The machine code of the call sites of slot readers and writers**
  (2026-11-21, from Lem). Lem's buffer functions took 1.0–1.25 times
  SBCL's time each alone in a loop (`%point<` 8.5 ns against 7,
  `character-at` 26 against 23, `end-buffer-p` 29 against 29) but their
  composition more: `form-offset` over a form 68 µs against 42, and a
  copy of `%skip-list-forward` gained 13 µs from `end-buffer-p` where
  SBCL's gained 5. The cause was the size of the code, which V8's own
  profiler (`node --prof`, the ticks of each instruction, the code read
  from `--perf-prof`'s jitdump with `objdump`) showed: the TurboFan code
  of `%point<` (four reads of `point` slots, two comparisons) was 7.6 KB,
  SBCL's 338 bytes; `point-change-line` 17 KB against 703,
  `%skip-list-forward`'s loop 13.9 KB against 1.1. A reader's site was
  300 bytes on its hot path: the cache vector, an `eqref` live constant,
  was tested for null, for an i31 and for its map at each of its eight
  reads and their bounds checked; the instance was cast again for the
  read by a checked cast (`(:ref.cast :instance)`, whose failure calls a
  function: V8 kept a flag of the earlier test and the call); and the
  three calls out of line followed. Now a site of a reader or writer
  that `defclass` defines is `%site-slot-read` / `%site-slot-write`
  (`src/prims.lisp`): the instance tested and cast once into a typed
  local, its layout compared with the four entries in a chain, the slot
  read or written; anything else (an unbound slot, a miss, a site not
  cached) is null and one call of `%gf-site-other1`/`2` in a branch marked
  cold. A live constant that is a simple vector is a global of type
  `(ref $vector)` (set by RUN with a cast, restored so from a heap image),
  so that reading an element checks the bounds only. A reader's site is
  about 25 instructions of machine code: `%point<` 2.9 KB,
  `point-change-line` 6.5 KB, `%move-to-position` 6 KB instead of 9.
  The steps, `tools/bench-lem.sh` (geometric mean, SBCL's time 1): 1.46
  before; branch hints alone (below) 1.45; only the first entry inline
  (the rest in `%gf-site-other1`) 1.43; with the typed constants and the
  instance cast once 1.40; two entries inline 1.25 (the sites of `point`'s
  readers see temporary `point`s and the buffer's `cursor`, a subclass:
  with one entry 4 million calls of `%gf-site-other1` in a probe of 60
  parses); four entries 1.21 (drawing objects of several classes in the
  redisplay). Branch hints: a branch of an IF that signals (a call of
  ERROR, CERROR, `%assert-error`, `%check-type-error`...) or begins with
  `(%cold)` is marked unlikely (`cold-branch-p`, `src/codegen.lisp`);
  marking every branch to a block named SLOW or FAIL as well changed
  nothing measurable and was not kept. Against main, interleaved on one
  slot: `tools/bench-lem.sh` 1.47 and 1.41 → 1.21 and 1.29 (`l-forms-forward`
  285 → 196 ms, `l-type-autodoc` 285 → 200, `l-type-indent` 71 → 51,
  `l-line-down` 150 → 141, the others within the noise; a probe typing
  2000 characters with a redisplay after each 163–167 → 149–160 ms);
  `bench/clos.lisp` (whole program) 1.27 → 1.25 (`c-accessors` 0.83 →
  0.56, `c-with-slots` 1.02 → 0.92); `tools/bench-libs.sh` 1.19 → 1.18,
  `bench/apps.lisp` and `bench/library.lisp` (0.90 → 0.91) within the
  noise. Then `slot-value` with a constant name (`with-slots`) the same
  way: `%cached-slot-read`/`-write` compare the four pairs of the site's
  cache and the entry of a class redefined with other slots (with the
  instance's number of slots) in one access, the rest calls
  `%slot-value-cached` (it was the four comparisons inline, then a call of
  `%slot-cache-miss` for the redefined class's entry): `c-with-slots` 0.92
  → 0.74, `c-slot-redef` 1.72 → 0.96 (in a script 123 → 98 and 107 → 50
  ms), `bench/clos.lisp` 1.25 → 1.18; a simple vector constant of a whole
  program is a global of type `(ref $vector)` as well; and `char=` /
  `char/=` of two values known to be characters (a declared parameter, a
  constant) is a `ref.eq` of the immediates, without their checks
  (`tools/bench-lem.sh` the same: 1.24 against 1.29 in one interleaved
  pair).
* **Calls between modules** (fasl or REPL code into the runtime, or into
  another file's module) go through the function cell into another Wasm
  instance. Measured with `--no-wasm-inlining` (2026-11), a direct `call`
  costs what a `call_ref` through the closure does; what a direct call within
  one module gains is the engine's inlining of the callee (2–3 times for
  small functions of `&optional` or `&key` parameters). Importing the
  runtime's functions into each module (a call of an imported function of
  another instance) would not be inlined either, so it is not done; what
  helps is a copy of the callee in the caller's module (`inline` sources,
  the small functions of `COMMON-LISP`, a file as one module). Such a call
  of a function of one argument returning it takes 2.6 ns in a loop (fasls
  compiled in the image, 2026-11-21), SBCL's 1.5: the caller loads the
  symbol, its function cell, the closure's code, the entry and the engine's
  internal function before the call (nine loads), and the callee sets
  `mv-count`, an imported mutable global (two loads and a store), 0.7 ns of
  the 2.6 (a callee that does not: 1.9). `mv-count` as a word of the linear
  memory that every module imports (one load and a store) took 2.3 ns in
  that loop but gained nothing in Lem's code (`tools/bench-lem.sh` 1.01 of
  its time, interleaved), so it was not kept; what would remove the store
  is returning the count with the value (a Wasm function of two results),
  a change of every function's type.
  the small functions of `COMMON-LISP`, a file as one module).
  The call through the function cell itself (2026-11-21): a function of
  one file called in a loop from another file's fasl (a fixnum fast path,
  too large to be copied) costs 3.45 ns an iteration (two arguments 3.70;
  a call to the same function in the caller's module and through a
  variable costs the same, so another instance costs nothing of its own);
  SBCL's full call in the same loop 2.65 (3.45). The sequence is the
  symbol's global, its function cell and the null test, the closure's
  `$code`, the entry, `call_ref`, and the callee's store of 1 into the
  imported `mv-count`. Upper bounds measured with throwaway compiler
  changes, on Lem (`tools/bench-lem.sh` on a held-out input, Clysm alone,
  best of 5–6 interleaved runs on one slot, geometric means of the time
  before): the callee's entry and closure cached in two globals of the
  calling module (one load for the target, filled at the first call and
  *never refreshed*, so not a valid implementation: an invalidating one,
  a registry of the modules caching a symbol cleared when its function
  cell is set, would have the same call site) 0.983–0.985 (the loop
  above 2.90 ns); the same cache guarded by comparing the function cell
  with the cached closure (valid as it is: a closure's code never
  changes) 0.98–1.00 (2.75 ns); no store of `mv-count` in the functions
  of Lem's files (multiple values lost) 0.992–0.993; both the unguarded
  cache and no store 0.947–0.948, but no convention keeps multiple values
  without most of that store (a reset after each call whose values are
  dropped, or a count returned as a second Wasm result by every
  function). Calling an imported function (fixed at instantiation, so it
  would need the same guard) loads its target from the instance as the
  cached global does. None of these was worth its cost (under 2% each),
  so the call sequence was not changed.
  What calls cost against SBCL (2026-11-21, probes loaded with `clysm
  script`, each callee another form's function, `sbcl --script` of the
  same file, best of three on one slot, 20 million calls in a loop): a call
  through the symbol of one to three arguments 66–75 ms against SBCL's
  67–73, `funcall` of a closure or of a symbol 67–80 against 71–85, a
  callee returning two values to `multiple-value-bind` 98 against 104, a
  call site cycling through four closures 77 against 94: the call itself is
  as fast as SBCL's full call. What was slower were the entries of
  functions with other parameters: with `&key` 206–252 ms against 75–82
  (the shared stub made the argument vector and the generic entry searched
  it; 88–94 with the keyed entries, see Calling convention), `&optional`
  with all of them given 89 against 73 (73 now that the entry is the main
  function; with fewer given still 88: the entry's tail call), `&rest` 100
  against 71 (its list). esrap (`a-esrap`, 1.9–2.0 times SBCL) is no
  call-heavier than SBCL's profile of it (the same functions take the same
  shares of the time, each about twice SBCL's), and the calls between its
  files are not what is left: its fifteen files concatenated and compiled as
  one file (one module, the calls between them direct, small functions
  copied, types flowing between them) take 91–97 ms against 109–120 as
  ASDF loads them, and the same one module with every call between its
  functions through the function cell (`known-global-lam` answering NIL:
  what one module per system with calls kept through the symbol would
  give, the engine inlining the targets of `call_ref` within it) 104–113.
  So one module per system would gain about 4% on esrap unless it also
  made the calls between files direct (as within a file: the redefinition
  of a function from another file would then not reach them, unless each
  such call tested the function cell as the guarded copies do), and
  `--no-wasm-inlining` takes it from 114 to 143 ms: what is left of esrap's
  factor is the code inside its functions (structure type tests of included
  structures, the lazy productions' closures, allocation), not the
  convention. The keyed, optional and arity entries left `bench/apps.lisp`
  (0.99 of the time, three interleaved rounds), `bench/libs.lisp`,
  `closures`, `functional`, `suite` and `programs` (compiled and in a
  script) unchanged within the noise, `p-call-conv` 0.84 and `k-higher-order`
  in a script 0.83; the stubs they replace were under 0.1% of the profiles
  of the library and Lem benchmarks.
* **Libraries** are within 2.5 to 3 (`l-split-sequence`, `l-babel`):
  the remaining time is mostly in the runtime's sequence functions called with
  keyword arguments of variables (`position` with `:key` and `:test`
  functions), cl-ppcre's matcher closures calling each other through
  `funcall`, and calls across files and into the runtime, which are calls of
  another Wasm instance (not inlined by the engine).
  Regular expressions (cl-ppcre, 2026-11-20): `char-equal` of two
  characters, `char-upcase` and `char-downcase` are inline for ASCII (a call
  for the others), `search` with `:test #'char=` or `#'char-equal` on
  strings compares the characters inline, constants are known at compile
  time (above, `defconstant`), and `scan` with three keywords uses a wide
  keyed effective method (above, CLOS). The time of `a-ppcre-tokens` went
  65 → 42 ms (2.4 → 1.56 times SBCL), `l-ppcre-replace` 248 → 200 (2.0 →
  1.54), `l-ppcre-scan` 1.45 → 1.34 times SBCL; probes written for the
  change (log lines with register groups, C-like tokens, split and replace
  by a function, case-insensitive scanners made at run time) 2.0 → 1.21
  times SBCL in geometric mean (`r-dynamic`, case-insensitive `\b...\b`,
  105 → 39 ms, 0.4 times SBCL). In Lem (`tools/bench-lem.sh`, the same
  slot): `l-syntax-scan` 312 → 218 ms (2.5 → 1.74 times SBCL), `l-search`
  52 → 35, geometric mean 2.00 → 1.75. The matchers' closures call each other
  with a cast and `call_ref` of the entry (`next-fn` declared `function`);
  what remains is the closures' own work: special variables (`*string*`,
  `*end-pos*`) read with a test of their binding, positions boxed between
  closures, character classes as lists of test closures. Ruled out
  (2026-11-20): typing the closure's field of a variable declared
  `function` (the fields of `:envcloN` are mutable, `labels` sets them, so
  a subtype cannot refine them; it would take struct types of their own,
  which the runtime's operations on closures of any shape do not know: a
  cast less per call, not measured); a class as a list of closures (8 per
  character in `[^\s()"';`,]`) runs at 1.35 times SBCL in a probe without
  cl-ppcre, the engine inlining the closures at the `call_ref` (they do
  not appear in a profile), and the second `endp` test of each iteration of
  `loop for x in` (at the step and again at the top) was left out with no
  change in time (the engine drops it), so neither was kept.
* **Non-local exits** are returns with the exit pending, tested after
  every call that is not a tail call (above, "Non-local exits"): a throw
  from a closure one frame below its `catch` costs what SBCL's does
  (`t-throw-closure` 1.0, was 13; 20 frames below, 0.43, was 4.4; in the
  image 21 ms instead of 280). The test costs a few percent in recursion
  through `funcall` (`h-fib-funcall` about 1.0 times SBCL); direct calls
  of functions that cannot exit are not tested.
* **Generic functions**: dispatch on EQL specializers (2.6 times, was 4.4:
  the entry of a class with EQL specializer objects calls a guard function
  shared by all generic functions, which compares the objects, kept in a
  vector, and then calls the method; call sites cache that entry for the
  class of the argument like any other (`%gf-site-value`), where they
  used to call the generic function; comparing the EQL
  objects in each generic function's own dispatch (out of line or inline),
  or letting that dispatch call the method the guard selects, were slower
  still, 2026-11; a copy of the guard for each position of the argument
  gained nothing), on many built-in classes with two specialized arguments
  (`c-builtin-eql` 3.5, the entries searched one after the other) or on
  many classes at one call site (2.1–3.7: the entries
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
  the number of classes; the first entry of the class's vector is now
  compared inline. Moving a generic function of one specialized argument
  to the classes' vectors past 8 entries instead of 32, 2026-11:
  `c-megamorphic` 0.94 in a whole program, the same in fasls, Lem's code
  3% slower). Generic functions with `&optional`/`&rest`/`&key`
  (`c-gf-lambda-lists` 1.3, was 2.6) and `make-instance` with a class that
  is not a constant (`c-make-instance-plain` 1.6, was 2.7) are below. Slot
  accessors are faster than SBCL's (`c-accessors` 0.53, was 0.83;
  `c-with-slots` 0.74, was 1.02): a call site of a reader, of `setf` of an
  accessor or of a `:writer` (of a class or a condition) keeps the slot
  locations of four classes and reads or writes the slot itself
  (`%site-slot-read`, see "The machine code of the call sites" below), and
  `slot-value` with a constant name compares the four entries of its cache
  and that of a redefined class in one access too (`%cached-slot-read`).
  The slots of a class redefined with other slots after it had instances
  are checked by their number (`c-slot-redef` 0.96, was 1.7; see the
  limitations).
* **Integers between 2^29 and 2^63** are boxed when they are stored in data
  structures or passed between functions of unknown types (fixnums in
  SBCL; a parameter or variable updated by integer arithmetic keeps them
  in an i64 local, and a function of such integers of known types called
  directly takes and returns i64s, see "Raw clones"). A box is a `$bignum`
  holding the value, made by one `struct.new` (about 2 ns with the work
  around it); storing a new box into an object that has survived a
  collection costs about 2 ns more (the engine's write barrier), so a
  structure slot updated with such integers is 7 times SBCL's time, a
  vector element 3.5 times. The generic helpers take them before the
  general code: the front helper of `+ - < =` decodes two integers within
  64 bits after its fixnum and double paths (a sum of a boxed integer and
  a fixnum 4 ns, was 6), the general arithmetic helper no longer repeats
  the fixnum test and tests complex numbers only after the decode failed,
  and EQL and EQUAL hash tables hash such a key without two calls. Probes
  of ordinary code over such values (Unix times, nanoseconds, 32-bit
  hashes, sums of lists, counters, structure slots, hash keys; 17 probes
  written for the change) went from 2.9 to 2.0 times
  SBCL, functions of 32-bit hashes that are not inlined (murmur3's steps,
  declared or masked) from 2.4 to 0.6. `a-local-time` (`bench/apps.lisp`)
  went from 1.42 to 1.36 only: its time is in splitting the string (`member`
  of characters with `char-equal`), the time zone's lookups and FORMAT,
  not in its integers (the integer helpers are under 3% of its profile;
  its seconds and days pass through CLOS slots and calls of unknown
  types, where they are boxes). A leaf of fused arithmetic that
  is a bignum holding its value is decoded by a call (`:wide-i64`): the
  test of a bignum inline made fixnum code slower (`p-queens` 149 -> 168
  ms, `p-fixnum-vector` 56 -> 73) although it made the sum of a list of
  such integers twice as fast.
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
  passes recorded outside a loop is undone. Declared types of variables
  are those of their values from where they are bound, and at `(safety 1)`
  and more every declaration the code relies on is checked where the value
  is given, as SBCL checks it, so that a wrong value is a `type-error`, never
  a Wasm trap at a cast (2026-11-21; before, only integer types were
  checked, and a `double-float`, `simple-vector`, `cons`, `function` or
  structure parameter given another object, or a `fixnum` parameter of a
  function called from code loaded into the image, trapped):
  * a parameter: the prologue tests the object before it is unboxed
    (`param-check-code`, `decl-check-code` in `src/codegen.lisp`: one
    `ref.test` of the storage kind, or the fixnum test and the range, or a
    bignum's i64 for a range beyond the fixnums; `sys::%check-declared-type`
    only for what the inline test does not decide, a bignum beyond 64 bits
    or an array of any rank); a raw copy tests only a class wider than the
    declaration; a call of a known function (of the same file or program)
    whose argument the caller's types prove of the declared type calls a
    copy that has no test of it (`call-arg-classes`: the class `:checked`,
    `*proven-params*`; `bench/apps.lisp`, base 669b52c: `a-local-time` 43 →
    42 ms, 41 before the checks, `a-ppcre-tokens` 46-47 → 44-45, `a-esrap`
    115 → 109); an inline copy tests the argument where it binds it
    (`inline-checked-arg`, left out where the argument's type decides it);
    a parameter declared a structure class is tested at the start of the
    body (`struct-param-checks`: `typep` of `(sys::%untyped x)`, which the
    declaration does not decide);
  * a `let`, `setq`, a free declaration's reference, a special variable of
    a proclaimed type, and `(the type x)` whose value is used but not
    returned (`checked-value`, `checked-the-form`; the test is left out
    where the type of the value shows it holds, so `(the fixnum (+ a b))`
    of small ranges and a variable bound to a constructor cost nothing);
  * the types tested are those `checked-decl-p` gives: integer types,
    structure classes, and the types `decl-check-ty` says the code relies
    on (floats, characters, symbols, lists and conses, functions, simple
    arrays of a storage kind; not types with other objects, `vector` or
    `(or null hash-table)`, whose values the code handles generically).
  A slot declared of a structure class or of a simple array of one kind of
  storage (`simple-vector`, `simple-string`, `(simple-array (signed-byte
  32) (*))`, a type defined by `deftype` as one) is checked where it is
  written: by the constructors, the SETF of the accessors (the function and
  its inline expansion), `(setf slot-value)` (`sys::%struct-slot-store-check`;
  a structure's slot locations are not cached by `slot-value`'s inline
  cache), as SBCL does (`%slot-store-check-form` in `runtime/struct.lisp`:
  the structure's predicate test, or `simple-array`, `simple-vector`,
  `simple-string` and `(simple-array * dims)`, the element type left out
  since `typep` of `(or (eql 0) chunk)` asks `subtypep` at run time; at
  `(safety 0)` nothing). The slot's entry in the structure's description
  records the check (`%slot-with-check`: the kind and the type checked), and
  only a checked structure-class slot is read as of its class
  (`struct-slot-node-ty`; a slot of a structure defined later is not); the
  compiled read of an array slot tests the storage kind too and signals a
  `type-error` for anything else (`struct-slot-check-code`, nothing at
  `(safety 0)`), which also covers a type defined by `deftype` after the
  structure (`tests/cases/struct-array-slots.lisp`,
  `tests/cases/struct-class-slots.lisp`, `tests/cases/declared-checks.lisp`).
  A slot declared of an integer type (`(unsigned-byte 32)`, `(integer 0
  100)`, `(mod 10)`, `bit`, `fixnum`...) is checked the same way
  (`%slot-integer-type`; a `fixnum` slot is checked as SBCL's 62-bit
  fixnum, since code written for SBCL stores larger integers in one), and
  read as of its range when that is within the fixnums (an i32; a wider
  one gains nothing: i64 arithmetic whose result is boxed by a call costs
  more than the generic way of small integers, `d-particles` took 1.2
  times as long with its `fixnum` slot read as an i64); an `&aux`
  variable of a BOA constructor without a value leaves such a slot 0.
  The reads of an included slot whose type is restated trust the
  included type (its accessors write the slot of the including
  structure's instances), and its constructors check both. What the checks cost (2026-11-21, against
  ee61f08 on the same slot, alternately): `bench/apps.lisp` 1.30-1.33 both
  (`a-local-time` 41 → 43 ms, `a-ppcre-tokens` 46 → 46-47, a fixnum
  parameter's test inline rather than a call of `$fixnump`: 49-50 with the
  call), `bench/declared.lisp` (`safety 0`) 1.05 both, `bench/alloc.lisp`
  1.13 both, `bench/programs.lisp` 0.91 both, Lem (`tools/bench-lem.sh`)
  1.47 → 1.43 (noise). An accessor of an object known to be of its class (by such a
  declaration, by a constructor, a type test or an access before) is the
  `struct.get`/`struct.set` of the field — no load of the class, no layout
  comparison. A variable declared of the type and bound once has a
  *shadow* local of the structure's Wasm type, set where it is bound (the
  object cast, or null for an instance of an including structure that has
  a slot vector, which then takes the call of the function; a variable
  bound to one with a shadow, as SETF's, copies it), and its accessors
  test the shadow for null (`struct-shadow-init-code`); other declared objects are tested with `ref.test` at each
  access, and so are the objects of a structure of typed slots, whose
  instances may have a slot vector. An object of a structure of field
  slots whose class is proved (by an access before, a constructor, a type
  test, a checked declaration or slot), or declared at `(safety 0)`, is cast, as every instance of its
  class is of its Wasm type (big structures, above; before them an
  including structure of more than 32 slots had a slot vector, and the
  cast trapped on it, `tests/cases/big-structs.lisp`). An inline copy of
  a function keeps the declarations of its variables. A declared integer type is
  checked by tests the type of the value decides where it can
  (`known-test-result`: `(<= 0 v)` and `sys::%integerp` of a value of a
  known range), so a `setq` of a `(unsigned-byte 32)` variable to a value
  computed in an i64 costs nothing. A declaration that would leave a value
  with no type (the NIL a variable is bound to before its first value)
  leaves it its own type. At `(safety 0)` nothing is checked (see "What
  `(safety 0)` does" below). **Unboxed representations**
  (`src/typed.lisp`): fixnums and integers within 64 bits, double and single
  floats, complex numbers of floats (two locals), references to specialized
  vectors, two-dimensional arrays with their storage and dimensions in locals
  (a variable bound to another one, as `(incf (aref c i j))` binds one,
  in an innermost loop uses those locals instead of loading the header again).
* **Arithmetic on values of unknown type** (undeclared code: structure
  slots, list elements, parameters; `src/prims.lisp`, `src/typed.lisp`).
  A tree of `+ - *` (and `1+ 1- logand logior logxor ash`) is computed in
  i64 when its operands are integers within 64 bits, and the trees of
  `+ - *` try f64 next when they are doubles; a tree of `+ - * / sqrt` with
  a double among its operands (a double constant, a value known to be one,
  an unboxed double variable) is computed in f64 first. Operands are
  evaluated in order and tested; when one is not of the kind or a step gives
  an infinity, a NaN or a negative square root, the generic operations compute the tree again from the operands
  already evaluated, so the conditions are those of the generic code. Only
  the root is boxed. A `let` variable bound (or, when assigned, updated) by
  such arithmetic and used twice or more as an operand keeps an f64 beside
  its eqref local (null while the f64 holds the value); the first other use
  makes the double once. `incf` of a variable by arithmetic on variables is
  one tree with the update. `(mod x k)` and `(rem x k)` by a constant are
  operations of the trees (a `mod` by a power of two under a mask: the low
  bits), and an operation with a constant beyond the fixnums
  (`#xFFFFFFFF`, `(expt 2 32)`) is a tree by itself, as the inline fixnum
  path cannot apply to it; a tree of a known 64-bit type wanted as an i64
  (`(let ((y (logand x #xFFFFFFFF))) ...)`) gives its i64 directly. `mod`
  and `floor` of a known 64-bit integer by a power of two are a mask and a
  shift; `ash` by a count of a known range is a shift. A
  `multiple-value-bind` of `floor truncate ceiling round` of a value of
  unknown type (or of a tree: `(floor (+ sec offset) 86400)` with
  parameters declared `integer`, as local-time writes it) by a constant
  fixnum divides in i64 inline (`spec-divmod-code`), the generic division
  otherwise. An *integer accumulator* (a `let` variable that is assigned,
  not captured, with an i64 beside its eqref local, null while the i64
  holds the value) is a variable known to hold integers that is updated by
  integer arithmetic at least as often as it is otherwise looked at, or
  (`note-dual-var`) a number of unknown type updated in a loop only by
  arithmetic with an operand known to be an integer, more often than it is
  looked at there: a counter from a value of unknown type, `(loop for k
  from start ...)` (one also updated by values of unknown types is not:
  they are fixnums most often, and the i64 and f64 attempts of each update
  made `b-catch` 8% slower). Its references as leaves of arithmetic read the i64
  (they do not count as uses), a comparison with it is done in i64
  (`dual-cmp-code`), and its update tries doubles after integers when it
  may hold one. A variable that may be something other than a number (the
  NIL `for j =` of `loop` starts with) is not one: `p-loop-macros` 66 -> 78
  ms. Trees of operands of unknown type with no
  double among them are not tried in f64 first: integer code (array
  elements summed) would pay for it. `truncate floor ceiling round` of two
  fixnums, `evenp oddp`, and `(reduce #'+ list)` (and `* max min logand
  logior logxor`) are inline. `bench/arith.lisp` measures this; an n-body
  over undeclared structure slots takes a third of its earlier time.
* **Division of floats** (`floor ceiling truncate round`, the `f` versions,
  `mod rem`) computes what SBCL does, in the inline code, the helper
  `:divmod` and `%float-divmod` alike: the float quotient rounded for the
  mode (`f64.trunc/floor/ceil/nearest` of `x/y`) and the remainder `x - q*y`
  (singles: every step rounded to single, an integer operand converted to a
  single first; a zero quotient of a negative quotient is -0.0 for `ffloor`
  and the others; checked against SBCL on 40000 lines of random, halfway,
  near-2^51/2^63, tiny and huge operands, all equal). It was a truncation
  adjusted by the remainder, which took about 30 instructions more for
  `round` and gave other low bits of the remainder than SBCL. When only the
  quotient is used, the remainder is not computed (and the quotient is
  tested for an infinity or NaN). The integral f64 becomes an i64 by adding
  1.5 * 2^52 and taking the low bits (below 2^51), not by
  `i64.trunc_sat_f64_s`: V8 makes that conversion of a scratch register
  whose upper half each such instruction also reads, so every iteration of
  a loop waited for the division of the one before (`round` of `x/3` in a
  loop: 6 ns, now 2.4). The quotient, as a leaf of fused integer arithmetic
  (`(incf s (round x 3))`) or bound to a variable (`let`, the first variable
  of `multiple-value-bind`) is kept as an i64 beside a null eqref, like an
  integer accumulator; `incf` evaluates a division after reading the
  variable as it does arithmetic, so the update is one tree. The slow
  branches are hinted unlikely (V8 then keeps the loop's values in
  registers). `programs.lisp` `p-float-round` 156 → 98 ms (SBCL 56).
  The front of the division helper divides a single by a single or a
  fixnum with the f32 instructions (an undeclared `(round x 2)` of singles
  in a loop 113 → 68 ms, SBCL 49). A `multiple-value-bind` whose value is
  not used compiles its body for effect: before, `(when (> r 1d0) (incf
  s))` as its last form made the integer of the accumulator `s` (a bignum
  beyond 2^29) at every iteration and left it in the eqref local, so the
  next update decoded it again (`p-float-round` 98 → 82 ms,
  `p-integer-math` 62 → 50). An integer leaf of fused arithmetic that is not
  a fixnum is decoded by one call (`:wide-i64`, -2^63 for anything that is
  not an integer within 64 bits; more than two digits fail at once), not by
  inline tests and two call sites in the loop.
* **Specialized arrays**: every upgraded element type (bit, the integer types
  of 7 to 64 bits, single and double floats, characters) has its own Wasm
  array type; `aref` on an array whose element type is known (from
  `make-array` or a declaration) is inline with a bounds check, and the
  generic function is only called to signal the error.
* **`loop for x across v`** takes the length of V once (V may not change
  while it is traversed, CLHS 3.6) and reads an element as `(if
  (%simple-string-p v) (schar v i) (if (simple-vector-p v) (svref v i)
  (aref v i)))`: V is not assigned, so each branch knows its type and the
  read is inline for a simple string too (the element read of an array of
  unknown type tests a simple vector only, and called `aref1` for each
  character of a string), and for a vector of a known type the tests fold
  (`known-test-result` decides `%simple-string-p` and `simple-vector-p`).
  An FNV hash of a string of unknown type: 66 -> 27 ms (SBCL 47),
  `p-hash-fn` 67 -> 43.
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
* **Two-dimensional arrays of unknown type** (undeclared code; 2026-11-21).
  A function whose parameter is the array of `(aref a i j)` (or its `setf`)
  in a loop has a version for simple arrays of element type T (the class
  `:md2` of the versions of a function, below): its entry tests the
  argument with one helper (`:simple-md2-p`: a header of rank 2 over a
  simple vector, no fill pointer, neither adjustable nor displaced, so that
  the header never changes) and the version holds it as `(:md2 :vector)`,
  storage, dimensions and offset in locals loaded once; any other array
  takes the general version, whose accesses test the header each time as
  before. A `let` variable of unknown type used that way in a loop of its
  body (a structure's slot, a function's result) compiles the body twice
  behind the same test (`md2-version-var` in `src/codegen.lisp`; bodies of
  at most 1200 nodes without lambdas; variables bound to it in the body,
  SETF's temporaries, share its locals). A subscript of unknown type (a
  counter of `(dotimes (i n))` with N from a slot) is tested to be an
  `i31` inline instead of calling the helper: a character or a negative
  fixnum is beyond any dimension compared unsigned. `(make-array (list n
  n) ...)` is left a call of MAKE-ARRAY (it was the inline one-dimensional
  form, which hid its type), so that a local matrix is known to be a
  simple two-dimensional array. `b-matmul` 147 → 91 ms (2.16 → 1.34 of
  SBCL), the same matrix product given its arrays as arguments 156 → 89
  (SBCL 280: it does not know their type), a grid stepped through a
  structure's slot 41 → 29, a sum with `array-dimension` bounds 10 → 7.
  Fusing `(incf s (* (aref ..) (aref ..)))` into one i64 tree (letting
  INCF read the variable after array elements) measured no faster and was
  left out. A special variable used so in a loop (`(aref *grid* i j)`) has
  its value tested on entry to the outermost such loop and, when a simple
  array of element type T, kept in locals with its storage, dimensions and
  offset (`sref-md2-wrap`); each access compares the variable's value with
  that header (one load and a `ref.eq`: the loop, or a function it calls,
  may assign or bind the variable) and takes the access of an array of
  unknown type when it differs: a grid stepped through `*grid*` 38 → 30 ms
  (SBCL 53). `array-dimension` (and `fill-pointer`,
  `array-row-major-index`) is known to return an index, so that the counter
  of `(dotimes (i (array-dimension a 0)))` is a fixnum: a sum over such
  bounds 8 → 5 ms, three reads of neighbours in such loops 42 → 19.
  Measured and not done: computing the row offset `i * dim1` once per outer
  iteration (written so by hand over a simple vector: 27 ms either way) and
  leaving out the test of the first subscript (unsafe, as a bound: `b-matmul`
  and `d-matmul` the same): V8 hoists or hides both, so neither was worth
  the code.
* **EQUALP** (2026-11-21): the function settles conses, simple strings
  (compared in its own loop), characters, fixnums and doubles without a
  call, and compares a fixnum, a single and a double float as doubles
  (exact for these; no generic `=`); it has no versions for numbers any
  more (they cost two tests on every call). Structures of one class whose
  slots are fields are compared by one helper (`:sinst-equalp`: a
  `br_table` on the count of fields into a chain of `struct.get`s, calling
  EQUALP only for slots that are not EQ), vectors with a fill pointer or
  displaced over their storage without the generic `length`/`vectorp`,
  hash tables in a function of their own. `%instance-size` is one helper
  (`:instance-nslots`); `:sinst-equalp` takes the count of fields from the
  class (an instance of another count: slot by slot), 14 → 13 ns. The
  EQUALP hash of a simple vector reads its elements directly, and a vector
  of characters hashes as the string it is EQUALP to (it did not: `(gethash
  (vector #\a #\b) h)` missed the key `"ab"`). `v-equalp-mixed` 2.16 → 1.22 of SBCL; a structure
  of two EQ slots 21 → 14 ns, a vector with a fill pointer against a
  simple vector 48 → 15 ns.
* **Structures**: up to 32 slots in fields of their own (see Object
  representation): a constructor is one allocation, an accessor an exact
  type test, a layout comparison and a `struct.get`; `typep` of a structure,
  `typecase` and the predicates test the type and the layout inline, and the
  included structures in constant time (`+class-inherits+` by depth); a
  predicate that fails on a structure of fewer field slots answers at the
  type test. `typecase` of a variable tests the variable itself, so the
  accessors in the clause taken need no check; `copy-structure` copies the
  fields (`:sinst-copy`). `bench/arrays.lisp`, two rounds: `a-struct-int`
  (predicates and accessors over included structures) 2.7 → 1.65,
  `a-struct-points` 1.07 → 0.72, `a-struct-dispatch` 1.58 → 1.06,
  `a-heap-sim` 0.77 → 0.58, `b-struct` 0.86 → 0.75; allocation of a
  three-slot structure 1.6 times faster, reads 1.35 times. A reader loop
  compiled to a fasl and loaded into the image: 2.0 → 1.7 times SBCL.
  Still slow: `a-nbody` (1.2; doubles in slots without declarations are
  boxed and their arithmetic generic).
* **Included structures**: the accessor of a structure on an instance of
  one including it (esrap's `result-%position` on a `successful-parse`)
  finds the class in the vector of included structures that the
  constructor put in the instance's slot vector field: one load from the
  instance instead of the layout's slot vector, its `+class-inherits+` slot
  and two type tests (when the field holds the empty vector, the Lisp
  function or `:struct-typep` answers). Probes compiled with `compile-file`,
  against the build before on the same slot (accessor of the parent on a mix
  of two children; `setf` of a parent's typed slot and read; predicate of an
  intermediate structure): 2.3 → 2.0, 2.6 → 1.8, 2.1 → 2.0 times SBCL.
  esrap (`a-esrap`, loaded into the image with these and the FTYPE checks
  below) only 2% faster in interleaved runs: its time is spread over calls
  between files, closures and allocation.
* **FTYPE proclamations** (`declaim ftype`, `*declared-ftypes*`, and the
  name's plist `sys::%ftype` for fasls loaded later): a function defined at
  top level checks its required arguments of the types on entry
  (`ftype-checked-function`, into variables declared of them), and the
  value of a call of one whose result is `(values TYPE &optional)` is
  checked where it is used as an argument, a variable's value or a test
  (`convert-value`; a call in tail position stays a tail call). Only types
  that TYPEP takes and of which the compiler makes something are used
  (`ftype-check-spec`: DEFTYPEs expanded, `(function ...)` as `function`,
  also inside OR/AND/NOT; VALUES, SATISFIES and types not defined yet are
  not checked). The checks are the integer tests of declared variables
  (which the compiler leaves out where the type is known: a test of its own
  for fixnum ranges, not left out, cost babel's encoders 40%) and a
  `ref.test` for vectors and strings; the type error, also that of a
  declared integer variable, is signalled out
  of line (`%signal-type-error`: a function whose only other calls are
  helpers needs no code for non-local exits, which cost a reader of a
  vector a third of its time). esrap's packrat cache (positions, chunks)
  computes in `i32` and indexes the chunks directly; a probe reading a
  vector through an FTYPE'd function 2.0 → 1.5 times SBCL.
* **`make-array`** of one dimension of element type T with only
  `:initial-element` makes the vector where the call is (`array.new`) when
  the dimension is a fixnum: a probe making vectors of 256 and 41 elements
  1.5 → 1.1–1.3 times SBCL. (Specialized element types the same way lost
  the type of the vector where the dimension's type was not known: a
  double-float vector program 20 times slower; they stay calls.) `simple-vector-p`,
  `double-float-p` and the like are primitives, so `(typep x '(simple-vector
  41))` narrows X itself.
* **Structures with `double-float` slots** (declared `:type double-float`,
  up to 32 slots) have a Wasm type of their own, made from the kinds of
  their slots (an `f64` field for each such slot), a subtype of the type of
  their slots but the last (`tshape-type`, `src/typed.lisp`), so that an
  including structure is of a subtype. Each module using one defines it.
  The constructor stores the numbers unboxed (signalling a `type-error` for
  another value), a reader leaves an `f64` where the compiler wants one,
  `setf` stores one (no allocation). Generic code (slot access by index,
  `copy-structure`, `equalp`, printing) uses functions DEFSTRUCT puts in
  the class (`+class-shape-ops+`); a heap image saves such a structure with
  a slot vector, which its accessors take by their general way. With it,
  the version of a function taken for double arguments is also made when a
  parameter meets a double slot or an operation on one (and functions using
  structure accessors may now have versions), and the square of a double is
  known not to be negative (its root is a double): an n-body loop over
  structures of declared doubles runs as fast as SBCL's (880 → 115 ms;
  SBCL 114).
* **Versions of a function**: a function with a loop or a recursive call
  whose parameters are used as numbers tests its arguments on entry and
  continues in a copy compiled for fixnums, or else in one compiled for
  doubles (the parameters used as numbers but not as integers: not with
  integer-only operations, as indexes or compared with an integer variable).
  This is what callers that do not know the function get (another form in
  the image, another file): Simpson's rule over doubles loaded into the
  image went from 120 to 23 ms. Small functions are
  copied into their callers — in the image with a test that the function has
  not been redefined.
* **Closures**: one allocation of the code (shared per lambda), the name
  and the values of the free variables in fields of the closure; a
  `labels` function finds its own closure in its first parameter.
  Interleaved runs against the previous layout (a struct of six entries
  and an environment vector): `bench/closures.lisp` 0.82 of the time
  (`k-cps-tree` 0.49, `k-cps-fib` 0.59, `k-matcher` 0.69), also with the
  code loaded into the image (0.82), `p-labels` 0.82, `p-closures` 0.88,
  `h-closures` 0.83; a call of an unknown function loads the code first,
  which costs calls through symbols and generic functions a little
  (`h-fib-generic` 1.03–1.09, `h-fib-symbol` 1.07–1.15 of the time before;
  the entries for one and two arguments copied into the closure as well
  gained nothing on the first and 5% on the second, so they are not). `apply` through `entryA`:
  `k-funcall-args` 0.77, `k-higher-order` 0.86, `p-call-conv` 0.87.
* **Functional code** (2026-11-21; `bench/functional.lisp`, combinator-style
  code written for it: matchers of closures over lists with backtracking through
  `(or (funcall k ...) ...)`, CPS walks, parser combinators returning two
  values, generators, memoization with `equal` tables keyed by argument
  lists, `compose`/`curry` pipelines, `every`/`some`/`mapcar` of lambdas
  over several lists). At steady state most of these are within 0.7–1.3
  times SBCL; the call of an unknown closure (a cast, two loads and a
  `call_ref`, the test of a pending exit) was not where the time went.
  What changed:
  - The compiler macros that expand `mapcar`, `mapc`, `every`, `some`, `map`,
    `maphash` and `member :test` into a loop at the call site put the lambda
    inside a `do`: a `(return x)` in the lambda was taken by the loop's own
    `block nil` (`(block nil (maphash (lambda (k v) (when ... (return v))) h)
    0)` returned 0). They use `%do`, a `do` whose block has a name of its own.
  - `every`, `some`, `notany`, `notevery` of a lambda or `#'name` over two
    or three sequences are a loop at the call site when they are all lists,
    and for two simple vectors (`%quantifier-lists-expansion`); `map` to
    `nil` or a list over two sequences the same (`%map2-expansion`); any
    other sequences call the function out of line, where three sequences no
    longer make an argument list for `apply` at each element and two simple
    vectors are read with `svref`. Two simple vectors through `every #'<`:
    5.4 → 1.5 times SBCL; `some` of a lambda over three lists 2.6 → about
    1; a probe of several of these 2.4 → 1.4.
  - `eql` of a value whose type the inference knows holds no boxed number
    (a fixnum, a character, a symbol, ...) is `ref.eq` (`ty-eq-comparable-p`),
    not the helper: the key compared in `(maphash (lambda (k v) (when (eql k
    key) ...)))`, `t-return-maphash` 2.25 → 1.0. `equal`'s inline test for
    objects only `eq` makes equal is one `ref.test` of i31 and one of symbol.
  - Strings of 8 characters or more are hashed (`sxhash`, `equal` tables,
    symbol names: `string-hash` in the compiler, `string-hash-loop-code` in
    `src/prims.lisp`) as four FNV-1a hashes of every fourth character,
    combined at the end, instead of one chain of multiplications: twice as
    fast on 50 characters (a million `sxhash` 34 → 18 ms, SBCL 31), the
    distribution of buckets unchanged on sets of keys (numbered names,
    English words, the names of every symbol). `k-memoize` (a key of two
    strings of 50 characters and two fixnums) 145 → 110 ms.
  - `map` to a vector or a string of one or two sequences, `map-into` a
    simple vector from one or two, and the quantifiers and `map` to `nil` or
    a list over two sequences of any other kinds (a list and a vector, a
    string) are a loop at the call site over *cursors*
    (`%cursor-loop-expansion`: a list followed by its tail, a vector read
    from its storage; one copy of the function's body for every mix of
    kinds). Where no result has to be allocated first, lists are not
    measured before the loop (`%lazy-count-form`: only the vectors bound the
    count, a list ends the loop at its end). Other results of `map-into`
    (a list, a vector with a fill pointer) go out of line. One thousand
    elements, best of two runs, before → after (SBCL): `map 'vector` of two
    simple vectors 14 → 8 ms (42), `map-into` of two 19 → 8 (36), `map
    'list` of a list and a vector 20 → 14 (31), `some` of a list and a
    vector 13 → 8 (19); `v-map-every` 2.16 → 1.76 times SBCL (the rest is
    mostly making its data: `loop ... collect` of 10000 conses is 2 times
    SBCL).
  - A variable both captured and assigned has its cell in a local of type
    `(ref null $cell)` (location `:cell`): a `let` or parameter that makes
    the cell, and a closure that captures it, which casts the field of its
    environment once on entry (`setup-free-vars`) instead of at every
    reference. A generator counting in a closed-over variable: 249 → 196 ms
    (SBCL 102).
  Not changed, measured: tier-up. A combinator probe of 10 repetitions
  (`p-rx-strings`: regular expressions compiled to closures at each match) is 3 times SBCL, 1.08 at 300 repetitions; with
  `--no-liftoff` 2.2, with a lower tiering budget slower. Generators stay
  at about 2 times SBCL; returning `(values x t)` is not the cost (the same
  generators returning a sentinel instead take as long). The machine code
  of the counting closure (`--print-wasm-code`) is mostly the fixnum tests
  of `<` and `1+` on the boxed values (an i31 is a fixnum or a character,
  so the range is tested), the call and the store, whose write barrier is
  skipped for an i31 at once.
* **Calls**: direct calls between the functions of a program; keyword
  arguments passed in fixed slots (in the image too, checked against the
  layout of the function's current definition); structure accessors inline.
  The layout object a closure holds and a call compares (EQ) is a symbol
  named after it (`key-layout-object`), so that a call read from a fasl
  finds it too, and `compile-file` lays out the calls of a function defined
  earlier in the file (`*compiled-key-layouts*`): such calls in a fasl took
  twice as long as in loaded code.
* **CLOS**: generic functions dispatch and combine methods without argument
  lists when they have one to eight *spread arguments* (past four, the
  functions are called with a vector of them, and the dispatch is in the
  function of the calls of more than four arguments; a McCLIM-like call of
  five required arguments and keywords takes 0.5 of its earlier time, about
  1.3 times SBCL): the required ones,
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
  the keyword arguments (with more than four required, optional and
  keyword arguments in all, the keyed main function still takes them in
  its slots and makes that list without checking it: a McCLIM-like
  `(draw-point* medium x y :ink ... :size ...)` 0.82 of its time in loaded
  code and fasls, about SBCL's). `compile-file` knows the layout of the generic
  functions of the file before they are made (`*note-key-layout-hook*`).
  Calls with one or two keywords take 0.2–0.3 of their earlier time in
  loaded code and fasls (`k-key-*` in scratch tests: about SBCL's time).
  With more than four required, optional and keyword arguments in all (and
  at most three required and optional), the keyed effective method is
  *wide*: its fourth argument is a simple vector of the fourth and later
  ones, which a method reads by the slot of its keyword (cl-ppcre's
  `(scan regex string :start s :end e :real-start-pos p)` of each match of
  `do-matches`: no list of keywords, no `%plist-find`; before, past four the
  keyword arguments went to the methods as a list). `%fast-class-of` finds
  the class of a simple string and of a function that is not a generic
  function without `class-of` (also of NIL, characters, simple vectors and
  single floats, bignums and ratios, `%builtin-index`; a bignum or ratio
  went to `class-of`, out of line). A generic function of one required
  argument keeps in its cache (`+gf-cache+`, slot 5) a vector of the
  functions of the built-in classes by that index (`%gf-note-builtin`,
  filled by `%gf-find1`): its dispatch reads it for an argument that is not
  an instance before searching the entries (`p-builtin-dispatch`, eight
  classes at a call site that cannot cache them: 0.69–0.73 of its time in
  the three modes, 2.5 times SBCL instead of 3.6; 0.6 when the call sites
  are not cached at all). An earlier jump table by representation in each
  such generic function (fixnum, cons, symbol, ...) gained in whole
  programs only. With the EQL call sites and the call sites of two
  arguments below, interleaved against the commit before (four rounds,
  best times): Lem's buffer code (`tools/bench-lem.sh`) 0.99 of its time,
  `bench/apps.lisp` 0.97 (yason's parsing 0.90), `bench/holdout.lisp` and
  `bench/programs.lisp` the same but for `p-builtin-dispatch`; `clos.fasl`
  of `bench/clos.lisp` 4% larger.
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
  defines their compiler macro for the rest of the file and writes it to
  the fasl for the files loaded after it; until 2026-11-21 the fasl did not
  have it: `compile-file` evaluates a `defclass`, which defined the compiler
  macro and set the flag of `%gf-site-macro-forms`, before expanding it
  again for the fasl, which then made none, so that in the files loaded
  after a class's own a call of its reader was a call of the generic
  function, 12 ns against 2 for a reader of Lem's `point` in a loop; a
  `defclass` or `define-condition` now always makes the forms; and a loaded file
  defines it again when the form that set its flag was expanded but not
  evaluated, which left the call sites of `clysm script` without caches;
  in the image a `define-compiler-macro` takes effect when its toplevel
  form is processed, not when it is evaluated, so that a macro expanding
  to a `defclass` or `defgeneric` and the code calling it, one toplevel
  `progn` compiled at once, gets them too: `c-megamorphic` in a script
  0.47 of its time, the methods' calls of the reader `size` were not
  cached; an image compiler macro whose expansion fails is not used): the class of an
  instance is compared with four entries and the slot read at the cached
  location, without an epoch read at each call (`%bump-clos-epoch` empties
  the caches of the sites that have been used, `*gf-sites*`). A site
  compares the class with the four entries and then has one access (or
  call) for the entry found; the class of an instance, and at a site of a
  generic function (not of a reader or writer that `defclass` defines) of
  a fixnum, a simple string or a symbol (one or two arguments; until
  2026-11 only a fixnum of two, the symbols of a call such as `(op :add x)`
  then going out of line: 0.45 of the time of such calls) is
  read inline from CLASS-OF's cache (`%gf-site-key-form`); other
  arguments, an unbound slot and a miss go to one function out of line
  (`%gf-site-other1`/`2`); a generic function with EQL specializers is
  cached too, by the classes of the arguments, the value being that of
  the entry of those classes (which tests the EQL specializer objects,
  `%gf-site-value`); a site that is not cached calls the function
  as before (with fixnums out of line too, `h-fib-generic`, a generic
  `fib` of an `integer` method, took 441 ms instead of 320). A site is about 55 Wasm instructions instead of 140: a
  module of two functions with six reader and writer calls 12.6 KB instead
  of 20.2, Lem's fasls 17.2 MB instead of 19.5 (its buffer code 2.17
  instead of 2.68 MB), all the fasls of the libraries the comparators load
  53.1 instead of 57.3 MB; the time is the same (`tools/bench-lem.sh`,
  `bench/apps.lisp` and `bench/clos.lisp` in the three modes within the
  noise). Comparing only the first entry inline (the rest out of line)
  made the sites smaller still (10.6 KB) but polymorphic sites slower
  (`c-accessors` 2.3 times its time, `bench/clos.lisp` 1.2), and Lem's
  sites of `point` readers saw a `cursor` (a subclass) first; readers and
  writers at about SBCL's time (`r-*` in scratch tests, fasl 0.7 of the
  earlier time); `slot-value` with a constant name and `make-instance` with a
  constant class use caches at the call site (with up to eight initargs
  passed as arguments, not a list). A constructor copies a
  template of the instance's slots holding the values of constant
  initforms, then stores the arguments and the other initforms; the
  effective methods of `initialize-instance` and `shared-initialize` are
  called with the instance and the list of initargs. The `append`, `nconc`,
  `max` and `min` method combinations of two or three methods make no list
  of the values. A primary method of required parameters whose body is a
  literal (`(defmethod kind ((x integer)) :integer)`) is noted
  (`*method-constants*`); when it alone makes the effective method (no
  `:before`, `:after` or `:around` method applies), that is a function
  named `%constant-emf` holding the value, which the dispatch inlined in
  the function of the generic function returns without calling it
  (`%call-entry-form`, as SBCL's constant-value dispatch functions):
  `c-builtin-eql` 127 → 110-115 ms (3.5 → 2.95 times SBCL), in a script
  144 → 117; the call sites still call it (the test there made
  `h-fib-generic` 10% slower).
  Not kept, measured (2026-11-21, interleaved A/B on one slot): an
  open-addressing table of all the entries probed inline in the function
  of the generic function (with a multiplicative hash; the hash of the
  table, `%mix-hash` of a class and the 0 of an unspecialized parameter,
  put consecutive classes in a few buckets) past 4, 8 or 32 entries was
  slower than the linear search of up to 32 entries (5 entries 1.25-1.34
  times the time, 10 entries 1.11-1.14, break-even about 20) and than the
  vectors of the classes past 32 (48 classes 1.2-1.5 times); keys of
  EQL specializer objects found by the inline dispatch itself (their
  markers searched in its list, the entries of their classes no longer
  testing them) instead of the guarded entries of their classes, 1.23
  times the time of `c-builtin-eql` and of calls of `op` through
  `funcall`; a call site of one argument calling the generic function at
  once when it is not cached: `p-builtin-dispatch` 0.85 of its time, but
  `c-megamorphic` 1.09-1.12 (the four classes of instances it still had
  are then dispatched by the generic function). A 20-class megamorphic
  call (`c-megamorphic`) is about 14 ns, an indirect call of 20 closures
  in turn about 5 ns in V8 (SBCL 3): the rest is the dependent loads from
  the closure to the cache and to the method's code.
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
* **Printing and reading numbers**: the shortest digits of a double or
  single are Schubfach's (Giulietti; Java's `Double.toString`): one Wasm
  helper (`%float-shortest`) multiplies the significand, shifted, by a
  126-bit power of ten of a table (63-bit halves in an immutable global,
  64x64->128 products in 32-bit halves) and takes the shortest decimal of
  the interval that reads back as the float. It prints as SBCL does where
  SBCL is not Schubfach: of two shortest decimals as near, the larger, and
  a subnormal as if it had all the bits of a normal float
  (`4.9406564584124654d-324`, SBCL's `flonum-to-digits` on the shifted
  significand; the table goes down to 10^-345 for them). Checked against
  SBCL on 1.8 million random floats in a model of the helper and 950000
  printed by both. An integer within 64 bits is printed by a helper
  dividing an i64 by 10 into the scratch buffer, a bignum by 10^9 passes
  into it (`%integer-decimal-into`), a ratio's parts without strings.
  Reading: a decimal integer of more than 8 digits is made by a helper
  (`%decimal-integer`: 18 digits in an i64, more in limbs multiplied by
  10^9 a group of nine at a time), from the reader (string streams read
  integers, ratios and floats of any length without a token buffer) and
  `parse-integer`; a float by `%decimal-float`: up to 2^53 and 10^22
  (2^24 and 10^10 for a single) one exact product (Clinger), else the
  19 significant digits times the same table's power of ten give 128 bits
  within 5 units of the exact product, which round correctly unless the
  bits after the significand are within 8 of a half (Eisel and Lemire);
  more digits, a result outside the normal floats, or that case: exact
  rational arithmetic, as before. Checked against exact rounding on 400000
  random decimals and halfway cases (singles are rounded once, not through
  a double; SBCL's own reader is off by one unit in some singles and
  subnormals). A float beyond the largest is a reader error, as in SBCL.
  `parse-integer` of a simple string is one Wasm helper call
  (`%parse-integer-fast`: bounds, whitespace, sign, digits in an i64 in
  any radix, both values); it gives the Lisp function the errors to signal,
  other arguments and digits beyond 63 bits outside base 10 (`"42"` 16 →
  8 ns, `"123456789"` 37 → 11, SBCL 6 and 10; `i-parse-ints` 2.1 → 1.1
  times SBCL). Output to a character file (its `fast-out` slot is
  `:file`): `write-char` at the call site and `terpri` call the octet
  store at once, a decimal fixnum (`princ`, `prin1`, `~D`) is written as
  octets straight into the file's buffer (`%fs-out-decimal`), and the
  scratch buffer of other numbers goes to the fast string writer
  (`write-char` 10 → 5 ns, `princ` of a fixnum 72 → 43, `~D~%` 66 → 26;
  SBCL 3.7, 43, 33). `i-file-roundtrip`'s time is mostly the host's write
  of the file when it is closed, which varies from 1 to 60 ms an iteration
  on this machine for SBCL as for Clysm (the writes and reads themselves
  take 0.1-0.4 ms on both).
* **Library**: sequence functions with one implementation per family and
  direct loops over lists, simple vectors and strings; `member`, `assoc`
  and `rassoc` with EQL (the default, or `:test #'eql`) of an item that EQ
  compares (fixnums, characters, symbols, conses, instances: not boxed
  numbers) are an EQ loop in a Wasm helper, the Lisp function only for
  other items and improper lists; `gethash` and `(setf gethash)` of a
  fixnum, character or symbol key read or store the value at the call
  site; hash tables for
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
  `(setf gethash)` of an existing entry are one call each of a Wasm helper
  (`ht-get`, `ht-put-old`) that tests that the object is a hash table, finds
  the entry of a fixnum, a character or a symbol (`ht-fast-index`, which
  casts the table's arrays once: a cast at each access cost about a third of
  the time) and reads or stores the value; it returns null for anything
  else, and the call site then calls the function. A key of another type
  (tested at the call site: an `i31` or a symbol) calls the function at
  once: a string key that went through the helper first made
  `p-word-count` 1.25 times slower. The call site is a call and a branch: a
  character key in an EQL table is looked up as fast as in SBCL. The whole lookup written out at each call site (the hash, the chain's
  first entry compared there) was slower (a loop of two lookups of
  characters 28 → 46 ms, Lem's `string-width` 113 → 140): V8 no longer
  inlined the larger functions that hold the lookups (`control-char` into
  `char-width`), while it does inline the helper. A new key of a fixnum,
  character or symbol is added without looking for it again, and a table
  grows by copying its arrays (`array.copy`) and chaining the entries
  again; what remains of `(setf gethash)` of new keys (2 times SBCL's
  time in a loop that fills tables of 20000 fixnums) is allocating the
  larger arrays (V8's large objects). Measured against the build without
  these changes (2026-11-21, interleaved runs on one slot): Lem's
  `l-string-width` 128 → 121 ms (`tools/bench-lem.sh` on the same file),
  `string-width` of Lem's own code in a loop 0.90 of its time, a reader's
  buffer with a fill pointer looked up in an `equal` table 0.55, new fixnum
  keys 0.72, `bench/library.lisp` 0.97 (`v-hash-churn` 0.78). String comparisons with literal keywords take no
  keyword list; the digits of a float the host makes (a double of 16-17
  digits) cross in groups of nine (`float_part`). Hash tables are
  `(setf gethash)` of a fixnum, a character or a symbol find the entry with
  one Wasm helper (`ht-fast-index`), which casts the table's arrays once
  (a cast at each access cost about a third of the time); `gethash` reads
  the value at the call site: a character key in an EQL table is looked
  up as fast as in SBCL. String comparisons with literal keywords take no
  keyword list. Hash tables are
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
  simple strings in a loop of its own. A vector with a fill pointer or adjustable of a
  constant element type (a reader's buffer) is made without parsing
  keywords, with its properties made once where the call is written;
  `vector-push-extend` of a character to a string buffer with room is a
  store at the call site; `member` of a list written as a constant is a
  chain of `eq` tests and with only `:test` a loop at the call site;
  `elt` of a simple array reads it at the call site. A string with a fill
  pointer is printed by `princ` without looking at the pretty printer's
  dispatch, and `class-of` tests an array header once, before the other
  representations. Such a string is hashed (`equal` tables, `sxhash`) by the
  Wasm loop of simple strings over its storage, compared by `equal` with
  another string over the storages (`string-region=`), and looked up in an
  `equal` table by a helper that compares the stored hashes and then the
  characters of the keys that are simple strings or strings with a fill
  pointer (yason keeps its reader's buffers as the keys of the tables it
  makes), without `equal`; `unread-char` of the
  character just read from a string input stream moves its index back at
  once; the printer looks at `*print-pprint-dispatch*` only once some
  table has been given an entry by `set-pprint-dispatch`. `read-char` and
  `peek-char` of string input streams were already inline in libraries'
  fasls: yason's string reader runs at 0.7 of SBCL's time. `gethash` of
  a string key in an `equal` table hashes it by that loop and, when
  simple, looks it up by the helper that compares simple strings, without
  the dispatch on the key's type (done in `%ht-put` too, it made puts of
  other keys 6% slower: `v-hash-churn`); `make-hash-table` with a test written as a constant and no
  option but `:size` makes the table without parsing keywords;
  `concatenate` of simple strings into a string (`apply` of a list of
  parts) copies them at once.
* **Tests**: a `let` without special variables used as a test (a chained
  comparison `(<= a x b)` binds its arguments) is compiled as the test of
  its body, and `not`/`null` of a test as the test negated, so no `T`/`NIL`
  is made in between (Lem's `eastasian-code-p`, a tree of `(<= lo code hi)`
  over a table of ranges, made and compared `T`/`NIL` at each node, and
  `icon-code-p`'s `(not (null (gethash ...)))` twice); `equal` with a
  character, a symbol or a fixnum written as a constant is `eq` (yason's
  `(equal c #\")` for each string it reads).
* **JSON and dates** (yason, cl-json, local-time; probes of each library
  function against SBCL). A generic function compiled in the image or a
  fasl found the class of an argument that is not an instance, fixnum or
  symbol with a call of `class-of`, which is in the runtime's module and
  not inlined by the engine (6 of the 16 ns of yason's `encode` of a
  string): `%fast-class-of`, copied into every dispatch, now gives an
  index in `class-of`'s cache for simple strings, conses, doubles and
  strings with a fill pointer over a simple string too (one read of the
  cache, one call of `class-of` for the rest). Strings with a fill pointer
  (what parsers make) are written by `write-string`/`princ` from their
  simple storage at once, copied by `princ-to-string`, read by
  `make-string-input-stream`/`read-from-string` from their storage (an
  integer 417 → 133 ns, a double 473 → 160; SBCL 80 and 163), and
  `stringp` of one reads the header's data without a call. A string
  output stream past 16384 characters keeps its full buffers in a list
  and starts a new one (64 KB, below the engine's large objects, which are
  collected with the old generation) instead of copying into a buffer
  twice the size; `get-output-stream-string` copies the chunks once (700000
  characters written one by one: 5.0 → 3.2 ms, SBCL 2.3).
  `with-output-to-string` (of the default element type) uses the scratch
  string stream of `format nil` and `princ-to-string`, whose buffer is
  kept (the stream has dynamic extent; a body left by a non-local exit
  does not give it back; a closed one is not reused): `(with-output-to-string
  (o) (write-string "abc" o))` 49 → 27 ns, SBCL 39;
  `princ` and `~D` of a fixnum into a string output stream put the digits
  into its buffer, `~mincol,padcharD` calls a function of four arguments
  (not `%fmt-integer`'s nine, an argument vector each); `peek-char t` of a
  string input stream skips whitespace in one loop over the readtable's
  ASCII table; `char-equal` of two ASCII characters is compared inline;
  `find`/`position` with only `:test` call a function of four arguments,
  and their scan of specialized vectors and tests with `<`, `<=`, `>`,
  `>=`, `=` are loops of their own (local-time's month table);
  `hash-table-p` is copied into image code (it used `load-time-value`);
  `(setf gethash)` of a string key in an `equal` table hashes it as
  `gethash` does. A `funcall` of a function held in a special variable
  (cl-json's handlers) is already a test for a closure and a `call_ref`
  (4 ns, SBCL 2.3); what remains of cl-json's time is mostly the
  collection of the data it builds, which survives (see Allocation).
  `tools/bench-libs.sh bench/apps.lisp`, the build before and after
  alternately on one slot, twice: yason's encoding 1.47–1.55 → 1.06–1.11
  times SBCL (107 → 71 ms), its parsing 1.66–1.73 → 1.35–1.44, cl-json's
  decoding 1.86–1.88 → 1.56–1.61, local-time 1.77–1.85 → 1.49–1.55; the
  file 1.78–1.80 → 1.56–1.59. `bench/io.lisp` 0.95 of its time compiled
  and 0.92 in a script, `bench/library.lisp` 0.99 and 0.98. The dispatch
  of a generic function grows by one index computation (the image by
  0.3%).
* **Real libraries, `bench/apps.lisp` from a profile** (2026-11-21; Clysm
  and SBCL's `sb-sprof` on each item, the excess ranked by function and the
  Wasm read): esrap's time is spread over its functions at 2-3 times
  SBCL's each, but some of it was things the compiler did not do. An
  integer type with an exclusive bound, `(integer 0 (n))` (Alexandria's
  `array-index`, esrap's `input-position` in its FTYPEs), was checked as
  `integerp` and a generic comparison and taken apart by a call
  (`integer-decl-test` now takes `(n)` as `n-1`, a fixnum test); a local
  `(declare (inline f))` of a function proclaimed `notinline` now inlines
  it, and SETF functions declaimed `inline` are (see "Compiling inside the
  running image"); `ldb` of a byte whose size or position is a constant
  variable (`(byte +chunk-divisor+ 0)`) is a mask (it was `ash` and
  `logand` by calls); calls of 5 to 12 arguments of unknown functions of
  required parameters take no argument vector (see Calling convention);
  `elt` of a declared specialized vector reads it inline (`%simple-array-p`
  is a primitive whose answer the type gives: local-time's
  `transition-position` called `storage-kind` for each probe);
  `find`/`position` over vectors of fixnums or octets read the elements
  inline (local-time's month table: through the `typed-ref` helper).
  `tools/bench-libs.sh bench/apps.lisp`, the build before and after
  interleaved on one slot, two rounds each: `a-esrap` 2.21–2.23 → 1.90–1.98
  times SBCL (117–129 → 102–115 ms), `a-cl-json-decode` 1.57–1.81 →
  1.40–1.46, `a-local-time` 1.38–1.42 → 1.32–1.38; the file 1.36–1.40 →
  1.30–1.34. Lem's comparator 1.48–1.50 → 1.44–1.46; `bench/suite.lisp`,
  `holdout.lisp`, `library.lisp` unchanged (compiled and in a script).
  What remains: local-time computes with Unix times, past the 30-bit
  fixnums, in bignums; esrap's cost is in calls between its files (a
  cross-file call of three arguments 5.3 ns, SBCL 3.4) and the class tests
  of its included structures; cl-json's decoding in the collection of the
  data it keeps.
* **local-time and esrap, second round** (2026-11-21, `sb-sprof` against
  the V8 profile, the Wasm read): local-time's time-zone lookup
  (`%subzone-as-of`, `transition-position`) read its tables through
  `storage-kind` and `typed-ref` and allocated a bignum per probe of the
  binary search. Now a slot declared a simple array of one storage kind
  (the zone's `(simple-array (signed-byte 32) (*))` and `simple-vector`)
  gives its reads that type (`struct-slot-node-ty`); an array element type
  with objects that are no numbers or characters (`(or (eql 0) chunk)`,
  esrap's chunk arrays) upgrades to T at compile time as at run time
  (`element-type-needs-t-p`); a test the argument types decide leaves the
  other branch out of the type of the `if` (`ti-cond`: `elt`'s
  `%simple-array-p` of a declared vector, so the element read is of the
  element type); a comparison of an integer known within 64 bits but not a
  fixnum (an `(signed-byte 32)` element) with an integer of unknown type is
  in i64, the other side decoded inline or by one call (`wide-cmp-code`),
  with no bignum made; `find`/`position` with `:start`/`:end` (and a
  `:test`/`:test-not`, constant `:from-end`) scan a simple string or simple
  vector at the call site, a known test called inline
  (`%bounded-find-expansion`: a probe of two bounded searches of a string
  205 → 148 ms, SBCL 189). `tools/bench-libs.sh bench/apps.lisp`, base and
  new alternately on one slot, four rounds: `a-local-time` 43–45 → 38–39 ms
  (1.29–1.33 → 1.12–1.15 times SBCL), the file 1.31–1.33 → 1.27–1.29;
  `a-esrap` unchanged (1.91–2.00). `bench/suite`, `holdout`, `library`,
  `programs` (compiled and in a script) and Lem's comparator unchanged.
  With the checks of those slots (on write, and the storage-kind test of
  each compiled read) `a-local-time` is 37–38 ms against the base's 44–45,
  three rounds. esrap's remaining factor is its calls: probes of one call of a
  notinline function, of a function given as argument and funcalled, and
  of the dynamic-extent closure of its `with-cached-result` take 2–3 times
  SBCL's time, and the closure's allocation is not the cost (the same
  probe with a global function instead is as slow).
* **Characters of strings kept as codes** (2026-11-21, Babel's encoders
  and decoders read with `*live-dump*`): V8 does not fold a character
  made and taken apart again (`+tag`, `ref.i31`, the cast, `i31.get_s`,
  `-tag`), so each of these cost per character. `(char-code (schar s
  i))` (and `char`) of a string read inline now leaves the storage's
  element as the i32 code (`char-code-elt-code`); `code-char` of a fixnum
  makes the character from its i32, and `(setf (schar s i) (code-char
  n))` stores the code as it is, tested against `char-code-limit` unless
  its type says so (`code-char-fixnum-arg`). For the last one the short
  form of `defsetf` no longer binds temporaries for the place's subforms
  and the value (the update function is called on them, which evaluates
  them in the same order; not when it is a macro), so the store sees the
  value's form; every `(setf (aref ...))`, `(setf (car ...))` etc. now
  compiles without the copies. A `block` returned from in its function
  whose type is a fixnum (or an i64 or double) is compiled in that
  representation when its value is wanted raw, its `return-from`s passing
  raw values (`cg-block`'s REP; through an `unwind-protect` the value is
  boxed): Babel's decoders' `setter-block`. LOOP gives a variable
  declared with a type defined by `deftype` (`for code of-type code-point
  = ...`, `(mod #x110000)`) the default of its expansion, so that it is
  declared and held raw instead of starting as NIL
  (`%loop-deftype-expansion`). `tools/bench-libs.sh`, base and new
  alternately on one slot, twice: `l-babel` 141–144 → 110–113 ms (2.07–2.09
  → 1.62–1.64 times SBCL), `l-split-sequence` 1.70 → 1.56–1.59, the file
  1.17 → 1.12–1.15; in `bench/apps.lisp` `a-ppcre-tokens` 45 → 43 ms, the
  rest unchanged (the file 1.30–1.31 → 1.25–1.29). Of Babel's three
  operations, UTF-8 encoding is now SBCL's time (24 ms for 50, SBCL 24),
  UTF-16 encoding 44 → 31 (SBCL 21), UTF-8 decoding about unchanged (48,
  SBCL 24: the decoder, the code-point counter and `make-string` of the
  result each take a third). `bench/library.lisp` and `io.lisp` (compiled
  and in a script) and Lem's comparator (1.49–1.53) unchanged.
* **A call of ERROR is not returned from** (2026-11-21, Babel's UTF-8
  decoder against probes of its shape): a loop whose error branches call
  `error` ran 2.5 times as long as the same loop without them (24 ms
  against 9-11, SBCL 9), though the branches are never taken. A call of a
  function through its symbol was compiled as one that may return, so
  everything the loop keeps (the indices, the arrays) was live across it
  and V8 spilled it on every iteration; the probe with `(error "bad")` (no
  arguments) is 16 ms, with a pure computation of the same size in the
  branch 13. The calls of a function in `*ti-noreturn*` (`error`) through
  its symbol now end as the direct calls of such functions did: a branch
  to the exit handler of the context after the call (`*call-noreturn*`,
  `checked-call`'s NORETURN), so that nothing is live after them. Babel's
  decoder 18 → 10 ms for 50 decodings of 330 KB (SBCL 9), `octets-to-string`
  44–47 → 35–40 (SBCL 23: what remains is `make-string` of the result, 11
  ms against SBCL's 3, a large object for V8 (a string of 4-byte
  characters over 128 KB), and the code-point counter, at SBCL's time).
  `tools/bench-libs.sh` base and new alternately, two rounds: `l-babel`
  1.55–1.64 → 1.46–1.55 times SBCL, the file 1.13–1.14 → 1.09–1.11;
  `bench/apps.lisp` `a-ppcre-tokens` 1.38–1.39 → 1.26–1.27, the rest within
  noise (the file 1.31–1.34 → 1.26–1.31); `bench/suite`, `library`, `io`,
  `programs` (compiled; `library` and `io` in a script too) unchanged,
  Lem's comparator 1.48–1.55 → 1.50–1.56 over four rounds (within its
  noise).
  Two things tried here that did not pay off: not saving the binding-stack
  pointer for a block or tagbody that never restores it (V8 drops the
  dead load itself: no change), and a peephole pass over the Wasm taking
  out box/unbox pairs (`ref.i31` then the cast and `i31.get_s`,
  `struct.new $double` then its read, `box-i32` then `int-i64`, and blocks
  and ifs boxing at every exit and unboxed after them). It rewrote 3363
  pairs and 2953 constants in `bench/suite.lisp`'s module (0.5% smaller),
  but times were unchanged: `bench/suite` 0.83 → 0.83, `library` 0.91–0.92 →
  0.90, `gabriel` 0.92–0.93 → 0.92, `arith` 0.56 → 0.56, apps, libs and Lem
  within noise. V8 folds such pairs when they are next to each other, and
  most unboxings (13000 of 21000) are of variables held boxed, which the
  types would have to change, not a peephole.

### Declared code, and SBCL at its best policy

The files above are undeclared code at SBCL's default policy, where SBCL
uses generic arithmetic and boxed doubles. `bench/declared.lisp` is the
other side: 19 programs written as one writes Lisp for speed in SBCL,
starting with `(declaim (optimize (speed 3) (safety 0) (debug 0)))`. They
have full type declarations (`fixnum`, `(unsigned-byte 32)`, `double-float`,
`(simple-array double-float (*))` and `(* *)`, `simple-string`,
`simple-vector`, `simple-bit-vector`), typed structure slots, `ftype`
proclamations, `inline` helpers and `dynamic-extent`, and nothing that
belongs to SBCL. They are the Benchmarks Game programs (n-body,
spectral-norm, mandelbrot, fannkuch-redux, binary-trees, fasta,
k-nucleotide, reverse-complement, pidigits), tak, an FFT and Gabriel's
triangle with declarations, and matrix multiplication, a bit-vector sieve,
a particle simulation over structures of doubles, a tokenizer over a simple
string, an `eql` table of fixnums, a quicksort over `(simple-array fixnum
(*))` and CRC-32. A `fixnum` declaration is only made where the value fits
in 30 bits (Clysm's fixnums), as portable code must.

`tools/bench.sh bench/declared.lisp` (2026-11-21, best of three, one slot):

| Program | SBCL ms | Clysm ms | Ratio |
|---|---|---|---|
| `d-nbody` | 208 | 732 | 3.52 |
| `d-particles` | 136 | 480 | 3.53 |
| `d-spectral-norm` | 137 | 393 | 2.87 |
| `d-matmul` | 108 | 297 | 2.75 |
| `d-triangle` | 140 | 351 | 2.51 |
| `d-fannkuch` | 142 | 334 | 2.35 |
| `d-tak` | 195 | 422 | 2.16 |
| `d-crc32` | 145 | 254 | 1.75 |
| `d-mandelbrot` | 165 | 221 | 1.34 |
| `d-fixnum-hash` | 175 | 223 | 1.27 |
| `d-pidigits` | 190 | 229 | 1.21 |
| `d-quicksort` | 165 | 193 | 1.17 |
| `d-fasta` | 123 | 137 | 1.11 |
| `d-tokenize` | 160 | 176 | 1.10 |
| `d-binary-trees` | 184 | 197 | 1.07 |
| `d-revcomp` | 115 | 117 | 1.02 |
| `d-sieve` | 124 | 110 | 0.89 |
| `d-k-nucleotide` | 110 | 93 | 0.85 |
| `d-fft` | 103 | 86 | 0.83 |
| geometric mean | | | 1.56 |

After items 1, 3 and 5 below (structure accessors, integer checks, array
headers): 1.48 against 1.57 for the code before, run by run on one slot;
on top of the `(safety 0)` work (da5b950), 1.11 against 1.27 (`d-nbody`
489 → 232 ms, `d-particles` 422 → 245, `d-matmul` 253 → 77; the same in
`clysm script`: 512 → 232, 430 → 242, 261 → 77).

In the image (`MODE=script`): 1.03 of the whole program; `d-tokenize` 1.67,
`d-tak` 1.12, `d-k-nucleotide` 1.10, the rest within 0.8–1.07. The same file
with `(safety 1)` in place of `(safety 0)`: 1.52 (SBCL's checks cost it
little: its times move within the noise, `d-matmul` 108 → 176 ms, the
bounds checks of a two-dimensional array).

`SBCL_OPT="(speed 3) (safety 0)" tools/bench.sh FILE` has SBCL proclaim that
policy before it compiles FILE (undeclared code at SBCL's best policy; the
Clysm side is the same). Default policy → `(speed 3) (safety 0)`, geometric
mean (slowest):

| File | Default | `(speed 3) (safety 0)` |
|---|---|---|
| `bench/suite.lisp` | 0.91 (`b-hash` 2.14) | 0.92 (`b-matmul` 2.23) |
| `bench/gabriel.lisp` | 0.94 (`g-traverse` 1.38) | 1.08 (`g-takl` 1.92) |
| `bench/holdout.lisp` | 0.91 (`h-fib-generic` 1.68) | 0.96 (`h-matmul-double` 2.22) |
| `bench/programs.lisp` | 1.01 (`p-builtin-dispatch` 2.86, was 3.62) | 1.08 (`p-builtin-dispatch` 4.13) |
| `bench/library.lisp` | 0.96 (`v-hash-churn` 2.40) | 1.03 (`v-map-every` 2.38) |
| `bench/vectors.lisp` | 0.46 (`x-fannkuch` 1.36) | 0.46 (`x-sort-key` 1.48) |
| `bench/arith.lisp` | 0.66 (`a-sum-overflow` 2.35) | 0.69 (`a-sum-overflow` 2.39) |
| `bench/arrays.lisp` | 0.76 (`a-struct-int` 1.69) | 0.77 (`a-struct-int` 1.54) |
| `bench/closures.lisp` | 1.09 (`k-memoize` 2.67) | 1.21 (`k-memoize` 2.77) |

Without declarations the policy gains SBCL little (2–11% in geometric
mean): it removes type and bounds checks but cannot remove generic
arithmetic. The items that gain most: `g-takl` (SBCL 51 → 26 ms),
`v-search-mismatch` (48 → 28), `g-browse` (139 → 97), `h-matmul-double`
(90 → 63), `p-coerce` (47 → 33), `p-double-2d` (36 → 26). Declared code is
where Clysm is behind: 1.56 against about 0.9–1.1 on undeclared code.
Until 2026-11-21 Clysm read no `optimize` quality but `debug` and `speed`
(for the frame records of the debugger), and `safety 0` changed nothing in
its code; see "What `(safety 0)` does" below for what it does now.

What Clysm generates for the slowest items (`tools/dump-func.lisp`),
ranked by the gain expected:

1. **Accessors of a variable declared as a structure** (`d-nbody`,
   `d-particles`, 3.5; also `arrays`' `a-struct-*`). `(declare (body a))`
   does not make `(body-x a)` a field read: every accessor loads the class
   from a `load-time-value` cell (with its null test), `ref.test`s
   `$ts-ddddddd`, compares the layout, and on a mismatch walks the
   inheritance vector, before the `struct.get` of the f64. `advance` does
   that 30 times per pair of bodies. SBCL at safety 0 reads the slot at a
   fixed offset. A declared variable of a structure type (checked once
   where it is bound, or trusted at safety 0) should keep a typed
   reference whose accessors are a bare `struct.get`. Expected: n-body and
   particles 3.5 → about 1.3.
   *Done (2026-11-21):* a structure class is a declared type (also as a
   declaration identifier, and as a slot type), and an accessor of an
   object known to be of the class is a `ref.test` of its Wasm type and
   the field access, the class loaded only on the slow path (see "Type
   inference" above). Before the `(safety 0)` work: `d-nbody` 785 → 683
   ms, `d-particles` 507 → 439 (the rest was item 2, whose float tests were
   around every one of those reads). The type of the object is tested once where a variable is
   bound (its shadow local, see "Type inference"): a test and a cast at
   every access took n-body 1.35 times as long as a bare cast, a
   `br_on_cast` 1.9 times, the shadow 1.1 times (V8 shares none of the
   tests between accesses); comparing the object with the one an access
   before found (`ref.eq`), for undeclared code, was slower than the
   test. With the `(safety 0)` work (da5b950): `d-nbody` 489 → 232 ms
   (2.3 → 1.08 SBCL), `d-particles` 422 → 245 (2.9 → 1.7).
2. **Floating-point trap emulation on every f64 operation** (`d-matmul`,
   `d-spectral-norm`, `d-nbody`, `d-particles`, `d-mandelbrot`). Each
   `f64.add`/`f64.sub` is followed by a NaN/infinity test (`x - x != 0`)
   and each `f64.mul`/`f64.div` by an exponent extraction and up to five
   comparisons, with a branch to the generic function (to signal
   `floating-point-overflow` etc. as SBCL does with hardware traps, at no
   cost there). In `matmul`'s inner loop that is about 15 instructions
   around one multiply-add. Dropping the tests under `(safety 0)` (or
   testing once where a value leaves the f64 world: a store, a return)
   is worth an estimated 1.3–1.6 on these programs.
3. **Integers declared `(unsigned-byte 32)` assigned in a loop** (`d-crc32`
   1.75). The value is computed in an i64, but the `setq` of the declared
   variable tests its type as for an object: when it is above the fixnum
   range it calls `$i64-int` (allocating an integer) only to `ref.test`
   it for a bignum, then compares the range. The same function without
   the declaration of `crc` runs in 77 ms against 136 (SBCL 76). An i64
   is an integer: the test is two i64 comparisons (none at safety 0).
   *Done:* the check uses `sys::%integerp` and comparisons that the
   types of the value decide (`known-test-result`), so it is folded away
   when the value is known to be in range, and `%bignump` of a value in
   an i64 is a comparison: `d-crc32` 282 → 157 ms (1.82 → 1.02 SBCL)
   before the `(safety 0)` work, which leaves the check out at safety 0;
   this is what makes it free at the default safety.
4. **Arithmetic of declared fixnums whose result is not proved a fixnum,
   and calls** (`d-tak` 2.2, `d-triangle` 2.5, `d-fannkuch` 2.35). `(1- x)`
   of a fixnum is boxed by a call of `$box-i32` and passed as an `eqref`;
   so the recursive calls of `tak` go to the version for `(t fixnum
   fixnum)` and the tail call to the main entry, and results come back
   boxed with `mv-count` set. `(the fixnum (1- x))` changes nothing. `(incf
   count ...)` of a `fixnum` variable keeps its overflow test into
   `%type-error`. `triangle-count` calls itself through the main entry and
   unboxes the result with `$int-i64`; `(decf (aref count r))` of a fixnum
   vector goes through the generic `aset1` with an `$i64-int`; `replace`
   of two fixnum vectors is a call of the general function. Under
   `(safety 0)`, or with an `ftype` that says the arguments are fixnums,
   the arguments and result of such arithmetic can be wrapped to 30 bits
   and calls made to the i32 version; SBCL passes untagged words. Expected
   about 2.2 → 1.2 on these three.
5. **Two-dimensional arrays** (`d-matmul`, `h-matmul-double`,
   `p-double-2d`). `(incf (aref c i j))` reloads the header of `c` (its
   data vector, with a cast, and both dimensions) on every iteration, and
   each subscript has its own bounds test besides the Wasm array's own.
   Hoisting the header of an array variable out of the loop and one test
   of the row-major index are the gain.
   *Done in part:* a variable of the representation `(:md2 kind)` bound
   to another one (SETF's, an inline function's parameter) in an
   innermost loop uses that one's locals (or copies them) instead of
   loading the header again. `d-matmul` 327 → 200 ms (2.8 → 1.75) before
   the `(safety 0)` work, 253 → 77 (2.2 → 0.66) with it. Outside an innermost
   loop it loads the header as before: keeping the locals live across the
   inner loop made `p-double-2d` 10% slower (register pressure in V8).
   Testing both subscripts with one branch (`and` of the two
   comparisons) measured no different and was left out. Left: the row
   offset `i * dim1` is computed at every access. Undeclared arrays: see
   "Two-dimensional arrays of unknown type" above.
6. **An inline function's double result is boxed** (`d-spectral-norm`).
   `eval-a` (`declaim inline`) returns through a block of type `eqref`: its
   f64 is put in a `struct.new $double` and unboxed by the caller at once,
   an allocation per inner iteration unless the engine removes it; the
   `(the fixnum (* ...))` of its denominator goes through `$i64-int` and
   `$int-i64` as well.

Smaller: every `block` and `let` saves the dynamic state
(`global.get $dyn`, `struct.get`) into a local; `dynamic-extent` is
ignored (the vectors are heap-allocated, which costs little here).
Where Clysm is ahead (FFT over `(simple-array double-float (*))`,
k-nucleotide's `equal` table of strings, the bit sieve) the inner loops are
typed array accesses with no float operations whose result is tested, or
library code.

#### What `(safety 0)` does

Clysm reads `safety` as SBCL does: `(declaim (optimize (safety N)))`
(`compile-time-proclaim` in `src/driver.lisp`, and `proclaim` in the image,
`sys::*proclaimed-safety*`, bound around a file by the whole-program
compiler and by `compile-file`, as SBCL's policy), and `(declare (optimize
(safety N)))` in any body with declarations (an entry `(:safety N)` of the
compiler's environment, `policy-safety`; `lam-safety` for the code
generator). At safety 1 to 3 declarations are checked (see "Type inference"
above). At safety 0 declarations are
trusted (`trust-types-p`):

* the value bound or assigned to a variable declared of an integer type,
  a free declaration's reference, the arguments of a function with an
  `ftype` and its result are not checked: they become `(the type form)`,
  and that `the` is marked `:trust`. A trusted `(the fixnum x)` of integer
  arithmetic within 64 bits is that arithmetic wrapped to 32 bits
  (`trusted-fixnum-code`), of anything else its i31 (`ref.cast` traps
  where the declaration lied): `(1- x)` of a declared fixnum passed to a
  function stays an `i32.sub` and a `ref.i31` (`$box-i32` before), the
  result of a call is a `ref.cast` (`$int-i64` before);
* the arguments of a call of a function of the same compilation whose
  required parameters are declared of integer types (itself, while it is
  converted: `*defun-param-specs*`) are such trusted `the`s too, so
  `tak`'s calls go to its fixnum version, the tail call too;
* `f64`/`f32` operations of declared floats have no trap tests (the IEEE
  result: an infinity, a NaN, a zero where the default signals
  `floating-point-overflow` and the like): `matmul`'s multiply-add is two
  instructions;
* an integer within 64 bits stored into an integer array is trusted to be
  an element (wrapped to the storage), so `(decf (aref count r))` of a
  `(simple-array fixnum (*))` is an inline store;
* a vector's subscript is not compared with its length before an inline
  access: the Wasm array's own test traps.

Wrong declarations give wrong numbers or a Wasm trap, never a corrupted
heap (a Wasm array or cast is always checked). A `fixnum` is Clysm's
31-bit fixnum: portable code declares `fixnum` only for values within 30
bits (`bench/declared.lisp` does; a library that declares hash values
`fixnum` at safety 0 and counts on 62 bits computes others here).

At any safety: a `block` that no `return-from` refers to is not made (its
body is the node; the `defun`'s block of nearly every function, and that of
an inline function's copy, whose double result now stays an f64 instead of
going through an `eqref` block: `eval-a` in `d-spectral-norm`), and
`replace` of two whole simple vectors of the same storage kind is one
`array.copy` (`replace-copy-code`; `fannkuch`'s permutation copy).

`tools/bench.sh bench/declared.lisp` (2026-11-21, one slot, before → after;
the SBCL times of `d-fixnum-hash` vary between 235 and 404 ms):

| Program | Clysm ms | Ratio to SBCL |
|---|---|---|
| `d-spectral-norm` | 418 → 148 | 2.82 → 1.01 |
| `d-fannkuch` | 364 → 139 | 2.43 → 0.89 |
| `d-crc32` | 282 → 152 | 1.81 → 0.97 |
| `d-nbody` | 797 → 520 | 3.64 → 2.32 |
| `d-mandelbrot` | 239 → 181 | 1.41 → 1.04 |
| `d-triangle` | 370 → 303 | 2.53 → 2.05 |
| `d-matmul` | 325 → 276 | 2.80 → 2.30 |
| `d-particles` | 516 → 451 | 3.46 → 2.97 |
| `d-tak` | 457 → 418 | 2.22 → 1.96 |
| `d-k-nucleotide` | 119 → 95 | 0.99 → 0.79 |
| `d-tokenize` | 189 → 180 | 1.10 → 1.01 |
| `d-binary-trees` | 244 → 233 | 1.23 → 1.14 |
| `d-quicksort` | 203 → 195 | 1.14 → 1.06 |
| `d-fasta` | 149 → 146 | 1.16 → 1.06 |
| `d-pidigits` | 239 → 235 | 1.18 → 1.13 |
| `d-revcomp` | 124 → 123 | 1.07 → 0.99 |
| `d-fft` | 95 → 98 | 0.90 → 0.89 |
| `d-sieve` | 116 → 123 | 0.89 → 0.92 |
| `d-fixnum-hash` | 282 → 254 | 0.70 → 1.08 |
| geometric mean | | 1.55 → 1.24 |

Interleaved runs of the two builds on one slot (best of three): 0.81 of
the time before in geometric mean. In the image (`MODE=script`): 1.01 of
the whole program. Undeclared code at the default policy, the two builds
interleaved: `suite` 0.98, `gabriel` 1.02 (`g-div2` 1.12 and `g-fft` 1.06
in one run of five, less code in both), `holdout` 0.99, `arrays` 0.99,
`vectors` 0.98, `programs` 1.00, `arith` 0.88. With `SBCL_OPT="(speed 3)
(safety 0)"`: `suite` 0.92, `gabriel` 1.09, as before. Libraries in the
image (`bench/apps.lisp`, the two builds interleaved, best of five): 1.01;
`tools/bench-libs.sh` 1.15 against SBCL with both builds.

What remains of the list above: the structure accessors (1) and
two-dimensional arrays (5) are separate work. Calls of known functions
with fixnum or double arguments now pass and return them raw ("Raw
clones" under Calling convention), interleaved against the build before
(best of three): `d-tak` 387 → 311 ms, `d-triangle` 276 → 262, `d-nbody`
469 → 430 (`declared.lisp` 0.98 in geometric mean; at `(safety 1)` 0.99,
`d-tak` 0.92); `b-fib` 28 → 21, `b-tak` 0.92 (`suite` 0.98), `closures`
0.97 (`k-church` 0.83), `gabriel` 0.99, `holdout`, `programs` 1.00,
`arith` 1.01.

Testing a float for an overflow only where it leaves an arithmetic tree
(2) was tried and measured (2026-11-21), and is not kept. In a tree of
`+ - * /` an infinity or a NaN goes on to the root (an infinity divisor
gives a zero), so the operations below the root need no test of being
finite, only `*` and `/` one of a zero that may be an underflow, and the
generic operations compute the whole tree again when the root's test fails
(the speculative and fused trees of `src/prims.lisp` already do; the trees
of declared doubles of `f64-binop-code` were given the same, with the
leaves, which must then have no side effects, evaluated first). Interleaved
runs against the build without it: with the zero test cheaper than the
exponent test on inner products, `d-matmul` at `(safety 1)` 0.92 but
`d-mandelbrot` 1.08 (1.51 before the tests had branch hints); with the
exponent test kept on products, `d-mandelbrot` 0.95, `d-matmul` 1.01; for
the undeclared trees alone `arith` 0.99, `suite` 1.01, `holdout` 1.00. The
tests are a few instructions beside a branch the engine predicts; the
values are not where the time goes. `tests/cases/float-traps-trees.lisp`
keeps the overflows of such trees tested.

Raw copies (2026-11-21, "Raw clones" under Calling convention): tail
calls of other raw copies with the same result, no closure parameter for
copies with a raw result, `length` of a declared vector not taken as a
call that may exit. Interleaved against the build before, best of five on
one slot: `bench/declared.lisp` 1.01 in geometric mean (`d-tak` 329 → 314
ms; `d-triangle` gets a raw result but 274 → 282, its time is in its loop,
not its calls), the same file at `(safety 1)` 0.96; best of three:
`suite` 1.01, `gabriel` 0.98, `holdout` 0.99, `arith` 1.02, `arrays` 1.01,
no item beyond the noise of a run.

Testing once per stretch of code without branches was tried and measured
too (2026-11-21), and is not kept either. The tests do cost at `(safety
1)`: without them (an experiment, not CL semantics) `d-nbody` ran 255 ms
against 700, `d-spectral-norm` 220 against 430, `d-particles` 195 against
310 — about one cycle per test, 27 tests in n-body's inner loop. What was
tried, each against the per-operation tests on `d-nbody` at `(safety 1)`,
best of five interleaved: (a) the operations of a stretch of straight-line
code (`let` bindings, `setq`s, a tree) computed into locals of their own
with their operands, each zero product or quotient OR-ed into a flag, the
values that no other operation consumed summed and the sum tested once
before anything that may see them (a call, a store, a conditional), the
operations gone through again by the generic functions when it failed:
817 ms against 679 — the operands kept live to the test are spilled; (b) a
tree compiled twice, a copy without tests and a checked copy run again
from the same leaves when the one test of the tree's value (and its
divisors) or the zero flag fails: 846 against 731, with the flag of a
product as an `i64` test of the shifted bits; 1089 with the flag replaced
by the product of all the products (tested for zero once); (c) the
checked copy replaced by `unreachable` changed nothing, so it is not the
code size. The reason is V8's code for an `f64` comparison whose result is
a value (not a branch): `ucomisd`, a branch on parity, `set`cc and a
`jmp`, as much as the test it replaces; and the stretches between stores
and conditionals are short (n-body stores a slot after every two
operations). Only removing the zero test of products (the underflow
condition, which SBCL does not trap by default) would gain much; that was
then done, with the trap set of SBCL's default (see "Floating-point
conditions" under Status and limitations).

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

Later the same day (2026-11-20 evening), measured on one slot one run after
the other, best of five, milliseconds ("main" is main at 017e37e, "now" this
work on top of it):

| Item | SBCL | Main | Now |
|---|---|---|---|
| `s-start` (`--eval t`) | 4 | 134 | 85 |
| `s-script` | 5 | 201 | 165 |
| `s-require-asdf` | 50 | 266 | 216 |
| `s-quicklisp` | 126 | 467 | 415 |
| `s-ql-alexandria` | 142 | 517 | 462 |
| `s-ql-cl-ppcre` | 171 | 519 | 480 |
| `s-ql-babel` | 213 | 601 | 552 |
| geometric mean / SBCL | | 7.0 | 6.0 |

* **The main thread.** An image started from a heap image runs its
  function as a computation that can wait (`%call-as-main-thread`, JSPI),
  on a stack of the engine's whose size `--wasm-stack-switching-stack-size`
  and `--stack-size` give, in any thread: such an image (the REPL) runs in
  Node.js's main thread (`host/run.mjs`), where starting the worker cost
  20 ms and ending it (joining the thread, freeing its heap) 12 to 17.
  `--stack-size` set at run time leaves the limit of the thread's own stack
  as it was, so the main thread's stays within the system's 8 MB, and a
  deep recursion outside a computation (a callback from JavaScript) still
  ends with a `RangeError`. The sockets and processes that the program calls
  as if they were synchronous (`Atomics.wait`) are served by a worker
  started when one is first needed. A program that does not start from a
  heap image (`clysm run`, `exec`, the tests, `CLYSM_NO_HEAP_IMAGE`) keeps
  the worker, as do `CLYSM_HEAP_MB`, a heap limit under 4 GB, a host without
  JSPI and `CLYSM_WORKER=1`.
* **Globals.** V8 validates the initializer of every global when it
  compiles a module and evaluates it when it instantiates it (about 15 ns
  an instruction and 0.1 µs a global each time). 59000 of the REPL image's
  80000 globals were the conses of constant lists; a constant that only the
  initializer of one other constant refers to is now built in that one's
  (`inline-single-use-constants`, `src/driver.lisp`): 30000 globals, the
  image 8.9 MB instead of 9.5, compiling and instantiating it 49 -> 42 ms,
  making the heap image's objects 15 -> 9.5 ms (`:image-objects`, one
  `global.get` for each object of a global, is a third as long).
* **Overlap.** The image compiles in V8's threads while the host imports
  its modules and reads the heap image.
* **Compiling a form in the image.** The module's octets crossed to the
  host one by one (`put_char_buf`, a string concatenated, then converted);
  they cross at once now (`js:from-octets`). The expander of a local macro
  is interpreted instead of compiled to a module of its own: every `loop`
  expands to a `macrolet` of `loop-finish`, and that module was a fifth of
  the time of compiling small `defun`s with a `loop`. Loading 300 small
  `defun`s from source: 0.90 -> 0.68 s.
* **Fasls.** `load` of a compiled file opened a file stream, which read the
  whole file into Lisp octets (4.5 MB for ASDF: 13 ms of reading and 10 of
  copying when ASDF and Quicklisp load), though the host reads the modules
  and decodes the text itself; `load` asks the host for the head of the
  file and reads a compiled file through it (a stream is opened only when a
  source form must be read).
* **The npm package** ships the heap image (`clysm-native.img`) and starts
  its REPL from it: `clysm eval t` 168 -> 120 ms.

Where the time of `--eval t` goes now (85 ms): Node.js and the host's
modules 19 ms; compiling and instantiating the image 42 ms (compiling, 18
ms in V8's threads, overlaps the rest); making the heap image's objects
9.5 ms (half of it Liftoff compiling `:image-objects` and
`:set-image-mutables`, 140 KB of code); the program 10 ms. The cold
compiler is what `s-script` adds (70 ms for one `defun`): Liftoff compiles
about 30 MB of code a second on one thread, and compiling a first `defun`
runs 780 functions of the compiler, 1.3 MB of code (`encode-module`,
`emit-instr`, `i32-op-code`... 20 to 50 KB each); loading Quicklisp and a
library compiles 2 MB of the image's functions and 0.7 MB of the fasls'.

**The compiler's code, smaller.** What Liftoff compiles is measured with
`node --trace-wasm-compilation-times` (the body size of each function
compiled). A first `defun` in the REPL ran 1.23 MB of the image's code
(150 KB of it what every start runs); now 1.05 MB:

* `emit-byte` was declared inline, and its `vector-push-extend` expands to
  the paths of every kind of vector: a hundred copies in `emit-instr`. In
  Clysm it is a call now (`emit-instr` 36 -> 11 KB, `encode-module` 31 ->
  20 KB).
* Inlining has a budget for the compiler's own functions (`src/`,
  `lib/live-image.lisp`, `runtime/loop.lisp`; set by
  `tools/build-repl.lisp`): a function larger than 400 nodes gets no copies
  of functions not declared INLINE, the others at most 200 nodes of them
  (`*inline-big-caller-nodes*`, `*inline-caller-budget*`). Without any
  inlining a first `defun` would run 0.91 MB, but for all code (the
  runtime's too) the budget made `b-int64` and `b-call` of
  `bench/suite.lisp` slower, so the runtime and programs keep theirs.
* Integer arithmetic on constants is folded when compiling
  (`constant-integer-form`): `ty-int32-p` computed `(- (expt 2 31))` each
  time.

`clysm script` of a `defun` and a call 168 -> 135 ms, `s-quicklisp` 426 ->
397, `s-ql-*` 15 to 20 ms less; compiling small `defun`s warm is no slower.
Tried: leaving out the compiler macros of `define-compiler-macro` in the
compiler's code (`vector-push-extend`'s and others): 3% less code run, no
time measurable; no inlining at all in the compiler's code: 0.98 MB, no
time measurable on a busy machine. What remains is mostly the runtime's
functions (`format`'s directives, sequence functions, `typep`, the
`loop` parser...), whose inlining the benchmarks need, and the check of a
pending exit after each call (12 octets a call).

Tried and not kept: compilation hints (`withCompilationHints`, the
functions that run at start compiled eagerly in background threads) are
not used by Node.js 24's V8 (13.6; `--experimental-wasm-compilation-hints`
changed nothing). V8 has no cache of compiled WebAssembly that Node.js
exposes (`v8.serialize` of a `WebAssembly.Module` gives 2 octets).

Also tried and not kept (2026-11-20 evening): a Node.js startup snapshot
(`node --build-snapshot`) cannot hold WebAssembly (`WebAssembly` is not
defined while the snapshot is built). V8's PGO
(`--experimental-wasm-pgo-to-file` / `-from-file`, a profile per module in
the working directory) compiles the functions that ran eagerly, and those
that tiered up with TurboFan, before the module is ready: `--eval t` 115 ->
270 ms, Quicklisp no faster. The image's objects from a passive element
segment (`array.new_elem`) instead of the 73000 `global.get`s of
`:image-objects`: V8 evaluates the segment when it instantiates the module,
no gain. Compiling a form before the heap image is saved (so that the
compiler's tables made when first used are in it): no gain. Ending the
worker by a message from it before it tears down: the main thread's exit
joins it anyway.

### Allocation, garbage collection and the engine's flags

Objects are V8 heap objects. Node.js is built without pointer compression,
so every Wasm struct has two 8-octet header words (map, hash): a cons takes
32 octets (SBCL: 16; measured), a boxed double 24, an instance about 40 plus its slot
vector (24 + 8 per slot). Nothing in the module can make these smaller.

Measured in 2026-11 (Node.js 24.21, V8 13.6; four CPUs; `bench/alloc.lisp`,
written for it: binary trees, trees of structures, lists copied over and over,
closures, boxed doubles, small vectors, and data that survives: 3 million
pushed conses, a hash table of 500000 lists, 300000 strings in a vector;
since 2026-11-21 also `s-ring`, data of medium lifetime, 6.7 times SBCL):

* **Garbage that dies young is cheap**: binary trees of depth 18 take 1.1–1.4
  times SBCL's time, the others 0.8–2.1 (small vectors 2.1, closures 1.7). Scavenges stay under
  12 ms (2.3 ms on `bench/gabriel.lisp`), full collections under 7 ms.
* **Data that survives is not**: building a list of 3 million conses that
  all stay alive is 4–7 times SBCL's time, almost all of it in scavenges
  that copy each object twice (into the other semi-space, then to the old
  generation), 50–100 ms each once the young generation has grown to its
  64 MB; full collections of a 400 MB heap take about 80 ms. Those are the
  longest pauses an application (an editor) sees.
* **Building lists** (2026-11-21, probes of `loop ... collect`, `push` and
  `nreverse`, `mapcar`, `copy-list`, `append`, `make-list`, `list`):
  in a heap that has grown, a cons costs about 2 ns to make (SBCL about 1;
  `(list i i i i)` 2 times SBCL), and collecting at the tail is as fast as
  SBCL (10000 elements: `loop collect` 0.9, `mapcar` 0.8, `copy-list` 1.1).
  A million conses kept alive are 6 times SBCL the first time a process
  builds them (the young generation grows, and each scavenge copies the
  list made so far: 75–140 ms of collection), as fast as SBCL once it has
  grown; building them with `push` and `nreverse` instead of at the tail
  is no faster then. Neither a typed local for the tail nor setting the cdr
  without the test of a cons changed TurboFan's code (it inlines the
  `:check-cons` helper and drops the test), but code still running in
  Liftoff (the first part of a short program) collects 30% faster with
  the test left out: `%setcdr`, `%setcar`, `rplaca` and `rplacd` of a
  value the inference knows is a cons are a field store (`typed-access-code`),
  and `loop`'s `collect` tests its tail (NIL or a cons) rather than the
  list, which makes the tail a cons where its cdr is set.
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
| `--scavenger-max-new-space-capacity-mb=64` (2026-11-21: young generation 128 MB instead of 64) | 0.95 (`bench/alloc.lisp`) | 0.99 | 1.00 | 0.82 (the ring below 0.44; walking 2000 fresh lists 3–4 times slower) |
| `--minor-ms --no-incremental-marking` (2026-11-21) | 0.96 (`s-build-list` 0.72) | 1.00 (`b-deriv` 1.11) | 1.01 (`g-dderiv` 1.15) | 0.68 (the ring 0.17) |

Measured again in 2026-11-21 (probes of data of medium lifetime, a branch
not merged): **the scavenger loses the order of lists**. A ring of 2000
lists of 500 elements (32 MB), one replaced at each step and another one
walked, takes 7–10 times SBCL's time, and almost none of it is collection
(430 of 2300 ms): walking the lists is 45 ns a cons instead of 2–4, a cache
miss each. Walking 2000 lists made just before is 2–4 times slower than in
a process where they were not copied (`--minor-ms`, which does not move
young objects: the ring 0.17 of the default's time), and a larger young
generation makes it worse (128 MB: 3–4 times slower again), so the layout
the scavenger makes, not the size of the objects, is the cost. (Most
likely its work list: the heads of many lists found at once are copied in
turn, so the conses of each list end up strided across the copies of the
others.) No other flag changes it (`--no-parallel-scavenge`,
`--single-threaded-gc`, `--no-page-promotion`, `--no-concurrent-marking`,
`--no-parallel-compaction`, a smaller young generation).
`--minor-ms` with `--no-incremental-marking` (with incremental marking the
stores into a large array during a full marking take the slow barrier:
`b-sieve` 1.56) is the best of the alternatives: `bench/alloc.lisp` 0.96,
the probes of surviving data 0.68, but `bench/programs.lisp` 1.02, `b-deriv`
1.11, `g-dderiv` 1.15 (garbage that dies young is allocated in swept pages),
`bench/apps.lisp` and Lem within the noise, and the flags must be given on
the command line of `node` (20–30 ms more start-up: Node.js's code cache
is made with the default flags), so the defaults stay. The young
generation of 128 MB is no better: it helps the data that dies before its
second scavenge, and makes the layout of what survives worse. Script mode
runs in the main thread, whose heap is configured before `host/run.mjs`
runs; a compiled program in a worker could be given another young
generation through `resourceLimits.maxYoungGenerationSizeMb`.
Measurements of large live data on this machine also depend on the other
processes: two benchmark slots share one 32 MB L3 cache, and the same walk
takes 33 or 276 ms.

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
three): geometric mean 3.00 times SBCL's time at first, 2.12 on main after
the fifth wave and the readers' buffers (autodoc parses 210 against 492
ms, indenting 53 against 125, `form-offset` over every form 145 against
333, highlighting the whole file 126 against 311, `string-width` of every
line 86 against 126, a search 28 against 53, 150 pages down 47 against
91). `tools/bench-libs.sh bench/apps.lisp`: 1.99 (yason 1.3-2.1, cl-json
1.4-2.1, local-time 2.0, cl-ppcre's tokens 2.7, esrap 2.8). In the editor, the slowest key typed in Lisp mode now takes
60-72 ms (184-233 at first). What remains is spread: the call sites of the
readers of Lem's classes `line` and `point` (`line-length`,
`%move-to-position`, `point-change-line`, `%point<`; making their sites
smaller, 55 Wasm instructions instead of 140, did not change the time:
what is left there is the calls of these small functions and `length` of
an undeclared string), `char-width`, the
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

The redisplay of a key typed (`l-type-redisplay`; a probe typing 2000
characters with a redisplay after each, 1.5 s against SBCL's 1.0, 2026-11-20):
a little over half of its time under `redraw-display` is `char-width` and
what it calls (`control-char` twice and `icon-code-p`: `gethash` of a
character in `eql` tables, and `eastasian-code-p` through its function cell):
650 ms against about 250 in SBCL (`sb-sprof`), the hash tables' part being
most of it; the
highlighting the edit triggers (`tm-scan-line`: cl-ppcre, and the text
properties of `lem/buffer/line`, `remove-elements` as fast as SBCL's) is
most of the rest. Found on the way and made general: `last` of one argument
walked the list twice (a compiler macro calls `%last1`, one pass: 0.28 of
the time on a list of 30); `second`, `third`, `cadr`... were calls of the
`:car`/`:cdr` helpers, now nested `car`/`cdr` compiled with the type of
their argument (a cons tested inline); `equal` settles `eq` arguments and a
symbol, fixnum or character first argument inline (in the image `equal` is
a call through the function cell). The redisplay probe and
`bench/apps.lisp` take 0.99 of their time before.

A fresh profile (2026-11-21, `tools/bench-lem.sh` on a 7000-line file:
geometric mean 1.59 times SBCL at the start of the round, one run; V8's
`--cpu-prof` against `sb-sprof` on the same actions, and `node --prof` with
`--print-wasm-code` for the ticks of each instruction): no function stands
out. Of `l-type-autodoc` (`parse-partial-sexp` from the start of the
function, 230 ns a character against SBCL's 142) and `l-forms-forward`,
each Lem function called per character (`point<=`, `character-at`, the
syntax predicates, `character-offset`, `%move-to-position`,
`point-change-line`) takes 1.1 to 1.5 times SBCL's time on its own in a
loop, and the calls between them, 20 or so a character, cross files: 1.1
ns more than SBCL's for each (see Calls between modules). Of the redisplay
(`l-type-redisplay`, a probe typing 1000 characters: 225 ms against 138),
`char-width` and what it calls is 1.4 times SBCL's time, the rest 1.85
(drawing objects made with `make-instance`, `with-point` points made and
deleted, `remove-if-not` of closures over the overlays): spread too. Found
and made general: the readers of classes defined in another file had no
call-site caches (see Generic functions), `character-offset` and
`character-at` made closures for the lambdas their copy of
`%character-offset` no longer calls, `assert` put a `restart-case` in each
function (see Local functions above). Interleaved runs of Clysm alone, best
of eight, against main before: geometric mean 0.98 (`l-forms-forward`
0.93, `l-type-indent` 0.93, `l-type-autodoc` 0.96, the others within 2%);
`tools/bench-lem.sh` 1.47 after (one run). `bench/suite.lisp` (script),
`holdout.lisp`, `clos.lisp`, `library.lisp` and `apps.lisp` are unchanged
(two interleaved rounds each: within the runs' spread; the code of the
holdout's `h-matmul-double` and `h-sieve-bits`, which looked 4% slower, is
the same). Measured and not kept: no call-site caches for the readers at
all (their calls then go to the generic function: 1.43 times the time),
`mv-count` in the linear memory (1.01). A micro-benchmark of one operation
in a loop of a function called once stays in Liftoff (no on-stack
replacement): such numbers mislead by 2 to 4 times; each operation has to be
a function called many times.

The next round (2026-11-21, 1.41–1.47 at its start) found why the parts at
1.0–1.25 made 1.6: the size of the machine code of the call sites of the
readers of `point` and `line` (see "The machine code of the call sites of
slot readers and writers" under Performance). With the sites compact,
`tools/bench-lem.sh` is 1.21–1.29 (`l-forms-forward` 1.0–1.13,
`l-type-autodoc` 0.96–1.07, `l-type-indent` 1.0–1.08). What is left:
`l-line-down`, `l-page-down` and the redisplay (1.4–1.8) are mostly
`char-width` (`wide-char-p`, `control-char`, `eastasian-code-p`: hash
tables of characters and a binary search, 40% of the busy time of
`l-line-down`); `l-syntax-scan` (1.45) is cl-ppcre's matchers (the
closures of a character class, a list of tests).

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
