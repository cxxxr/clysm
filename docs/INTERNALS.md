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
  (Characters as interned structs, so that a fixnum is any i31, were
  measured and not done: Performance, "Characters out of the i31 space".)
* **NIL / T** — singleton `$symbol` structs held in immutable globals (NIL is a
  real symbol, so `symbol-name`/`symbol-plist` work on it).
* **cons** — `struct { car: mut eqref, cdr: mut eqref }`.
* **symbol** — `struct { name: mut (ref null $string), package: mut (ref null $package),
  value: mut eqref, function: mut (ref null $closure), plist: mut eqref, hash: mut i32, flags: mut i32 }`.
  Unbound = null value slot. The symbols of a whole program, a library or
  the REPL image are immutable globals made empty (`struct.new_default`),
  and `:init-symbols`, the first thing `_start` does, sets their names,
  hashes, property lists and flags from two passive data segments: the
  characters of the names, one octet each, and three octets a symbol (its
  package, its flags, the length of its name) in the order of `:symtab`,
  the vector of the symbols that `%init-packages` puts in their packages
  (`symbol-table-data` in `src/driver.lisp`). Each symbol's initializer had
  the characters of its name as an `i32.const` each, a third of the global
  section: the module of one export is 250 KB smaller (4,518,062 →
  4,267,475 bytes; with `gzip -9` 998,490 → 961,175), the REPL image 386 KB
  (9,716,468 → 9,330,167), and a library instantiates and starts in 17.5 ms
  instead of 22.1 (`lib.create()`, median), the REPL image without a heap
  image in 185 ms instead of 196 (best of 11). A name of more than
  255 characters or of a character beyond 255 is still in the initializer.
* **string** — `(array (mut i32))` of code points; **simple-vector** — `(array (mut eqref))`;
  also byte, bit and `f64` vectors. Non-simple arrays (fill pointers, adjustable,
  multi-dimensional, displaced) use a header struct around a simple array.
  A constant string of characters below 256 is a global of its length only
  (`array.new_default`, not inlined into the list or vector that holds
  it), and `:init-strings` copies its characters from a passive segment of
  one octet a character, in the order of `:strtab` (`set-string-table` in
  `src/driver.lisp`, made again after shaking): `_start` calls it after
  `:init-symbols`, and `:image-load` first, since a heap image does not
  write the contents of the image's constant strings (`:strtab` is not
  among the objects it makes empty). Its initializer had an `i32.const` a
  character: the module of one export 4,214,514 → 4,163,772 bytes (gzip
  973,724 → 971,312), the REPL image 9,355,652 → 9,285,446.
* **bignum** — `struct { sign: i32, digits: (ref null (array i32)) }`
  (base 2^32 digits, unsigned in the i32 elements; the kernels compute in
  i64, and Lisp code never reads a digit, as most are not fixnums). Integers
  beyond the fixnum range that fit in 64 bits are an **ibox**, `struct {
  value: i64 }` (24 octets), computed with i64 arithmetic; their digits are
  made when the general bignum code asks for them. (Until 2026-11-23 one
  struct of three fields held both: see "The box of an integer within 64
  bits" under Performance.)
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
  test in constant time (`inherits-depth-check-code`); the description of
  a structure known at compile time ends with the names of the structures
  it includes, so that an object known to be of it is known to be of each
  of them (`struct-class-includes-p`); a structure's
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
function tests at the call site for a closure; other objects go to the
`:coerce-function` helper, which reads the function cell of a symbol
(`coerce-function-code`; the symbol's case was at the call site too, 23
octets more at each `funcall`: see "The size of a function's code" under
Performance). `funcall` of a constant symbol is the call of the function by
its name. The calls that are rarely made — the other arm of a guarded
direct call, a call of `error`, `warn`, `signal` or of a function that
never returns — pass their arguments and the symbol to the helper
`(:cellcall k)`, which reads the function cell and tail-calls the entry
(six octets at the call site instead of 33; it returns null with an exit
pending as the call does, unlike `(:callsym k)`, which throws it).

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
values back from their known positions at the normal exit. The call that
grows the stack is hinted unlikely: without the hint the engine saved the
values live across it on every entry (a function of two special parameters,
`v-sp-param` of `bench/control.lisp`, 5.1 -> 3.9 ns a call).

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

The handlers in effect are *frames* that stay in their places: the first
`*handler-clusters*` (a fixnum, the depth) elements of the simple vector
`*handler-frames*`, each a vector `#(types clause function)`. `handler-bind`
and `handler-case` fill the frame at the current depth (the helper
`:handler-frame`, which stores the constant vector of their types only when
the frame holds another) and bind `*handler-clusters*` to one more, so that
entering one allocates nothing and stores no new object into an old one (see
"Handler frames" under Performance for why). `handler-case` is one
`sys:%catch` (a `catch` whose tag is not recorded in the list of active tags)
whose tag is its frame; `signal` goes down the frames, stores the number of
the clause in the frame and leaves for it with the condition, and the clauses
are inline code after the catch. A `handler-bind` stores its function in the
frame: of one handler the function itself, of more one closure of the index
of the type and the condition; `sys::%reuse-closure` gives it the closure
the frame holds when that is a closure of the same function over the same
cells and values, so that a `handler-bind` in a loop makes none. `signal`
calls a handler with the handlers below its frame: it writes the index of
that frame as the TYPES of the frame at the depth it was called at (a frame
that says "skip down to I") and binds the depth to one more, since the
frames in between are still in use by the code that signalled. A
computation of `js:async-function` has frames of its own (the code outside
goes on, with its frames, while the computation waits), whose first frame
continues with the frames and depth around it (TYPES is `(frames . depth)`);
a thread starts with none. `restart-case` is built the same way:
its frame `#(clauses chosen restarts function...)` is the one at the depth
`*restart-clusters*` of the vector `*restart-frames*` (the helper
`:restart-frame`), and the tag of its `%catch`, where
CLAUSES is a constant vector that names the restarts and says where their
report, interactive and test functions are (each stored in the frame only
when it is another object than the frame holds, a closure being the one
the frame holds when its function, cells and values are the same); the
restart objects are made
only when they are asked for (`compute-restarts`, `find-restart` of one of
its names, a condition associated with them: `%cluster-restarts`), and
`invoke-restart` stores the number of the clause in CHOSEN. The list of
restart objects a frame holds is emptied when the frame is entered again,
so a restart object kept from an earlier use of the frame is not among the
active ones (`invoke-restart` of it signals a `control-error`). Entering
made a restart structure for each clause, a list of them and a cons, and set
the tag of each (`v-restart-nosignal`, a `restart-case` of two restarts and
a `with-simple-restart` that are not used: 2.0 times SBCL's time), then one
vector bound to the chain `*restart-clusters*` (1.3, then 0.93 with the
other work), and with the frames nothing but the closure of a report
function whose variables changed: see "Restart frames" under Performance. `assert` and `check-type` establish
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

**The compiler in Wasm against the same compiler on SBCL (2026-11-22).**
`l-selfhost-compile` (the runtime and `bench/suite.lisp`, 30000 lines) was
1.5 times the compiler on SBCL. Profiles of both (`node --cpu-prof`, its
call tree summed by function; `sb-sprof` in `:cpu` and `:alloc` modes on
the same compilation) put the difference after the front end: reading,
macro expansion and conversion take 0.6 s in Wasm and 1.0 s on SBCL (which
compiles the macros' expanders), `build-module` (inference, code
generation, encoding) 3.1 s against 1.2 s, of it 0.55 s of garbage
collection against 0.04. What was found and done, all in the compiler's
own code unless said; its output is the same octets as before for the
runtime with `bench/suite.lisp`, a shaken program, a library shaken with
its symbols, the compiler itself and the REPL image (compiled on SBCL by
the compiler before and after, on the same sources), for `bench/suite.lisp`
compiled by the two compilers in Wasm, and for the fasls of ASDF,
`bench/suite.lisp` and `bench/programs.lisp` made by `compile-file` in the
two images (but the image's id and the marks of time):

* **A declaration of a variable bound outside was checked at every
  reference.** `with-build-section` hashes the 5.5 MB of the module in a
  loop under `(declare (type (simple-array (unsigned-byte 8) (*)) bytes))`
  written in the `let` after the parameter's binding: a free declaration,
  whose references are each `(the type (checked-value ...))`, and the check
  of that type was a call of `%array-type-match` (it finds the element type
  of the object and compares it with the normalized element type of the
  specifier): 266 ms of the 3.9 s, 8 on SBCL. The declaration is now where
  the parameter is bound, and `typep` of a simple vector of a kind of
  storage, `(simple-array (unsigned-byte 8) (*))` and the like with the
  element types of `*storage-kinds*` written as such, is the test of the
  object's kind (`%typep-expansion`, runtime/types.lisp: `ref.test` of the
  octets, else `%storage-kind` compared with the kind's code; an array with
  a header goes to the general test as before). `typep` of an octet vector
  over a list of mixed objects 89 -> 9 ns an object (SBCL 4), an `etypecase`
  of octets, strings and lists 42 -> 10 ns (SBCL 4), the loop under the
  free declaration 241 -> 11 ms for 6 MB (SBCL 6; 5 with the declaration at
  the binding).
* **`write-sequence` of octets to a file put them one by one**
  (`%fs-put-octet`, 76 ms for the module): they are copied at once
  (`%fs-put-octets` with a start and an end, `array.copy`).
* **Three walks of all the instructions before the module is encoded**:
  `inline-single-use-constants` looked for the constants that code refers
  to, and `order-functions-by-use` counted the calls of each function and
  then the uses of each global, each over the bodies of every function: 190
  ms each, 40 ns an instruction, because the instructions of a whole
  program are by then cold memory in the order the collector left them
  (below). `wasm-func-uses` walks a body once and keeps the names it calls,
  reads and sets, in order, for that body; in a whole program
  (`build-module`) it does so when the function is added, while the
  instructions are in the processor's caches: 70 ms for all of them. The
  three passes read those lists. (Tried and not kept: the walk of
  `coalesce-locals` made then too. It took half its time, and the encoding
  that follows it at the end took that much more: the first of the two to
  walk a function there pays for the memory. Encoding a function when it
  is added would save both, with the indexes of functions and globals left
  as relocations; see the negative result above for the fasls.)
* **The tables keyed by nodes.** The types of nodes (`*node-types*`), and
  what `cold-branch-p` and `arm-recurses` found (`*cold-cache*`,
  `*recurse-cache*`), are EQ hash tables keyed by conses: 865000 nodes,
  2.8 million lookups, each a call to the host for the identity hash, and a
  new entry of the host's `WeakMap` for each node (0.3 to 2 microseconds as
  that table grows; `identity_hash` and its transitions were 330 ms, 8.5%;
  SBCL spends 12% of the compilation in its own address-based EQ tables).
  The nodes that name a variable (`:lref`, `:lset`, and a `:let` by the
  first variable it binds: 44% of the nodes) now keep what is noted about
  them in the variable (`lvar-ntys`: for each table a short list of (node .
  value), the first 16 nodes of a variable, the rest in the table as
  before; `node-note`, `set-node-note` in src/types.lisp). A copy of a
  function compiled for given types has a table of its own and so a record
  of its own in the variables, dropped with it. The caches of
  `cold-branch-p` and `arm-recurses` count their entries themselves and
  are emptied at the same points as before (their answers can depend on
  when they are asked: whether a called function was found never to
  return). The other nodes have nothing cheap to hash in them, and an id
  in every node would need the nodes to be something else than lists (the
  compiler builds and takes them apart as lists in several hundred
  places). Second step (2026-11-23): a node whose first part leads to a
  variable keeps its notes in that variable too (`node-key-lvar` follows
  the first argument of a `:prim` or `:call`, the test of an `:if`, the
  first form of a `:progn`, to three parts down: `(car x)`, `(if (null x)
  ...)`), and a variable's record is a vector of up to 24 nodes with their
  notes, not a list. 71% of the 3.3 million lookups are then answered by a
  variable, 26% by the tables (constants 16%: 518000) and 3% are of nodes
  past the 24 of their variable. That a node never comes to lead to
  another variable after something was noted about it (which would lose
  the note) was checked by keeping every note in a table as well and
  comparing each answer, over a compilation of the runtime: none differed;
  and the output is the same octets (the same checks as above).
  `identity_hash` 411 -> 253 ms in a profile on a busy machine;
  `l-selfhost-compile` 3122 -> 3069 ms (the minimum of eight, medians
  3201 -> 3131), the compiler on SBCL unchanged (2505 and 2537).
  Not kept: the type of a `:const` node taken from its value where it is
  asked instead of noted (518000 lookups less). It is the type that would
  be noted, but a constant that the inference never visited then has its
  type instead of none, and the code generated differs in places (a
  library shaken with its symbols 21 octets smaller, the compiler and the
  REPL image different): not the same output, so left for a change that
  is measured as one of the generated code.
* **An instance's identity hash is a number from a counter** (runtime:
  the `:identity-hash` helper, src/prims.lisp). A structure or an object of
  a class keeps its hash in its third field; it was the host's identity
  hash, asked when the instance was first hashed: a call to the host and
  an entry in its `WeakMap` for every variable, block and function of the
  compiler, and for every structure a program puts in an EQ or EQL table.
  It is now the next value of `:ih-next`, a global that the image exports
  and the modules compiled in it import (`ih_next`, as `poll_count`): 200000
  new structures and objects put in an EQ table and looked up again, five
  times, 2690 -> 290-360 ms (SBCL 52). The number is not saved with a heap
  image (the instances' hashes are): a restored image counts again from 1,
  so two instances may share a hash, which a table tolerates.
* **Environments of the inference.** `ti-cond` returned two fresh copies
  of the environment (a vector of the types of all the variables of the
  function), `refine-env` copied one again for each refinement, and the
  join after the branches made another: 600 MB of the 1.6 GB the compiler
  allocates for this compilation (`sb-sprof :alloc`). The environment on
  entry is now one of the two that `ti-cond` returns (every caller sets
  `*env*` next and nothing else holds it), refinements of an environment
  the caller owns are made in place (`nrefine-env`), and so is the join of
  two environments that are dropped (`env-njoin`).
* `md2-version-body-p` made a closure for every node it walked (150 MB):
  two functions made once. `tree-member`, which looks for `(:br label)` in
  the code of every function that has an exit through its cleanups,
  called `equal` on every part of the code (3% of the compilation on
  SBCL): the conses that begin with the operator are compared.

`l-selfhost-compile` (`tools/bench-libs.sh`, the best of three, twice, the
tree before and the tree after in turn on one slot; ms):

| | before | after |
|---|---|---|
| the compiler on SBCL | 2673, 2633 | 2710, 2536 |
| the compiler in Wasm | 4112, 4046 | 3387, 3332 |
| ratio | 1.54, 1.54 | 1.25, 1.31 |

(Ten rounds of the four in turn while the other slots and the test CPUs
were busy, minimum and median: on SBCL 2881, 3031 -> 2739, 3006; in Wasm
4554, 4828 -> 3622, 3970; 1.58 -> 1.32.) The other items of
`tools/bench-libs.sh` and those of `tools/bench-startup.sh` are as before
(`s-ql-alexandria` 335-343 -> 328-332 ms, `s-ql-cl-ppcre` 350-361 ->
331-349, `s-require-asdf` 182-193 -> 181-182, `s-start` 71-72 -> 67-69).

First builds in the image with an empty fasl cache (a whole process each,
the original tree and the new one in turn on one slot): loading Quicklisp's
setup, which compiles ASDF, 4.07 -> 3.54 s (median of three; 3.54 -> 3.32
the best); `(ql:quickload :cl-ppcre)` 2.00 -> 1.85 s and `:alexandria` 0.89
-> 0.86 s (the best of eight; medians 2.04 -> 1.96 and 0.94 -> 0.92; SBCL
0.92 and 0.45). The small ones are mostly the cold compiler (V8 compiling
the image's functions as they first run), which these changes do not
touch. `bench/suite.lisp`, `alloc`, `clos`, `programs` and `library`: the
geometric means within 0.02 of before over two rounds each, no item
outside the noise. Size: the REPL image 8875825 -> 8892569 octets (the
compiler's new functions, the tests of kinds where `typep` called a
function), the shaken module of one export (`make test-lib`) 925558 ->
924453.

What remains of the difference (profile after the changes): garbage
collection, 0.5 s on a quiet machine and over 1 s when the other cores use
the memory, against 0.04 s on SBCL: 2.8 GB are allocated and 450 MB are
alive at the end (the nodes and the instructions of the whole program,
whose conses are 32 octets against 16), and each scavenge copies what is
alive of the last 16-32 MB; walking instructions and nodes that the
scavenger has laid out (encoding 0.45 s against 0.15; a walk of fresh
instructions is 5 ns an instruction, of scavenged ones 40: no scavenge at
all, a young generation of 1 GB, makes `wasm-func-uses` 41 ms instead of
242, but the process is no faster for it, it pays in page faults); and the
identity hashes of the remaining nodes (0.2 s).

**The functions of a whole program are encoded when they are compiled
(2026-11-23).** `add-func` encodes the body of a function at once
(`encode-function-early`, src/wasm.lisp) when the module is a whole
program that is not shaken (the module's `early` slot, `build-module`),
and drops its instructions. What is not known then is left out of the
octets and noted as a relocation, (position, kind, nref) in a vector: the
index of a function (a call, a `ref.func`, the call where a checked cast
fails) and the index of a global, since `order-functions-by-use` puts both
in order at the end; whether a global read is followed by
`ref.as_non_null` (the closure's global may be made after); and the
functions that `ref.func` declares, whose order is that of the table of
functions. `encode-function-body` then copies the octets between the
relocations and writes the indexes, and moves the offsets of the branch
hints by what was written before them (a hint at the position of a
relocation is of the instruction after it). An nref is the record of a
name the encoded functions refer to, found by one hash of the name when
the function is encoded: it counts the calls, reads and writes as they are
encoded, which is what `order-functions-by-use` and
`inline-single-use-constants` asked of each function's instructions (no
walk for them at all now), and holds the index while the module is
encoded, so that the name is not hashed again for each reference. A
function that cannot be encoded yet (a type not yet in the module) keeps
its instructions and is encoded with the module as before; so are the
functions of a shaken program, which `shake-module` and src/preinit.lisp
walk and rewrite, and those of modules made in the image (src/live.lisp,
which finds the helpers a module needs from the instructions and encodes
a unit's functions twice). `tools/dump-func.lisp` keeps the instructions
(`*keep-instructions*`).

The octets of the modules are the same (runtime + `bench/suite.lisp`, the
compiler, the REPL image, on SBCL and in Wasm; the shaken ones do not take
this path). The instructions were 200 MB of conses alive until the end of
the compilation, which every scavenge until their promotion had copied,
and cold when the encoder walked them: the heap at the end 407 -> 240 MB,
the process 770 -> 606 MB, the time in scavenges 1743 -> 1067 ms (on a
busy machine). `l-selfhost-compile`, ten rounds in turn (minimum, median;
ms):

| | notes in variables | encoded early | with nrefs |
|---|---|---|---|
| the compiler in Wasm | 2950, 3059 | 2586, 2841 | 2543, 2700 |
| the compiler on SBCL | 2337, 2509 | | 2177, 2302 |

The earlier attempt at this (above: each segment of a file's unit encoded
when the segment ended, no faster) was in the image, where a unit's
instructions are a few megabytes that are encoded soon after; here they
are the whole program's. Both hosts gain, so the ratio moves less than
the times: 1.26 -> 1.17. What is the compiler's own in the remaining
difference: the scavenges (2.4 GB allocated, 240 MB alive at the end),
and the host's identity hash for the nodes that lead to no variable.
(The types that are only kinds, no range and no class, are one object for
each set of kinds, `kinds-ty`: 50 MB less allocated, 3187 -> 3123 ms.)

All of the above against the tree before it (`tools/bench-libs.sh`, the
best of three, twice, in turn on one slot, 2026-11-23): the compiler in
Wasm 4025, 3751 -> 2713, 2543 ms, on SBCL 2519, 2500 -> 2181, 2159,
`l-selfhost-compile` 1.60, 1.50 -> 1.24, 1.18; the other items of
`bench/libs.lisp` and of `tools/bench-startup.sh` as before. First builds
in the image (the best of six): cl-ppcre 1503 -> 1391 ms, alexandria 693
-> 681 (the modules made in the image are not encoded early). The REPL
image 8875825 -> 8930905 octets with all of it.

**The modules made in the image are encoded early too (2026-11-23).**
`add-func` acts by the module's `early` slot: `:program` as above; `:live`
for the modules of src/live.lisp (a form, the unit of a file or of a
system, the module of each segment of a system compiled file by file),
which first makes the function's list of `wasm-func-refs` (the functions,
globals and cast types it refers to, each once in the order of the code:
what `add-called-helpers`, `live-used-globals`, `global-segments`, the
units' imports and now `remove-unreachable-functions` go by), then encodes
it and drops the instructions; `:uses` for a shaken program. A function of
a unit is encoded once and its relocations written for each module it is
put in (its segment's and the unit's): the index an nref holds is marked
with the encoding it is for (`*encode-token*`). Not with `*live-dump*`,
which prints the instructions. The fasls are the same: `compile-file` of
ASDF, `bench/suite.lisp` and `bench/programs.lisp`, and the system fasls
of alexandria, cl-ppcre, babel and split-sequence with Quicklisp's own
files, compared with those the image before made (all but the image's id
and the marks of time). `compile-file` of ASDF 2925 -> 2727 ms, of
`bench/programs.lisp` 853 -> 738; first builds with Quicklisp (the best of
eight): cl-ppcre 1376 -> 1246 ms, babel 1231 -> 1174, alexandria 655 ->
663.

**The body of an interpreted function is made closures (2026-11-23,
runtime/eval.lisp).** The compiler in Wasm runs the expanders of the
macros of the program it compiles in the image's evaluator (the compiler
in the REPL image compiles one after a few uses; SBCL compiles them all),
and `%eval` walks a form every time: it finds the operator, looks each
variable up by name in the environment, and expands the macros of the
expander's own body again at every call (the backquote 37 ms,
`destructuring-bind` 24 ms: 276 ms of the 2.3 s of `l-selfhost-compile`
were interpretation). From its second call on, an interpreted function
runs closures made of its body (`%cc`: a function of the environment for
each form, macros expanded once, a lexical variable taken from its place
in the environment). The environment stays `%eval`'s list with the same
entries, so a form `%cc` does not take apart is left to `%eval` in it
(bindings of special variables, `flet`, `labels`, `macrolet`, symbol
macros, `progv`); the static environment is the environment of the call
that makes the closures, extended as they will extend it. A function
whose parameters are all required and not special binds them without the
walk of its lambda list (one function for the two cases, with the same
free variables: the functions of a generic function that interpreted code
defines are put together into one closure, `%make-gf-closure`, which takes
them to have one layout; ASDF's `find-component` trapped otherwise).
`*eval-closures*` NIL gives the evaluator as it was. Interpretation in `l-selfhost-compile` 276 -> 26 ms, the allocation
of the compilation 2374 -> about 1900 MB (with the next change);
`(fib 22)` interpreted 13 -> 6 ms. The macros of an expander's body are
expanded once, as when it is compiled: the constants of its backquotes
are then the same objects in every expansion (they were made again at each
call), and the compiler in Wasm merges them as the compiler on SBCL
always did: its module of the runtime with `bench/suite.lisp` is 4.7 KB
smaller (17 globals less), no longer the octets it made before (the
compiler on SBCL makes the same octets as before).

**The type of a constant is the type of its value (2026-11-23).**
`node-type` of a `:const` node is `const-ty` of the value, not a note in
`*node-types*`: 518000 lookups by the identity of a cons less (the
constants were most of what was left in the tables), `l-selfhost-compile`
2447 -> 2346 ms (medians 2516 -> 2423). A constant that the inference did
not visit (one the code generator made) now has its type where it had
none, so the code differs in places (a library shaken with its symbols 21
octets smaller). The benchmark files against the tree before, two rounds
each in turn: `suite` 0.71, 0.70 -> 0.70, 0.71; `programs` 0.78, 0.78 ->
0.78, 0.79; `declared` 0.94, 0.94 -> 0.94, 0.93; `gabriel` 0.87, 0.81 ->
0.85, 0.87; `library` 0.74, 0.74 -> 0.75, 0.74; `control` 0.73, 0.75 ->
0.75, 0.72; `alloc` 1.37, 1.33 -> 1.37, 1.33; `clos` 1.08, 1.08 -> 1.06,
1.09; no item moved beyond the noise.

**What the code generator allocates (2026-11-23).** `sb-sprof :alloc` of
the compilation after the changes above, 1.75 GB on SBCL: lists 470 MB,
of which `append` (the splices of backquotes copying the code of the
parts into that of the whole) is 80 MB, 5% of all; the rest of the lists
is the instructions themselves, made once each by some hundred
functions, none more than a few megabytes. So splicing without copying, or
shared `(:local.get n)` instructions, would save a few percent of the
allocation at most; writing octets instead of instructions would save the
lists, and is a rewrite of how every `cg` function returns its code. The
larger allocators were elsewhere: the copies of the inference's
environments (225 MB: `ti-exit` 70, which copied the environment at every
`return-from` and `go` before joining it with the exit's; it joins the
environment itself now), the types (122 MB before `kinds-ty`), the
evaluator (above). Time in scavenges for `l-selfhost-compile`: 286 ->
255-263 ms, allocation 2374 -> 1905-1936 MB.

With all of it, against main before any of these changes
(`tools/bench-libs.sh`, the best of three, twice, in turn on one slot,
2026-11-23): the compiler in Wasm 3883, 3669 -> 2488, 2331 ms; on SBCL
2510, 2440 -> 2181, 2160; `l-selfhost-compile` 1.55, 1.50 -> 1.14, 1.08;
the other items of `bench/libs.lisp` and `tools/bench-startup.sh` as
before or a little lower. First builds with Quicklisp and an empty cache
(the best of six, in turn): cl-ppcre 1800 -> 1426 ms, babel 1247 -> 1132,
alexandria 879 -> 803; Quicklisp's setup, which compiles ASDF, 3240 ->
2640 (the best of three). In the 2.1 s that the compiler in Wasm now
takes for the benchmark (a profile on a quiet slot): garbage collection
275 ms (SBCL 40), the notes of nodes 230 ms (`node-note`, `set-node-note`,
`node-key-lvar` 136, `identity_hash` 76 for the nodes that lead to no
variable), the walk of `inline-calls` over every function 100 ms,
`map-subnodes` 93 ms of its own in all the walks that use it; the rest is
flat (no function above 2.5%).

On the tree of 2026-11-23 that has the pass leaving out dead code
(`prune-dead-code`, which early encoding encodes the result of) and the
other changes of that day, the same comparison (before: that tree without
these commits): the compiler in Wasm 4309, 4086 -> 2507, 2630 ms; on SBCL
2784, 2665 -> 2259, 2354; `l-selfhost-compile` 1.55, 1.53 -> 1.11, 1.12.
First builds (the best of six): cl-ppcre 1613 -> 1370 ms, babel 1486 ->
1315, alexandria 738 -> 719. The octets are those of that tree's compiler
with the two changes that are meant to change them (the identity hash of
instances, the types of constants): runtime + `bench/suite.lisp`,
`bench/programs.lisp` shaken, a library shaken with its symbols, the
compiler, the REPL image, and `bench/suite.lisp` by the compiler in Wasm.
(A place that wrote a node's type into `*node-types*` itself, added that
day in `cg-let`, lost the type of a node kept in its variable: the types
of nodes are read and written through `node-type` and `set-node-type`
only.)

**Tried and not kept: a `case` of many keys as a `br_table` (2026-11-23).**
`case` is a chain of `eql` tests in Clysm and a jump table in SBCL, and the
dispatch of `map-subnodes`, `ti-1` and `emit-instr` on the kind of a node
has up to 40 keys. Implemented: a special form `(%switch index arm...)`
(an IR node `:switch`: the arm numbered by the index, a `br_table` in
nested blocks, typed as an i32 when its arms are integers), and `case` /
`ecase` of eight keys or more that are all symbols, all characters or all
fixnums expanding to the number of the clause found by a `%switch` (for
symbols on the low bits of the symbol's hash, which is the hash of its
name and known when the code is compiled, each arm comparing the one or
two keys of that hash; for characters and fixnums on the value less the
smallest key) and then either a balanced tree of `if`s on that number,
with the clauses as its leaves (they stay arms of `if`s: tail calls, typed
values), or a second `%switch` with the clauses as arms. All tests of
tests/cases passed with it. A dispatch (a call of a function that is one
`case` returning a constant, keys drawn in a pseudo-random order and then
in a fixed cycle; ns for the call and the loop around it, best of three,
on a benchmark slot):

| keys | chain, cycle | search, cycle | chain, random | tree, random | two tables, random | SBCL |
|---|---|---|---|---|---|---|
| 8 symbols | 6.0 | 6.0–6.3 | 5.0 | 16.5 | 17.0 | 3.7–4.3 |
| 16 symbols | 6.0–6.3 | 6.0–6.3 | 5.3 | 15.5 | 16.2 | 4.7 |
| 24 symbols | 6.3 | 6.0–6.3 | 6.0 | 15.5 | 16.7 | 4.3–4.7 |
| 40 symbols | 7.0 | 6.0–6.3 | 7.0 | 17.0 | 19.0 | 4.7–5.0 |
| 40 characters | 7.0–7.3 | 6.0–6.3 | 7.0–7.7 | 15.0 | 15.8 | 4.0–4.3 |
| 40 fixnums | 7.0 | 6.0–6.3 | 6.7–7.7 | 14.5 | 15.5 | 4.3–4.7 |

The chain costs 0.03–0.05 ns a key (V8 compiles it to a compare with the
symbol's global and a branch for each key, and the processor predicts the
one branch that is taken): 40 keys are 1 ns more than 8, which is all a
table could save, and it saves it only when the keys come in an order the
predictor of indirect branches learns. When they do not (the order of a
linear congruence here), the `br_table`s are mispredicted indirect jumps,
9 to 12 ns for the dispatch, and the chain is 2 to 3 times faster than
the search. So the chain stays; the code was removed.


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
* A compiled form that is not a definition (a call of the functions, a
  `defbench`-like macro, a `let` around a `setf`) no longer ends the batch
  (2026-11-22): the definitions and forms after it are read and compiled
  into the same module before it has run, so that a function or closure
  defined before it and called after it (a closure in a `defvar`, a
  function in a property list, a helper called by the next benchmark) is
  called within the module. What reading or compiling the forms after it
  depends on must not change when it runs: `%batch-passable-p` lets it pass
  only when, processed as the compiler does at top level (`progn`,
  `eval-when` of `:execute`, macros expanded), it defines no macro, type,
  structure, class, setf expander, symbol macro, constant or package (those
  forms end the batch as before) and its code does not mention what changes
  the reader or the compiler's environment (`%batch-unsafe`:
  `set-macro-character`, `*package*`, `*read-base*`, `*features*`, `eval`,
  `load`, `proclaim`, `export`, ...; found by name when first needed, since
  symbols in a constant would keep `compile-file`, `require` and the
  defining macros in a program whose unused functions are left out). What it changes anyway, through
  functions it calls, is seen when it has run: a `(%batch-check k)` after
  it compares `*package*`, `*readtable*`, the reader's variables, `*features*`
  and `*env-epoch*`, which `%set-macro-function`, `proclaim` (not
  `declaim`, which the compiler saw), the readtable's and the packages'
  functions and `load` count up, with their values when the batch began to
  run (`%batch-begin`); when one differs it throws, the rest of the batch
  does not run and the file is read again from after that form. A `#.` in
  a form read after such a form is not evaluated (`*batch-read-ahead*`):
  the batch runs and the form is read again. None of the benchmark files
  stops a batch so. `tests/cases/load-batch-ahead.lisp` checks a macro, the
  reader's base, a reader macro, a `#.` and a package changed by functions
  that such a form calls. A `defbench` file is now a few batches of 32
  forms: `t-throw-closure` in a script 26 → 13 ms (compiled 12),
  `r-funcall-return` 95 → 50, `g-dderiv` 245 → 178 (compiled 142).

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
  call into another module. Tried and not kept (2026-11-21): a copy of
  the runtime's small functions with a loop or a recursion (938 of them up
  to 150 conses, 1089 up to 250, their sources kept in the image as the
  inline sources are: +370 KB) compiled into each fasl and each batch of
  `load` that calls them, at most 2000 conses of copies per module, in
  place of the call through the symbol that no compiling of the call
  applied to (`equal` split into its common cases and `%equal-other` to be
  small enough). The probes got faster (`equal` of a list in a loop 90 →
  48 ms, `%array-storage` 25 → 8), the programs did not: in a script
  `bench/suite.lisp` 1.07 times its time, `programs` 1.01, `gabriel` 1.01,
  `control` and `clos` 1.00 (`p-print-object` 1.5, `b-sort` 1.2 slower),
  and `tools/bench-lem.sh` 1.45/1.49 against 1.45/1.47, `bench/apps.lisp`
  1.26/1.32 against 1.28/1.27, `bench/libs.lisp` 1.19/1.14 against
  1.19/1.19 (two rounds each). Pitfalls met on the way: a runtime function
  may be written as a call of itself that the compiler open-codes
  (`(defun cddr (x) (cddr x))`), so its source cannot be renamed; a
  compiler macro may turn the copy's body into a call of the function
  itself (`equal`'s calls `%equal-2`, whose source is `(equal a b)`);
  the copy needs the block of the function's name; and copies in every
  form at the REPL made ANSI's file tests slow enough for its watchdog.
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
  `b-tak` in a script 117 → 95 ms (whole program 86). The modules of fasls
  (a file compiled into one module) are 56% small on average already (of
  Lem's 448, 135 under half, 14 under 40%; Babel's `strings.lisp` 14%), as
  is the REPL image itself: the 2753 fillers that Lem's fasls got changed
  nothing measurable (`tools/bench-lem.sh` 1.49 and 1.56 against 1.50 and
  1.49 without them, `bench/apps.lisp` 1.29 and 1.28 against 1.30 and 1.27,
  `bench/libs.lisp` 1.17 and 1.18 against 1.16 and 1.14; two rounds each,
  interleaved on one slot).
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
  1.02 times their time, scripts 1.04 and 1.06): not kept. (2026-11-22:
  `t-throw-closure` was no case of it: a full collection before it, a
  `sleep`, made it no faster, and a store of a symbol read by the same form
  into an old symbol's value in a loop took the same time as one of an
  image's symbol. Its thrower is a closure of a `defvar`, compiled in
  another module than the loop calling it; with both in one batch it
  takes the compiled program's time.)
* **The effective methods of `:before` and `:after` methods and of the
  short method combinations were closures of the runtime** calling the
  methods (`%standard-fast-emf`, `%short-fast-emf`), so in a script or a
  fasl the call of each method was a call into another module. The
  expansion of `defmethod` of a method with one such qualifier now also
  makes the combining code for the arity of its function
  (`%combiner-form`: the `:before`/`:after` loops, `progn`, `and`, `or`,
  `+`, `list`, and `append`, `nconc`, `max`, `min` of two or three methods
  without a list), noted with the method (`%note-method-combiner`, in
  the method's `%extra`), and the effective method is made by the combiner
  of one of its methods of that arity, in their module (2026-11-22; only
  for methods compiled in the image, `*native-eval-hook*` being set: in a
  whole program the runtime's closures are in the methods' module):
  `c-comb-all` in a script 137 → 98 ms (compiled 87), `c-short-combination`
  61 → 45 (35), `p-method-comb` 88 → 63 (46). `tests/cases/method-combiners.lisp`.
* What `MODE=script` also shows comes from causes described elsewhere: a
  guarded copy of a fixnum function whose value is assigned back
  (`a-lcg`, `p-hash-fn`'s `xorshift`), a loop at top level in Liftoff.

**Script against compiled, benchmark by benchmark** (2026-11-23). Each
benchmark of `bench/suite`, `programs`, `gabriel`, `control`, `clos`,
`holdout`, `library`, `functional`, `closures` and `arrays` was timed loaded
from source and as a whole program (the same slot, the runs interleaved,
best of three), and for those above 1.05 the profiles of both
(`node --cpu-prof`, self time per function) and the emitted Wasm
(`*live-dump*`, `tools/dump-func.lisp`) were compared. The times are those
that the benchmarks print, after a first run of each: compiling is not in
them. (It is in the time of the process: `bench/programs.lisp`, 870 lines,
takes 7.1 s of CPU as a script, of which `live-compile-forms` 1.0 s: 0.8 s
type inference and code generation, 0.13 s finishing and encoding the
modules; reading the forms ahead and `%batch-passable-p` do not show.) What
the differences are made of:

* **A function of the runtime that the engine inlines in a whole program is
  a call in a script.** `v-plist` (1.7: `get`, `getf`, `remprop`,
  `get-properties`), `b-format` (the functions that the compiler macro of
  `format` calls: `sys::%fmt-decimal`, `sys::%fmt-princ`), `p-equal`,
  `v-find-adjustable` (`char-equal` under `find`), `p-coerce`, `p-symbols`,
  `v-assoc-member`: in the whole program the profile has their time in the
  benchmark's own function, in the script in the functions of the image.
  The call costs 2 to 3 ns more than the inlined code (`b-format`: 36
  million calls, 84 against 16 ms of self time in the benchmark's function).
  *How the call is made is a small part of it*: a call of up to four
  arguments of a function of another module is now a call of an imported
  function in a module evaluated at once (the REPL, `load` of a source
  file; `live-direct-call-code`, `src/codegen.lisp`): the image's entry of
  that function for the number of arguments, which the host asks the image
  for (the exported helper `:direct-entry`, from the closures that the
  compiler returns with the module, `sys::*direct-closures*`) when it
  instantiates the module. The call is guarded by one `ref.eq` of the
  symbol's function with the closure it had when the call was compiled, a
  constant of the module; when it differs (a redefinition, `trace`, `(setf
  fdefinition)`, `fmakunbound`) the call goes through the function cell as
  before (`(:callsym k)`). `bench/library.lisp` 0.976 of its script time,
  `suite` 0.987, `programs` 0.990, `control` 0.992, `clos` 0.994 (four
  rounds, the order alternating). `tests/cases/direct-entries.lisp`. Not
  for a module of a fasl or one that a heap image may hold
  (`*live-snapshots*`): their imports are resolved by name when a heap
  image is linked (`host/link.mjs`), and a unit's code is shared with the
  modules that run its segments. For fasls it would take the symbols of
  the imported functions written with the module, resolved by `load`, the
  same imports in the modules of the segments, and no such module in an
  image that records its modules. The engine inlines no imported function
  either: what the whole program gains by inlining stays.
  **Copies of the runtime's leaf functions** were measured for that
  (2026-11-23) and not kept: the small functions with a loop whose code
  calls only themselves, each other, functions with an inline source and
  functions that signal (66 of 194 with a loop and at most 150 conses:
  `getf`, `sys::%putf`, `sys::%assoc-eql`, `sys::%member-eql`, `nth`,
  `nthcdr`, `remprop`, `get-properties`, `copy-list`, `butlast`...; +33 KB
  of sources in the image) copied as a local function (`labels`) into a
  caller that calls them in a loop, under the guard of the symbol's
  function (in a fasl against the image's function, kept as a property).
  On top of the imported entries: `library` 0.99 (`v-plist` 186 → 154 ms
  with both, the whole program 107), the other files unchanged, and
  `p-generic-2d` 83 → 148 ms: a local function in a loop keeps the loop
  from the versions that the image's loops get (guards hoisted, arrays of
  two dimensions). `bench/libs.lisp` and `bench/apps.lisp` (fasls) 1.003
  (both orders). Such copies would have to be functions of the module that
  the loop optimizations know as calls without effects on the guards.
* **`#'1+` given to a function of the file** (`k-church` 2.5,
  `k-generators` 1.2): the `funcall` of it is a `call_ref` whose target the
  engine inlines in a whole program and cannot in a script (`1+` of the
  image: 1.7 ns a call). A closure of the user's module doing the same
  would not be `eq` to `#'1+`.
* **Copies that made the callers too large.** `p-method-comb` (1.4): the
  methods were in their caller's module and still not inlined, because each
  copy of a small function that checks an argument carried a copy of
  `sys::%type-error`'s call of `error`. Functions that
  only signal an error keep no source now (`noreturn-source-p`,
  `src/driver.lisp`): `p-method-comb` 61 → 49 ms (the whole program 45).
* **The dispatch of `gethash` on the key** (`p-word-count` 1.5,
  `b-hash-string`): `sys::%gethash-marked` and
  `sys::%ht-put-other`, which the compiler macros call for a key that is no
  fixnum, character or symbol, were calls into the image before the helper
  of strings and lists. They keep their source (`*runtime-inline-also*`):
  `p-word-count` 103 → 76 ms (80).
* **A constructor named as its structure** (`p-struct-float` 1.27: `(vec3 x
  y z)`) was taken for a recursive function, as its body mentions its name
  in `(find-class 'vec3)`, and not copied into its callers
  (`tree-calls-p`, `src/frontend.lisp`): 66 → 61 ms (54), the doubles no
  longer made objects for the call.
* **A guarded copy that is not one tree of arithmetic** (`p-hash-fn` 2.2):
  the integer accumulators take the value of any copy that is an integer
  within 64 bits (`cg-dual-i64`, described with the copies below): 96 → 45
  ms (45).
* `p-equal` (1.6): the two `"two"` of one form are one object in a compiled
  file and two in a loaded one, where `equal` compares them character by
  character. `p-signal-warn` (1.6), `p-clos-app`, `p-cnm-args`,
  `c-cnm-class-change`: the runtime's signalling and dispatch calling the
  user's closures and back, each a call between modules.

Also measured and not kept:

* **Sources of more functions of the runtime.** Of all its small functions
  without a loop (806, +94 KB): `bench/library.lisp` 1.044 of its time
  (`suite` 0.995, `programs` 0.989): a copy whose body calls two functions
  of the image makes two calls between modules of one. Of those whose body
  makes at most one call on any path (339 more, +44 KB): `programs` 0.988,
  `library` 1.000, `suite` 0.994. Of those that call nothing (131 more, +17
  KB): no difference from the selection kept (0.993 against 0.995 over 149
  benchmarks).
* **Larger batches**: 128 forms in place of 32: `programs` 1.020, `clos`
  1.025, `control` 0.981, `gabriel` 0.989; 12 forms: 1.007, 1.019, 1.042,
  0.995. The batches already end where they must (a `defclass`, a
  `defstruct`, a `defmacro`: `bench/programs.lisp` is 9 batches and 6
  forms compiled alone).

(These two were measured with the variant always run after the build
before it, which the measure below, with the order alternating, showed to
cost the later one 1 to 2%.)

A benchmark of some 20 ms measured after others in one process moves by 10
to 20% with what ran before it (`p-handler-case` 37 or 45 ms, `v-hash-equalp`
19 to 26, between builds that compile them to the same code), and the
copies of `gethash`'s dispatch make the short hash benchmarks slower
there while the same benchmarks alone, repeated three times as long, are
faster (`v-hash-equalp` 88 → 58 ms): presumably the copy in the user's
module starts in the engine's baseline tier, where the image's function was
optimized by the benchmarks before.

With what is kept (script time against the build before, and against the
same file compiled, before → after; four rounds of compiled, before and
after, the order of the last two alternating, best of four): `suite` 1.013
(1.02 → 1.03), `programs` 0.986 (1.085 → 1.07), `gabriel` 0.981 (1.08 →
1.06), `control` 0.995 (1.09 → 1.09), `clos` 0.981 (1.09 → 1.07), `holdout`
0.970 (1.01 → 0.98), `library` 0.980 (1.14 → 1.115), `functional` 1.002
(1.09), `closures` 0.988 (1.16 → 1.14), `arrays` 1.004 (1.01 → 1.02),
`arith` 1.026 (0.98 → 1.00); all 251 benchmarks 0.992 (1.074 → 1.065).
Slower: `v-hash-equalp` 18 → 23 ms and `a-hash-counts` 48 → 57 (the copies
of the dispatch, see above), `p-seq-vector` 42 → 47. Whole programs are the
same octets as before; `bench/libs.lisp` and `bench/apps.lisp`, whose
libraries are fasls, 1.005 of their time (the later of two runs). The REPL
image is 9.4 KB larger (8,875,571 → 8,884,982 octets), the module of one
export unchanged (925,558). With the imported entries (the next commit) all 251 are at 1.047 of
the compiled time in the measure that had the leaf copies too (`library`
1.08, `closures` 1.125, `clos` 1.06, `control` 1.07, `programs` 1.07; the
REPL image 15 KB larger again, 8,899,983 octets).

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
of them as a whole program does. So do the small functions of the runtime
that their sources call, except those that only signal an error
(`noreturn-source-p`: inside the copy of each small function that checks
an argument, `endp` for one, `sys::%type-error` was copied too, as `(error
'type-error :datum ... :expected-type ...)`, a keyed call with its two ways
of passing the arguments on a path that does not run; the bodies of methods
got too large for the engine to inline them), and the dispatch of
`gethash` and `(setf gethash)` on the kind of the key
(`*runtime-inline-also*`); see "Script against compiled, benchmark by
benchmark" above. The guarded copies of small functions
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
  `i31` where it is assigned to a variable (the fixnum helpers 5-6 ms
  against 3). The types of the loop's version come from the inference of
  the whole function, in which the else branches exist; inferring the
  version again with the guards true (as `compile-spec-clone` does for a
  function) would type such variables, but the variables would then need
  representations of their own in that version, converted where it hands
  over to the other. For the commonest case the integer accumulators do
  without it (2026-11-21): a variable assigned the value of a guarded copy
  called on it, `(setq x (lcg x))`, whose copy ends in integer arithmetic
  of the fused trees under `let`s binding only the variable itself
  (`guarded-int-copy` in `src/typed.lisp`), counts that arithmetic as an
  update (`dual-uses`), so the variable gets an i64 beside its eqref local
  in both versions of the loop, which share it; `dual-lset-code` stores
  the copy's arithmetic (with the copy's parameters replaced by the
  variable, whose i64 is read) into the i64, and the call made when the
  guard fails into the eqref local. `a-lcg` in the image 18 → 4 ms (as a
  whole program, 4; it was 26 before the integers beyond the fixnums
  stayed in i64), `bench/arith.lisp` in the image 0.875 of its time
  before; the other files unchanged. A copy that is other code (several
  statements, assignments of its parameter: `p-hash-fn`'s `xorshift`) was
  still made an integer at each assignment, a bignum struct for a value
  between 2^29 and 2^32. When the copy's value is an integer within 64
  bits whatever its argument (the type of the copy's body: it ends in
  `logand` with a constant, `mod`, `ldb`...), the assignment is an update
  of the accumulator too (2026-11-23, `int64-copy-p`): `cg-dual-i64` compiles the
  copy to an i64 (`cg-rep`) stored in the variable's i64, the call that a
  failed guard makes into its eqref local; and a `let` variable bound to an
  accumulator in code compiled in the image (the argument of the guarded
  copy, the copy's parameter) gets a pair of locals of its own, copied
  from the accumulator's without making the integer (`dual-alias-src`).
  `p-hash-fn` in a script 96 → 45 ms (the whole program 45).
  `tests/cases/guarded-accumulators.lisp` (starts beyond 64 bits, not
  integers, the function redefined to return a list).
* Doubles in a loop's version (2026-11-21). Before the version (with the
  guards taken as true) is compiled, `version-typed-vars` finds the
  variables that are doubles there: all those assigned or bound in the
  loop are taken as doubles and one is dropped when its value or one of its
  assignments is not a double with the others' types (a fixed point; the
  types of references of those variables come from `*version-lvar-tys*` in
  `node-ty`, and `assumed-node-ty` then also follows a tail that is
  arithmetic or a variable). A variable bound in the loop gets an f64
  local in the version (its representation set for that compilation and
  restored: it is not live at a segment's head, where the versions hand
  over). A variable bound outside it must be an unboxed double variable
  (`fdual-local`, its eqref and f64 locals): the version is entered only
  when each holds a double (`version-entry-code`, which unboxes one held
  boxed into the f64 local), and in it the f64 local always holds the value
  (`*version-fdual-vars*`): reads take it (`cg-rep-1`, `spec-leaf-class`),
  stores write it and clear the eqref local (`dual-lset-code`), so that the
  pair is valid for the general version, which needs no conversion where it
  takes over (an interrupt check after a redefinition) nor after the loop.
  `(setq v (+ (decay v) 0.001d0))` with `decay` of another form: in the
  image 22 → 17 ms (as a whole program 16-17); the bench files loaded into
  the image unchanged (their loops have no such variables).
* A loop at top level runs once, in Liftoff: 14 ms against 7 with
  `--no-liftoff` (as in a whole program, whose `main` also runs once).
  Outlining the loop into chunks called repeatedly was measured by hand and
  does not pay: a local function is inlined back (28-44 ms), and a global
  function taking the loop's variables as parameters loses their types
  (30-46 ms); outlining would need typed parameters (a version of the
  outlined function for the types the variables have).

**Entry versions of local functions and closures.** A local function or
closure with a loop whose parameters or free variables are used as numbers
gets entry versions too (`entry-cloneable-p`, `entry-clone-free-vars` in
`src/codegen.lisp`): its signature covers the required parameters and then
the free variables that are neither special nor assigned (whose value is
the closure's for the whole call); the general version tests the
parameters and the closure's fields (`closure-field-code`) and tail-calls
the copy with the same closure, which is analysed with those types
(`compile-spec-clone` sets the free variables' type and representation for
the copy and restores them) and copies those free variables into unboxed
locals on entry (`*clone-free-reps*`, `setup-free-vars`). A body with a
lambda or local function of its own still gets none (it would be compiled
twice). Other direct calls do not choose a copy (only the entry does); in the
copy itself, a call of the function with its own closure (a recursive local
function) calls the copy directly when the arguments are of its classes
(`self-clone-call-code`, `*spec-self*`): a fixnum or double by type, a
32-bit integer (`(1+ row)` of a fixnum) after an inline test of the fixnum
range, else the general version (`p-queens` 219 → 209 ms, five interleaved
rounds). An element access to an array of unknown type
whose subscript is a 32-bit integer not known to be a fixnum (`(+ row c)`
of two fixnums) takes the i32 as it is (`speculative-locate-code-1`: out of
range it is beyond every dimension compared unsigned), boxed only for the
generic function. Measured (2026-11-21, best of three interleaved runs on
one slot against the build before): `p-queens` 137 → 107 ms (SBCL 66),
`k-church` 41 → 27 (SBCL 76; in the image 78 → 53), `k-cps-tree` 87 → 79,
`k-funcall-args` 113 → 102; `bench/closures.lisp` 1.00 → 0.95 times SBCL,
the other files unchanged within noise.

The three vectors of `p-queens` (`(make-array n :initial-element nil)` of
an `n` of unknown type) were still of unknown type in the copy: the inline
`make-array` has a branch for a fixnum size (a simple vector) and one for
anything else (a simple array of any rank over the same storage), and the
union of the two had no class. It is now that array of any rank
(`union-array-class`, `src/types.lisp`), whose one-subscript `aref` tests
for the storage only; `p-queens` 110–138 → 86–93 ms (three interleaved
rounds against the build before; SBCL 66).

**Inline fixnum operations with an operand known** (`inline-fixnum-op`,
`inline-fixnum-cmp`, `prim-arg-codes`, `src/prims.lisp`): an operation of
a fixnum and a value of unknown type takes the fixnum's i32 as it is
(`*fixnum-known-codes*`: its code, the i32 followed by `ref.i31`) and
tests only the other operand; a constant operand is not tested, and `(+ x
k)`, `(- x k)` of a constant test the sum alone, against the range a
fixnum plus K reaches (a character, or a sum beyond the fixnums, falls
outside it: one comparison for both tests). `logand`, `logior`, `logxor` of
two fixnums do not test the result. The test of an operand of unknown type
stays an `i31` test, its extraction and one range comparison (characters
are `i31`s too); with no `i31` below the fixnums a signed comparison could
replace the addition and the unsigned one, which was not tried (one
instruction).

Measured and not done (2026-11-21, interleaved on one slot):

* *Return types of self-recursive functions by fixed point* (its calls of
  itself taken to return nothing, then the union of the body's types,
  widened, until stable; only in the analysis for any arguments, since in a
  copy for some types a call of itself with other arguments may return what
  the copy does not). A count returned by a recursive walk becomes an
  integer of no bound, which makes the variable it is added to an integer
  accumulator (decoding each value returned): `g-traverse`'s `travers`
  156–182 → 181–205 ms. Kept only for results that are no such integers
  (doubles: a recursive sum of doubles over a tree), it measured within the
  noise of the boxing of each value returned, and the numeric functions
  that would gain run in their fixnum copies, where it does not apply.
* *Accumulators fed by data* (`(setq s (+ s (svref v i)))`, `(incf s x)`
  over a list, `s` undeclared, as an integer accumulator with its i64):
  239–251 → 237–244 ms and 111–121 → 116–123 (SBCL 178, 110): the test of
  the eqref local, the decoding of the element and the i64 addition with
  its overflow test cost what the inline fixnum addition and the boxing do.

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

**The base case of a recursive function** is copied into its calls of
itself (`partial-inline-candidate`, `src/inline.lisp`): when the body of a
known global function is an `if` whose test makes no call and whose one
branch is a constant, a variable or an `if` of them (at most 16 nodes each,
`*partial-inline-max-nodes*`), and whose other branch calls the function
itself, each such call in the function becomes `(let ((x a) ...) (if test
small (f x ...)))` (the residual call is not copied again; under `(the
fixnum ...)` at `(safety 0)` it gets the `the`, so that its raw result is
not boxed for the `if`). The calls that end in the base case, most of a
recursion's, then make no call. V8 makes a frame for every Wasm call (the
frame marker and the instance pushed, the parameters spilled, the stack
checked) and has no shrink-wrapping, so in `tak` (`(if (not (< y x)) z
...)`) the frames of the leaves were most of the time: `d-tak` 308 → 77-94
ms (SBCL 200-216 at `(speed 3) (safety 0)`; `bench/declared.lisp` 1.03-1.05
→ 0.96-0.99 in geometric mean), `b-tak` 82-84 → 53-55, `b-fib` 22-23 →
17-18, `b-deriv` 189-196 → 146-152 (`suite` 0.75-0.76 → 0.74); `programs`,
`gabriel`, `vectors`, `arith`, `arrays` the same. Copies into other callers, and small branches that
are primitives (`row-major-aref`'s `%simple-ref`), were tried first: the
one-export library module grew by 16.5 KB and `p-print-object` was 8%
slower; as kept it grows by 0.6 KB. The copies of a function whose own
calls were copied into it after it was found small are limited to three
times the size limit: mutually recursive small functions (`tak` calling a
`tak2` that calls `tak`) copied each other's growing bodies until the
compiler ran out of heap.

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
worker (Start-up and loading). A shaken program starts its runtime from the
heap that the runtime's toplevel forms made when the program was compiled,
in a data segment of the module (Size and start-up).

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
* **The image as it starts from the file.** The file also holds the
  image's own module made for starting from it (`imageVariant` in
  `host/common.mjs`, when the host that saves it has the image's octets,
  as `host/run.mjs` does), which `host/run.mjs` compiles instead of the
  image's file (of which it reads only the end, the `clysm.build`
  section). Its globals whose objects the heap image fills, conses, simple
  vectors and symbols (a symbol's name and hash are mutable fields for
  this, written by its record), are made empty: their initializers are
  those of the custom section `clysm.image-defaults` (`image-defaults` of
  the module, set by `add-image-functions` in `src/driver.lisp`: the
  initializers, then pairs of a global and the index of its initializer).
  V8 decodes and validates every global's initializer when it compiles a
  module (about 10 ns an octet) and evaluates it when it instantiates it
  (about 100 ns a global and 20 ns an instruction): in the REPL image they
  were 1.55 MB, two thirds of it the contents of constant lists, of
  symbols' names and of vectors, and of the objects they made all but the
  first cons of each list were dropped by the heap image anyway (it makes
  them itself). The global section is 0.76 MB there: compiling and
  instantiating the REPL image 43 -> 25 ms. A tool that rewrites the file
  without knowing these octets leaves their place stale: the hosts check
  that they end with the image's build (`imageOctets`), and compile the
  image's file if they do not compile. `linkHeapImage` in `host/link.mjs`
  links the modules of a heap image into one (below) and keeps them, after
  the linked module (cxxxr/lem's `desktop/link-image.mjs` writes the file
  itself and drops them: it should call it). The page host
  (`host/web/clysm.mjs`) compiles them too; its `__clysm_module`, which a
  page gives a worker that starts the image by its toplevel forms, is then
  the image's own octets rather than the module compiled.
* **Compilation hints.** The image starts with code that has never run, and
  V8 compiles each WebAssembly function the first time it is called, which
  for Clysm's large functions costs tens to hundreds of microseconds each.
  A run that starts from the file with every function instrumented
  (`instrumentWasm`: before its body, a function sets its octet in a table
  in the linear memory) learns which functions run; the modules in the file
  and the image then get a `metadata.code.compilation_priority` section
  naming them (`withCompilationHints`), and V8 started with
  `--wasm-compilation-hints` compiles them with the modules, in background
  threads (Electron's V8). Node.js 24's V8 (13.6) reads instead the first
  proposal's `compilationHints` section (a hint octet for each defined
  function; `withEagerHints`) with `--experimental-wasm-compilation-hints`,
  which `host/run.mjs` sets: `bin/clysm`, once it has saved the REPL's heap
  image, runs `lib/image-training.lisp` in the image started from it with
  `CLYSM_TRAIN_IMAGE=1` (the image instrumented as above), and the image's
  module in the file gets the section naming the 1070 functions that ran
  (start, compiling a `defun`, a macro, `loop`, `format`). The hint is
  0x12 (eager; Liftoff as the "top" tier, which dynamic tiering does not
  keep a function that runs long from leaving): an eager hint with the
  default top tier makes `WebAssembly.compile` never settle, and one with
  TurboFan compiles them all with it. The compilation of the module waits
  for them, so the set is kept to what nearly every use runs: one that
  added loading Quicklisp and libraries (1850 functions) made `--eval t`
  9 ms slower and loading no faster.
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
out in the same Wasm instance called that JavaScript through `js:call`, it
gets the original condition back, so `handler-case` sees the condition
itself. Another instance receives a `js:js-error` with the original
JavaScript Error and message: the condition belongs to its originating
heap's packages and classes. This also applies to promise rejections and
to libraries using independently installed copies of the host. Relaying
the JavaScript Error preserves its identity.

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
; wrote dist/outline.{wasm,wasm.debug,mjs,d.ts}: 2 exports
```

```js
import { outline, summarize } from './dist/outline.mjs';
outline(text);                       // a Lisp function, in this thread
await Promise.all(files.map(summarize));
```

`bin/clysm library` compiles the program, loads it once to read its exports,
and writes `NAME.mjs` (an ES module whose named exports are the Lisp
functions; `create(options)` makes another instance), `NAME.d.ts` (the types
given with `:type`) and the host files in `clysm/`. The names and places of
the module's functions (its custom sections `name` and `clysm.sources`, 7 %
of it) are moved to `NAME.wasm.debug`, a module of those sections only: the
loader reads it beside `NAME.wasm` when it first names the frames of a stack
trace (the `debug` option gives its bytes when the module is not loaded from
a file), and a page or a package can leave it out, the frames then being
`wasm-function[N]`. `host/lib.mjs` is the
loader: `load(path, options)` instantiates the module in the calling thread
(Node.js, Electron's main process), runs its toplevel forms, and returns
`{ instance, exports, eval }`; the instance lives on between calls. Options
are `args`, `stdout`/`stderr` (functions of a string), `stdin` (a function
returning the next line), `modules`, `import`, `require`, `fs` and `debug`.
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

#### A package of JavaScript

A library can be what a package of JavaScript exports, with nothing of
JavaScript written for it (`tests/lib/package.lisp`):

```lisp
(js:declare-types "export interface Place { kind: 'line'; line: number }")
(js:export "version" 113 :type "number")              ; a constant
(defparameter *place-error*
  (js:define-class "PlaceError" :extends "Error" :super-arguments '(1)
    :constructor (lambda (this reason message)
                   (declare (ignore message))
                   (js:set this "name" "PlaceError")
                   (js:set this "reason" reason))
    :type "declare class PlaceError extends Error {
  readonly reason: string;
  constructor(reason: string, message: string);
}"))
(js:export "PlaceError" *place-error*)
(js:export "parseLine"
           (lambda (text)
             (or (parse-line text)
                 (js:throw (js:new *place-error* "not-a-line" "not a line"))))
           :type "(text: string) => Place")
(js:export "firstPlace" #'first-place :entry "places" :type "(places: Place[]) => Place")
```

- `JS:EXPORT` exports any value: a function, a constant (converted by
  `JS:TO-JS` once) or a class. `:entry` puts it in a module of its own,
  `ENTRY.mjs` beside `NAME.mjs` (with `ENTRY.d.ts`), for a package's
  subpath (`"./places"` of its `exports`); the instance is the same.
- `JS:LIBRARY-OPTIONS :locations '("./lib.wasm" "../assets/lib.wasm")`
  lets a non-embedded library name the places its Wasm can be shipped, in
  order, relative to the generated loader. The first existing path is
  passed to the normal loader, so `JS:LIBRARY-LOCATION` and module-relative
  imports describe that actual file. If none exists, loading the first
  path reports the normal file error. Lazy libraries search on first use;
  `useLibrary` still overrides the source and loader options. With no
  locations, the existing sibling-Wasm default is unchanged. Eager libraries
  resolve once during import, and `create()` uses that same file.
- `JS:REEXPORT` describes a public entry made from named exports of other
  modules, without adding runtime exports or handwritten JavaScript:
  `(js:reexport '("firstPlace") :from "./places.js" :entry "public")`.
  Names may be one string, a list, or a vector; FROM is the complete module
  specifier, including its extension, and ENTRY is a sibling module name.
  `:type-only t` writes `export type` only in the declaration file, for
  interfaces and types. Values use native ES re-exports, preserving their
  identity and the original module's lazy initialization. The entry may
  also have its own exports and declarations. Duplicate export names and
  invalid entry/export names are rejected. Referenced modules must be
  shipped separately; the generator does not copy them or infer types.
- `JS:DEFINE-CLASS` makes a class of JavaScript whose constructor, methods,
  getters and static functions are Lisp functions (the methods and getters
  are given the object first). It may extend a global class (`"Error"`) or
  one defined before; `:super-arguments` are the positions of the
  constructor's arguments given to the constructor it extends. `new` and
  `instanceof` work from JavaScript and from Lisp (`JS:NEW` of the class).
- `JS:THROW` throws a value of JavaScript out of the call that JavaScript
  made, as it is: an Error of its own class, or `new Error(message)` as
  JavaScript code would have thrown it. In Lisp it is a `JS:JS-ERROR`,
  which a handler can take. What a function of JavaScript throws (or a
  promise rejects with) and no Lisp handler takes leaves the call from
  JavaScript the same way: the caller gets the very value that was thrown
  (`undefined` and `false` too), not a LispError. A Lisp error that no
  handler takes is still a LispError.
  `(js:throw value :message fallback)` supplies a string for the Lisp
  condition's message when the value has no string `message` property.
  An existing string message (including an empty one) takes precedence;
  the value thrown to JavaScript is unchanged. Without `:message` the
  fallback remains the Lisp printed representation of the value. Direct
  calls with `:message` can be shaken without retaining the Lisp printer.
- `clysm library --shake --preinit-through definitions.lisp definitions.lisp exports.lisp -o lib.wasm`
  initializes the runtime and the application's definitions through the named
  source file during compilation. Their heap is embedded in the module;
  later files still run each time the module loads. This lets the shaker
  remove helpers used only to build packages, classes, and initial data.
  This option requires the SBCL compiler and `--shake`. The boundary must
  name one of the input source files. Initialization before it should be
  deterministic and independent of the host: its side effects happen at
  build time and may happen more than once as the shaker computes reachability.
  Put `JS:EXPORT`, `JS:LIBRARY-OPTIONS`, `JS:DECLARE-TYPES`, and other host
  interactions after the boundary. JavaScript objects cannot be saved in
  the heap. Without this option, application initialization remains at
  module load time.
- `:raw t` (of `JS:EXPORT` and `JS:DEFINE-CLASS`) gives a function its
  arguments as JavaScript has them, not converted by `JS:TO-LISP`: values
  of JavaScript whose `JS:TYPEOF` tells `undefined`, `null` and `false`
  apart, and a long string that Lisp reads a part of with `JS:CALL` of its
  methods (`indexOf`, `slice`) without having it copied.
- `JS:GET-RAW`, `JS:CALL-RAW`, `JS:FUNCALL-RAW` and `JS:AWAIT-RAW` give the value as
  JavaScript has it, and a function of `JS:RAW-FUNCTION` given to
  JavaScript (a callback, a listener) is given its arguments so: a text
  handed on, or read a part of (`(js:call-raw text "slice" start end)`),
  stays JavaScript's.
- `JS:DECLARE-TYPES` adds declarations (interfaces, types) to the `.d.ts`
  of the library or of an entry; `:type` of an export or a class is its own
  declaration there.
- `(js:library-options :lazy t)` makes the modules lazy: importing one loads
  nothing. `NAME.library.mjs` loads the module at the first call of an
  export or the first use of a class (`loadSync`, in the calling thread);
  the constants are written into the modules, and the classes are made
  there before the library is loaded, which then gives them their
  functions. `useLibrary(module, options)` of `NAME.library.mjs` gives,
  before that, another module of the same exports (a build with more of
  them, for tests) or options of `load()`. `:output :error` sends what
  Lisp writes to its standard output to the process's standard error (a
  program whose standard output is a protocol). A lazy library's modules
  have no `create` or `default`. A constant is a copy there, not the value
  the loaded library has: an object that Lisp changes later is better a
  function's value.
- `(js:library-options :embed t :compression :gzip)` additionally compresses
  the embedded bytes before base64 encoding. A bundled MIT-licensed fflate
  0.8.3 decoder restores them synchronously, including function/source names.
  No fetch, eval, worker, or asynchronous startup is needed. It works with
  eager and lazy libraries; `useLibrary` still accepts uncompressed Wasm
  bytes or a compiled module. Compression defaults to NIL and requires
  EMBED. Only compressed libraries copy the decoder and its license.
- `(js:library-options :embed t)`: the modules hold the module's bytes (in
  base64) and load it with `host/web/lib.mjs`, which imports no module of
  Node.js: a library for a page, a browser's worker or Electron's renderer,
  and for Node.js as well. Such a program has no files, processes or
  command line; its standard output is `console.log` (a line at a time) and
  its error output `console.error`. It is loaded at once, in the calling
  thread (`new WebAssembly.Module`): Chrome compiles a module of up to 8 MB
  so on a page's main thread, Firefox and Safari any. `useLibrary` takes
  another module as its bytes or a `WebAssembly.Module`.
- A class's declaration (`:type`) names what it extends; a class it
  extends that the library does not export is declared with
  `JS:DECLARE-TYPES`.

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
  13000 forms of Lem's fasls took 1.6 s, and takes 0.4 s. An entry of a
  module (`%fasl-wasm`, `%fasl-unit`, `%fasl-segment`, with its vector of
  constants) is now written in a form of its own that `load` decodes
  without the reader (`%fasl-bin-encode`, `%fasl-bin-decode` in
  `runtime/eval.lisp`: `!N;` and N characters of tagged objects, lengths
  in front of strings, shared objects labelled; any object of another kind
  than symbols, conses, simple vectors and strings, characters, numbers and
  pathnames, or a cycle, leaves the entry to the reader). Its symbols are
  numbered in the file, and found once in it: again in a later entry only
  if the one found is no longer in its home package (the package may have
  changed between entries; a symbol whose home is P is the one P names so).
  Nor are the reader's and printer's variables bound for each entry. Reading
  ASDF's 1106 entries 38 -> 14 ms (in the profiler); `(require :asdf)` and
  loading Quicklisp and Babel 1 to 5 % less CPU, within the noise of the
  machine in time.

ASDF itself, `contrib/asdf-hooks.lisp` and `contrib/fixups.lisp` are compiled
once per image build (`~/.cache/common-lisp/clysm-asdf/NAME-IMAGEID.fasl`,
`%load-compiled-bundled`); the fasls of other images are deleted only when a
month old, since several images (the REPL's, an application's) may be in
use: deleting them made the other image compile ASDF again, ten seconds.
**System fasls** (on by default since 2026-11-22: `asdf::*clysm-system-fasls*`
is `:auto`; `t` for every qualifying system, `nil` or `CLYSM_SYSTEM_FASLS=0`
off, `CLYSM_SYSTEM_FASLS=1` is `t`; `asdf::*clysm-system-fasl-modes*`, an
alist of system names and modes, chooses for single systems). A
system's files can be compiled into one fasl of one module
(`contrib/asdf-hooks.lisp`; `sys::%system-compile-file` and the functions
after it in `runtime/eval.lisp`; "A unit compiled file by file" in
`src/live.lisp`), so that the calls between its files are direct calls that
V8 can inline and that carry the types of their arguments and values, as
within one file. The system fasl is *the product of the compile itself*:
it is the output of the compile-op of each of the system's files, and
ASDF's own compiling and loading of each file in turn (the files before it
loaded when it is compiled, as with a fasl for each file) builds it. The
compile-op of a file compiles it into the system's compilation unit, which
stays open between the files (its state, the bindings of
`call-with-compilation-unit`, kept in a list and bound again with `progv`
around each file: `unit-open`, `call-in-unit`); each file is read and
processed as `compile-file` processes it (its own `*package*`,
`*readtable*` and policy; its compile-time side effects happen once, in the
same state as for a fasl of its own), the module being cut into a segment
at the start of each file. The load-op of the file runs modules made for
its segments alone from the *same compiled functions* as the unit's module
(`unit-running-module`): each defines the functions of its segments and the
globals they are the first to use, and exports them (`u:N:M`); the
functions and globals of earlier files that its code uses (a guarded direct
call into an earlier file, a closure, a constant, a cache) are imported
from the modules that defined them (`host/common.mjs` keeps their exports
while the system is loaded), so they are the same objects as in the final
module. Their objects are written and read as in a fasl (`%su-text`), so a
literal is no more `eq` to the compile-time object than when loaded from a
fasl. When the last file is compiled, the unit's module is finished and
written (`NAME--system.fasl` next to where the fasl of the first file would
go, with the list of its files in `NAME--system.files`, written last): each
function is compiled once. A file is up to date when the system fasl is
(newer than every file and the `.asd`, made from these files): a change to
one compiles them all again. Each file's segments begin with
`(sys::%system-file-start)`, which gives `*package*` and `*readtable*` the
values ASDF's load of a file of its own gives them. In a later image, the
load of the first file loads the system fasl, which runs the files in
order, and the loads of the others only mark them done
(`asdf::*clysm-system-fasls-loaded*` lists the systems loaded so). A call
from one file to a function another file of the system defines is guarded
as the calls between the forms of a batch of `load` are (`(if
(%defun-guard 'f) direct call through the cell)`, the index of a form being
that of its file: `unit-begin-file`), so a redefinition of `f` (at the
REPL, by `trace`, by a later file) is still seen; a call of a function of a
later file goes through its symbol. Only systems whose components are plain
Lisp source files (in modules, with static files) without around-compile
hooks qualify, and not in an image that records its modules for a heap
image (`CLYSM_SAVE_IMAGE`). A segment module is made from what is new
since the last one (`*unit-index*`: the functions after those already in a
module, the constants, closures and other globals pushed since), the
helpers it calls are those of its own functions and globals, and the
locals of a function are coalesced once for both of its encodings
(`wasm-func-coalesced`), so the work of a file does not grow with the
files before it; whether a system fasl is up to date is found once in an
ASDF session (`*clysm-system-fasl-valid*`: ASDF asks for every file, Lem's
1142 times). Objects that cannot be written in a fasl (an instance made by
a macro's expansion, whose `make-load-form` the compiler does not see:
log4cl's `make-logger`) make the segment load from its source forms, as
`compile-file` writes them, and the system is not made into a system fasl.
With `:auto`, a system being developed is compiled file by
file: when a source file of a system is newer than its (complete) system
fasl, `NAME--system.dev` is written next to it and the system fasl
deleted, and from then on its files have fasls of their own, so an edit
compiles that file (and those ASDF finds depending on it), not the whole
system again (lem/core: some 15 s); a change of the `.asd` or of a
dependency does not count. `asdf::clysm-system-fasl-reset` forgets the
notes and the system fasl. A file that fails to compile into the unit (an
error, ASDF's `retry` or `accept`) leaves it: the files after it are
compiled to fasls of their own, and `NAME--system.broken` keeps the system
from being tried again until a file or the `.asd` changes. The methods are
on `source-file` and `parent-component`, not `cl-source-file` and `system`:
a library's method with those specializers (asdf-flv's `perform :around` of
`load-op`) would replace ours. `tests/asdf/twofiles` loads a system from
its system fasl and redefines functions of one file that the other calls;
`tests/asdf/sysfasl` checks that each file's compile-time side effects
happen once per build, that a macro of one file can call a function of the
file before (and makes an object of a class whose `make-load-form` is in
the file before), that the first build loads no file from source forms and
that a changed file makes the system fasl again (with `t`);
`tests/asdf/sysdev` that with `:auto` an edit switches the system to fasls
of each file with the same results, that a second edit compiles that file
alone, and that a reset makes the system fasl again.

A survey of 100 Quicklisp systems (2026-11-22, by another agent, on the
version before the development mode): the compile-time side effects happen
once, the fingerprints of what the systems define are those of fasls of
each file in 98 of them (iterate, trivia and cl-pdf among them), asdf-flv
and fiveam load from system fasls, the test suites give the same results;
log4cl fell back (an object a fasl cannot hold, now noted). On Lem (its
systems through ASDF, lem/core and 30 others as system fasls): first build
19.4–20.1 → 22.3–23.3 s (encoding each function twice, for the modules
that load each file in the first image and for the system fasl, and the
engine compiling those modules), warm load 1.48–1.55 → 1.08–1.14 s, cache
62 → 56 MB.

The first version (2026-11-21) made the system fasl after the system was
loaded, by compiling its files again from source in the loaded image: the
compile-time side effects ran twice, the expansions differed from those of
the files compiled in turn (a macro testing `fboundp` of a function of a
later file, iterate's counter-named clause functions), the first build
compiled everything twice (96 Quicklisp systems: 1.72 times the time) and
the cache held each system twice (1.94).
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
and of the operands after it. A tree of sums, differences and products is
tested once, at its root (`f64-tree-code`: an infinity or a NaN among its
steps makes the result one), and computed again step by step when that
fails, so the condition and the operation that signals it are the same. `(expt float -n)` whose `float^n` is past the floats
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
time unless noted). Measured again on 2026-11-23 (SBCL 2.6.0, Node.js
24.22, one run of each file one after the other, every result the same on
both sides); the column "Before" is the table as it was, measured at
various times up to 2026-11-21, before most of the work of 2026-11-21 and
22 (structure-typed variables, raw clones, the base case copied into
recursive calls, sequence functions looping at the call site).

| File | Programs | Geometric mean | Before | Slowest | Fastest |
|---|---|---|---|---|---|
| `bench/suite.lisp` | 24, used for tuning | 0.76 | 0.94 | `b-deriv` 1.44 (`b-bignum` 1.38, was 1.91) | `b-mandel` 0.08 |
| `bench/gabriel.lisp` | 10 Gabriel benchmarks, used for tuning | 0.96 | 1.01 | `g-traverse` 1.48 | `g-fft` 0.38 (`g-ctak` 0.59) |
| `bench/holdout.lisp` | 17 ordinary programs written after the first two files | 0.78 | 1.06 | `h-fib-generic` 1.46 (`h-clos-methods` 1.02, was 1.68) | `h-mandel-complex` 0.09 |
| `bench/libs.lisp` (`tools/bench-libs.sh`) | cl-ppcre, Alexandria (its test suite and utilities), split-sequence, Babel from Quicklisp; the self-hosted compiler compiling `bench/suite.lisp` | 1.13 | 1.2 | `l-selfhost-compile` 1.51, `l-ppcre-replace` 1.50 (`l-babel` 1.39, was 2.2) | `l-alexandria-tests` 0.29 |
| `bench/programs.lisp` | 63 more programs | 0.84 | 1.19 | `p-builtin-dispatch` 2.61 (`p-bignum-misc` 1.66) | `p-remove-duplicates` 0.03 |
| `bench/arith.lisp` | 13 probes of arithmetic in undeclared code (numbers in slots, lists, vectors, hash tables) | 0.53 | 0.56 | `a-hash-counts` 1.42 (`a-sum-overflow`, bignums beyond 2^63, was 2.33) | `a-simpson` 0.10 |
| `bench/vectors.lisp` | 13 probes of algorithmic code over simple vectors without declarations | 0.51 | 0.43 | `x-fannkuch` 1.09 | `x-list-to-vector` 0.06 |
| `bench/declared.lisp` | 19 programs with full declarations at `(speed 3) (safety 0)` (see below) | 0.94 | 1.24 (1.56 before `safety` was read) | `d-triangle` 1.41, `d-particles` 1.35 | `d-tak` 0.37, `d-matmul` 0.54 |

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
not. In 2026-11-22, with batches that go on past compiled forms and the
combining code of effective methods made in the methods' module (see
"Compiling inside the running image"; script time against the same file
compiled, best of three, interleaved with the build before, one round):
`bench/suite.lisp` 1.11 → 1.06, `programs` 1.15 → 1.07, `clos` 1.09 →
1.06, `control` 1.21 → 1.12, `gabriel` 1.12 → 1.09, `holdout` 1.04 → 1.01
(`t-throw-closure` 2.0 → 1.08: it was not the write barrier but the
thrower, a closure of another module; `r-funcall-return` 2.3 → 1.05,
`c-comb-all` 1.47 → 1.08, `c-short-combination` 1.71 → 1.12,
`p-method-comb` 1.90 → 1.37); compiled, the same files against SBCL
unchanged. The REPL image is 39 KB larger (9.90 → 9.93 MB), the module of
one export 17.5 KB (4.559 → 4.576 MB). Still slow in a script: `p-hash-fn` 2.3
(the guarded copy of `xorshift`), `v-go-closure` 3.7 (`map` of the
runtime calling the closure), `p-equal` and `p-coerce` 1.6, `p-signal-warn`
1.5.

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
  A site of another generic function of one or two arguments compares
  its key (or keys) with the four entries in the same way
  (`%site-key-entry1`/`2`: the cache cast once, the value of the entry
  or null): `h-fib-generic` 0.96 of its time, `tools/bench-lem.sh`
  1.25-1.26 → 1.24, `bench/apps.lisp` 1.29 → 1.27, `bench/clos.lisp`
  the same.
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
* **Tests that are IFs, compiled as branches** (2026-11-22). The test of
  an IF that is itself an IF, an OR or a NOT of one (`cond`, `and`,
  `%endp`'s `(cond ((consp x) nil) ((null x) t) (t error))`, the `(or (%endp
  a) (%endp b))` of `mapc` over two lists) was an i32 made by nested
  `if (result i32)`s and tested again. V8 kept that i32 (a `sete`, a
  `test`, another branch) and, the REF.TEST of the leaf no longer being
  the condition of the branch into the arm, did not know the type of the
  value there: the `(car l)` after `(%endp l)` cast `l` to a cons again
  (its null, i31 and map tests). Now `branch-code` (`src/codegen.lisp`)
  compiles such a test as blocks and BR_IFs: each leaf branches to the arm
  (or to the next leaf), a leaf that is a constant (the NIL of `%endp`, the
  T of OR) makes the test before it branch to its target, and a test falls
  through where it can. The engine then drops the casts after the test
  (in a `mapc` of a lambda over two lists, both lists' casts) and peels
  the first iteration of loops it did not peel before (more allocation
  sites in the code of `b-deriv`; `--no-wasm-loop-peeling` takes 3% of
  the gain of the `mapc` probes back).
  `mapc` of a lambda over two lists 0.70-0.79 of its time, `mapcan` over
  two 0.85; interleaved A/B against the commit before (best of 3-4 runs,
  the order alternated): `bench/library.lisp` 0.955, `bench/closures.lisp`
  0.946, `bench/functional.lisp` 0.965, `bench/control.lisp` 0.98,
  `bench/clos.lisp` 0.99, `bench/suite.lisp` 0.99, `bench/programs.lisp`
  0.99, `bench/gabriel.lisp` and `bench/holdout.lisp` 1.00 (an A/A run of
  the same build is 1.01). `v-mapping` is the same (43 ms either way): its
  `mapc` is a small part, the rest is allocation and collection of
  32-octet conses (half its ticks outside the module's code). Slower at first: `b-deriv` 1.08-1.11, whose `mapcar`
  loops over two to four elements around a recursive call V8 then peeled
  differently (with `--no-wasm-loop-peeling` it was 0.95 of the time
  before; that flag makes `bench/suite.lisp` 1.035 times slower in all, so
  peeling stays; `--wasm-loop-peeling-max-size` changes nothing, and V8
  peels the loops of the earlier code too: its `deriv` was 10.1 KB
  unpeeled and 11.9 peeled, the new one 10.3 and 13.5). So an IF for
  effect (the end test of a `do`, `dolist`, `mapcar` loop) is compiled as
  branches only when its test begins with a compound test itself
  (`effect-branch-test-p`: an OR of `%endp`s, as in `mapc` over two lists),
  and the `(%endp l)` of a loop over one list keeps the IF of an i32:
  `b-deriv` 0.95-0.97 of the time before the change, the `mapc`/`mapcar`
  probes 0.865 (0.92 with every effect IF as branches); interleaved against
  main before it: `bench/gabriel.lisp` 0.96 (`g-dderiv` 0.82),
  `bench/library.lisp` 0.98, `bench/suite.lisp` 1.00, the others 0.99-1.00.
  The REPL image grows by 18 KB (0.2%), the module of one export
  (`tests/lib/one.lisp`) by 5.6 KB. Measured and not kept: a hint that the
  first leaf of such a test branches to its arm (`b-deriv` the same),
  the leaves' hinted BR_IFs as hinted IFs around a BR (the same), and
  the i31 test of the fused integer arithmetic's operands hinted likely
  (no faster: the out-of-line `wide-i64` call still spills the loop's
  registers). Also with it: `coerce` to `double-float`, `single-float`
  or `float` with a constant type is `float` with a prototype (its result
  type known: `(incf x (coerce i 'single-float))` 6 → 1 ns, it was a call
  of `%coerce-to-single` and a boxed single), `fill` of a list is one pass
  (it took the list's length first: 4 → 1 ns an element) and `replace`
  between a simple vector and a list reads or writes the vector with
  `svref` (0.7 of the time).
* **Error helpers are not copied into their callers** (2026-11-22). The
  small functions copied into callers (`src/inline.lisp`) included
  `%type-error`, whose body made the list of ERROR's arguments (four conses)
  inline at every path that signals: every `%endp` of a list loop, every
  checked `the`. A function of `cold-function-p` (`%type-error`, `error`,
  a function that never returns) is now called: the REPL image 9.78 → 9.59
  MB, the module of one export 4480382 → 4404741 octets, the time the same
  (interleaved, five rounds: `bench/gabriel.lisp` 0.99, `bench/suite.lisp`
  1.00, `bench/library.lisp` 0.98).
* **Handler frames** (2026-11-22; `runtime/cond.lisp`, the helper
  `:handler-frame` and `sys::%reuse-closure` in `src/prims.lisp`).
  `handler-bind` and `handler-case` made a cluster (a vector with the
  enclosing cluster, the types and the functions) at each entry and bound
  `*handler-clusters*` to it: a store of a young object into an old symbol,
  which takes the slow path of V8's write barrier (`RecordWrite`) every
  time, on top of the allocations. `v-hb-nosignal` (a `handler-bind` of
  two closures in a loop) took 8 ns a pass against SBCL's 5, most of it
  there. A cluster kept per site and used again while its parent and
  functions stay the same had been measured and not kept (a `handler-bind`
  of global functions 71 -> 68 ms, the one of closures 80 -> 103 with the
  tests): nothing is allocated in the loop, so no scavenge promotes the
  kept cluster, which stays young and takes the barrier at each store all
  the same. What is different now: nothing that is stored at an entry is
  new. The handlers are frames that stay in a vector (see "Non-local
  exits"), the variable holds the depth (a fixnum: no barrier), the types
  are a constant stored only when the frame holds other ones, and the
  closure of a `handler-bind` is the one the frame holds when it is of the
  same function over the same cells and values (`%reuse-closure`: its
  code compares the fields of the old closure with the values the new one
  would get, and makes it only when they differ), stored only when it is
  another. Entering a `handler-case` in a loop: three loads and compares
  and the binding of the depth, no allocation. Per pass, interleaved on
  one slot (ns; SBCL): `handler-case` around an inline form 7.8 -> 3.2
  (5.2), around a call 7.0 -> 3.2 (6.2), `handler-bind` of two closures
  9.2 -> 4.5 (5.1), of one 8.8 -> 4.4 (4.8). `bench/control.lisp`
  `v-hb-nosignal` 78 -> 47 ms (SBCL 47), `v-hc-nosignal` 61 -> 38,
  `v-hb-nested` 76 -> 61, `bench/programs.lisp` `p-handler-case-noerror`
  65 -> 38 (SBCL 47), `p-handler-case` 38 -> 34, `bench/holdout.lisp`
  `h-conditions` 101 -> 82. The frame is found by a function of the
  module (`:handler-frame`: the vector, the frame or the call that grows
  the vector, the store of the types) rather than by that code at each of
  the 85 sites of the runtime: V8 copies it into the hot callers, and the
  REPL image is 15 KB smaller than with the code inline, and faster
  (`handler-case` 3.9 -> 3.2 ns: its casts need no checks).
  Signalling is as before (`t-error-3-calls` 27 -> 24 ms, `v-cerror-warn`
  78 -> 82).
* **Restart frames** (2026-11-22). `restart-case`, `restart-bind` and
  `with-simple-restart` on the model of the handler frames above: the
  depth `*restart-clusters*` and the frames of `*restart-frames*`, nine
  slots each (a `restart-case` of more than six report, interactive and
  test functions gets a wider frame, `%wide-restart-frame`), instead of a
  vector made at each entry and stored into the old symbol. Per entry
  (interleaved on one slot): a `restart-case` of two restarts around a
  call 6.4 -> 3.7 ns, `with-simple-restart` with a format argument that
  changes (its report closure is made each time and stored into the
  frame) 8.6 -> 5.5; `v-restart-nosignal` 66 -> 50 ms (SBCL 70),
  `v-restarts` (restarts invoked) the same. The compiled code of the
  REPL image is 10 KB larger (a `restart-case` site stores each of its
  functions with the comparison).
* **Grown vectors: measured, not kept** (2026-11-22). A vector grown by `vector-push-extend` made with
  `array.new_default`, the old elements copied and only the rest filled
  with NIL (instead of `array.new` filled with NIL, then the copy) was 1.10
  times slower: V8 clears the new array anyway, so it is one more pass.
* **Integer accumulators fed by values of unknown types** (2026-11-22).
  A fixnum is 30 bits here (62 in SBCL), so a sum of a loop often leaves
  the fixnums, and a variable that is no integer accumulator then makes a
  boxed integer at each step. `v-special-misc` (`(incf s (v-sp-param i
  1))` and `(incf s (logand (v-read6) 15))` in one loop) spent a third of
  its time in the generic `+` and the integers it made (329 of 1060 ticks
  in `:add`), although each of its two halves alone was at SBCL's time: a
  variable updated by values of unknown types was no accumulator
  (`note-dual-var` wanted every update in a loop to have an operand known
  to be an integer). One such update is enough now: `v-special-misc` 114
  -> 82 ms (1.36 -> 1.0 of SBCL). `b-catch`, whose four updates of `s`
  include one of unknown type, pays for it as measured before (37 -> 40
  ms; 0.45 -> 0.5 of SBCL); that every accumulator of numbers of unknown
  types becomes one (no update with a known integer) was measured and is
  not kept (`bench/control.lisp` 1.02: `r-mapcar-return` 67 -> 99,
  `r-mapc-go` 57 -> 78; since 2026-11-23 a variable stepped by one value
  of unknown type is one, with a step of its own that does not have those
  costs: "Sums of values of unknown type" below). The cleanup forms of `unwind-protect` are
  compiled for effect (they were compiled for their value, dropped: an
  `incf` of an accumulator there made its integer for nothing).
* **Two fixnum tests, one branch each** (2026-11-22;
  `inline-fixnum-pair-code`). The inline path of `(+ a b)` and the like
  with both operands of unknown type tested them with `ref.test i31` of
  each, `i32.and`, one `br_if`. V8 then makes a flag of each test
  (`test`, `setz`), tests their AND, and tests each flag again where the
  `i31` is taken out (it does not know which test held): eight
  instructions more than `test`/`jnz` twice, which is what a `br_if` on
  each test gives (the type of the operand is known after its own
  branch). Interleaved on one slot: `bench/suite.lisp` 0.980 (`b-matmul`
  0.84, `b-fib`, `b-tak` 0.88), `programs` 0.984, `holdout` 0.984,
  `gabriel` 0.993, `arith` 0.994; the REPL image 9 KB larger.
* **`loop`: the termination tests of the first iteration** (2026-11-22;
  `runtime/loop.lisp`). A `for` clause over a list, a vector or numbers
  tests for the end among the forms at the head of the body and again
  right after its step (so that the clauses after it are not stepped when
  it ends the loop): two tests an iteration. The tests at the head that
  are made again after their steps are now made before the first
  iteration only (nothing is evaluated between the test after the step
  and the head of the next iteration; not with a `for =` clause, whose
  step is among the forms of the head). V8 had merged the two in the
  loops measured (no change in time); the code is smaller.
* **Hints on every branch to the generic path: measured, not kept**
  (2026-11-22). The 42 `br_if`s to the slow paths of the fused integer
  trees and of the accumulators that have no hint, hinted unlikely:
  `bench/suite.lisp` 1.035 (`b-sort` 1.18, `b-struct` 1.11, `b-matmul`
  1.09), `arith` 0.994, the REPL image 38 KB larger. The same for
  `read-char` of a file inline (every branch to the call hinted, the call
  inside the primitive instead of an `or` around it): `i-file-roundtrip`'s
  `read-char` loop the same (2.1 ns a character; SBCL 1.3).
  `i-file-roundtrip` makes the same system calls as SBCL (one `write` per
  file, one `pread` per open for reading; `strace -c`): its time is the
  loops (`read-char` 137 against 73 µs for 56 K characters, `peek-char`
  and `read-char` 213 against 113, the writes 220 against 197).
* **Checked casts in Lem's `char-width`** (2026-11-21; `char-width` and
  what it calls are 40% of the busy time of `l-line-down`). A checked
  cast (`(:ref.cast T)`, which signals a TYPE-ERROR instead of trapping)
  is `block; br_on_cast; call castfn; end`; `br_on_cast` takes no branch
  hint, so V8 keeps the call, with the moves of the live registers to the
  stack and back around it, in the middle of the hot code. The hash
  table helpers (`:ht-get`, `:ht-fast-index`, `:ht-eq-index`,
  `:ht-region-index`...) cast the table's own vectors, which cannot be of
  another type, with such casts, three helpers deep at every `gethash` of
  a character; they are plain casts now. A probe of 600 lines down with
  a redisplay after each (interleaved on one slot, best of 3 of 8):
  127.5 → 120.5 ms, the redisplay probe 148 → 143.5. Measured and not
  kept: every checked cast as `local.tee S; ref.test T; if (ref T)
  [hinted likely] (local.get S; ref.cast T) else (local.get S; call
  castfn)`, which lays the call out of the way: the line-down probe 120.5
  → 115.5 in one round but the same in another (112–117 either way),
  `bench/suite.lisp` 0.82 → 0.80 and `bench/programs.lisp` 0.90 → 0.87
  (one run each), but the code is larger (19 bytes a cast instead of 11)
  and code run once paid for it: `l-alexandria-tests` 78–80 → 94–102 ms,
  `tools/bench-startup.sh` 3–6% slower (`s-ql-babel` 529–554 → 586 ms);
  only inside loops: no better. Branch hints on the i64 and double paths
  of undeclared arithmetic (fused trees: the i31 test likely, overflows
  and the double path's checks unlikely) made `character-at` no smaller
  and the line-down probe no faster (115.5 → 116.5); `char=` of one value
  known to be a character (the character of a simple string) checking only
  the other left Lem, `bench/apps.lisp` and cl-ppcre's scans unchanged
  (`l-syntax-scan` 213–222 ms either way). cl-ppcre's time (`l-syntax-scan`
  1.4, `a-ppcre-tokens` 1.2) is the matchers' closures: V8 inlines the
  tests of a character class into the closure that loops over them; what
  is left in the character searcher's loop is the special variable
  `*string*` read at each character (its value cell, the map and the
  bounds of the string).
* **Special variables read once around a loop: measured, not kept**
  (2026-11-21). A loop (a TAGBODY with a GO back) that calls no function
  but a few of the implementation's (arithmetic, comparisons, `schar`,
  `car`...), binds no special variable and assigns none read the value
  cells of the special variables it reads into locals on its entry, each
  use then testing the local (null: unbound, signalled as before). It
  applied where expected (cl-ppcre's character searcher keeps `*string*`
  and `*end-pos*` in registers) but changed no time: the load of a value
  cell is not what that loop waits for (the string's map and bounds
  checks and the character checks remain), and the other loops reading
  special variables call functions (cl-ppcre's matchers call the next
  matcher, `bench/control.lisp`'s `v-special-*` bind them). Interleaved
  on one slot against the build without it: `a-ppcre-tokens` 43 → 44 ms,
  `l-ppcre-scan` 119 → 118, `l-syntax-scan` and `tools/bench-lem.sh`
  (1.18 → 1.17), `bench/apps.lisp`, `bench/control.lisp` (0.75 → 0.76),
  `bench/suite.lisp` (0.81) and `bench/programs.lisp` (0.88) within the
  noise. Within one function between calls there is little to gain
  either: cl-ppcre's closures read each special variable once per call.
  **Since kept (2026-11-22), with the character check it waited for.**
  `char=` / `char/=` of one value known to be a character (the character
  of `schar` of a simple string, a declared one, a constant) and one of
  unknown type compared codes, the known one's read from the string
  without making a character, the other tested inline (an immediate at
  least the character tag; the helper `:check-char` only signals): it was
  two calls of `:check-char`, one per argument, at every element of
  cl-ppcre's character searcher (`(char= (schar *string* i) chr)`, `chr`
  a variable the closure captured from a parameter). A probe of that
  searcher, 30000 scans of 2000 characters in a script: 80 → 64 ms (SBCL
  43); with `chr` declared a character 47, with `*string*` bound to a
  local around the loop 64 → 46. Then the loop's reads, as above
  (`sref-loop-wrap`, src/codegen.lisp): in a loop that calls no function
  (`:call`, `:funcall`, `:lcall`...), assigns no special variable and binds
  none, the value cells of the program's special variables it reads (not
  those of COMMON-LISP or the implementation, which its primitives may
  set) and the variables of the closure's environment it reads (never
  assigned: not in cells) are read once on entry, each use taking the
  local (an empty cell, an unbound variable, read again by the use, which
  signals): the probe 64 → 58 ms, a probe of three matchers and a greedy
  repetition 100 → 71 ms (SBCL 73). Interleaved, two rounds on one slot:
  `tools/bench-lem.sh` (held-out input) 1.17/1.18 → 1.13/1.15
  (`l-type-redisplay` 1.92/1.96 → 1.73/1.83), a page-down probe 301–306 →
  288–289 ms, the syntax-scan probe 1173–1201 → 1161–1179;
  `bench/apps.lisp`, `tools/bench-libs.sh`, `bench/library.lisp`
  (compiled 0.74–0.75 both, script 1.12 both) unchanged. What is left of
  cl-ppcre's time is not in loops (a profile naming each closure by its
  `create-matcher-aux` method): the character-class tests (as SBCL's), the
  alternation's loop over its matchers calling each, the string and
  look-behind matchers and the scanner, each call reading `*string*` and
  `*end-pos*` with their declared types checked and calling the next
  matcher through `funcall`. The one-export module +162 bytes, the REPL
  image +9.9 KB.
* **A closure's variables in a loop that calls functions** (2026-11-22;
  `sref-loop-symbols`). The variables of the closure's environment that
  are never assigned (not in cells) cannot change, whatever the loop
  calls; yet they were read once on entry only in loops without calls,
  the rule of the special variables. The fields of a closure are mutable
  to the engine (LABELS stores the closures of the other functions into
  them after making them), so V8 reads a field again after every call,
  and with it tests its type again where it is used as a vector:
  `p-queens` (a local function with three vectors and `n` from its
  environment, a loop over the columns that calls itself) tested null,
  fixnum and map of each of the three vectors at every column, fifteen
  instructions each (instruction-level profile: half of the loop's
  ticks). In a loop that calls functions the variables it refers to more
  than once are now read on entry too: `p-queens` 80 -> 68 ms (1.28 ->
  1.08 of SBCL). Those referred to once are not (they would only be
  kept in the frame across the calls; reading every variable on entry
  measured the same, `p-queens` 69 ms with either). The other files, four
  rounds of the three builds interleaved on one slot: `holdout` 80.7 ->
  81.9 (geometric mean of the times, ms), `library` 62.2 -> 62.1,
  `functional` 25.6 -> 24.9, `programs` 52.1 -> 51.4.
* **Immutable fields for a closure's variables: evaluated, not done**
  (2026-11-22). The fields of the `$envcloN` types are mutable for two
  writers: LABELS (the closures of functions that refer to each other are
  made first and stored into each other's fields) and the loader of heap
  images, which makes every object empty and fills it in a second pass
  (`:make-env-closure`, `:closure-env-set`). Immutable fields would let
  V8 keep a variable's value, and what it knows of its type, across the
  calls of a function. What that is worth was measured first with the
  same information given to the engine another way: every variable of
  the environment that a function refers to more than once read into a
  local at its entry (a throwaway change of `setup-free-vars`).
  `bench/closures.lisp` 1.008 of the time, `bench/functional.lisp` 1.047
  (`f-equal-hash` 1.27, `f-cc-interp` 1.08), `bench/gabriel.lisp` 1.012:
  nothing to gain outside loops (reading a field again after a call costs
  what reading the frame does, and the type tests it repeats are few
  outside loops; in loops the rule above has them), and the eager reads
  cost. Against that, the change needs a second family of closure types
  or environments in vectors for the closures LABELS patches, and a loader
  of heap images that makes closures after the objects in their fields
  (in dependency order, the only objects of an image that could not be
  made empty). Not done; the closure types are as they were.
* **`read-char` and `peek-char` of a file in a loop** (2026-11-22;
  `fs-fast-char-code`, `inline-char-payload`). The inline code tested at
  each character that the stream is an instance with enough slots whose
  FAST slot holds a `$fsbuf` (null, fixnum and map of each: seventeen
  instructions of the fifty a `read-char` loop ran per character), the
  values of the loop were loaded from and stored to the frame at every
  iteration (ten moves), and the character was compared with NIL twice
  (the `or` around the inline code, then the program's own test).
  Now: (1) the stream, as an instance, and the `$fsbuf` found the last
  time the code ran in this call are kept in three locals, and when the
  stream is the same and its FAST slot still holds that buffer (the slot
  is cleared by `unread-char` and `close`) the type tests are not made
  again; (2) the call that reads other streams and characters is inside
  the primitive (`sys::%fs-fast-char-or`), its result not compared with
  NIL on the fast path, and the branches to it are hinted unlikely; (3)
  the calls of `:check-char` in an inline character comparison (`char=`
  of a value of unknown type, which only signal) are hinted unlikely too:
  with (2) and (3) V8 keeps the loop's values in registers
  (`--print-wasm-code`: no moves at the loop's head). The hints of (2)
  alone, tried before the others, changed nothing. A `read-char` loop
  over a 56 K file, with the open and the close: 127 -> 107 us (SBCL 58:
  1.8 against 1.0 ns a character), `peek-char` and `read-char` 203 -> 183
  (SBCL 107). What is left is about 35 instructions a character against
  the load, add and store of the position that both must do: the bounds
  test of the octets, null tests of the buffer and of the octets, the
  slot's comparison, the character made and tested again by the program's
  `char=`.
* **The store of the count of values at each return: measured again, not
  changed** (2026-11-22; see "Multiple values" for the measurement of
  2026-11-20). With the code of today (faster elsewhere, so the store's
  share could have grown), the named functions of the benchmark files
  compiled without the store of 1 into `mv-count` at their returns (an
  upper bound of any convention that leaves it out; the runtime as it is),
  five interleaved rounds on one slot: `bench/suite.lisp` 0.998 (`b-call`
  116 -> 113 ms, `b-deriv` 143 -> 143), `bench/gabriel.lisp` 1.008
  (`g-browse` 165 -> 165). Nothing differs from the earlier result: the
  store is one write of a global whose value the engine keeps across no
  call anyway.
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
  The sequence functions of the runtime call the usual two-argument tests
  inline on lists too, as they did on vectors (`%known-tests` in
  `runtime/core.lisp`: `equal`, `eq`, `=`, `char=`, `char-equal`, `string=`,
  `string-equal` of two arguments, which `%with-test` makes of the
  designators): `find`/`position` (also with `:start`/`:end`, and `:key #'car`
  or `#'first`, an alist's keys), `count`, `remove` of a whole list, and
  `member`/`assoc` (which had some of them); `char-equal` of two characters
  compares ASCII letters without calling `char-upcase`. Probes in a script,
  before → after (SBCL): `position` with `:test #'char-equal :key #'car` 266
  → 131 ms (112), `find :test #'char-equal` 209 → 120 (113), `find :test
  #'char=` 143 → 111 (97), `count :test #'string=` 73 → 46 (97), `find
  :test #'string-equal :key #'car` 83 → 56 (40), `remove :test #'equal` 38
  → 33 (26). (`equalp` is left out: its inline expansion is 6 KB a loop; the
  image grew by 47 KB.)
  With system fasls (on by default, see ASDF), the calls between a system's files are
  guarded direct calls in one module. Made by the compile itself (2026-11-22,
  feature off and on alternately on one slot, two rounds each):
  `tools/bench-libs.sh bench/apps.lisp` `a-esrap` 1.89–1.96 → 1.76–1.80
  times SBCL, `a-ppcre-tokens` 1.22 → 1.10–1.16, the others the same but
  `a-yason-parse` 1.13–1.19 → 1.35–1.40. That one is warming up only: run
  five times in a row in the same process, its rounds take 87, 76, 74, 74,
  73 ms with fasls of each file and 111, 77, 73, 73, 74 with system fasls
  (three processes agree), and yason's module is the same function for
  function whether it is made alone or after Lem's 25 systems; it is slow
  only while system fasls of esrap, cl-json, local-time or cl-ppcre are
  loaded too (not of yason itself: with yason's and Alexandria's only,
  91–95 ms), so it is the engine's work on their larger modules when the
  first benchmark starts, not their code. Lem's comparator 1.11–1.14 → 1.05–1.08
  (Lem's systems load through ASDF: `lem/core` is one system fasl);
  `bench/libs.lisp` 1.05 → 1.02. First build (a fresh cache, the system with
  its dependencies, a whole process of about 0.3 s of start-up, one slot):
  alexandria 676 → 706 ms, cl-ppcre 1349 → 1604, babel 1521 → 1634, esrap
  1043 → 1170, cl-json 571 → 625, yason 804 → 906, local-time 578 → 627,
  str 3259 → 4095 (the first version: about 1.7 times); cl-jpeg 33.8 → 33.6
  s (the first version ran its compile-time work twice). What the first
  build adds is making the modules that load each file (about a tenth) and
  writing the system fasl's text; the loads from a warm cache take the same
  time (334–508 ms against 344–508), and the cache is 0.8–1.05 times the
  size of the fasls of each file (the first version: 1.9).
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
* **The reader** (`runtime/read.lisp`) reads tokens, strings and `#\x`
  straight from a simple string between an index and an end: those of a
  string input stream, or the text of a character input file whose octets
  are all ASCII (`%rd-file-text`: decoded once, the first time the reader
  asks, and kept in the stream's `text` slot, so that a character's index
  is its octet's position and `read-char`, `unread-char` and
  `file-position` of the stream agree with it; a file of other octets is
  read character by character as before). A symbol's name is upcased into
  a scratch string and looked up there, in the package's tables and those
  of the packages it uses (`%region-find-symbol`, the hash of a range and
  `ht-region-index`), and the package of `PKG:NAME` in the table of
  packages, so that only a new symbol makes a string; any token of
  constituents that cannot be a number (one beginning with a letter, or
  without a decimal digit and not only dots: `+`, `*x*`, `&rest`, `1+` is
  not) is a symbol read so, a lone dot is the consing dot, a string with
  escapes is copied in runs, the syntax type and the reader macro of an
  ASCII character are read at the call site of the reader's loops
  (`%syntax-fast`; at every call of `%syntax` it made the module of one
  export 3 KB larger), lists are made in order. The
  reading loop reads an ASCII character of a file inline (`%fs-fast-char`).
  Measured against the build before (2026-11-21, interleaved runs on one
  slot): `read` of forms from a 7 KB file 3.3 → 1.4 times SBCL's time,
  `b-read` 1.47 → 1.15, `i-read-stream` 1.55 → 1.05, `i-read-data` 1.48 →
  1.21–1.27, `(a . b)` 2.8 → 1.2, `()` 1.8 → 1.15; `i-write-mix` 1.55 →
  1.09–1.24 (`terpri` and `fresh-line` of a string output stream at the
  call site, `get-output-stream-string` without earlier chunks one copy).
  In `clysm script` the same benchmarks take 0.70–0.85 of their time.
* **Printing and FORMAT**: `output-object` writes a number at once (no
  circularity, no standard pretty printing: `prin1-to-string` of a float,
  `write-to-string` with `:base`), `princ` of a symbol in the standard
  cases writes its name, `~:R` writes the cardinal into a string output
  stream and changes its last word there, and a second scratch string
  stream serves a `format nil` or `princ-to-string` made while the first is
  in use (`~R` made a new stream each time). `i-print-numbers` 1.36–1.45 →
  1.09–1.16, `i-format-english` 1.14 → 1.0, `i-format-justify` 1.43 → 1.3
  (the pretty stream skips the expansion of tabs when none was queued);
  `bench/io.lisp` 0.89 → 0.82. `terpri` and `fresh-line` of a string
  output stream store the newline at the call site when the buffer has
  room (else one call). Measured and not kept (2026-11-22): FORMAT's
  literal characters and strings, `~A` of a simple string and `~D` of a
  fixnum stored into the string stream of `format nil` at the call site
  (no function of the directive): in `clysm script` (where every such
  function is a call through the symbol's function cell) `i-format-table`
  60 → 55 ms and the other `i-format-*` within noise, for 12 KB more in
  the module of one export (the runtime's own messages are FORMAT calls),
  and a literal character as an inline `write-char` (5 KB). A line of ASCII octets is decoded by
  `read-line` in one copying loop (`%utf8-decode`): `read-line` over a 58 KB
  file 3.8 → 1.7 times SBCL's time, which is now mostly the copy of the
  file's octets from the memory into a Lisp vector when it is opened
  (`%mem-get-octets`: four octets a step, 0.30 ns an octet, opening and
  reading a character of a 78 KB file 25 → 24 µs; eight octets from one
  load of 64 bits was slower). SBCL reads its buffer lazily (4 µs there);
  reading the octets lazily here would help only a stream that is not read
  to its end, while every reader of a file stream (the inline
  `%fs-fast-char`, `read-line`, `read-sequence`, `file-length`, `listen`,
  the reader's text) takes the octets up to the fill pointer as the whole
  file: not done.

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
  test the shadow for null (`struct-shadow-init-code`). The body of the
  `let` that binds such variables, or of the function whose parameters
  they are, is compiled in two arms of one test of the objects' types
  (`shadow-version-code`; a body of up to `*shadow-version-max*`, 4000,
  instructions and operands): in the arm where the shadows are the cast
  objects the engine's type analysis drops each accessor's null test and
  the call of the function behind it, and with no call joining after each
  access it keeps what it knows of the loaded fields in registers; the
  other arm is the same code, where a null shadow takes the functions (an
  instance with a slot vector, or an object not of the type, which
  signals). `d-particles` 201 -> 156 ms (1.38 -> 1.08 of SBCL; the null
  tests alone, in one arm, were 5% of it), `d-nbody` 231 -> 203. What is
  left of `d-particles` is its first loop (two sums over the particles: 1.5
  ns an element against 0.7): the type test of each element read from the
  simple vector (null, fixnum, map: seven instructions; Wasm has no
  unchecked cast), the bounds test and the loop's stack check, beside the
  two additions SBCL's loop is. Other declared objects are tested with `ref.test` at each
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
  (`note-dual-var`) a number of unknown type updated in a loop by
  arithmetic, at least once with an operand known to be an integer or
  (2026-11-23) by a step by one value of unknown type (`(incf s x)`,
  `(setq s (- s (f y)))`, `loop`'s `sum`: see "Sums of values of unknown
  type" below), more often than it is looked at there: a counter from a
  value of unknown type, `(loop for k from start ...)`, a sum of the
  values of calls or of a list's elements (see also "Integer
  accumulators fed by values of unknown types" under Performance). Its references as leaves of arithmetic read the i64
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
* **Sums of values of unknown type** (2026-11-23; `acc-step-p`,
  `dual-lset-code` in `src/typed.lisp`, the helpers `:acc-add64` and
  `:acc-add` in `src/prims.lisp`). A sum that leaves the fixnums (2^29:
  of sizes, times, products, squares) was an integer made at each step:
  `(incf n (fourth (coerce-all i)))`, `(incf s v)` of what a closure
  returns, `(dolist (x l) (incf s x))` each called `:add` and allocated 40
  octets once the sum was beyond 2^29 (`p-coerce` spent 15% of its time
  there, `f-generators` twice the time of the same pipeline over fixnums).
  Such a variable (not captured, a number or of unknown type, stepped in a
  loop by `(+ var x)`, `(+ x var)` or `(- var x)` with X one value, and
  looked at there less often than it is updated) is now an integer
  accumulator, as one with an update by a known integer is (the rule of
  "Integer accumulators fed by values of unknown types": one such update
  in the loop is enough, the other updates take the general code), and
  its step is:
  * *inline*, the case of every step of most sums: the accumulator is in
    its i64 (its eqref local null) and within 2^62, X is a fixnum (so the
    sum is within 64 bits: no test of its overflow): four tests, each
    hinted unlikely, and the addition.
    Nothing is called in it, so V8 keeps the loop's values in registers;
  * *else* `:acc-add64` (the accumulator and X decoded whatever holds them,
    a fixnum or a bignum holding its value; the sum as an i64, with no
    integer made; V8 copies it into loops that take it often), and when
    that cannot be (-2^63: a double, a ratio, a sum beyond 64 bits, what is
    not a number) `:acc-add`, the generic sum, whose result stays in the
    eqref local until a step finds an integer there again. A double X goes
    to `:acc-add` at once (a sum of doubles started from 0 takes one call
    a step, as before), and a variable X that may hold an unboxed double
    (the value of a function copied in, `(incf s (dist p q))`) is added in
    f64 to an accumulator that holds a double before it is made an object
    (as the tree of doubles did: `a-points` unchanged).
  What made the same idea slower when it was first tried (above: `b-catch`)
  and again in this work, each found in the machine code:
  * *Three copies of the update* (an attempt for fixnums, the i64 attempt
    with its `:wide-i64` calls, the f64 attempt, the generic call) made
    `sum-until` of `bench/control.lisp` too large for V8 to copy into its
    caller: `r-mapcar-return` 67 -> 100 ms, `r-mapc-go` 55 -> 75, `b-catch`
    35 -> 42. Hence the two helpers: the step is about the size of the
    inline fixnum addition it replaces.
  * *The first step of each call took the helper*: the variable started as
    the fixnum 0 in its eqref local. One bound to a constant starts in its
    i64 (`cg-let`); `r-mapc-go` back to its time.
  * *A step whose value is used* (`mapcar` collecting `(incf s x)`) made
    the integer after the paths had joined, with a test of the eqref local
    V8 could not remove and a spill: the fixnum of the sum is now made in
    the inline path (`r-mapcar-return` 1.36 -> 0.99 of its time before).
    That is still the integer made at each step, with the accumulator's
    tests on top: against the build with the mapping functions of wave 39
    (no header cons) the same item was 62 -> 73 ms. So a step whose value
    is used does not make a variable an accumulator (`dual-uses` follows
    which forms are for effect: the statements of a `tagbody`, the forms of
    a `progn` but the last, down the branches of `if`, the bodies of `let`,
    `multiple-value-bind`, `block`); `r-mapcar-return` 61 -> 60.
  * *A fixnum made for another use was kept in the eqref local*, so the
    next step decoded it: `dual-materialize-code` now keeps there only an
    integer beyond the fixnums (which it would have to allocate again).
  * *An accumulator returned from its loop* (`(return s)` inside the
    `loop` of a generator's consumer) counted as looked at in the loop at
    each step, so it was not one: a variable that is the value of a
    `return-from` out of the loops is looked at once (`dual-uses`).
  Also with it: the reads of an accumulator and of a float quotient in
  the update of another (`(incf s (floor x))`) are hinted to be in their
  i64, and the overflow tests of all fused i64 arithmetic unlikely. V8
  then keeps the values of the loop in registers across the cold calls
  (`:wide-i64`, `:add`, `:divmod`): the loop of `p-float-round` had its
  counter, its sum and the double on the stack, 10.6 ns an iteration, now
  6.2 (SBCL 5.7). Hinting the fixnum test of every fused tree as well
  (not only in the updates of accumulators) was measured again and again
  gains nothing: `suite` 1.00, `programs` 0.99, `arith` 1.01 (`b-special`
  0.79, `a-mixed-poly` 1.10); not kept.
  Rebased on wave 39 (2026-11-23), whose control work had made a sum an
  accumulator when one of its updates in the loop has an operand known to
  be an integer: the two rules are one (`note-dual-var`: an update by a
  known integer or a step for effect by one value, the other updates
  taking the general code). Interleaved against that build (d60362f):
  `suite` 0.98 (`b-catch` 39 -> 33 ms: its step of unknown type is the
  short one now), `control` 0.96 (`v-special-misc` 84 -> 75,
  `v-unwind-protect` 39 -> 31), `programs` 0.96 (`p-float-round` 88 -> 56,
  `p-double-2d` 58 -> 43, `p-ratio-misc` 68 -> 54, `p-coerce` 47 -> 42
  after the library work's own paths), `functional` 0.98, `holdout` 0.97,
  `arith` 0.98.
  Measured (interleaved against the build before, e5669e1, best of four
  on one slot while other work ran on the machine, ms): `p-coerce` 113 ->
  77 (with the changes of "Numeric items" below; the sum alone 104 -> 79),
  `p-float-round` 95 -> 59 (SBCL 61–67), `b-int64` 117 -> 96 for 100 runs
  (SBCL 117), `f-generators` 161 -> 144 for 30 runs (SBCL 78–86: the
  squares are still made, 40 octets each, and `evenp` of one is a call),
  `b-matmul` 106 -> 96 (SBCL 80), `p-vector-push` 156 -> 109,
  `a-int-funcs` 138 -> 115, `v-special-misc` 122 -> 93, `v-unwind-protect`
  41 -> 34, `v-catch-dynamic` 15 -> 9, `r-mapcar-return` 73 -> 75 and
  `b-catch` 38 -> 39 (the two that the first versions made slower).
  Geometric means of the whole of this work (new / before): `suite` 0.97,
  `programs` 0.955, `holdout` 0.97, `arith` 0.96, `arrays` 0.93
  (`a-nbody-typed` 0.66, `a-fp-stack` 0.76: the float trees below),
  `vectors` 0.97, `control` 0.95, `functional` 0.99, `gabriel` 1.00,
  `declared` 1.00 at its `(safety 0)` and 0.98 at `(safety 1)` (`d-nbody`
  0.78, `d-matmul` 0.81, `d-mandelbrot` 0.88); nothing slower beyond the
  noise of a run (`p-print-object` 35 -> 40, `g-div2` 75 -> 81 in one
  round). Against SBCL after it: `suite` 0.69, `programs` 0.76, `holdout`
  0.75, `arith` 0.48, `arrays` 0.62, `vectors` 0.41, `functional` 0.98,
  `declared` 0.93. Loaded into the image (`clysm script`, the two
  images alternately): `suite` 0.97, `programs` 0.97 (`p-float-round` 86
  -> 56 ms, `p-coerce` 150 -> 105, `p-double-2d` 58 -> 44). The REPL image grows by 42 KB (0.5%), the shaken module
  of one export by 3.1 KB (925.6 -> 928.6 KB).
* **A sum, difference or product of doubles is tested once**
  (`f64-tree-code`, `f64-step-rewrite`, `fused-f64-code`). Every float
  operation is tested for an infinity or a NaN (see "Floating-point
  conditions"), and `(incf s (* (aref a i k) (aref b k j)))` tested the
  product (five instructions) and the sum (three with two branches) in a
  loop of twenty. A sum, a difference or a product of an infinity or a NaN
  is one, so the last result of a tree of `+ - *` tells for every step:
  the tree is computed without tests, and when its result is not finite it
  is computed again from the values of its leaves with the test of each
  step, whose generic function signals what the step signals. Only a step
  after which nothing but variables and constants is evaluated is put off
  so (`(+ (* a a) (svref v 5))` signals the overflow before the index
  error, as before; `tests/cases/accumulators.lisp`); a division is never
  inside (`x / infinity` is 0). `incf` and `decf` of an unboxed double
  variable by such a tree, which they expand into a `let` of the delta,
  are one tree with the update when the delta does not assign the variable
  (`f64-step-rewrite`); the trees of doubles of unknown type that the
  fused arithmetic tries (`fused-f64-code`: only `+ - *`, all leaves
  evaluated before) test their root alone. `h-matmul-double` 138 -> 107 ms
  (SBCL 108), `p-double-2d` 59 -> 46 (SBCL 42), `p-double-vector` 104 -> 92
  (SBCL 54; the rest is its two vectors of 1.6 MB, below), `a-nbody-typed`
  358 -> 237; `bench/declared.lisp` unchanged (it has no tests: `(safety
  0)`), 0.98 with its `declaim` of the policy removed. Testing the root of
  a sum by its exponent, as a product is, instead of by the subtraction,
  was no better (the same file 1.01, `h-matmul-double` 1.06): the root
  keeps the test of its own operation.
* **The box of an integer within 64 bits is 24 octets** (2026-11-23;
  `$ibox` in `src/layout.lisp`). An integer between 2^29 and 2^63 was a
  `$bignum` (sign, limbs, value) with null limbs: 40 octets, and limbs
  made in it when the general bignum code asked. It is now a struct of
  its i64 alone, and a `$bignum` is a sign and limbs (32 octets, 40
  before). The two are separate final types, not a type and its
  subtype: with `$bignum` a subtype of the box (the first version) the
  test of "an integer that is not a fixnum" was a `ref.test` of a type
  that is not final, which V8 answers for a bignum by walking its
  supertypes, and the bignum kernels were 3% slower in geometric mean
  (the Fibonacci sums and the products by a fixnum 1.07, `a-sum-overflow`
  1.08–1.2 with the accessors as helpers that V8 did not copy into
  `:big-add`); as two final types every test is one comparison of the
  map, "a box" needs no test of a value that is not 0 after it, and the
  17 probes of bignum operations are 0.996 of their time before
  (`a-sum-overflow` 1.02). The helpers that took the value, the limbs or
  the sign of either through one type (`:box-value`, `:big-limbs`,
  `:big-sign`, `:int-box-p`) stand where the code is not hot; the
  decoding of operands (`num-decode-code`), `eql`, `evenp`, `:wide-i64`,
  `:int-i64`, the sum of two bignums and the rationals test the type they
  want. Limbs for the value of a box are made at each call that wants
  them (`:bignum-limbs`: a box in the general bignum code, a product of
  a box and a bignum); a heap image keeps its form (sign, limbs, value),
  read into either type (`:make-big`). A list of 300 000 squares made
  and summed (`f-sq-list` of the probes): 109 -> 61 ms; `f-generators`
  0.93. Slower with it: `b-ratio` (sums of ratios of bignums), 1.12 times
  its time while `%gcd-big`'s helpers (`:int64-op`, `:int-neg`,
  `:int-limbs`, `:integerp`, `%bignump`) went through the four helpers
  above, 1.07 with the two types tested in them; the rest was not found.
* **`a-esrap` and the accumulators** (2026-11-23, a regression looked
  for and not found). A bisection of wave 39 gave `a-esrap` 92 ms before
  the rule "one update by a known integer is enough" and 112 after it.
  Compiling esrap with its dependencies makes 82 accumulators, none of
  them by that rule and none in esrap's own code (the steps by one value
  are in Quicklisp's tar reader, Alexandria's `mean` and `variance`,
  `count-leaves`, and the `reduce` of the benchmark's own form: 3000
  steps a run); esrap's system fasl differs by 87 octets across that
  commit. The item (two runs of 46 ms) takes one of two times, about 92
  and about 110 ms, on the same build from one batch of runs to the next
  (aafd89a: 111, 108, 92; wave 39: 111, 93, 96; this branch: 109, 91,
  106, best of four to six each), so two builds measured in different
  batches differ by 20% for nothing. With ten runs in the process
  instead of two, five rounds alternately: aafd89a 462–490 ms, wave 39
  447–471, this branch 433–468.
* **A sum stepped by a boxed integer** (2026-11-23). The step of an
  accumulator (above) took its helper when the value was not a fixnum; a
  box is now added inline after the fixnum's tests (`ref.test $ibox`, its
  value, the checked sum): a list of 10 000 integers beyond the fixnums
  summed, `(dolist (x l) (incf s x))`, 3.6 -> 1.3 ns an element (SBCL
  1.05; 73 -> 27 ms for 2000 runs), `loop`'s `sum` the same.
* **A sum of doubles from 0 is an unboxed double** (2026-11-23;
  `*dual-fvars*`, `dual-lset-code`, `:acc-step`). `(let ((s 0)) (dolist
  (x l) (incf s x)) s)` over doubles made a double at each step (as SBCL
  does: 4.5 ns an element in both). An accumulator that may hold a double
  (not one known to hold integers) has an f64 local beside its i64; while
  the f64 holds the value its eqref local is a *mark*, an i31 below the
  fixnums that no Lisp object is (`dual-mark-code`). The step adds a
  double to it inline (the mark's test, `ref.test` of the double, the
  sum, the test that it is finite), a variable's unboxed double (`(incf s
  (dist p q))`) the same; the first double added to the integer 0, a
  fixnum added to the double, and every other case go through one helper,
  `:acc-step`, which returns the accumulator's new eqref (null, the mark
  or the object of the generic sum) and leaves the i64 or the bits of the
  f64 in a global (`:acc-bits`): one call and one result keep the step
  short. Every other use of the variable makes the double an object
  first and keeps it (`dual-b-code`, `dual-materialize-code`: a read, a
  comparison, another update). Per element, ns (SBCL): a list of doubles
  summed 4.1 -> 1.4 (4.5), `loop`'s `sum` 4.6 -> 2.6 (3.6), a simple
  vector 3.4 -> 2.5 (3.6), a structure's slot over a list 4.2 -> 2.6
  (3.6), a sum and a sum of squares 6.8 -> 4.8 (10.6).
  *The size of the step and the engine's limit.* V8 copies a function
  into its callers only when its body is at most 500 octets
  (`--wasm-inlining-max-size`), and the step is now 226 octets longer
  than the inline fixnum addition it replaced (the accumulator's own
  path 96, a boxed integer 59, the double 71): `go-lambda` of
  `bench/control.lisp` went 411 -> 470 -> 541 octets and was no longer
  copied into its caller (`r-mapc-go` 1.23 times its time; the same
  cause as `r-mapcar-return` above). Measured: both fast paths only in
  the helper (410 octets: `r-mapc-go` 0.89, but the engine's copy of the
  helper in the loop spills the loop's values and stores the result
  through the global: a list of boxed integers 1.3 -> 2.5 ns an element,
  a vector of doubles no faster than the generic sum); the node count of
  a function as an estimate of its size (16 nodes are 364 octets in one
  function, 56 are 1407 in another: no use). Kept: the paths inline, and
  the host sets the engine's limit to 640 octets (`host/run.mjs`,
  `CLYSM_WASM_INLINE_MAX`), which gives the functions that the steps
  pushed past 500 their place back, and is faster by itself: with one
  build, 640 against 500, interleaved: `suite` 0.95 (`b-clos` 82 -> 61
  ms, `b-fib` 17 -> 15, `b-tak` 49 -> 45), `control` 0.98 (`r-mapc-go` 73
  -> 57), `gabriel` 0.98 (`g-traverse` 133 -> 117), `programs` 1.00
  (`p-cnm-args` 32 -> 22, `p-specials` 39 -> 28), `library`, `holdout`,
  `functional`, `arith`, `clos`, `closures` 0.995–1.006
  (`c-cnm-function` 23 -> 28, `p-print-object` 31 -> 35 the slower
  ones). 800 was worse for the one item tried (`r-mapc-go` 269 -> 349).
  On real code, the same build with 500 and 640 alternately: Lem on the
  held-out input 0.953 (every item 0.88–0.98, `l-type-redisplay` 41 -> 36
  ms, `l-type-autodoc` 163 -> 154), `bench/apps.lisp` 0.96
  (`a-cl-json-decode` 87 -> 74, `a-esrap` 109 -> 103), `bench/libs.lisp`
  1.01 (`l-babel` 87 -> 92, the others the same).
  A host that cannot set the flag (a browser) copies fewer of those
  functions. Against the commit before (the box and its step), with the
  flag in both: `suite`, `control`, `programs`, `gabriel`, `library`
  1.00, `arith` 0.98 (`a-stats-double` 45 -> 41), `functional` 1.02,
  `holdout` 1.03 (items of under 10 ms); `b-catch` 34 -> 36 (its
  accumulator's reads test the mark; 39–41 before the rebase), and a
  list of boxed integers 1.3 -> 2.2 ns an element (the same inline
  path, placed worse by the engine with the double's beside it; 3.6
  before the box).
* **Numeric items still behind SBCL, and why** (2026-11-23; each profiled
  by instruction: `node --prof --prof-sampling-interval=100 --perf-prof`,
  the ticks placed on the code read from the jitdump with `objdump`, and
  the ticks outside it on the symbols of the `node` binary):
  * **Bignums: no 64-bit digits.** V8 13.6 (Node.js 24.21) has no
    multiplication of 64 bits into 128: the wide-arithmetic proposal's
    `i64.mul_wide_u` (0xFC 0x16) does not validate, and there is no flag
    for it (`--experimental-wasm-wide-arithmetic` is not a flag of this
    V8; `node --v8-options` lists none), so the kernels of 64-bit digits
    could not be measured; they would need four 32-bit products a digit
    pair as now. What the passes cost as they are: an addition is 9
    instructions a 32-bit digit (two loads, two additions, the store and
    the shift, and three bounds checks of two instructions, which V8 does
    not remove or hoist), a product by a fixnum 8, against SBCL's 4 to 6 a
    64-bit digit. Probes of each operation on operands SBCL cannot fold
    (special variables; times SBCL): the Fibonacci sum of
    `p-bignum-misc` (3000 additions up to 2080 bits) 1.37, 300 products by
    a fixnum 1.8, `expt` of 3 to 2000 2.0, products of 7900 by 8400 bits
    3.2, squares 1.8, `floor` of 7900 by 4200 bits 2.8, `isqrt` of 10^400
    4.6, `ash` 2.2, comparisons 1.7, `gcd` 0.5, printing 3^2000 1.1,
    `parse-integer` 1.5. `b-bignum` (the factorial of 2000, 0.34 ms) is
    1.1 to 1.5 over 500 runs and 1.55 over the 50 of the suite, which end
    before the kernels have left Liftoff.
  * **`p-bignum-misc` was mostly arithmetic on constants**, which SBCL
    does when it compiles: `(expt 3 2000)`, `(mod (expt 7 500) 1000007)`,
    `(gcd (expt 6 300) (expt 15 200))`, `(isqrt (expt 10 400))`. The
    folding of integer arithmetic on constants (`constant-integer-form`)
    stopped at 256 bits and at `expt` and `ash` by 256; it now makes
    integers up to 4096 bits (512 octets of a module's data; the length
    of a power is bounded before it is computed) and folds `mod rem gcd
    lcm isqrt logcount` too. What is left of the item is its 3000 bignum
    additions, the printing, and 300 products: 1.59–1.79 -> 1.5 (84 ->
    72 ms).
  * **Integers between 64 and 128 bits** (`a-sum-overflow`, 1.35: a sum
    of fourth powers): each step is a product of i64s made a bignum of
    three digits (`:mul-wide`) and a sum of two bignums (`:add-gen`,
    `:big-add`, `:mag-add`), four objects where SBCL makes two; not
    changed.
  * **Vectors above 128 KB** are large objects for V8, each in pages of
    its own that are mapped when it is made and unmapped when it dies:
    `(make-array 20000 :element-type 'double-float)` takes 27 µs (SBCL
    5.8), one of 200 000 elements 0.27 ms (SBCL 0.09), of 2000 elements
    0.3 µs (SBCL 0.7). Half of the time is in the kernel (`munmap`,
    `mmap`, the page faults of the fill, by `--prof`'s ticks in libc and
    the binary), and no flag of the engine changes it (measured:
    `--minor-ms` 1.0–1.5 times the time, `--no-concurrent-sweeping`,
    `--no-memory-reducer`, `--single-threaded-gc`, `--no-page-promotion`
    1.0, `--semi-space-growth-factor=4`, `--min-semi-space-size=16`,
    `--scavenger-max-new-space-capacity-mb=64` 1.1–1.3). It is what is
    left of `p-double-vector` (two vectors of 1.6 MB a run: 38% of its
    time, its loop of products now at SBCL's whole time), `p-u32-vector`
    (2 MB: its two loops are SBCL's instructions with a bounds check) and
    the sieves.
  * **Ratios** (`p-ratio-misc` 1.33–1.45, now 1.1): 25 ns an operation in
    `:ratio-op`, spread over its decoding, checked products and the two
    gcds; nothing stands out. The gcd of i64s (`:gcd64`, binary) takes the
    count of low zero bits from the difference before its absolute value,
    a step four instructions deep where it was six, and `gcd` of integers
    within 64 bits uses it instead of Euclid's divisions: `gcd` of two
    24-bit integers 47 -> 24 ns, of 40-bit ones 72 -> 36 (SBCL 49 and 72);
    a sum of ratios in a loop 0.85 of its time (1.37 times SBCL),
    `p-ratio-misc` 0.79 (211 -> 166 ms for 90 runs). When one operand is
    8 bits or more longer than the other it takes one remainder first:
    without it `(gcd x 360)` took the binary steps a bit at a time and
    `p-integer-math` was 1.33 times its time before (51 -> 68 ms; 0.97
    with the remainder).
  * **`p-coerce`** (2.1 -> 1.4): after its sum (above), `(coerce "abc"
    'list)` (25%: `%seq-to-list` read each element through the generic
    storage access; the library work of the same wave reads a simple
    string or vector inline),
    `(truncate x)` of a fixnum (the general `:divmod` with a divisor of 1:
    a division with one argument now returns a fixnum as it is,
    `:divmod-one`, `:quot-one`), and the lists and floats it makes.
  * **`p-calc`** (1.2) is `intern` of one-character strings (20%),
    `find-package` of `:keyword` at each call (5%), `sxhash` and the write
    barrier of its token list; not arithmetic.
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
* **Type tests by the kind of the object** (2026-11-21). `typecase`,
  `etypecase` and `typep` of a type of several kinds compute the kind of
  the object once (`sys::%type-code`: inline, an `i31` told apart as a
  fixnum or a character, then `ref.test` of the abstract `array` heap type
  and of the common structures; the helper `$type-code` for the rare
  kinds) and test each clause by a mask of kinds (`sys::%kind-in`: a shift
  and an AND), with the clause's full test only for the kinds where it is
  not decided (`%type-code-masks` in `runtime/types.lisp`: `number` is
  sure of six kinds, `string` sure of simple strings and in doubt for
  array headers, a structure name in doubt for instances, a class name
  not known: the full test). The type inference takes `%kind-in` as a type
  test, so the clause taken knows the object's type as before (an
  `(integer 0 100)` clause's `fixnump` folds after the kind). `typep` uses
  it for types of 3 to 12 kinds among which specialized vectors or
  complex numbers, or of 5 or more (`sequence`, `number`, `real`,
  `vector`, `(and number (not integer))`), not `(or string symbol)`, whose
  two tests cost less; a `typecase` when it has four clauses or more, or a
  clause of such a type (two or three clauses of one test each, `cons`,
  `string`, a structure, cost less than the kind). Against a642c95 on one
  slot: `p-typep-misc` 2.00 → 1.44 times SBCL (190 → 138 ms), `p-typecase`
  1.40 → 1.29, `v-typecase-deftype` 1.76 → 1.71 (its class names are
  tested by their type cells as before); `bench/programs.lisp` 0.93 →
  0.89, the other files and Lem within the noise. A class defined by
  `defclass` or `define-condition` before the test (in the same file or
  program: the macro records the name at compile time, `%defclass`) is of
  the instance and function kinds, so a clause of it costs other objects
  one mask test; its `typep` answers a function from the function's own
  layout (a generic function's) and anything else at once, without
  `class-of` and `%subclassp` (`%typep-cell-expansion`):
  `v-typecase-deftype` 1.71 → 1.41 times SBCL, `v-typep-class` 1.15 →
  0.86. A per-call-site cache of the last layout and its answer
  (invalidated by a counter of class finalizations) was tried and is not
  kept: a site sees many layouts in turn, so the cache missed and was
  written again (`v-typep-class` 155 → 205 ms), while the search of a
  precedence list of three to five classes costs about as much as a hit.
  A `typecase` of two clauses or more of structure names and such classes
  reads an instance's layout and its class precedence list once
  (`%typecase-layout-tests` in `runtime/boot.lisp`), and each of those
  clauses is a comparison with the layout and a search of the list:
  `v-typecase-deftype` 145-147 → 136-137 ms (1.52 → 1.37 times SBCL).
  A cache of runtime
  `typep`'s test per symbol (on the plist) was tried and is not kept:
  `v-typep-runtime` 100 → 106-111 ms (a plist lookup and a closure cost
  more than `find-class`), and an EQ table of compound specifiers costs
  the host's identity hash of a list.
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
  initforms and of constant default initargs (`c-make-instance-plain`
  0.91 of its time; they were called each time), then stores the arguments and the other initforms; the
  effective methods of `initialize-instance` and `shared-initialize` are
  called with the instance and the list of initargs. The `append`, `nconc`,
  `max` and `min` method combinations of two or three methods make no list
  of the values. A primary method of required parameters whose body is a
  literal (`(defmethod kind ((x integer)) :integer)`) is noted
  (`%note-method-constant`); when it alone makes the effective method (no
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
  the closure to the cache and to the method's code. Holding the cache in
  the closure of the generic function's function (its one variable, the
  metaobject in the cache's header, the closure's field set with the
  cache) to save two of those loads changed nothing measurable
  (`bench/clos.lisp` 0.99-1.00 of its time compiled and in a script,
  `h-fib-generic`, `p-builtin-dispatch` the same); the guard of up to four
  EQL specializer objects compared one by one instead of in a loop was
  1.04-1.09 times the time of `c-builtin-eql` and calls of `op`.
  Past 32 entries, a generic function of several specialized arguments
  (whose classes cannot keep its functions) searches the table of its
  entries in its own function (`%gf-table-probe-form`), not in
  `%gf-find2` out of line, and the hash of the table multiplies that of
  the keys (`%hash-spread`; it was h ^ h>>3 ^ h>>6 of each key and
  h*32 ^ h>>3 ^ k to mix them, which put ten consecutive classes and
  the 0 of an unspecialized parameter in three buckets of 32): 49 pairs
  of seven classes (`collide`) 0.70 of their time (SBCL is 0.23 of it);
  a table of one specialized argument the same.
  Reading CLASS-OF's cache of built-in classes as a `load-time-value`
  constant instead of the variable `*builtin-classes*` (the vector is
  filled again, never replaced) was slower where it mattered: in the
  dispatch of a generic function (`%builtin-class-of`) calls of an EQL
  generic function through `funcall` 1.11-1.16 times their time and
  `c-eql-dispatch` 1.18, at call sites `op` 1.10 and `h-fib-generic`
  1.02-1.03 (a first measure of 0.97 was noise); reverted.
* **Dispatch compiled as code** (2026-11-22; `runtime/clos.lisp`, "Dispatch
  compiled as code"). The caches above end in an indirect call of an
  effective method, which the engine does not inline; a generic function
  whose methods the compiler can see is now dispatched by a function of its
  own made from them. The compiler keeps, for the unit it is compiling (a
  whole program, the forms of a batch of `load`, a file given to
  `compile-file`, the files of a system's fasl: `*gf-unit*`,
  `%make-gf-unit`), the methods that `defmethod` defines there
  (`%gf-unit-note-method`) and the classes of `defclass`
  (`%gf-unit-note-class`); in the image it also looks at the generic
  functions, methods and classes defined before the unit (`%ct-live-gf`,
  `%ct-live-methods`, `%ct-live-class`). A call of a generic function of one
  to four required parameters and no others (a symbol outside the
  implementation's packages; not a reader or writer of `defclass`, whose
  sites read and write the slots) is
  `(if (%static-known-p d) (%static-call d arg...) the-call-as-before)`
  (`%gf-unit-site`, from the compiler macro of the call sites of one or two
  arguments and from `convert-compound` for three and four), `d` being a
  symbol that names the *discriminating function* of the generic function in
  this unit. The functions are made when the unit (or a segment of it, a
  file of a system) ends (`gf-unit-flush`, `%gf-unit-flush`,
  `%gf-disc-form`), so they know the methods defined after the call too; the
  test is decided when the code is generated, and only one arm is compiled.
  Three operators of the compiler carry this (`src/frontend.lisp`):
  `(%static-lambda sym (lambda ...))` is the function, known as `sym` to
  `(%static-call sym arg...)`, a direct call, when it closes over nothing
  (the symbols are uninterned and name nothing at run time);
  `(%static-function sym)` is its closure. The expansion of `defmethod`
  gives the function of the method such a name: the function of the
  arguments of a method without `call-next-method`, and for one with it a
  function of the next method (a function of the arguments), the cell and
  the arguments, which the function that `%fast-method-maker` makes calls
  (`+method-static+`, the seventh of a method's extras, holds it).

  The discriminating function is a decision tree over the specialized
  arguments in turn, among the methods still applicable: the objects of
  EQL specializers first (fixnums, characters and symbols written in the
  `defmethod`, compared with `eq`; a generic function with another EQL
  object has no tree), then the built-in classes by the tests of their
  representations (`%ct-kinds`: fixnum, character, null, keyword, symbol,
  cons, simple string, double, single, simple vector, bignum, ratio, other
  strings, complex; kinds with the same subtree share one test, and
  `symbolp`, `integerp`, `floatp`, `stringp` and `listp` stand for their
  groups), then the classes whose instances are told by the identity of
  their class (those of the unit under a specializer, and in the image the
  class named by the specializer and up to 12 of its subclasses), each
  compared with a cell of the function's vector; when every class
  specializer at the position is one whose objects are all of the known
  kinds (`%ct-closed-classes`), any other object takes the methods
  unspecialized there, otherwise it leaves the tree. A leaf is the
  effective method of the standard method combination written out: the
  `:before` methods, the primary method and the `:after` methods called
  directly (`%static-call`; a method defined before the unit through a cell
  holding its function); the next method of a method with
  `call-next-method` (an `:around` method, a primary one) is a function
  calling the rest, made once for the methods it calls and shared by the
  leaves (so the call of the next method in a method's function sees one
  target when the hierarchy is linear). A leaf needing a function that is
  not known (a method evaluated rather than compiled, one declared
  `(safety 3)`, whose `call-next-method` checks its arguments, a last
  primary method that calls `call-next-method`) is left out, as is a tree of
  more than 48 leaves or 40 methods.

  The tree is right for the methods and class precedence lists the compiler
  assumed, and it is used only while the generic function has exactly those
  (`%gf-disc-valid-p`): the standard method combination and class, no
  argument precedence order, each method of the tree a method of the
  generic function with the same qualifiers and specializers and, for those
  of the unit, the very function the tree calls (a method redefined later,
  by another file or at the REPL, has another), no other method; and for
  each key of the tree (an EQL object, a kind, a class) the class
  specializers of the methods at that position that its class has in its
  precedence list are, in order, the ones assumed (a class for which this
  fails has an empty cell: its instances leave the tree). The comparison is
  made at the fourth call that falls out of the tree since the methods or
  classes last changed (`%gf-disc-arm`; a program being loaded changes them
  at every definition); `%bump-clos-epoch`, which every change of a method
  or class calls, empties the cells of the trees compared since and makes
  them compare again. The hot path is one read of the state, the tests, and
  the call. A call that leaves the tree is the call as it was before (the
  cache of the call site for one and two arguments, there in the
  discriminating function, or the generic function); a unit compiled
  before a later unit added a method keeps calling that way (its tree is
  dead: the later unit's has the method). The evaluator has macros for the
  three operators (a form of a unit evaluated when it is compiled).

  Measured (interleaved on one slot, against the commit before; ms):

  | | before | after | SBCL |
  |---|---|---|---|
  | `c-builtin-eql` | 99 | 31 | 39 |
  | `c-eql-dispatch` | 58 | 20 | 29 |
  | `c-megamorphic` (20 classes in turn) | 120 | 57 | 69 |
  | `c-gf3` | 133 | 67 | 101 |
  | `c-gf4` (`:before`, `call-next-method`) | 148 | 96 | 121 |
  | `c-cnm4-args` (three arguments) | 62 | 32 | 49 |
  | `c-comb-all` (`:around`, `:before`, `:after`) | 78 | 39 | 92 |
  | `c-cnm4` | 27 | 26 | 54 |
  | `h-fib-generic` | 310 | 155 | 198 |
  | `p-builtin-dispatch` | 222 | 74 | 88 |
  | `p-clos-app` | 73 | 52 | 50 |
  | `h-clos-methods` | 93 | 61 | |
  | `p-method-comb` | 48 | 39 | |
  | `b-clos` | 105 | 87 | 113 |

  `bench/clos.lisp` 1.07 → 0.74 compiled (in a script the same items take
  0.24–0.59 of their time: `c-builtin-eql` 124 → 30 ms, `c-eql-dispatch`
  82 → 21, `c-gf3` 162 → 72, `c-megamorphic` 148 → 87, the methods of an
  earlier batch being called through cells; the others within 3%),
  `bench/programs.lisp` 0.78 → 0.76, `bench/holdout.lisp` 0.75 → 0.70,
  `bench/suite.lisp` 0.72 → 0.70, Lem's buffer code
  (`tools/bench-lem.sh`) 0.99 and 1.02 before, 1.02 and 1.02 after (two
  interleaved rounds; `l-forms-forward` 156 → 145 ms, `l-type-autodoc`
  188 → 172, the others the same), `bench/apps.lisp` 1.16 and 1.18 before,
  1.22 and 1.18 after (within the runs' spread), start-up the same within
  2% (`s-start` 68, 70 → 69, 73 ms; the others mixed).
  Forty classes at one site in random order (2000 objects): 68 ms for ten
  million calls against 214 with the caches (SBCL 151); ten classes 48
  against 100; four 44 against 39 (the site's own cache of four was
  enough). Sizes: the REPL image 8,876,297 → 8,948,930 bytes (the making
  of the trees); the shaken module of one export 925,558 → 927,705 (174
  bytes of code, in `%bump-clos-epoch`, the rest the names of the new
  functions and variables: it has no tree, and `tests/lib/nodispatch.lisp`
  still no dispatch); the fasls of Lem and its libraries 53.18 → 53.45 MB
  (`lem_core--system.fasl` 16.13 → 16.37).

  Measured on the way, not kept: an index from a perfect hash of the
  class's identity hash (a multiplier found when the tree is compared,
  the class and the leaf number read from a table, then `case` on the
  number) instead of the chain of comparisons, by hand for the 20 classes
  of `c-megamorphic`: 76–85 ms against 52 for the chain (the compiler's
  `case` is itself a chain of comparisons: a `br_table` there was not
  tried, the chain of 40 being at SBCL's time already). Not done:
  generic functions with `&optional`, `&rest` or `&key` parameters
  (`c-gf-lambda-lists` 1.12-1.18: their methods are functions of the
  spread arguments and their calls go through entries by the number of
  arguments), long method combinations, `(setf name)` generic functions, and
  `make-instance` (`b-make-instance` 1.39, `c-make-instance-plain` 1.21:
  the constructor copies a template and runs three plans in loops; with
  the class known in the unit the instance could be made by code that
  stores each slot, compared with the class's plan as the trees are with
  the methods).
  Since (the same day, three commits):
  *The making of the trees is the compiler's*: `%make-gf-unit`,
  `%gf-unit-note-method`, `%gf-unit-site`, `%gf-unit-flush`, `%gf-disc-form`
  and the `%ct-` functions are at the end of `src/frontend.lisp` (in the
  runtime every program carried their names); the runtime's macros call
  them only in a unit (`*gf-unit*` is bound by the compiler alone), and a
  check of a built-in kind carries its sample object. The runtime keeps
  what runs: `%gf-disc-arm`, `%gf-disc-valid-p`, `%gf-disc-regenerate`. The
  shaken module of one export is 808 bytes over main (933,372 → 934,180;
  it was +2,147), the REPL image 8,973,515 bytes.
  *A dead tree is replaced in the image.* A unit compiled before a later
  one added a method to the same generic function kept the earlier way of
  calling for good. Where the compiler is in the image
  (`*native-eval-hook*`), the state of such a function becomes -500 and
  counts the calls up; then `%gf-disc-regenerate` compiles a discriminating
  function from the generic function as it is (`%gf-disc-form` with an
  empty unit: every method through a cell), which the function calls from
  then on (its state is that function; one for a name and epoch, kept in
  the property `%gf-disc-replacement`, at most six for a name;
  `%bump-clos-epoch` starts over). In Lem's scenario
  (`tools/bench-lem.sh`), of the discriminating functions called at least
  four times since the last definition, 39 were live and 6 dead (four
  generic functions: `window-redraw`, `convert-modeline-element`, two of
  the display areas; cl-ppcre's `create-matcher-aux`); now 43 are live, 4
  call a remade one and 2 (`create-matcher-aux`, called fewer than 500
  times) count. The times are the same: Lem 1.00 and 1.01 before, 1.00 and
  1.01 after, `bench/apps.lisp` 1.17 and 1.18 before, 1.17 and 1.17 after
  (two interleaved rounds against main): what Lem's actions and these
  libraries spend in the dispatch of generic functions that are not
  readers is too little to show.
  *Short method combinations.* `defgeneric` notes the method combination
  of the unit's generic function (`%gf-unit-note-combination`; that of a
  generic function defined before is read from it); with `+`, `append`,
  `list`, `max`, `min`, `nconc`, `and`, `or` or `progn` (and no option but
  the order) a leaf is the operator applied to the direct calls of the
  primary methods in that order, one method alone being called without it
  (not `list`), inside the `:around` methods as above;
  `%gf-disc-valid-p` compares the combination, its order and its
  definition. `c-short-combination` 1.14 → 0.97 (38 → 32
  ms); `bench/clos.lisp` 0.72.
  *`make-instance` at the call site* (2026-11-23). A call with a constant
  class and up to six constant keywords makes the instance itself when the
  constructor says that it is a copy of its template with each argument
  stored in one slot (`%ctor-plans`: no initform or default initarg to
  evaluate, only the standard methods; slot 12 of the constructor holds
  `*ctor-generation*` then, slot 13 the slots of the arguments): the
  template is copied and the arguments stored inline, without the call of
  `%ctor-make-instance-N` and its loops over the plans; anything else calls
  it as before. By hand, five million instances of a class of one slot: 55
  ms through the constructor, 23 copying the template inline, 16 with the
  slot vector made in place (which needs the class's slots where the call is
  compiled: not done), a structure 13, SBCL 41. `b-make-instance` 1.39 →
  0.97 (56 → 36 ms), `c-make-instance` 0.86. Unchanged: a class that is
  not a constant (`c-make-instance-plain` 1.16: the search of the site's
  constructors) and a class with a method on `initialize-instance` or
  `shared-initialize` (`p-clos-app`, whose `account` has an `:after`
  method with a keyword: the general path with the list of initargs).
  `tests/cases/clos-dispatch-regenerate.lisp` is the test of the remade
  trees (three files compiled and loaded in turn).
  *Trees for generic functions with `&optional`, `&rest` and `&key`*
  (2026-11-23). A generic function whose methods are functions of at most
  four *spread* arguments (`%fast-shape`: the required ones, the optional
  ones with the unbound marker for those not given, the list of the rest)
  has a discriminating function too, of the spread arguments
  (`%ct-ll-shape`, `%ct-unit-shape`; the descriptor of the cells has the
  shape, and `%gf-disc-valid-p` compares it and the number of optional
  parameters). A call site of any number of arguments the lambda list
  admits passes them spread (`%gf-unit-site`: the markers, `(list ...)` of
  the arguments after the positional ones), so one function serves every
  entry shape; the tree is over the required arguments as before and a leaf
  calls the methods' functions of the spread arguments directly
  (`%spread-method-forms` gives them the names of the unit, and for a
  method with `call-next-method` the function of the next method, the cell
  and the spread arguments, `+method-static+`). The keyword arguments are
  checked in the leaf, only when the call has some: against the keywords of
  the generic function, read from a cell when the tree is compared (0: no
  `&key`, `:any`: `&allow-other-keys`), and those of the leaf's methods,
  which are constants of the leaf (`%gf-disc-check-keys`: one pass with
  `eq`; `%check-more-keys` for the errors and `:allow-other-keys`); the
  comparison of the tree includes the keywords of each method. A call that
  leaves the tree calls the generic function's own function of the spread
  arguments (`+gf-core+`, in a cell), not the generic function with the
  arguments made a list again. Two things found with Lem's and cl-ppcre's
  generic functions: a *function* argument had no kind in the trees
  (`cl-ppcre:scan` of a scanner fell out of its tree 220,000 times in the
  redisplay probe), now `compiled-function` (a function whose name is not
  an instance: a generic function is of its own class); and in the image a
  call with constant keywords of a generic function with `&key` stays the
  call of its keyed main function (`%gf-keyed-entry`: no list is made and
  the methods search none), which the tree's list of keyword arguments
  does not beat (six million calls with two keywords: 66 ms the keyed
  entry, 156 the tree, in a script; a whole program has no keyed entry
  for a generic function, and there the tree is the faster, below). A dead tree
  is replaced by one made in the image only while the generic function
  still takes the arguments the tree's callers pass (`%gf-disc-same-shape-p`:
  a generic function defined again with `&key` in place of `&optional`
  would take the marker for a list). Measured (interleaved with main on
  one slot, best of 4 to 8; ms):

  | | main | now | SBCL |
  |---|---|---|---|
  | `c-gf-lambda-lists` | 140 | 89 | 119 |
  | `(render o)`, `&optional` not given, 6M calls | 58 | 35 | 56 |
  | `(render o 2)` | 62 | 34 | 70 |
  | `(configure o)`, `&key` and a `:before` method | 60 | 31 | 67 |
  | `(configure o :size 3)` | 130 | 84 | 92 |
  | `(configure o :color t :size 2)` | 183 | 125 | 137 |
  | `(gather o 1 2 3)`, `&rest` and `call-next-method` | 97 | 62 | 51 |
  | `(kw o :color t :size 2)`, two methods with `&key` | 162 | 110 | 137 |
  | `(kw o k 2)`, the keyword in a variable | 109 | 73 | 76 |

  In a script the same probes take 0.56–0.72 of their time (those with
  constant keywords are the same code as before), `c-gf-lambda-lists`
  128–133 → 100–108 ms; `bench/clos.lisp` 0.96–0.99 of its time compiled
  and in a script, `programs` 0.99, `suite`, `holdout`, `closures` and
  `functional` 0.97–1.01 (four interleaved rounds each; no item moved by
  more than the spread of two runs of the same pair). What `gather` still
  spends is the three conses of its rest list and `length` of it, a call
  (SBCL counts the arguments of a `&rest` list that is only measured,
  without making it). `tests/cases/clos-dispatch-lambda-lists.lisp`.

  *`make-instance` of a class with `:after` methods* (2026-11-23). A class
  whose applicable methods of `initialize-instance` and `shared-initialize`
  are the standard ones and `:after` methods (the usual way to finish an
  instance) took the general path: the list of initargs, the search of it
  for every slot, the effective methods through their caches. Its
  constructor now has the plans of a plain class and, in slot 14, the
  functions of the `:after` methods in the order they run
  (`%creation-afters`: the method's function of the spread arguments —
  the instance, T for `shared-initialize`, the list of the initargs — or
  the method function itself for a method that has none): the slots are
  filled from the template and the arguments as for a plain class, the
  list of the initargs is made once (those given, then the default
  initargs in order, each evaluated once), and the functions are called
  with it. The keys of the call are checked once, when the constructor is
  computed, against the class's valid initargs, which include the
  keywords of these methods (a key that no slot takes has no slot in the
  plan). With no initform or default initarg to evaluate, the entry of
  the number of arguments does it without a loop over the plans (slot 16;
  `%ctor-make-instance-N/other`, out of the entry so that the entry stays
  small enough for the engine to inline: with the code in the entry
  `c-make-instance-plain` was 7% slower); otherwise
  `%ctor-make-instance-afters`. A `:before`, `:around` or primary method
  of the class, a metaclass, a method on `allocate-instance` or
  `make-instance` leave the constructor's path as before (the constructor
  is computed again at any change of a method: `*ctor-generation*`).
  Measured (four million calls; ms, main → now, SBCL in parentheses,
  interleaved with main on one slot): a class with
  `initialize-instance :after ((a account) &key (bonus 0))` and three
  initargs given 319 → 117 (220); a class of one slot with an empty
  method with `&key` 190 → 69 (78), with `&rest initargs` 198 → 84 (56),
  with `&key (bonus 0)` given 236 → 107 (71: the list of the initargs and
  the method's search of it); two instances of a class with default
  initargs, an initform that conses and an `:after` method 631 → 276
  (337); a subclass with a `shared-initialize :after` method besides 555
  → 182 (225); two classes given at run time 619 → 205 (323). `p-clos-app` 54 → 30 ms (SBCL
  50: 1.19 → 0.60). Made the instance in the call site first (the
  template copied and the methods called from there): the same times for
  these classes, and every site of a plain class 3–7% slower for its
  larger code (`b-make-instance` 37 → 39, `c-make-instance-plain` 28 →
  30): not kept. `tests/cases/make-instance-afters.lisp`.
  Not done, measured by hand before: the slot vector built in place
  instead of the copy of the template (23 → 16 ms for five million
  instances of one slot) needs the class's slots where the call is
  compiled and a comparison of them at run time, as the trees have.

  *The test for a hash table at `gethash`* (2026-11-23; found in the
  profile of Lem's redisplay, whose `char-width` is three `gethash` of a
  character). The helpers of `gethash`, `(setf gethash)` and `remhash`
  tested `$instance` and then cast to the table's own type (`$si17`): two
  walks of the map's supertypes. They test `$si17` (then the class, as
  before). Probes of a table of characters, of fixnums and of symbols and
  `incf` of `gethash` (interleaved with main, two runs of four rounds):
  0.94 and 0.99, 0.87 and 0.87, 0.94 and 0.91, 0.93 and 0.92 of their time; `b-hash` and `p-hash-*` the same within the
  runs' spread (0.99–1.05 in single comparisons).

  *Lem, counted* (2026-11-23; a probe typing the benchmark's text sixty
  times with a redisplay after each key, 2.1 s). Of the discriminating
  functions called at least four times since the last definition 53 use
  their tree, 5 one remade in the image (`window-redraw`,
  `convert-modeline-element`, the two display areas, `variable-value-aux`)
  and 2 count (`create-matcher-aux`, under 500 calls); with the function
  kind above no generic function falls out of its tree more than some
  hundred times in the probe (`variable-value-aux` 506). No `make-instance`
  takes the general path (`%make-instance-of` is not called once in the
  probe). CLOS is not where the redisplay's time is: every function of
  the dispatch, the constructors, `slot-value` and the initialization
  together are 50 ms of 2159 (2.3%; `initialize-point-slot-values` 17,
  `%ctor-make-instance` 7), `print-object` none. By self time:
  `control-char` 20% (the `gethash` of a character inlined in it: its
  ticks are on the loads of the table's fields and the type tests of the
  key, see the test above), `icon-code-p` 6.5%, `char-width` 4.3%,
  `string-width` 2.7%, `separate-objects-by-width` 2.4%, `subseq` 2.2%,
  `split-string-by-character-type` 2.2%, `eastasian-code-p` 2.2%, then
  some sixty functions under 2% each. `tools/bench-lem.sh` against main:
  geometric mean 1.03, 1.07, 1.01, 1.00 on main and 0.95, 1.01, 1.04, 1.06
  now (four interleaved rounds on the held-out input): the same.
  `l-type-redisplay` 39–48 → 38–45 ms (SBCL 28–32), `l-syntax-scan`
  174–181 → 178–187 (147), `l-line-down` 121–134 → 117–127,
  `l-type-autodoc` 173–182 → 164–171, `l-search` 18–21 → 20–24 (not
  explained; SBCL 29). `bench/apps.lisp` 1.14 and 1.11 on main, 1.16 and
  1.15 now (two rounds; `a-esrap` reads 88 or 103–111 ms, the other items within 4%).

  *The ordinary call, looked at again* (2026-11-23; `tools/iprof.py`, the
  ticks of `node --prof` on the code of `--print-wasm-code`). Probes of
  five million calls each (ms, Clysm against SBCL): `funcall` of a
  closure without and with a variable 88 / 154 and 93 / 142, a known
  global function not inlined 118 / 175, a keyword call 97 / 236, `apply`
  of `+` 24 / 24, `floor` of two values 85 / 84, `fib` 15 / 61, a closure
  over a list by a higher-order function 158 / 177. The test for a
  pending exit after a call is one compare with the null in the root
  register and a branch never taken (fused), and is already left out for
  a known function that cannot exit (`lam-may-exit-p`); nothing was found
  to remove there, nor in the prologue (the stack check, as SBCL has
  none; `--no-wasm-stack-checks` was measured before). What `b-call`
  (1.10) has over SBCL is one item: `(rfun 1 2 3)` of `(defun rfun (&rest
  xs) (length xs))`, 368 ms against 133 for five million: three conses
  and the call of `length`, then of `length-of-list`, where SBCL takes
  the number of arguments and makes no list (a `&rest` list used only by
  `length`, `car`, `nth` or `apply` is not made: "rest conversion");
  `(length list)` alone is 190 against 134. The loop of `length-of-list`
  put in `length` itself (no second call for a list) was slower
  (`z-rest-length` 383 → 444, the others the same): not kept. Not done: a
  `&rest` parameter that is only measured or taken apart, kept as the
  arguments.

  *Script-mode short-combination recursion* (2026-11-23, verified again
  after the handoff). `MODE=script tools/bench.sh bench/clos.lisp` on
  `1b02cb3` fails after `c-gf-lambda-lists`. A bisection of recent main
  finds `e07f4a4` as the first bad commit; its parent passes the regression
  program. APPEND/NCONC trees failed their comparison because those
  combinations lack `:identity-with-one-argument`. A dead tree was
  regenerated after 500 calls, and the regenerated tree eventually selected
  itself as its replacement. `04a79e5` accepts the unary identity of APPEND
  and NCONC and marks regenerated descriptors so that a failed replacement
  stays on its fallback. The fixed script completes all 18 benchmarks.
  `tests/cases/clos-dispatch-short-combinations.lisp` exercises repeated
  calls, combinations, and later method changes.

  *Short rest lists kept as elements* (2026-11-23). The inliner can copy
  functions of required parameters and `&rest` when the rest list does not
  escape: at most eight rest arguments and 60 IR nodes (30 for ordinary
  functions). `scalarize-lists` also recognizes local LIST bindings. Each
  element is evaluated once, left to right, into its own variable; list
  cursors become indices. LENGTH, CAR, CDR tests, NTH (including a variable
  index), DOLIST and APPLY at a statically known cursor use those elements.
  Invalid NTH indices retain the ordinary NTH path; a type-error datum is
  reconstructed on the error path, rather than replaced with NIL. Captured,
  special, declared, escaped or destructively modified lists retain their
  representation. `tests/cases/rest-list-elements.lisp`
  covers ordering, aliases, cursor mutation, closures, escaping lists,
  nested loops and variable indices.

  This is call-site scalar replacement, not a new rest-argument ABI:
  indirect calls, large consumers, more than eight arguments, and lists
  passed through generic dispatch still allocate. APPLY can avoid making
  its own list while its callee still builds one. In particular the
  `gather` generic-function probe with CALL-NEXT-METHOD remains future work.
  Two interleaved base/new rounds on slot 2, best of two processes each
  (`04a79e5` as base; milliseconds): `b-call` 119/117 -> 62/66;
  60 million calls of rest LENGTH 396/400 -> 28/27, CAR 160/163 -> 27/26,
  DOLIST sum 346/345 -> 270/266. APPLY forwarding to another rest consumer
  is unchanged (354/337 -> 357/345). In that combined probe, LENGTH of an
  ordinary preexisting list regresses 203/203 -> 236/227. Its Wasm and the
  LENGTH helpers are unchanged apart from ordering and labels; alone it
  does not reproduce the slowdown (234/216 -> 222/225 ms). Keep
  that observed regression with the result, rather than claiming every
  list operation benefits.

  *Keyed constructor after-methods* (2026-11-23). `%creation-afters`
  instantiates each method's existing `+method-keyed-maker+` against the
  constructor's actual keys. When every applicable after-method supports
  that ABI, slot 7 of the after-plan holds the two lists of keyed functions.
  Fixed constructor entries whose slot 16 is current pass the original
  argument values in four slots, with a shared vector for values beyond
  the fourth, rather than making and searching an initarg plist. Absent
  keywords, supplied-p parameters, repeated keys and method ordering retain
  their existing keyed-method semantics. Constructors with default initargs
  or nonconstant initforms, and method sets requiring an actual rest list,
  retain the list path. Method changes invalidate the plan as before.
  `tests/cases/make-instance-keyed-afters.lisp` covers both ABIs, original
  argument values after slot mutation, duplicates, defaults and replacement.

  The loops are in `%ctor-make-instance-N/KEYED-AFTERS`, not in `/OTHER`:
  putting them inline first slowed the default-initarg probe 265-270 ->
  300-305 ms and dynamic classes 211-215 -> 231-233. Separating the helper
  recovered those costs and improved the keyword target further. Two
  interleaved rounds on slot 2, four million instances: the one-slot class
  with a keyword 107/109 -> 69/65 ms (SBCL 80/85); empty &KEY 67/68 ->
  63/62; the &REST fallback 83/82 -> 75/77; the default-initarg probe
  270/288 -> 263/271; plain classes 31/31 -> 31/30. These small changes
  should not be read more precisely than the spread of the runs.

  For both new optimizations together against `04a79e5`, the geometric
  means of Clysm's time per item (new/base, two interleaved rounds) are:
  suite 1.003/1.018, CLOS 0.986/0.993, programs 0.969/0.972. Against SBCL
  they are suite 0.628/0.616, CLOS 0.698/0.692, programs 0.689/0.679.
  These are limited call-site optimizations; the generic-function rest
  list and the generic constructor path are not eliminated.

  *Continuation validation and sizes* (2026-11-23). The final script-mode
  CLOS run completes all 18 items. Full ANSI is 21931/21932, with only
  SUBTYPEP-COMPLEX.8. The compiled, interpreted, native-image and self-hosted
  case suites each pass 120/121: PROCESS fails on both the base and new
  code in this sandbox (a direct Node subprocess control reports
  `spawnSync sh EPERM`). Thus `make test-all` cannot be reported green in
  this environment. Its remaining components were run individually:
  test-lib, ASDF and the self-host compiler build pass, and all case modes
  have only that process failure. No test was disabled.

  Relative to `04a79e5`, the shaken one-export module is 908284 -> 908947
  bytes (+663, below the 970000-byte budget), the REPL image 8631272 ->
  8681448 (+50176, 0.58%). The generic rest probe remains 61/60 -> 60/62 ms
  (SBCL 53): removing its list needs scalar arguments across the guarded
  dispatch and next-method boundary, not just an inline consumer.

  Sizes of the round: the REPL image 8,596,135 → 8,629,564 bytes (the
  making of the spread trees, the constructors' `:after` paths: nine
  entries each with its other part), the shaken module of one export
  907,414 → 908,284. `make test-all` and the ANSI tests (21931 of 21932)
  as before.

  `tools/iprof.py` (new) does the method of the rounds before in one
  command: `run` a module (or the image with a file) under `node --prof
  --print-wasm-code`, `top` for the functions by ticks, `show NAME [MIN]`
  for the code of one with the ticks of each instruction.
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
  a b)` adds inline) and a lambda is inline. So are (2026-11-22,
  `runtime/hof.lisp`) `find-if`, `position-if`, `count-if`, `member-if`,
  `assoc-if`, `remove-if`, `delete-if` and their `-not` forms, `find`,
  `position`, `count`, `assoc`, `remove` and `delete` with `:key`, `:test`
  or `:test-not`, and `reduce` of a lambda or `#'name` (with
  `:initial-value` or `:key`, or of a list), over a list or any vector
  (from its storage; a simple vector is its own, so no call is made), with
  no `:start`, `:end`, `:from-end` or `:count` (those calls stay as they
  are); `remove` of a vector goes out of line, and `remove` of a list shares
  the elements after the last one removed (the list itself when none is).
  A function that is not written in place is called from the loop, where
  the engine can inline it (from the runtime's function it could not, in a
  module loaded from source or a fasl). In the image, `#'name` of a
  function of the runtime that a call does not open-code (`string=`,
  `char-equal`; `*open-coded-function-p-hook*`, the compiler's primitives
  and transforms) keeps the runtime's function, whose loop calls it within
  the runtime's module: in a loop at the call site it was a call into
  another module at each element (`(find s list :test #'string=)` in a
  script 15 → 19 ms). The runtime's own code and the compiler's (packages
  whose names begin with `CLYSM`, `%hof-user-code-p`) keep their calls: the
  file is the last of the runtime, and the expanders are defined only for
  the compiler (`%define-expander-functions`, `%define-image-compiler-macro`:
  a shaken program leaves them and the functions their sources name out),
  so the module of one export is 0.8 KB larger (4.498 MB) and the REPL
  image 43 KB (9.86 MB). The loops over vectors of `map`, `every`, `some`
  and the like take a simple vector without calling `%array-storage` and
  `%array-total-offset` (a call into the runtime's module each time, in a
  script): `v-go-closure` in a script 11 → 2 ms (compiled 3). A probe of
  fifteen such calls (best of three, SBCL / before / after, ms): compiled
  `find-if` of a lambda over a list 40 / 63 / 8, `find` with `:key #'car`
  41 / 65 / 8, `count-if` 42 / 22 / 9, `assoc` with a lambda `:test` 29 /
  24 / 11, `reduce` of a lambda 64 / 53 / 28, `member-if` 53 / 55 / 33,
  `count-if :key #'cdr` 53 / 79 / 16, geometric mean against SBCL 0.88 →
  0.39; in a script against compiled 1.27 → 1.14 (`assoc` 42 → 11,
  `count-if` 38 → 9, `member-if` 79 → 32, `reduce` 77 → 28). The benchmark
  files, new against before (best of three, one round; clysm's time,
  compiled / in a script): `library` 0.944 / 0.991 (`v-assoc-member` 714 →
  506, in a script 688 → 599; `v-find-adjustable` 129 → 78), `programs`
  0.984 / 0.959 (`p-alist-plist` 42 → 36, `p-seq-vector` 58 → 36),
  `control` 1.01 / 0.94-0.98, `suite`, `holdout`, `clos`, `gabriel` within
  noise. `sort` with a key or a predicate stays a call (a merge sort at
  each call site would be large): `(sort list (lambda ...) :key ...)` 1.75
  times SBCL compiled. Other mapping calls stay out of
  line with `#'name` passed as a lambda of known arity (no `&rest` entry for
  `max`): a function holding several inline loops ran slower than with the
  calls (`p-seq-vector` 1.3-1.45), because V8 tiers a function up as a whole
  and inlines call targets by feedback. `concatenate` of two or three
  strings, `string-upcase`/`-downcase` and `parse-integer` are direct;
  `format` compiles `~<...~:>` on an argument to `pprint-logical-block`;
  the reader reads string streams without the accessors' type tests (see
  The reader, below Files);
  `find-package` of a symbol remembers the last one found. `gethash` and
  `(setf gethash)` of an existing entry are one call each of a Wasm helper
  (`ht-get`, `ht-put`; see below) that tests that the object is a hash table, finds
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
  with removed entries compacts in place.
  **Hash tables in Wasm helpers (2026-11-21).** What was left of a
  hash-table operation was the Lisp code around the helpers (each slot of
  the table's slot vector read with a cast, fixnums tested by a call, a
  dispatch on the test and the key's type), so the common paths are now
  whole helpers in `src/prims.lisp`: `(setf gethash)` of a fixnum,
  character or symbol stores or adds the entry (`ht-put`: the add of
  `%ht-add` when there is room; only growing calls Lisp), `remhash` of
  one unlinks it (`ht-rem`); a simple string or a list in an `equal` table
  is hashed, found (stored hashes first, then the characters, or `equal`
  of lists of the same hash) and read or stored (`ht-get-equal`,
  `ht-put-equal`, which also remember the entry for the `(setf gethash)`
  after it), and so is a string of ASCII characters in an `equalp` table;
  an instance in an `eq`/`eql` table (`ht-get-id`, `ht-put-id`) and a
  string or list in one are found by their identity hash. `sxhash` and
  the hashing of `equal` tables are `sxhash-fast` (fixnums, characters,
  symbols, strings, doubles) and `equal-list-hash` (the loop of
  `%list-hash` over a list's elements without a call per element); the
  scan of the entries by `maphash`, `loop`'s `hash-keys` and
  `with-hash-table-iterator` is a loop of its own (`%ht-scan-prim`: the
  `do` cost a block per step). `make-hash-table` makes no vectors: an
  empty table shares empty ones (never written: an entry is only added
  after the room for it is made) and the first entry makes them for the
  size asked for (25 ns in SBCL, 70 before; JSON readers make a table for
  every object); `clrhash` empties the vectors instead of making new ones.
  The helpers that every call site of `gethash`/`(setf gethash)` reaches
  (`ht-get`, `ht-put`, for fixnums, characters and symbols) and the call
  site itself (one type test, the helper, the function) are kept as small
  as before: the other keys go through `%gethash-marked`/`%ht-put-other`,
  which try the other helpers first (a first version with the identity
  hash in `ht-get`'s index helper and three helpers at each call site was
  as fast on these benchmarks, and Lem's `l-string-width`, an `eql` table
  of characters, took 141 and 133 ms in two runs against 135; the small
  call site was kept). `tools/bench-lem.sh`: 1.53 and 1.49 before, 1.51
  and 1.51 after (measure it with the same
  FILE for both builds: by default it reads the tree's own
  `src/prims.lisp`, which this change made 6% longer, and the build
  after then looks 10% slower). Measured (`tools/bench.sh`, against SBCL, before → after, one
  slot): `p-hash-misc` 1.67–1.89 → 1.20–1.29, `k-memoize` 1.79–1.84 →
  1.44–1.53, `v-hash-churn` 1.68–1.80 → 1.21, `v-hash-mixed-keys`
  1.23–1.70 → 0.96–1.28, `v-sxhash` 1.44–1.48 → 1.03–1.07, `b-hash` 1.43
  → 1.20–1.25, `v-hash-equalp` 1.69 → 1.40–1.46, `f-memo-lists` 2.0 →
  1.0, `f-equal-hash` 1.55 → 0.88, `p-word-count` 1.43 → 1.14; files:
  `bench/library.lisp` 0.92 → 0.85, `bench/functional.lisp` 1.16 →
  1.10, `bench/closures.lisp` 1.05 → 1.01, `suite`, `programs`,
  `holdout`, `control`, `arith` unchanged (within ±3%); in the image
  (`MODE=script`) the same. Probes (ns per operation, Clysm before →
  after, SBCL): new fixnum keys in a table made for them 23 → 10 (8.5);
  `setf` then `remhash` of a fixnum 31 → 18 (7.7); `gethash` of a
  two-element list in an `equal` table 38 → 17 (21), of a short string 22
  → 17 (16); a structure in an `eq` table 15.7 → 12 (5), a list 44 → 26
  (5). Still slow: `a-hash-counts` 1.6 (its values pass the fixnums of 31
  bits and are boxed), `v-hash-iterate` 1.8 (`loop`'s `hash-keys` 1.5
  per entry of SBCL's), `k-memoize` (hashing two strings of 50
  characters is half of its time, although `sxhash` of a string is faster
  than SBCL's: 50 characters 18 ns against 35, 1000 characters 306 against
  901; the rest is `&rest` and `apply`), and an `eq` table keyed by
  conses, whose identity hash is the host's `WeakMap` (a new key costs
  about 500 ns against 31 in SBCL: the map's insertion and its ephemeron
  table in each scavenge). `multiple-value-bind` of an `if`, `progn` or
  `let` whose branches end in `values` (or a constant) stores each
  branch's values into the variables' locals (`cg-mv-into`; a branch of
  another kind goes through the values buffer as before):
  `with-hash-table-iterator`'s `(next)` over 3000 entries 8.8 → 6.6 µs
  (SBCL 4). Tried and not kept: as many chains as entries
  instead of twice as many (less to allocate when a table grows) changed
  nothing measurable on `b-hash` and made `b-hash-string` 1.08 times
  slower. An identity hash in every cons (a third field, given from a
  counter by `identity-hash`, no host call): an `eq` table keyed by
  conses fills at 47 ns a key instead of 772 and finds one in 16 ns
  instead of 27, but a cons grows from 32 to 40 bytes and code that makes
  lists pays for it everywhere (`s-build-list` 1.31 times slower,
  `s-ring` 1.45, `a-list-churn` 1.22; `bench/gabriel.lisp` 1.06,
  `bench/programs.lisp` 1.04, `bench/alloc.lisp` 1.02). The host's
  `WeakMap` replaced by a strong `Map` (an upper bound for any table on
  the host's side, such as one per hash table): new keys 695 → 256 ns,
  lookups unchanged, and the self-hosted compiler compiling
  `bench/suite.lisp` (whose type inference keys an `eq` table by
  conses; `identity_hash` is 210 ms of its 5.5 s) unchanged within the
  noise (with the `Map` 5.4–5.8 s, with the `WeakMap` 5.5–6.1). Eight lanes in the string hash
  from 32 characters on: no faster (the four lanes are bound by the
  loads; 1000 characters 306 → 311 ns).
  **A hash table is a structure of field slots (2026-11-22).** It was an
  instance whose seventeen slots (`+ht-keys+`... in runtime/hash.lisp)
  were in a slot vector, so that a lookup went instance → slot vector →
  key, bucket and next vectors, with a bounds check at each slot read.
  It is now a `$si17` (src/layout.lisp), its slots in the instance's own
  fields: `%make-ht` makes it, `%ht-ref` / `%ht-set!` read and set a slot
  of a constant index with one `struct.get` / `struct.set`, and the
  helpers (`ht-get`, `ht-put`, `ht-eq-index`...) take the table itself
  where they took its slot vector. Heap images and the runtime's heap of
  a shaken module save it as any structure of field slots (`:sinst`);
  `ph-instantiable-classes` (src/preinit.lisp) reads the fields of
  `*specializer-methods*` there instead of its slot vector (otherwise the
  methods of the classes without instances are kept: the one-export
  module grew by 137 KB until it did). One chain of dependent loads
  shorter per lookup; the slots' values are still `eqref` (a cast of the
  key vector, a test of the fixnum shift at each read), which typed
  fields would take away but which a heap image would then have to know
  as a kind of its own. Interleaved, two rounds on one slot (before →
  after): `bench/apps.lisp` 1.23 → 1.205, `tools/bench-libs.sh` 1.085 →
  1.065, `v-hash-churn` 0.98 → 0.89, `v-hash-iterate` 1.34 → 1.25,
  `p-hash-misc` 1.19 → 1.15, `v-hash-equalp` 1.32 → 1.28; `suite`,
  `library`, `programs`, `functional` and Lem's actions (`l-line-down`,
  `l-page-down`, the redisplay probe: medians within 1–2%) unchanged; a
  loop of 24 million lookups of characters and fixnums 120 → 117 ms
  (SBCL 260). The one-export module 2,011,874 → 2,011,438 bytes, the REPL
  image +1.7 KB. Lem's `char-width` chain is not faster: what it waits
  for there is the misses on the tables' vectors after the redisplay's
  other work, which the slot vector was only one of.
  The pretty printer prints a
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
* **Rarely taken paths in loops, probed** (2026-11-21, `build`-time probes
  of one loop over 300 000 octets with one cold branch, SBCL in brackets):
  no cold branch 38 ms [33]; a branch that calls an unknown function and
  goes on 54 [40]; one that calls `warn` with three arguments 79 [52]; an
  undeclared accumulator (generic `+`, its fallback a call) 62 [117]; a
  loop of `(svref v i)` declared fixnum 46 against 27 at `(safety 0)` [25,
  27]. The fallbacks Clysm generates itself (the overflow of fixnum
  arithmetic, a subscript out of bounds, a failed type test) are already
  out of the way: a hinted `br_if` to a block that calls the helper, or a
  branch ending in `unreachable`, which V8 makes deferred blocks; hinting
  the type tests (`%type-error`) as well changed nothing (46 → 46-49), nor
  did testing a fixnum inline instead of calling `$fixnump` (V8 inlines
  it); the cost of the declared fixnum is the test itself (`ref.test`,
  `i31.get_s`, the range). What was not out of the way is user code: an IF
  whose arm calls `error`, `cerror`, `warn`, `signal`, `break` or
  `invoke-debugger` (or a function of the file found never to return) on
  every way through it is now hinted the other way (the branch hints'
  `cold-branch-p`, which also takes `warn`, `signal`, `break`,
  `invoke-debugger`, a function of the file found never to return, and a
  cold part anywhere in a node other than an IF, memoized in
  `*cold-cache*`; an IF counts only when its test or both its arms do, so
  that a COND whose last clause signals does not mark each earlier clause
  likely, which made `b-deriv` 1.56 → 1.74), and V8 makes that arm a
  deferred block, spilling there only: the `warn` probe 79 → 32-36 ms
  [52]. An unknown call in a branch (54) is not known to be cold and keeps
  its cost. `bench/suite`, `programs`, `gabriel`, `library`, `arith`,
  `declared`, `io` (compiled; `library`, `io` in a script), apps and libs
  within noise over two or three rounds each (the machine was loaded:
  `b-deriv` 1.52-1.56 → 1.48-1.56, apps 1.31 → 1.26-1.31, libs 1.08-1.14 →
  1.10-1.14); Lem's comparator 1.51-1.54 → 1.47-1.48.
* **split-sequence and esrap at the instruction level** (2026-11-22;
  `node --prof --prof-sampling-interval=100 --perf-prof`, each tick's pc
  placed in the TurboFan code object that held it and that code read from
  the jitdump with `objdump`, against `sb-sprof` and SBCL's `disassemble`).
  split-sequence's time was the runtime's `position` with `:start`: the
  loop of `%eql-scan` (the plain EQL search of `position`/`find`/`count`)
  started its index from the undeclared `start`, so it was a boxed integer
  with generic `+` and `>=` per element; it is a declared fixnum now (after
  `%check-bounds`), and a character searched in a string is compared by its
  code with the element as it is stored (`=` of `char-code` of `%schar`,
  which `schar-code-node-code` reads as the i32 element; `eq`, `eql` and
  `char=` of a known character with such a read compare codes too,
  `char-codes-eq-code`), not a character made of each element: a probe of
  `(position #\b s :start i :key #'identity :test #'eql)` 300 → 200 ms
  (SBCL 180, which scans with AVX2). The checks of FTYPEs and declarations
  that the type of the value decides (`symbolp`, `consp`, `listp`, `null`,
  `characterp`, `functionp`, `double-float-p`, `fixnump`: three
  `ref.test :symbol` of the same argument at the entry of esrap's `(setf
  cached)`, one of the FTYPE, one of its `the`, one of the declaration) are
  left out (`known-test-result`, whose case for `characterp`, `consp`,
  `symbolp` and `numberp` now takes the others), an integer of unknown size is unboxed
  with its fixnum case inline (it was a call of `$int-i64`, 3% of the
  profile of split-sequence), and a structure predicate's case for an
  object of no `$siK` type (a structure with a slot vector including it,
  or no structure) is one call of `$struct-typep` instead of a copy of the
  search at each test: the image 9.896 → 9.847 MB. `tools/bench-libs.sh`,
  the build before and after on one slot: `l-split-sequence` 1.51 → 1.22
  times SBCL (103 → 83 ms; its newline split 18 → 6.5 ms, the space split
  44 → 33), the file 1.11 → 1.07. `bench/apps.lisp` unchanged (1.30-1.31
  both, `a-esrap` 1.98 both); Lem's comparator on a held-out file 1.25-1.26
  → 1.20-1.22 (`l-line-down` 1.46-1.53 → 1.33-1.39); `bench/suite.lisp`
  0.78-0.80 → 0.79-0.82 and `library.lisp` 0.82-0.83 both compiled, 1.10 → 1.05 and 1.13
  both in a script (two or three interleaved runs; within the noise).
  What esrap's time is (`a-esrap` 1.9-2.0): no function stands out, as
  found before, and per instruction no construct does either. Of the ticks
  in its TurboFan code, 14.5% are in the first instructions of functions
  (8.7% on the `push rbp` where a call lands: the indirect call's
  misprediction, which costs the load chain that computes the target) and
  12.6% in that chain before each `call_ref` (the symbol's function cell,
  the closure's `$code`, the entry, then V8's own `WasmInternalFunction`
  and its code pointer: the hottest loads of RULE/TRANSFORM, `(setf
  cached)` and COMPILED-SEQUENCE, 30-70 ticks each, cache misses on V8's
  trusted objects); 5.8% the out-of-line part of V8's subtype test (an
  instance of a structure including the one tested, `successful-parse`
  tested as `result` or `error-result`: map, its type info, the length of
  the supertypes and the entry, three dependent loads, against SBCL's one
  compare of the layout id); 2.5% the stores of `mv-count`, 2.7% null
  tests. SBCL's `result-position` is 10 instructions on its hot path and
  does not test its argument at all (its FTYPE is trusted); Clysm's tests
  the `$si3` type and the layout. Probes of the operations esrap does, a
  loop over 1000 mixed `failed-parse`/`successful-parse` compiled to fasls
  (20 million each, SBCL in brackets): `error-result-p` 38-60 ms [13], the
  inherited accessor inline 47 [22], through a call of another file's
  function 98 [48-51], the same call within one module 56 (V8 inlines it),
  allocation of the structure 61 [91], a lazy production closure made and
  called 272 [244]. So what is left is the calls between modules (the
  chain and no inlining, measured before at 17% of esrap) and the subtype
  test of included structures. Not done: making a parent's and a
  child's instances the same Wasm type (size classes of 4, 8, 16, 32
  fields, so V8's exact-map case hits) would cost the padding of every
  small structure and does not fit the typed structures (`TS-...` types
  extend `$siK` field by field); estimated gain on esrap under 6%.
* **The size of a function's code, and what the engine inlines**
  (2026-11-22; real libraries as system fasls: esrap, cl-ppcre, Lem).
  V8 decides what to inline by the size in octets of the callee's code as
  the module has it (`src/wasm/inlining-tree.h`, `--trace-wasm-inlining`):
  no function of more than 500 octets (`--wasm-inlining-max-size`), one of
  N octets only at a call counted N/2 times when its caller is optimized,
  and a caller grows to five times its size at most, 7000 octets at most (a
  larger one by a tenth). The code that never runs counts like the rest,
  and the compiler left much of it: esrap's `result-position` (a slot
  read, a `functionp` test, a call and a slot write) was 565 octets and
  `successful-parse-production` 656, neither ever inlined, of which a
  quarter was the code after field reads that cannot fail (the call of
  the accessor's function, with the class found for it). With `--wasm-inlining-max-size=1000`
  esrap ran 9% faster and cl-ppcre's tokens up to 15%, which showed what
  the size cost. What is done about it:
  * `prune-dead-code` (`src/wasm.lisp`, on every function added to a
    module) leaves out what follows a branch, a return, a tail call or
    `unreachable`, and what follows a block that cannot be left at its
    end; puts the instructions of a block that nothing branches to in its
    place; decides `(i32.const C)` before `i32.eqz`, `br_if` and an `if`
    without a label (the tests the types decided: `(if (i32.const 1) ...)`
    three deep where a declared type was already known); and makes
    `local.set X local.get
    X` a `local.tee` or nothing. A whole program is 4.4% smaller by it
    alone (`bench/closures.lisp` with the runtime: 5,422,808 → 5,186,229
    octets).
    (The validation takes the code after a block for reachable whatever
    the block ends with, so an `unreachable` takes the place of what is
    cut there; and a block whose last instruction is its only branch to
    itself cannot lose that branch, since a branch drops what is on the
    stack below its operands and the end of a block does not.
    `CLYSM_EAGER_VALIDATION=1` makes the host validate every function of
    every module when it is made, which is how such code is found: the
    engine otherwise validates a function when it first runs.)
  * The class of a structure in an accessor, a predicate or a constructor
    (`(load-time-value (find-class 'name))`, about 47 octets at each:
    the call of `find-class` through its symbol with the test for a
    pending exit) is found through the helper that calls a symbol's
    function (about 23).
  * The other arm of a guarded direct call (the function was redefined)
    was `funcall` of the constant symbol as of any object, 60 octets and
    more: it passes the arguments and the symbol to `(:cellcall k)` (see
    Calling convention), as do the calls of `error`, `warn`, `signal` and
    of the functions that never return; `sys::%type-error` is no longer
    copied into its callers by the code compiled in the image (`(error
    'type-error :datum x
    :expected-type 'list)` with its five arguments, 100 octets at the end
    test of each `dolist`: `cold-function-p` in `live-inline-call`, as the
    whole-program inliner had it).
  * `funcall` of an object of unknown type tests for a closure in place
    and leaves a symbol to the helper (14 octets instead of 37).
  * `#'f` of a function of another file of the system was `(fdefinition
    'f)` in place, a call of `sys::%function-name-symbol` (1.2-1.9% of
    esrap's profile by itself) and `symbol-function` with its tests for a
    macro, some 300 octets: it reads the function cell (`function-cell-node`).
  * The compiler knows the structures a structure includes (the last
    element of its description, `struct-class-includes-p` in
    `src/types.lisp`): an object found to be of a structure by an
    accessor, a predicate or a constructor stays of it when an accessor
    or a predicate of an included structure sees it (`ty-intersect` took
    the class of the last one, so `(successful-parse-%production r)`
    then `(result-detail r)` then `(setf (successful-parse-%production
    r) ...)` tested the class three times, with three classes found), its
    accessors read the field through the Wasm type of the object's own
    structure (the engine's test of an object's exact type is one
    comparison, of a supertype three loads more) with no test of the
    class, and its predicates are decided (`known-test-result`).
  * The inline `aref` and `char` of an array of unknown type read a simple
    vector or string in place and, for the others, took the storage of a
    one-dimensional array header (a fill pointer, adjustable, displaced)
    in place too, 40 octets at each access: the helper `(:hdr-locate
    KIND)` gives the storage and the index there, or null for the general
    function (`speculative-locate-code-1`; `tests/cases/aref-headers.lisp`).
    Against the build before: `bench/arrays.lisp` 0.96 and 0.99 of the
    time, `holdout` 0.97 and 1.00, `p-vector-push` (AREF of a vector with
    a fill pointer in a loop) 141-153 ms for both.
  * A declared integer range within the fixnums is one unsigned
    comparison of the distance from its lower bound (`decl-test-code`),
    8 octets less at each declared parameter.
  * `funcall` in a function of at most 40 nodes keeps the symbol's case in
    place (`*small-caller-nodes*`). V8 lets a function grow by inlining to
    five times its own size, so the shorter code took budget from short
    higher-order functions: `my-map` of `bench/closures.lisp` went from
    175 to 150 octets, its budget from 875 to 750, and the inlining of the
    functions it calls through `funcall` stopped two octets short (750
    against 752, `--trace-wasm-inlining`): `k-higher-order` 137 → 150 ms,
    `k-church` 26 → 29, `f-pipeline` 43 → 50. With the rule they are at
    137-149, 27-28 and 44-51 again (and `k-generators`, which the shorter
    code had taken from 100 to 84, at 104: the same budget the other
    way). With these three, against the build before them: `closures`
    1.015, `functional` 0.96, `programs` 0.97, `apps` 1.00, Lem 0.995;
    and all of it against the build before any: `apps` 0.954 (`a-esrap`
    98 → 86 ms, 1.56 times SBCL; `a-ppcre-tokens` 46 → 40, 1.05), `libs`
    0.987.
  What is still not inlined in esrap, and why (the trace after the
  changes): its small callers have small budgets, five times their own
  size (a lambda of 48 octets calls `successful-parse-production`, 270
  octets, 54 000 times in the run: 318 is over 240; the stub of a `&rest`
  function, 14 octets, cannot take the 521 of `text`); `cached` and
  `(setf cached)` are 856 and 2232 octets with their three
  representations of a cell (SBCL does not inline them either), each
  called through its entry that unboxes the position; `rule/transform`
  is 4502. In cl-ppcre four calls are refused, of lambdas of 501 to 1567
  octets, a few thousand calls each: its hot calls are those of the
  matchers through `funcall`, which the engine inlines where one site
  sees one closure.
  esrap's functions, before and after, in octets: `result-position` 565
  → 321, `successful-parse-production` 656 → 270, `find-chunk` 516 →
  317, `result-unsatisfied-predicate-p` 799 → 438, the lambda of
  `compile-sequence` 1688 → 967, `rule/transform` 7853 → 4502; the code
  of its module 547 → 476 KB. The REPL image 8,875,824 → 8,270,547
  octets (its heap image 11.06 → 10.46 MB), the one-export module 925,558
  → 887,235, the self-hosted compiler 9,171,099 → 8,495,628.
  Interleaved with the build before on one slot (the best of two rounds
  of three runs each): `tools/bench-libs.sh bench/apps.lisp` `a-esrap` 99
  → 90 ms (1.77 → 1.61 times SBCL), `a-ppcre-tokens` 46 → 41 (1.18 →
  1.05), the others within 3% (`a-yason-parse` 113 → 120 there, 98 for
  both when it runs alone eight times: the order of warming up); esrap
  loaded alone 51 → 44 ms (SBCL 30); cl-ppcre alone, its tokens 23.0 or
  26.5 (it has two speeds) → 19.5-21.0 ms, a scan, a replacement and an
  alternation the same; `tools/bench-libs.sh` `l-selfhost-compile` 4038
  → 3781 ms (1.60 → 1.49 times SBCL: the compiler itself is 7% smaller),
  `l-alexandria-tests` 74 → 67, `l-ppcre-replace` 178 → 176 and
  `l-ppcre-scan` 118 → 119 (cl-ppcre's time is in its matchers' calls of
  each other, below). Lem's comparator on the held-out file, four
  rounds: 0.92, 0.93, 1.00 and 1.04 of the time before
  (`l-forms-forward` 144-156 → 134-144 and `l-type-autodoc` 171-195 →
  161-175 in every round; `l-type-redisplay` 32-53 → 37-46, its noise).
  `bench/closures.lisp` 0.97 and 1.00 of the time (`k-generators` 100 →
  84; `k-higher-order` 137 → 150 in two comparisons of three and
  `k-church` 26 → 29 with the shorter `funcall`, which a chain of closures
  called through undeclared variables does not show, 51 against 52 ms:
  what the engine inlines of the recursive `my-map` changed),
  `functional` 1.00 and 0.97 (`f-pipeline` 43 → 50 in one pair of
  rounds), `suite` 1.00, `programs` 0.99 and 1.00 (`p-struct-tree` 80 →
  73, `p-struct-float` 54 → 49); in a script `closures` 1.02,
  `functional` 0.99, `suite` 1.00, `programs` 1.01 (`p-struct-float` 68 →
  51; its first benchmark, `p-cnm-args`, 35-54 for both).
  Measured and not done (the probes of `build/p` of that branch):
  * *The engine's flags.* On the code before: `--no-wasm-inlining` 1.2-1.5
    times the time (esrap 495 → 600 hundreds of microseconds, tokens 230
    → 335), `--experimental-wasm-assume-ref-cast-succeeds` (every cast
    free: an upper bound, not a mode to run in) esrap 0.93, tokens 0.85,
    scan 0.97, `--no-wasm-stack-checks` 0.97, a larger
    `--wasm-inlining-budget` nothing, `--wasm-inlining-call-indirect`
    nothing. After the changes `--wasm-inlining-max-size=1000` gains 3%
    on esrap and nothing on cl-ppcre, so the defaults stay.
  * *The entry in the closure* (one load less before `call_ref`). The
    chain a call through a closure waits for is the closure's `$code`,
    the entry, V8's `WasmInternalFunction`, its call target and the code
    pointer table (five loads from the closure, the instance in
    parallel); the entry in the closure object would take one of them
    and cost 8 octets a closure for each arity kept there. A chain of
    eight closures each calling the next (declared `function`, cl-ppcre's
    shape) takes 48 ms for 3 million walks against SBCL's 29, 42 with
    every cast free: the call itself, not what precedes it, is the
    difference (the first instructions of the functions called, where a
    mispredicted indirect call lands, have 10-12% of the ticks of both
    libraries, the chain before `call_ref` 4-6%).
  * *The cast of a closure.* `ref.cast $closure` of a closure with free
    variables (a subtype) is the engine's slow path: the map, its type
    information, the supertype at depth 0. The ticks of that path are
    13.5% of cl-ppcre's matchers (each reads `next-fn`, declared
    `function`, from its closure and casts it) and 5.4% of esrap's, which
    read as the cost of the cast. They are not: with the final types
    tested first at the call (`br_on_cast` of `$envclo2`, `$envclo1`, ...
    then the cast: one comparison of the map for the closures cl-ppcre
    makes, no load) the chain of eight closures takes the same 47-49 ms
    and cl-ppcre's probes the same (tokens 20.0-20.5 ms, scan 20.0-20.2,
    replace 27.2-27.8, alternation 12.8-13.0, against 19.8-21.0, 20.5,
    27.5-30.0, 12.5-13.0), and through a helper the chain takes 56. The
    ticks are the wait for the closure object itself, which the call
    needs next whatever tests it: the engine starts the loads of the
    cast and of the `$code` together. Every cast of every kind taken out
    (`--experimental-wasm-assume-ref-cast-succeeds`, measured again after
    the changes above) is 6% of the tokens, 3% of the scan, 4% of the
    replacement and 10% of the alternation, of which the closure casts
    are a part. So closures with typed environments (fields `(ref
    $closure)` for the variables declared `function`: a closure type for
    each count of such fields, which the entries of a function, heap
    images and the operations on closures of any shape would have to
    know) were not built: what they remove measures as nothing. A
    closure is still `{code, name, free variables...}`, a final subtype
    `$envcloN` of `$closure` with N mutable `eqref` fields (immutable
    fields for closures that LABELS does not patch were evaluated on
    another branch at the same time, and not done there either).
  * *The type tests of included structures.* A list of 1000 structures
    of two structures including a third, walked with the accessors and a
    predicate of the included one (`build/p/clo.lisp`), takes 88 ms for
    20 000 walks against SBCL's 20: at each access `ref.test $siN` of an
    object with more slots (the slow path), the layout compared, then the
    slot vector's length and its element at the structure's depth. Final
    types for the instances (`$sfN`, tested exactly before `$siN`) would
    take the engine's slow path away where the including structures are
    known. But esrap compiled at `(safety 0)` (no test of the class at
    all, a bare cast) is not faster (50-54 ms against 46), so its time
    is not there, and nothing else measured here walks such lists.
* **Large strings, design note (not done)**: a string is an `(array (mut
  i32))`, four bytes a character, so one of more than 32 768 characters is
  a large object for V8 (over 128 KB), allocated in pages of its own:
  `(make-string 330000)` takes 0.30 ms (SBCL 0.075), an octet vector of
  330 000 0.14 ms (SBCL 0.013), the cost of fresh pages rather than of the
  collector (44 scavenges of 0.08 ms in 400 such strings). Babel's
  `octets-to-string` spends 11 of its 35-40 ms there. Strings of 8-bit
  storage would make such strings a quarter of the size, but CL lets any
  character be stored into a string made as CHARACTER (Babel's, and
  `make-string`'s default), so the 8-bit storage would have to be widened
  on the first store of a character beyond 255: the string becomes a
  header over a storage that can be replaced, every inline `char`/`schar`
  gains a load and a second storage kind to test (`:string` /
  `:string8`), and the runtime's string functions (the reader, FORMAT,
  `string=`, the hashes, cl-ppcre's scanners through them) take two
  storages. Expected: 8 of Babel's 35-40 ms (`l-babel` about 1.45 → 1.35),
  against a few percent on every program that reads strings; not worth it
  as such. An 8-bit storage for BASE-STRING only (where the element type
  says so) is cheaper but nothing measured here makes base strings.
* **Characters out of the i31 space, measured and not done** (2026-11-22).
  Fixnums and characters share `i31ref`, so "is this a fixnum" on an
  unknown value is the i31 test plus a range check (`+ 2^29`, unsigned
  `<`), two for an operation of two operands (two adds, an OR, a compare).
  The alternative studied: characters as a final struct `$char {code: i32}`,
  interned (so that EQ, EQL, the `eq`-comparable fast paths and the hash
  tables need not change) in a table read by CODE-CHAR and SCHAR, and
  fixnum = `ref.test i31`. What depends on the encoding: `+char-tag+` in
  `src/prims.lisp` (57 places: the fixnum input checks of the arithmetic
  helpers and fast paths, `char-code`/`code-char`/`schar`/`char=`, the
  string and stream helpers, `:characterp`, `:check-char`, `:type-code`),
  `src/typed.lisp` (14), `src/codegen.lisp` (constants, TYPECASE's kind
  tests, self-clone checks), every `ref.test :i31` read as "fixnum or
  character" (`:eql`, `sxhash-fast`, the EQ/EQL table fast paths of an
  i31 or a symbol, `%type-code`), the heap image (`%img-ref` writes a
  character as its i31 word; restoring would intern it). Not the hosts
  (characters cross as i32 codes) nor fasls (constants are written as
  text, `C` and the character). Estimated in two probes, each A/B against
  the same tree on the same slot, two rounds:
  - *The gain*, an upper bound: the fixnum input checks dropped (unsound,
    only for measuring: arithmetic on a character then misbehaves) from
    the pair check of the inline operations, `i64-decode-code`,
    `num-decode-code`, `fixnum-pair-code` and MOD/REM. Compiled: `arith`
    0.53-0.54 → 0.51, `suite` 0.80 → 0.76-0.77 (`b-struct` 108 → 86 ms,
    `b-clos` 103 → 86, `b-matmul` 99 → 85, `b-call` 131 → 120), `gabriel`
    0.88-0.89 → 0.86, `holdout` 0.82-0.83 → 0.79, `programs` 0.81-0.85 →
    0.82; in a script `suite` 66 → 63 ms (geometric mean of the times),
    `programs` 62.8 → 61.3. The REPL image 1.5% smaller (9.89 → 9.74 MB).
  - *The cost*: characters still i31, but each one made from a code read
    from a 65536-entry global table (`array.get`, a bounds test, the i31
    built beyond it) and each code taken from a character through a
    dependent load and compare in that table (as the cast and the field
    read of a struct would be), inline at every site the compiler boxes or
    unboxes a character. Compiled `io` 0.87 → 0.90-0.95, `library` 0.83 →
    0.86-0.87 (`v-string-compare` 77 → 100 ms, `v-string-case` 105 →
    125, `v-equalp-mixed` 158 → 178, `v-seq-copying` 88 → 98); libs 1.09
    → 1.11 (`l-ppcre-scan` 133 → 144-148 ms, `l-split-sequence` 107 →
    117-119, Babel unchanged); Lem's comparator unchanged (1.19-1.23 both).
    With the box and unbox as calls of helpers instead (V8 inlines them
    only within its budget) the cost was twice that: libs 1.07 → 1.13-1.18,
    Lem 1.17 → 1.25. The probe leaves out costs the real change adds:
    the write barrier on storing a character (an i31 skips it), constant
    characters read from the table, `characterp` a map compare.
  - *Size and start-up*: a table of the first 256 characters is free
    (0.09 ms, made by `array.new_fixed` in a global's initializer); all of
    the BMP eagerly costs 1.9-2.5 ms and 2 MB at start-up, all of Unicode
    45 ms and 36 MB, so beyond 256 the table would be filled lazily (one
    more null test per character made). The REPL image with the inline
    probe: 0.9% larger (9.89 → 9.98 MB).
  So about 3-5% on undeclared arithmetic (compiled and in a script)
  against 2-5% on string, stream and sequence code and up to 25-30% on
  string comparisons, with the libraries (cl-ppcre, split-sequence)
  slower, the opposite of the campaign's priority, for a change touching
  some 300 places and the image format: not done. The cost is in the
  runtime's character loops that box each character to unbox it again
  (`string=`, `string-equal`, `char-equal`, the case functions, EQUALP on
  strings, cl-ppcre's scanners through `char`); were those kept on codes
  (as `(char-code (schar s i))` already is), most of the cost would go
  and the change would be worth measuring again.

* **Library functions slower than SBCL, item by item (2026-11-23).** The
  items of `bench/library.lisp` and `bench/programs.lisp` still above SBCL
  were each split into a probe per form (the same form in a loop, SBCL and
  Clysm), which finds the slow function at once; the causes and what was
  done:
  - *String comparisons on codes.* `string=`, `string-equal`, `string<`
    and the eight others, `equal` of a string that is not simple and
    `equalp` of strings are two Wasm helpers (`:string-eq-gen`,
    `:string-compare-gen` in `src/prims.lisp`) that take the two
    designators and the four bounds as they are: a simple string, a
    header right over a simple string (a fill pointer, adjustable,
    displaced) or a symbol gives its storage, offset and length
    (`string-data-code`), the bounds are checked (`string-bounds-code`),
    and the loop compares the i32 codes of the two storages, folding the
    case of ASCII letters in place. They answer -1 for what they do not
    decide (another designator, bounds to signal, two characters beyond
    ASCII that differ when case is ignored) and the Lisp function
    (`%string-eq-slow`, `%string-compare-slow`: the general loop only)
    does then. Before, only two simple strings had a loop of fixnum
    operations (a character object made and taken apart per character),
    and a string with a fill pointer took `%array-storage`,
    `%array-total-offset`, `length` and `%check-bounds` as calls. Probes
    (ms, before → after, SBCL): `string=` of a simple string and one
    with a fill pointer 89 → 27 (27), `string>=` with `:start1`/`:start2`
    69 → 24 (33), `string/=` 81 → 29 (49), `string-equal` 53 → 26 (31),
    `string-lessp` with `:end1`/`:end2` 61 → 30 (67), `string<` 42 → 27
    (40). `v-string-compare` 1.31 → 0.69.
  - *Packages named by constants.* `find-package` kept one name (the last
    symbol looked up); `(intern name :keyword)`, `(find-symbol name
    "VP2")` and `(find-package :vp1)` in one loop hashed the name each
    time (22 ns; SBCL 1). `find-package` of a string, a keyword or a
    quoted symbol, and such a package argument of `find-symbol` and
    `intern`, now keep the package in a cons made at load time where the
    call is written (`%find-package-cached`, runtime/package.lisp), valid
    while `*%package-epoch*` (counted by every change of the table of
    package names) is the same and no package has local nicknames.
    `find-package` 44 → 4 ms for 2 million, `find-symbol` with a package
    name 75 → 47 (SBCL 54); `v-intern-packages` 1.29 → 1.02, `p-symbols`
    1.22 → 0.96.
  - *SUBSTITUTE.* It found the length of a list, copied it and walked the
    copy (three walks), and with `:count` made a vector of marks as long
    as the sequence (a large object for 20000 elements) and copied the
    sequence. A fresh list is now made in one walk, a vector is copied
    and replaced in place, last element first for `:from-end`, counting
    down `:count`; only a list read from its end still marks first.
    `substitute` of a list 79 → 35 ms (SBCL 53), `substitute-if` with
    `:count` on a vector 57 → 16 (38), `nsubstitute :from-end :count` of
    a string 69 → 35 (39); `v-substitute` 1.22 → 0.91.
  - *Lists read from the end* (`find`, `position`, `count`, `reduce`
    with `:from-end`, through `%do-region`). The length was found (a
    walk), the region copied to one vector (another, and 40 KB of new
    memory for 5000 elements) and the vector read backwards. The region
    is now copied in one walk into vectors of 64 (`%list-chunks`), read
    last first; a list read to its end is not measured first.
    `position :from-end :key` 57 → 40 ms (SBCL 39-42), `find-if
    :from-end :key` 47 → 29 (58-60); `count-if-not :from-end :key` 121 →
    105 (68-71: the copy is still 2/3 of it; SBCL reverses the list).
    Tried and not kept: the last match of a walk forwards for `find` and
    `position` (the order of the calls is not specified for them): it
    calls the test on every element, and was slower than the copy
    wherever the match is near the end (`find-if :from-end` 47 → 128);
    blocks of 32 conses kept and each block copied to a scratch vector
    when it is read (the conses are read twice: `count-if-not` 121 →
    159).
  - *Copies of sequences.* `fill` of a vector is `array.fill`
    (`%storage-fill`), `replace` of a vector onto itself `array.copy`
    (which allows the overlap), `reverse`/`nreverse` of simple vectors
    and strings read and write the storage without the dispatch on its
    kind, `coerce` of a vector or a string to a list and `concatenate` of
    lists and simple vectors likewise; `%array-storage` and
    `%array-total-offset` answer for a header right over its storage
    without a second call. Probes (20000 elements, 500 times): `replace`
    onto itself 9 → 1 ms (SBCL 4), `nreverse` of an adjustable vector 10
    → 5 (4), `reverse` 25-26 → 17-20 (14-24), `coerce` of a simple vector
    to a list 161 → 96 (84-94). What is left of `v-seq-copying` (1.24 →
    1.09) is the allocation: a vector of 20000 elements is 160 KB, a
    large object for V8 (`copy-seq` 16-18 ms against 8, `concatenate` to
    a vector 71 against 54-57), as strings beyond 32 768 characters are.
    `(coerce (list ...) 'vector)` is `(vector ...)` and `(coerce "a"
    'character)` the character: `p-coerce` 2.10 → 1.35 (what is left
    there: SBCL removes the three allocations it can see through).
  - *A structure predicate of an object that is no instance* (NIL, a
    number, a list, a string: what `(when (node-p x) ...)` is given most
    of the time) called the helper `:struct-typep`, which then found it
    was no instance; it is now told apart at the call site by the test
    of `$instance`. A list of mixed objects 69 → 56 ms for 33 million
    tests. Testing `$instance` first (one supertype) instead of the type
    of N field slots: a string 1.6 → 1.4 ns, a structure of the type
    1.5 → 2.0; not kept.
  - *Not gained, with the reason.*
    `v-typecase-deftype`, `p-typecase`, `p-typep-misc` (1.23-1.30): the
    code is already one kind code per object (`%type-code`: at most
    eleven `ref.test`, each a map compare for the final types) and a
    bit test per clause; the tests of a loop of `(typep x 'node)` over
    non-instances cost 1.0-1.6 ns against SBCL's 0.45 (a lowtag test, and
    a jump table over widetags for `typecase`), and Wasm has no way to
    read an object's type but `ref.test` (ordering the kinds by the
    clauses moves the cost between kinds: floats 7 → 3 tests, conses 3 →
    8).
    `v-map-every` (1.35) and `v-find-list-key` (1.19) are at or below
    SBCL once warm (200 repetitions instead of 10: 1.11 and 0.93): every
    form of `v-map-every` in a loop of its own is 0.2-0.5 of SBCL, whose
    10 repetitions are short enough to be run mostly before the tier-up
    (see Tiering of the engine); SBCL's `map` there is specialized on
    the types it derives for the `let*` variables (4 times its speed
    when the sequences come from special variables).
    `v-hash-iterate` (1.25-1.43, 9 ms): `maphash`, `loop` and
    `with-hash-table-iterator` are already loops over the key and value
    vectors (1.2-1.8 ns an entry, SBCL 1.1-1.3: two bounds-checked reads
    and the test of the deleted marker); the index of `loop`'s scan is
    now an i32 (a rule of the type inference for `%ht-scan-prim`), within
    the noise. `t-return-maphash` was 3 against 6 ms (the timer's
    resolution); with ten times the work (`bench/control.lisp` now) it is
    33 against 52 ms, 1.6: 5.5 ns for a `maphash` left by `return-from`
    after 4.5 entries on average, of which the entry to the loop (the
    class test, two vectors, the fill count, the marker) is 3.
    `v-equalp-mixed`, `p-equal` (1.24): `equalp` of strings now uses the
    helper above (equal strings of 11 characters 42 ms for 3 million,
    SBCL 44-47); the rest is calls (`equalp` of a structure 66 against
    45: the helper calls `equalp` for each pair of fields that are not
    `eq`; `equal` of a list of six objects 53 against 28: a call for
    each element that is a string, a list or a float; settling strings
    and doubles in the loop made it slower, 53 → 70, the function no
    longer fitting V8's inlining of the recursive call).
    The libraries (`l-babel` 1.4, `l-alexandria-utils` 1.3,
    `l-split-sequence` 1.2): profiles are flat. Babel: its decoders and
    encoders are already i32 loops with inline array access (the Wasm
    was read); `concatenate` and `make-string` of the 330 000 characters
    and the two octet vectors are 0.6 of its 2.1 ms a run, the fresh
    pages of large objects (see the design note above). split-sequence
    and Alexandria: `subseq` (12 ns a token), the closure called per
    character, conses (`hash-table-alist`) and `length` of lists just
    made; the same forms compiled as a whole program are 0.45-0.8 of SBCL
    (`position` with `:start`, `:key`, `:test` in a loop 0.45,
    `position-if` 0.73, `subseq` 0.81) and 1.2-1.4 times that when
    loaded into the image: each call of a function of the runtime from
    a library's module is a `call_ref` through the symbol's function
    (five dependent loads, no inlining; see "V8 inlines no call into
    another module"), about 2.3 ns a call. Keyword arguments are not the
    cost (the call passes them in the slots of the function's keyed
    entry when its layout matches).
  Measured (`tools/bench.sh`, the build before and after alternately on
  one slot, the best of two rounds of three runs, against SBCL):
  `bench/library.lisp` 0.75 → 0.71 (`v-string-compare` 1.31 → 0.69,
  `v-substitute` 1.22 → 0.91, `v-coerce-seq` 1.25 → 1.00,
  `v-intern-packages` 1.29 → 1.02, `v-seq-copying` 1.24 → 1.09),
  `bench/programs.lisp` 0.78 → 0.79 (`p-coerce` 2.10 → 1.35, `p-symbols`
  1.22 → 0.96; the other items within ±8% both ways, as two runs of one
  build are), `bench/control.lisp` 0.745 → 0.725, `bench/suite.lisp` 0.725
  → 0.71 and `bench/io.lisp` 0.72 → 0.69 (one round); in a script
  (`MODE=script`) `library` takes 0.92 of its time before
  (`v-string-compare` 84 → 45 ms, `v-substitute` 102 → 67,
  `v-intern-packages` 192 → 153) and `programs` the same (1.12 → 1.115 of
  the compiled file, four rounds); `tools/bench-libs.sh` 1.02 → 1.03 and
  `bench/apps.lisp` 1.19 → 1.20 (two rounds: unchanged, their time is not
  in these functions); Lem's actions (`tools/bench-lem.sh`, two rounds)
  1.047 → 1.009, each of the nine 1-7% faster (`l-type-indent` 45 → 42
  ms, `l-forms-forward` 146 → 138, `l-search` 19 → 18). The REPL image is 21 KB larger (8,875,825 →
  8,896,821 bytes; the two helpers, the chunked loop of `%do-region`'s
  six uses), the module of one export 6.9 KB (925,558 → 932,435; the
  general loops of the string comparisons had been 28 KB with their
  typed copies while they still held the loops for simple strings, which
  the helpers replace).


* **The same items, second pass (2026-11-23): the machine code.** The
  TurboFan code of the loops was read (`node --print-wasm-code
  host/run.mjs FILE.wasm`, the function found by its name).
  - *The entries of a hash table.* `maphash` left by `return-from` (the
    loop unrolled four times by V8) is twelve instructions an entry: the
    count and the bounds of the key vector, the key, the deleted marker,
    the bounds of the value vector, the value, the test. `%with-ht-vectors`
    and `loop`'s `hash-keys` bound the two vectors with `(the
    simple-vector ...)` and the count with `(the fixnum ...)`, a test and
    an error path each at the entry of the loop; they are now
    `%ht-keys-vector`, `%ht-vals-vector` and `%ht-fill-count`
    (`src/prims.lisp`), a field read whose type the inference knows
    (rules in `src/types.lisp`). The vectors must stay variables of the
    vector's Wasm type: read with `%ht-ref` (an `eqref`), `%svref` cast the
    value vector at each entry (a null test, an i31 test and a compare of
    the map inside the loop: V8 does not hoist a cast that is under a
    branch) and the loop was slower than before (165 → 171 ms).
    `maphash` left by `return-from`, 3 million times over 8 entries: 165
    → 153 ms (SBCL 93-98); `maphash` summing 8 entries 376 → 362 (270-283);
    `loop`, `with-hash-table-iterator` within the noise (16-18 ms against
    12). What is left is not the entry of the loop (estimated at 3 ns
    above: wrong) but the two bounds tests and the generic `+`.
  - *A list read from its end, again.* `%do-region`'s loop over the chunks
    was a `tagbody` of four tags (the two nested loops by hand, so that a
    `return` in the body leaves the macro's block); a `tagbody` of several
    tags is a loop around a dispatch on the tag, which every element went
    through. It is two `%do` (the `do` without a block): `count-if-not
    :from-end :key` of 5000 elements 105-108 → 75-85 ms (SBCL 68-71),
    `reduce :from-end` the same way. Of the 80, 37 is `%list-chunks` (the
    whole count read forwards is 27: the walk is the same, the rest is the
    4.4 KB of vectors and their stores) and 44 the loop (25 for the same
    loop written by hand over the chunks). Not gained: the chunk read from
    a variable of the vector's type instead of `%vref` (75-85 both ways).
  - *MAP, EVERY, SOME, MAP-INTO of a list and a vector, measured and not
    built.* The call site's loop reads each sequence through a cursor (a
    test of list or storage, then of the storage's kind, at each element).
    Loops written by hand for the two kinds, 10 million elements: `(map
    'vector #'+ list vector)` 43-45 → 31 ms (SBCL with the types declared
    24-26), `map-into` of a vector and a list 33-36 → 25 (11-12), `every
    #'<=` 26-28 → 19-20 (28); `map 'list` of one vector is already faster
    through the cursor (32-33 against 34-39). Cursors that read a simple
    vector from a variable of its Wasm type, chosen by a kind code, with no
    copy of the loop: 43 → 38-43, 33-36 → 31-36, `every` 26 → 28, `map
    'list #'cons` of a list and a string 58 → 70: not kept. The gain is
    only there with a copy of the loop (and of the function written in
    place, which is inline in it) for each pair of kinds, four or five
    copies at every call of `map` with two sequences, where a function
    holding several inline loops already ran slower (`p-seq-vector`
    above); it would take `v-map-every` from 1.11 to about 0.98 once warm
    and 1.35 to about 1.25 as the benchmark runs it. Not built.
  - *TYPECASE: what each kind costs, and a kind field.* `classify` of
    `bench/programs.lisp` on twelve objects of one kind, 36 million calls
    (ms, SBCL in brackets): a fixnum 101 [89], a character 121 [108], a
    string 114 [109], a simple vector 121 [114], a cons 115 [105], NIL 120
    [106], a symbol 138 [117], a double 141 [106], a bignum 169 [97], an
    instance 176 [117], a closure 191 [110], a single float 210 [96], a
    ratio 214 [127]; the call alone 35. So the kinds found by the first
    four tests of `%type-code` are within 5-13% of SBCL, and each test
    after them is 0.33 ns (12 ms here): the kinds at the end of the chain
    (7 to 11 tests) are 1.3-2.2 times SBCL. `ref.test` then `br_if` and
    `br_on_cast` are the same machine code, so only the number of tests
    counts. Three ways to have fewer, estimated from these numbers on the
    mix of `p-typecase` (1.26):
    (1) *A kind field.* The structs would be subtypes of one struct whose
    first field is an i32 kind (arrays cannot be: they stay `ref.test`):
    i31, array, cons, then one test of the supertype and a load. The late
    kinds would cost what a symbol does now (about 130): `p-typecase` -12%
    (1.26 → 1.10), `v-typecase-deftype` less (its late kinds are
    instances: 176 → 130 for 6 of 17 objects, -8%). Against it: the field
    is 8 octets more in every closure, instance, symbol, ratio and double
    (a single float and a bignum have room), every `struct.get` index of
    the runtime moves by one, and with the cons included the cost is the
    one measured for the identity hash in a cons (`s-build-list` 1.31,
    `a-list-churn` 1.22, `bench/gabriel.lisp` 1.06). Not built.
    (2) *The kinds a TYPECASE names first, in the order of its clauses*,
    the others by the helper: `p-typecase` -6.5% (floats 7-11 tests → 3-4,
    a bignum 9 → 2; a cons 3 → 8, a ratio the whole chain and a call).
    (3) *The number types under one empty supertype* tested after the
    cons: a double 7 → 5 tests, a single float 11 → 7, a bignum 9 → 6, a
    ratio 10 → 8, and one more for NIL, symbols and instances (+3% for
    them): `p-typecase` -5%.
    None reaches SBCL (a lowtag test and a jump table over widetags), and
    (2) and (3) move the cost onto the kinds real programs dispatch on
    most (symbols, structures); the chain is left as it is.
  The files (the commit before and this one alternately on one slot, two
  rounds; one for `programs`): `bench/control.lisp` 0.755 → 0.735
  (`t-return-maphash` 51 → 48 ms, 1.65 → 1.55), `bench/library.lisp` 0.73
  → 0.705 (every item 0-6% faster, also those these changes do not
  reach: the runs, not the code; `v-hash-iterate` 9 and 10 ms),
  `bench/programs.lisp` 0.78 → 0.795 (`p-hash-misc` 61 → 61, `p-word-count`
  79 → 70; unrelated items ±10% both ways); in a script `library` 1.13 →
  1.14 of the compiled file (three rounds, no item beyond ±7%). The REPL
  image is 2 KB smaller (8,896,821 → 8,894,748 bytes), the module of one
  export 116 bytes larger (932,551).

* **Equality, hashing and type dispatch, third pass (2026-11-23).** What
  was changed, with the item it was measured on (ms; SBCL in brackets):
  - *TYPECASE and TYPEP without a kind code* (`%typecase-fused-form`,
    runtime/types.lisp). A `typecase` by the kind computed `%type-code`
    (a chain of tests ending in a number) and then tested each clause's
    mask of kinds, with the full test for the kinds that need one: the
    code read from `--print-wasm-code` was the chain, a shift of 1 by the
    code, and a `test`/`jnz` per clause before the object's own. The
    clauses' tests are now decided for each of the 18 kinds when the form
    is expanded (`%tcf-subst` puts T or NIL for each `%kind-in` and
    simplifies), and the tests that tell the kinds apart branch straight
    to what is left for theirs: the body of the first clause all objects
    of the kind are of, after the full tests of the clauses before it that
    some are of (first matching clause wins, as before: only the tests
    that the kind decides are gone). A fixnum goes from the test of the
    i31 to its clause; arms that several kinds share are written once
    (a block the kinds jump to, where `%kind-is` tells the type
    inference what the object is); a kind whose arm is that of the
    objects no test finds is not tested unless two tests of other arms
    follow it; the layout and class list of the clauses of class names
    are read in the instance's arm only. `typep` of a type of several
    kinds (`%typep-by-code-p`) is such a `typecase` of one clause, and
    `vectorp` and `arrayp` have compiler macros that are too (`vectorp`
    called a helper testing the kinds of storage one after the other:
    3.3 million tests of mixed objects 150 → 44 ms [19]). `%type-code`
    itself, where it is still used (the kinds only it tells apart), tests
    the object for null once and uses `br_on_cast` for the final types
    (V8 kept a null test before each `ref.test` and materialised the
    result of each compare with `setz`/`movzx`/`test`). `p-typecase`
    1.24 → 0.96 (88 → 68 [71]), `v-typecase-deftype` 1.22 → 0.93 (118 →
    90 [97]), an `etypecase` of six clauses 92 → 80 [96], `(typep x
    '(and number (not integer)))` 58 → 48 [35]. In a script `p-typecase`
    103 → 83, `p-typep-misc` 127 → 101, `v-typecase-deftype` 123 → 95.
    The inliner converted the test of a declared type of a copied
    function's argument outside any function (`inline-checked-arg`,
    src/inline.lisp): a block in the test's expansion belonged to no
    function and an exit to it was a non-local exit to nowhere (the
    program ended silently); it is converted as code of the caller.
  - *Similar literal lists are one object.* `const-global`
    (src/codegen.lisp) makes one global for conses whose car and cdr are
    the same constants, in the code of programs (not the runtime's own,
    which keeps state in quoted conses: with it there, `typep` of
    `structure-object` was true of a package), as SBCL's `compile-file`
    does and CLHS 3.2.4.4 allows; strings were shared already. `equal`,
    `tree-equal` and `equalp` of two such literals are settled by EQ:
    `p-equal` 1.23 → 0.88 (92 → 66 [75]). A program that changes a quoted
    list now changes every literal similar to it.
  - *TREE-EQUAL with `:test #'equal`* is `equal` of the trees (the same
    descent; a cons against an atom is false for both), with `#'eql` the
    loop without a test: `v-equal-trees` 1.13 → 0.54 (161 → 76 [142];
    200 lists of five elements, 5000 times, 284 → 68 [244]).
  - *SXHASH of a single float, an integer beyond the fixnums and a ratio*
    in the Wasm helper (`:sxhash-fast`, `:integer-hash`; they called
    `%sxhash`, then `%number-hash`, which called itself for the parts of
    a ratio): 2 million of a bignum 200 → 104 [96], of a ratio 239 → 95
    [95]; `v-sxhash` 1.05 → 0.84 (118 → 94 [112]).
  - *EQUALP of vectors.* A simple vector is told apart before the numbers
    in `equalp`, and `%equalp-arrays` reads a vector with a fill pointer
    or adjustable over simple storage through its header's fields (it
    called `vectorp`, `length`, `%array-storage`, `%array-total-offset`
    for each side); symbols among the elements are not given to the
    recursive call. A vector of four against one (1 million) 380 → 349
    [304], a vector with a fill pointer against a simple one 212 → 183
    [90], a list holding a vector 176 → 166 [153]; `v-equalp-mixed` 1.19
    → 1.14 (148 → 141 [124]).
  The files, this tree and the one before alternately on one slot (two
  rounds, best of each; times SBCL): `bench/library.lisp` 0.683 → 0.656,
  `bench/programs.lisp` 0.692 → 0.683, `bench/suite.lisp` 0.662 → 0.652,
  `bench/control.lisp` 0.651 → 0.651; in a script `library` 1.149 →
  1.124 of the compiled file and `programs` 1.102 → 1.080;
  `bench/apps.lisp` 1.128 → 1.104 (`a-yason-parse` 110 → 96 [88]),
  `tools/bench-libs.sh` 1.031 → 1.038 and Lem's actions 1.059 → 1.044
  (both within the noise). Nothing slower beyond what two runs of one
  build differ by (±8% on items of 10-40 ms).
  *Still behind SBCL, with what it would take.*
  `v-hash-iterate` (1.29, 9 ms) and `t-return-maphash` (1.39): the loop
  is as short as two arrays allow, a bounds test for the key and one for
  the value at each entry (V8 removes neither; the fill count is a local
  already). One vector holding key and value side by side (entry I at 2I
  and 2I+1) would make it one bounds test and one cache line an entry,
  and is a change of every helper of hash tables; not built.
  `p-typep-misc` (1.16): single tests in a loop over eleven mixed
  objects, 1.1 ns a test against 0.45 (`(typep x 'node)` 59 [15],
  `(typep x '(integer 0 *))` 39 [16], `listp` 39 [15]): each is one or
  two `ref.test` already, the rest is the loop (the generic `incf` of the
  sum, the `dolist`'s test) around it.
  `v-equalp-mixed` (1.14): `equalp` of two structures 286 [161] (the
  helper `:sinst-equalp` calls `equalp` through its function for each
  pair of fields that are not EQ: a fixnum against a float, two
  strings); the same test written into the helper for numbers and
  strings would take about half of it.
  `v-hash-equalp` (1.15, 15 ms) and `p-hash-misc` (1.13): `equalp`
  tables of vectors hash and compare in Lisp (`%equalp-hash`, 18 [12]
  for 40000 stores); `make-hash-table` three times 55 [39]; `push` onto
  `(gethash symbol table)` with a `remhash` every hundredth 110 [75],
  which is the conses and two helper calls. *Measured and not kept:*
  `(incf (gethash k h))` and `(push x (gethash k h))` finding the entry
  once (a probe, a read and a store at the index instead of `gethash`
  then `(setf gethash)`): the `push` loop 99 → 92, but the three calls
  it takes are calls into another module for code loaded in the image,
  where a string key then misses the `equal` helper at the call site
  (`p-word-count` in a script 69 → 117).
  `p-calc` (1.14): not `intern` any more (300 000 of a one-character
  name in the keyword package 63 [72], the tokenizer 174 [237]); it is
  the evaluator, 39 [23], whose division makes a ratio at each step
  (`:ratio-op`, see `b-ratio`), and the parser's reads of a special
  variable, 35 [28].
  `b-hash` (1.07-1.10): 200 000 `gethash` of fixnums ten times 177
  [206], the stores 68 [60]: the growth of the vectors.
  `eql` of two single floats that are not EQ is 71 [33] for 2 million
  (`:eql`, then `:eql-gen`); `tree-equal` without a test of two fresh
  lists 142 [110].
  The REPL image is 106 KB smaller (8,596,251 → 8,490,379 bytes: the
  clauses of the runtime's `typecase`s no longer test a kind code each),
  the module of one export 3.5 KB larger (907,414 → 910,925).

* **Literal identity at source compilation boundaries (2026-11-23).**
  Cons coalescing is gated by the lambda's program provenance, recorded
  when the frontend creates it, rather than by its optimization policy.
  Runtime literals stay distinct; live compilation returns the original
  object before considering coalescing. `literal-cons-identity.lisp`
  checks EVAL and COMPILE against the actual objects in their source,
  distinct literals from source LOAD, and equality after COMPILE-FILE.
  It passes in both whole-program and native script modes.

* **Adjacent hash entries (2026-11-23; experiment, reverted below).** Keys and values occupy cells
  2I and 2I+1 of one vector. Chain links and saved lookup indices still
  count entries; the former value-vector field holds capacity. Growth,
  compaction, package lookup, all Wasm probes, iteration and heap-image
  rehashing use the same layout. This saves one vector allocation per
  nonempty table. Image version 7 rejects images with the old layout.
  Initial compiled hash tests, native hash tests and the 1,505 ANSI hash,
  iteration and package tests pass. The first `v-hash-iterate` measurement
  is 10 ms: no demonstrated speedup yet; repeated comparisons follow below.

* **EQUALP structure fields and vector hashes in Wasm (2026-11-23).**
  The structure helper calls one shared field helper for fixnums, single
  and double floats, and ASCII strings. Large integers, ratios, Unicode
  case differences and other objects use ordinary EQUALP. The vector hash
  loop handles fixnums, symbols, ASCII strings and nested simple vectors;
  empty and character-leading vectors keep the string-compatible path.
  Three alternating runs of the same precompiled probes on one slot:
  structure comparison 253 -> 204 ms (best), vector stores 18 -> 17 ms,
  vector lookups 20 -> 20 ms. The latter changes are within noise.
  Four equality/hash regression files pass, including exact large integer
  comparisons, Unicode and vector/string keys. The probe module shrinks
  3,082 bytes because field fallback code is shared.

* **Empty hash-table prototypes, measured and not kept (2026-11-23).**
  Four empty tables (one per test) were copied by a Wasm helper loading
  their 17 fields into a fresh struct with a fresh identity hash. Generic
  construction replaced requested options; constant-test construction
  only replaced size. The three-table creation/insertion probe, three
  alternating runs on one slot: ordinary 46, 56, 42 ms; copies 104, 44,
  47 ms. Best 42 -> 44 ms, no gain, and the probe module grew 5,246 bytes.
  Hash-model and native tests passed. Reverted: copying the fields adds
  loads where initialization already has constants; allocating first-entry
  storage is still needed. Avoid repeating this without eliminating those
  loads or changing how the first entry is allocated.

* **Single floats at EQL call sites (2026-11-23).** The bit comparison
  moves from `:eql-gen` to the small `:eql` helper, avoiding the call to
  the general numeric cases. Signed zero still compares by bits.
  Five alternating runs on one slot, twenty million comparisons of two
  independently read single-float boxes (both builds print `distinct: T`):
  65, 66, 66, 64, 63 -> 43, 43, 46, 42, 43 ms. Best 63 -> 42 ms,
  median 65 -> 43 ms. A prior probe using literal floats was inconclusive
  (56 -> 57 ms); it did not ensure distinct boxes. Typed-element and
  dynamic single-float tests pass, including mixed types and signed zero.

* **Inlining budget after interleaving (2026-11-23).** Encoded helper
  bodies in the probe module: `:ht-get` 106 bytes, `:ht-put` 435,
  `:ht-put-id` 483. `:ht-put-equal` grew to 677 (650 before), which
  exceeds the host's 640-byte V8 limit. Its new-entry path is now a
  separate 255-byte `:ht-insert`; the store helper is 453 bytes. Existing
  entries retain the cached-key path. Hash-model and equal-table tests
  pass. TurboFan disassembly of interleaved MAPHASH still contains both
  bounds checks (for 2I and 2I+1); adjacency alone did not deliver the
  predicted single check. Keep that distinction in performance claims.

* **Hash layout tradeoffs and iteration offsets (2026-11-23).** Against
  the same equality improvements with separate key/value vectors, three
  alternating runs of the probes give three-table construction/insertion
  50 -> 41 ms with interleaving. MAPHASH is 9 -> 9 ms; explicit iterator
  14 -> 15, LOOP keys/values 13 -> 14, LOOP values 12 -> 14, early return
  46 -> 44 ms. One fewer allocation is useful, but iteration is not a
  demonstrated improvement. Longer iteration probes (100 repetitions,
  three interleaved rounds; baseline 2878b0c / interleaving candidate /
  experimental direct cell offsets): MAPHASH 99 / 93 / 99 ms, explicit
  iterator 147 / 157 / 152, LOOP key/value 143 / 149 / 154, LOOP values
  137 / 137 / 142, early-return MAPHASH 490 / 456 / 462. Direct offsets
  remove repeated index scaling but give no consistent gain: not kept.
  Splitting insertion out of the EQUAL store helper, three alternating
  rounds, changes equal-list stores 44 -> 40 ms and reads 43 -> 45 ms;
  the code-size reduction is established, these small time changes are not.

* **Interleaving rejected on real applications (2026-11-23).** The
  retained tree uses separate key/value vectors and image version 6.
  Four alternating rounds against 2878b0c put ESRAP 83 -> 97 ms and Yason
  parsing 91 -> 107 ms with interleaving. An isolated comparison retaining
  the equality changes and the small EQUAL store helper confirms the
  layout's cost (two alternating rounds): interleaved ESRAP 100, 101 ms,
  separate vectors 86, 91; Yason 109, 118 versus 99, 102. SBCL times in
  those rounds are essentially unchanged. Keeping insertion in the larger
  store helper does not recover the loss (ESRAP 97, 100; Yason 121, 125).
  The reduced allocation in the construction probe does not compensate
  for the application regressions or remove the two bounds checks. The
  representation, package accesses, iterators and heap-image changes are
  reverted. The small store helper and EQUALP/EQL improvements remain.

* **Retained tree against 2878b0c (2026-11-23).** Two alternating
  baseline/new rounds on one benchmark slot, each using the scripts'
  best of three runs; four rounds for script mode because early CLOS
  timings were bimodal. Geometric means from each item's best measured
  time (Clysm/SBCL): library 0.663 -> 0.652, control 0.657 -> 0.626,
  programs 0.679 -> 0.677, suite 0.639 -> 0.641, apps 1.096 -> 1.117,
  libraries 1.028 -> 1.016, Lem with the supplied input 1.054 -> 1.019.
  Script/compiled: library 1.120 -> 1.120, programs 1.085 -> 1.090.
  Clysm-only time ratios, new/baseline, are respectively 0.990, 0.963,
  1.041, 1.030, 1.043, 1.017, 0.959; script library 1.003 and programs
  1.004. Small aggregate differences are not established speedups.
  No retained broad-suite item is slower by more than 15% in these
  best-time comparisons. `v-hash-iterate` is 9 -> 10 ms compiled
  (9 -> 9 script); longer focused iteration probes are unchanged.
  `t-return-maphash` 45 -> 45, `v-equalp-mixed` 134 -> 128,
  `v-hash-equalp` 15 -> 15, Lem redisplay 43 -> 42 ms. Three alternating
  focused rounds confirm structure EQUALP 253 -> 196 ms; simple-vector
  hash stores 17 -> 16 and reads 18 -> 19 remain noise. Three-table
  construction/insertion is 50 -> 51 ms. The retained EQUAL store helper
  is 438 bytes, with a 243-byte insertion helper. The one-export module
  shrinks 910,925 -> 907,730 bytes (budget unchanged at 970,000), and
  the REPL image 8,490,379 -> 8,476,185. Library/ASDF checks and the
  self-hosted compiler build pass. The process test is blocked by this
  sandbox's Node `spawnSync` returning EPERM even with exit status 0;
  the same failure reproduces on 2878b0c and cached main 1b02cb3e.
  Final compiled, interpreted, native-script and self-hosted test runs
  each pass 118/119 files, with only that process failure. `make test-all`
  stops there; its remaining components were run separately. Full ANSI
  passes 21,931/21,932, with only the known SUBTYPEP-COMPLEX.8 failure.
* **Lists and mapping, third pass (2026-11-23): checked casts where a
  cons is certain, and loops per kind of sequence.** Instruction-level
  profiles (`node --prof --prof-sampling-interval=100` with
  `--print-wasm-code`, the ticks placed on the TurboFan listing) and
  `--trace-gc` of the list items still behind SBCL.
  - *CAR and CDR of a value known not to be NIL* (`typed-access-code`,
    `src/typed.lisp`): after `(null x)` was false, `(car x)` was a test
    of cons, the field on one arm and the call of the helper (which
    signals) on the other; V8 keeps that call as a path that returns, and
    knows nothing of `x` after it. It is a `br_on_cast` over the helper's
    call followed by `unreachable` (the helper signals the error of type
    LIST and does not return; a plain checked cast would say CONS), after
    which the engine drops the casts of the later reads of the same
    variable.
    `browse-match` of `g-browse` (three clauses that read `(car pat)`,
    `(car dat)`, `(cdr dat)` after two `null` tests): 162-164 → 116-118 ms
    (SBCL 139-140: 1.20 → 0.87). `rplaca`, `rplacd` and the SETFs of CAR
    and CDR checked their cons with a call of `:check-cons`; they are the
    checked cast `ref.cast :cons` (`g-destru` 80 → 74 ms, 1.07 → 1.00).
  - *`%endp` of the variable itself.* LOOP's `for x in` tested its list
    with `(endp g)`, whose compiler macro binds its argument: what the
    test learnt was of that binding, and `(car g)`, `(cdr g)` tested the
    cons again with the helper's call behind. LOOP uses `%endp` of its own
    variable (the macro is in `runtime/boot.lisp` now, before
    `loop.lisp`): `loop for x in l always`, 8 elements, 16 → 12 ms for
    1.5 million (SBCL 4), `maximize` 70 → 61 (19).
  - *LOOP's NCONC, and APPEND of a fresh `(list ...)`*, called `last` of
    the tail before linking each list and `last` of the list after it:
    the tail variable is the last cons so far, as for COLLECT, and the
    list is followed to its end in place (no call). `nconc (list i i)`
    188-203 → 141-154 ms (SBCL 93), `for (a . b) in ... append (list a b)`
    61-67 → 47 (26), `collect ... into` with `append ... into` 85-95 →
    66 (47). `p-loop-macros` itself is within the noise (its time is the
    sum and collect clauses, already at or below SBCL: each clause kind
    alone is 0.4-1.0 of SBCL except these and `always`/`maximize` over a
    constant list of 8, 3-4 times: the entry of the loop, 6 ns).
  - *MAP, MAP-INTO, EVERY, SOME, NOTANY, NOTEVERY of `#'name` over two
    sequences (and MAP to a vector of one): a loop for each mix of kinds*
    (`%kind-loops-expansion`, `runtime/seq.lisp`): lists and simple
    vectors in any mix, two simple strings, and the cursor loop for the
    rest, chosen by tests of the kinds at the call site. This is the
    design measured and not built before (above) because of the call
    site's size; what is different: only for `#'name` (the call copied
    into each loop is a primitive or a direct call, not a lambda's body),
    so a site is five or six short loops. As out-of-line local functions
    (`flet` with `notinline`, which needed a mark on the function for
    `local-inlinable-p`) the same loops ran no faster than inline and the
    module was 50 KB larger for the two uses in the runtime (each local
    function has an entry, a closure constant and its locals); inline
    they are 9 KB there. Ten thousand elements, 100 times, ms before →
    after (SBCL): `map-into` a vector from a vector and a list 28-30 →
    15-16 (12), from itself 14-15 → 9-10 (8), `some #'>` of a vector and
    a list 17-18 → 12-13 (10), `every #'<=` of a list and a vector 14-16
    → 12-13 (26), `map 'vector #'+` of a list and a vector 33-37 → 32-34
    (32); `map 'list #'cons` of a list and a string unchanged (66-72, SBCL
    38: allocation). `v-map-every` 0.92 of its time with 100 repetitions
    of its body, 0.94-1.00 as the benchmark runs it (10 repetitions: a fifth
    of its ticks are in Liftoff's code of the function and 20% in the
    collector, see Tiering of the engine).
  - *Not gained, with the reason.*
    `v-mapping` (1.6): of 360 ticks, 177 are the TurboFan code of the
    function, 32 Liftoff's, 80-100 the collector. Each form alone, 120
    repetitions (ms, SBCL): `mapc` of a lambda over two lists 10 (9),
    `mapcar #'+` 29 (26), `mapcar #'cons` 37 (28), `mapcan` of one list
    18 (13), of two 30 (24), `mapcar` of a lambda over three 34 (22). The
    lambda is inline (no closure, no call), the end tests are one
    `ref.test` a list, the tail's store has no write barrier on its path
    (a test of the page's flag, not taken for young objects); what is
    left an element is the allocation (five stores for a cons of 32
    octets, SBCL three for 16), a load-use stall on the car of the second
    list (the lists are 160 KB each and the young generation moves them),
    and `(+ x y z)` as two i64 additions with their overflow tests.
    The young generation's size decides the rest: with semi-spaces of 2
    or 4 MB (`CLYSM_NURSERY_MB`) `v-mapping` is 31-32 ms instead of 38-45
    (the nursery stays in the second-level cache), but `bench/alloc.lisp`
    is then 2.07 times slower (`s-ring` 8.6, `s-build-list` 4.4) and
    `suite` and `library` 1.03: not a setting to change.
    `a-list-churn`, `s-build-list`, `s-ring` (1.2-1.4): allocation and
    scavenges only (`s-build-list`: 1336 of 2027 ticks in the collector;
    `s-ring`: `length` of lists the scavenger moved, 623 of 1628); the
    floor of 32-octet conses.
    `g-traverse` (1.13): `travers` spends its ticks on the map compare
    after the first load of each node and each cons of the list of sons
    (cache misses on 120- and 32-octet objects); `g-dderiv`, `b-deriv`
    (1.1): allocation and 10% in the collector, the rest `get`/`getf`
    and the `call_ref` of the property's function. Property lists as a
    hash for hot symbols were not tried: the lists here are of one or two
    properties.
    `i-format-justify` (1.2): flat, no function above 8% (the
    justification's padding, the string streams, the pretty printer's
    queue for the logical block; 13% in the engine's runtime).
    `p-queens`, `d-triangle`: one function each, bounds checks (see
    "`d-triangle`, instruction by instruction").
    `v-find-list-key` (1.15-1.2): 161 of 440 ticks are `%list-chunks`
    (the lists read from their end), as measured before.
  Interleaved with the commit before on one slot (`bench/*.lisp`
  compiled, best of 5 rounds, time against the base's): `gabriel` 0.94-0.96
  (`g-browse` 0.72, `g-destru` 0.93, `g-takl` 0.88-0.90), `alloc` 0.97,
  `holdout` 0.98, `control` 0.99, `programs`, `suite`, `io` 1.00,
  `declared` 1.01, `library` 0.99-1.015 (two runs of 5 and 8 rounds: no
  item differs twice); in `clysm script` `gabriel` 0.98 (`g-browse`
  0.80), `suite` 1.00, `programs` 0.97-1.02, `library` 1.02 (one run of 3
  rounds, the machine loaded). The REPL image is 30 KB smaller (8,596,609
  → 8,566,898 bytes: the cast is shorter than the test, the two arms and
  the helper's call), a compiled program 2-5 KB larger (the kind loops of
  the runtime's two `every` of two sequences), the module of one export
  106 bytes (907,414 → 907,520).

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

`tools/bench.sh bench/declared.lisp` (2026-11-23, best of three; the last
column is the ratio on 2026-11-21, when the file was added and Clysm read
no `safety`):

| Program | SBCL ms | Clysm ms | Ratio | 2026-11-21 |
|---|---|---|---|---|
| `d-triangle` | 131 | 185 | 1.41 | 2.51 |
| `d-particles` | 143 | 193 | 1.35 | 3.53 |
| `d-pidigits` | 191 | 237 | 1.24 | 1.21 |
| `d-fasta` | 129 | 142 | 1.10 | 1.11 |
| `d-binary-trees` | 179 | 197 | 1.10 | 1.07 |
| `d-quicksort` | 175 | 189 | 1.08 | 1.17 |
| `d-nbody` | 211 | 224 | 1.06 | 3.52 |
| `d-revcomp` | 119 | 123 | 1.03 | 1.02 |
| `d-crc32` | 149 | 150 | 1.01 | 1.75 |
| `d-spectral-norm` | 140 | 137 | 0.98 | 2.87 |
| `d-mandelbrot` | 179 | 176 | 0.98 | 1.34 |
| `d-fft` | 102 | 100 | 0.98 | 0.83 |
| `d-sieve` | 124 | 117 | 0.94 | 0.89 |
| `d-fannkuch` | 145 | 135 | 0.93 | 2.35 |
| `d-tokenize` | 163 | 146 | 0.90 | 1.10 |
| `d-fixnum-hash` | 176 | 147 | 0.84 | 1.27 |
| `d-k-nucleotide` | 105 | 71 | 0.68 | 0.85 |
| `d-matmul` | 142 | 76 | 0.54 | 2.75 |
| `d-tak` | 206 | 76 | 0.37 | 2.16 |
| geometric mean | | | 0.94 | 1.56 |

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
(90 → 63), `p-coerce` (47 → 33), `p-double-2d` (36 → 26). Declared code was
where Clysm was behind: 1.56 against about 0.9–1.1 on undeclared code on
2026-11-21, 0.94 on 2026-11-23.
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

**`d-triangle`, instruction by instruction** (2026-11-22; `node --prof
--prof-sampling-interval=50` for the ticks of each pc, `node
--print-wasm-code` for the TurboFan code; SBCL's `disassemble`). The loop
of `triangle-count` runs 11.6 M iterations a run, 2.8% of which make the
recursive call. SBCL's iteration is 9 instructions (three loads, the
board's load and compare, the counter), all in registers. V8's is about
25: the same loads and compare, plus a bounds check for each of the four
arrays (`cmp`/`jnc`, the lengths of three of them spilled; the one of
`from` repeats the loop's own test: V8 eliminates no array bounds check),
the loop's stack check, the reload of `n` and of `from`, the counter
spilled and reloaded each iteration, and the sign extensions of the
indexes. What each costs, measured with V8's own switches (not CL
semantics): `--no-wasm-stack-checks` nothing (273 → 270-281 ms);
`--experimental-wasm-skip-bounds-checks` alone makes it slower (354 ms:
the register allocator then keeps the loop index in a stack slot, a store
and a load on the loop-carried path); with the arm that makes the call
marked cold (`sys::%cold`, so a deferred block where the spills go) the
same switch gives 154 ms, SBCL's time — but the cold mark alone gains
nothing (275). So the excess is the bounds checks with the register
allocation around them; Wasm GC arrays have no unchecked access, and
neither a typed reference nor a hoisted `array.len` changes what V8 emits.
Not taken further: passing `--experimental-wasm-skip-bounds-checks` would
apply to all code, including code that relies on the trap.

Measured again (2026-11-22, the code of main then, interleaved runs on one
slot): the cold mark on the arm with the call now gives 263 → 206 ms by
itself (the counter and the loop's values stay in registers on the path
without the call; `--print-wasm-code` shows the call, its spills and the
stores in a deferred block), so `if-branch-hint` (`src/codegen.lisp`) now
gives that hint without a mark: in a loop, an arm that calls the function
itself (by its name or as a local function) is hinted unlikely when the
other arm calls it not and leaves the loop not (a search that recurses on
the moves that apply: `d-triangle` 250 → 201 ms, 1.90 → 1.49 of SBCL;
`p-queens` 77 → 70). Not for the IFs of a test (`(do ... ((or (= j 36)
(try j d))))`, gabriel's TRIANG: there the call is what the loop does).
Hinting every arm with a call out of line (the call heuristic of static
branch prediction) was measured and is not kept: it misses the method call
of a generic function's site cache, whose other arm is a slot's writer
(`p-method-comb` 42 → 50 ms), and gives 1,672 hints in the runtime against
a handful. The module of one export is 99 bytes larger.
The hint needs the test as branches: since an IF for effect makes its
compound test an i32 of nested IFs unless the test begins with a compound
test (`effect-branch-test-p`, for MAPC's loops), the AND of TRIANGLE-COUNT
was materialized (`setc`, a spill of the i32, its test) and tested once with
the hint, and the gain was gone (main with the hint 251 ms, without 263).
`cg-effect` now uses `branch-code` too for a compound test whose arm gets
`recursive-arm-hint`: 251 → 185 ms (1.37 of SBCL); no other function of
`gabriel.lisp` or of the runtime changes.

The board as a `(simple-array fixnum (*))` (the user's choice, not an
inference) was 1.45x slower than the simple vector, not 0.87: `(eql (aref
board f) 1)` (the frontend makes it an `EQ`) boxed each element read (a
range test and `box-i32`) for a `REF.EQ`, because the i32 comparison was
only made when one operand was an unboxed variable. `EQ`/`EQL` of two
integers within 32 bits is now an `i32.eq` when either is computed as an
i32 (`i32-natural-node-p`: an unboxed variable, an element of a vector of
integers, a raw call's result), for `EQ` when one of them is a fixnum (two
equal integers are then the same fixnum); with the hint above that board
runs in 168 ms against the simple vector's 201. What is left between the
two is the comparison with a fixnum constant: Node's V8 has no pointer
compression, an i31 1 is the word `0x100000000`, which `cmp` cannot take
as an immediate, so it holds a register in the loop and the counter is
spilled instead (an `i32` element compares with an immediate). Giving the
board an i32 representation behind the program's back (lead 1 of the
notes: a simple vector proven to hold only fixnums) would need every
function that sees the vector, here a global function its caller passes it
to, to agree on it; not done.
Loading `over[m]` and `to[m]` only after the test of the element before
(the `let` of the three written as nested `let`s and `when`s, by hand)
changes nothing here (192 ms against 182-203) and makes SBCL's faster (126
against 139): the loop runs at 0.8 ns an iteration, three cycles for its
seventeen instructions, and is not bound by their number.

Specialized vectors in the linear memory instead of Wasm GC arrays were
measured (2026-11-22) and not done. The idea: a Wasm GC `array.get`/`set`
always tests the index against the length (V8 never removes it), while on
a 64-bit host an access to the linear memory has no test (guard pages and
the trap handler). A `(simple-array double-float (*))`, `(unsigned-byte
32)` and the like would be a struct of an offset and a length into a
memory the runtime manages, with only the Lisp-level test that the safety
asks for. Measured:

* *Upper bound on the benchmarks.* The compiled bench files run with V8's
  `--experimental-wasm-skip-bounds-checks` (which drops the test of *every*
  GC array, simple vectors of T too, so more than the linear memory could
  give), best of three, interleaved with normal runs on one slot. The time
  with the flag divided by the time without, as a geometric mean:
  `declared` 0.99, the same file at `(safety 1)` 0.99, `arrays` 0.99,
  `vectors` 0.96, `arith` 1.00. The best items: `d-matmul` 0.86,
  `x-matrix` 0.88, `x-sieve` 0.86, `a-hash-counts` 0.90. `d-triangle` is
  *slower* with the flag, 255-295 → 351-379 ms in five runs of each: V8
  places and allocates registers for its loop differently. The bounds
  tests are not what keeps it at about twice SBCL's time.
* *The loops by hand.* Wasm modules written with `src/wasm.lisp` (triangle-count
  as Clysm compiles it at safety 0, a 600×600 flat double matmul, a sieve
  over 20M octets), each over GC arrays and over the memory (the offset
  read from the struct once at the function's entry): triangle with its
  three `fixnum` vectors in the memory 0.92 of the GC version (292 → 265
  ms), with the board too as a vector of `i32` 0.83 (but the board as a
  GC `i32` array alone is already 0.87: most of that is the board's `T`
  elements, not the tests); matmul 0.88-0.91 (101-105 → 91); sieve 0.92.
  About 10% in the kernels that do nothing but index specialized vectors.
* *Freeing.* Wasm GC has no finalization, so the memory of a vector could
  be given back only through a host `FinalizationRegistry`, whose callbacks
  run only after the running job ends: three million objects registered
  in one synchronous run, none freed during it, 2.47 million after
  returning to the event loop. A Lisp program runs its toplevel forms in
  one job, so a loop making temporary double vectors would grow the memory
  without bound. Registering also costs about 34 ns a vector (one million
  structs of an offset and a length), against 8.6 ns for making a GC array
  of 16 doubles; a bump allocation in the memory with its zeroing costs 6
  ns, but nothing frees it.
* *What else it would take.* About a hundred places in the compiler that
  name the vector types (`src/prims.lisp` 61, `src/layout.lisp` 23,
  `src/driver.lisp` 11, `src/typed.lisp` 8), the generic accessors, heap
  images (which are themselves loaded at offset 0 of the memory, so the
  arena would have to be saved and placed elsewhere), constant vectors in
  fasls, adjustable arrays (whose old storage would have to be freed), and
  the host's octet transfers through the memory.

So at most a few percent on the files that index specialized vectors
most, none on `d-triangle`, against an allocator without a way to free
during a run. A Lisp-level test that the compiler hoists out of a loop
does not help either while the Wasm array keeps its own test, and the
upper bound above says that test is cheap where it is not the whole loop.

### Size and start-up

Measured on 2026-11-23 (sizes of the `.wasm`; peak RSS by `/usr/bin/time`,
best of three; `node -e 0` takes 43 MB and 0.01 s):

| Module | Size | `gzip -9` | Peak RSS |
|---|---|---|---|
| `tests/hello.lisp` (`clysm compile`, run by `clysm exec`) | 5.2 MB | 1.40 MB | 136 MB |
| the same, `--shake` | 2.9 MB | 0.84 MB | 87 MB |
| the same, `--shake-symbols` | 2.9 MB | 0.84 MB | |
| a library of one export, `(+ a b)` (`clysm library`, imported by Node.js) | 4.9 MB | 1.28 MB | 121 MB (0.16 s) |
| the same, `--shake-symbols` | 1.25 MB | 0.38 MB | 62 MB (0.04 s) |
| the REPL image with the compiler (`build/clysm-native.wasm`, `clysm eval t`) | 8.6 MB | 2.2 MB | 129 MB |
| its heap image (`build/clysm-native.img`) | 10.8 MB | 3.0 MB | |

Hello keeps the printer and `format`, which it uses; the library uses
neither, so a shaken module leaves them out (see below). `clysm exec` runs
the module in a worker thread with a large stack, which a library imported
by a program does not have.

A whole program carries the runtime: `tests/hello.lisp` was 4.3 MB when
`--shake` was written (1.25 MB with gzip; the tests after calls for a pending exit, "Non-local exits",
account for 8% of it). With `--shake` (`bin/clysm compile --shake`, `bin/clysm library
--shake`) the global functions that nothing refers to by name are left out
(`shake-module` in `src/wasm.lisp`): the definition of each function is a
small Wasm function `(:install SYMBOL N)`, kept only when the symbol is
reached from the exports, the start function or the initializer of a reached
global, and the functions and globals that are then unreachable are dropped.
Hello became 3.6 MB (2.9 now, with the work below), and a library started in about 50 ms
instead of 65 (12 ms to compile the module, the rest the toplevel forms of
the runtime). Every symbol stays, so `intern` and `read` work, but a function
called only by a name made at run time (`(funcall (intern ...))`, `eval` of
new code, `~/name/` in a control string made at run time) is not there.
With `--shake-symbols` (implies `--shake`) the symbols nothing refers to go
too (`shake-module :drop-symbols t`, and the symbol table is made again of the
symbols left): the program promises not to look up by name a symbol its
code does not refer to. What stays is every symbol that reached code or a
reached constant names (the keywords of `&key` parameters, the names the
printer and the reports of conditions use, the symbols of `defvar`s),
those whose definition's value is used, and every package (`:pkgtab`).
The initial value of a variable is then set by a function `(:install
SYMBOL :value N)` of its own, kept only with its symbol, and an install
whose value is dropped at once goes with the `drop`. The names left out
of `COMMON-LISP` and of the program's own packages (not the runtime's,
but `COMMON-LISP-USER`) are a third data segment: `find-symbol`, and so
`intern` and the reader, signal a `package-error` ("... is not in this
module: it was compiled with --shake-symbols") when a name not found is
one of them, instead of making a new symbol that is not the one meant.
`DEFPACKAGE` makes anew a symbol it exports that was left out, and does
not import one (`%defpackage-import`); the printer looks up names without
the check (`%find-symbol`). The module of one export is 43 KB smaller with
it (4,224,522 bytes, 951,011 with `gzip -9`: 3,440 symbols of 5,050 are
reached from the code the runtime's toplevel forms keep, so it gains more
as that code goes, issue #30); `tests/lib/symbols.lisp` is a library
compiled with it.
The expander of a macro is kept only when something refers to the macro's
name (the definition is wrapped in `sys:%when-reached`, installed as a
function definition is): compiled code has expanded the macro already.
`SHAKE=1 tools/run-tests.sh` runs the test suite this way. What remains is
mostly reached through generic paths (the printer, `format`, CLOS dispatch,
conditions) and the constants and closures of the global section (0.33 MB).

A `block` and a `tagbody` save the binding stack pointer on entry, for an
exit out of special bindings or an unwind made in them to restore it; the
save (`(global.get :dyn) (struct.get :dynstate 0) (local.set n)`, 7 octets)
is now made only when the code compiled in them restores it
(`used-save-bsp-code`: `restore-bsp-code` notes the local in the
function's `fstate`). It was most of the prologue of GETF and of every
`do` and `dolist`: the module of one export 2,011,969 → 1,982,417 bytes,
the REPL image 8,849,051 → 8,756,399;
the time the same (geometric means of the benchmark files 0.97-1.01).

`order-functions-by-use` (`src/wasm.lisp`, before a whole program, a
library or the REPL image is encoded) gives the indices below 128, one
octet of LEB128 instead of two, to the functions that calls and `ref.func`
name the most (the entries for a wrong number of arguments in every
closure's code, `:apply-generic`, the casts and helpers), and to the
globals referred to the most among those whose initializers refer to no
other defined global (the symbols, the runtime's state): the module of one
export 4,163,772 → 4,053,666 bytes (gzip 971,312 → 965,589), the REPL image
9,285,446 → 8,873,131. The others keep their order: sorting all the
functions by use made the module as small but 25 KB larger with gzip,
which compresses functions of the same file next to each other better.

What the global section of the module of one export holds then (330 KB):
3,932 static closures, 130 KB (33 octets each: the `ref.func`s of the
seven entries, most of them the shared ones for a wrong number of
arguments, the code struct, the name; near the least a constant
expression can take), 1,755 constant lists, 98 KB (16,000 conses: the
`struct.new` of each, 3 octets, and the `global.get` of its element; a
bytecode for them run at start-up would save the `struct.new`s, about
30 KB, and little with gzip, so it was not done), 5,078 symbols, 35 KB,
and vectors, 27 KB.
For pages, the module is best served as a file of its own, which the browser
caches for every page that uses it.

**The runtime's state made when the program is compiled** (`src/preinit.lisp`,
with `--shake` on SBCL). The runtime's toplevel forms make its packages,
classes, generic functions and their methods, structures, readtables and
tables; run when a module starts, they kept their definers (`%defmethod`,
`%define-structure-class`, `%finalize-class`, `%defpackage`,
`%init-standard-readtable`...) and all those reach in every shaken module:
2.2 of the 3.6 MB of code of the module of one export was reached only from
them. Now the program is compiled as before; then a module whose start
function runs only the runtime's toplevel forms and saves the heap
(`%preinit-save`: the format of Heap images, with the kind of each of the
image's objects after the roots) is run with `host/run.mjs` (0.3 s), and
the module made has instead a passive data segment of those objects. Its
start function copies it into the memory and makes them
(`:image-load-preinit`: the loader of heap images, its words LEB128
numbers, a record's object given by the difference of its index from the
previous one's, symbols' records without their names), then calls
`%image-start` (a package's tables of symbols are saved as vectors of their
symbols and made into tables again; the host variables; the tables hashed by
identity), then runs the program's toplevel forms. The objects are shaken
with the code (`*shake-heap-hook*` of `shake-module`): an object is kept
when kept code, a kept global or a kept object refers to it, and what a
symbol names (its value, function and properties) only when the symbol is
referred to, other than by a package's tables, so that what the runtime
defines stays as the definitions did, by being named. The expander of a
macro (the property `%macro-function`) is kept only when code or a
constant names the macro, not only an object such as the pretty printer's
table of forms (those of COMMON-LISP also when the reader is kept: the
forms of text read and evaluated); the properties of compiler macros only
when `compiler-macro-function` is kept, documentation only when
`documentation` is. A constant list or vector whose fields are as its
initializer made them has no record, and the conses and vectors that its
initializer made are not written again. `CLYSM_NO_PREINIT=1` compiles as
before; `CLYSM_PREINIT_STATS=1` prints what the objects are and what keeps
them. The module of one export: 4,518,062 -> 3,628,248 bytes (`gzip -9`
998,495 -> 919,111): code 3.59 -> 2.75 MB, globals 774 -> 602 KB, branch
hints 114 -> 90 KB, and 166 KB of data (67 KB with gzip). `tests/hello.lisp`
with `--shake`: 4.85 -> 3.81 MB. Loading the module of one export in a new
Node.js process: compiling it 13 -> 10 ms, instantiating it and its start
function 40-46 -> 14-20 ms. The definers are now reached only through what
needs them at run time: the expanders of `defstruct`, `define-condition`,
`defpackage`... (kept because the reader was: the printer refers to
`*standard-readtable*`, whose functions are the reader's), and the dispatch
of a generic function, which fills its caches when it is called. What keeps
the rest of the code is the printer (issue #30's item 1), the methods of
generic functions (`*method-fast-functions*` holds every method's function:
item 2, below) and `eval_string`'s reader and evaluator (item 4). A function that
only the runtime's toplevel forms called is no longer there for `eval` of
new code (`eval_string` of `(symbol-package 'car)`), as the definitions
that nothing names were not before.

**The message of an unhandled error without the printer** (issue #30's item
1). An error that reaches JavaScript (a `LispError`) or ends a program is
reported with its message and its type; they were `princ-to-string` and
`prin1-to-string`, which kept the printer, the pretty printer and its
dispatch table, `format`, and the reader (`%output-symbol` refers to
`*standard-readtable*`, whose functions are the reader's) in every module.
Now `%condition-message` and `%condition-type-string` (`runtime/cond.lisp`)
call them only when the program has them anyway, and otherwise a small
printer of their own: strings, symbols (with their packages and `|...|`),
numbers, characters, lists and vectors (eight levels, fifty elements),
structures as `#S(...)`, other objects `#<TYPE {id}>`, and a `format` of
the directives that messages use (`~A ~S ~D ~B ~O ~X ~C ~% ~& ~~ ~P ~* ~?
~^`, `~[...~]`, `~(...~)`, `~{...~}`; one it does not know is written as it
is). Whether the program has them is told by the plist of `PRINT-OBJECT`,
to which `print.lisp` gives the full printer's functions (`%full-message`,
`%full-prin1`, `%full-format`): a shaken program keeps a symbol's plist only
if it refers to the symbol, as the printer does (`*print-object-hook*`) and
a program that defines methods of `print-object`; `%condition-message` reads
it through `sys:%weak-symbol`, a reference to a symbol that `shake-module`
and the heap do not follow (`(:global.get (:sym S) :weak)`). The reports of
the standard conditions are `%condition-message` properties (a function of
the condition returning a control and its arguments) rather than report
functions that called `format`, so that both printers use them;
`define-condition`'s `:report` is as before, and a program's report
function is called by either. `warn` writes the message the same way, and
the reports of the runtime's restarts use `%message-format`.

What else kept the printer in the module of one export: the methods of
`print-object` and `describe-object` (kept by the tables of the functions
of all methods, `*specializer-methods*` and `*generic-function-infos*`,
which every module has: `%preinit-save` takes them out of those, so that
they stay with their generic functions, which only the printer refers to;
`*class-finalization-hook*` empties their caches through their names'
plists; the dispatch then finds their functions in the methods); the print
functions of the structures `thread` and `readtable` (now methods of
`print-object`); the interactive functions of the runtime's `use-value` and
`store-value` restarts, which read and evaluate a form (now called by
`invoke-restart-interactively`, the restarts naming them by
`%weak-symbol`); `%gf-arg-error` (`format nil`); the version of a logical
pathname (`princ-to-string`); the stream functions that write to a pretty
stream, which reached its line breaking (`%maybe-output`, now called
through the stream's `output` slot, which the pretty printer fills when it
makes the stream); and `eval_string`, whose reader, evaluator and printer a
shaken module now has only when it is compiled with `--eval-string`
(without it `eval_string` returns `; Error: this module has no
evaluator...`; a module without `--shake` has them as before). The messages
are those of the full printer: `tests/lib/messages.lisp`, compiled with
`--shake`, must print `tests/lib/messages.expected` (`make test-lib`). The
module of one export: 3,628,248 -> 2,469,838 bytes (`gzip -9` 920,048 ->
618,266): of code, `pprint.lisp` 204 -> 28 KB, `format.lisp` 199 -> 0,
`read.lisp` 103 -> 1, `print.lisp` 88 -> 16, `loop.lisp` 48 -> 0,
`clos.lisp` 535 -> 397; the small printer is 30 KB, which a program that prints carries too (`tests/hello.lisp` with `--shake` 3,782,300 -> 3,812,977; the REPL image 9,723,973 -> 9,776,337). What keeps most of the
rest is CLOS: the tables of all methods' functions and
`*generic-function-infos*` keep every generic function and method (item 2),
and through the method combinations the evaluator.

**Methods of classes that cannot have instances** (issue #30's item 2).
A method is kept with its generic function, and `*method-fast-functions*`
(and the tables like it) hold every method's functions: the methods of Gray
streams and of the streams of subprocesses were kept in every shaken
module, with what their code reaches. When the heap is saved,
`%preinit-drop-methods` (`runtime/image.lisp`) removes the methods that
specialize a parameter on one of a set of classes or a subclass
(`%remove-method`, then the entries of the tables of methods no generic
function has, those a definition replaced among them, and the classes'
caches of dispatch); a generic function left without methods gets the
function of the dispatch that one copy serves (`%make-list-gf-function`)
instead of its own. The set is first the classes of metaclass
`standard-class` that some method specializes, of which no metaobject (a
class, slot definition, generic function, method or method combination) is
an instance, and that are not conditions. The module is then shaken, and
`ph-instantiable-classes` (`src/preinit.lisp`, called by `shake-module`'s
`:verify` before it changes the module) checks that none of them, nor a
subclass, can have instances: an instance of a standard class is made by
`make-instance`, `allocate-instance`, `change-class` or `make-condition` of
the class or its name, so it can when kept code or a constant names the
class (as a `defclass` of the program names its superclasses), when a kept
object is an instance of it, or when a kept object or global refers to the
class or its name other than as the class system does (its name,
superclasses, subclasses and precedence list, the `%class` property of its
name, the cell that compiled `typep` tests read, `*specializer-methods*`).
If some can, the heap is made again with their methods kept (one more run
of the toplevel forms and shake, about 1.5 s: a program that defines a Gray
stream). As for functions, a class that only a name made at run time names
(`intern`, `read`, `eval_string` of a new `defclass`) is not there for it:
its inherited methods may be gone. The module of one export: 3,628,248 ->
3,489,581 bytes (`gzip -9` 920,048 -> 894,729): code 2.75 -> 2.61 MB
(`process.lisp` 67 -> 0.5 KB, `gray.lisp` 72 -> 17 KB); the methods of the
Gray streams and of the streams of subprocesses are left out, and the
generic functions of Gray streams have no dispatch of their own
(`tests/hello.lisp` with `--shake`: 3,782,300 -> 3,635,773). The REPL image
carries the 6.6 KB of `%preinit-drop-methods`. What
`*method-fast-functions*` still holds (160 KB of code) is the methods of
classes that have instances (the metaobjects, conditions, streams): their
generic functions are kept by the printer and `make-instance`.

The three together, on the symbol and string tables of data segments: the
start function of a module made from the heap fills the symbols and strings
first (`:init-symbols`, `:init-strings`); `:symtab`, `:pkgtab` and
`:strtab` keep their symbols and strings, not what they name; with
`--shake-symbols` an object of the heap that refers to a symbol keeps the
symbol's global, a symbol that only a package's table refers to is left out
of the heap and of the vector of the package's symbols (the symbols that
shadow others stay), and `%image-start` sets `*%dropped-symbols*`. The
module of one export: 2,034,879 bytes (`gzip -9` 571,412), 2,037,301 with
`--shake-symbols` (558,841); main before them 4,107,539 (979,112).

**What the dispatch knows of a method is in the method.** The function of
a method's spread arguments, the maker of one from its next method, the
maker of its function of keyed arguments, the value of a constant body,
the slot of a reader or writer that `defclass` made and the combiner of a
qualified method were in six hash tables keyed by the method
(`*method-fast-functions*`...), which the dispatch reads when it fills a
cache: any call of a generic function kept the tables, and so the
functions of every method of every generic function. They are now a
vector in a slot of the method, `%extra` (`%method-extra`,
`runtime/clos.lisp`), kept with the method, which its generic function
keeps, which its name keeps. Nothing else changes (a lookup is a slot
read instead of a `gethash`). The module of one export: 2,041,819 ->
1,801,118 bytes (`gzip -9` 575,060 -> 522,771): `clos.lisp` 378 -> 258 KB
of code, `class0.lisp` 25 -> 12, `gray.lisp` 16 -> 6.

**The update of obsolete instances only after a redefinition.** Every
access to a slot of an instance that may be obsolete
(`%ensure-current-instance`: `make-condition` does it) kept the update of
an instance of a redefined class, its error and
`update-instance-for-redefined-class` with its methods. The update is now
called through `*obsolete-instance-updater*`, which `%note-obsolete-layout`
sets when a class is first redefined: a program that redefines no class
does not carry it. The module of one export: 1,801,118 -> 1,755,743 bytes
(`gzip -9` 522,771 -> 511,904).

**The small printer names characters without the file of Unicode names.**
`char-name` of a character above Latin-1 reads `data/unicode-names.txt`
from the Clysm installation (`%unicode-names`), so the small printer's
`#\` and `~:C` kept pathnames, `open`, `close` and `read-line` in every
shaken module. It now calls `char-name` only when the program has it
(`%small-char-name`, through `%weak-symbol`), and otherwise writes the
names of the first 256 characters and `U+XXXX` (as `char-name` does when
the file is not there). `tests/lib/messages.lisp` prints named characters.
The module of one export: 1,755,743 -> 1,732,962 bytes (`gzip -9` 511,904
-> 504,757).

**Names in the class system's data are not calls.** A symbol that a kept
object of the heap refers to keeps what it names, its function too; the
classes refer to many symbols as names: of slots (a file stream's slot
`pathname`, the slot `close` of a directive of `format`, a pathname's
`directory`, `count`...), of classes, of accessors, and in the initforms
and types of slots, whose functions the class has compiled. Those kept the
functions `pathname`, `close` (with its generic function and methods),
`directory` and `count` in every shaken module. `ph-class-data-objects`
(`src/preinit.lisp`) finds that data from the class `t` through the
subclasses: each class's slot vector, the lists of its superclasses,
subclasses, precedence list, slots and slot locations, its slot
definitions (their slot vectors, initforms, types, initargs, readers and
writers; a structure's slots are lists), the forms of its default
initargs, and the properties `%struct-description` and `%struct-slots`
of a structure's name (`*preinit-data-properties*`). A symbol such an
object refers to is kept with its value and properties but not its
function (`ph-mark-data-symbol`; its record is written without the
function), unless something else refers to it; a constant global that it
refers to is marked `:data-global`, and `shake-module` walks that global's
initializer with `*shake-data-context*`, its symbols then passed to the
heap's hook as `:data-symbol`, until something else refers to the global
(`src/wasm.lisp`). As for a function called by a name made at run time, a
function that only the name of a slot or class names is not there for
`(funcall (slot-definition-name ...))`. `--shake-why` names the class of
an instance of the heap, and the instance's name when it has one
(`<STRUCTURE-CLASS PATHNAME>`, `<STANDARD-GENERIC-FUNCTION CLOSE>`).
The module of one export: 1,732,962 -> 1,605,008 bytes (`gzip -9` 504,757
-> 466,857).

**Generic functions without the dispatch.** The generic functions that
`%preinit-drop-methods` leaves without methods (those of Gray streams,
which the stream functions call for a Gray stream) had the shared function
of the dispatch, `%make-list-gf-function`, and so `%gf-invoke`, the
filling of caches and the making of effective methods (`%compute-fast-emf`,
the method combinations, and through them the evaluator) were in every
module whose stream functions were. The runtime's generic functions with
one primary method that specializes nothing (`slot-missing`,
`slot-unbound`, `no-applicable-method`, `no-next-method`...) were called
through the dispatch too. Both now get `%make-empty-gf-function`
(`runtime/clos.lisp`): a call signals that no method applies, or calls the
one method's function, unless `*empty-gf-dispatch*` is set, which
`%add-method-unlocked` does (to `%gf-invoke`) when a method is first
added, and `%preinit-drop-methods` clears after the runtime's own; such a
method's generic function is the new function. A program that adds no
method and calls only such generic functions has no dispatch. The update
of the instances tracked by `track-instances` is likewise called through
`*live-instances-updater*`, which `track-instances` sets. The small printer
writes a generic function as its `print-object` does
(`#<STANDARD-GENERIC-FUNCTION NAME>`). `tests/lib/nodispatch.lisp` is such
a library. The module of one export: 1,605,008 -> 1,353,620 bytes (`gzip
-9` 466,857 -> 408,606): code 1.18 -> 0.95 MB.

Two names the runtime refers to as data: the operation of the
`floating-point-overflow` of `%single-overflow`, `'coerce`, kept `coerce`
and with it `%coerce-to-function` and the evaluator (now `(%weak-symbol
coerce)`), and the keyword constructor of a structure (the property
`%struct-constructor`, which only `#S` reads: now kept only with
`%sharp-s`, `*preinit-property-users*`; `tests/cases/sharp-s.lisp`). The
module of one export: 1,353,620 -> 1,290,161 bytes (`gzip -9` 408,606 ->
390,452). `CLYSM_SHAKE_WHY_LARGEST=N` makes `--shake-why` list the N
nodes that dominate the most (150), and `CLYSM_PREINIT_STATS=1` also
lists the kept generic functions and instances by class.

**Branch hints of the runtime's large functions.** In a shaken module the
functions of the runtime whose bodies are larger than 800 octets get no
branch hints (`*runtime-hint-limit*`, `large-runtime-function-p`,
`src/wasm.lisp`, bound by `compile-program` with `--shake`): four fifths
of the runtime's hints are in them (the generic sequence functions, the
reader, `format`). Measured on whole programs compiled with `--shake`,
five interleaved runs on one slot, time without them / with them:
`bench/suite.lisp` 1.007, `bench/library.lisp` 1.007, `bench/programs.lisp`
0.994, `bench/clos.lisp` 1.002, `bench/gabriel.lisp` 0.988. Without any
hints in the runtime's functions `bench/suite.lisp` was 1.018-1.024
(`b-sieve` 1.23-1.31: the small functions that programs call in their
loops, as `aref` of a vector of unknown type, need theirs), so the small
functions keep them. The module of one export: 1,290,161 -> 1,266,522
bytes (`gzip -9` 390,452 -> 377,083): branch hints 29,969 -> 6,331.

**Names of slots and classes in the code.** A quoted symbol in code keeps
what the symbol names, its function too, as `(funcall 'f)` needs; a
symbol that code uses as the name of a slot or a structure's class is not
called. A slot access by a constant name (`slot-value`, `(setf
slot-value)`, `slot-boundp` through `%cached-slot-access`, and the
runtime's `%condition-slot`) now refers to the name by `%weak-symbol`, and
`(load-time-value (find-class 'name))` of the accessors and predicates of
structures (`cg-ltv`, `src/codegen.lisp`) too: the class keeps its name
and its properties (`ph-mark-data-symbol`). The report of `file-error`
read the slot `pathname`, and `pathnamep` the class `pathname`: they kept
the parser of namestrings with `position`, `remove` and `substitute`.
`equal` of two pathnames now compares their components, as CLHS says,
instead of their namestrings, and `sxhash` hashes the components: neither
writes namestrings. The module of one export: 1,266,522 -> 1,206,445
bytes (`gzip -9` 377,083 -> 359,909).

**No typed copies of the runtime's large workers in a shaken module.** The
typed (spec and raw) copies and entry versions of functions were a fourth
of the code of the module of one export, most of them of the workers of
the sequence functions (`%remove`, `%count-matches`, `%member`,
`%replace`: a copy for each class of item or bounds a caller gives,
10 KB each). With `--shake`, a function of the runtime with five required
parameters or more and more than 300 nodes gets none
(`*runtime-copy-limit*`, `spec-max-nodes`, `src/codegen.lisp`, bound by
`compile-program`). Measured as the branch hints above (time without /
with): `bench/suite.lisp` 1.010-1.014, `bench/library.lisp` 1.010,
`bench/programs.lisp` 0.997, `bench/gabriel.lisp` 0.982, `bench/clos.lisp`
0.997. (`b-sieve` takes 42 or 52 ms from one run to the next on the same
module, and 52 with this, whose code for it is the same instruction for
instruction: not a change of its code.) Without the limit on the number of
parameters (all runtime functions over 300 nodes) the module was 38 KB
smaller still but `b-sieve` and `g-dderiv` 1.2-1.3 slower. The module of
one export: 1,206,445 -> 1,085,879 bytes (`gzip -9` 359,909 -> 331,662).

Where the module of one export is after these (1,085,879 bytes, `gzip -9`
331,662; `tests/lib/one.max-bytes` 1,120,000): code 720 KB (199 KB with
gzip), the data segments 226 KB (83 KB: the heap, 127 KB of it, and the
names of the 5,179 symbols), globals 124 KB (38 KB), branch hints 6 KB.
What keeps the most code: `subtypep` (68 KB, from the upgrading of an
unusual element type in `make-array` and `make-string`, the general paths
of `concatenate`, `coerce` and `map`, and condition types that are
compound), `%find-position` (52 KB, one function: `find-if` and the
fallbacks of `position` for sequences that are not simple), the small
printer (`%small-format` 43 KB, `%small-print` 24 KB), the loader of the
heap (`:start` 37 KB), `typep` of a type known only at run time (23 KB),
the generic arithmetic (`:mul`, `:div`, `:divmod`, `expt`: 60 KB), the
stream functions (27 KB) and `equalp` (12 KB). The heap's largest part is
the class system: 1,024 slot definitions of 127 classes.

**`subtypep` only when the program has it.** The upgrading of an element
type that `%normalize-element-type` does not know by its form
(`%upgrade-element-type`), the general sequence types of `concatenate`,
`coerce`, `map` and `make-sequence` (`%seq-type-spec-general`), the
binary element types of `open` (`%binary-element-spec`) and the `and` of
condition types (`%condition-class-for-type`) called `subtypep`, which
kept it (68 KB of code with what it calls) in every module that makes an
array. They now call it through `%weak-subtypep` (`%weak-symbol`): with
it, as before; in a shaken program that does not refer to `subtypep`, by
the type's form (`%small-upgrade-element-type`: a `deftype` by its
expansion, `member`/`eql` by the types of the items, `or` when its
members upgrade alike, `and` by a member that is specialized, otherwise
`t`; a sequence type `and` by its first member that is a sequence type, a
class by its superclasses; a class among condition types by
`%subclassp`). An upgraded type is then a type that holds the elements
but may be `t` where `subtypep` would tell a narrower one.
`tests/lib/nodispatch.lisp` makes such arrays and sequences without
`subtypep`, `tests/cases/element-types.lisp` with it. The module of one
export: 1,085,879 -> 1,026,823 bytes (`gzip -9` 331,662 -> 316,060).

**The small printer searches its strings itself.** `position`,
`position-if`, `position-if-not` and `find` of the small printer (the
directives of a control string, the characters that need `|...|`) were
calls of the generic functions, whose general case is `%find-position`
(46 KB: a loop for each kind of sequence, test and key). They are now
`%small-position` and `%small-position-if`, loops over a string; the
small upgrader of element types has its own loops too. `%find-position`
stays as it was for the programs that call the sequence functions (a
smaller general case would make their calls on vectors with fill pointers,
lists with `:key` and the like slower). The module of one export:
1,026,823 -> 961,571 bytes (`gzip -9` 316,060 -> 303,707).

The decimal digits of a bignum split it by a power of ten made by
squaring (`%integer-power`) rather than by `expt`, whose cases for every
kind of number (23 KB) the printer of integers then does not keep, and
the small upgrader of element types drops the `nil`s of an `or` itself
instead of with `remove`. The module of one export: 961,571 -> 940,564
bytes (`gzip -9` 303,707 -> 298,877), below issue #30's 0.3 MB.

The REPL image grew by 54 KB with these changes (9,004,262 -> 9,058,442
bytes): `%set-method-extra` was copied into the toplevel form of each
`defmethod` that notes something of its method (20 KB), and
`%pathname-equal` got a typed copy for each typed copy of `equal` that
calls it (8 KB). Both are now `notinline`: 9,044,220 bytes (+40 KB: the
new functions, the small upgrader and its callers' second ways, the
shake's limits in the compiler).

`--shake-why REPORT` (with `compile` or `library`; it implies `--shake`)
writes what keeps the code: its size by source file; the functions, globals
and symbols that the most code would go with, which is what each dominates
in the graph of references from the roots (the exports, the start function,
the globals kept whatever refers to them); and each kept function, largest
first, with the first chain of references that reached it. `make test-lib`
compiles the module of one export (`tests/lib/one.lisp`) and fails when it
is larger than `tests/lib/one.max-bytes`, lowered as the runtime a library
carries is made smaller (issue #30); it also compiles `tests/lib/clos.lisp`
shaken, whose classes and Gray stream must still dispatch to the methods
they inherit from the runtime's.

`tools/prof.sh FILE` (and `tools/prof-script.sh` for a file loaded into the
image) profile a program with the V8 CPU profiler; `tools/dump-func.lisp FILE
NAME...` prints the Wasm generated for functions. For code compiled in the
image (a fasl, the REPL, `load`), `(setq clysm::*live-dump* '("NAME" ...))`
prints the Wasm of the functions of those names to `*trace-output*` as they
are compiled (`compile-file` the file again to see them). With
`CLYSM_EAGER_VALIDATION=1` the host leaves the engine's lazy validation
off, so that a module with a function that does not validate is refused
when it is made, not when that function first runs (after a change of the
code generator: the image, the fasls of some systems and the tests loaded
with it).

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
* **Inlining (measured, not kept).** `--wasm-inlining-factor=5` (V8's default 3): TurboFan
  inlines into a function until its graph is that many times the function's
  own size (and at most 7000 nodes), taking the calls in the order of the
  counts the baseline code collected when the function tiered up, so the
  inlining of a short caller of several small functions depends on when it
  tiered up. `g-dderiv` (DDERIV: GET and GETF inlined twice, or F* inlined
  into F+ instead) ran 135-140 ms in most processes and 120 ms in one of ten
  to forty, the same module; with 5, 111-115 ms every time. The benchmark
  files with and without it, best of five interleaved: gabriel 0.97,
  functional 0.96, vectors 0.98, declared, suite, control, alloc, arith,
  closures 0.99, programs, clos, arrays, library 1.00 (`c-make-instance`
  1.07, `p-nonlocal` 1.05 the most slower); in the image (`clysm script`)
  suite 0.99, gabriel 0.99, programs, library, functional 1.00; starting
  hello the same (set in the worker, see above; on the command line it
  costs the 35 ms of Node.js's code cache). But on real programs it is not
  neutral: Lem (`tools/bench-lem.sh`, factor 5 against 3, three and four
  rounds) 1.018 and 1.008 (`l-forms-forward` 1.03-1.04 every round,
  `l-type-redisplay` 1.06-1.14), with 4 1.011; `tools/bench-libs.sh` 1.005,
  `bench/apps.lisp` 1.002. Not kept: the larger budget is spent where the
  short benchmarks gain and costs the large programs' tier-up. Two compiles of one source are
  byte-identical (sequential or parallel, after the fasls are made again);
  only `clysm.sources` names the file as the command line gave it.
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
  time. (Up to 256 bits then; 4096 since 2026-11-23, with `mod rem gcd lcm
  isqrt logcount`: see "Numeric items still behind SBCL".)

`clysm script` of a `defun` and a call 168 -> 135 ms, `s-quicklisp` 426 ->
397, `s-ql-*` 15 to 20 ms less; compiling small `defun`s warm is no slower.
Tried: leaving out the compiler macros of `define-compiler-macro` in the
compiler's code (`vector-push-extend`'s and others): 3% less code run, no
time measurable; no inlining at all in the compiler's code: 0.98 MB, no
time measurable on a busy machine. What remains is mostly the runtime's
functions (`format`'s directives, sequence functions, `typep`, the
`loop` parser...), whose inlining the benchmarks need, and the check of a
pending exit after each call (12 octets a call).

Tried and not kept: compilation hints in the `metadata.code.compilation_priority`
format (`withCompilationHints`) are not used by Node.js 24's V8 (13.6;
`--experimental-wasm-compilation-hints` changed nothing); its own older
format is (see Heap images, and below). V8 has no cache of compiled WebAssembly that Node.js
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

Later (2026-11-21), one slot, best of five, milliseconds ("main" is main
at ee61f08, "now" this work on top of it; the machine was busy, another
slot running Lem's benchmarks):

| Item | SBCL | Main | Now |
|---|---|---|---|
| `s-start` (`--eval t`) | 4 | 100 | 77 |
| `s-script` | 5 | 154 | 99 |
| `s-require-asdf` | 52 | 257 | 201 |
| `s-quicklisp` | 141 | 433 | 356 |
| `s-ql-alexandria` | 158 | 504 | 399 |
| `s-ql-cl-ppcre` | 181 | 510 | 408 |
| `s-ql-babel` | 237 | 592 | 479 |
| geometric mean / SBCL | | 5.94 | 4.56 |

(On a quieter machine `s-start` is 91 -> 64 ms and `s-script` 125 -> 79.)

* **The image as it starts from the heap image** (see Heap images): its
  constant lists, vectors and symbols made empty, compiling and
  instantiating the REPL image 43 -> 25 ms, and 4 MB less to allocate and
  copy in the young generation. `host/run.mjs` reads the heap image first
  and compiles the image's module from it (reading the header, then those
  octets, then the heap while it compiles), and does not read the image's
  9 MB file.
* **Compilation hints** that Node.js's V8 reads (`compilationHints`, see
  Heap images): the 1070 functions of the image that starting, compiling a
  `defun` and loading run are compiled by Liftoff in V8's threads while the
  image compiles, instead of one by one on the main thread (1.1 MB:
  `s-script` 120 -> 79 ms, `--eval t` 70 -> 64).
* **A module's toplevel forms** are compiled with it: each runs once when
  the module is loaded, so the compiler names them (`eager-funcs` of the
  module: `run`, the `run-seg`s, `unit-dispatch` and the main entries of
  the toplevel functions) in the module's `compilationHints` section, and
  V8 compiles them in its threads instead of one after the other when
  called (`new WebAssembly.Module` returns at once, 3 ms for ASDF's module,
  and they are compiled in the background after it): for ASDF's fasl
  280 of the 790 KB of code that loading it compiles; `(require :asdf)`
  199 -> 185 ms, Quicklisp and Babel 466 -> 445 (a busy machine: within
  its noise).

  Tried and not kept (2026-11-21): naming also the other functions that run
  while a fasl loads. The first load of a fasl ran its module instrumented
  (`instrumentWasm`), and when the file had loaded the host wrote the
  functions that ran into the hints of the module in the file, in place (a
  section `clysm.train` of one octet marked it trained): ASDF's module
  then names 2408 of its 11542 functions. Loading was no faster
  (`(require :asdf)` 170/204 -> 181/200 ms, min/median, interleaved; the
  same CPU time, 610 ms; Quicklisp with Alexandria 348/383 -> 354/408, with
  Babel 448/493 -> 470/508): the background threads compile the functions in
  the order of their indexes, while the main thread calls them in the order
  the toplevel forms run, and compiles itself most that it reaches before
  them.
* **Host.** The places of the image's functions in its sources
  (`clysm.sources`, 3 MB of strings for 18000 functions) are read when a
  stack trace first needs them, and `node:child_process` (with much of
  Node.js's own JavaScript) when a process is first started.

Tried and not kept (2026-11-21): the young generation growing faster at
start (`--semi-space-growth-factor`, which can be set at run time; the
start allocates about 10 MB that all live on, and copies them through six
scavenges, 10 ms): 16 makes `--eval t` 10 ms faster but loading ASDF 10 ms
slower and Quicklisp no faster (larger scavenges, and a full collection),
4 is between. The image's objects from a global's initializer (V8
evaluates it when instantiating) instead of `:image-objects`, a function
of 31000 `global.get`s that Liftoff compiles in 2 ms into 1 MB of code: no
gain. A larger training set for the hints (loading Quicklisp and three
libraries, 1850 functions; or compiling and loading a file of a class, a
generic function and a structure, 1580): the module's compilation waits
for them, `--eval t` 3 to 9 ms slower and loading no faster.

What remains of `--eval t` (64 ms on a quiet slot): Node.js and its ESM
loader 15 ms, reading the heap image (10.7 MB) and compiling the image 17
(the main thread waits for it 9), instantiating it 12 (its 7500 static
closures, 31000 globals, and V8 making a reference for each of the 17000
functions in the table), making the heap's objects 8, the program 8 (the
host variables, the REPL), and the collections of the young generation
(10 ms over all of it).

**The floor, and what is above it (2026-11-22).** Whole processes on one
slot, 40 runs of each one after the other in every round (`tools/startup-ab.mjs`
does this for the items of `tools/bench-startup.sh` and several builds or
settings: on a busy machine a process's start differs by 10-20% from run
to run, more than most of what is measured here), least / median
milliseconds:

| Process | least | median |
|---|---|---|
| `/bin/true` | 1.0 | 1.7 |
| `sbcl --non-interactive --eval t` | 2.5 | 3.1 |
| `node -e 0` | 12.2 | 13.6 |
| `node` of an empty CommonJS file | 10.3 | 13.2 |
| `node` of an empty ES module | 11.2 | 13.7 |
| ... that imports `node:fs`, `node:worker_threads`, `node:url` | 13.4 | 15.6 |
| ... that compiles and instantiates an empty WebAssembly module | 11.1 | 13.5 |
| `--eval t` from the heap image (main, e5669e1) | 64.8 | 69.8 |
| `--eval t` without the heap image | 156 | 173 |

Node.js itself is four times SBCL's whole start: no item can go below 11
to 12 ms, which is 4 times SBCL for `s-start` and 3 times for `s-script`
whatever Clysm does (with those two at the floor and the others as they
are the geometric mean would be 2.6). `CLYSM_TRACE_START=1` makes
`host/run.mjs` print the times of its steps (milliseconds since the
process started) when it exits. Main, `--eval t`:

| Step | at | took |
|---|---|---|
| Node.js until `host/run.mjs` runs (11 for an empty module; `node:worker_threads` is 1.0 of the rest, `node:v8` 0.3) | 13.5 | 13.5 |
| the main thread chosen (`node:v8`, the heap image is there) | 15 | 1.4 |
| the engine's flags | 15.2 | 0.2 |
| the image's octets read from the heap image (8.4 MB) | 17.4 | 2.1 |
| `WebAssembly.compile` called (it copies the octets) | 19.4 | 1.9 |
| the heap read (2.6 MB), `host/common.mjs`, `host/lib.mjs` imported, the imports made | 23 | 3.6 |
| waiting for the module (V8's threads: 12 ms from the call) | 31 | 7 to 8 |
| instantiated | 42 to 47 | 10.5 to 16 |
| the heap image's objects made (`image_load` 12.5) | 55 to 63 | 13.5 to 16 |
| the program | 56 to 65 | 1.2 |
| the exit event; then Node.js's teardown | | 5 |

and inside those:

* **Collections**: six scavenges, 10.8 ms (`--trace-gc`), four of them
  while the module is instantiated and the heap image loaded. Everything
  made lives on (4.1 MB by instantiating, 5.6 MB by `image_load`), the
  young generation starts at 1 MB and doubles, and what survives one
  scavenge stays in the young generation until the next: every object is
  copied twice.
* **Compiling** (the image as it starts from the heap image, 8.4 MB, in
  one process again and again): 12.3 ms; 5.6 without its
  `metadata.code.branch_hint` section (273 KB: V8 makes a table of the
  hints of each function while it decodes the module), no different without
  the name section or the `compilationHints`.
* **Instantiating**: 6.4 ms warm, all of it the 34439 globals (without the
  element segment of 18404 functions 6.0, without the data segments the
  same). V8 runs its decoder of functions over each initializer that is
  not a single constant; modules of 8000 globals of one kind, per global:
  an `i32.const`, a `ref.null` or a `ref.func` 12 ns to instantiate and 27
  to compile; `struct.new_default` of a cons 93 and 71; a static closure
  (seven `ref.func`, two `struct.new`) 320 and 230. The image has 18980
  default structs (the symbols and the conses that the heap image fills),
  8029 static closures, 2032 vectors, 5174 mutable nulls.
* **The heap image's objects**: 29257 of the globals and 96560 others
  (81815 conses, 8589 strings, 1961 vectors, 1641 instances, 1360 bignums,
  619 structures, 266 closures); `image_load` takes 2.1 ms when run again
  and again in one process and 12.5 the first time: three of the scavenges
  (7 ms); the rest was not found (compiled by TurboFan before it runs, by a
  compilation hint, below, it is no faster).
* **After the program**: the engine's tasks for the main thread run as
  soon as the program returns to the event loop, before a timer or
  `setImmediate` can: after `(require :asdf)` they are the steps of a
  collection of the old generation that began while ASDF loaded (the old
  generation at 20 MB: `--trace-incremental-marking` shows 157 steps and
  the mark-compact, 8 to 12 ms every time; 0.3 ms after `--eval t`).
  Then, the event loop empty, Node.js frees the environment and the heap
  (5 ms). And `process.stdin.isTTY`, which `host/run.mjs` asked when it
  started, makes the stream of the standard input: 1.7 ms.

What was done (main e5669e1 against this work, and this work with
`CLYSM_SLOW_EXIT=1`: 20 rounds of all items, one process of each after the
other, on one slot; least / median ms):

| Item | SBCL | Main | Now | Now, the end left to Node.js |
|---|---|---|---|---|
| `s-start` (`--eval t`) | 2.8 / 3.4 | 66.1 / 73.3 | 60.6 / 67.8 | 67.6 / 73.1 |
| `s-script` | 4.0 / 4.4 | 78.2 / 85.3 | 72.7 / 85.1 | 77.9 / 84.8 |
| `s-require-asdf` | 48.1 / 50.3 | 181.2 / 200.6 | 173.1 / 188.2 | 183.1 / 199.2 |
| `s-quicklisp` | 121.4 / 131.9 | 293.8 / 322.4 | 285.0 / 304.6 | 291.3 / 311.6 |
| `s-ql-alexandria` | 136.0 / 145.4 | 327.4 / 350.7 | 314.2 / 335.3 | 316.5 / 337.5 |
| `s-ql-cl-ppcre` | 167.3 / 174.3 | 336.3 / 375.5 | 328.2 / 352.1 | 335.5 / 360.4 |
| `s-ql-babel` | 211.7 / 223.7 | 399.3 / 421.3 | 372.7 / 399.3 | 372.5 / 406.2 |
| geometric mean / SBCL | | 4.52 / 4.54 | 4.29 / 4.31 | 4.47 / 4.43 |

(Another 20 rounds just before: 4.45 / 4.47 -> 4.30 / 4.27.
`tools/bench-startup.sh`, best of seven whole milliseconds: 4.38 -> 4.28,
with SBCL's `s-script` 5 ms in the one and 4 in the other. The machine was
busy with other work all day: each of these differs from the next by more
than a single change does, and only runs interleaved in one invocation
compare.) What runs long is as it was, main against this in two rounds
one after the other on one slot: `bench/suite.lisp` in the image
(`MODE=script`) 0.998 of its time, `bench/library.lisp` 1.011,
`tools/bench-lem.sh` 1.033 (against SBCL 1.01 and 1.07 for main, 1.04 and
1.05 for this: its rounds differ more than the builds, and none of the
code that these run changed).

* **The end of the process** (`host/run.mjs`, in the main thread; a
  program in a worker, `clysm run` or `exec`, ends as before). When the
  exit event has run its listeners the process ends at once
  (`process.reallyExit`, as `process.exit()` does; not when a listener was
  added after the host's): the 5 ms of teardown. And when the program's
  main function has returned (`%call-as-main-thread` counts its calls,
  nested in an image started from a heap image, and tells the host when
  the outermost returns: `%main-done`, `__clysm_main_done`), the host
  exits if nothing is left that would run later. It looks when the promise
  jobs queued have run, and those they queue: in a function of
  `process.nextTick`, which Node.js calls when the queue of jobs is empty
  and before it goes on to the engine's tasks (twice, as a job may ask for
  a tick itself); a `setImmediate` would run after the collection. Nothing
  is left when nothing keeps the event loop alive
  (`process.getActiveResourcesInfo()`: a timer, a socket, a file being
  read, a process, an immediate other than the host's own flush of the
  output), no `beforeExit` listener is there, and the engine owes the
  program no promise that the event loop does not know of (the host counts
  the pending `WebAssembly.compile`, `instantiate` and `Atomics.waitAsync`).
  A program that leaves a timer, a promise job, a chain of 200 `await`s, a
  tick that makes a job that sets a timer, or a module being compiled with
  a function to call then, runs them as before (tried so). Whether the
  input is a terminal is asked for when the REPL first needs it, with
  `tty.isatty`, which makes no stream: a pipe's or a terminal's handle
  would be among the active resources at the end.
  `CLYSM_SLOW_EXIT=1` leaves the end to Node.js. (`--no-incremental-marking-task`
  set when the program ends does not stop the steps already queued.)
* **`probe-file`, `truename`, `file-write-date`** asked Node.js three and
  two times each (`existsSync`, `realpathSync`, `statSync`), and
  `fs.realpathSync` is written in JavaScript and looks at every directory
  of the path: 22 µs for a fasl in the cache when warm, about 60 in a
  process that has just started (on a busy machine). ASDF takes the true name of a file every time it
  looks up the file's date (`normalize-namestring` in `get-file-stamp`, the
  key of its cache), the output of every file of a system fasl is the
  system's fasl, and `contrib/asdf-hooks.lisp` probed each source file
  before taking its date: loading Quicklisp and Babel made 572 calls of
  `realpathSync` (33 ms on a busy machine; 87 of them for Babel's fasl),
  1018 of `existsSync` and 1060 of `statSync`. Now one call of the host
  each (`__clysm_fs_probe`, `__clysm_fs_mtime` in `host/common.mjs`: `stat`
  without an exception for a file that is not there, and the system's
  `realpath`, `fs.realpathSync.native`, 6 µs; a host whose file system is
  not Node.js's, a page or `CLYSM_TRACE_FILES`, is asked as before), and
  the system fasl's checks take the date only: 466 `realpath`s of 6 µs,
  1120 `stat`s of 1 µs, 10 `existsSync`.
* **`do-symbols`** asked `find-symbol` for the name of every external
  symbol of every package used (the name hashed again, the tables of the
  package and of all it uses looked up); a symbol is inherited unless one
  of its name is present, which two lookups with the hash the symbol
  already has tell (`%package-symbol-list`): 300 times over a package that
  uses COMMON-LISP and two others 24 -> 10 ms (SBCL 2: it makes no list).
  `uiop:define-package` iterates over the symbols of every package it
  defines (about 40 when ASDF loads): its `ensure-package` is 36 of the
  59 ms of Lisp code in a profile of `(require :asdf)`, the largest part
  of the load. `find-symbol`, `gethash` of names in an `equal` table and
  `do-external-symbols` take SBCL's time once warm (21 / 22, 40 to 51 /
  36 and 6 / 6 ms in the same test), but here they run in Liftoff's code;
  loading ASDF's fasl a second and a third time in one process takes 72
  and 90 ms (SBCL: 43).

Measured and not kept (2026-11-22):

* **No branch hints in the image's large functions**, as in a shaken
  module (`*runtime-hint-limit*`, see Size and start-up). The image's
  section has 65249 hints in 6277 functions (273 KB; 3157 of them in
  functions of 200 octets or less, 11604 up to 800, 27420 up to 3000,
  23068 in larger ones; 7768 in the 256 toplevel functions, which a heap
  image never runs), and V8 puts each into a hash table of its function
  as it decodes the module, before the image can start. Without those of
  the functions larger than 800 octets (66 KB) the image compiles in 8.3
  ms instead of 12.3 (300 octets: 7.0; none: 5.6) and `--eval t` is 3.6 ms
  faster. But `bench/suite.lisp` in the image (`MODE=script`, main against
  it, two rounds interleaved) is 1.014 of its time, the benchmarks of the
  runtime's large functions the most (`b-sort` 77 -> 83 ms, `b-print` 71 ->
  76, `b-string` 74 -> 79, `b-hash` 140 -> 146, `b-format` 104 -> 108; Lem
  0.985); and without those of the compiler's functions only (`src/`: 179
  KB with the limit of 800 octets, 150 KB with none), `compile-file` of
  three benchmark files, warm, best of six in each of three rounds: 1226,
  1159, 1113 ms with all the hints, 1235, 1218, 1197 with the limit, 1227,
  1210, 1137 with none. 2 to 5% of what runs long for 3 ms of every start:
  not kept. (Leaving out only the toplevel functions' would be 0.8 ms.)
* **The young generation larger at start**, again (see above, 2026-11-21),
  this time with the growth factor set to 16 only until the heap image is
  loaded and the engine's own again before the program runs: starting takes
  two scavenges (2.5 ms) instead of six, `--eval t` 1 to 3 ms and
  `s-script` 2 to 5 ms faster, but Quicklisp with Alexandria 13 ms slower
  in both the least and the median of 30 rounds (334 / 351 against 321 /
  338) and Babel 3 to 4: the 10 MB are still to be copied twice, in two
  scavenges of 5 ms when the program first fills 16 MB, and the young
  generation stays that large. On node's command line (which costs nothing
  now that the program runs in the main thread: `node --min-semi-space-size=16
  -e 0` takes the time of `node -e 0`): `--min-semi-space-size=16` `--eval t`
  65 -> 59 ms and `s-script` 77 -> 71, loading ASDF and Babel no faster;
  `--initial-old-space-size=256` 5 ms slower; `--minor-ms` 3 to 5 ms
  slower. No flag of V8 13.6 promotes what survives its first scavenge.
* **Node.js's compile cache** (`NODE_COMPILE_CACHE`) for the host's
  modules: 63.9 against 62.8 ms; the 112 KB of `host/*.mjs` are compiled
  lazily and little of them runs. **A start-up snapshot** of the host's
  JavaScript (`node --snapshot-blob`; `WebAssembly` is there again in the
  process started from it, so the host's state could be in one): a
  snapshot of a file that requires `node:fs` and `node:v8` and makes 2000
  closures is 6.5 MB and its main function runs 14.6 ms after the process
  starts, an empty CommonJS file at 10.3: reading the snapshot costs more
  than the 5 ms of importing the host's modules that it could save. A
  single executable (SEA) was not built: what it has for starting sooner
  is a code cache and such a snapshot.
* **Compilation hints of other kinds** (the octet of each function in the
  `compilationHints` section: the strategy, the first tier, the top tier).
  For the 1173 functions that the training names: 0x13 (lazy with the
  first tier, eager with the top one, Liftoff) is as 0x12; 0x03 (the
  default top tier) makes `s-script` 133 ms instead of 83; 0x23 (TurboFan
  as the top tier, compiled in the background from the start) `--eval t`
  550 ms. 0x23 or 0x22 for `:image-load` alone: TurboFan compiles it (5 ms,
  in a thread, before the image is instantiated) and loading the heap image
  takes the same 14 ms. 0x23 for the nine helpers of hash tables and of
  names that the profiles of loading show (`:ht-get`, `:ht-put`,
  `:ht-get-equal`, `:ht-put-equal`, `:ht-rem`, `:ht-region-index`,
  `:string-hash`, `:member-eq-loop`, `:package-ref`), so that they do not
  run in Liftoff's code until they tier up: all the items 0.978 / 0.969 of
  their time (least / median) in 15 rounds and 0.983 / 1.004 in 25, within
  what two runs of one build differ by; for 42 helpers 1.011 / 0.998. In
  the image `bench/suite.lisp` 1.009 and `bench/library.lisp` 1.000 with
  the nine (they are compiled without the counts of their calls).
* **`node:worker_threads` imported when a worker is first needed** would
  save 1 ms, but `isMainThread` comes from it (the worker could be told
  by an argument instead).

What remains of `--eval t` (58 to 61 ms on the same slot): Node.js and the
host's modules until the image's octets are being compiled 19 (the floor
is 11 to 12 for the whole process), the main thread then waiting for the
module 5 to 8 while it imports the rest, instantiating 10 to 11 and the
heap image's objects 13 (a third of both the six scavenges), the program
1, the end 0.5. Of what is above the floor, the globals' initializers
(about 9 ms to validate and to evaluate), the branch hints (6 to 7 ms of
the compilation, for part of which the main thread waits), the copying of
what survives (11 ms) and the 8.4 MB read and copied (4 ms) are the
engine's work on what the image is; fewer objects made at start would
lower them (not measured: those that only the compiler uses, made when it
is first used, which `s-script` and loading a source file then pay at
once). After the start, loading ASDF is its `define-package`s and Liftoff
compiling 0.8 MB of ASDF's code (0.28 of it in the background, its
toplevel functions) and 0.65 MB of the image's that the training did not
see (30 MB a second, on the main thread, where the functions are first
called).

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

The engine's defaults were kept (until 2026-11-22: the young generation's
limit, below); each alternative was measured against them
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

**The young generation's limit is 256 MB a semi-space** (2026-11-22; the
engine's is 64). What the measurements above did not try: `--minor-ms` and
the semi-space flags were given on the command line with the other
defaults, and a larger young generation only through
`--scavenger-max-new-space-capacity-mb=64` (128 MB). Measured this time,
with `--trace-gc` to split the collector from the program:

* `s-build-list` is all collection (30 scavenges, 800 of 920 ms: the
  program's own part is 100 ms); `s-ring` is 270 ms of collection in 1400,
  the rest the walk of lists whose conses the scavenger interleaved
  (above).
  SBCL here collects every 54 MB (`bytes-consed-between-gcs`), but its
  conses are half the size, so the same program fills V8's 64 MB twice as
  often with objects twice as large to copy, twice each.
* The size of the semi-spaces, for a compiled program (a worker:
  `resourceLimits.maxYoungGenerationSizeMb`, three semi-spaces) in
  `bench/alloc.lisp`, time against the default's (geometric mean, and the
  two items): 128 MB 1.21 of the time of 256 (`s-build-list` 551 against
  204 ms, `s-ring` 264 against 107), 512 MB 0.96 (`s-ring` 78), 1 GB no
  better than 512. With 256 MB the items that make garbage that dies young
  are the same when they run after the young generation has grown
  (the file with its `s-` items first: 0.99–1.03 each).
* The young generation grows only while what is in it survives (by the
  engine's factor of 2 when what survived since the last growth is more
  than its size): `bench/suite.lisp`, `gabriel`, `programs`, `library`,
  `control`, `holdout` are the same within 1% as compiled programs
  (0.993–1.006 with 256 MB, three rounds each) and in the image
  (`suite` 0.99, `gabriel` 0.99, `library` 1.00 of the time with 64 MB).
* **`--semi-space-growth-factor=4`** (it can be set while the program
  runs, in the main thread too) reaches the limit sooner: a new process
  builds the list in 243 instead of 387 ms and the ring in 178 instead of
  257. Not kept: a script that loads libraries then has a young
  generation of 256 MB where 64 would have done, and touching it for the
  first time is 100000 page faults (`bench/apps.lisp` in the image 1.10 of
  the time, `a-cl-json-decode` 87 → 108 ms, 820 instead of 400 MB
  resident; with the factor of 2 and the limit of 256 MB, 1.01 and 405 MB).
* **`--min-semi-space-size`** (16 MB: `clysm eval t` 65 → 61 ms, the two
  scavenges that copy the heap image's objects are not run) makes the same
  scripts slower (`bench/apps.lisp` 1.065, 555 MB resident) and was not
  kept; 8 MB does nothing for start-up.
* **`--initial-old-space-size=256`, `--initial-heap-size=512`** (no full
  collection while the heap is small): `bench/alloc.lisp` 1.00 and 1.05,
  `bench/suite.lisp` 1.01. Not kept.
* On the command line of `node`, `--max-semi-space-size` and
  `--min-semi-space-size` cost nothing at start-up (`clysm eval t` 60.7 and
  62.0 ms, best of 25, in a worker 88.2 and 87.4); `--minor-ms` costs 6 ms
  in the main thread and 26 ms in a worker.

So `host/run.mjs` gives a program's worker a young generation of three
semi-spaces of 256 MB, and `bin/clysm` (and `bin/clysm.mjs`) start `node`
with `--max-semi-space-size=256` for the REPL image, which runs in the
main thread, whose heap exists before `host/run.mjs` runs
(`tools/bench-lem.sh` and `tools/bench-startup.sh`, which start the image
themselves, do the same). `CLYSM_NURSERY_MB` names another size (64: the
engine's). What it costs: a program much of whose data survives is given
more memory (the self-hosted compiler compiling `bench/suite.lisp`: 1.2 GB
resident instead of 0.8, in the same time, 4.0–4.2 s), and its longest
scavenge is longer (that compilation: 197 ms instead of 59; building the
list: 110 instead of 57), since a scavenge copies what survived of a young
generation four times the size. Data that lives long is still copied
twice, so a program that keeps everything (the compiler) gains nothing;
data that dies before 256 MB more have been allocated is not copied at all.
Against main, interleaved in one slot through the tools, two or three rounds
(time against SBCL's; with the changes to the list functions below):
`bench/alloc.lisp` 1.36–1.39 → 0.96–0.98 (`s-build-list` 3.9 → 1.2, `s-ring`
9.3 → 1.7, `s-build-hash` 0.71–0.93 → 0.59–0.63, `s-build-strings`
1.02–1.12 → 0.90–0.93; in `clysm script` the same: 0.72 of the time),
`gabriel` 0.86–0.88 → 0.86–0.87, `suite` 0.71 → 0.70, `library` 0.74–0.75 →
0.73–0.74, `programs` 0.79 → 0.78–0.80, `control` 0.71–0.72 → 0.73,
`holdout` 0.76–0.77 → 0.76–0.77, `io` 0.70–0.71 → 0.70–0.72, Lem 1.03–1.09 →
1.05–1.08, `bench/apps.lisp` 1.21–1.23 → 1.21–1.23, start-up 4.10–4.15 →
4.10–4.21 (`clysm eval t` 78–79 ms in both, the machine being loaded),
`tools/bench-libs.sh` 1.07–1.08 → 1.09–1.11: the self-hosted compiler is
2–4% slower (`l-selfhost-compile` 4.82–4.91 → 5.00–5.01 s; alone, three
rounds, 3.93–4.19 → 4.02–4.22 s), the cost of 100000 page faults more.
With semi-spaces of 128 MB it is not slower and takes 0.92 GB, and the
ring is 264 ms instead of 107 (780 with the engine's 64 MB), the list 551
instead of 204 (684): the choice for a machine short of memory.

**The growth past 64 MB is earned** (2026-11-23, compiled programs). The
engine grows the young generation when what survived since it last grew is
more than its size, however little of each scavenge that is: the
self-hosted compiler, whose scavenges of 64 MB keep 7–30%, ended with
semi-spaces of 256 MB that saved it no copying (what it keeps lives long:
the part kept does not fall when the space doubles, 0.05–0.27 at 128 MB,
0.09–0.30 at 256) and cost 0.4 GB, 100000 page faults and scavenges of 197
ms. Growing pays when what a scavenge keeps dies soon after (the ring:
0.48 kept at 64 MB, 0.24 at 128, 0.14 at 256) or when it keeps nearly all.
`nurseryControl` in `host/run.mjs` decides that from the scavenges
themselves: `v8.GCProfiler` records the heap before and after each
collection, and for each scavenge of semi-spaces of 64 MB or more the part
kept is what is left in the young generation and what the old one gained,
over what there was. `--semi-space-growth-factor`, which the engine reads
at each growth and which can be set while the program runs, is 1 (nothing
grows; on the command line the engine makes it 2) or 2: growth as the
engine decides until two scavenges at the present size have been seen,
then while they keep 80% or more, or 15% or more unless the last growth
was of no use (the part kept is more than 0.7 of what it was at the size
before), which holds until they keep 25 points more than then.

Nothing of the program's own thread can run it: a compiled program never
returns to its event loop, and no JavaScript runs at a collection.
Measured: `worker.getHeapStatistics()` from the main thread is answered
only when the worker's event loop turns (0 samples in 1.5 s of a busy
worker); the host functions the program calls are no clock (the compiler
makes 3.8 million calls of `identity_hash`, none in its first four
seconds, when the young generation grows). What does run in a busy worker
is a message of the inspector: the main thread, 250 ms after it started
the worker (`superviseNursery`; a short program never loads the
inspector), opens a session of the `NodeWorker` domain and every 50 ms
sends the worker `Runtime.evaluate` of the call, which the worker handles
at its next stack check (0.1–0.9 ms later, at times some hundred). No
domain of the debugger is enabled, and `bench/suite.lisp` runs in the same
time with and without it (1.000). The profiler is on only while the
semi-spaces are of 32 MB or more, and, once there is nothing to decide
(the limit reached, or no growth for four scavenges), only while the old
generation gains a tenth of a semi-space between two runs and one run in
twenty: recording the heap at every scavenge was 5% of the time of a
program whose garbage dies young in semi-spaces of 64 MB (`a-list-churn`
70 → 75 ms). It is opt-in (`CLYSM_NURSERY_ADAPT=1`; off by default, decided at integration: it opens an inspector session into the worker and gives back half of the list-building gain),
`CLYSM_NURSERY_TRACE=1` prints each decision.

Interleaved in one slot (the machine loaded; engine's 64 MB, the limit of
256 MB alone, and with the control): the self-hosted compiler on
`bench/suite.lisp` 4.85–4.91 s and 0.81 GB, 4.91–5.04 s and 1.18 GB,
4.80–4.89 s and 0.81 GB (214000 page faults, as with 64 MB); in
`bench/alloc.lisp`, `s-ring` 460–490, 98–107, 98–146 ms, `s-build-list`
632–664, 202–226, 294–477 ms (369 in the last version; SBCL 163–190: its
list of one repetition, 192 MB, is all alive at each scavenge until it is
dropped, which the control cannot tell from data that stays, so it grows
later than the engine alone, which had 256 MB in time for the second
repetition), the items of garbage that dies young the same;
`bench/alloc.lisp` against SBCL 1.36–1.39 on main, 0.96–0.98 with the
limit alone, 1.07 with the control.

The image in the main thread (`clysm script`, the REPL) has the limit of
256 MB without the control: nothing can interrupt the main thread to run
it (a worker that did would have to be started with the image: 30 ms),
and there `bench/apps.lisp`, Lem and start-up were the same with 64 and
256 MB. Every entry that starts the image itself passes the same
`--max-semi-space-size` as `bin/clysm`: `tools/run-tests.sh`,
`tests/ansi/run.sh`, `make repl`, `tools/bench-lem.sh`,
`tools/bench-startup.sh` (a `node host/run.mjs IMAGE` typed by hand has
the engine's 64 MB). **Pauses**: the longest scavenge copies what survived
of one semi-space, about 1 ms a megabyte, so `CLYSM_NURSERY_MB` is also
the bound of the pause: 64 is the engine's (scavenges of at most 57–59 ms
measured where 256 MB gave 110–197), and a smaller one shortens them in
proportion at the cost of the table above (`--max-semi-space-size=8`:
1.35–1.5 on the allocation probes). In the
browser and in Electron the engine's default stays: a page cannot set the
flag, and `ui/electron/main.js` passes `jsFlags` of the application's
options to `js-flags` for whoever wants another (not measured there: Lem's
comparator, in Node.js, is the same with 64 and 256 MB).

**Start-up: the scavenges of the heap image** (2026-11-23; not changed).
`clysm eval t` runs six scavenges, five of them copying the objects of the
heap image as it is restored (12 MB; 1–7 ms each, 10–15 of 65 ms). They
are born in the young generation (a `struct.new` of Wasm allocates there;
nothing a module or the host can say makes the engine allocate them old:
`--single-generation` is read when the heap is made and the code the
engine generates allocates young inline), so each is copied twice whenever
it is scavenged, and what can be saved is only what a short process never
gets to: copies put off until after it has ended. Two ways were measured,
best and median of 31 `clysm eval t` in a slot, and `bench/apps.lisp` in
the image:

| | `clysm eval t` | `bench/apps.lisp`, resident |
|---|---|---|
| as it is | 63.6, 69.9 ms | 1.00, 411 MB |
| `--semi-space-growth-factor=4` set by `host/run.mjs` until the image is loaded, then 2 | 62.3, 67.0 | |
| 8 | 64.1, 68.4 | 0.98, 411 MB |
| 16 (two scavenges instead of six) | 61.8, 64.8 | 1.00, 551 MB |
| 32 | 61.9, 66.2 | |
| `--min-semi-space-size=16` on the command line (above) | 60.9, 64.9 | 1.065, 555 MB |

The factor can be set at run time and set back, but the young generation
it leaves is twice as large as it would have been (16 MB instead of 8)
with 12 MB in it that count as survivors at the next scavenge, and a
script that then loads libraries reaches semi-spaces of 128 MB instead of
64: 2–5 ms of 65 for 140 MB. A scavenge the engine is not asked for cannot
be avoided without a larger space, and a larger space cannot be given
back (the engine shrinks it only when idle), so `host/run.mjs` is as it
was.

**Lists and sequences of a few elements** (2026-11-22; 40 probes of the
functions that make a list, a vector or a string, on arguments of 3 to 15
elements held in variables: SBCL folds `(length (copy-seq "literal"))` and
does not allocate at all, which reads as 5–7 times). Garbage that dies
young costs what it did (a cons 2 ns), so what could be gained was the work
around the allocation:

| | before | after | SBCL |
|---|---|---|---|
| `(make-list 8 :initial-element x)` | 70 ms | 36 | 26 |
| `(butlast l)` (8 elements) | 111 | 68 | 48 |
| `(coerce vector 'list)` (8) | 87 | 55 | 37 |
| `(coerce list 'string)` (4) | 43 | 34 | 19 |
| `(reverse string)` (15) | 116 | 51 | 42 |
| `(subseq vector 2 6)` | 49 | 39 | 30 |
| `(copy-seq vector)` (8) | 55 | 47 | 40 |
| `(loop for x in l append (list x i))` (3) | 78 | 53 | 27 |
| `(string-upcase string)` (15) | 95 | 80 | 31 |

(3 million calls each.) `make-list` and `butlast` counted with generic
arithmetic (their count is now a fixnum, and `butlast` builds its list in
order in one pass after the length); `reverse`, `subseq` and `coerce` went
through the storage's element type and its code for a simple vector or
string (`%vector-like`, `%vref`, `%vset` per element) and now read and
write those two kinds directly; `%copy-elements` copies fewer than 16
elements of a simple vector in a loop (`array.copy` of references is a
call of the engine's runtime: 19 ns for 8 elements against 8); `loop`'s
`append` copied each list with `copy-list` and then walked the copy for
its last cons, and now copies onto the tail in one pass
(`%loop-append-after`), and links a list made by `(list ...)` in the
clause, which is fresh, without copying it; `string-upcase` and
`string-downcase` have no call in their loop while the characters are
below 128 (`%string-case-rest` does the rest). The mapping functions
written at a call site (`%mapfun-expansion`) and `mapcar` itself made a
header cons for each list they built, which SBCL allocates on the stack:
they now test the tail (NIL or the last cons) as `loop`'s `collect` does,
one cons less per call (`b-deriv`, whose `mapcar`s are over two or three
arguments, 149 → 145 ms, `g-dderiv` 142 → 137, `v-mapping` 34 → 33; the
module 2.8 KB smaller). `v-seq-copying` (vectors of 20000 elements, which are
large objects) is 5% slower, 75 → 79 ms over six rounds, for a reason not
found: apart, `reverse` of such a simple vector is 108 → 88 ms for 2000,
of an adjustable one (the same code as before) 112 → 125, `copy-seq`
77 → 80. Still slower and left: `set-difference` of 8 and
3 elements 2.0 (the set table's set-up), `fill` of 16 elements 2.5 (the
keywords and the storage of the array), `string-upcase` 2.5 (the string is
filled with zeros, then written: 26 ns for 15 characters), `loop ... when
(oddp x) collect` 1.6. Since (2026-11-23): `(%set-schar s i (code-char n))`
of a fixnum N stores N as it is (`src/typed.lisp`: the character was made,
an `i31` with its tag, and taken apart again, three shifts a character in
the engine's code), `string-upcase` 80 → 65–68 ms (SBCL 29–33); `fill` of
the whole of a simple vector, or of a simple string with a character, is
a loop over it before the keywords' bounds are looked at, 35 → 21 ms
(SBCL 12–14).

The other listed items are not allocation (instruction-level profiles,
ticks of `--prof` placed on `--print-wasm-code`'s listing): `a-list-churn`
is 14 instructions a cons in a loop the engine unrolled four times (the
bump allocation, the header's two words, the fields, the test of the
list), equal to SBCL's time when run alone (68 against 72 ms; 16 of them
collection); `g-traverse` spends its ticks on the first read of each node
and of each cons of its list of sons (the cache: the objects are 120 and
32 octets); `p-vector-push` is 45 of 140 ms in `:add` (its sum passes 2^29
and is a boxed integer from then on) and 32 in growing the vector (arrays
of 512 KB are large objects); `b-deriv` and `g-browse` are one function
each (`deriv`, with 15% of the time in scavenges of a young generation of
1 MB; `browse-match`, 5%).

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

Checked casts (a `ref.cast` that signals a `type-error` instead of trapping,
`define-checked-casts` and `emit-instr` in `src/wasm.lisp`) were a `block`
around a `br_on_cast` to its end and a call of the cast's function
(`(:castfn T)`, which signals) on the fall-through, the call's result being
the cast value. V8 made that call a path that returns into the code: every
value live across a cast in a loop was spilled to the stack and loaded again
at each iteration (instruction-level profiles of Lem, `--print-wasm-code`:
a loop over a list of three fixnum accumulators did six stack accesses per
element). The function never returns normally (`%type-error`; a non-local
exit from a handler is thrown as the exception by `(:callsym k)`), so:

* each function's checked casts outside loops and `try_table`s branch with a
  `br_on_cast_fail` to a block of their type around the function's body,
  whose end calls the cast's function and is `unreachable` (one call per
  type in the function; `emit-function-code`, `checked-cast-types`). A
  cast is 6 octets instead of 13, the REPL image 0.6% smaller
  (9.20 → 9.15 MB);
* those in a loop keep the block in place, the call followed by
  `unreachable` (14 octets). Branching out of the loop from each of its
  casts as well made V8's code of the loops slower (`bench/programs.lisp`
  0.969 of main's time against 0.951 for the casts in place, three rounds);
* those in a `try_table` (`handler-case`, `unwind-protect`, `catch`) stay in
  place too: the error must be signalled where it can be caught.

With nothing live after the call, V8 keeps the values in registers (that
loop 50 → 20 ms; it is also peeled). Re-encoding every cast as an `if` on
`ref.test` hinted likely (tried before) gained 2-3% on hot code but grew a
cast to 19 octets and made code that runs once slower
(`l-alexandria-tests` 78 → 94-102 ms); this does not. Against main
before, interleaved on one slot (best of 2-3 rounds each, time against
SBCL's): `bench/suite.lisp` 0.80 → 0.77 (0.959; `b-clos` 1.26 → 1.04,
`b-sort` 0.54 → 0.47, `b-print`, `b-format`, `b-read` 0.89-0.92), the same
in `clysm script` 0.881 of the time, `library.lisp` 0.942,
`programs.lisp` 0.97-0.99 (two orders of the rounds), `control.lisp`
0.988, `gabriel.lisp` 0.993, `apps.lisp` 0.983, `tools/bench-lem.sh` 1.47 →
1.39 (0.95; 0.93 in another pair of rounds), `tools/bench-libs.sh` 0.988
(`l-alexandria-tests` the same: 8 runs of the test suite in a process,
three processes a round, three rounds), start-up 0.987. On the later main
(the cold branch hints, the hash helpers with plain casts, the filler
functions): suite 0.977, library 0.975, programs 0.979, apps 0.950,
`tools/bench-libs.sh` 0.97-1.01, Lem and start-up and `clysm script` of the
suite unchanged (0.99-1.01), the module of one export (`tests/lib/one.lisp`)
4.64 → 4.59 MB, the REPL image 9.90 → 9.83 MB. Casts inside loops
also branching to the blocks (smaller still, 9.12 MB): the same except
`programs.lisp`, 2-3% slower than in place. Tried and not kept: the call of the type-error function
from the untyped `car`/`cdr` (NIL, else the error) made a cast too, testing
NIL first (suite and programs within 1%, and the error's expected type
would be CONS); the hint `(:hint 1)` on the `if` of that test changed
nothing in V8's code. The type errors of the inline stores into typed
structure slots of doubles (`src/typed.lisp`) are followed by
`unreachable` too. A slow path that does return (the generic `aref` of an
array of unknown kind, fixnum overflow to `:add`) keeps its call; those are
hinted unlikely.

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

A round on 2026-11-22 (`tools/bench-lem.sh` on the held-out input: 1.15–1.16,
`l-type-redisplay` 1.5–1.8, `l-syntax-scan` 1.35–1.4, `l-line-down` and
`l-page-down` 1.3–1.4, the rest 0.75–1.05) profiled each action with V8's
CPU profiler and `node --prof` (ticks of each instruction, the code from
`--perf-prof`'s jitdump) against `sb-sprof` of the same action repeated
in one process. What it found, by item:

* The redisplay (typing with a redraw after each key, 3.6 s against SBCL's
  2.7 s for 1000 keys): no function stands out. Against SBCL's totals of the
  same functions: `char-width` and what it calls 1.1 (but `wide-char-p`
  1.8–2.4 inside the redisplay while `l-string-width`, the same chain in a
  tight loop, is 1.0: its ticks sit on the loads of the hash tables'
  vectors, a chain of eight dependent loads — symbol, value, instance,
  slot vector, bucket vector and its length, the bucket, the key — that
  stays in the cache only in the tight loop; SBCL's is about four),
  `create-logical-line` 1.8 and `create-drawing-objects` 1.5 (large
  functions holding many inline copies; the ticks are spread over them),
  the highlighting of the edited line (cl-ppcre) 2.0, the mode line 1.85.
  (Lem's `variable-value`, a generic function with an EQL specializer
  and an `&optional` parameter, looked like 2.0 in a profile whose
  anonymous functions were named after their toplevel form, which
  lumped its methods with everything else of the file; it is 1.5% of the
  redisplay, see the call sites below.)
* `l-syntax-scan`: cl-ppcre's character-class tests take what SBCL's take;
  the matchers' closures (`create-matcher-aux`, the character searcher,
  the scanner) twice SBCL's time, in the code around the calls of the next
  matcher (the character searcher's loop is 40 instructions an iteration:
  `*string*` reloaded and its type tested, the character of `schar` boxed
  and tested as a character, `chr` reloaded from the closure).
* Loading Lem's 360 fasls with ASDF (1.4 s): 20% of it was `subtypep` of
  two classes, which ASDF's `coerce-class` calls for every operation it
  plans, through the set algebra of types (`%type-not`, `equal` of the
  expansions); 4% the emptying of every used generic-function call site at
  each method added (`%bump-clos-epoch` walked all of them each time, the
  emptied ones too).

Made general:
* `subtypep` of two standard, structure or condition classes (or their
  names) whose precedence list is known answers from the precedence list
  (`%standard-class-type`, runtime/types.lisp). Lem's load 1407 → 1160 ms
  (three interleaved runs each on one slot: 1407 1411 1407 against 1175 1161
  1146), `tools/bench-startup.sh` 4.26 → 4.19 (`s-ql-babel` 390 → 373 ms).
* `%bump-clos-epoch` empties the sites in its list and empties the list: a
  site is noted again when it is filled (`%gf-site-register`).
* `make-instance` of a class without any initarg (Lem's `point`: every
  `with-point` and `copy-point`) took the slow path: the constructor's test
  for "only the standard methods apply" was the class's list of valid
  initargs, NIL for such a class. A constructor of more than eight
  initargs (Lem's `make-attribute`, eleven) fills a copy of the template
  from the arguments put in a vector, as the entries of up to eight do,
  instead of `nth` of the argument list for each slot.
* `loop ... by #'cddr` (any function of COMMON-LISP named with `function`
  or `quote`) calls the function directly, so that it is compiled inline;
  it was bound to a variable and called with `funcall` at each step (every
  plist walk).
* `nreverse` of a list is a helper of the module (`:nreverse-list`): a
  loop in place of the call through the function cell; another sequence
  calls the function. (The one-export library module 4,084,602 →
  4,066,560 bytes, the REPL image 8,933,012 → 8,907,553, with the other
  changes of this round.)

The actions themselves were unchanged within the runs' spread (two
interleaved rounds each: geometric mean 1.16 and 1.15 before, 1.16 and
1.15 after; `l-line-down` 1.34–1.38 → 1.29). What would gain more: a hash
table as a struct of its own with typed fields (the chain of loads above:
the slot vector and the casts of its elements go; done since, see "A hash
table is a structure of field slots") and the matchers' closures of cl-ppcre.

Measured and not kept (2026-11-22): call-site caches for generic functions
of one or two required parameters and `&optional`, `&rest` or `&key`
ones (at most four spread arguments), at a call passing the required
arguments alone: the four entries keyed on the required arguments' classes
as the sites of one or two arguments, the entry a function of the spread
arguments (the unbound marker for each optional one, NIL for the rest),
the misses out of line through `%gf-find-n-k`. Correct (EQL specializers,
methods added, keyword methods), but the function of such a generic
function already has its dispatch inline (`%gf-scall`): a probe of
`(vv obj :default)` with EQL methods 167–169 ms either way in a script
(SBCL 87); `render`/`configure` of bench/clos.lisp in a loop compiled as a
program 103–142 → 155–166 ms (slower: the engine inlines the generic
function's own dispatch into the caller of a whole program), in a script
within the noise; `bench/clos.lisp` 1.08–1.09 both compiled, 1.07 → 1.06
in a script; Lem's redisplay probe 1356–1390 against 1371–1385 ms; the
REPL image +16.8 KB.

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

### Applications in one module (`clysm build`)

Libraries loaded from fasls were the last broad area behind SBCL, and the
cause was measured (see "V8 inlines no call into another module"): a call
from a library's module into the runtime's, or into another system's, is a
`call_ref` of about 2.3 ns that the engine cannot inline, and what was tried
inside the model of loading (copies of leaf functions, direct imported
entries) did not pay. `clysm build` is the other route: the systems of an
application are compiled **with the runtime into one module, as a whole
program** (direct calls, copies of small functions, the type inference and
the dispatch over all of it).

```
clysm build [--system NAME]... [--asd FILE.asd]... [--load FILE]... [--asdf] [--eval FORM]... [-o OUT.wasm] [FILE...]
```

**What existed, and why it was not enough.** `clysm compile FILE...` (and
`--system` of `bin/clysm.mjs test`, `tools/system-files.lisp`) compiles the
files of a system on SBCL, the cross compiler evaluating what the files do
at compile time *on SBCL*, in mirrored packages (`CLYSM-T/NAME`), with its
own reader. That holds for plain code and breaks on what real systems do
when they are compiled: cl-ppcre does not compile (`(declaim
#.*standard-optimize-settings*)`: the variable is the target's), nor does
anything that uses a reader macro, a readtable, `#+`/`#-` of features of the
target's image, ASDF's `:around-compile`, a macro that calls a function of
the implementation, or a literal that is not a list, vector, string, number
or symbol; and the files are found by a list, not by the plan ASDF makes.
The heap image links the modules of an image into one (`linkHeapImage`,
Lem's desktop application), but not with the runtime: the linked module
holds the modules of the fasls, the image's own module stays apart, and the
functions the linked code calls in it remain imports (and the calls between
files remain calls through the function cells): the engine has two modules
and inlines nothing across them. (Read from `host/link.mjs`; not measured
separately, Lem's desktop build needing Electron.)

**The design: the compiler of the REPL image, in a second state**
(`src/static.lisp`). The image that builds is the REPL image, where ASDF,
Quicklisp, `compile-file` and the systems' compile-time worlds already
work. ASDF loads the systems into it as ever (from their fasls, compiling
those that are missing), and *each source file is compiled into the program
just before ASDF's load-op of it* (`perform :before` in
`contrib/asdf-hooks.lisp`, which calls `*static-file-hook*`; within the
file's `:around-compile` hook): then the files before it are loaded and it
is not, which is the state `compile-file` compiled it in. The file is read
by the image's reader (its readtable, `*package*`, `#.`, `#+`), its toplevel
forms are processed by the whole-program driver (`process-toplevel`), and
what they evaluate at compile time (`eval-when`, `defmacro`, `in-package`,
`defpackage`, `declaim`) is evaluated in the image as `compile-file` does.
* The compiler's state of a whole program (the tables of `reset-compiler`,
  the state of the code generator, the mode: `*static-state-variables*`) is
  bound while a file is compiled into the program (`call-in-static-state`)
  and *unbound* around everything the image itself compiles or evaluates
  meanwhile (`call-in-live-state`, `src/frontend.lisp`: `host-eval`, the
  expanders of macros, `live-compile-forms`, the units of `compile-file`),
  since both are the same compiler. What a form evaluated in the image
  records for the compiler (a constant, a compiler macro, the inline
  accessors of a `defstruct`, a special proclamation) goes to the tables of
  both (`table-put`).
* The packages of the program are the image's own (`*image-packages*`,
  `src/reader.lisp`): no mirror, so a symbol a macro interns is the
  program's.
* The runtime's files are compiled first, as for any whole program, without
  evaluating their compile-time forms again (`*static-image*` is
  `:runtime`: the image is that runtime) and without the image's macros
  (what the files say alone, as on SBCL): `tests/hello.lisp` built this way
  is the module `clysm compile` makes but for the order of some functions
  (5.248 against 5.245 MB), and the 116 files of `tests/cases` pass
  (`STATIC=1 tools/run-tests.sh`, part of `make test-all`).
* The files given after the systems are the program's own: compiled, not
  loaded (their `defun`s are defined in the image after each file, for the
  macros of the next, as `clysm compile` does on SBCL); `#+clysm-static`
  reads there. The `--eval` forms are compiled last.
* **Literal objects.** A whole program holds numbers, characters, symbols,
  strings, conses, simple vectors and arrays as globals. Libraries have
  other constants (a vector of octets or a hash table read with `#.`, a
  pathname, an instance with `make-load-form`, a list that contains
  itself): `constant-node` makes such a one a cell whose car is the object
  once made and whose cdr the function that makes it, set by a toplevel
  form put before the one being compiled (`emit-pending-forms`): an
  instance, structure or hash table by its `make-load-form`, the objects in
  those forms the same way; anything else read from its text as a fasl
  writes it. Each object is made once, when first referred to, and is the
  same object at every reference.
* **ASDF in the program** (`--asdf`, or when a system depends on `asdf` or
  `uiop`: Lem asks `asdf:system-relative-pathname` when it is loaded):
  `third_party/asdf/asdf.lisp` is compiled into the program as a file that
  is loaded in the image and cannot be evaluated again (`:loaded`: no
  compile-time evaluation, the image's macros), after the forms the REPL
  image runs when it starts (the features of the host, the default
  directory); then a form registers the systems of the program (and the
  systems their definition files are named after) as immutable, with
  version and definition file. 2.8 MB.
* Two things found on the way are in the whole-program compiler for all:
  `inline-single-use-constants` copied the initializer of the rest of a
  list into each cons before it (the square of the length: 4 GB and no end
  for Babel's tables of ten thousand entries; now each instruction is
  placed once), and `long-constant-p` and `const-global` recursed along
  the cdr of a list (the engine's stack); both walk a list in a loop now.

**What differs from loading the systems.** A call is bound to the
definition in the program, as in a file given to `compile-file` (CLHS
3.2.2.3 allows it within a file; here it holds across files and systems):
a redefinition when the program runs reaches `funcall`, `apply`, `#'name`
through the cell, and functions declaimed `notinline`. A method added, a
class redefined, a special variable set work as ever. The program has no
compiler (`eval` interprets). A file's compile-time forms are evaluated
once more than by `compile-file` and `load` when its fasl is also compiled
in the same run. A system loaded into the image before the build is not in
the program (a warning says so).

**A call with keywords of a generic function** was the one case found where
the whole program was slower than the image: `(ppcre:scan scanner string
:start s :end e)` in `do-matches` made a vector of the eight arguments for
the generic entry at each call, where code compiled in the image calls the
keyed entry of the function, its layout tested (`live-keyed-call-code`,
`*live-key-layout-hook*`). The program's files are now compiled with that
hook (the layout of the function as it is in the image, tested when the
call runs): `a-ppcre-tokens` built 57 -> 49 ms (46 loaded from fasls).
`clysm compile` on SBCL had no image to ask and made the vector too: it
now notes the layouts that the program's own `defgeneric` and `defmethod`
forms give when they are expanded (`sys::%note-key-layout` calls
`%program-key-layout-hook`, bound by `compile-program-1` around the
program's files; the runtime's generic functions are not noted, its code
is as before) and compiles such calls the same way: `c-gf-lambda-lists`
1.30, 1.20 -> 0.88, 0.95 times SBCL (the build before and after
alternately, two rounds), a generic function of two methods called with
`:start` and `:end` 3 million times twice 329-347 -> 231-254 ms;
`bench/clos.lisp` 0.72, 0.71 -> 0.72, 0.70 and `bench/programs.lisp` 0.69,
0.70 -> 0.69, 0.70 (the rest unchanged).

**Measured** (2026-11-23, `MODE=static tools/bench-libs.sh FILE` and
`MODE=static tools/bench-lem.sh FILE` against the same without `MODE`, on
one slot, alternately, two rounds of three runs; times SBCL's):

| | loaded from fasls | one module |
|---|---|---|
| `bench/libs.lisp`, geometric mean | 1.00, 1.00 | 0.79, 0.79 |
| `l-alexandria-tests` | 0.26 | 0.07 |
| `l-alexandria-utils` | 1.26, 1.32 | 1.20, 1.16 |
| `l-split-sequence` | 1.21, 1.21 | 0.99, 1.04 |
| `l-ppcre-scan` | 1.18, 1.21 | 1.21, 1.12 |
| `l-ppcre-replace` | 1.38, 1.41 | 1.47, 1.43 |
| `l-babel` | 1.51, 1.43 | 1.68, 1.73 |
| `bench/apps.lisp`, geometric mean | 1.06, 1.16 (1.17, 1.15 in a quieter run) | 1.07, 1.07 (1.04, 1.08) |
| `a-esrap` | 1.67, 1.48 (1.76, 1.63) | 1.39, 1.37 |
| `a-yason-parse` | 1.18, 1.35 | 1.13, 1.11 |
| `a-cl-json-decode` | 1.17, 1.45 | 1.33, 1.35 (1.14, 1.23) |
| `a-ppcre-tokens` | 1.00, 1.05 | 1.19, 1.16 |
| Lem (`tools/bench-lem.sh`), geometric mean | 1.03, 1.09 (1.05) | 0.92, 0.90 (0.86 in the quieter run) |
| `l-line-down`, `l-page-down` | 1.24, 1.25 | 1.00-1.10, 0.98-1.06 |
| `l-string-width` | 1.16, 1.10 | 0.82, 0.79 |
| `l-type-redisplay` | 1.41, 1.97 | 1.25, 1.29 |
| `l-syntax-scan` | 1.27, 1.23 | 1.32, 1.31 |

(The machine was busy with tests during the second run; the first, in
parentheses, had the keyword calls not yet fixed.) What calls the runtime
and other systems much gains (Alexandria, split-sequence, esrap, yason,
Lem's buffer and redisplay code: 0.6 to 0.9 of the time).

**What was slower built than loaded** (`l-babel`, `l-ppcre-replace`,
`a-ppcre-tokens`, `l-syntax-scan`, 5-15% in that table) was not the engine
nor the size of the module (tier-up, the inlining budget, the width of
function indexes, the worker were each ruled out: the same program in a
worker or the main thread, with `--no-wasm-inlining`, `--liftoff-only`,
`--no-liftoff`, kept the difference). It was two things the image's
compiler does for code it compiles and the whole-program compiler did not:
* **A function declaimed `inline` with `&optional`, `&key` or `&rest`**
  was not copied (the copies of a whole program are made of the nodes of
  functions of required parameters, `src/inline.lisp`; the image copies
  the source, `inline-expansion-call`). Babel's `decoding-error`
  (`&optional`) is called in every arm of its decoders: called, its value
  is any object, and the code point of *every* arm was boxed and tested
  again. A whole program now keeps the source of such a function of the
  program (`*program-inline-expansions*`, from `process-defun`, once the
  runtime is compiled: the runtime's code is as before) and copies it at
  its calls, on SBCL too: `babel:octets-to-string` of 330 000 characters,
  300 times, 245 -> 181-202 ms (loaded from fasls: 190-205).
* **What `compile-file` evaluates when it compiles a file** and
  `process-toplevel` did not: `defstruct`, `defclass`, `deftype`,
  `define-condition`, `defsetf`... (`sys::%compile-time-definers`), and a
  `defconstant` whose value can be computed and is a number, character,
  symbol or string (`%cf-note-defconstant`). The rest of the file is then
  compiled with the class (the type tests of a structure's instances) and
  the constant (cl-ppcre's `(find chr +whitespace-char-string+ :test
  #'char=)` as comparisons). The build did have them *when `compile-file`
  had just compiled the same file for its fasl* (its compile-op comes
  before the load-op that the build hooks), that is in the first build
  after the image changed, and not in the builds after it: the same
  program was 14 functions and 169 globals smaller and 13% faster (25%
  with `--no-wasm-inlining`) in a first build, which is how it was found
  (a form added *after* the benchmark made it slower: it was the second
  build). `process-toplevel` now evaluates them for a program built in the
  image (`*static-compile-time-definers*`, `static-note-defconstant`).
After both (same method, two rounds): `bench/libs.lisp` 0.78, 0.76
(loaded 1.00, 1.00), `bench/apps.lisp` 1.08, 1.03 (loaded 1.19, 1.18);
`l-ppcre-replace` 1.36, 1.42 (loaded 1.41, 1.47), `a-ppcre-tokens` 1.16,
1.07 (loaded 1.09, 1.07), `l-ppcre-scan` 1.16, 1.25 (1.16, 1.08),
`l-babel` 1.60, 1.51 (loaded 1.42, 1.46: the decoders are now the same
code or better, the rest of that benchmark is `concatenate` and
`make-string` of large strings, where the two differ by which run of the
process it is), `a-esrap` 1.38, 1.41 (loaded 1.58, 1.66), `a-yason-parse`
1.11, 1.04 (1.35, 1.36), `l-alexandria-utils` 1.09, 1.06 (1.30, 1.30).

Building: `bench/libs.lisp`'s seven systems (76 files) 11 s, of which the
runtime 3 s; Lem's benchmark (41 systems, 397 files, with ASDF) 80-105 s
and a module of 31 MB; a small program 3.4 s (`clysm compile` on SBCL takes
about 2 s). The REPL image is 59 KB larger (`src/static.lisp` and the
constants' code: 8.596 -> 8.655 MB); the shaken module of one export
(`tests/lib/one.max-bytes`) is unchanged, the mode adding nothing to
programs not built with it. `tests/static/` (`make test-static`) builds an
application of two systems and a file: macros and a function of
`eval-when` across systems, a reader macro, `defconstant`s made by a macro,
`#.` constants (a hash table, octets, a pathname, instances with
`make-load-form` and their identity), a method on another system's generic
function, `intern` at run time and a `notinline` function redefined.

**`--shake`** leaves the functions that nothing refers to out of the built
module (`shake-module`, as `clysm compile --shake` in the compiler compiled
by itself: without the heap made beforehand, `src/preinit.lisp`, which
only the compiler on SBCL has): `tests/hello.lisp` 5.25 -> 3.90 MB,
`tests/static`'s application 5.33 -> 3.98 MB, the seven systems of
`bench/libs.lisp` with the benchmarks 9.10 -> 7.61 MB (the build 11 -> 18
s). The contract is that of `--shake`: of the library benchmarks only
Alexandria's own tests fail it (rt evaluates the forms of the tests, kept
as data, when it runs: the functions they name are not referred to by
code).

**A heap image of a built program** (`--main FUNCTION --image`). The
mechanism of the REPL image (Heap images) serves a built program as it is:
its module has the image functions of any whole program. `--main` ends the
program with a form that calls FUNCTION, or, with `CLYSM_APP_IMAGE` in the
environment, saves the heap there with FUNCTION as what to call
(`sys::save-image`); `bin/clysm build --image` runs the module once that
way (`CLYSM_SAVE_IMAGE=1`) and leaves `OUT.img` next to `OUT.wasm`, and
`clysm exec OUT.wasm` starts from it when it is there and not older than
the module (`CLYSM_NO_HEAP_IMAGE=1`: not). The toplevel forms of the
systems then do not run at a start. `tests/static`'s application 0.30 ->
0.09 s (an image of 6.4 MB); Lem's benchmark program (41 systems, 397
files, ASDF) to its first form 1.0-1.2 -> 0.30-0.39 s, an image of 52 MB
beside the module of 34 MB; its benchmarks take the same time started
either way (two rounds on a slot). What the systems do when they are
loaded is then done when the image is saved, on the machine that builds: a
program that reads its environment in a toplevel form (a path, the time,
a file) has what it read then, as with any saved image.

Next: the heap of a shaken built program made beforehand (preinit in the
image); the linked image and compilation hints for the start of a built
program (as Lem's desktop application has); the module in a browser page
(`host/web`).

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
