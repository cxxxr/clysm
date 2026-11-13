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
