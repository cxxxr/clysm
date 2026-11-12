# ASDF for Clysm

`asdf.lisp` is ASDF 3.3.7, the latest release (MIT license, see the header of
the file; https://asdf.common-lisp.dev/archives/asdf-3.3.7.lisp) with a small
port to Clysm. `clysm-port.diff` is the complete change against the upstream
file, and `port.py` applies it to a new upstream release:

```
curl -o asdf.lisp https://asdf.common-lisp.dev/archives/asdf-3.3.N.lisp
python3 port.py asdf.lisp
```

`(require :asdf)` loads it (the runtime's REQUIRE finds it through the Clysm
installation directory).

The port adds `#+clysm` branches where UIOP dispatches on the implementation:

* the implementation check, `implementation-type` (`:clysm`) and
  `architecture` (`:wasm`);
* `getenv` / `(setf getenv)` (also unsetting), `getcwd`, `chdir`,
  `delete-empty-directory`, `quit`, `raw-command-line-arguments` — implemented with the Node.js host
  (`sys::%getenv`, `sys::%getcwd`, … in `runtime/js.lisp`);
* `*unspecific-pathname-type*` is NIL;
* ASDF's module provider is added to `sys:*module-provider-functions*`, so
  `(require :some-system)` loads systems through ASDF.

Not available: `run-program` / `launch-program` (no subprocesses), image
dumping and fasl concatenation. Compiled files go to
`~/.cache/common-lisp/clysm-<version>-<os>-wasm/` as usual.
