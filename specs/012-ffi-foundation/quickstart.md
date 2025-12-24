# Quickstart: FFI Foundation

**Date**: 2025-12-24
**Branch**: `012-ffi-foundation`

## Prerequisites

```bash
# Enter development environment
nix develop

# Verify tools are available
sbcl --version          # SBCL 2.4+
wasmtime --version      # wasmtime 27.0+
wasm-tools --version    # For validation
node --version          # Node.js 22+ (for JS tests)
```

## Basic Usage

### 1. Import a Host Function

```lisp
;; Declare a host function that prints to console
(ffi:define-foreign-function console-log "host.console.log" (:string) :void)

;; Use it like a normal Lisp function
(console-log "Hello from Lisp!")
```

### 2. Export a Lisp Function

```lisp
;; Define a Lisp function
(defun calculate-tax (amount)
  (floor (* amount 108/100)))

;; Export it for host to call
(ffi:export-function 'calculate-tax
  :as "calculateTax"
  :signature ((:fixnum) :fixnum))
```

### 3. Dynamic Host Calls

```lisp
;; Call host functions dynamically
(let ((random-value (ffi:call-host "math.random")))
  (format t "Random: ~A~%" random-value))
```

## Host Setup

### wasmtime (Rust)

```rust
use wasmtime::*;

let engine = Engine::default();
let module = Module::from_file(&engine, "output.wasm")?;
let mut store = Store::new(&engine, ());

// Provide imports
let host_console_log = Func::wrap(&mut store, |caller: Caller<'_, ()>, msg: ExternRef| {
    // Handle string from Lisp
    println!("Lisp says: {:?}", msg);
});

let imports = [host_console_log.into()];
let instance = Instance::new(&mut store, &module, &imports)?;

// Call exported Lisp function
let calculate_tax = instance.get_typed_func::<i32, i32>(&mut store, "calculateTax")?;
let result = calculate_tax.call(&mut store, 1000)?;
println!("Tax on 1000: {}", result);  // 1080
```

### JavaScript (Node.js)

```javascript
const fs = require('fs');

const wasmBuffer = fs.readFileSync('output.wasm');

const imports = {
  host: {
    'console.log': (msg) => {
      console.log('Lisp says:', msg);
    }
  },
  math: {
    random: () => Math.random()
  }
};

WebAssembly.instantiate(wasmBuffer, imports).then(({ instance }) => {
  // Call exported Lisp function
  const result = instance.exports.calculateTax(1000);
  console.log('Tax on 1000:', result);  // 1080
});
```

## Supported Types

| Type | Lisp | Wasm | Host (JS) | Host (Rust) |
|------|------|------|-----------|-------------|
| `:fixnum` | Integer | i32 | number | i32 |
| `:float` | Float | f64 | number | f64 |
| `:string` | String | externref | string* | ExternRef |
| `:boolean` | t/nil | i32 (1/0) | boolean | bool |
| `:anyref` | Any | anyref | object | ExternRef |
| `:void` | - | (none) | undefined | () |

*String handling requires UTF-8 encoding/decoding on host side.

## Error Handling

```lisp
;; Handle host errors
(handler-case
    (ffi:call-host "host.file.read" "/nonexistent")
  (ffi:ffi-host-error (e)
    (format t "Error: ~A~%" (ffi:ffi-error-message e))))

;; Type errors
(handler-case
    (console-log (make-instance 'some-class))  ; Not a string!
  (ffi:ffi-type-error (e)
    (format t "Cannot marshal: ~A~%" (ffi:ffi-type-error-actual-value e))))
```

## Testing

```bash
# Run unit tests
rove tests/unit/ffi-types-test.lisp

# Run contract tests (Wasm validation)
rove tests/contract/ffi-section-test.lisp

# Run integration tests (requires wasmtime/node)
rove tests/integration/ffi-import-test.lisp
```

## Compilation

```lisp
;; Compile a file with FFI declarations
(clysm:compile-file "my-ffi-app.lisp" :output "output.wasm")

;; Validate the output
;; (automatically done, or manually:)
;; $ wasm-tools validate output.wasm
```

## Common Patterns

### Optional Host Features

```lisp
;; Check if host provides a function before using
(defun safe-log (message)
  (handler-case
      (ffi:call-host "host.console.log" message)
    (ffi:ffi-host-error ()
      ;; Fallback: do nothing if logging unavailable
      nil)))
```

### Callback Pattern

```lisp
;; Register a callback for host events
(defun on-click (event-data)
  (format t "Clicked: ~A~%" event-data)
  t)  ; Return true to indicate handled

(ffi:export-function 'on-click
  :as "onClick"
  :signature ((:anyref) :boolean))
```

### Batch Operations

```lisp
;; Export multiple related functions
(defun api-get (path) ...)
(defun api-post (path body) ...)
(defun api-delete (path) ...)

(ffi:export-function 'api-get :as "apiGet" :signature ((:string) :anyref))
(ffi:export-function 'api-post :as "apiPost" :signature ((:string :anyref) :anyref))
(ffi:export-function 'api-delete :as "apiDelete" :signature ((:string) :boolean))
```

## Host Environment Requirements (T067)

### wasmtime Requirements

```bash
# Minimum version: wasmtime 27.0+ with WasmGC support
wasmtime --version

# Required flags for running Clysm modules:
wasmtime --wasm gc --wasm function-references --wasm exceptions program.wasm

# Building the Rust test harness:
cd test-harness/wasmtime
cargo build --release
```

**wasmtime Configuration:**
- WasmGC must be enabled (`--wasm gc`)
- Function references required (`--wasm function-references`)
- Exception handling for error propagation (`--wasm exceptions`)

### Node.js Requirements

```bash
# Minimum version: Node.js 22+ with WasmGC support
node --version

# V8 flags may be needed for WasmGC features:
node --experimental-wasm-gc program.js

# Running the test harness:
node test-harness/node/ffi-host.js
```

**Node.js WasmGC Support:**
- Node.js 22+ has native WasmGC support
- Older versions may need `--experimental-wasm-gc` flag
- For string externref handling, the host must handle opaque references

### Using the Test Harness

```bash
# Run all FFI tests across both environments:
./test-harness/run-ffi-tests.sh

# Run only Node.js tests:
./test-harness/run-ffi-tests.sh --nodejs-only

# Run only wasmtime tests:
./test-harness/run-ffi-tests.sh --wasmtime-only

# Run only Common Lisp tests:
./test-harness/run-ffi-tests.sh --lisp-only
```

### Providing Host Functions

**Module Namespaces:**
Host functions are organized by namespace (e.g., `host.*`, `env.*`):

| Namespace | Purpose | Example Functions |
|-----------|---------|-------------------|
| `host` | Application-specific imports | `log`, `add`, `random` |
| `env` | Environment utilities | `print`, `exit`, `getenv` |

**JavaScript Import Object:**
```javascript
const imports = {
    host: {
        log: (msg) => console.log('[Lisp]', msg),
        add: (a, b) => a + b,
        random: () => Math.random()
    },
    env: {
        print: (msg) => process.stdout.write(String(msg) + '\n'),
        exit: (code) => process.exit(code)
    }
};
```

**Rust Import Registration:**
```rust
linker.func_wrap("host", "add", |a: i32, b: i32| -> i32 { a + b })?;
linker.func_wrap("host", "log", |_: ExternRef| println!("..."))?;
```

### Type Marshalling Details

**i31ref (fixnum):**
- 31-bit signed integer packed in i31ref
- Host receives/returns i32
- Sign-extension handled automatically

**externref (string):**
- WasmGC array passed as opaque externref
- Host cannot directly read string content without memory access
- For simple cases, pass string indices or use host-side storage

**anyref:**
- Passthrough for any Lisp value
- Host receives opaque reference
- Useful for callbacks and storing Lisp objects

## Limitations

1. **Sync only**: Async/await patterns are not supported in this phase
2. **No linear memory**: Strings use WasmGC arrays, not linear memory
3. **Fixed arity**: Variadic functions need wrapper handling
4. **Type safety**: Runtime type checks for dynamic calls
