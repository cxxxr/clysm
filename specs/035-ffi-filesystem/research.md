# Research: FFI Filesystem Access

**Feature**: 035-ffi-filesystem
**Date**: 2025-12-27
**Status**: Complete

## Research Topics

### R1: Stream I/O Scope Decision

**Question**: The spec's acceptance scenarios mention `read-line` and `write-line` but functional requirements only specify `read-file-contents` and `write-file-contents`. Which approach?

**Decision**: Whole-file operations only

**Rationale**:
- Clysm is a compiler, not a full runtime library. Simpler APIs reduce implementation complexity.
- Whole-file operations (`read-file-contents`, `write-file-contents`) are sufficient for the stated goals: configuration loading, data processing, code generation.
- Line-based I/O can be built in Lisp on top of whole-file reads using string operations.
- Stream state management (current position, buffering) adds significant complexity without clear benefit.
- The existing stream I/O (015-ffi-stream-io) already provides stdin/stdout character/line operations for console interaction.

**Alternatives considered**:
| Alternative | Reason Rejected |
|-------------|-----------------|
| Full stream I/O (read-line, write-line, read-char, write-char) | Adds state management complexity; can be built on whole-file |
| Minimal stream (read-line + write-line only) | Still requires position tracking; hybrid approach confuses API |

**Action**: Update spec acceptance scenarios to use `read-file-contents` instead of `read-line`.

---

### R2: File Handle Representation in WasmGC

**Question**: How should file handles be represented in the WasmGC type system?

**Decision**: Opaque externref handle from host

**Rationale**:
- Constitution mandates WasmGC-First with no linear memory.
- Host file descriptor (fd) is an i32 in WASI, but must be wrapped.
- Using `externref` allows the host to manage the underlying descriptor.
- The Lisp `file-stream` struct wraps the externref with direction and state.

**Technical approach**:
```lisp
;; file-stream is a WasmGC struct
(defstruct file-stream
  (handle :type externref)      ; Opaque host handle
  (direction :type symbol)      ; :input or :output
  (pathname :type string)       ; Original filename
  (open-p :type boolean))       ; T if still open
```

In WasmGC:
```wat
(type $file-stream (struct
  (field $handle (mut externref))   ; Host file handle
  (field $direction (ref $symbol))  ; :input or :output
  (field $pathname (ref $string))   ; Original filename
  (field $open_p (mut i31ref))      ; Boolean: T or NIL
))
```

---

### R3: file-error Condition Design

**Question**: How should filesystem errors be represented in the condition system?

**Decision**: Extend existing `stream-error` pattern with pathname slot

**Rationale**:
- Existing `stream-error` (014-condition-system) is for stream-related errors.
- ANSI CL specifies `file-error` as a separate error type with `:pathname` slot.
- Follow ANSI CL semantics: `file-error` is subtype of `error` with `pathname` slot.

**Implementation**:
```lisp
(defclass file-error (error)
  ((pathname
    :initarg :pathname
    :reader file-error-pathname
    :documentation "The pathname that caused the error"))
  (:documentation "Condition for filesystem-related errors."))
```

**Error scenarios**:
| Scenario | Condition | Slots |
|----------|-----------|-------|
| File not found | file-error | :pathname "foo.txt" |
| Permission denied | file-error | :pathname "foo.txt" |
| Directory not found | file-error | :pathname "dir/foo.txt" |
| Closed stream access | stream-error | :stream <file-stream> |

---

### R4: FFI Import Namespace

**Question**: What namespace should filesystem FFI imports use?

**Decision**: `clysm:fs` namespace (following `clysm:io` pattern)

**Rationale**:
- Existing I/O uses `clysm:io` namespace.
- Filesystem is distinct from console I/O, deserves separate namespace.
- WASI separates `wasi:cli` (stdio) from `wasi:filesystem`.

**FFI declarations**:
```lisp
(ffi:define-foreign-function %open-file "clysm:fs.open"
  (:string :symbol :symbol :symbol) :externref)

(ffi:define-foreign-function %close-file "clysm:fs.close"
  (:externref) :void)

(ffi:define-foreign-function %read-all "clysm:fs.read-all"
  (:externref) :string)

(ffi:define-foreign-function %write-all "clysm:fs.write-all"
  (:externref :string) :void)
```

---

### R5: Host Shim Architecture

**Question**: How should the JavaScript host shim be structured for dual-environment support?

**Decision**: Single JavaScript module with environment detection

**Rationale**:
- Both wasmtime and browser use JavaScript for Wasm instantiation.
- wasmtime can use Node.js-style fs module or WASI Preview2.
- Browser uses virtual filesystem (in-memory or IndexedDB).
- Single module with `getImports()` function following io-shim.js pattern.

**Architecture**:
```javascript
// fs-shim.js
class FileSystem {
  constructor(backend) {
    this.backend = backend; // 'wasi' | 'virtual' | 'node'
  }

  open(path, direction, ifExists, ifDoesNotExist) { ... }
  close(handle) { ... }
  readAll(handle) { ... }
  writeAll(handle, content) { ... }
}

export function getImports(backend = 'auto') {
  const fs = new FileSystem(detectBackend(backend));
  return {
    'clysm:fs': {
      'open': fs.open.bind(fs),
      'close': fs.close.bind(fs),
      'read-all': fs.readAll.bind(fs),
      'write-all': fs.writeAll.bind(fs)
    }
  };
}
```

---

### R6: WASI Preview2 Integration

**Question**: How does this integrate with WASI Preview2 filesystem API?

**Decision**: Adapter layer translates clysm:fs to WASI calls

**Rationale**:
- WASI Preview2 uses Component Model with different ABI.
- wasmtime provides `wasi:filesystem` interface.
- Our FFI layer abstracts over the WASI complexity.
- Host shim (JavaScript) calls WASI functions internally.

**WASI mapping**:
| clysm:fs | WASI Preview2 |
|----------|---------------|
| open | `wasi:filesystem/types.descriptor.open-at` |
| close | Descriptor dropped (RAII) |
| read-all | `wasi:filesystem/types.descriptor.read` (loop until EOF) |
| write-all | `wasi:filesystem/types.descriptor.write` (loop until complete) |

---

### R7: with-open-file Macro Expansion

**Question**: How should `with-open-file` expand to use `unwind-protect`?

**Decision**: Standard expansion pattern with keyword argument processing

**Expansion pattern**:
```lisp
(with-open-file (stream "file.txt" :direction :input)
  (read-file-contents stream))

;; Expands to:
(let ((stream (open-file "file.txt" :direction :input)))
  (unwind-protect
      (progn (read-file-contents stream))
    (when stream
      (close-file stream))))
```

**Keyword argument handling**:
- `:direction` - Required, `:input` or `:output`
- `:if-exists` - For output, `:supersede` (default) or `:error`
- `:if-does-not-exist` - `:error` (input default) or `:create` (output default)

---

## Summary

All research topics resolved. Ready for Phase 1 design.

| Topic | Decision | Impact |
|-------|----------|--------|
| Stream I/O scope | Whole-file only | Simpler API, fewer tests |
| File handle | Opaque externref | WasmGC compliant |
| file-error | ANSI-compliant with pathname | Consistent condition hierarchy |
| FFI namespace | clysm:fs | Clear separation from I/O |
| Host shim | Single module with backends | Code reuse |
| WASI | Adapter pattern | Abstraction over WASI Preview2 |
| with-open-file | unwind-protect expansion | Safe resource management |
