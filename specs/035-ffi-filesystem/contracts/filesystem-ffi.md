# Contract: Filesystem FFI

**Feature**: 035-ffi-filesystem
**Date**: 2025-12-27
**Version**: 1.0.0

## Overview

This contract defines the FFI interface between Clysm-compiled WebAssembly modules and host filesystem implementations. The host must provide these imports under the `clysm:fs` namespace.

## FFI Import Declarations

### clysm:fs.open

Opens a file and returns an opaque handle.

**Signature**:
```
(pathname: string, direction: string, if-exists: string, if-does-not-exist: string) → externref
```

**Parameters**:
| Parameter | Type | Values | Description |
|-----------|------|--------|-------------|
| pathname | string | Any valid path | File path to open |
| direction | string | "input", "output" | Access mode |
| if-exists | string | "supersede", "error" | Output file behavior |
| if-does-not-exist | string | "error", "create" | Missing file behavior |

**Returns**: Opaque file handle (externref)

**Errors**: Throws on:
- File not found (when if-does-not-exist = "error")
- File exists (when if-exists = "error")
- Permission denied
- Invalid path

**Example Host Implementation**:
```javascript
function open(pathname, direction, ifExists, ifDoesNotExist) {
  const flags = direction === 'input' ? 'r' :
                (ifExists === 'supersede' ? 'w' : 'wx');
  try {
    return fs.openSync(pathname, flags);
  } catch (e) {
    if (e.code === 'ENOENT' && ifDoesNotExist === 'create') {
      return fs.openSync(pathname, 'w');
    }
    throw e;
  }
}
```

---

### clysm:fs.close

Closes an open file handle.

**Signature**:
```
(handle: externref) → void
```

**Parameters**:
| Parameter | Type | Description |
|-----------|------|-------------|
| handle | externref | File handle from open |

**Returns**: Nothing

**Errors**: Throws on invalid handle (optional; may silently ignore)

**Example Host Implementation**:
```javascript
function close(handle) {
  if (handle != null) {
    fs.closeSync(handle);
  }
}
```

---

### clysm:fs.read-all

Reads the entire contents of a file as a UTF-8 string.

**Signature**:
```
(handle: externref) → string
```

**Parameters**:
| Parameter | Type | Description |
|-----------|------|-------------|
| handle | externref | Open file handle (input direction) |

**Returns**: String containing file contents (UTF-8 decoded)

**Errors**: Throws on:
- Invalid handle
- Handle not opened for input
- Read failure

**Example Host Implementation**:
```javascript
function readAll(handle) {
  const buffer = fs.readFileSync(handle, { encoding: 'utf-8' });
  return buffer;
}
```

---

### clysm:fs.write-all

Writes a string to a file, replacing existing contents.

**Signature**:
```
(handle: externref, contents: string) → void
```

**Parameters**:
| Parameter | Type | Description |
|-----------|------|-------------|
| handle | externref | Open file handle (output direction) |
| contents | string | UTF-8 string to write |

**Returns**: Nothing

**Errors**: Throws on:
- Invalid handle
- Handle not opened for output
- Write failure
- Disk full

**Example Host Implementation**:
```javascript
function writeAll(handle, contents) {
  fs.writeSync(handle, contents, null, 'utf-8');
}
```

---

## Wasm Import Section

The compiled Wasm module will contain these imports:

```wat
(import "clysm:fs" "open" (func $fs_open
  (param $pathname (ref $string))
  (param $direction (ref $string))
  (param $if_exists (ref $string))
  (param $if_dne (ref $string))
  (result externref)))

(import "clysm:fs" "close" (func $fs_close
  (param $handle externref)))

(import "clysm:fs" "read-all" (func $fs_read_all
  (param $handle externref)
  (result (ref $string))))

(import "clysm:fs" "write-all" (func $fs_write_all
  (param $handle externref)
  (param $contents (ref $string))))
```

---

## Error Handling Contract

### Host Error → Lisp Condition Translation

| Host Error | Lisp Condition | Notes |
|------------|----------------|-------|
| ENOENT | file-error | File not found |
| EEXIST | file-error | File exists (if-exists :error) |
| EACCES | file-error | Permission denied |
| ENOTDIR | file-error | Path component not directory |
| EMFILE | file-error | Too many open files |
| Any exception | ffi-host-error | Caught by FFI layer |

The Clysm FFI layer translates `ffi-host-error` to `file-error` with the pathname.

---

## Test Scenarios

### Contract Tests (verify Wasm module validates)

| Test | Input | Expected |
|------|-------|----------|
| Import section structure | Compiled module | Contains 4 clysm:fs imports |
| open function type | wasm-tools validate | Correct signature |
| close function type | wasm-tools validate | Correct signature |
| read-all function type | wasm-tools validate | Correct signature |
| write-all function type | wasm-tools validate | Correct signature |

### Integration Tests (verify behavior with host)

| Test | Setup | Action | Expected |
|------|-------|--------|----------|
| Read existing file | Create "test.txt" | `(read-file-contents "test.txt")` | Returns contents |
| Read missing file | No file | `(read-file-contents "missing.txt")` | Signals file-error |
| Write new file | No file | `(write-file-contents "new.txt" "data")` | Creates file |
| Write existing file | Create file | `(write-file-contents "existing.txt" "new")` | Overwrites |
| UTF-8 roundtrip | None | Write "こんにちは", read back | Matches |
| Close then read | Open, close | Read from closed | Signals file-error |

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-12-27 | Initial contract definition |
